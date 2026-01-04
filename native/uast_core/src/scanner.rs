//! High-performance parallel directory scanner.
//!
//! Uses rayon for parallelism and walkdir for efficient traversal.
//! Each thread creates its own parser since parsers are not thread-safe.
//!
//! # Architecture
//!
//! 1. walkdir collects file list first (not thread-safe for parallel iteration)
//! 2. rayon processes files in parallel using a thread pool
//! 3. Each thread creates its own parser for the file's language
//! 4. Matches are collected via callback or returned in batch
//!
//! # Usage
//!
//! ```ignore
//! use uast_core::scanner::{scan_directory, ScanOptions};
//!
//! let options = ScanOptions::default();
//! let stats = scan_directory(Path::new("/my/project"), "(function_declaration)", &options, |path, matches| {
//!     println!("{}: {} matches", path.display(), matches.len());
//!     true // continue scanning
//! })?;
//! ```

use crate::error::Error;
use crate::ffi::{cstr_to_str_len, UastResult};
use crate::grammars;
use crate::parser::Parser;
use crate::query;
use parking_lot::Mutex;
use rayon::prelude::*;
use std::ffi::{c_char, c_void};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use walkdir::WalkDir;

// ============================================================================
// FFI Types
// ============================================================================

/// File match result for FFI.
///
/// Contains information about a single file that had matches.
#[repr(C)]
#[derive(Clone)]
pub struct FileMatch {
    /// Pointer to the file path (not null-terminated, use file_path_len)
    pub file_path: *const c_char,
    /// Length of file_path in bytes
    pub file_path_len: u32,
    /// Number of matches in this file
    pub match_count: u32,
    /// Line number of first match (1-indexed, 0 if no matches)
    pub first_match_line: u32,
}

impl Default for FileMatch {
    fn default() -> Self {
        Self {
            file_path: std::ptr::null(),
            file_path_len: 0,
            match_count: 0,
            first_match_line: 0,
        }
    }
}

/// Scan options for directory scanning.
///
/// All fields have sensible defaults via `ScanOptions::default()`.
#[repr(C)]
#[derive(Clone)]
pub struct ScanOptions {
    /// Maximum directory depth (0 = unlimited)
    pub max_depth: u32,
    /// Follow symbolic links
    pub follow_links: bool,
    /// Include hidden files (files starting with '.')
    pub include_hidden: bool,
    /// Maximum file size to parse in bytes (0 = unlimited)
    pub max_file_size: u64,
    /// Number of threads for parallel processing (0 = auto, uses available CPUs)
    pub threads: u32,
}

impl Default for ScanOptions {
    fn default() -> Self {
        Self {
            max_depth: 0,           // Unlimited
            follow_links: false,    // Don't follow symlinks by default
            include_hidden: false,  // Skip hidden files by default
            max_file_size: 10 * 1024 * 1024, // 10MB default limit
            threads: 0,             // Auto-detect thread count
        }
    }
}

/// Statistics from a directory scan.
///
/// Provides information about scan progress and results.
#[repr(C)]
#[derive(Clone, Default)]
pub struct ScanStats {
    /// Total number of files scanned
    pub files_scanned: u64,
    /// Number of files that had at least one match
    pub files_matched: u64,
    /// Total number of pattern matches across all files
    pub total_matches: u64,
    /// Number of files skipped (unsupported extension, too large, etc.)
    pub files_skipped: u64,
    /// Number of files that failed to parse
    pub parse_errors: u64,
    /// Total scan time in milliseconds
    pub elapsed_ms: u64,
}

/// Callback type for file matches during scanning.
///
/// Called for each file that has matches. Return `true` to continue scanning,
/// `false` to stop early.
pub type FileMatchCallback = extern "C" fn(file_match: *const FileMatch, user_data: *mut c_void) -> bool;

/// Thread-safe wrapper for user_data pointer.
///
/// Raw pointers are not `Sync` by default, but for FFI callbacks the caller
/// is responsible for ensuring thread safety of the user_data they pass.
#[derive(Clone, Copy)]
struct SyncUserData(*mut c_void);

// SAFETY: The caller is responsible for ensuring that the user_data pointer
// is thread-safe or that the callback handles synchronization internally.
unsafe impl Sync for SyncUserData {}
unsafe impl Send for SyncUserData {}

// ============================================================================
// High-Level Rust API
// ============================================================================

/// Result from scanning a single file.
pub struct FileScanResult {
    /// Path to the file
    pub path: PathBuf,
    /// All matches found in the file
    pub matches: Vec<query::Match>,
    /// Whether any errors occurred during parsing
    pub had_error: bool,
}

/// Scan a directory for files matching a pattern.
///
/// This function walks the directory tree, filters files by supported extensions,
/// and executes the pattern query on each file in parallel.
///
/// # Arguments
///
/// * `root` - Root directory to scan
/// * `pattern` - Tree-sitter query pattern (S-expression syntax)
/// * `options` - Scan options (depth, hidden files, etc.)
/// * `callback` - Called for each file with matches; return `false` to stop
///
/// # Returns
///
/// Statistics about the scan operation.
///
/// # Errors
///
/// Returns an error if the root directory cannot be read.
pub fn scan_directory<F>(
    root: &Path,
    pattern: &str,
    options: &ScanOptions,
    callback: F,
) -> Result<ScanStats, Error>
where
    F: Fn(&Path, &[query::Match]) -> bool + Sync,
{
    let start = std::time::Instant::now();

    // Atomic counters for thread-safe statistics
    let files_scanned = AtomicU64::new(0);
    let files_matched = AtomicU64::new(0);
    let total_matches = AtomicU64::new(0);
    let files_skipped = AtomicU64::new(0);
    let parse_errors = AtomicU64::new(0);
    let should_stop = AtomicBool::new(false);

    // Configure thread pool if specified
    let pool = if options.threads > 0 && options.threads != rayon::current_num_threads() as u32 {
        Some(
            rayon::ThreadPoolBuilder::new()
                .num_threads(options.threads as usize)
                .build()
                .map_err(|e| Error::internal(format!("Failed to create thread pool: {}", e)))?,
        )
    } else {
        None
    };

    // Build file list first (walkdir is not thread-safe for parallel iteration)
    let walker = WalkDir::new(root)
        .follow_links(options.follow_links)
        .max_depth(if options.max_depth > 0 {
            options.max_depth as usize
        } else {
            usize::MAX
        });

    let files: Vec<PathBuf> = walker
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            // Skip hidden files if configured
            if !options.include_hidden {
                if let Some(name) = e.file_name().to_str() {
                    if name.starts_with('.') {
                        return false;
                    }
                }
            }
            true
        })
        .filter(|e| {
            // Skip files that are too large
            if options.max_file_size > 0 {
                if let Ok(meta) = e.metadata() {
                    if meta.len() > options.max_file_size {
                        return false;
                    }
                }
            }
            true
        })
        .filter(|e| {
            // Only include files with supported extensions
            if let Some(ext) = e.path().extension() {
                if let Some(ext_str) = ext.to_str() {
                    let ext_with_dot = format!(".{}", ext_str);
                    return grammars::language_for_extension(&ext_with_dot).is_some();
                }
            }
            false
        })
        .map(|e| e.path().to_path_buf())
        .collect();

    // Process files in parallel
    let pattern = pattern.to_string();

    let process_files = || {
        files.par_iter().for_each(|path| {
            // Check if we should stop early
            if should_stop.load(Ordering::Relaxed) {
                return;
            }

            files_scanned.fetch_add(1, Ordering::Relaxed);

            // Get extension for language detection
            let ext = match path.extension().and_then(|e| e.to_str()) {
                Some(e) => format!(".{}", e),
                None => {
                    files_skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            // Create parser for this file's language
            // Each thread creates its own parser since parsers are not thread-safe
            let mut parser = match Parser::for_extension(&ext) {
                Ok(p) => p,
                Err(_) => {
                    files_skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            // Read file contents
            let source = match std::fs::read_to_string(path) {
                Ok(s) => s,
                Err(_) => {
                    files_skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            // Parse the file
            let tree = match parser.parse(&source) {
                Ok(t) => t,
                Err(_) => {
                    parse_errors.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            // Execute query
            match query::execute_collect(&tree, &pattern) {
                Ok(matches) if !matches.is_empty() => {
                    files_matched.fetch_add(1, Ordering::Relaxed);
                    total_matches.fetch_add(matches.len() as u64, Ordering::Relaxed);

                    // Call the callback
                    let should_continue = callback(path, &matches);
                    if !should_continue {
                        should_stop.store(true, Ordering::Relaxed);
                    }
                }
                Err(_) => {
                    // Query failed for this language, skip
                    // This can happen if the pattern doesn't apply to this language
                }
                _ => {
                    // No matches, nothing to report
                }
            }
        });
    };

    // Run in custom thread pool if configured, otherwise use global
    if let Some(pool) = pool {
        pool.install(process_files);
    } else {
        process_files();
    }

    Ok(ScanStats {
        files_scanned: files_scanned.load(Ordering::Relaxed),
        files_matched: files_matched.load(Ordering::Relaxed),
        total_matches: total_matches.load(Ordering::Relaxed),
        files_skipped: files_skipped.load(Ordering::Relaxed),
        parse_errors: parse_errors.load(Ordering::Relaxed),
        elapsed_ms: start.elapsed().as_millis() as u64,
    })
}

/// Scan a directory with an FFI callback.
///
/// This is an internal function that handles the raw FFI callback and user_data
/// pointer in a thread-safe manner.
fn scan_directory_ffi(
    root: &Path,
    pattern: &str,
    options: &ScanOptions,
    callback: FileMatchCallback,
    user_data: SyncUserData,
) -> Result<ScanStats, Error> {
    let start = std::time::Instant::now();

    // Extract the raw pointer - it's Copy so this is fine
    // This avoids capturing the SyncUserData wrapper in closures
    let user_data_ptr = user_data.0;

    // Atomic counters for thread-safe statistics
    let files_scanned = AtomicU64::new(0);
    let files_matched = AtomicU64::new(0);
    let total_matches = AtomicU64::new(0);
    let files_skipped = AtomicU64::new(0);
    let parse_errors = AtomicU64::new(0);
    let should_stop = AtomicBool::new(false);

    // Configure thread pool if specified
    let pool = if options.threads > 0 && options.threads != rayon::current_num_threads() as u32 {
        Some(
            rayon::ThreadPoolBuilder::new()
                .num_threads(options.threads as usize)
                .build()
                .map_err(|e| Error::internal(format!("Failed to create thread pool: {}", e)))?,
        )
    } else {
        None
    };

    // Build file list first (walkdir is not thread-safe for parallel iteration)
    let walker = WalkDir::new(root)
        .follow_links(options.follow_links)
        .max_depth(if options.max_depth > 0 {
            options.max_depth as usize
        } else {
            usize::MAX
        });

    let files: Vec<PathBuf> = walker
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            if !options.include_hidden {
                if let Some(name) = e.file_name().to_str() {
                    if name.starts_with('.') {
                        return false;
                    }
                }
            }
            true
        })
        .filter(|e| {
            if options.max_file_size > 0 {
                if let Ok(meta) = e.metadata() {
                    if meta.len() > options.max_file_size {
                        return false;
                    }
                }
            }
            true
        })
        .filter(|e| {
            if let Some(ext) = e.path().extension() {
                if let Some(ext_str) = ext.to_str() {
                    let ext_with_dot = format!(".{}", ext_str);
                    return grammars::language_for_extension(&ext_with_dot).is_some();
                }
            }
            false
        })
        .map(|e| e.path().to_path_buf())
        .collect();

    // Process files in parallel
    let pattern = pattern.to_string();

    // Convert user_data pointer to usize for thread-safe passing
    // usize is Copy + Send + Sync, so it can be safely captured by move closures
    let user_data_usize = user_data_ptr as usize;

    let process_files = || {
        files.par_iter().for_each(|path| {
            if should_stop.load(Ordering::Relaxed) {
                return;
            }

            files_scanned.fetch_add(1, Ordering::Relaxed);

            let ext = match path.extension().and_then(|e| e.to_str()) {
                Some(e) => format!(".{}", e),
                None => {
                    files_skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            let mut parser = match Parser::for_extension(&ext) {
                Ok(p) => p,
                Err(_) => {
                    files_skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            let source = match std::fs::read_to_string(path) {
                Ok(s) => s,
                Err(_) => {
                    files_skipped.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            let tree = match parser.parse(&source) {
                Ok(t) => t,
                Err(_) => {
                    parse_errors.fetch_add(1, Ordering::Relaxed);
                    return;
                }
            };

            match query::execute_collect(&tree, &pattern) {
                Ok(matches) if !matches.is_empty() => {
                    files_matched.fetch_add(1, Ordering::Relaxed);
                    total_matches.fetch_add(matches.len() as u64, Ordering::Relaxed);

                    // Convert path to string for FFI
                    let path_str = path.to_string_lossy();
                    let path_bytes = path_str.as_bytes();

                    let first_line = matches
                        .first()
                        .and_then(|m| m.captures.first())
                        .map(|c| c.start_row + 1)
                        .unwrap_or(0);

                    let file_match = FileMatch {
                        file_path: path_bytes.as_ptr() as *const c_char,
                        file_path_len: path_bytes.len() as u32,
                        match_count: matches.len() as u32,
                        first_match_line: first_line,
                    };

                    // Call the C callback - convert usize back to pointer
                    if !callback(&file_match, user_data_usize as *mut c_void) {
                        should_stop.store(true, Ordering::Relaxed);
                    }
                }
                Err(_) => {}
                _ => {}
            }
        });
    };

    if let Some(pool) = pool {
        pool.install(process_files);
    } else {
        process_files();
    }

    Ok(ScanStats {
        files_scanned: files_scanned.load(Ordering::Relaxed),
        files_matched: files_matched.load(Ordering::Relaxed),
        total_matches: total_matches.load(Ordering::Relaxed),
        files_skipped: files_skipped.load(Ordering::Relaxed),
        parse_errors: parse_errors.load(Ordering::Relaxed),
        elapsed_ms: start.elapsed().as_millis() as u64,
    })
}

/// Scan a directory and collect all results.
///
/// This is a convenience function that collects all matches into a vector
/// rather than processing them via callback.
///
/// # Arguments
///
/// * `root` - Root directory to scan
/// * `pattern` - Tree-sitter query pattern
/// * `options` - Scan options
///
/// # Returns
///
/// A tuple of (results, statistics).
pub fn scan_directory_collect(
    root: &Path,
    pattern: &str,
    options: &ScanOptions,
) -> Result<(Vec<FileScanResult>, ScanStats), Error> {
    let results: Mutex<Vec<FileScanResult>> = Mutex::new(Vec::new());

    let stats = scan_directory(root, pattern, options, |path, matches| {
        results.lock().push(FileScanResult {
            path: path.to_path_buf(),
            matches: matches.to_vec(),
            had_error: false,
        });
        true // continue scanning
    })?;

    Ok((results.into_inner(), stats))
}

// ============================================================================
// FFI Exports
// ============================================================================

/// Get default scan options.
///
/// Returns a `ScanOptions` struct with sensible defaults.
#[unsafe(no_mangle)]
pub extern "C" fn uast_scan_options_default() -> ScanOptions {
    ScanOptions::default()
}

/// Scan a directory for files matching a pattern.
///
/// # Arguments
///
/// * `root` - Root directory path (UTF-8, not null-terminated)
/// * `root_len` - Length of root path in bytes
/// * `pattern` - Tree-sitter query pattern (UTF-8, not null-terminated)
/// * `pattern_len` - Length of pattern in bytes
/// * `options` - Scan options (or null for defaults)
/// * `callback` - Called for each file with matches (or null to just count)
/// * `user_data` - Passed to callback
/// * `stats_out` - Receives scan statistics (or null to ignore)
///
/// # Returns
///
/// `UastResult::Ok` on success, error code otherwise.
///
/// # Safety
///
/// - `root` must be a valid pointer to `root_len` bytes of UTF-8
/// - `pattern` must be a valid pointer to `pattern_len` bytes of UTF-8
/// - `options` must be null or a valid pointer to `ScanOptions`
/// - `callback` must be null or a valid function pointer
/// - `stats_out` must be null or a valid pointer to `ScanStats`
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scan_directory(
    root: *const c_char,
    root_len: u32,
    pattern: *const c_char,
    pattern_len: u32,
    options: *const ScanOptions,
    callback: Option<FileMatchCallback>,
    user_data: *mut c_void,
    stats_out: *mut ScanStats,
) -> UastResult {
    // Parse root path
    let root_str = match unsafe { cstr_to_str_len(root, root_len) } {
        Ok(s) => s,
        Err(e) => return e,
    };

    // Parse pattern
    let pattern_str = match unsafe { cstr_to_str_len(pattern, pattern_len) } {
        Ok(s) => s,
        Err(e) => return e,
    };

    // Get options (use defaults if null)
    let options = if options.is_null() {
        ScanOptions::default()
    } else {
        unsafe { (*options).clone() }
    };

    // Wrap user_data in thread-safe wrapper
    let user_data_sync = SyncUserData(user_data);

    // Perform the scan
    let result = if let Some(cb) = callback {
        // With callback - use scan_directory_ffi to handle the raw pointer
        scan_directory_ffi(Path::new(root_str), pattern_str, &options, cb, user_data_sync)
    } else {
        // No callback, just count
        scan_directory(Path::new(root_str), pattern_str, &options, |_, _| true)
    };

    match result {
        Ok(stats) => {
            if !stats_out.is_null() {
                unsafe {
                    *stats_out = stats;
                }
            }
            UastResult::Ok
        }
        Err(_) => UastResult::InternalError,
    }
}

/// Scan a directory with language filtering.
///
/// Only scans files matching the specified language.
///
/// # Arguments
///
/// * `root` - Root directory path
/// * `root_len` - Length of root path
/// * `pattern` - Tree-sitter query pattern
/// * `pattern_len` - Length of pattern
/// * `language` - Language name to filter by (e.g., "javascript")
/// * `language_len` - Length of language name
/// * `options` - Scan options (or null for defaults)
/// * `callback` - Called for each file with matches
/// * `user_data` - Passed to callback
/// * `stats_out` - Receives scan statistics
///
/// # Safety
///
/// Same as `uast_scan_directory`.
#[unsafe(no_mangle)]
pub unsafe extern "C" fn uast_scan_directory_language(
    root: *const c_char,
    root_len: u32,
    pattern: *const c_char,
    pattern_len: u32,
    language: *const c_char,
    language_len: u32,
    options: *const ScanOptions,
    callback: Option<FileMatchCallback>,
    user_data: *mut c_void,
    stats_out: *mut ScanStats,
) -> UastResult {
    use crate::error;

    error::clear_last_error();

    // Parse all string arguments
    let root_str = match unsafe { cstr_to_str_len(root, root_len) } {
        Ok(s) => s,
        Err(e) => return e,
    };

    let pattern_str = match unsafe { cstr_to_str_len(pattern, pattern_len) } {
        Ok(s) => s,
        Err(e) => return e,
    };

    let language_str = match unsafe { cstr_to_str_len(language, language_len) } {
        Ok(s) => s,
        Err(e) => return e,
    };

    // Verify language is registered
    if !grammars::has_language(language_str) {
        error::set_last_error(Error::unknown_language(language_str));
        return UastResult::UnknownLanguage;
    }

    // Get options
    let options = if options.is_null() {
        ScanOptions::default()
    } else {
        unsafe { (*options).clone() }
    };

    // Wrap user_data in thread-safe wrapper
    let user_data_sync = SyncUserData(user_data);

    // Use the language-specific scan helper
    let result = scan_directory_language_ffi(
        Path::new(root_str),
        pattern_str,
        language_str,
        &options,
        callback,
        user_data_sync,
    );

    match result {
        Ok(stats) => {
            if !stats_out.is_null() {
                unsafe {
                    *stats_out = stats;
                }
            }
            UastResult::Ok
        }
        Err(_) => UastResult::InternalError,
    }
}

/// Internal helper for language-filtered FFI scanning.
fn scan_directory_language_ffi(
    root: &Path,
    pattern: &str,
    language: &str,
    options: &ScanOptions,
    callback: Option<FileMatchCallback>,
    user_data: SyncUserData,
) -> Result<ScanStats, Error> {
    let start = std::time::Instant::now();

    // Extract the raw pointer - it's Copy so this is fine
    let user_data_ptr = user_data.0;

    // Atomic counters
    let files_scanned = AtomicU64::new(0);
    let files_matched = AtomicU64::new(0);
    let total_matches = AtomicU64::new(0);
    let files_skipped = AtomicU64::new(0);
    let parse_errors = AtomicU64::new(0);
    let should_stop = AtomicBool::new(false);

    // Build file list, filtering by language
    let walker = WalkDir::new(root)
        .follow_links(options.follow_links)
        .max_depth(if options.max_depth > 0 {
            options.max_depth as usize
        } else {
            usize::MAX
        });

    let files: Vec<PathBuf> = walker
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.file_type().is_file())
        .filter(|e| {
            if !options.include_hidden {
                if let Some(name) = e.file_name().to_str() {
                    if name.starts_with('.') {
                        return false;
                    }
                }
            }
            true
        })
        .filter(|e| {
            if options.max_file_size > 0 {
                if let Ok(meta) = e.metadata() {
                    if meta.len() > options.max_file_size {
                        return false;
                    }
                }
            }
            true
        })
        .filter(|e| {
            // Only include files with matching language
            if let Some(ext) = e.path().extension() {
                if let Some(ext_str) = ext.to_str() {
                    let ext_with_dot = format!(".{}", ext_str);
                    if let Some(lang_name) = grammars::language_name_for_extension(&ext_with_dot) {
                        return lang_name.eq_ignore_ascii_case(language);
                    }
                }
            }
            false
        })
        .map(|e| e.path().to_path_buf())
        .collect();

    // Process files in parallel
    let pattern = pattern.to_string();

    // Convert user_data pointer to usize for thread-safe passing
    let user_data_usize = user_data_ptr as usize;

    files.par_iter().for_each(|path| {
        if should_stop.load(Ordering::Relaxed) {
            return;
        }

        files_scanned.fetch_add(1, Ordering::Relaxed);

        let ext = match path.extension().and_then(|e| e.to_str()) {
            Some(e) => format!(".{}", e),
            None => {
                files_skipped.fetch_add(1, Ordering::Relaxed);
                return;
            }
        };

        let mut parser = match Parser::for_extension(&ext) {
            Ok(p) => p,
            Err(_) => {
                files_skipped.fetch_add(1, Ordering::Relaxed);
                return;
            }
        };

        let source = match std::fs::read_to_string(path) {
            Ok(s) => s,
            Err(_) => {
                files_skipped.fetch_add(1, Ordering::Relaxed);
                return;
            }
        };

        let tree = match parser.parse(&source) {
            Ok(t) => t,
            Err(_) => {
                parse_errors.fetch_add(1, Ordering::Relaxed);
                return;
            }
        };

        match query::execute_collect(&tree, &pattern) {
            Ok(matches) if !matches.is_empty() => {
                files_matched.fetch_add(1, Ordering::Relaxed);
                total_matches.fetch_add(matches.len() as u64, Ordering::Relaxed);

                if let Some(cb) = callback {
                    let path_str = path.to_string_lossy();
                    let path_bytes = path_str.as_bytes();

                    let first_line = matches
                        .first()
                        .and_then(|m| m.captures.first())
                        .map(|c| c.start_row + 1)
                        .unwrap_or(0);

                    let file_match = FileMatch {
                        file_path: path_bytes.as_ptr() as *const c_char,
                        file_path_len: path_bytes.len() as u32,
                        match_count: matches.len() as u32,
                        first_match_line: first_line,
                    };

                    if !cb(&file_match, user_data_usize as *mut c_void) {
                        should_stop.store(true, Ordering::Relaxed);
                    }
                }
            }
            _ => {}
        }
    });

    Ok(ScanStats {
        files_scanned: files_scanned.load(Ordering::Relaxed),
        files_matched: files_matched.load(Ordering::Relaxed),
        total_matches: total_matches.load(Ordering::Relaxed),
        files_skipped: files_skipped.load(Ordering::Relaxed),
        parse_errors: parse_errors.load(Ordering::Relaxed),
        elapsed_ms: start.elapsed().as_millis() as u64,
    })
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_options_default() {
        let opts = ScanOptions::default();
        assert_eq!(opts.max_depth, 0);
        assert!(!opts.follow_links);
        assert!(!opts.include_hidden);
        assert_eq!(opts.max_file_size, 10 * 1024 * 1024);
        assert_eq!(opts.threads, 0);
    }

    #[test]
    fn test_scan_stats_default() {
        let stats = ScanStats::default();
        assert_eq!(stats.files_scanned, 0);
        assert_eq!(stats.files_matched, 0);
        assert_eq!(stats.total_matches, 0);
        assert_eq!(stats.files_skipped, 0);
        assert_eq!(stats.parse_errors, 0);
        assert_eq!(stats.elapsed_ms, 0);
    }

    #[test]
    fn test_file_match_default() {
        let fm = FileMatch::default();
        assert!(fm.file_path.is_null());
        assert_eq!(fm.file_path_len, 0);
        assert_eq!(fm.match_count, 0);
        assert_eq!(fm.first_match_line, 0);
    }

    #[test]
    fn test_ffi_scan_options_default() {
        let opts = uast_scan_options_default();
        assert_eq!(opts.max_depth, 0);
        assert!(!opts.follow_links);
        assert!(!opts.include_hidden);
        assert_eq!(opts.max_file_size, 10 * 1024 * 1024);
        assert_eq!(opts.threads, 0);
    }

    #[test]
    fn test_scan_nonexistent_directory() {
        let opts = ScanOptions::default();
        let result = scan_directory(
            Path::new("/nonexistent/path/that/should/not/exist/xyz123"),
            "(identifier)",
            &opts,
            |_, _| true,
        );
        // Should succeed but find no files
        assert!(result.is_ok());
        let stats = result.unwrap();
        assert_eq!(stats.files_scanned, 0);
    }

    #[test]
    fn test_scan_empty_pattern() {
        let opts = ScanOptions::default();
        // Empty pattern should still work (returns no matches)
        let result = scan_directory(Path::new("."), "", &opts, |_, _| true);
        // May fail due to invalid pattern, which is acceptable
        // Just verify it doesn't panic
        let _ = result;
    }

    #[test]
    fn test_scan_options_clone() {
        let opts1 = ScanOptions {
            max_depth: 5,
            follow_links: true,
            include_hidden: true,
            max_file_size: 1024,
            threads: 4,
        };
        let opts2 = opts1.clone();
        assert_eq!(opts1.max_depth, opts2.max_depth);
        assert_eq!(opts1.follow_links, opts2.follow_links);
        assert_eq!(opts1.include_hidden, opts2.include_hidden);
        assert_eq!(opts1.max_file_size, opts2.max_file_size);
        assert_eq!(opts1.threads, opts2.threads);
    }

    #[test]
    fn test_scan_stats_clone() {
        let stats1 = ScanStats {
            files_scanned: 100,
            files_matched: 10,
            total_matches: 50,
            files_skipped: 5,
            parse_errors: 2,
            elapsed_ms: 1234,
        };
        let stats2 = stats1.clone();
        assert_eq!(stats1.files_scanned, stats2.files_scanned);
        assert_eq!(stats1.files_matched, stats2.files_matched);
        assert_eq!(stats1.total_matches, stats2.total_matches);
        assert_eq!(stats1.files_skipped, stats2.files_skipped);
        assert_eq!(stats1.parse_errors, stats2.parse_errors);
        assert_eq!(stats1.elapsed_ms, stats2.elapsed_ms);
    }
}
