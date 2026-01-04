//! Native DLL grammar loader for languages that cannot compile to WASM.
//!
//! Some grammars (doxygen, vim, cobol) cannot be compiled to WASM due to
//! external scanner complexity or other limitations. This module provides
//! native DLL loading as a fallback tier.
//!
//! # Loading Priority
//!
//! 1. Built-in (Tier 1+2) - compiled into binary
//! 2. WASM (Tier 3) - loaded from .wasm files
//! 3. Native DLL (Tier 3b) - loaded from native libraries as fallback
//!
//! # Directory Search Order
//!
//! 1. `grammars-native/` next to the executable
//! 2. `$UAST_NATIVE_PATH` environment variable
//! 3. User's `.uast/grammars-native/` directory
//!
//! # Platform-Specific Extensions
//!
//! - Windows: `.dll`
//! - Linux: `.so`
//! - macOS: `.dylib`
//!
//! # DLL Export Convention
//!
//! Each grammar DLL exports a function `tree_sitter_{name}` that returns
//! a `*const TSLanguage` pointer.

use libloading::{Library, Symbol};
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};
use tree_sitter::ffi::TSLanguage;
use tree_sitter::Language;

use crate::error::Error;

// ============================================================================
// Configuration
// ============================================================================

/// Get the platform-specific library extension.
#[cfg(target_os = "windows")]
const NATIVE_LIB_EXT: &str = "dll";

#[cfg(target_os = "linux")]
const NATIVE_LIB_EXT: &str = "so";

#[cfg(target_os = "macos")]
const NATIVE_LIB_EXT: &str = "dylib";

#[cfg(not(any(target_os = "windows", target_os = "linux", target_os = "macos")))]
const NATIVE_LIB_EXT: &str = "so"; // Default to .so for other Unix-like systems

/// Languages that are known to require native loading (WASM incompatible).
/// These are prioritized for native DLL search.
pub const NATIVE_PREFERRED_LANGUAGES: &[&str] = &[
    "doxygen",
    "vim",
    "cobol",
];

// ============================================================================
// Caching
// ============================================================================

/// Type alias for the tree-sitter language entry point function.
type LanguageEntryFn = unsafe extern "C" fn() -> *const TSLanguage;

/// Cache of loaded libraries to keep them alive.
/// Libraries must stay loaded while their Languages are in use.
static LOADED_LIBRARIES: Lazy<RwLock<HashMap<String, Box<Library>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Cache of loaded native languages (name -> Language).
static NATIVE_CACHE: Lazy<RwLock<HashMap<String, Language>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Native loading statistics.
static NATIVE_STATS: Lazy<RwLock<NativeLoadStats>> =
    Lazy::new(|| RwLock::new(NativeLoadStats::default()));

/// Statistics for native grammar loading.
#[derive(Default, Clone)]
pub struct NativeLoadStats {
    pub loads_attempted: usize,
    pub loads_succeeded: usize,
    pub loads_failed: usize,
    pub cache_hits: usize,
}

// ============================================================================
// Public API
// ============================================================================

/// Load a native grammar by name.
///
/// This function will:
/// 1. Check the in-memory cache
/// 2. Search local directories for native library
/// 3. Load and cache the result
///
/// # Example
///
/// ```ignore
/// if let Some(lang) = load_native_grammar("doxygen") {
///     let mut parser = Parser::new();
///     parser.set_language(&lang).unwrap();
/// }
/// ```
pub fn load_native_grammar(name: &str) -> Option<Language> {
    let name_lower = name.to_lowercase();
    let canonical = crate::tiers::normalize_name_pub(&name_lower);
    let name_to_use = if canonical.is_empty() { &name_lower } else { canonical };

    // Check in-memory cache first (instant)
    {
        let cache = NATIVE_CACHE.read();
        if let Some(lang) = cache.get(name_to_use) {
            if let Some(mut stats) = NATIVE_STATS.try_write() {
                stats.cache_hits += 1;
            }
            return Some(lang.clone());
        }
    }

    // Update stats
    if let Some(mut stats) = NATIVE_STATS.try_write() {
        stats.loads_attempted += 1;
    }

    // Try to find and load the native library
    let lib_path = find_native_library(name_to_use)?;

    match load_native_library(name_to_use, &lib_path) {
        Ok(language) => {
            // Cache the language
            {
                let mut cache = NATIVE_CACHE.write();
                cache.insert(name_to_use.to_string(), language.clone());
            }

            // Update stats
            if let Some(mut stats) = NATIVE_STATS.try_write() {
                stats.loads_succeeded += 1;
            }

            Some(language)
        }
        Err(e) => {
            eprintln!("UAST: Failed to load native grammar '{}': {}", name, e);
            if let Some(mut stats) = NATIVE_STATS.try_write() {
                stats.loads_failed += 1;
            }
            None
        }
    }
}

/// Check if a native grammar is available (without loading it).
pub fn is_native_available(name: &str) -> bool {
    find_native_library(&name.to_lowercase()).is_some()
}

/// Check if a language is known to prefer native loading.
pub fn prefers_native(name: &str) -> bool {
    let name_lower = name.to_lowercase();
    let canonical = crate::tiers::normalize_name_pub(&name_lower);
    let name_to_use = if canonical.is_empty() { &name_lower } else { canonical };

    NATIVE_PREFERRED_LANGUAGES.iter().any(|&l| l == name_to_use)
}

/// Get native loading statistics.
pub fn get_native_stats() -> NativeLoadStats {
    NATIVE_STATS.read().clone()
}

/// Reset native loading statistics.
pub fn reset_native_stats() {
    if let Some(mut stats) = NATIVE_STATS.try_write() {
        *stats = NativeLoadStats::default();
    }
}

/// Clear the native grammar cache.
pub fn clear_native_cache() {
    {
        let mut cache = NATIVE_CACHE.write();
        cache.clear();
    }
    {
        let mut libs = LOADED_LIBRARIES.write();
        libs.clear();
    }
}

/// Get cache statistics.
pub fn native_cache_stats() -> (usize, Vec<String>) {
    let cache = NATIVE_CACHE.read();
    let count = cache.len();
    let names: Vec<String> = cache.keys().cloned().collect();
    (count, names)
}

/// List all available native grammars (locally present).
pub fn available_native_grammars() -> Vec<String> {
    let mut grammars = Vec::new();

    for search_dir in native_search_paths() {
        if let Ok(entries) = std::fs::read_dir(&search_dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                let path = entry.path();
                if path.extension().map(|e| e == NATIVE_LIB_EXT).unwrap_or(false) {
                    if let Some(name) = extract_language_name_from_lib(&path) {
                        if !grammars.contains(&name) {
                            grammars.push(name);
                        }
                    }
                }
            }
        }
    }

    grammars.sort();
    grammars
}

// ============================================================================
// Internal Functions
// ============================================================================

/// Find the native library file in local directories.
fn find_native_library(name: &str) -> Option<PathBuf> {
    let lib_names = get_library_names(name);

    for base_path in native_search_paths() {
        for lib_name in &lib_names {
            let lib_path = base_path.join(lib_name);
            if lib_path.exists() {
                return Some(lib_path);
            }
        }
    }

    None
}

/// Get possible library file names for a grammar.
/// Handles variations in naming conventions.
fn get_library_names(name: &str) -> Vec<String> {
    let name_underscore = name.replace('-', "_");
    let name_hyphen = name.replace('_', "-");

    vec![
        // Standard tree-sitter naming: tree-sitter-{name}.{ext}
        format!("tree-sitter-{}.{}", name, NATIVE_LIB_EXT),
        format!("tree-sitter-{}.{}", name_underscore, NATIVE_LIB_EXT),
        format!("tree-sitter-{}.{}", name_hyphen, NATIVE_LIB_EXT),
        // Alternative: tree_sitter_{name}.{ext}
        format!("tree_sitter_{}.{}", name, NATIVE_LIB_EXT),
        format!("tree_sitter_{}.{}", name_underscore, NATIVE_LIB_EXT),
        // Linux/macOS may prefix with lib
        #[cfg(not(target_os = "windows"))]
        format!("libtree-sitter-{}.{}", name, NATIVE_LIB_EXT),
        #[cfg(not(target_os = "windows"))]
        format!("libtree_sitter_{}.{}", name_underscore, NATIVE_LIB_EXT),
    ]
}

/// Load a native library and get the language.
fn load_native_library(name: &str, lib_path: &Path) -> Result<Language, Error> {
    // Normalize name for entry point
    let entry_name = normalize_entry_point_name(name);
    let symbol_name = format!("tree_sitter_{}", entry_name);

    // Check if already loaded
    {
        let libs = LOADED_LIBRARIES.read();
        if libs.contains_key(name) {
            let lib = libs.get(name).unwrap();
            unsafe {
                let func: Symbol<LanguageEntryFn> = lib.get(symbol_name.as_bytes()).map_err(|e| {
                    Error::internal(format!(
                        "Failed to get symbol '{}' from cached library: {}",
                        symbol_name, e
                    ))
                })?;
                let lang_ptr = func();
                if lang_ptr.is_null() {
                    return Err(Error::internal(format!(
                        "Language entry point '{}' returned null",
                        symbol_name
                    )));
                }
                return Ok(Language::from_raw(lang_ptr));
            }
        }
    }

    // Load the library
    let lib = unsafe {
        Library::new(lib_path).map_err(|e| {
            Error::internal(format!(
                "Failed to load native grammar library '{}': {}",
                lib_path.display(),
                e
            ))
        })?
    };

    // Get the entry point function
    let language = unsafe {
        let func: Symbol<LanguageEntryFn> = lib.get(symbol_name.as_bytes()).map_err(|e| {
            Error::internal(format!(
                "Failed to find entry point '{}' in '{}': {}",
                symbol_name,
                lib_path.display(),
                e
            ))
        })?;

        let lang_ptr = func();
        if lang_ptr.is_null() {
            return Err(Error::internal(format!(
                "Language entry point '{}' returned null pointer",
                symbol_name
            )));
        }

        Language::from_raw(lang_ptr)
    };

    // Validate the language
    if language.node_kind_count() == 0 {
        return Err(Error::internal(format!(
            "Loaded language '{}' appears invalid (no node kinds)",
            name
        )));
    }

    // Store the library to keep it alive
    {
        let mut libs = LOADED_LIBRARIES.write();
        libs.insert(name.to_lowercase(), Box::new(lib));
    }

    Ok(language)
}

/// Normalize a language name to its entry point function name.
fn normalize_entry_point_name(name: &str) -> String {
    name.replace('-', "_")
}

/// Extract language name from a library filename.
fn extract_language_name_from_lib(path: &Path) -> Option<String> {
    let filename = path.file_name()?.to_str()?;

    // Try tree-sitter-{name}.{ext} format
    let without_ext = filename.strip_suffix(&format!(".{}", NATIVE_LIB_EXT))?;

    // Handle lib prefix on Unix
    let without_prefix = without_ext
        .strip_prefix("libtree-sitter-")
        .or_else(|| without_ext.strip_prefix("libtree_sitter_"))
        .or_else(|| without_ext.strip_prefix("tree-sitter-"))
        .or_else(|| without_ext.strip_prefix("tree_sitter_"))?;

    if without_prefix.is_empty() {
        return None;
    }

    Some(without_prefix.to_string())
}

/// Get all native library search paths.
fn native_search_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // 1. Next to executable in grammars-native/
    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            paths.push(exe_dir.join("grammars-native"));
        }
    }

    // 2. Environment variable
    if let Ok(native_path) = env::var("UAST_NATIVE_PATH") {
        for part in native_path.split(if cfg!(windows) { ';' } else { ':' }) {
            paths.push(PathBuf::from(part));
        }
    }

    // 3. User cache directory
    if let Some(home) = dirs_next_home() {
        paths.push(home.join(".uast").join("grammars-native"));
    }

    // 4. Working directory
    paths.push(PathBuf::from("grammars-native"));

    paths
}

/// Get the user's home directory (cross-platform).
fn dirs_next_home() -> Option<PathBuf> {
    #[cfg(windows)]
    {
        env::var("USERPROFILE").ok().map(PathBuf::from)
    }
    #[cfg(not(windows))]
    {
        env::var("HOME").ok().map(PathBuf::from)
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_native_search_paths_not_empty() {
        let paths = native_search_paths();
        assert!(!paths.is_empty(), "Should have at least one search path");
    }

    #[test]
    fn test_get_library_names() {
        let names = get_library_names("doxygen");
        assert!(!names.is_empty());
        assert!(names.iter().any(|n| n.contains("doxygen")));
    }

    #[test]
    fn test_normalize_entry_point_name() {
        assert_eq!(normalize_entry_point_name("c-sharp"), "c_sharp");
        assert_eq!(normalize_entry_point_name("common-lisp"), "common_lisp");
        assert_eq!(normalize_entry_point_name("vim"), "vim");
    }

    #[test]
    fn test_prefers_native() {
        assert!(prefers_native("doxygen"));
        assert!(prefers_native("vim"));
        assert!(prefers_native("cobol"));
        assert!(!prefers_native("javascript")); // Built-in
        assert!(!prefers_native("sql")); // WASM
    }

    #[test]
    fn test_extract_language_name() {
        #[cfg(target_os = "windows")]
        {
            assert_eq!(
                extract_language_name_from_lib(Path::new("tree-sitter-doxygen.dll")),
                Some("doxygen".to_string())
            );
            assert_eq!(
                extract_language_name_from_lib(Path::new("tree_sitter_vim.dll")),
                Some("vim".to_string())
            );
        }

        #[cfg(target_os = "linux")]
        {
            assert_eq!(
                extract_language_name_from_lib(Path::new("libtree-sitter-doxygen.so")),
                Some("doxygen".to_string())
            );
        }
    }

    #[test]
    fn test_stats_initial() {
        reset_native_stats();
        let stats = get_native_stats();
        assert_eq!(stats.loads_attempted, 0);
        assert_eq!(stats.cache_hits, 0);
    }

    #[test]
    fn test_is_native_available_nonexistent() {
        assert!(!is_native_available("definitely_not_a_real_grammar_xyz123"));
    }

    #[test]
    fn test_available_native_grammars_returns_vec() {
        let grammars = available_native_grammars();
        let _ = grammars.len(); // Just verify it doesn't panic
    }
}
