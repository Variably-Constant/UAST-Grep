//! WASM grammar loader for Tier 3 languages.
//!
//! Loads tree-sitter grammar WASM files from disk or downloads them on-demand
//! from GitHub Releases. Downloaded grammars are cached locally for future use.
//!
//! # Loading Priority
//!
//! 1. Check in-memory cache (instant)
//! 2. Check local file system (fast)
//! 3. Download from GitHub Releases (first-time only, then cached)
//!
//! # Directory Search Order (Local)
//!
//! 1. `grammars-wasm/` next to the executable
//! 2. `$UAST_WASM_PATH` environment variable
//! 3. User's `.uast-grep/grammars/` directory (also used for cache)
//!
//! # Environment Variables
//!
//! - `UAST_WASM_PATH` - Custom directory for WASM grammars
//! - `UAST_OFFLINE` - Set to "1" to disable downloading (local only)
//! - `UAST_WASM_URL` - Override the GitHub Releases base URL

use std::collections::HashMap;
use std::env;
use std::fs;
#[cfg(feature = "wasm-download")]
use std::io::Read;
use std::path::PathBuf;
use std::sync::RwLock;

use once_cell::sync::Lazy;
use tree_sitter::Language;

// ============================================================================
// Configuration
// ============================================================================

/// GitHub repository for WASM grammar releases.
/// WASM grammars are bundled in the same release as the binaries.
/// Format: https://github.com/{owner}/{repo}/releases/download/{version}/{grammar}.wasm
///
/// To override, set UAST_WASM_URL environment variable.
const GITHUB_OWNER: &str = "MarkusMcNugen";
const GITHUB_REPO: &str = "UAST-Grep";
const GRAMMAR_VERSION: &str = "v1.0.0";  // Update with each release

/// Construct the download URL for a grammar.
fn get_download_url(grammar_name: &str) -> String {
    // Check for URL override
    if let Ok(custom_url) = env::var("UAST_WASM_URL") {
        return format!("{}/{}.wasm", custom_url.trim_end_matches('/'), grammar_name);
    }

    format!(
        "https://github.com/{}/{}/releases/download/{}/{}.wasm",
        GITHUB_OWNER, GITHUB_REPO, GRAMMAR_VERSION, grammar_name
    )
}

// ============================================================================
// Caching
// ============================================================================

/// Cache of loaded WASM languages (name -> Language)
static WASM_CACHE: Lazy<RwLock<HashMap<String, Language>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// WASM engine for tree-sitter (required for WASM language loading)
/// Uses tree-sitter's re-exported wasmtime Engine
#[cfg(feature = "wasm")]
static WASM_ENGINE: Lazy<tree_sitter::wasmtime::Engine> =
    Lazy::new(|| tree_sitter::wasmtime::Engine::default());

/// WASM store for tree-sitter (required for WASM language loading)
#[cfg(feature = "wasm")]
static WASM_STORE: Lazy<RwLock<Option<tree_sitter::WasmStore>>> =
    Lazy::new(|| RwLock::new(None));

/// Download statistics
static DOWNLOAD_STATS: Lazy<RwLock<DownloadStats>> =
    Lazy::new(|| RwLock::new(DownloadStats::default()));

#[derive(Default, Clone)]
pub struct DownloadStats {
    pub downloads_attempted: usize,
    pub downloads_succeeded: usize,
    pub downloads_failed: usize,
    pub bytes_downloaded: usize,
    pub cache_hits: usize,
}

// ============================================================================
// Public API
// ============================================================================

/// Load a WASM grammar by name.
///
/// This function will:
/// 1. Check the in-memory cache
/// 2. Search local directories
/// 3. Download from GitHub Releases (if not offline)
/// 4. Cache the result for future use
///
/// # Example
///
/// ```ignore
/// if let Some(lang) = load_wasm_grammar("sql") {
///     let mut parser = Parser::new();
///     parser.set_language(&lang).unwrap();
/// }
/// ```
pub fn load_wasm_grammar(name: &str) -> Option<Language> {
    let name_lower = name.to_lowercase();
    let canonical = crate::tiers::normalize_name_pub(&name_lower);
    let name_to_use = if canonical.is_empty() { &name_lower } else { canonical };

    // Check in-memory cache first (instant)
    {
        let cache = WASM_CACHE.read().ok()?;
        if let Some(lang) = cache.get(name_to_use) {
            if let Ok(mut stats) = DOWNLOAD_STATS.write() {
                stats.cache_hits += 1;
            }
            return Some(lang.clone());
        }
    }

    // Try to find locally first
    let wasm_bytes = if let Some(local_path) = find_local_wasm_file(name_to_use) {
        fs::read(&local_path).ok()?
    } else {
        // Not found locally - try to download
        download_wasm_grammar(name_to_use)?
    };

    // Load the WASM grammar
    let language = load_wasm_bytes(name_to_use, &wasm_bytes)?;

    // Cache it in memory
    {
        let mut cache = WASM_CACHE.write().ok()?;
        cache.insert(name_to_use.to_string(), language.clone());
    }

    Some(language)
}

/// Check if a WASM grammar is available (locally or can be downloaded).
pub fn is_wasm_available(name: &str) -> bool {
    let name_lower = name.to_lowercase();

    // Check if it's a known WASM language
    if crate::tiers::is_wasm(&name_lower) {
        return true;
    }

    // Or if a local file exists
    find_local_wasm_file(&name_lower).is_some()
}

/// Check if a grammar is cached locally (no download needed).
pub fn is_locally_cached(name: &str) -> bool {
    find_local_wasm_file(&name.to_lowercase()).is_some()
}

/// Get the cache directory path.
pub fn get_cache_dir() -> Option<PathBuf> {
    if let Some(home) = dirs_next_home() {
        let cache_dir = home.join(".uast").join("grammars");
        return Some(cache_dir);
    }
    None
}

/// Ensure the cache directory exists.
pub fn ensure_cache_dir() -> Option<PathBuf> {
    let cache_dir = get_cache_dir()?;
    fs::create_dir_all(&cache_dir).ok()?;
    Some(cache_dir)
}

/// Get download statistics.
pub fn get_download_stats() -> DownloadStats {
    DOWNLOAD_STATS.read().map(|s| s.clone()).unwrap_or_default()
}

/// Reset download statistics.
pub fn reset_download_stats() {
    if let Ok(mut stats) = DOWNLOAD_STATS.write() {
        *stats = DownloadStats::default();
    }
}

/// Pre-download a grammar to the local cache (without loading).
/// Useful for offline preparation.
pub fn prefetch_grammar(name: &str) -> Result<PathBuf, String> {
    let name_lower = name.to_lowercase();

    // Already cached?
    if let Some(path) = find_local_wasm_file(&name_lower) {
        return Ok(path);
    }

    // Download it
    let bytes = download_wasm_grammar(&name_lower)
        .ok_or_else(|| format!("Failed to download grammar: {}", name))?;

    // Save to cache
    let cache_dir = ensure_cache_dir()
        .ok_or_else(|| "Failed to create cache directory".to_string())?;
    let cache_path = cache_dir.join(format!("{}.wasm", name_lower));

    fs::write(&cache_path, &bytes)
        .map_err(|e| format!("Failed to write cache file: {}", e))?;

    Ok(cache_path)
}

/// Pre-download all WASM grammars to the local cache.
pub fn prefetch_all_grammars() -> Vec<Result<String, String>> {
    crate::tiers::WASM_LANGUAGES
        .iter()
        .map(|&name| {
            prefetch_grammar(name)
                .map(|_| name.to_string())
                .map_err(|e| format!("{}: {}", name, e))
        })
        .collect()
}

// ============================================================================
// Local File Search
// ============================================================================

/// Find the WASM file in local directories (not downloading).
fn find_local_wasm_file(name: &str) -> Option<PathBuf> {
    let filename = format!("{}.wasm", name);

    // 1. Check next to executable
    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            let path = exe_dir.join("grammars-wasm").join(&filename);
            if path.exists() {
                return Some(path);
            }
        }
    }

    // 2. Check UAST_WASM_PATH environment variable
    if let Ok(wasm_path) = env::var("UAST_WASM_PATH") {
        let path = PathBuf::from(&wasm_path).join(&filename);
        if path.exists() {
            return Some(path);
        }
    }

    // 3. Check user's cache directory
    if let Some(cache_dir) = get_cache_dir() {
        let path = cache_dir.join(&filename);
        if path.exists() {
            return Some(path);
        }
    }

    // 4. Check working directory
    let path = PathBuf::from("grammars-wasm").join(&filename);
    if path.exists() {
        return Some(path);
    }

    None
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
// Downloading
// ============================================================================

/// Download a WASM grammar from GitHub Releases.
#[cfg(feature = "wasm-download")]
fn download_wasm_grammar(name: &str) -> Option<Vec<u8>> {
    // Check if offline mode is enabled
    if env::var("UAST_OFFLINE").map(|v| v == "1").unwrap_or(false) {
        eprintln!("UAST: Offline mode - skipping download for '{}'", name);
        return None;
    }

    // Check if it's a known WASM language
    if !crate::tiers::is_wasm(name) {
        return None;
    }

    let url = get_download_url(name);
    eprintln!("UAST: Downloading grammar '{}' from {}", name, url);

    // Update stats
    if let Ok(mut stats) = DOWNLOAD_STATS.write() {
        stats.downloads_attempted += 1;
    }

    // Perform the download
    let response = match ureq::get(&url)
        .timeout(std::time::Duration::from_secs(30))
        .call()
    {
        Ok(resp) => resp,
        Err(e) => {
            eprintln!("UAST: Failed to download '{}': {}", name, e);
            if let Ok(mut stats) = DOWNLOAD_STATS.write() {
                stats.downloads_failed += 1;
            }
            return None;
        }
    };

    // Check status
    if response.status() != 200 {
        eprintln!("UAST: Download failed for '{}': HTTP {}", name, response.status());
        if let Ok(mut stats) = DOWNLOAD_STATS.write() {
            stats.downloads_failed += 1;
        }
        return None;
    }

    // Read the response body
    let mut bytes = Vec::new();
    if let Err(e) = response.into_reader().read_to_end(&mut bytes) {
        eprintln!("UAST: Failed to read response for '{}': {}", name, e);
        if let Ok(mut stats) = DOWNLOAD_STATS.write() {
            stats.downloads_failed += 1;
        }
        return None;
    }

    // Update stats
    if let Ok(mut stats) = DOWNLOAD_STATS.write() {
        stats.downloads_succeeded += 1;
        stats.bytes_downloaded += bytes.len();
    }

    // Cache to disk for future use
    if let Some(cache_dir) = ensure_cache_dir() {
        let cache_path = cache_dir.join(format!("{}.wasm", name));
        if let Err(e) = fs::write(&cache_path, &bytes) {
            eprintln!("UAST: Warning - failed to cache '{}': {}", name, e);
        } else {
            eprintln!("UAST: Cached '{}' to {:?}", name, cache_path);
        }
    }

    Some(bytes)
}

/// Stub for when download feature is disabled.
#[cfg(not(feature = "wasm-download"))]
fn download_wasm_grammar(name: &str) -> Option<Vec<u8>> {
    // Download disabled - only use local files
    let _ = name;
    None
}

// ============================================================================
// WASM Loading
// ============================================================================

/// Load WASM bytes into a Language.
#[cfg(feature = "wasm")]
fn load_wasm_bytes(name: &str, bytes: &[u8]) -> Option<Language> {
    use tree_sitter::WasmStore;

    // Initialize WASM store if needed
    {
        let mut store_guard = WASM_STORE.write().ok()?;
        if store_guard.is_none() {
            match WasmStore::new(&*WASM_ENGINE) {
                Ok(store) => *store_guard = Some(store),
                Err(e) => {
                    eprintln!("Failed to create WASM store: {}", e);
                    return None;
                }
            }
        }
    }

    // Load the language using store.load_language()
    let mut store_guard = WASM_STORE.write().ok()?;
    let store = store_guard.as_mut()?;

    match store.load_language(name, bytes) {
        Ok(lang) => Some(lang),
        Err(e) => {
            eprintln!("Failed to load WASM grammar '{}': {}", name, e);
            None
        }
    }
}

/// Stub for when WASM feature is disabled.
#[cfg(not(feature = "wasm"))]
fn load_wasm_bytes(_name: &str, _bytes: &[u8]) -> Option<Language> {
    None
}

// ============================================================================
// Utilities
// ============================================================================

/// List all available WASM grammars (locally cached).
pub fn available_wasm_grammars() -> Vec<String> {
    let mut grammars = Vec::new();

    // Check all search paths
    for search_dir in wasm_search_paths() {
        if let Ok(entries) = fs::read_dir(&search_dir) {
            for entry in entries.filter_map(|e| e.ok()) {
                let path = entry.path();
                if path.extension().map(|e| e == "wasm").unwrap_or(false) {
                    if let Some(stem) = path.file_stem() {
                        grammars.push(stem.to_string_lossy().to_string());
                    }
                }
            }
        }
    }

    grammars.sort();
    grammars.dedup();
    grammars
}

/// List all WASM grammars that can be downloaded.
pub fn downloadable_grammars() -> Vec<&'static str> {
    crate::tiers::WASM_LANGUAGES.to_vec()
}

/// Get all WASM search paths.
fn wasm_search_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // Next to executable
    if let Ok(exe_path) = env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            paths.push(exe_dir.join("grammars-wasm"));
        }
    }

    // Environment variable
    if let Ok(wasm_path) = env::var("UAST_WASM_PATH") {
        paths.push(PathBuf::from(wasm_path));
    }

    // User cache
    if let Some(cache_dir) = get_cache_dir() {
        paths.push(cache_dir);
    }

    // Working directory
    paths.push(PathBuf::from("grammars-wasm"));

    paths
}

/// Clear the in-memory WASM cache.
pub fn clear_wasm_cache() {
    if let Ok(mut cache) = WASM_CACHE.write() {
        cache.clear();
    }
}

/// Get cache statistics.
pub fn wasm_cache_stats() -> (usize, Vec<String>) {
    let cache = WASM_CACHE.read().unwrap();
    let count = cache.len();
    let names: Vec<String> = cache.keys().cloned().collect();
    (count, names)
}

/// Delete a cached grammar from disk.
pub fn delete_cached_grammar(name: &str) -> Result<(), String> {
    if let Some(cache_dir) = get_cache_dir() {
        let cache_path = cache_dir.join(format!("{}.wasm", name.to_lowercase()));
        if cache_path.exists() {
            fs::remove_file(&cache_path)
                .map_err(|e| format!("Failed to delete: {}", e))?;
        }
    }
    Ok(())
}

/// Delete all cached grammars from disk.
pub fn clear_disk_cache() -> Result<usize, String> {
    let cache_dir = get_cache_dir()
        .ok_or_else(|| "Cache directory not found".to_string())?;

    let mut count = 0;
    if let Ok(entries) = fs::read_dir(&cache_dir) {
        for entry in entries.filter_map(|e| e.ok()) {
            let path = entry.path();
            if path.extension().map(|e| e == "wasm").unwrap_or(false) {
                if fs::remove_file(&path).is_ok() {
                    count += 1;
                }
            }
        }
    }
    Ok(count)
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wasm_search_paths_not_empty() {
        let paths = wasm_search_paths();
        assert!(!paths.is_empty(), "Should have at least one search path");
    }

    #[test]
    fn test_dirs_next_home_returns_path() {
        let home = dirs_next_home();
        if let Some(path) = home {
            assert!(!path.as_os_str().is_empty());
        }
    }

    #[test]
    fn test_cache_starts_empty() {
        clear_wasm_cache();
        let (count, names) = wasm_cache_stats();
        assert_eq!(count, 0);
        assert!(names.is_empty());
    }

    #[test]
    fn test_is_wasm_available_known_grammar() {
        // SQL is a known WASM language
        assert!(is_wasm_available("sql"));
    }

    #[test]
    fn test_is_wasm_available_nonexistent() {
        assert!(!is_wasm_available("definitely_not_a_real_grammar_xyz123"));
    }

    #[test]
    fn test_downloadable_grammars() {
        let grammars = downloadable_grammars();
        assert_eq!(grammars.len(), 34, "Should have 34 WASM grammars");
        assert!(grammars.contains(&"sql"));
        assert!(grammars.contains(&"kotlin"));
    }

    #[test]
    fn test_get_download_url() {
        let url = get_download_url("sql");
        assert!(url.contains("sql.wasm"));
        assert!(url.contains("github.com"));
    }

    #[test]
    fn test_get_download_url_with_override() {
        env::set_var("UAST_WASM_URL", "https://example.com/grammars");
        let url = get_download_url("kotlin");
        assert_eq!(url, "https://example.com/grammars/kotlin.wasm");
        env::remove_var("UAST_WASM_URL");
    }

    #[test]
    fn test_download_stats() {
        reset_download_stats();
        let stats = get_download_stats();
        assert_eq!(stats.downloads_attempted, 0);
        assert_eq!(stats.cache_hits, 0);
    }

    #[test]
    fn test_available_wasm_grammars_returns_vec() {
        let grammars = available_wasm_grammars();
        let _ = grammars.len(); // Just verify it doesn't panic
    }
}
