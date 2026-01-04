//! Dynamic DLL loading for tree-sitter grammar plugins.
//!
//! This module provides functionality to load tree-sitter grammar DLLs at runtime,
//! enabling support for all 74+ languages without embedding them at compile time.
//!
//! # Architecture
//!
//! Grammar DLLs follow the naming convention `tree-sitter-{name}.dll` and export
//! a function `tree_sitter_{name}` that returns a `*const TSLanguage` pointer.
//!
//! # Example
//!
//! ```ignore
//! use uast_core::dynamic_loader::{load_grammar_dll, discover_grammars};
//! use std::path::Path;
//!
//! // Load a specific grammar
//! let lang = load_grammar_dll("javascript", Path::new("tree-sitter-javascript.dll"))?;
//!
//! // Discover all available grammars in a directory
//! let grammars = discover_grammars(Path::new("./grammars"));
//! for (name, path) in grammars {
//!     println!("Found grammar: {} at {:?}", name, path);
//! }
//! ```

use libloading::{Library, Symbol};
use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::collections::HashMap;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use tree_sitter::ffi::TSLanguage;
use tree_sitter::Language;

use crate::error::Error;

/// Type alias for the tree-sitter language entry point function.
/// All grammar DLLs export a function with signature: `extern "C" fn() -> *const TSLanguage`
type LanguageEntryFn = unsafe extern "C" fn() -> *const TSLanguage;

/// Cache of loaded libraries to keep them alive.
/// Libraries must stay loaded while their Languages are in use, as Language
/// objects hold pointers into the library's memory.
static LOADED_LIBRARIES: Lazy<RwLock<HashMap<String, Box<Library>>>> =
    Lazy::new(|| RwLock::new(HashMap::new()));

/// Load a tree-sitter grammar from a DLL file.
///
/// The DLL should export a function named `tree_sitter_{name}` that returns
/// a pointer to a TSLanguage structure.
///
/// # Arguments
///
/// * `name` - The language name (e.g., "javascript", "python", "c-sharp")
/// * `dll_path` - Path to the grammar DLL file
///
/// # Returns
///
/// The loaded tree-sitter Language on success.
///
/// # Errors
///
/// Returns an error if:
/// - The DLL cannot be loaded
/// - The entry point function cannot be found
/// - The returned language pointer is invalid
///
/// # Safety
///
/// This function loads and executes code from a DLL. Only load DLLs from trusted sources.
pub fn load_grammar_dll(name: &str, dll_path: &Path) -> Result<Language, Error> {
    // Normalize the name for the entry point (e.g., "c-sharp" -> "c_sharp")
    let entry_name = normalize_entry_point_name(name);
    let symbol_name = format!("tree_sitter_{}", entry_name);

    // Check if already loaded
    {
        let libs = LOADED_LIBRARIES.read();
        if libs.contains_key(name) {
            // Library already loaded, get the language again
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
        Library::new(dll_path).map_err(|e| {
            Error::internal(format!(
                "Failed to load grammar DLL '{}': {}",
                dll_path.display(),
                e
            ))
        })?
    };

    // Get the entry point function
    let language = unsafe {
        let func: Symbol<LanguageEntryFn> = lib.get(symbol_name.as_bytes()).map_err(|e| {
            Error::internal(format!(
                "Failed to find entry point '{}' in '{}': {}. \
                Try looking for the actual exported symbol name in the DLL.",
                symbol_name,
                dll_path.display(),
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

    // Validate the language is usable
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
///
/// Tree-sitter entry points use underscores for word separators:
/// - "c-sharp" -> "c_sharp"
/// - "common-lisp" -> "commonlisp" (some grammars remove hyphens entirely)
/// - "markdown-inline" -> "markdown_inline"
///
/// # Arguments
///
/// * `name` - The language name from the DLL filename
///
/// # Returns
///
/// The normalized name suitable for the entry point function.
fn normalize_entry_point_name(name: &str) -> String {
    // Replace hyphens with underscores
    name.replace('-', "_")
}

/// Discover available grammar DLLs in a directory.
///
/// Scans the directory for files matching `tree-sitter-*.dll` and extracts
/// the language name from each filename.
///
/// # Arguments
///
/// * `base_path` - Directory to scan for grammar DLLs
///
/// # Returns
///
/// A vector of (language_name, dll_path) tuples for each discovered grammar.
pub fn discover_grammars(base_path: &Path) -> Vec<(String, PathBuf)> {
    let mut grammars = Vec::new();

    if !base_path.is_dir() {
        return grammars;
    }

    let entries = match std::fs::read_dir(base_path) {
        Ok(e) => e,
        Err(_) => return grammars,
    };

    for entry in entries.flatten() {
        let path = entry.path();

        // Only consider .dll files
        if path.extension() != Some(OsStr::new("dll")) {
            continue;
        }

        // Extract filename
        let filename = match path.file_name().and_then(|f| f.to_str()) {
            Some(f) => f,
            None => continue,
        };

        // Match tree-sitter-{name}.dll pattern
        if let Some(name) = extract_language_name(filename) {
            grammars.push((name, path));
        }
    }

    // Sort by language name for consistent ordering
    grammars.sort_by(|a, b| a.0.cmp(&b.0));

    grammars
}

/// Extract the language name from a grammar DLL filename.
///
/// # Arguments
///
/// * `filename` - The DLL filename (e.g., "tree-sitter-javascript.dll")
///
/// # Returns
///
/// The extracted language name (e.g., "javascript"), or None if the filename
/// doesn't match the expected pattern.
fn extract_language_name(filename: &str) -> Option<String> {
    // Expected pattern: tree-sitter-{name}.dll
    let without_ext = filename.strip_suffix(".dll")?;
    let name = without_ext.strip_prefix("tree-sitter-")?;

    // Skip empty names
    if name.is_empty() {
        return None;
    }

    Some(name.to_string())
}

/// Get the default grammar DLL search paths.
///
/// Returns paths where grammar DLLs are typically located:
/// 1. Relative to the executable: `../runtimes/win-x64/native/`
/// 2. Current directory
/// 3. A `grammars` subdirectory of the current directory
///
/// # Returns
///
/// A vector of PathBuf entries representing potential grammar directories.
pub fn default_grammar_paths() -> Vec<PathBuf> {
    let mut paths = Vec::new();

    // Relative to executable (typical install layout)
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            // Check for sibling runtimes directory
            paths.push(exe_dir.join("../runtimes/win-x64/native"));
            paths.push(exe_dir.join("runtimes/win-x64/native"));
            // Same directory as executable
            paths.push(exe_dir.to_path_buf());
        }
    }

    // Current directory
    if let Ok(cwd) = std::env::current_dir() {
        paths.push(cwd.join("grammars"));
        paths.push(cwd);
    }

    // Environment variable override
    if let Ok(grammar_path) = std::env::var("UAST_GRAMMAR_PATH") {
        for part in grammar_path.split(';') {
            paths.insert(0, PathBuf::from(part));
        }
    }

    paths
}

/// Find a grammar DLL by name in the default search paths.
///
/// # Arguments
///
/// * `name` - The language name (e.g., "javascript")
///
/// # Returns
///
/// The path to the grammar DLL if found, None otherwise.
pub fn find_grammar_dll(name: &str) -> Option<PathBuf> {
    let dll_name = format!("tree-sitter-{}.dll", name);

    for base_path in default_grammar_paths() {
        let dll_path = base_path.join(&dll_name);
        if dll_path.exists() {
            return Some(dll_path);
        }
    }

    None
}

/// Load a grammar by name, searching in default paths.
///
/// This is a convenience function that combines `find_grammar_dll` and `load_grammar_dll`.
///
/// # Arguments
///
/// * `name` - The language name (e.g., "javascript")
///
/// # Returns
///
/// The loaded Language on success, or an error if the grammar cannot be found or loaded.
pub fn load_grammar_by_name(name: &str) -> Result<Language, Error> {
    let dll_path = find_grammar_dll(name).ok_or_else(|| {
        Error::internal(format!(
            "Grammar DLL for '{}' not found in search paths. \
            Set UAST_GRAMMAR_PATH environment variable or use --grammar-path option.",
            name
        ))
    })?;

    load_grammar_dll(name, &dll_path)
}

/// Check if a grammar DLL is available (without loading it).
///
/// # Arguments
///
/// * `name` - The language name to check
///
/// # Returns
///
/// `true` if a DLL for this language exists in the search paths.
pub fn has_grammar_dll(name: &str) -> bool {
    find_grammar_dll(name).is_some()
}

/// Get a list of all discovered grammars from default paths.
///
/// # Returns
///
/// A vector of language names that have available grammar DLLs.
pub fn available_dynamic_grammars() -> Vec<String> {
    let mut all_grammars = Vec::new();

    for base_path in default_grammar_paths() {
        let grammars = discover_grammars(&base_path);
        for (name, _) in grammars {
            if !all_grammars.contains(&name) {
                all_grammars.push(name);
            }
        }
    }

    all_grammars.sort();
    all_grammars
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_language_name() {
        assert_eq!(
            extract_language_name("tree-sitter-javascript.dll"),
            Some("javascript".to_string())
        );
        assert_eq!(
            extract_language_name("tree-sitter-c-sharp.dll"),
            Some("c-sharp".to_string())
        );
        assert_eq!(
            extract_language_name("tree-sitter-python.dll"),
            Some("python".to_string())
        );
        assert_eq!(extract_language_name("something-else.dll"), None);
        assert_eq!(extract_language_name("tree-sitter-.dll"), None);
        assert_eq!(extract_language_name("tree-sitter-rust"), None); // No .dll extension
    }

    #[test]
    fn test_normalize_entry_point_name() {
        assert_eq!(normalize_entry_point_name("javascript"), "javascript");
        assert_eq!(normalize_entry_point_name("c-sharp"), "c_sharp");
        assert_eq!(normalize_entry_point_name("markdown-inline"), "markdown_inline");
        assert_eq!(normalize_entry_point_name("common-lisp"), "common_lisp");
    }

    #[test]
    fn test_default_grammar_paths_not_empty() {
        let paths = default_grammar_paths();
        // Should have at least some default paths
        assert!(!paths.is_empty());
    }
}
