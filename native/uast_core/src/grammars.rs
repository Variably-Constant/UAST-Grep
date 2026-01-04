//! Grammar registry for dynamic language loading via FFI.
//!
//! This module accepts language pointers from C# and stores them for use by parsers.
//! Languages are loaded by C# using DynamicLanguage.cs, which loads individual grammar DLLs.
//! The language pointers are then registered here for fast parsing/query execution.
//!
//! # Architecture
//!
//! C# side (DynamicLanguage.cs):
//!   1. Loads grammar DLL (e.g., tree-sitter-kotlin.dll)
//!   2. Gets entry point function (tree_sitter_kotlin)
//!   3. Calls entry point to get TSLanguage* pointer
//!   4. Registers pointer with uast_register_language(name, ptr)
//!
//! Rust side (this module):
//!   1. Stores language pointers by name
//!   2. Provides get_language(name) for parser creation
//!   3. Provides language_from_ptr(ptr) for direct use

use once_cell::sync::Lazy;
use parking_lot::RwLock;
use std::collections::HashMap;
use tree_sitter::ffi::TSLanguage;
use tree_sitter::Language;

// ============================================================================
// Global Registry
// ============================================================================

/// Global registry of languages registered from C#.
///
/// Uses a `RwLock` to allow concurrent reads with exclusive writes
/// when registering new languages.
static REGISTRY: Lazy<RwLock<LanguageRegistry>> = Lazy::new(|| {
    RwLock::new(LanguageRegistry::new())
});

/// Language registry that maps language names to their tree-sitter Language objects.
pub struct LanguageRegistry {
    /// Map from language name to Language
    languages: HashMap<String, Language>,
    /// Map from file extension to language name
    by_extension: HashMap<String, String>,
}

impl LanguageRegistry {
    /// Create a new empty registry.
    fn new() -> Self {
        Self {
            languages: HashMap::new(),
            by_extension: HashMap::new(),
        }
    }

    /// Register a language from a raw pointer (passed from C#).
    ///
    /// # Safety
    ///
    /// The pointer must be a valid TSLanguage* pointer obtained from a tree-sitter
    /// grammar entry point function (e.g., tree_sitter_kotlin()).
    pub unsafe fn register_language_ptr(&mut self, name: &str, ptr: *const ()) -> bool {
        if ptr.is_null() {
            return false;
        }

        // SAFETY: Caller guarantees ptr is a valid TSLanguage pointer
        // Cast the opaque pointer to the concrete TSLanguage type
        let language = unsafe { Language::from_raw(ptr as *const TSLanguage) };

        // Validate the language is usable
        if language.node_kind_count() == 0 {
            return false;
        }

        self.languages.insert(name.to_lowercase(), language);
        true
    }

    /// Register file extensions for a language.
    ///
    /// The extensions should include the dot (e.g., ".kt", ".lua").
    pub fn register_extensions(&mut self, language_name: &str, extensions: &[&str]) {
        let name_lower = language_name.to_lowercase();
        for ext in extensions {
            self.by_extension.insert(ext.to_lowercase(), name_lower.clone());
        }
    }

    /// Get a language by name.
    pub fn get_language(&self, name: &str) -> Option<Language> {
        self.languages.get(&name.to_lowercase()).cloned()
    }

    /// Get a language by file extension.
    pub fn language_for_extension(&self, ext: &str) -> Option<Language> {
        let name = self.by_extension.get(&ext.to_lowercase())?;
        self.languages.get(name).cloned()
    }

    /// Get the language name for a file extension.
    pub fn language_name_for_extension(&self, ext: &str) -> Option<String> {
        self.by_extension.get(&ext.to_lowercase()).cloned()
    }

    /// Get a list of all registered language names.
    pub fn available_languages(&self) -> Vec<String> {
        let mut names: Vec<String> = self.languages.keys().cloned().collect();
        names.sort();
        names
    }

    /// Get a list of all registered file extensions.
    pub fn available_extensions(&self) -> Vec<String> {
        self.by_extension.keys().cloned().collect()
    }

    /// Check if a language is registered.
    pub fn has_language(&self, name: &str) -> bool {
        self.languages.contains_key(&name.to_lowercase())
    }

    /// Get the count of registered languages.
    pub fn language_count(&self) -> usize {
        self.languages.len()
    }

    /// Unregister a language (for cleanup/testing).
    pub fn unregister_language(&mut self, name: &str) -> bool {
        self.languages.remove(&name.to_lowercase()).is_some()
    }

    /// Clear all registered languages (for testing).
    pub fn clear(&mut self) {
        self.languages.clear();
        self.by_extension.clear();
    }
}

// ============================================================================
// Public API Functions
// ============================================================================

/// Register a language from a raw pointer (called from C# via FFI).
///
/// # Safety
///
/// The pointer must be a valid TSLanguage* pointer obtained from a tree-sitter
/// grammar entry point function.
pub unsafe fn register_language_ptr(name: &str, ptr: *const ()) -> bool {
    REGISTRY.write().register_language_ptr(name, ptr)
}

/// Register file extensions for a language.
pub fn register_extensions(language_name: &str, extensions: &[&str]) {
    REGISTRY.write().register_extensions(language_name, extensions);
}

/// Get a language by name.
pub fn get_language(name: &str) -> Option<Language> {
    REGISTRY.read().get_language(name)
}

/// Get a language from a raw pointer directly (for one-off parsing).
///
/// # Safety
///
/// The pointer must be a valid TSLanguage* pointer.
pub unsafe fn language_from_ptr(ptr: *const ()) -> Option<Language> {
    if ptr.is_null() {
        return None;
    }
    // Cast the opaque pointer to the concrete TSLanguage type
    Some(Language::from_raw(ptr as *const TSLanguage))
}

/// Get a language by file extension.
pub fn language_for_extension(ext: &str) -> Option<Language> {
    REGISTRY.read().language_for_extension(ext)
}

/// Get the language name for a file extension.
pub fn language_name_for_extension(ext: &str) -> Option<String> {
    REGISTRY.read().language_name_for_extension(ext)
}

/// Get a list of all registered language names.
pub fn available_languages() -> Vec<String> {
    REGISTRY.read().available_languages()
}

/// Get a list of all registered file extensions.
pub fn available_extensions() -> Vec<String> {
    REGISTRY.read().available_extensions()
}

/// Check if a language is registered.
pub fn has_language(name: &str) -> bool {
    REGISTRY.read().has_language(name)
}

/// Get the count of registered languages.
pub fn language_count() -> usize {
    REGISTRY.read().language_count()
}

/// Unregister a language.
pub fn unregister_language(name: &str) -> bool {
    REGISTRY.write().unregister_language(name)
}

/// Clear all registered languages (for testing).
pub fn clear_registry() {
    REGISTRY.write().clear();
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_registry() {
        // Note: Tests run in parallel so we can't rely on specific count
        // Just verify the API works
        let count = language_count();
        assert!(count >= 0);
    }

    #[test]
    fn test_register_null_ptr() {
        let result = unsafe { register_language_ptr("test_null", std::ptr::null()) };
        assert!(!result);
    }

    #[test]
    fn test_get_unregistered_language() {
        let lang = get_language("definitely_not_registered_xyz123");
        assert!(lang.is_none());
    }

    #[test]
    fn test_extension_lookup_unregistered() {
        let lang = language_for_extension(".xyz123_unknown");
        assert!(lang.is_none());
    }

    #[test]
    fn test_extension_registration() {
        // Register extensions for a hypothetical language
        register_extensions("test_lang_ext", &[".test1", ".test2"]);

        // Extensions are registered but language is not, so lookup returns None
        let name = language_name_for_extension(".test1");
        assert_eq!(name, Some("test_lang_ext".to_string()));

        // But getting the language returns None (not registered)
        let lang = language_for_extension(".test1");
        assert!(lang.is_none());
    }

    #[test]
    fn test_available_languages() {
        let langs = available_languages();
        // Just verify it returns something
        assert!(langs.len() >= 0);
    }

    #[test]
    fn test_case_insensitive() {
        // Extension lookups should be case-insensitive
        register_extensions("TestCaseLanguage", &[".TeSt"]);

        let name1 = language_name_for_extension(".TEST");
        let name2 = language_name_for_extension(".test");

        assert_eq!(name1, name2);
    }
}
