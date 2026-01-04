//! Built-in language grammars for self-contained operation.
//!
//! This module provides access to 71 tree-sitter grammars that are statically
//! compiled into the library. No external DLLs or language registration required.
//!
//! # Supported Languages
//!
//! All 71 languages from the grammar registry are available:
//! - Tier 1: agda, bash, c, cpp, c-sharp, css, go, haskell, html, java,
//!           javascript, json, julia, ocaml, php, python, ruby, rust, scala,
//!           typescript, tsx, verilog
//! - Tier 2: arduino, bicep, bitbake, cairo, csv, cuda, doxygen, hcl, lua,
//!           make, markdown, objc, toml, vim, vue, xml, yaml, zig
//! - Tier 3: ada, angular, apex, awk, clojure, cmake, cobol, comment,
//!           commonlisp, crystal, cue, d, dart, dhall, dockerfile, elixir,
//!           elm, erlang, fsharp, fortran, graphql, groovy, kotlin, latex,
//!           nix, perl, powershell, proto, r, sql, swift
//!
//! # Usage
//!
//! ```ignore
//! use uast_core::uast::builtin::get_builtin_language;
//!
//! if let Some(language) = get_builtin_language("rust") {
//!     // Use the language for parsing
//! }
//! ```

use tree_sitter::Language;

// Delegate to the main builtin_languages module which has all 71 grammars
use crate::builtin_languages;

/// Get a built-in language by name.
///
/// Returns `Some(Language)` if the language is available as a built-in,
/// `None` otherwise.
///
/// # Arguments
///
/// * `name` - The language name (case-insensitive). See module docs for full list.
///
/// # Returns
///
/// The tree-sitter Language if available, None if not recognized.
pub fn get_builtin_language(name: &str) -> Option<Language> {
    builtin_languages::get_builtin_language(name)
}

/// Get a built-in language by file extension.
///
/// Returns `Some(Language)` if the extension maps to a built-in language,
/// `None` otherwise.
///
/// # Arguments
///
/// * `ext` - The file extension (with or without leading dot, case-insensitive)
///
/// # Returns
///
/// The tree-sitter Language if available, None if not recognized.
pub fn get_builtin_language_for_extension(ext: &str) -> Option<Language> {
    builtin_languages::get_builtin_language_for_extension(ext)
}

/// Get the language name for a file extension (for built-in languages only).
///
/// # Arguments
///
/// * `ext` - The file extension (with or without leading dot, case-insensitive)
///
/// # Returns
///
/// The canonical language name if available, None otherwise.
pub fn get_builtin_language_name_for_extension(ext: &str) -> Option<&'static str> {
    builtin_languages::get_builtin_language_name_for_extension(ext)
}

/// Check if a language is available as a built-in.
///
/// # Arguments
///
/// * `name` - The language name (case-insensitive)
///
/// # Returns
///
/// `true` if the language is built-in, `false` otherwise.
pub fn is_builtin_language(name: &str) -> bool {
    builtin_languages::is_builtin_language(name)
}

/// Get a list of all available built-in language names.
///
/// # Returns
///
/// A slice of canonical language names.
pub fn available_builtin_languages() -> Vec<&'static str> {
    builtin_languages::available_builtin_languages()
}

/// Get a list of all file extensions supported by built-in grammars.
///
/// # Returns
///
/// A slice of file extensions (with leading dots).
pub fn available_builtin_extensions() -> Vec<&'static str> {
    builtin_languages::available_builtin_extensions()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_builtin_rust() {
        let lang = get_builtin_language("rust");
        assert!(lang.is_some());

        let lang = get_builtin_language("rs");
        assert!(lang.is_some());

        let lang = get_builtin_language("RUST");
        assert!(lang.is_some());
    }

    #[test]
    fn test_get_builtin_python() {
        let lang = get_builtin_language("python");
        assert!(lang.is_some());

        let lang = get_builtin_language("py");
        assert!(lang.is_some());
    }

    #[test]
    fn test_get_builtin_javascript() {
        let lang = get_builtin_language("javascript");
        assert!(lang.is_some());

        let lang = get_builtin_language("js");
        assert!(lang.is_some());
    }

    #[test]
    fn test_unknown_language() {
        let lang = get_builtin_language("notareallanguage123");
        assert!(lang.is_none());
    }

    #[test]
    fn test_is_builtin() {
        assert!(is_builtin_language("rust"));
        assert!(is_builtin_language("python"));
        assert!(is_builtin_language("javascript"));
        assert!(is_builtin_language("go"));
        assert!(!is_builtin_language("notareallanguage123"));
    }

    #[test]
    fn test_available_languages() {
        let langs = available_builtin_languages();
        assert!(langs.contains(&"rust"));
        assert!(langs.contains(&"python"));
        assert!(langs.contains(&"javascript"));
        // Should have 70 languages
        assert!(langs.len() >= 70);
    }

    #[test]
    fn test_available_extensions() {
        let exts = available_builtin_extensions();
        assert!(exts.contains(&".rs"));
        assert!(exts.contains(&".py"));
        assert!(exts.contains(&".js"));
    }
}
