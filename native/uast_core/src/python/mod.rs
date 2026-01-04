//! Python bindings for UAST-Grep via PyO3.
//!
//! This module provides Python bindings for the core UAST-Grep functionality:
//! - `UastParser` - Parse source code into AST
//! - `UastTree` - Parsed AST tree with traversal methods
//! - `UastNode` - Individual AST nodes
//! - `PatternMatcher` - Match UAST patterns against trees
//! - `RuleScanner` - Scan source code with YAML rules
//!
//! # Example
//!
//! ```python
//! from uast_grep import UastParser, PatternMatcher, RuleScanner
//!
//! # Parse source code
//! parser = UastParser("python")
//! tree = parser.parse("def hello(): pass")
//!
//! # Pattern matching
//! matcher = PatternMatcher("FunctionDeclaration")
//! matches = matcher.matches(tree)
//!
//! # Rule scanning
//! scanner = RuleScanner()
//! scanner.load_rules("rules/security.yaml")
//! results = scanner.scan("def hello(): pass", "python")
//! ```

#[cfg(feature = "python")]
mod parser;
#[cfg(feature = "python")]
mod tree;
#[cfg(feature = "python")]
mod matcher;
#[cfg(feature = "python")]
mod scanner;
#[cfg(feature = "python")]
mod types;

#[cfg(feature = "python")]
pub use parser::*;
#[cfg(feature = "python")]
pub use tree::*;
#[cfg(feature = "python")]
pub use matcher::*;
#[cfg(feature = "python")]
pub use scanner::*;
#[cfg(feature = "python")]
pub use types::*;

#[cfg(feature = "python")]
use pyo3::prelude::*;

/// The uast_grep Python module.
///
/// Provides cross-language AST search and analysis capabilities.
#[cfg(feature = "python")]
#[pymodule]
#[pyo3(name = "_native")]
fn uast_grep(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Parser classes
    m.add_class::<parser::PyUastParser>()?;

    // Tree classes
    m.add_class::<tree::PyUastTree>()?;
    m.add_class::<tree::PyUastNode>()?;
    m.add_class::<tree::PyQueryMatch>()?;
    m.add_class::<tree::PySourceSpan>()?;

    // Matcher classes
    m.add_class::<matcher::PyPatternMatcher>()?;
    m.add_class::<matcher::PyMatchResult>()?;

    // Scanner classes
    m.add_class::<scanner::PyRuleScanner>()?;
    m.add_class::<scanner::PyScanResult>()?;

    // Helper functions
    m.add_function(wrap_pyfunction!(supported_languages, m)?)?;
    m.add_function(wrap_pyfunction!(is_uast_pattern, m)?)?;
    m.add_function(wrap_pyfunction!(language_for_extension, m)?)?;
    m.add_function(wrap_pyfunction!(get_extensions, m)?)?;
    m.add_function(wrap_pyfunction!(version, m)?)?;

    Ok(())
}

/// Get a list of all supported languages.
///
/// Returns a list of language names that can be used with `UastParser`.
/// Includes both built-in languages (37) and WASM languages (34).
#[cfg(feature = "python")]
#[pyfunction]
fn supported_languages() -> Vec<String> {
    crate::builtin_languages::available_languages()
}

/// Check if a pattern string is a UAST pattern (PascalCase).
///
/// UAST patterns use PascalCase node kinds like `FunctionDeclaration`
/// which are auto-translated to native tree-sitter node types.
///
/// Native patterns use snake_case like `function_item` or S-expressions.
#[cfg(feature = "python")]
#[pyfunction]
fn is_uast_pattern(pattern: &str) -> bool {
    crate::uast::is_uast_pattern(pattern)
}

/// Get the language name for a file extension.
///
/// Args:
///     ext: File extension including the dot (e.g., ".py", ".rs")
///
/// Returns:
///     Language name or None if unknown.
#[cfg(feature = "python")]
#[pyfunction]
fn language_for_extension(ext: &str) -> Option<String> {
    crate::builtin_languages::language_for_extension(ext).map(|s| s.to_string())
}

/// Get file extensions for a language.
///
/// Args:
///     language: Language name (e.g., "python", "rust")
///
/// Returns:
///     List of file extensions including the dot.
#[cfg(feature = "python")]
#[pyfunction]
fn get_extensions(language: &str) -> Vec<String> {
    crate::builtin_languages::get_extensions(language)
        .into_iter()
        .map(|s| s.to_string())
        .collect()
}

/// Get the library version.
#[cfg(feature = "python")]
#[pyfunction]
fn version() -> &'static str {
    "0.1.0"
}
