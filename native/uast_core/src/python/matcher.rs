//! Python bindings for PatternMatcher.

use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::collections::HashMap;
use std::sync::Arc;

use crate::builtin_languages;
use crate::matching::{parse_simple_pattern, CapturedValue, MatchResult, Pattern, PatternMatcher};
use crate::parser::ParsedTree;
use crate::uast::convert::convert_node_to_uast;
use crate::python::tree::{PySourceSpan, PyUastTree};
use crate::python::types::to_py_err;

/// A pattern matcher for UAST patterns.
///
/// The matcher can use either UAST patterns (cross-language, PascalCase) or
/// native tree-sitter patterns (language-specific, snake_case).
///
/// Example:
///     >>> matcher = PatternMatcher("FunctionDeclaration")
///     >>> matches = matcher.matches("def hello(): pass", "python")
///     >>> len(matches)
///     1
#[pyclass(name = "PatternMatcher")]
pub struct PyPatternMatcher {
    pattern_str: String,
    language: Option<String>,
}

#[pymethods]
impl PyPatternMatcher {
    /// Create a new pattern matcher.
    ///
    /// Args:
    ///     pattern: The pattern to match. Can be:
    ///         - UAST pattern: `FunctionDeclaration`, `$NAME`, `$$ARGS`
    ///         - Native pattern: `function_definition`, `function_item`
    ///         - S-expression: `(function_definition name: (identifier) @name)`
    ///     language: Optional language hint for native patterns.
    ///
    /// Example:
    ///     >>> # UAST pattern (works across languages)
    ///     >>> matcher = PatternMatcher("FunctionDeclaration")
    ///     >>> # Native pattern (language-specific)
    ///     >>> matcher = PatternMatcher("function_definition", "python")
    #[new]
    #[pyo3(signature = (pattern, language=None))]
    fn new(pattern: &str, language: Option<&str>) -> Self {
        PyPatternMatcher {
            pattern_str: pattern.to_string(),
            language: language.map(|s| s.to_string()),
        }
    }

    /// Match the pattern against source code.
    ///
    /// Args:
    ///     source: The source code to search.
    ///     language: The programming language of the source.
    ///
    /// Returns:
    ///     A list of `MatchResult` objects.
    ///
    /// Example:
    ///     >>> matcher = PatternMatcher("FunctionDeclaration")
    ///     >>> matches = matcher.matches("def hello(): pass", "python")
    ///     >>> matches[0].node.kind()
    ///     'function_definition'
    fn matches(&self, source: &str, language: &str) -> PyResult<Vec<PyMatchResult>> {
        // Get the language from built-in grammars
        let ts_language = builtin_languages::get_builtin_language(language)
            .ok_or_else(|| to_py_err(format!("unknown language: {}", language)))?;

        // Create tree-sitter parser directly
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_language)
            .map_err(|e| to_py_err(format!("failed to set language: {}", e)))?;

        // Parse the source
        let tree = parser.parse(source, None)
            .ok_or_else(|| to_py_err("failed to parse source"))?;

        // Wrap in ParsedTree
        let parsed_tree = ParsedTree::from_raw(tree, source, ts_language, language);
        let tree_arc = Arc::new(parsed_tree);

        // Convert to UAST
        let uast_root = convert_node_to_uast(tree_arc.root_node(), tree_arc.source(), language);

        // Parse and compile the pattern
        let lang = self.language.as_deref().unwrap_or(language);
        let pattern = parse_simple_pattern(&self.pattern_str, lang).map_err(to_py_err)?;

        // Match
        let matcher = PatternMatcher::new();
        let matches = matcher.find_all(&uast_root, &pattern);

        // Convert to Python results
        Ok(matches
            .into_iter()
            .map(|m| PyMatchResult::from_match_result(m, tree_arc.clone()))
            .collect())
    }

    /// Match the pattern against a parsed tree.
    ///
    /// Args:
    ///     tree: A `UastTree` to search.
    ///
    /// Returns:
    ///     A list of `MatchResult` objects.
    fn matches_tree(&self, tree: &PyUastTree) -> PyResult<Vec<PyMatchResult>> {
        let tree_arc = Arc::new(ParsedTree::clone_from(tree.inner()));
        let language = tree.inner().language_name();

        // Convert to UAST
        let uast_root = convert_node_to_uast(tree.inner().root_node(), tree.inner().source(), language);

        // Parse and compile the pattern
        let lang = self.language.as_deref().unwrap_or(language);
        let pattern = parse_simple_pattern(&self.pattern_str, lang).map_err(to_py_err)?;

        // Match
        let matcher = PatternMatcher::new();
        let matches = matcher.find_all(&uast_root, &pattern);

        // Convert to Python results
        Ok(matches
            .into_iter()
            .map(|m| PyMatchResult::from_match_result(m, tree_arc.clone()))
            .collect())
    }

    /// Get the pattern string.
    #[getter]
    fn pattern(&self) -> &str {
        &self.pattern_str
    }

    /// Check if this is a UAST pattern (PascalCase).
    fn is_uast_pattern(&self) -> bool {
        crate::uast::is_uast_pattern(&self.pattern_str)
    }

    fn __repr__(&self) -> String {
        format!("PatternMatcher(pattern='{}')", self.pattern_str)
    }
}

/// A pattern match result.
#[pyclass(name = "MatchResult")]
#[derive(Clone)]
pub struct PyMatchResult {
    /// The matched node kind.
    #[pyo3(get)]
    pub kind: String,
    /// The matched text.
    #[pyo3(get)]
    pub text: String,
    /// The source location.
    #[pyo3(get)]
    pub span: PySourceSpan,
    /// Captured metavariables.
    captures: HashMap<String, String>,
}

impl PyMatchResult {
    pub fn from_match_result(result: MatchResult, _tree: Arc<ParsedTree>) -> Self {
        let captures: HashMap<String, String> = result
            .captures
            .iter()
            .map(|(name, value)| {
                let text = match value {
                    CapturedValue::Single(node) => {
                        node.text.clone().unwrap_or_default()
                    }
                    CapturedValue::Multiple(nodes) => {
                        nodes
                            .iter()
                            .filter_map(|n| n.text.clone())
                            .collect::<Vec<_>>()
                            .join(", ")
                    }
                };
                (name.clone(), text)
            })
            .collect();

        PyMatchResult {
            kind: result.node.kind.to_string(),
            text: result.node.text.clone().unwrap_or_default(),
            span: PySourceSpan {
                start_line: result.span.start_line,
                start_column: result.span.start_column,
                end_line: result.span.end_line,
                end_column: result.span.end_column,
                start_byte: result.span.start_offset,
                end_byte: result.span.end_offset,
            },
            captures,
        }
    }
}

#[pymethods]
impl PyMatchResult {
    /// Get all captured metavariables as a dictionary.
    fn captures(&self, py: Python<'_>) -> PyResult<PyObject> {
        let dict = PyDict::new_bound(py);
        for (name, value) in &self.captures {
            dict.set_item(name, value)?;
        }
        Ok(dict.into())
    }

    /// Get a specific capture by name.
    fn get_capture(&self, name: &str) -> Option<String> {
        self.captures.get(name).cloned()
    }

    /// Get all capture names.
    fn capture_names(&self) -> Vec<String> {
        self.captures.keys().cloned().collect()
    }

    fn __repr__(&self) -> String {
        format!(
            "MatchResult(kind='{}', line={}, captures={:?})",
            self.kind,
            self.span.start_line,
            self.captures.keys().collect::<Vec<_>>()
        )
    }
}

// Helper to clone ParsedTree (needed because it's not Clone by default)
trait CloneFrom {
    fn clone_from(other: &ParsedTree) -> ParsedTree;
}

impl CloneFrom for ParsedTree {
    fn clone_from(other: &ParsedTree) -> ParsedTree {
        // Re-parse the source since ParsedTree doesn't implement Clone
        let ts_language = builtin_languages::get_builtin_language(other.language_name())
            .expect("language should be available");
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_language).expect("language should be valid");
        let tree = parser.parse(other.source(), None).expect("parsing should succeed");
        ParsedTree::from_raw(tree, other.source(), ts_language, other.language_name())
    }
}
