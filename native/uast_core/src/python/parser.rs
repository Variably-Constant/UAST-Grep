//! Python bindings for UastParser.

use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use std::sync::Arc;

use crate::builtin_languages;
use crate::parser::ParsedTree;
use crate::python::tree::PyUastTree;
use crate::python::types::to_py_err;

/// A parser for a specific programming language.
///
/// Create a parser by specifying the language name. The parser can then
/// be used to parse source code into AST trees.
///
/// Example:
///     >>> parser = UastParser("python")
///     >>> tree = parser.parse("def hello(): pass")
///     >>> print(tree.root().kind())
///     'module'
#[pyclass(name = "UastParser")]
pub struct PyUastParser {
    language: String,
}

#[pymethods]
impl PyUastParser {
    /// Create a new parser for the specified language.
    ///
    /// Args:
    ///     language: The language name (e.g., "python", "rust", "javascript").
    ///               Use `supported_languages()` to get the full list.
    ///
    /// Raises:
    ///     ValueError: If the language is not supported.
    #[new]
    fn new(language: &str) -> PyResult<Self> {
        // Validate the language is available
        if !crate::builtin_languages::is_language_available(language) {
            return Err(to_py_err(format!(
                "Unknown language '{}'. Use supported_languages() to see available languages.",
                language
            )));
        }

        Ok(PyUastParser {
            language: language.to_string(),
        })
    }

    /// Parse source code into an AST tree.
    ///
    /// Args:
    ///     source: The source code to parse.
    ///     path: Optional file path for error reporting.
    ///
    /// Returns:
    ///     A `UastTree` containing the parsed AST.
    ///
    /// Raises:
    ///     ValueError: If parsing fails.
    ///
    /// Example:
    ///     >>> tree = parser.parse("def hello(): pass")
    ///     >>> tree.has_error()
    ///     False
    fn parse(&self, source: &str, path: Option<&str>) -> PyResult<PyUastTree> {
        // Get the language from built-in grammars
        let ts_language = builtin_languages::get_builtin_language(&self.language)
            .ok_or_else(|| to_py_err(format!("unknown language: {}", self.language)))?;

        // Create tree-sitter parser directly
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_language)
            .map_err(|e| to_py_err(format!("failed to set language: {}", e)))?;

        // Parse the source
        let tree = parser.parse(source, None)
            .ok_or_else(|| to_py_err("failed to parse source"))?;

        // Wrap in ParsedTree
        let parsed_tree = ParsedTree::from_raw(tree, source, ts_language, &self.language);

        Ok(PyUastTree::new(parsed_tree, path.map(|s| s.to_string())))
    }

    /// Parse source code and return a Python dictionary representation.
    ///
    /// This is useful for quick inspection or when you need to serialize
    /// the AST to JSON or other formats.
    ///
    /// Args:
    ///     source: The source code to parse.
    ///
    /// Returns:
    ///     A dictionary representation of the AST.
    ///
    /// Example:
    ///     >>> ast = parser.parse_to_dict("def hello(): pass")
    ///     >>> ast['kind']
    ///     'module'
    fn parse_to_dict(&self, py: Python<'_>, source: &str) -> PyResult<PyObject> {
        // Get the language from built-in grammars
        let ts_language = builtin_languages::get_builtin_language(&self.language)
            .ok_or_else(|| to_py_err(format!("unknown language: {}", self.language)))?;

        // Create tree-sitter parser directly
        let mut parser = tree_sitter::Parser::new();
        parser.set_language(&ts_language)
            .map_err(|e| to_py_err(format!("failed to set language: {}", e)))?;

        // Parse the source
        let tree = parser.parse(source, None)
            .ok_or_else(|| to_py_err("failed to parse source"))?;

        // Wrap in ParsedTree for the helper function
        let parsed_tree = ParsedTree::from_raw(tree, source, ts_language, &self.language);

        node_to_dict(py, parsed_tree.root_node(), &parsed_tree)
    }

    /// Get the language this parser is configured for.
    #[getter]
    fn language(&self) -> &str {
        &self.language
    }

    fn __repr__(&self) -> String {
        format!("UastParser(language='{}')", self.language)
    }
}

/// Convert a tree-sitter node to a Python dictionary.
fn node_to_dict<'a>(
    py: Python<'_>,
    node: tree_sitter::Node<'a>,
    tree: &ParsedTree,
) -> PyResult<PyObject> {
    let dict = PyDict::new_bound(py);

    // Basic properties
    dict.set_item("kind", node.kind())?;
    dict.set_item("is_named", node.is_named())?;
    dict.set_item("is_error", node.is_error())?;

    // Location
    let start = node.start_position();
    let end = node.end_position();
    dict.set_item("start_line", start.row + 1)?;  // 1-indexed for Python
    dict.set_item("start_column", start.column)?;
    dict.set_item("end_line", end.row + 1)?;
    dict.set_item("end_column", end.column)?;
    dict.set_item("start_byte", node.start_byte())?;
    dict.set_item("end_byte", node.end_byte())?;

    // Text content
    dict.set_item("text", tree.node_text(node))?;

    // Field name (if this node is a named field of its parent)
    // Note: This is not available without parent context, set to None
    dict.set_item("field_name", py.None())?;

    // Children
    let children_list = PyList::empty_bound(py);
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.is_named() {
            children_list.append(node_to_dict(py, child, tree)?)?;
        }
    }
    dict.set_item("children", children_list)?;

    Ok(dict.into())
}
