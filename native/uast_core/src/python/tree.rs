//! Python bindings for UastTree, UastNode, and related types.

use pyo3::prelude::*;
use pyo3::types::{PyDict, PyList};
use std::sync::Arc;
use streaming_iterator::StreamingIterator;

use crate::parser::ParsedTree;
use crate::query::CompiledQuery;
use crate::python::types::to_py_err;
use crate::uast::schema::SourceSpan as RustSourceSpan;

/// A source location span with start and end positions.
#[pyclass(name = "SourceSpan")]
#[derive(Clone)]
pub struct PySourceSpan {
    /// Start line (1-indexed).
    #[pyo3(get)]
    pub start_line: u32,
    /// Start column (0-indexed).
    #[pyo3(get)]
    pub start_column: u32,
    /// End line (1-indexed).
    #[pyo3(get)]
    pub end_line: u32,
    /// End column (0-indexed).
    #[pyo3(get)]
    pub end_column: u32,
    /// Start byte offset.
    #[pyo3(get)]
    pub start_byte: u32,
    /// End byte offset.
    #[pyo3(get)]
    pub end_byte: u32,
}

#[pymethods]
impl PySourceSpan {
    #[new]
    fn new(
        start_line: u32,
        start_column: u32,
        end_line: u32,
        end_column: u32,
        start_byte: u32,
        end_byte: u32,
    ) -> Self {
        PySourceSpan {
            start_line,
            start_column,
            end_line,
            end_column,
            start_byte,
            end_byte,
        }
    }

    fn __repr__(&self) -> String {
        format!(
            "SourceSpan({}:{}-{}:{}, bytes {}..{})",
            self.start_line, self.start_column,
            self.end_line, self.end_column,
            self.start_byte, self.end_byte
        )
    }

    /// Convert to a dictionary.
    fn to_dict(&self, py: Python<'_>) -> PyResult<PyObject> {
        let dict = PyDict::new_bound(py);
        dict.set_item("start_line", self.start_line)?;
        dict.set_item("start_column", self.start_column)?;
        dict.set_item("end_line", self.end_line)?;
        dict.set_item("end_column", self.end_column)?;
        dict.set_item("start_byte", self.start_byte)?;
        dict.set_item("end_byte", self.end_byte)?;
        Ok(dict.into())
    }
}

impl From<tree_sitter::Node<'_>> for PySourceSpan {
    fn from(node: tree_sitter::Node<'_>) -> Self {
        let start = node.start_position();
        let end = node.end_position();
        PySourceSpan {
            start_line: start.row as u32 + 1,  // 1-indexed
            start_column: start.column as u32,
            end_line: end.row as u32 + 1,
            end_column: end.column as u32,
            start_byte: node.start_byte() as u32,
            end_byte: node.end_byte() as u32,
        }
    }
}

/// A parsed AST tree.
///
/// The tree maintains ownership of the source code and provides methods
/// for traversing and querying the AST.
#[pyclass(name = "UastTree")]
pub struct PyUastTree {
    /// The underlying parsed tree.
    inner: Arc<ParsedTree>,
    /// Optional file path.
    path: Option<String>,
}

impl PyUastTree {
    pub fn new(tree: ParsedTree, path: Option<String>) -> Self {
        PyUastTree {
            inner: Arc::new(tree),
            path,
        }
    }

    pub fn inner(&self) -> &ParsedTree {
        &self.inner
    }
}

#[pymethods]
impl PyUastTree {
    /// Get the root node of the tree.
    fn root(&self) -> PyUastNode {
        PyUastNode::new(self.inner.clone(), 0)
    }

    /// Get all nodes in the tree via depth-first traversal.
    ///
    /// Returns:
    ///     A list of all nodes in pre-order traversal.
    fn walk(&self) -> Vec<PyUastNode> {
        let mut nodes = Vec::new();
        let root = self.inner.root_node();
        collect_nodes(&mut nodes, root, self.inner.clone());
        nodes
    }

    /// Find all nodes of a specific kind.
    ///
    /// Args:
    ///     kind: The node kind to search for (e.g., "function_definition").
    ///
    /// Returns:
    ///     A list of matching nodes.
    fn find_by_kind(&self, kind: &str) -> Vec<PyUastNode> {
        let mut nodes = Vec::new();
        let root = self.inner.root_node();
        find_by_kind_recursive(&mut nodes, root, kind, self.inner.clone());
        nodes
    }

    /// Execute a tree-sitter query on this tree.
    ///
    /// Args:
    ///     query: A tree-sitter S-expression query string.
    ///
    /// Returns:
    ///     A list of query matches.
    ///
    /// Example:
    ///     >>> matches = tree.query("(function_definition name: (identifier) @name)")
    ///     >>> for m in matches:
    ///     ...     print(m.captures)
    fn query(&self, query: &str) -> PyResult<Vec<PyQueryMatch>> {
        let compiled = CompiledQuery::new(&self.inner, query).map_err(to_py_err)?;

        let mut cursor = tree_sitter::QueryCursor::new();
        let source = self.inner.source();
        let root = self.inner.root_node();

        let mut matches = cursor.matches(compiled.inner(), root, source.as_bytes());
        let mut results = Vec::new();

        while let Some(m) = matches.next() {
            let capture_names = compiled.capture_names();
            let captures: Vec<(String, PyUastNode)> = m
                .captures
                .iter()
                .map(|c| {
                    let name = capture_names[c.index as usize].clone();
                    (name, PyUastNode::from_ts_node(self.inner.clone(), c.node))
                })
                .collect();

            results.push(PyQueryMatch {
                pattern_index: m.pattern_index as u32,
                captures,
            });
        }

        Ok(results)
    }

    /// Check if the tree has any parse errors.
    fn has_error(&self) -> bool {
        self.inner.has_error()
    }

    /// Get the total number of nodes in the tree.
    fn node_count(&self) -> usize {
        self.inner.node_count()
    }

    /// Get the source code.
    fn source(&self) -> &str {
        self.inner.source()
    }

    /// Get the language name.
    fn language(&self) -> &str {
        self.inner.language_name()
    }

    /// Get the file path (if set).
    #[getter]
    fn path(&self) -> Option<&str> {
        self.path.as_deref()
    }

    /// Convert the tree to a JSON string.
    fn to_json(&self) -> PyResult<String> {
        let root = self.inner.root_node();
        let json = node_to_json(root, &self.inner);
        serde_json::to_string_pretty(&json).map_err(to_py_err)
    }

    /// Convert the tree to a dictionary.
    fn to_dict(&self, py: Python<'_>) -> PyResult<PyObject> {
        node_to_py_dict(py, self.inner.root_node(), &self.inner)
    }

    fn __repr__(&self) -> String {
        format!(
            "UastTree(language='{}', nodes={}, has_error={})",
            self.inner.language_name(),
            self.inner.node_count(),
            self.inner.has_error()
        )
    }
}

/// An AST node.
///
/// Nodes are lightweight references into the tree. They provide access
/// to node properties and traversal methods.
#[pyclass(name = "UastNode")]
#[derive(Clone)]
pub struct PyUastNode {
    tree: Arc<ParsedTree>,
    /// Node ID within the tree (based on byte position for stable reference).
    node_start_byte: usize,
    node_end_byte: usize,
}

impl PyUastNode {
    pub fn new(tree: Arc<ParsedTree>, _node_id: usize) -> Self {
        let start = tree.root_node().start_byte();
        let end = tree.root_node().end_byte();
        PyUastNode {
            tree,
            node_start_byte: start,
            node_end_byte: end,
        }
    }

    pub fn from_ts_node(tree: Arc<ParsedTree>, node: tree_sitter::Node<'_>) -> Self {
        PyUastNode {
            tree,
            node_start_byte: node.start_byte(),
            node_end_byte: node.end_byte(),
        }
    }

    fn get_node(&self) -> Option<tree_sitter::Node<'_>> {
        // Find node by byte range
        find_node_by_range(
            self.tree.root_node(),
            self.node_start_byte,
            self.node_end_byte,
        )
    }
}

fn find_node_by_range(
    node: tree_sitter::Node<'_>,
    start_byte: usize,
    end_byte: usize,
) -> Option<tree_sitter::Node<'_>> {
    if node.start_byte() == start_byte && node.end_byte() == end_byte {
        return Some(node);
    }

    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if let Some(found) = find_node_by_range(child, start_byte, end_byte) {
            return Some(found);
        }
    }
    None
}

#[pymethods]
impl PyUastNode {
    /// Get the node kind (type).
    fn kind(&self) -> String {
        self.get_node()
            .map(|n| n.kind().to_string())
            .unwrap_or_default()
    }

    /// Check if this is a named node (not anonymous punctuation).
    fn is_named(&self) -> bool {
        self.get_node().is_some_and(|n| n.is_named())
    }

    /// Check if this is an error node.
    fn is_error(&self) -> bool {
        self.get_node().is_some_and(|n| n.is_error())
    }

    /// Get the source text for this node.
    fn text(&self) -> String {
        self.get_node()
            .map(|n| self.tree.node_text(n).to_string())
            .unwrap_or_default()
    }

    /// Get the source location span.
    fn span(&self) -> Option<PySourceSpan> {
        self.get_node().map(PySourceSpan::from)
    }

    /// Get the start line (1-indexed).
    fn start_line(&self) -> u32 {
        self.get_node()
            .map(|n| n.start_position().row as u32 + 1)
            .unwrap_or(0)
    }

    /// Get the start column (0-indexed).
    fn start_column(&self) -> u32 {
        self.get_node()
            .map(|n| n.start_position().column as u32)
            .unwrap_or(0)
    }

    /// Get the end line (1-indexed).
    fn end_line(&self) -> u32 {
        self.get_node()
            .map(|n| n.end_position().row as u32 + 1)
            .unwrap_or(0)
    }

    /// Get the end column (0-indexed).
    fn end_column(&self) -> u32 {
        self.get_node()
            .map(|n| n.end_position().column as u32)
            .unwrap_or(0)
    }

    /// Get the parent node.
    fn parent(&self) -> Option<PyUastNode> {
        self.get_node()
            .and_then(|n| n.parent())
            .map(|p| PyUastNode::from_ts_node(self.tree.clone(), p))
    }

    /// Get the child nodes.
    fn children(&self) -> Vec<PyUastNode> {
        self.get_node()
            .map(|n| {
                let mut cursor = n.walk();
                n.children(&mut cursor)
                    .map(|c| PyUastNode::from_ts_node(self.tree.clone(), c))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get named child nodes (excludes anonymous punctuation).
    fn named_children(&self) -> Vec<PyUastNode> {
        self.get_node()
            .map(|n| {
                let mut cursor = n.walk();
                n.children(&mut cursor)
                    .filter(|c| c.is_named())
                    .map(|c| PyUastNode::from_ts_node(self.tree.clone(), c))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get a child node by field name.
    fn child_by_field(&self, field: &str) -> Option<PyUastNode> {
        self.get_node()
            .and_then(|n| n.child_by_field_name(field))
            .map(|c| PyUastNode::from_ts_node(self.tree.clone(), c))
    }

    /// Get the number of children.
    fn child_count(&self) -> usize {
        self.get_node().map(|n| n.child_count()).unwrap_or(0)
    }

    /// Get the number of named children.
    fn named_child_count(&self) -> usize {
        self.get_node().map(|n| n.named_child_count()).unwrap_or(0)
    }

    /// Get the next sibling node.
    fn next_sibling(&self) -> Option<PyUastNode> {
        self.get_node()
            .and_then(|n| n.next_sibling())
            .map(|s| PyUastNode::from_ts_node(self.tree.clone(), s))
    }

    /// Get the previous sibling node.
    fn prev_sibling(&self) -> Option<PyUastNode> {
        self.get_node()
            .and_then(|n| n.prev_sibling())
            .map(|s| PyUastNode::from_ts_node(self.tree.clone(), s))
    }

    /// Convert to a dictionary.
    fn to_dict(&self, py: Python<'_>) -> PyResult<PyObject> {
        if let Some(node) = self.get_node() {
            node_to_py_dict(py, node, &self.tree)
        } else {
            Ok(py.None())
        }
    }

    fn __repr__(&self) -> String {
        if let Some(node) = self.get_node() {
            let start = node.start_position();
            format!(
                "UastNode(kind='{}', line={}, col={})",
                node.kind(),
                start.row + 1,
                start.column
            )
        } else {
            "UastNode(invalid)".to_string()
        }
    }
}

/// A query match result.
#[pyclass(name = "QueryMatch")]
#[derive(Clone)]
pub struct PyQueryMatch {
    /// Index of the matched pattern.
    #[pyo3(get)]
    pub pattern_index: u32,
    /// Captured nodes: (capture_name, node) pairs.
    captures: Vec<(String, PyUastNode)>,
}

#[pymethods]
impl PyQueryMatch {
    /// Get all captures as a dictionary.
    fn captures(&self, py: Python<'_>) -> PyResult<PyObject> {
        let dict = PyDict::new_bound(py);
        for (name, node) in &self.captures {
            // Convert node to Python object by creating a Bound wrapper
            let node_obj = Py::new(py, node.clone())?.into_any();
            dict.set_item(name, node_obj)?;
        }
        Ok(dict.into())
    }

    /// Get a specific capture by name.
    fn get_capture(&self, name: &str) -> Option<PyUastNode> {
        self.captures
            .iter()
            .find(|(n, _)| n == name)
            .map(|(_, node)| node.clone())
    }

    /// Get capture names.
    fn capture_names(&self) -> Vec<String> {
        self.captures.iter().map(|(n, _)| n.clone()).collect()
    }

    fn __repr__(&self) -> String {
        let names: Vec<&str> = self.captures.iter().map(|(n, _)| n.as_str()).collect();
        format!("QueryMatch(pattern={}, captures={:?})", self.pattern_index, names)
    }
}

/// Collect all nodes recursively.
fn collect_nodes(nodes: &mut Vec<PyUastNode>, node: tree_sitter::Node<'_>, tree: Arc<ParsedTree>) {
    nodes.push(PyUastNode::from_ts_node(tree.clone(), node));
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        collect_nodes(nodes, child, tree.clone());
    }
}

/// Find nodes by kind recursively.
fn find_by_kind_recursive(
    nodes: &mut Vec<PyUastNode>,
    node: tree_sitter::Node<'_>,
    kind: &str,
    tree: Arc<ParsedTree>,
) {
    if node.kind() == kind {
        nodes.push(PyUastNode::from_ts_node(tree.clone(), node));
    }
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        find_by_kind_recursive(nodes, child, kind, tree.clone());
    }
}

/// Convert a node to JSON.
fn node_to_json(node: tree_sitter::Node<'_>, tree: &ParsedTree) -> serde_json::Value {
    let start = node.start_position();
    let end = node.end_position();

    let children: Vec<serde_json::Value> = {
        let mut cursor = node.walk();
        node.children(&mut cursor)
            .filter(|c| c.is_named())
            .map(|c| node_to_json(c, tree))
            .collect()
    };

    serde_json::json!({
        "kind": node.kind(),
        "text": tree.node_text(node),
        "start_line": start.row + 1,
        "start_column": start.column,
        "end_line": end.row + 1,
        "end_column": end.column,
        "children": children
    })
}

/// Convert a node to a Python dictionary.
fn node_to_py_dict(
    py: Python<'_>,
    node: tree_sitter::Node<'_>,
    tree: &ParsedTree,
) -> PyResult<PyObject> {
    let dict = PyDict::new_bound(py);
    let start = node.start_position();
    let end = node.end_position();

    dict.set_item("kind", node.kind())?;
    dict.set_item("text", tree.node_text(node))?;
    dict.set_item("is_named", node.is_named())?;
    dict.set_item("start_line", start.row + 1)?;
    dict.set_item("start_column", start.column)?;
    dict.set_item("end_line", end.row + 1)?;
    dict.set_item("end_column", end.column)?;

    let children = PyList::empty_bound(py);
    let mut cursor = node.walk();
    for child in node.children(&mut cursor) {
        if child.is_named() {
            children.append(node_to_py_dict(py, child, tree)?)?;
        }
    }
    dict.set_item("children", children)?;

    Ok(dict.into())
}
