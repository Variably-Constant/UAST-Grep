//! Tree-sitter to UAST Converter.
//!
//! This module provides functionality to convert tree-sitter parse trees into
//! the strongly-typed UAST schema defined in `schema.rs`.
//!
//! # Architecture
//!
//! The converter uses the language-specific mappings from `mappings.rs` to
//! determine the UAST kind for each tree-sitter node type. It then recursively
//! converts child nodes and extracts relevant properties.

use crate::uast::mappings::{get_mappings, NodeKindMappings};
use crate::uast::schema::{SourceSpan, UastKind, UastNode};
use serde_json::json;

/// Options for UAST conversion.
#[derive(Debug, Clone)]
pub struct ConvertOptions {
    /// Include raw source text for all nodes (not just leaves).
    pub include_all_text: bool,
    /// Include anonymous nodes (like punctuation).
    pub include_anonymous: bool,
    /// Maximum depth to convert (0 = unlimited).
    pub max_depth: usize,
    /// Include native tree-sitter type info.
    pub include_native_types: bool,
}

impl Default for ConvertOptions {
    fn default() -> Self {
        Self {
            include_all_text: false,
            include_anonymous: false,
            max_depth: 0,
            include_native_types: true,
        }
    }
}

/// Converter that transforms tree-sitter nodes into UAST nodes.
pub struct UastConverter<'a> {
    source: &'a str,
    language: &'a str,
    mappings: &'static NodeKindMappings,
    options: ConvertOptions,
}

impl<'a> UastConverter<'a> {
    /// Create a new converter for the given language.
    pub fn new(source: &'a str, language: &'a str) -> Self {
        Self {
            source,
            language,
            mappings: get_mappings(language),
            options: ConvertOptions::default(),
        }
    }

    /// Create a converter with custom options.
    pub fn with_options(source: &'a str, language: &'a str, options: ConvertOptions) -> Self {
        Self {
            source,
            language,
            mappings: get_mappings(language),
            options,
        }
    }

    /// Convert a tree-sitter tree to a UAST root node.
    pub fn convert_tree(&self, tree: &tree_sitter::Tree) -> UastNode {
        self.convert_node(tree.root_node(), 0)
    }

    /// Convert a single tree-sitter node to a UAST node.
    pub fn convert_node(&self, node: tree_sitter::Node, depth: usize) -> UastNode {
        // Check depth limit
        if self.options.max_depth > 0 && depth >= self.options.max_depth {
            return self.create_truncated_node(node);
        }

        // Get the UAST kind from mappings
        let native_type = node.kind();
        let kind_str = self.mappings.get(native_type);
        let kind = UastKind::from_str(kind_str);

        // Create the base node
        let span = SourceSpan::from_tree_sitter(node);
        let mut uast_node = UastNode::new(kind, self.language, span);

        // Set native type if requested
        if self.options.include_native_types {
            uast_node.native_type = Some(native_type.to_string());
        }

        // Set is_named flag
        uast_node.is_named = Some(node.is_named());

        // Extract name if present (for functions, classes, variables, etc.)
        if let Some(name_node) = node.child_by_field_name("name") {
            let name_text = self.get_node_text(name_node);
            uast_node.name = Some(name_text.to_string());
        }

        // For leaf nodes or if include_all_text is set, include raw source text
        if node.child_count() == 0 || self.options.include_all_text {
            let text = self.get_node_text(node);
            if !text.is_empty() {
                uast_node.text = Some(text.to_string());
            }
        }

        // Extract common fields into properties
        self.extract_common_fields(&mut uast_node, node);

        // Convert children
        let child_indices = self.get_convertible_child_indices(&node);
        let children: Vec<UastNode> = if self.options.include_anonymous {
            child_indices
                .into_iter()
                .filter_map(|i| node.child(i))
                .map(|child| self.convert_node(child, depth + 1))
                .collect()
        } else {
            child_indices
                .into_iter()
                .filter_map(|i| node.named_child(i))
                .map(|child| self.convert_node(child, depth + 1))
                .collect()
        };

        if !children.is_empty() {
            uast_node.children = children;
        }

        uast_node
    }

    /// Get the source text for a node.
    fn get_node_text(&self, node: tree_sitter::Node) -> &str {
        let start = node.start_byte();
        let end = node.end_byte();
        if end <= self.source.len() && start <= end {
            &self.source[start..end]
        } else {
            ""
        }
    }

    /// Extract common fields from tree-sitter node into UAST properties.
    fn extract_common_fields(&self, uast_node: &mut UastNode, node: tree_sitter::Node) {
        // Field mappings from tree-sitter field names to UAST property names
        let field_mappings = [
            ("operator", "operator"),
            ("left", "left"),
            ("right", "right"),
            ("condition", "condition"),
            ("consequence", "consequence"),
            ("alternative", "alternative"),
            ("value", "value"),
            ("type", "type"),
            ("return_type", "returnType"),
            ("superclass", "superclass"),
            ("pattern", "pattern"),
        ];

        for (ts_field, uast_field) in &field_mappings {
            if let Some(field_node) = node.child_by_field_name(ts_field) {
                // Store field references as metadata
                // The actual nodes are in children, this just notes which child is which field
                let field_text = self.get_node_text(field_node);
                if !field_text.is_empty() {
                    // For operators, store the actual operator text
                    if *ts_field == "operator" {
                        uast_node.properties.insert(
                            uast_field.to_string(),
                            json!(field_text),
                        );
                    }
                }
            }
        }

        // Add additional metadata based on node type
        self.add_type_specific_properties(uast_node, node);
    }

    /// Add properties specific to certain node types.
    fn add_type_specific_properties(&self, uast_node: &mut UastNode, node: tree_sitter::Node) {
        match uast_node.kind {
            UastKind::FunctionDeclaration | UastKind::MethodDeclaration => {
                // Check for async/generator modifiers
                for i in 0..node.child_count() as u32 {
                    if let Some(child) = node.child(i) {
                        match child.kind() {
                            "async" => {
                                uast_node.properties.insert("isAsync".to_string(), json!(true));
                            }
                            "generator" | "*" => {
                                uast_node.properties.insert("isGenerator".to_string(), json!(true));
                            }
                            _ => {}
                        }
                    }
                }
            }
            UastKind::VariableDeclaration => {
                // Check for const/let/var
                for i in 0..node.child_count() as u32 {
                    if let Some(child) = node.child(i) {
                        match child.kind() {
                            "const" => {
                                uast_node.properties.insert("isConst".to_string(), json!(true));
                            }
                            "let" => {
                                uast_node.properties.insert("isLet".to_string(), json!(true));
                            }
                            "var" => {
                                uast_node.properties.insert("isVar".to_string(), json!(true));
                            }
                            _ => {}
                        }
                    }
                }
            }
            UastKind::BinaryExpression => {
                // The operator is already extracted via field mapping
            }
            UastKind::ForEachStatement | UastKind::ForInStatement | UastKind::ForOfStatement => {
                // Extract iterator variable name
                if let Some(left) = node.child_by_field_name("left") {
                    let var_text = self.get_node_text(left);
                    uast_node.properties.insert("iteratorVariable".to_string(), json!(var_text));
                }
            }
            _ => {}
        }
    }

    /// Get child indices that should be converted to UAST.
    fn get_convertible_child_indices(&self, node: &tree_sitter::Node) -> Vec<u32> {
        if self.options.include_anonymous {
            // Include all children
            (0..node.child_count() as u32).collect()
        } else {
            // Only named children, filtering out pure syntax tokens
            (0..node.named_child_count() as u32)
                .filter(|&i| {
                    if let Some(child) = node.named_child(i) {
                        self.should_include_child(&child)
                    } else {
                        false
                    }
                })
                .collect()
        }
    }

    /// Determine if a child node should be included.
    fn should_include_child(&self, node: &tree_sitter::Node) -> bool {
        // Always include named nodes
        if node.is_named() {
            return true;
        }

        // Filter out common syntax tokens
        let node_type = node.kind();
        !matches!(
            node_type,
            "(" | ")"
                | "{"
                | "}"
                | "["
                | "]"
                | ","
                | ";"
                | ":"
                | "."
                | "->"
                | "=>"
                | "::"
                | "<"
                | ">"
        )
    }

    /// Create a truncated node when max depth is reached.
    fn create_truncated_node(&self, node: tree_sitter::Node) -> UastNode {
        let span = SourceSpan::from_tree_sitter(node);
        let mut uast_node = UastNode::new(UastKind::Unknown, self.language, span);
        uast_node.properties.insert("truncated".to_string(), json!(true));
        uast_node.properties.insert("childCount".to_string(), json!(node.child_count()));
        uast_node.native_type = Some(node.kind().to_string());
        uast_node
    }
}

/// Convert a tree-sitter tree to UAST with default options.
pub fn convert_tree_to_uast(tree: &tree_sitter::Tree, source: &str, language: &str) -> UastNode {
    let converter = UastConverter::new(source, language);
    converter.convert_tree(tree)
}

/// Convert a tree-sitter tree to UAST with custom options.
pub fn convert_tree_to_uast_with_options(
    tree: &tree_sitter::Tree,
    source: &str,
    language: &str,
    options: ConvertOptions,
) -> UastNode {
    let converter = UastConverter::with_options(source, language, options);
    converter.convert_tree(tree)
}

/// Convert a tree-sitter node to UAST.
pub fn convert_node_to_uast(
    node: tree_sitter::Node,
    source: &str,
    language: &str,
) -> UastNode {
    let converter = UastConverter::new(source, language);
    converter.convert_node(node, 0)
}

// ============================================================================
// UAST Document - Complete parse result with metadata
// ============================================================================

/// A complete UAST document with metadata.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UastDocument {
    /// Schema version.
    pub version: String,
    /// Source language.
    pub language: String,
    /// Optional source file path.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_path: Option<String>,
    /// Grammar source (builtin, registered, etc.).
    pub grammar_source: String,
    /// Whether the parse had errors.
    pub has_errors: bool,
    /// Parse errors (if any).
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub errors: Vec<ParseError>,
    /// The root UAST node.
    pub root: UastNode,
}

/// A parse error from tree-sitter.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ParseError {
    /// Error message.
    pub message: String,
    /// Error location.
    pub span: SourceSpan,
    /// Error severity (error, warning, etc.).
    pub severity: String,
}

impl ParseError {
    /// Create a new parse error.
    pub fn new(message: String, span: SourceSpan, severity: &str) -> Self {
        Self {
            message,
            span,
            severity: severity.to_string(),
        }
    }

    /// Create from a tree-sitter error node.
    pub fn from_error_node(node: tree_sitter::Node, source: &str) -> Self {
        let span = SourceSpan::from_tree_sitter(node);
        let text = get_node_text_static(node, source);
        let preview = if text.len() > 50 {
            format!("{}...", &text[..50])
        } else {
            text.to_string()
        };

        Self {
            message: format!("Syntax error: unexpected '{}'", preview.replace('\n', "\\n")),
            span,
            severity: "error".to_string(),
        }
    }

    /// Create from a tree-sitter missing node.
    pub fn from_missing_node(node: tree_sitter::Node) -> Self {
        let span = SourceSpan::from_tree_sitter(node);
        Self {
            message: format!("Syntax error: missing '{}'", node.kind()),
            span,
            severity: "error".to_string(),
        }
    }
}

/// Helper to get node text without borrowing self.
fn get_node_text_static<'a>(node: tree_sitter::Node<'a>, source: &'a str) -> &'a str {
    let start = node.start_byte();
    let end = node.end_byte();
    if end <= source.len() && start <= end {
        &source[start..end]
    } else {
        ""
    }
}

/// Collect all parse errors from a tree.
pub fn collect_parse_errors(tree: &tree_sitter::Tree, source: &str) -> Vec<ParseError> {
    let mut errors = Vec::new();
    collect_errors_recursive(tree.root_node(), source, &mut errors);
    errors
}

fn collect_errors_recursive(node: tree_sitter::Node, source: &str, errors: &mut Vec<ParseError>) {
    if node.is_error() {
        errors.push(ParseError::from_error_node(node, source));
    } else if node.is_missing() {
        errors.push(ParseError::from_missing_node(node));
    }

    for i in 0..node.child_count() as u32 {
        if let Some(child) = node.child(i) {
            collect_errors_recursive(child, source, errors);
        }
    }
}

/// Create a complete UAST document from a tree-sitter tree.
pub fn create_uast_document(
    tree: &tree_sitter::Tree,
    source: &str,
    language: &str,
    source_path: Option<&str>,
    grammar_source: &str,
) -> UastDocument {
    let errors = collect_parse_errors(tree, source);
    let root = convert_tree_to_uast(tree, source, language);

    UastDocument {
        version: "1.0".to_string(),
        language: language.to_string(),
        source_path: source_path.map(|s| s.to_string()),
        grammar_source: grammar_source.to_string(),
        has_errors: !errors.is_empty(),
        errors,
        root,
    }
}

/// Create a UAST document with custom conversion options.
pub fn create_uast_document_with_options(
    tree: &tree_sitter::Tree,
    source: &str,
    language: &str,
    source_path: Option<&str>,
    grammar_source: &str,
    options: ConvertOptions,
) -> UastDocument {
    let errors = collect_parse_errors(tree, source);
    let root = convert_tree_to_uast_with_options(tree, source, language, options);

    UastDocument {
        version: "1.0".to_string(),
        language: language.to_string(),
        source_path: source_path.map(|s| s.to_string()),
        grammar_source: grammar_source.to_string(),
        has_errors: !errors.is_empty(),
        errors,
        root,
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_convert_options_default() {
        let options = ConvertOptions::default();
        assert!(!options.include_all_text);
        assert!(!options.include_anonymous);
        assert_eq!(options.max_depth, 0);
        assert!(options.include_native_types);
    }

    #[test]
    fn test_source_span_from_tree_sitter() {
        // This would require an actual tree-sitter tree, so we test the struct directly
        let span = SourceSpan::new(1, 0, 5, 10, 0, 100);
        assert_eq!(span.start_line, 1);
        assert_eq!(span.start_column, 0);
        assert_eq!(span.end_line, 5);
        assert_eq!(span.end_column, 10);
    }

    #[test]
    fn test_parse_error_creation() {
        let span = SourceSpan::new(1, 0, 1, 10, 0, 10);
        let error = ParseError::new("Test error".to_string(), span, "error");
        assert_eq!(error.message, "Test error");
        assert_eq!(error.severity, "error");
    }

    #[test]
    fn test_uast_document_serialization() {
        let span = SourceSpan::new(1, 0, 1, 10, 0, 10);
        let root = UastNode::new(UastKind::SourceFile, "rust", span);

        let doc = UastDocument {
            version: "1.0".to_string(),
            language: "rust".to_string(),
            source_path: Some("test.rs".to_string()),
            grammar_source: "builtin".to_string(),
            has_errors: false,
            errors: vec![],
            root,
        };

        let json = serde_json::to_string(&doc).unwrap();
        assert!(json.contains("\"version\":\"1.0\""));
        assert!(json.contains("\"language\":\"rust\""));
        assert!(json.contains("\"sourcePath\":\"test.rs\""));
        assert!(json.contains("\"hasErrors\":false"));
    }

    #[cfg(feature = "builtin-grammars")]
    mod with_grammars {
        use super::*;
        use tree_sitter::Parser as TsParser;

        fn parse_rust(source: &str) -> tree_sitter::Tree {
            let mut parser = TsParser::new();
            parser
                .set_language(&tree_sitter_rust::LANGUAGE.into())
                .unwrap();
            parser.parse(source, None).unwrap()
        }

        #[test]
        fn test_convert_rust_function() {
            let source = "fn hello() {}";
            let tree = parse_rust(source);

            let uast = convert_tree_to_uast(&tree, source, "rust");

            assert_eq!(uast.kind, UastKind::SourceFile);
            assert_eq!(uast.language, "rust");
            assert!(!uast.children.is_empty());

            // Find the function declaration
            let func = uast.descendants_of_kind(UastKind::FunctionDeclaration);
            assert_eq!(func.len(), 1);
            assert_eq!(func[0].name, Some("hello".to_string()));
        }

        #[test]
        fn test_convert_rust_with_error() {
            let source = "fn bad( { }"; // Invalid syntax
            let tree = parse_rust(source);

            let doc = create_uast_document(&tree, source, "rust", Some("test.rs"), "builtin");

            assert!(doc.has_errors);
            assert!(!doc.errors.is_empty());
        }

        #[test]
        fn test_convert_with_max_depth() {
            let source = "fn a() { fn b() { fn c() {} } }";
            let tree = parse_rust(source);

            let options = ConvertOptions {
                max_depth: 2,
                ..Default::default()
            };

            let uast = convert_tree_to_uast_with_options(&tree, source, "rust", options);

            // At depth 2, some nodes should be truncated
            let truncated: Vec<_> = uast.descendants()
                .into_iter()
                .filter(|n| n.properties.contains_key("truncated"))
                .collect();

            // There should be some truncated nodes
            assert!(!truncated.is_empty() || uast.children.is_empty());
        }

        #[test]
        fn test_convert_includes_native_type() {
            let source = "let x = 42;";
            let tree = parse_rust(source);

            let uast = convert_tree_to_uast(&tree, source, "rust");

            // The root should have a native type
            assert!(uast.native_type.is_some());
            assert_eq!(uast.native_type.as_deref(), Some("source_file"));
        }
    }
}
