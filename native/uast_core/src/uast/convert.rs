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
        // Use byte range for memory efficiency instead of allocating String
        if let Some(name_node) = node.child_by_field_name("name") {
            let start = name_node.start_byte() as u32;
            let end = name_node.end_byte() as u32;
            if start < end {
                uast_node.name_range = Some((start, end));
            }
        }

        // For leaf nodes or if include_all_text is set, store text range
        // Use byte range for memory efficiency instead of allocating String
        if node.child_count() == 0 || self.options.include_all_text {
            let start = node.start_byte() as u32;
            let end = node.end_byte() as u32;
            if start < end {
                uast_node.text_range = Some((start, end));
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
                let field_text = self.get_node_text(field_node);
                if !field_text.is_empty() {
                    // For operators, store the actual operator text as String
                    // (operators are short like "+", "-", "==", so String is fine for properties)
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
                // Extract iterator variable name as String
                // (properties need JSON-serializable values, and variable names are typically short)
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
// Arena-based converter (feature = "arena")
// ============================================================================

#[cfg(feature = "arena")]
pub use arena_convert::*;

#[cfg(feature = "arena")]
mod arena_convert {
    use super::*;
    use crate::uast::arena::{ArenaUastNode, PropertyKey, PropertyValue, UastArena};
    use crate::uast::mappings::get_mappings;
    use crate::uast::schema::UastKind;

    /// Convert a tree-sitter tree to an arena-backed UAST tree.
    ///
    /// This is the most memory-efficient way to convert, as all nodes are
    /// allocated from a single memory pool.
    ///
    /// # Arguments
    /// * `arena` - The arena to allocate nodes from
    /// * `tree` - The tree-sitter parse tree
    /// * `source` - The source code
    /// * `language` - The language identifier
    ///
    /// # Returns
    /// A reference to the root arena node, which lives as long as the arena.
    pub fn convert_tree_to_arena<'arena>(
        arena: &'arena UastArena,
        tree: &tree_sitter::Tree,
        source: &str,
        language: &'arena str,
    ) -> &'arena ArenaUastNode<'arena> {
        let converter = ArenaConverter::new(arena, source, language);
        converter.convert_node(tree.root_node(), 0)
    }

    /// Arena-based converter that allocates all nodes from a bump allocator.
    struct ArenaConverter<'arena> {
        arena: &'arena UastArena,
        language: &'arena str,
        mappings: &'static crate::uast::mappings::NodeKindMappings,
    }

    impl<'arena> ArenaConverter<'arena> {
        fn new(arena: &'arena UastArena, source: &str, language: &'arena str) -> Self {
            // Allocate source in arena so it lives as long as the arena
            // (used for text extraction via get_text()/get_name() on nodes)
            let _source = arena.alloc_str(source);
            let mappings = get_mappings(language);
            Self { arena, language, mappings }
        }

        fn convert_node(
            &self,
            node: tree_sitter::Node,
            depth: usize,
        ) -> &'arena ArenaUastNode<'arena> {
            // Convert children first
            let mut children_vec: Vec<ArenaUastNode<'arena>> = Vec::new();

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.is_named() {
                    // Recursively convert but get the owned node first
                    let child_node = self.convert_node_owned(child, depth + 1);
                    children_vec.push(child_node);
                }
            }

            // Allocate children slice in arena
            let children = self.arena.alloc_children(children_vec);

            // Map the node kind using the same approach as UastConverter
            let native_type = node.kind();
            let kind_str = self.mappings.get(native_type);
            let kind = UastKind::from_str(kind_str);

            // Extract name range if present
            let name_range = node.child_by_field_name("name").map(|name_node| {
                (name_node.start_byte() as u32, name_node.end_byte() as u32)
            });

            // Text range for leaf nodes
            let text_range = if node.child_count() == 0 {
                Some((node.start_byte() as u32, node.end_byte() as u32))
            } else {
                None
            };

            // Extract properties
            let properties = self.extract_properties(&node, kind);

            // Allocate the native type string
            let native_type_str = Some(self.arena.alloc_str(node.kind()));

            // Create the arena node
            let arena_node = ArenaUastNode {
                kind,
                span: SourceSpan::from_tree_sitter(node),
                text_range,
                name_range,
                children,
                properties,
                native_type: native_type_str,
                language: self.language,
                is_named: node.is_named(),
            };

            self.arena.alloc_node(arena_node)
        }

        /// Convert a node and return the owned value (not a reference).
        /// Used for building the children vector.
        fn convert_node_owned(
            &self,
            node: tree_sitter::Node,
            depth: usize,
        ) -> ArenaUastNode<'arena> {
            // Convert children first
            let mut children_vec: Vec<ArenaUastNode<'arena>> = Vec::new();

            let mut cursor = node.walk();
            for child in node.children(&mut cursor) {
                if child.is_named() {
                    let child_node = self.convert_node_owned(child, depth + 1);
                    children_vec.push(child_node);
                }
            }

            // Allocate children slice in arena
            let children = self.arena.alloc_children(children_vec);

            // Map the node kind using the same approach as UastConverter
            let native_type = node.kind();
            let kind_str = self.mappings.get(native_type);
            let kind = UastKind::from_str(kind_str);

            // Extract name range if present
            let name_range = node.child_by_field_name("name").map(|name_node| {
                (name_node.start_byte() as u32, name_node.end_byte() as u32)
            });

            // Text range for leaf nodes
            let text_range = if node.child_count() == 0 {
                Some((node.start_byte() as u32, node.end_byte() as u32))
            } else {
                None
            };

            // Extract properties
            let properties = self.extract_properties(&node, kind);

            // Allocate the native type string
            let native_type_str = Some(self.arena.alloc_str(node.kind()));

            ArenaUastNode {
                kind,
                span: SourceSpan::from_tree_sitter(node),
                text_range,
                name_range,
                children,
                properties,
                native_type: native_type_str,
                language: self.language,
                is_named: node.is_named(),
            }
        }

        /// Extract properties for a node.
        fn extract_properties(
            &self,
            node: &tree_sitter::Node,
            kind: UastKind,
        ) -> &'arena [(PropertyKey, PropertyValue)] {
            let mut props: Vec<(PropertyKey, PropertyValue)> = Vec::new();

            // Extract operator if present
            if let Some(op_node) = node.child_by_field_name("operator") {
                let start = op_node.start_byte() as u32;
                let end = op_node.end_byte() as u32;
                props.push((PropertyKey::Operator, PropertyValue::ByteRange(start, end)));
            }

            // Extract iterator variable for for-in/for-of loops
            if let Some(left) = node.child_by_field_name("left") {
                if matches!(kind, UastKind::ForEachStatement | UastKind::ForInStatement | UastKind::ForOfStatement) {
                    let start = left.start_byte() as u32;
                    let end = left.end_byte() as u32;
                    props.push((PropertyKey::IteratorVariable, PropertyValue::ByteRange(start, end)));
                }
            }

            // Check for async/generator modifiers
            if matches!(kind, UastKind::FunctionDeclaration | UastKind::MethodDeclaration) {
                for i in 0..node.child_count() as u32 {
                    if let Some(child) = node.child(i) {
                        match child.kind() {
                            "async" => {
                                props.push((PropertyKey::IsAsync, PropertyValue::Bool(true)));
                            }
                            "generator" | "*" => {
                                props.push((PropertyKey::IsGenerator, PropertyValue::Bool(true)));
                            }
                            _ => {}
                        }
                    }
                }
            }

            // Check for variable declaration modifiers
            if kind == UastKind::VariableDeclaration {
                for i in 0..node.child_count() as u32 {
                    if let Some(child) = node.child(i) {
                        match child.kind() {
                            "const" => {
                                props.push((PropertyKey::IsConst, PropertyValue::Bool(true)));
                            }
                            "let" => {
                                props.push((PropertyKey::IsLet, PropertyValue::Bool(true)));
                            }
                            "var" => {
                                props.push((PropertyKey::IsVar, PropertyValue::Bool(true)));
                            }
                            _ => {}
                        }
                    }
                }
            }

            self.arena.alloc_properties(props)
        }
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
            // Name is now stored as byte range, use get_name() to extract
            assert_eq!(func[0].get_name(source), Some("hello"));
        }

        #[test]
        fn test_convert_uses_byte_ranges() {
            let source = "fn hello() {}";
            let tree = parse_rust(source);

            let uast = convert_tree_to_uast(&tree, source, "rust");

            // Find the function
            let func = uast.descendants_of_kind(UastKind::FunctionDeclaration);
            assert_eq!(func.len(), 1);

            // Verify name is stored as range, not string
            let func_node = func[0];
            assert!(func_node.name_range.is_some(), "name should be stored as byte range");
            assert!(func_node.name.is_none(), "name should NOT be stored as String");

            // Verify we can extract the name using the range
            assert_eq!(func_node.get_name(source), Some("hello"));
        }

        #[test]
        fn test_convert_leaf_nodes_use_text_range() {
            let source = "let x = 42;";
            let tree = parse_rust(source);

            let uast = convert_tree_to_uast(&tree, source, "rust");

            // Find identifiers (leaf nodes)
            let identifiers = uast.descendants_of_kind(UastKind::Identifier);

            // All leaf nodes should have text_range set (not text String)
            for ident in identifiers {
                if ident.children.is_empty() {
                    // This is a true leaf node - should have text_range, not text
                    assert!(
                        ident.text_range.is_some(),
                        "Leaf nodes should have text_range set"
                    );
                    assert!(
                        ident.text.is_none(),
                        "Leaf nodes should NOT have text String allocated"
                    );
                }
            }
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

        #[cfg(feature = "arena")]
        mod arena_tests {
            use super::*;
            use crate::uast::arena::UastArena;
            use crate::uast::convert::convert_tree_to_arena;

            fn parse_rust(source: &str) -> tree_sitter::Tree {
                let mut parser = TsParser::new();
                parser.set_language(&tree_sitter_rust::LANGUAGE.into()).unwrap();
                parser.parse(source, None).unwrap()
            }

            #[test]
            fn test_arena_convert_basic() {
                let source = "fn hello() {}";
                let tree = parse_rust(source);
                let arena = UastArena::new();

                let root = convert_tree_to_arena(&arena, &tree, source, "rust");

                assert_eq!(root.kind, UastKind::SourceFile);
                assert!(root.children.len() > 0);
                assert!(arena.allocated_bytes() > 0);
            }

            #[test]
            fn test_arena_convert_function() {
                let source = "fn my_func(x: i32) -> i32 { x + 1 }";
                let tree = parse_rust(source);
                let arena = UastArena::new();

                let root = convert_tree_to_arena(&arena, &tree, source, "rust");

                // Find function
                let funcs = root.descendants_of_kind(UastKind::FunctionDeclaration);
                assert_eq!(funcs.len(), 1);

                let func = funcs[0];
                assert_eq!(func.get_name(source), Some("my_func"));
            }

            #[test]
            fn test_arena_convert_preserves_structure() {
                let source = "fn a() {} fn b() {}";
                let tree = parse_rust(source);
                let arena = UastArena::new();

                let root = convert_tree_to_arena(&arena, &tree, source, "rust");

                let funcs = root.descendants_of_kind(UastKind::FunctionDeclaration);
                assert_eq!(funcs.len(), 2);
            }

            #[test]
            fn test_arena_to_owned_roundtrip() {
                let source = "fn test() { let x = 42; }";
                let tree = parse_rust(source);
                let arena = UastArena::new();

                let arena_root = convert_tree_to_arena(&arena, &tree, source, "rust");
                let owned_root = arena_root.to_owned(source);

                // The owned version should have the same structure
                assert_eq!(arena_root.kind, owned_root.kind);
                assert_eq!(arena_root.children.len(), owned_root.children.len());
            }

            #[test]
            fn test_arena_memory_efficiency() {
                let source = "fn a() {} fn b() {} fn c() {} fn d() {} fn e() {}";
                let tree = parse_rust(source);
                let arena = UastArena::new();

                let _root = convert_tree_to_arena(&arena, &tree, source, "rust");

                // Arena should have allocated memory
                let bytes = arena.allocated_bytes();
                assert!(bytes > 0);

                // Verify we can report memory usage
                println!("Arena allocated {} bytes for {} chars of source", bytes, source.len());
            }
        }
    }
}
