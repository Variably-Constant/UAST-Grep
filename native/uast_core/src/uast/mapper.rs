//! Generic tree-sitter to UAST JSON mapper.
//!
//! This module provides the core transformation logic that converts tree-sitter
//! parse trees into UAST JSON format.
//!
//! # Built-in Grammar Support
//!
//! When the `builtin-grammars` feature is enabled, the mapper will automatically
//! use built-in grammars for supported languages (Rust, C, C++) without requiring
//! language registration from C# via FFI.
//!
//! # Error Reporting
//!
//! Parse errors are collected from tree-sitter's error and missing nodes and
//! included in the JSON output under the "errors" field.

use crate::parser::Parser;
use crate::uast::builtin::get_builtin_language;
use crate::uast::mappings::{get_mappings, NodeKindMappings};
use serde_json::{json, Value};
use tree_sitter::Parser as TsParser;

/// Represents a parse error from tree-sitter.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub start_line: usize,
    pub start_column: usize,
    pub end_line: usize,
    pub end_column: usize,
    pub start_byte: usize,
    pub end_byte: usize,
    pub severity: &'static str,
}

impl ParseError {
    fn to_json(&self) -> Value {
        json!({
            "message": self.message,
            "span": {
                "startLine": self.start_line,
                "startColumn": self.start_column,
                "endLine": self.end_line,
                "endColumn": self.end_column,
                "startByte": self.start_byte,
                "endByte": self.end_byte
            },
            "severity": self.severity
        })
    }
}

/// Parse source code and return UAST as JSON.
///
/// # Arguments
///
/// * `language` - The language name (e.g., "rust", "c", "cpp", or any registered language)
/// * `source` - The source code to parse
/// * `source_path` - Optional file path for the source (for diagnostics)
///
/// # Returns
///
/// A JSON string containing the UAST representation, or an error message.
///
/// # Language Resolution
///
/// The function attempts to resolve the language in this order:
/// 1. Built-in grammars (when `builtin-grammars` feature is enabled)
/// 2. Languages registered via FFI from C#
///
/// If neither source provides the language, an error is returned.
pub fn parse_to_uast_json(
    language: &str,
    source: &str,
    source_path: Option<&str>,
) -> Result<String, String> {
    // First, try to get a built-in language
    if let Some(builtin_lang) = get_builtin_language(language) {
        // Use the built-in grammar directly
        let mut ts_parser = TsParser::new();
        ts_parser
            .set_language(&builtin_lang)
            .map_err(|e| format!("Failed to set builtin language {}: {}", language, e))?;

        let tree = ts_parser
            .parse(source, None)
            .ok_or_else(|| "Failed to parse source with builtin grammar".to_string())?;

        // Get mappings for this language
        let mappings = get_mappings(language);

        // Collect parse errors from the tree
        let root = tree.root_node();
        let errors = collect_parse_errors(root, source);

        // Convert tree to UAST JSON
        let uast = map_node(root, source, language, mappings);

        // Wrap in root object with metadata
        let error_json: Vec<Value> = errors.iter().map(|e| e.to_json()).collect();
        let result = json!({
            "version": "1.0",
            "language": language,
            "sourcePath": source_path,
            "grammarSource": "builtin",
            "hasErrors": !errors.is_empty(),
            "errors": error_json,
            "root": uast
        });

        return serde_json::to_string_pretty(&result)
            .map_err(|e| format!("Failed to serialize UAST: {}", e));
    }

    // Fall back to registered languages (via FFI from C#)
    let mut parser = Parser::new(language)
        .map_err(|e| format!("Failed to create parser for {}: {}", language, e))?;

    // Parse the source
    let tree = parser
        .parse(source)
        .map_err(|e| format!("Failed to parse source: {}", e))?;

    // Get mappings for this language
    let mappings = get_mappings(language);

    // Collect parse errors from the tree
    let root = tree.root_node();
    let errors = collect_parse_errors(root, source);

    // Convert tree to UAST JSON
    let uast = map_node(root, source, language, mappings);

    // Wrap in root object with metadata
    let error_json: Vec<Value> = errors.iter().map(|e| e.to_json()).collect();
    let result = json!({
        "version": "1.0",
        "language": language,
        "sourcePath": source_path,
        "grammarSource": "registered",
        "hasErrors": !errors.is_empty(),
        "errors": error_json,
        "root": uast
    });

    serde_json::to_string_pretty(&result)
        .map_err(|e| format!("Failed to serialize UAST: {}", e))
}

/// Collect parse errors from a tree-sitter tree.
///
/// Tree-sitter is error-tolerant - it produces a tree even with syntax errors.
/// Error nodes are marked with `is_error()` (unexpected tokens) or
/// `is_missing()` (expected tokens that weren't found).
fn collect_parse_errors(root: tree_sitter::Node, source: &str) -> Vec<ParseError> {
    let mut errors = Vec::new();
    collect_errors_recursive(root, source, &mut errors);
    errors
}

fn collect_errors_recursive(node: tree_sitter::Node, source: &str, errors: &mut Vec<ParseError>) {
    if node.is_error() {
        // ERROR node - unexpected token or sequence
        let text = get_node_text(node, source);
        let preview = if text.len() > 50 {
            format!("{}...", &text[..50])
        } else {
            text.to_string()
        };

        errors.push(ParseError {
            message: format!("Syntax error: unexpected '{}'", preview.replace('\n', "\\n")),
            start_line: node.start_position().row + 1,
            start_column: node.start_position().column,
            end_line: node.end_position().row + 1,
            end_column: node.end_position().column,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            severity: "error",
        });
    } else if node.is_missing() {
        // MISSING node - expected token not found
        errors.push(ParseError {
            message: format!("Syntax error: missing '{}'", node.kind()),
            start_line: node.start_position().row + 1,
            start_column: node.start_position().column,
            end_line: node.end_position().row + 1,
            end_column: node.end_position().column,
            start_byte: node.start_byte(),
            end_byte: node.end_byte(),
            severity: "error",
        });
    }

    // Recurse into children
    for i in 0..node.child_count() {
        if let Some(child) = node.child(i as u32) {
            collect_errors_recursive(child, source, errors);
        }
    }
}

/// Map a single tree-sitter node to UAST JSON.
fn map_node(
    node: tree_sitter::Node,
    source: &str,
    language: &str,
    mappings: &NodeKindMappings,
) -> Value {
    let node_type = node.kind();
    let node_kind = mappings.get(node_type);

    // Build the base node
    let mut obj = json!({
        "nodeKind": node_kind,
        "language": language,
        "span": {
            "startLine": node.start_position().row + 1,
            "startColumn": node.start_position().column + 1,
            "endLine": node.end_position().row + 1,
            "endColumn": node.end_position().column + 1,
            "startByte": node.start_byte(),
            "endByte": node.end_byte()
        },
        "extensions": {
            "nativeNodeType": node_type,
            "isNamed": node.is_named(),
            "childCount": node.child_count(),
            "namedChildCount": node.named_child_count()
        }
    });

    // Extract name if present (for functions, classes, variables, etc.)
    if let Some(name_node) = node.child_by_field_name("name") {
        let name_text = get_node_text(name_node, source);
        obj["name"] = json!(name_text);
    }

    // For leaf nodes, include the raw source text
    if node.child_count() == 0 {
        let text = get_node_text(node, source);
        obj["rawSource"] = json!(text);
    }

    // Map common fields that exist across many languages
    let field_mappings = [
        ("body", "body"),
        ("parameters", "parameters"),
        ("arguments", "arguments"),
        ("condition", "condition"),
        ("consequence", "consequence"),
        ("alternative", "alternative"),
        ("left", "left"),
        ("right", "right"),
        ("operator", "operator"),
        ("value", "value"),
        ("type", "type"),
        ("initializer", "initializer"),
        ("declarator", "declarator"),
        ("function", "function"),
        ("object", "object"),
        ("property", "property"),
        ("index", "index"),
        ("return_type", "returnType"),
        ("superclass", "superclass"),
        ("pattern", "pattern"),
    ];

    for (ts_field, uast_field) in &field_mappings {
        if let Some(field_node) = node.child_by_field_name(ts_field) {
            obj[*uast_field] = map_node(field_node, source, language, mappings);
        }
    }

    // Recursively map named children (filter out syntax tokens like punctuation)
    let children: Vec<Value> = (0..node.named_child_count() as usize)
        .filter_map(|i| node.named_child(i as u32))
        .filter(|child| should_include_child(child))
        .map(|child| map_node(child, source, language, mappings))
        .collect();

    if !children.is_empty() {
        obj["children"] = json!(children);
    }

    obj
}

/// Get the source text for a node.
fn get_node_text<'a>(node: tree_sitter::Node<'a>, source: &'a str) -> &'a str {
    let start = node.start_byte();
    let end = node.end_byte();
    if end <= source.len() && start <= end {
        &source[start..end]
    } else {
        ""
    }
}

/// Determine if a child node should be included in the children array.
/// Filters out pure syntax tokens while keeping semantically meaningful nodes.
fn should_include_child(node: &tree_sitter::Node) -> bool {
    // Always include named nodes
    if node.is_named() {
        return true;
    }

    // Filter out common syntax tokens
    let node_type = node.kind();
    !matches!(
        node_type,
        "("
            | ")"
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(feature = "builtin-grammars")]
    fn test_parse_rust_builtin() {
        let source = r#"
fn main() {
    println!("Hello, world!");
}
"#;
        let result = parse_to_uast_json("rust", source, Some("test.rs"));
        assert!(result.is_ok(), "Failed to parse Rust: {:?}", result);

        let json_str = result.unwrap();
        assert!(json_str.contains("\"language\": \"rust\""));
        assert!(json_str.contains("\"grammarSource\": \"builtin\""));
        assert!(json_str.contains("\"sourcePath\": \"test.rs\""));
    }

    #[test]
    #[cfg(feature = "builtin-grammars")]
    fn test_parse_c_builtin() {
        let source = r#"
int main() {
    printf("Hello, world!\n");
    return 0;
}
"#;
        let result = parse_to_uast_json("c", source, Some("test.c"));
        assert!(result.is_ok(), "Failed to parse C: {:?}", result);

        let json_str = result.unwrap();
        assert!(json_str.contains("\"language\": \"c\""));
        assert!(json_str.contains("\"grammarSource\": \"builtin\""));
    }

    #[test]
    #[cfg(feature = "builtin-grammars")]
    fn test_parse_cpp_builtin() {
        let source = r#"
#include <iostream>

int main() {
    std::cout << "Hello, world!" << std::endl;
    return 0;
}
"#;
        let result = parse_to_uast_json("cpp", source, Some("test.cpp"));
        assert!(result.is_ok(), "Failed to parse C++: {:?}", result);

        let json_str = result.unwrap();
        assert!(json_str.contains("\"language\": \"cpp\""));
        assert!(json_str.contains("\"grammarSource\": \"builtin\""));
    }

    #[test]
    #[cfg(feature = "builtin-grammars")]
    fn test_parse_rust_function() {
        let source = r#"
pub fn add(a: i32, b: i32) -> i32 {
    a + b
}
"#;
        let result = parse_to_uast_json("rust", source, None);
        assert!(result.is_ok());

        let json_str = result.unwrap();
        // Verify the function name is captured
        assert!(json_str.contains("\"add\""));
    }

    #[test]
    #[cfg(feature = "builtin-grammars")]
    fn test_parse_with_errors() {
        // Invalid Rust syntax - parser should still return a tree with error nodes
        let source = "fn main( { }";
        let result = parse_to_uast_json("rust", source, None);
        // Parse should succeed (tree-sitter is error-tolerant)
        assert!(result.is_ok());
    }

    #[test]
    fn test_parse_unregistered_language() {
        // Without builtin-grammars, or for unknown languages
        let source = "console.log('hello');";
        let result = parse_to_uast_json("javascript", source, None);
        // Should fail because JavaScript is not a built-in grammar
        assert!(result.is_err());
    }

    #[test]
    #[cfg(feature = "builtin-grammars")]
    fn test_uast_structure() {
        let source = "fn foo() {}";
        let result = parse_to_uast_json("rust", source, None);
        assert!(result.is_ok());

        let json_str = result.unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json_str).unwrap();

        // Check top-level fields
        assert_eq!(parsed["version"], "1.0");
        assert_eq!(parsed["language"], "rust");
        assert_eq!(parsed["grammarSource"], "builtin");
        assert!(parsed["root"].is_object());

        // Check root node has expected fields
        let root = &parsed["root"];
        assert!(root["nodeKind"].is_string());
        assert!(root["span"].is_object());
        assert!(root["extensions"].is_object());
    }
}
