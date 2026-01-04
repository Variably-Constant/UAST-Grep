//! Node operations and FFI conversion utilities.
//!
//! This module provides functions to convert tree-sitter nodes to FFI-safe
//! representations that can be passed across the C#/Rust boundary.

use crate::ffi::{UastNode, UastPoint, UastRange};
use std::ffi::c_char;

// ============================================================================
// Node Conversion
// ============================================================================

/// Convert a tree-sitter Node to an FFI-safe UastNode.
///
/// The returned node contains pointers to strings that are only valid
/// while the source tree is alive. The caller must ensure the tree
/// is not freed while using the node.
///
/// # Arguments
///
/// * `node` - The tree-sitter node to convert
/// * `field_name` - Optional field name if this node is a named field of its parent
///
/// # Safety
///
/// The returned `UastNode` contains pointers that are only valid while
/// the tree that owns the node is alive.
pub fn node_to_ffi(node: tree_sitter::Node<'_>, field_name: Option<&'static str>) -> UastNode {
    let kind = node.kind();
    let kind_ptr = kind.as_ptr() as *const c_char;
    let kind_len = kind.len() as u32;

    let (field_name_ptr, field_name_len) = match field_name {
        Some(name) => (name.as_ptr() as *const c_char, name.len() as u32),
        None => (std::ptr::null(), 0),
    };

    let start_point = node.start_position();
    let end_point = node.end_position();

    UastNode {
        kind: kind_ptr,
        kind_len,
        field_name: field_name_ptr,
        field_name_len,
        range: UastRange {
            start_byte: node.start_byte() as u32,
            end_byte: node.end_byte() as u32,
            start_point: UastPoint {
                row: start_point.row as u32,
                column: start_point.column as u32,
            },
            end_point: UastPoint {
                row: end_point.row as u32,
                column: end_point.column as u32,
            },
        },
        child_count: node.child_count() as u32,
        named_child_count: node.named_child_count() as u32,
        is_named: node.is_named(),
        is_missing: node.is_missing(),
        has_error: node.has_error(),
        // Use the node's unique id in the tree
        id: node.id() as u64,
        parent_id: node.parent().map_or(0, |p| p.id() as u64),
    }
}

/// Convert a tree-sitter Node to a UastNode, inferring field name from cursor.
///
/// This is a convenience function for use during tree traversal with a cursor.
///
/// # Arguments
///
/// * `cursor` - The tree cursor positioned at the node to convert
pub fn node_from_cursor(cursor: &tree_sitter::TreeCursor<'_>) -> UastNode {
    node_to_ffi(cursor.node(), cursor.field_name())
}

// ============================================================================
// Text Extraction Helpers
// ============================================================================

/// Extract the text for a node from source code.
///
/// # Arguments
///
/// * `node` - The node to get text for
/// * `source` - The source code the node was parsed from
///
/// # Returns
///
/// The substring of source corresponding to the node's byte range.
/// Returns an empty string if the range is invalid.
pub fn node_text<'a>(node: tree_sitter::Node<'_>, source: &'a str) -> &'a str {
    let range = node.byte_range();
    if range.start <= source.len() && range.end <= source.len() {
        &source[range]
    } else {
        ""
    }
}

/// Get the text for a node with UTF-8 validation.
///
/// # Arguments
///
/// * `node` - The node to get text for
/// * `source` - The source bytes
///
/// # Returns
///
/// The text as a string slice, or None if the bytes are not valid UTF-8.
pub fn node_text_checked<'a>(node: tree_sitter::Node<'_>, source: &'a [u8]) -> Option<&'a str> {
    let range = node.byte_range();
    if range.start <= source.len() && range.end <= source.len() {
        std::str::from_utf8(&source[range]).ok()
    } else {
        None
    }
}

// ============================================================================
// Node Navigation Helpers
// ============================================================================

/// Find a child node by field name.
///
/// # Arguments
///
/// * `node` - The parent node
/// * `field_name` - Name of the field to find
///
/// # Returns
///
/// The child node if found, or None.
pub fn child_by_field_name<'a>(
    node: tree_sitter::Node<'a>,
    field_name: &str,
) -> Option<tree_sitter::Node<'a>> {
    node.child_by_field_name(field_name)
}

/// Get all named children of a node.
///
/// # Arguments
///
/// * `node` - The parent node
///
/// # Returns
///
/// An iterator over named child nodes.
pub fn named_children<'a>(
    node: tree_sitter::Node<'a>,
) -> impl Iterator<Item = tree_sitter::Node<'a>> {
    let mut cursor = node.walk();
    node.named_children(&mut cursor).collect::<Vec<_>>().into_iter()
}

/// Get all children of a node (including anonymous).
///
/// # Arguments
///
/// * `node` - The parent node
///
/// # Returns
///
/// An iterator over all child nodes.
pub fn all_children<'a>(
    node: tree_sitter::Node<'a>,
) -> impl Iterator<Item = tree_sitter::Node<'a>> {
    let mut cursor = node.walk();
    node.children(&mut cursor).collect::<Vec<_>>().into_iter()
}

/// Find the first ancestor matching a predicate.
///
/// # Arguments
///
/// * `node` - The starting node
/// * `predicate` - Function that returns true for matching nodes
///
/// # Returns
///
/// The first matching ancestor, or None if no match found.
pub fn find_ancestor<'a, F>(node: tree_sitter::Node<'a>, mut predicate: F) -> Option<tree_sitter::Node<'a>>
where
    F: FnMut(tree_sitter::Node<'a>) -> bool,
{
    let mut current = node.parent();
    while let Some(parent) = current {
        if predicate(parent) {
            return Some(parent);
        }
        current = parent.parent();
    }
    None
}

/// Find the first descendant matching a predicate (depth-first).
///
/// # Arguments
///
/// * `node` - The starting node
/// * `predicate` - Function that returns true for matching nodes
///
/// # Returns
///
/// The first matching descendant, or None if no match found.
pub fn find_descendant<'a, F>(node: tree_sitter::Node<'a>, mut predicate: F) -> Option<tree_sitter::Node<'a>>
where
    F: FnMut(tree_sitter::Node<'a>) -> bool,
{
    let mut cursor = node.walk();

    // Start at the first child
    if !cursor.goto_first_child() {
        return None;
    }

    loop {
        let current = cursor.node();
        if predicate(current) {
            return Some(current);
        }

        // Try to go deeper
        if cursor.goto_first_child() {
            continue;
        }

        // Try siblings
        if cursor.goto_next_sibling() {
            continue;
        }

        // Backtrack
        loop {
            if !cursor.goto_parent() {
                return None;
            }
            // Check if we've returned to the starting node
            if cursor.node().id() == node.id() {
                return None;
            }
            if cursor.goto_next_sibling() {
                break;
            }
        }
    }
}

/// Count descendants matching a predicate.
///
/// # Arguments
///
/// * `node` - The starting node
/// * `predicate` - Function that returns true for matching nodes
///
/// # Returns
///
/// The count of matching descendants.
pub fn count_descendants<'a, F>(node: tree_sitter::Node<'a>, mut predicate: F) -> usize
where
    F: FnMut(tree_sitter::Node<'a>) -> bool,
{
    let mut count = 0;
    let mut cursor = node.walk();

    if !cursor.goto_first_child() {
        return 0;
    }

    loop {
        if predicate(cursor.node()) {
            count += 1;
        }

        if cursor.goto_first_child() {
            continue;
        }

        if cursor.goto_next_sibling() {
            continue;
        }

        loop {
            if !cursor.goto_parent() {
                return count;
            }
            if cursor.node().id() == node.id() {
                return count;
            }
            if cursor.goto_next_sibling() {
                break;
            }
        }
    }
}

// ============================================================================
// Node Kind Helpers
// ============================================================================

/// Check if a node kind represents an error.
pub fn is_error_kind(kind: &str) -> bool {
    kind == "ERROR" || kind == "MISSING"
}

/// Check if a node is a comment.
///
/// This is a heuristic check that works for most languages.
pub fn is_comment_kind(kind: &str) -> bool {
    kind == "comment"
        || kind == "line_comment"
        || kind == "block_comment"
        || kind == "documentation_comment"
        || kind.ends_with("_comment")
}

/// Check if a node is a string literal.
///
/// This is a heuristic check that works for most languages.
pub fn is_string_kind(kind: &str) -> bool {
    kind == "string"
        || kind == "string_literal"
        || kind == "template_string"
        || kind == "raw_string"
        || kind == "interpolated_string"
        || kind.ends_with("_string")
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    // NOTE: The parsing tests below are marked #[ignore] because they require
    // a language to be registered via uast_register_language(). In production,
    // C# loads grammar DLLs and registers them. For integration testing,
    // run these tests with a registered language.

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_node_to_ffi() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let x = 42;").unwrap();
        let root = tree.root_node();

        let ffi_node = node_to_ffi(root, None);

        assert!(!ffi_node.kind.is_null());
        assert!(ffi_node.kind_len > 0);
        assert!(ffi_node.field_name.is_null());
        assert_eq!(ffi_node.field_name_len, 0);
        assert!(ffi_node.is_named);
        assert!(!ffi_node.has_error);
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_node_to_ffi_with_field() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let x = 42;").unwrap();
        let root = tree.root_node();

        let ffi_node = node_to_ffi(root, Some("test_field"));

        assert!(!ffi_node.field_name.is_null());
        assert_eq!(ffi_node.field_name_len, 10);
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_node_text() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let source = "const x = 42;";
        let tree = parser.parse(source).unwrap();
        let root = tree.root_node();

        let text = node_text(root, source);
        assert_eq!(text, source);
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_child_by_field_name() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("function foo() {}").unwrap();
        let root = tree.root_node();

        // Get the function declaration
        let func = root.child(0).unwrap();
        assert_eq!(func.kind(), "function_declaration");

        // Get the name field
        let name = child_by_field_name(func, "name");
        assert!(name.is_some());
        assert_eq!(tree.node_text(name.unwrap()), "foo");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_named_children() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let x = 1; let y = 2;").unwrap();
        let root = tree.root_node();

        let children: Vec<_> = named_children(root).collect();
        assert_eq!(children.len(), 2);
        assert_eq!(children[0].kind(), "lexical_declaration");
        assert_eq!(children[1].kind(), "lexical_declaration");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_find_ancestor() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("function f() { let x = 1; }").unwrap();

        // Find the number literal
        let number = find_descendant(tree.root_node(), |n| n.kind() == "number").unwrap();

        // Find its function ancestor
        let func = find_ancestor(number, |n| n.kind() == "function_declaration");
        assert!(func.is_some());
        assert_eq!(func.unwrap().kind(), "function_declaration");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_find_descendant() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("function f() { return 42; }").unwrap();
        let root = tree.root_node();

        let number = find_descendant(root, |n| n.kind() == "number");
        assert!(number.is_some());
        assert_eq!(tree.node_text(number.unwrap()), "42");
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_count_descendants() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let a = 1; let b = 2; let c = 3;").unwrap();
        let root = tree.root_node();

        let count = count_descendants(root, |n| n.kind() == "number");
        assert_eq!(count, 3);
    }

    #[test]
    fn test_is_error_kind() {
        assert!(is_error_kind("ERROR"));
        assert!(is_error_kind("MISSING"));
        assert!(!is_error_kind("identifier"));
    }

    #[test]
    fn test_is_comment_kind() {
        assert!(is_comment_kind("comment"));
        assert!(is_comment_kind("line_comment"));
        assert!(is_comment_kind("block_comment"));
        assert!(!is_comment_kind("identifier"));
    }

    #[test]
    fn test_is_string_kind() {
        assert!(is_string_kind("string"));
        assert!(is_string_kind("string_literal"));
        assert!(is_string_kind("template_string"));
        assert!(!is_string_kind("identifier"));
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_node_range() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let x = 42;").unwrap();
        let root = tree.root_node();

        let ffi_node = node_to_ffi(root, None);

        assert_eq!(ffi_node.range.start_byte, 0);
        assert!(ffi_node.range.end_byte > 0);
        assert_eq!(ffi_node.range.start_point.row, 0);
        assert_eq!(ffi_node.range.start_point.column, 0);
    }

    #[test]
    #[ignore = "Requires registered language - run integration tests instead"]
    fn test_node_from_cursor() {
        use crate::parser::Parser;
        let mut parser = Parser::new("javascript").unwrap();
        let tree = parser.parse("let x = 1;").unwrap();
        let mut cursor = tree.tree().walk();

        let ffi_node = node_from_cursor(&cursor);
        assert_eq!(ffi_node.range.start_byte, 0);

        // Move to first child
        cursor.goto_first_child();
        let ffi_child = node_from_cursor(&cursor);
        assert!(!ffi_child.kind.is_null());
    }
}
