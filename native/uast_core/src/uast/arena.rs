//! Arena-allocated UAST tree for memory-efficient scanning.
//!
//! This module provides arena-backed UAST nodes that allocate all memory
//! from a single bump allocator, eliminating per-node allocation overhead.

use bumpalo::Bump;
use crate::uast::schema::{SourceSpan, UastKind};

/// Arena-allocated UAST tree.
///
/// All nodes in the tree are allocated from a single memory pool,
/// which is freed all at once when the arena is dropped.
pub struct UastArena {
    arena: Bump,
}

impl Default for UastArena {
    fn default() -> Self {
        Self::new()
    }
}

impl UastArena {
    /// Create a new arena with default capacity.
    pub fn new() -> Self {
        Self {
            arena: Bump::new(),
        }
    }

    /// Create a new arena with pre-allocated capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            arena: Bump::with_capacity(capacity),
        }
    }

    /// Allocate a node in the arena.
    pub fn alloc_node<'a>(&'a self, node: ArenaUastNode<'a>) -> &'a ArenaUastNode<'a> {
        self.arena.alloc(node)
    }

    /// Allocate a slice of children from an iterator.
    pub fn alloc_children<'a, I>(&'a self, children: I) -> &'a [ArenaUastNode<'a>]
    where
        I: IntoIterator<Item = ArenaUastNode<'a>>,
        I::IntoIter: ExactSizeIterator,
    {
        self.arena.alloc_slice_fill_iter(children)
    }

    /// Allocate a string in the arena.
    pub fn alloc_str(&self, s: &str) -> &str {
        self.arena.alloc_str(s)
    }

    /// Allocate a slice of properties.
    pub fn alloc_properties<'a>(&'a self, props: Vec<(PropertyKey, PropertyValue)>) -> &'a [(PropertyKey, PropertyValue)] {
        if props.is_empty() {
            return &[];
        }
        self.arena.alloc_slice_fill_iter(props)
    }

    /// Get the total allocated bytes in this arena.
    pub fn allocated_bytes(&self) -> usize {
        self.arena.allocated_bytes()
    }

    /// Get the number of chunks allocated.
    pub fn chunk_count(&mut self) -> usize {
        self.arena.iter_allocated_chunks().count()
    }
}

/// Known property keys (no String allocation needed).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PropertyKey {
    /// Function/method is async
    IsAsync,
    /// Function is a generator
    IsGenerator,
    /// Variable declared with const
    IsConst,
    /// Variable declared with let
    IsLet,
    /// Variable declared with var
    IsVar,
    /// Operator symbol (+, -, ==, etc.)
    Operator,
    /// Iterator variable in for-in/for-of
    IteratorVariable,
    /// Export is default export
    IsDefault,
    /// Member is static
    IsStatic,
    /// Member is public
    IsPublic,
    /// Member is private
    IsPrivate,
    /// Member is protected
    IsProtected,
    /// Custom property key (stores index into string table or byte range)
    Custom(u32),
}

impl PropertyKey {
    /// Convert from a string key name.
    pub fn from_str(key: &str) -> Self {
        match key {
            "isAsync" | "is_async" => PropertyKey::IsAsync,
            "isGenerator" | "is_generator" => PropertyKey::IsGenerator,
            "isConst" | "is_const" => PropertyKey::IsConst,
            "isLet" | "is_let" => PropertyKey::IsLet,
            "isVar" | "is_var" => PropertyKey::IsVar,
            "operator" => PropertyKey::Operator,
            "iteratorVariable" | "iterator_variable" => PropertyKey::IteratorVariable,
            "isDefault" | "is_default" => PropertyKey::IsDefault,
            "isStatic" | "is_static" => PropertyKey::IsStatic,
            "isPublic" | "is_public" => PropertyKey::IsPublic,
            "isPrivate" | "is_private" => PropertyKey::IsPrivate,
            "isProtected" | "is_protected" => PropertyKey::IsProtected,
            _ => PropertyKey::Custom(0), // Custom keys need special handling
        }
    }

    /// Get the string representation of this key.
    pub fn as_str(&self) -> &'static str {
        match self {
            PropertyKey::IsAsync => "isAsync",
            PropertyKey::IsGenerator => "isGenerator",
            PropertyKey::IsConst => "isConst",
            PropertyKey::IsLet => "isLet",
            PropertyKey::IsVar => "isVar",
            PropertyKey::Operator => "operator",
            PropertyKey::IteratorVariable => "iteratorVariable",
            PropertyKey::IsDefault => "isDefault",
            PropertyKey::IsStatic => "isStatic",
            PropertyKey::IsPublic => "isPublic",
            PropertyKey::IsPrivate => "isPrivate",
            PropertyKey::IsProtected => "isProtected",
            PropertyKey::Custom(_) => "custom",
        }
    }
}

/// Property values for arena-allocated nodes.
#[derive(Debug, Clone)]
pub enum PropertyValue {
    /// Boolean value
    Bool(bool),
    /// String stored as byte range into source
    ByteRange(u32, u32),
    /// Interned string (for short values like operators)
    Interned(&'static str),
    /// Number value
    Number(f64),
}

impl PropertyValue {
    /// Get as boolean if applicable.
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            PropertyValue::Bool(b) => Some(*b),
            _ => None,
        }
    }

    /// Get the string value, extracting from source if needed.
    pub fn get_string<'a>(&'a self, source: &'a str) -> Option<&'a str> {
        match self {
            PropertyValue::Interned(s) => Some(s),
            PropertyValue::ByteRange(start, end) => {
                let start = *start as usize;
                let end = *end as usize;
                if end <= source.len() && start <= end {
                    Some(&source[start..end])
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Get as number if applicable.
    pub fn as_number(&self) -> Option<f64> {
        match self {
            PropertyValue::Number(n) => Some(*n),
            _ => None,
        }
    }
}

/// Arena-backed UAST node.
///
/// Uses references instead of owned data, with all memory coming from the arena.
#[derive(Debug)]
pub struct ArenaUastNode<'arena> {
    /// The UAST node kind.
    pub kind: UastKind,
    /// Source location span.
    pub span: SourceSpan,
    /// Byte range for text content (lazy extraction).
    pub text_range: Option<(u32, u32)>,
    /// Byte range for node name (lazy extraction).
    pub name_range: Option<(u32, u32)>,
    /// Child nodes (slice instead of Vec).
    pub children: &'arena [ArenaUastNode<'arena>],
    /// Properties (slice instead of HashMap).
    pub properties: &'arena [(PropertyKey, PropertyValue)],
    /// Native tree-sitter node type.
    pub native_type: Option<&'arena str>,
    /// Language identifier.
    pub language: &'arena str,
    /// Whether this is a named node (vs anonymous).
    pub is_named: bool,
}

impl<'arena> ArenaUastNode<'arena> {
    /// Get the text content from source using the byte range.
    pub fn get_text<'a>(&self, source: &'a str) -> Option<&'a str> {
        self.text_range.and_then(|(start, end)| {
            let start = start as usize;
            let end = end as usize;
            if end <= source.len() && start <= end {
                Some(&source[start..end])
            } else {
                None
            }
        })
    }

    /// Get the name from source using the byte range.
    pub fn get_name<'a>(&self, source: &'a str) -> Option<&'a str> {
        self.name_range.and_then(|(start, end)| {
            let start = start as usize;
            let end = end as usize;
            if end <= source.len() && start <= end {
                Some(&source[start..end])
            } else {
                None
            }
        })
    }

    /// Get a property value by key.
    pub fn get_property(&self, key: PropertyKey) -> Option<&PropertyValue> {
        self.properties.iter()
            .find(|(k, _)| *k == key)
            .map(|(_, v)| v)
    }

    /// Check if this node has children.
    pub fn has_children(&self) -> bool {
        !self.children.is_empty()
    }

    /// Get the number of children.
    pub fn child_count(&self) -> usize {
        self.children.len()
    }

    /// Convert to owned UastNode for serialization or API compatibility.
    ///
    /// This allocates new Strings and Vecs, so use sparingly.
    pub fn to_owned(&self, source: &str) -> crate::uast::schema::UastNode {
        use std::collections::HashMap;

        let mut node = crate::uast::schema::UastNode::new(
            self.kind,
            self.language,
            self.span,
        );

        // Set text/name from ranges
        if let Some(text) = self.get_text(source) {
            node.text = Some(text.to_string());
        }
        if let Some(name) = self.get_name(source) {
            node.name = Some(name.to_string());
        }

        // Convert children recursively
        for child in self.children {
            node.children.push(child.to_owned(source));
        }

        // Convert properties
        let mut props = HashMap::new();
        for (key, value) in self.properties {
            let json_value = match value {
                PropertyValue::Bool(b) => serde_json::json!(*b),
                PropertyValue::Number(n) => serde_json::json!(*n),
                PropertyValue::Interned(s) => serde_json::json!(*s),
                PropertyValue::ByteRange(start, end) => {
                    let start = *start as usize;
                    let end = *end as usize;
                    if end <= source.len() && start <= end {
                        serde_json::json!(&source[start..end])
                    } else {
                        serde_json::json!(null)
                    }
                }
            };
            props.insert(key.as_str().to_string(), json_value);
        }
        node.properties = props;

        // Set native type
        node.native_type = self.native_type.map(|s| s.to_string());
        node.is_named = Some(self.is_named);

        node
    }

    /// Iterate over all descendants depth-first.
    pub fn descendants(&self) -> impl Iterator<Item = &ArenaUastNode<'arena>> {
        ArenaDescendants::new(self)
    }

    /// Find all descendants of a specific kind.
    pub fn descendants_of_kind(&self, kind: UastKind) -> Vec<&ArenaUastNode<'arena>> {
        self.descendants().filter(|n| n.kind == kind).collect()
    }
}

/// Iterator over all descendants of an ArenaUastNode.
struct ArenaDescendants<'a, 'arena> {
    stack: Vec<&'a ArenaUastNode<'arena>>,
}

impl<'a, 'arena> ArenaDescendants<'a, 'arena> {
    fn new(root: &'a ArenaUastNode<'arena>) -> Self {
        Self { stack: vec![root] }
    }
}

impl<'a, 'arena> Iterator for ArenaDescendants<'a, 'arena> {
    type Item = &'a ArenaUastNode<'arena>;

    fn next(&mut self) -> Option<Self::Item> {
        let node = self.stack.pop()?;
        // Push children in reverse order so first child is processed first
        for child in node.children.iter().rev() {
            self.stack.push(child);
        }
        Some(node)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_arena_creation() {
        let arena = UastArena::new();
        assert_eq!(arena.allocated_bytes(), 0);
    }

    #[test]
    fn test_arena_with_capacity() {
        let arena = UastArena::with_capacity(4096);
        // Bump allocates a chunk immediately when capacity is requested
        // The allocated bytes should be at least the requested capacity
        assert!(arena.allocated_bytes() >= 4096);
    }

    #[test]
    fn test_alloc_str() {
        let arena = UastArena::new();
        let s = arena.alloc_str("hello");
        assert_eq!(s, "hello");
        assert!(arena.allocated_bytes() > 0);
    }

    #[test]
    fn test_property_key_from_str() {
        assert_eq!(PropertyKey::from_str("isAsync"), PropertyKey::IsAsync);
        assert_eq!(PropertyKey::from_str("is_async"), PropertyKey::IsAsync);
        assert_eq!(PropertyKey::from_str("operator"), PropertyKey::Operator);
        assert!(matches!(PropertyKey::from_str("unknown"), PropertyKey::Custom(_)));
    }

    #[test]
    fn test_property_value_bool() {
        let v = PropertyValue::Bool(true);
        assert_eq!(v.as_bool(), Some(true));
        assert_eq!(v.as_number(), None);
    }

    #[test]
    fn test_property_value_byte_range() {
        let source = "hello world";
        let v = PropertyValue::ByteRange(0, 5);
        assert_eq!(v.get_string(source), Some("hello"));
    }

    #[test]
    fn test_arena_node_get_text() {
        let source = "fn hello() {}";
        let _arena = UastArena::new();

        let node = ArenaUastNode {
            kind: UastKind::FunctionDeclaration,
            span: SourceSpan::empty(),
            text_range: Some((3, 8)), // "hello"
            name_range: Some((3, 8)), // "hello"
            children: &[],
            properties: &[],
            native_type: Some("function_item"),
            language: "rust",
            is_named: true,
        };

        assert_eq!(node.get_text(source), Some("hello"));
        assert_eq!(node.get_name(source), Some("hello"));
    }

    #[test]
    fn test_arena_node_to_owned() {
        let source = "fn hello() {}";
        let _arena = UastArena::new();

        let node = ArenaUastNode {
            kind: UastKind::FunctionDeclaration,
            span: SourceSpan::new(1, 0, 1, 13, 0, 13),
            text_range: None,
            name_range: Some((3, 8)), // "hello"
            children: &[],
            properties: &[],
            native_type: Some("function_item"),
            language: "rust",
            is_named: true,
        };

        let owned = node.to_owned(source);
        assert_eq!(owned.kind, UastKind::FunctionDeclaration);
        assert_eq!(owned.name, Some("hello".to_string()));
        assert_eq!(owned.native_type, Some("function_item".to_string()));
    }

    #[test]
    fn test_arena_allocated_bytes() {
        let arena = UastArena::new();

        // Allocate some strings
        arena.alloc_str("hello");
        arena.alloc_str("world");

        assert!(arena.allocated_bytes() > 0);
    }
}
