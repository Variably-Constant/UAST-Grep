//! UAST (Universal Abstract Syntax Tree) Module
//!
//! This module provides functionality to convert tree-sitter parse trees into
//! a language-agnostic UAST representation, both as JSON and as strongly-typed
//! Rust structures.
//!
//! # Overview
//!
//! The UAST mapper transforms tree-sitter's concrete syntax trees into a simplified,
//! universal format that can be consumed by analysis tools regardless of the source
//! language. This enables:
//!
//! - Cross-language code analysis
//! - Unified AST queries
//! - Language-agnostic pattern matching
//!
//! # Architecture
//!
//! - `builtin`: Built-in grammar support (when `builtin-grammars` feature is enabled)
//! - `mappings`: Static mapping tables from tree-sitter node types to UAST NodeKind strings
//! - `mapper`: The core transformation logic that converts tree-sitter nodes to JSON
//! - `schema`: Strongly-typed Rust UAST schema (UastKind, UastNode, SourceSpan, etc.)
//! - `convert`: Tree-sitter to UAST converter producing typed structures

pub mod builtin;
pub mod convert;
pub mod mappings;
mod mapper;
pub mod schema;

// Re-export builtin language utilities
pub use builtin::{
    available_builtin_extensions, available_builtin_languages, get_builtin_language,
    get_builtin_language_for_extension, get_builtin_language_name_for_extension,
    is_builtin_language,
};

// Re-export mappings utilities
pub use mappings::{get_mappings, get_native_types_for_uast, is_uast_pattern, NodeKindMappings};

// Re-export JSON mapper (for backward compatibility)
pub use mapper::parse_to_uast_json;

// Re-export schema types (renamed to avoid collision with ffi::UastNode)
pub use schema::{
    AssignmentOperator, BinaryOperator, LiteralKind, SourceSpan, UastKind,
    UastNode as TypedUastNode, UnaryOperator, Visibility,
};

// Re-export converter types and functions
pub use convert::{
    collect_parse_errors, convert_node_to_uast, convert_tree_to_uast,
    convert_tree_to_uast_with_options, create_uast_document, create_uast_document_with_options,
    ConvertOptions, ParseError, UastConverter, UastDocument,
};
