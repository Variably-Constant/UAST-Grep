//! Pattern Matching Module for UAST-Grep
//!
//! This module provides pattern matching capabilities against UAST trees,
//! supporting both UAST patterns (cross-language, PascalCase) and native
//! tree-sitter patterns (snake_case/S-expressions).
//!
//! # Pattern Syntax
//!
//! ## UAST Patterns (PascalCase - auto-detected)
//! ```text
//! FunctionDeclaration   - Match by NodeKind
//! $NAME or \u{2200}NAME        - Metavariable capture (single node)
//! $$NAME or \u{2200}\u{2200}NAME      - One or more nodes
//! $$$NAME or \u{2200}\u{2200}\u{2200}NAME    - Zero or more nodes
//! *                     - Wildcard (match any single node)
//! ```
//!
//! ## Native Patterns (snake_case)
//! ```text
//! function_item         - Direct tree-sitter node type (Rust)
//! function_definition   - Direct tree-sitter node type (Python/C)
//! (function_item ...)   - S-expression query (starts with paren)
//! ```
//!
//! # Architecture
//!
//! - `pattern`: Pattern AST types (Pattern, PatternNode, MetavarQuantifier)
//! - `parser`: Parse pattern strings into Pattern AST
//! - `matcher`: Match patterns against UAST trees
//! - `constraints`: Match constraints (kind, regex, structural)
//! - `query_compiler`: Compile UAST patterns to tree-sitter queries

mod constraints;
mod matcher;
mod parser;
mod pattern;
mod query_compiler;

// Re-export public types
pub use constraints::{
    AllConstraint, AnyConstraint, Constraint, FollowsConstraint, HasConstraint, InsideConstraint,
    KindConstraint, LengthConstraint, NotConstraint, NotHasConstraint, NotInsideConstraint,
    PatternConstraint, PrecedesConstraint, RegexConstraint, StopBehavior,
};

pub use matcher::{CapturedValue, MatchResult, PatternMatcher};

pub use parser::{parse_pattern, parse_simple_pattern, PatternParseError};

pub use pattern::{
    MetavarQuantifier, Pattern, PatternNode, METAVAR_PREFIX, METAVAR_PREFIX_FORALL,
};

pub use query_compiler::{compile_to_tree_sitter_query, QueryCompilationError};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::uast::schema::{SourceSpan, UastKind, UastNode};

    fn make_test_node(kind: UastKind, name: Option<&str>) -> UastNode {
        let mut node = UastNode::new(kind, "rust", SourceSpan::empty());
        if let Some(n) = name {
            node = node.with_name(n);
        }
        node
    }

    #[test]
    fn test_parse_simple_uast_pattern() {
        let pattern = parse_simple_pattern("FunctionDeclaration", "rust").unwrap();
        assert!(matches!(pattern.root, PatternNode::Kind(_)));
    }

    #[test]
    fn test_parse_metavar_single() {
        let pattern = parse_simple_pattern("$NAME", "rust").unwrap();
        assert!(matches!(
            pattern.root,
            PatternNode::Metavar {
                quantifier: MetavarQuantifier::Single,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_metavar_one_or_more() {
        let pattern = parse_simple_pattern("$$NAME", "rust").unwrap();
        assert!(matches!(
            pattern.root,
            PatternNode::Metavar {
                quantifier: MetavarQuantifier::OneOrMore,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_metavar_zero_or_more() {
        let pattern = parse_simple_pattern("$$$NAME", "rust").unwrap();
        assert!(matches!(
            pattern.root,
            PatternNode::Metavar {
                quantifier: MetavarQuantifier::ZeroOrMore,
                ..
            }
        ));
    }

    #[test]
    fn test_parse_wildcard() {
        let pattern = parse_simple_pattern("*", "rust").unwrap();
        assert!(matches!(pattern.root, PatternNode::Wildcard));
    }

    #[test]
    fn test_parse_native_pattern() {
        let pattern = parse_simple_pattern("function_item", "rust").unwrap();
        assert!(matches!(pattern.root, PatternNode::Native(_)));
    }

    #[test]
    fn test_parse_s_expression() {
        let pattern =
            parse_simple_pattern("(function_item name: (identifier) @name)", "rust").unwrap();
        assert!(matches!(pattern.root, PatternNode::Native(_)));
    }

    #[test]
    fn test_matcher_kind() {
        let node = make_test_node(UastKind::FunctionDeclaration, Some("my_func"));
        let pattern = parse_simple_pattern("FunctionDeclaration", "rust").unwrap();
        let matcher = PatternMatcher::new();

        let result = matcher.try_match(&node, &pattern);
        assert!(result.is_some());
    }

    #[test]
    fn test_matcher_kind_mismatch() {
        let node = make_test_node(UastKind::IfStatement, None);
        let pattern = parse_simple_pattern("FunctionDeclaration", "rust").unwrap();
        let matcher = PatternMatcher::new();

        let result = matcher.try_match(&node, &pattern);
        assert!(result.is_none());
    }

    #[test]
    fn test_matcher_wildcard() {
        let node = make_test_node(UastKind::FunctionDeclaration, Some("any"));
        let pattern = parse_simple_pattern("*", "rust").unwrap();
        let matcher = PatternMatcher::new();

        let result = matcher.try_match(&node, &pattern);
        assert!(result.is_some());
    }

    #[test]
    fn test_matcher_metavar_capture() {
        let node = make_test_node(UastKind::FunctionDeclaration, Some("my_func"));
        let pattern = parse_simple_pattern("$FUNC", "rust").unwrap();
        let matcher = PatternMatcher::new();

        let result = matcher.try_match(&node, &pattern);
        assert!(result.is_some());
        let match_result = result.unwrap();
        assert!(match_result.captures.contains_key("FUNC"));
    }

    #[test]
    fn test_find_all_matches() {
        let child1 = make_test_node(UastKind::FunctionDeclaration, Some("func1"));
        let child2 = make_test_node(UastKind::IfStatement, None);
        let child3 = make_test_node(UastKind::FunctionDeclaration, Some("func2"));

        let root = UastNode::new(UastKind::SourceFile, "rust", SourceSpan::empty())
            .with_child(child1)
            .with_child(child2)
            .with_child(child3);

        let pattern = parse_simple_pattern("FunctionDeclaration", "rust").unwrap();
        let matcher = PatternMatcher::new();

        let matches = matcher.find_all(&root, &pattern);
        assert_eq!(matches.len(), 2);
    }
}
