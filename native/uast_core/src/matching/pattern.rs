//! Pattern AST types for UAST pattern matching.
//!
//! This module defines the abstract syntax tree for patterns that can be matched
//! against UAST trees.

use crate::uast::schema::UastKind;
use std::collections::HashSet;

/// Primary metavariable prefix character (Section symbol, U+00A7).
/// This is the recommended prefix as it's clearly a symbol and avoids
/// conflicts with PowerShell's $ variable syntax.
pub const METAVAR_PREFIX: char = '\u{00A7}';  // §

/// Alternative metavariable prefix (ForAll symbol, U+2200).
/// Mathematical meaning: "for all" - matches any node.
pub const METAVAR_PREFIX_FORALL: char = '\u{2200}';  // ∀

/// Alternative ASCII-friendly metavariable prefix.
/// Use single quotes in PowerShell to prevent variable expansion: '$NAME'
pub const METAVAR_PREFIX_DOLLAR: char = '$';

/// All valid metavariable prefix characters.
pub const METAVAR_PREFIXES: [char; 3] = [METAVAR_PREFIX, METAVAR_PREFIX_FORALL, METAVAR_PREFIX_DOLLAR];

/// Specifies how many nodes a metavariable matches.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MetavarQuantifier {
    /// Matches exactly one node ($VAR or \u{2200}VAR).
    Single,
    /// Matches one or more nodes ($$VAR or \u{2200}\u{2200}VAR).
    OneOrMore,
    /// Matches zero or more nodes ($$$VAR or \u{2200}\u{2200}\u{2200}VAR).
    ZeroOrMore,
}

impl MetavarQuantifier {
    /// Returns the minimum number of nodes this quantifier matches.
    pub fn min_count(&self) -> usize {
        match self {
            MetavarQuantifier::Single => 1,
            MetavarQuantifier::OneOrMore => 1,
            MetavarQuantifier::ZeroOrMore => 0,
        }
    }

    /// Returns whether this quantifier can match multiple nodes.
    pub fn is_multiple(&self) -> bool {
        matches!(self, MetavarQuantifier::OneOrMore | MetavarQuantifier::ZeroOrMore)
    }
}

/// A node in the pattern AST.
#[derive(Debug, Clone, PartialEq)]
pub enum PatternNode {
    /// Match by UAST node kind (e.g., FunctionDeclaration).
    Kind(UastKind),

    /// A metavariable that captures matched nodes.
    Metavar {
        /// The variable name (without prefix).
        name: String,
        /// How many nodes to match.
        quantifier: MetavarQuantifier,
        /// Whether this is an anonymous variable (_).
        is_anonymous: bool,
    },

    /// A sequence of patterns to match against children.
    Sequence(Vec<PatternNode>),

    /// Match any of the alternatives (OR).
    AnyOf(Vec<PatternNode>),

    /// Match all patterns simultaneously (AND).
    AllOf(Vec<PatternNode>),

    /// Negation - the inner pattern must NOT match.
    Not(Box<PatternNode>),

    /// A native tree-sitter pattern (snake_case type or S-expression).
    Native(String),

    /// Wildcard - matches any single node.
    Wildcard,

    /// Structural pattern with expected properties and children.
    Structural {
        /// The expected node kind (None means any).
        kind: Option<UastKind>,
        /// Expected properties with their pattern values.
        properties: Vec<(String, PatternNode)>,
        /// Expected children patterns.
        children: Vec<PatternNode>,
    },

    /// Literal value to match.
    Literal(LiteralPattern),
}

/// A literal value pattern.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralPattern {
    String(String),
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null,
}

impl PatternNode {
    /// Create a metavariable pattern.
    pub fn metavar(name: impl Into<String>, quantifier: MetavarQuantifier) -> Self {
        let name = name.into();
        let is_anonymous = name == "_";
        PatternNode::Metavar {
            name,
            quantifier,
            is_anonymous,
        }
    }

    /// Create an anonymous metavariable pattern.
    pub fn anonymous(quantifier: MetavarQuantifier) -> Self {
        PatternNode::Metavar {
            name: "_".to_string(),
            quantifier,
            is_anonymous: true,
        }
    }

    /// Create a kind pattern.
    pub fn kind(kind: UastKind) -> Self {
        PatternNode::Kind(kind)
    }

    /// Create a native pattern.
    pub fn native(pattern: impl Into<String>) -> Self {
        PatternNode::Native(pattern.into())
    }

    /// Create a sequence pattern.
    pub fn sequence(patterns: Vec<PatternNode>) -> Self {
        PatternNode::Sequence(patterns)
    }

    /// Create an any-of (OR) pattern.
    pub fn any_of(patterns: Vec<PatternNode>) -> Self {
        PatternNode::AnyOf(patterns)
    }

    /// Create an all-of (AND) pattern.
    pub fn all_of(patterns: Vec<PatternNode>) -> Self {
        PatternNode::AllOf(patterns)
    }

    /// Create a negation pattern.
    pub fn not(inner: PatternNode) -> Self {
        PatternNode::Not(Box::new(inner))
    }

    /// Create a structural pattern.
    pub fn structural(
        kind: Option<UastKind>,
        properties: Vec<(String, PatternNode)>,
        children: Vec<PatternNode>,
    ) -> Self {
        PatternNode::Structural {
            kind,
            properties,
            children,
        }
    }

    /// Check if this is a metavariable pattern.
    pub fn is_metavar(&self) -> bool {
        matches!(self, PatternNode::Metavar { .. })
    }

    /// Check if this is a multiple-match metavariable.
    pub fn is_multiple_metavar(&self) -> bool {
        matches!(
            self,
            PatternNode::Metavar {
                quantifier: MetavarQuantifier::OneOrMore | MetavarQuantifier::ZeroOrMore,
                ..
            }
        )
    }

    /// Check if this is a wildcard pattern.
    pub fn is_wildcard(&self) -> bool {
        matches!(self, PatternNode::Wildcard)
    }

    /// Check if this is a native pattern.
    pub fn is_native(&self) -> bool {
        matches!(self, PatternNode::Native(_))
    }

    /// Get all metavariable names in this pattern (recursively).
    pub fn collect_metavars(&self) -> HashSet<String> {
        let mut result = HashSet::new();
        self.collect_metavars_into(&mut result);
        result
    }

    fn collect_metavars_into(&self, set: &mut HashSet<String>) {
        match self {
            PatternNode::Metavar {
                name,
                is_anonymous: false,
                ..
            } => {
                set.insert(name.clone());
            }
            PatternNode::Sequence(patterns)
            | PatternNode::AnyOf(patterns)
            | PatternNode::AllOf(patterns) => {
                for p in patterns {
                    p.collect_metavars_into(set);
                }
            }
            PatternNode::Not(inner) => {
                inner.collect_metavars_into(set);
            }
            PatternNode::Structural {
                properties,
                children,
                ..
            } => {
                for (_, p) in properties {
                    p.collect_metavars_into(set);
                }
                for p in children {
                    p.collect_metavars_into(set);
                }
            }
            _ => {}
        }
    }
}

/// A compiled pattern ready for matching.
#[derive(Debug, Clone)]
pub struct Pattern {
    /// The root pattern node.
    pub root: PatternNode,
    /// The original pattern source text.
    pub source_text: String,
    /// The target language for this pattern.
    pub language: String,
    /// Set of metavariable names used in the pattern.
    pub metavariables: HashSet<String>,
    /// Whether this is a native tree-sitter pattern.
    pub is_native: bool,
}

impl Pattern {
    /// Create a new pattern.
    pub fn new(root: PatternNode, source_text: String, language: String) -> Self {
        let metavariables = root.collect_metavars();
        let is_native = matches!(root, PatternNode::Native(_));
        Pattern {
            root,
            source_text,
            language,
            metavariables,
            is_native,
        }
    }

    /// Check if this pattern uses any metavariables.
    pub fn has_metavars(&self) -> bool {
        !self.metavariables.is_empty()
    }

    /// Check if this pattern is a simple kind-only pattern.
    pub fn is_simple_kind(&self) -> bool {
        matches!(self.root, PatternNode::Kind(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_metavar_quantifier_min_count() {
        assert_eq!(MetavarQuantifier::Single.min_count(), 1);
        assert_eq!(MetavarQuantifier::OneOrMore.min_count(), 1);
        assert_eq!(MetavarQuantifier::ZeroOrMore.min_count(), 0);
    }

    #[test]
    fn test_metavar_quantifier_is_multiple() {
        assert!(!MetavarQuantifier::Single.is_multiple());
        assert!(MetavarQuantifier::OneOrMore.is_multiple());
        assert!(MetavarQuantifier::ZeroOrMore.is_multiple());
    }

    #[test]
    fn test_pattern_node_metavar() {
        let node = PatternNode::metavar("FOO", MetavarQuantifier::Single);
        assert!(node.is_metavar());
        assert!(!node.is_multiple_metavar());

        let node = PatternNode::metavar("BAR", MetavarQuantifier::ZeroOrMore);
        assert!(node.is_metavar());
        assert!(node.is_multiple_metavar());
    }

    #[test]
    fn test_pattern_node_anonymous() {
        let node = PatternNode::anonymous(MetavarQuantifier::Single);
        if let PatternNode::Metavar {
            name,
            is_anonymous,
            ..
        } = node
        {
            assert_eq!(name, "_");
            assert!(is_anonymous);
        } else {
            panic!("Expected Metavar");
        }
    }

    #[test]
    fn test_collect_metavars() {
        let pattern = PatternNode::sequence(vec![
            PatternNode::metavar("A", MetavarQuantifier::Single),
            PatternNode::metavar("B", MetavarQuantifier::OneOrMore),
            PatternNode::anonymous(MetavarQuantifier::ZeroOrMore), // Should not be collected
        ]);

        let vars = pattern.collect_metavars();
        assert!(vars.contains("A"));
        assert!(vars.contains("B"));
        assert!(!vars.contains("_"));
        assert_eq!(vars.len(), 2);
    }

    #[test]
    fn test_pattern_new() {
        let root = PatternNode::metavar("X", MetavarQuantifier::Single);
        let pattern = Pattern::new(root, "$X".to_string(), "rust".to_string());

        assert!(pattern.has_metavars());
        assert!(pattern.metavariables.contains("X"));
        assert!(!pattern.is_native);
    }

    #[test]
    fn test_pattern_is_simple_kind() {
        let pattern = Pattern::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            "FunctionDeclaration".to_string(),
            "rust".to_string(),
        );
        assert!(pattern.is_simple_kind());
        assert!(!pattern.is_native);

        let pattern = Pattern::new(
            PatternNode::Native("function_item".to_string()),
            "function_item".to_string(),
            "rust".to_string(),
        );
        assert!(!pattern.is_simple_kind());
        assert!(pattern.is_native);
    }
}
