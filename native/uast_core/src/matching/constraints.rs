//! Match constraints for pattern matching.
//!
//! This module provides constraint types that can be applied to filter or validate
//! pattern matches based on various criteria.

use super::matcher::PatternMatcher;
use super::pattern::PatternNode;
use crate::uast::schema::UastNode;
use regex::Regex;
use std::fmt::Debug;

/// Specifies how far to traverse when checking relational constraints.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StopBehavior {
    /// Only check immediate parent/children.
    Neighbor,
    /// Check all ancestors/descendants until end.
    End,
}

/// Base trait for all constraints.
pub trait Constraint: Debug + Send + Sync {
    /// Evaluate this constraint against a node.
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool;

    /// Clone this constraint into a boxed trait object.
    fn clone_box(&self) -> Box<dyn Constraint>;
}

impl Clone for Box<dyn Constraint> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

// ============================================================================
// Basic Constraints
// ============================================================================

/// Matches node's source text against a regular expression.
#[derive(Debug, Clone)]
pub struct RegexConstraint {
    /// The compiled regex pattern.
    pub pattern: Regex,
}

impl RegexConstraint {
    /// Create a new regex constraint.
    pub fn new(pattern: &str) -> Result<Self, regex::Error> {
        Ok(RegexConstraint {
            pattern: Regex::new(pattern)?,
        })
    }
}

impl Constraint for RegexConstraint {
    fn evaluate(&self, node: &UastNode, _matcher: &PatternMatcher) -> bool {
        if let Some(text) = &node.text {
            self.pattern.is_match(text)
        } else if let Some(name) = &node.name {
            self.pattern.is_match(name)
        } else {
            false
        }
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Matches nodes by their node kind.
#[derive(Debug, Clone)]
pub struct KindConstraint {
    /// The required node kind as a string.
    pub kind: String,
}

impl KindConstraint {
    /// Create a new kind constraint.
    pub fn new(kind: impl Into<String>) -> Self {
        KindConstraint { kind: kind.into() }
    }
}

impl Constraint for KindConstraint {
    fn evaluate(&self, node: &UastNode, _matcher: &PatternMatcher) -> bool {
        node.kind.as_str().eq_ignore_ascii_case(&self.kind)
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

// ============================================================================
// Logical Constraints
// ============================================================================

/// Negates another constraint.
#[derive(Debug, Clone)]
pub struct NotConstraint {
    /// The constraint to negate.
    pub inner: Box<dyn Constraint>,
}

impl NotConstraint {
    /// Create a new negation constraint.
    pub fn new(inner: Box<dyn Constraint>) -> Self {
        NotConstraint { inner }
    }
}

impl Constraint for NotConstraint {
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool {
        !self.inner.evaluate(node, matcher)
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires all inner constraints to pass (AND).
#[derive(Debug, Clone)]
pub struct AllConstraint {
    /// All constraints that must pass.
    pub constraints: Vec<Box<dyn Constraint>>,
}

impl AllConstraint {
    /// Create a new all (AND) constraint.
    pub fn new(constraints: Vec<Box<dyn Constraint>>) -> Self {
        AllConstraint { constraints }
    }
}

impl Constraint for AllConstraint {
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool {
        self.constraints.iter().all(|c| c.evaluate(node, matcher))
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires any inner constraint to pass (OR).
#[derive(Debug, Clone)]
pub struct AnyConstraint {
    /// Any of these constraints must pass.
    pub constraints: Vec<Box<dyn Constraint>>,
}

impl AnyConstraint {
    /// Create a new any (OR) constraint.
    pub fn new(constraints: Vec<Box<dyn Constraint>>) -> Self {
        AnyConstraint { constraints }
    }
}

impl Constraint for AnyConstraint {
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool {
        self.constraints.iter().any(|c| c.evaluate(node, matcher))
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

// ============================================================================
// Structural Constraints
// ============================================================================

/// Requires the node to appear inside an ancestor matching a pattern.
#[derive(Debug, Clone)]
pub struct InsideConstraint {
    /// The pattern the ancestor must match.
    pub ancestor_pattern: PatternNode,
    /// How far to search up the tree.
    pub stop_by: StopBehavior,
}

impl InsideConstraint {
    /// Create a new inside constraint.
    pub fn new(ancestor_pattern: PatternNode, stop_by: StopBehavior) -> Self {
        InsideConstraint {
            ancestor_pattern,
            stop_by,
        }
    }
}

impl Constraint for InsideConstraint {
    fn evaluate(&self, _node: &UastNode, _matcher: &PatternMatcher) -> bool {
        // Note: This requires parent tracking which isn't currently in UastNode
        // For now, return true (constraint not enforced without parent refs)
        // TODO: Add parent tracking to UastNode or use a context-aware matcher
        true
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires the node to NOT appear inside an ancestor matching a pattern.
#[derive(Debug, Clone)]
pub struct NotInsideConstraint {
    /// The pattern the ancestor must NOT match.
    pub ancestor_pattern: PatternNode,
    /// How far to search up the tree.
    pub stop_by: StopBehavior,
}

impl NotInsideConstraint {
    /// Create a new not-inside constraint.
    pub fn new(ancestor_pattern: PatternNode, stop_by: StopBehavior) -> Self {
        NotInsideConstraint {
            ancestor_pattern,
            stop_by,
        }
    }
}

impl Constraint for NotInsideConstraint {
    fn evaluate(&self, _node: &UastNode, _matcher: &PatternMatcher) -> bool {
        // Note: Requires parent tracking
        true
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires the node to have a descendant matching a pattern.
#[derive(Debug, Clone)]
pub struct HasConstraint {
    /// The pattern a descendant must match.
    pub descendant_pattern: PatternNode,
    /// How far to search down the tree.
    pub stop_by: StopBehavior,
}

impl HasConstraint {
    /// Create a new has constraint.
    pub fn new(descendant_pattern: PatternNode, stop_by: StopBehavior) -> Self {
        HasConstraint {
            descendant_pattern,
            stop_by,
        }
    }
}

impl Constraint for HasConstraint {
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool {
        let stop_at_neighbor = self.stop_by == StopBehavior::Neighbor;
        matcher.has_matching_descendant(node, &self.descendant_pattern, stop_at_neighbor)
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires the node to NOT have a descendant matching a pattern.
#[derive(Debug, Clone)]
pub struct NotHasConstraint {
    /// The pattern no descendant should match.
    pub descendant_pattern: PatternNode,
    /// How far to search down the tree.
    pub stop_by: StopBehavior,
}

impl NotHasConstraint {
    /// Create a new not-has constraint.
    pub fn new(descendant_pattern: PatternNode, stop_by: StopBehavior) -> Self {
        NotHasConstraint {
            descendant_pattern,
            stop_by,
        }
    }
}

impl Constraint for NotHasConstraint {
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool {
        let stop_at_neighbor = self.stop_by == StopBehavior::Neighbor;
        !matcher.has_matching_descendant(node, &self.descendant_pattern, stop_at_neighbor)
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

// ============================================================================
// Sibling Constraints
// ============================================================================

/// Requires a sibling after this node to match a pattern.
#[derive(Debug, Clone)]
pub struct PrecedesConstraint {
    /// The pattern a following sibling must match.
    pub sibling_pattern: PatternNode,
    /// Whether the sibling must be immediately after.
    pub immediate: bool,
}

impl PrecedesConstraint {
    /// Create a new precedes constraint.
    pub fn new(sibling_pattern: PatternNode, immediate: bool) -> Self {
        PrecedesConstraint {
            sibling_pattern,
            immediate,
        }
    }
}

impl Constraint for PrecedesConstraint {
    fn evaluate(&self, _node: &UastNode, _matcher: &PatternMatcher) -> bool {
        // Note: Requires parent tracking to access siblings
        true
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires a sibling before this node to match a pattern.
#[derive(Debug, Clone)]
pub struct FollowsConstraint {
    /// The pattern a preceding sibling must match.
    pub sibling_pattern: PatternNode,
    /// Whether the sibling must be immediately before.
    pub immediate: bool,
}

impl FollowsConstraint {
    /// Create a new follows constraint.
    pub fn new(sibling_pattern: PatternNode, immediate: bool) -> Self {
        FollowsConstraint {
            sibling_pattern,
            immediate,
        }
    }
}

impl Constraint for FollowsConstraint {
    fn evaluate(&self, _node: &UastNode, _matcher: &PatternMatcher) -> bool {
        // Note: Requires parent tracking to access siblings
        true
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

// ============================================================================
// Other Constraints
// ============================================================================

/// Matches the node against a pattern.
#[derive(Debug, Clone)]
pub struct PatternConstraint {
    /// The pattern to match.
    pub pattern: PatternNode,
}

impl PatternConstraint {
    /// Create a new pattern constraint.
    pub fn new(pattern: PatternNode) -> Self {
        PatternConstraint { pattern }
    }
}

impl Constraint for PatternConstraint {
    fn evaluate(&self, node: &UastNode, matcher: &PatternMatcher) -> bool {
        let mut env = std::collections::HashMap::new();
        matcher.try_match_node(node, &self.pattern, &mut env)
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Matches based on the length of captured collection.
#[derive(Debug, Clone)]
pub struct LengthConstraint {
    /// Minimum length (inclusive).
    pub min_length: Option<usize>,
    /// Maximum length (inclusive).
    pub max_length: Option<usize>,
    /// Exact length required.
    pub exact_length: Option<usize>,
}

impl LengthConstraint {
    /// Create a constraint for exact length.
    pub fn exact(length: usize) -> Self {
        LengthConstraint {
            min_length: None,
            max_length: None,
            exact_length: Some(length),
        }
    }

    /// Create a constraint for minimum length.
    pub fn at_least(length: usize) -> Self {
        LengthConstraint {
            min_length: Some(length),
            max_length: None,
            exact_length: None,
        }
    }

    /// Create a constraint for maximum length.
    pub fn at_most(length: usize) -> Self {
        LengthConstraint {
            min_length: None,
            max_length: Some(length),
            exact_length: None,
        }
    }

    /// Create a constraint for a range.
    pub fn between(min: usize, max: usize) -> Self {
        LengthConstraint {
            min_length: Some(min),
            max_length: Some(max),
            exact_length: None,
        }
    }
}

impl Constraint for LengthConstraint {
    fn evaluate(&self, _node: &UastNode, _matcher: &PatternMatcher) -> bool {
        // For a single node, count is 1 (matches C# behavior)
        let count = 1;

        if let Some(exact) = self.exact_length {
            if count != exact {
                return false;
            }
        }

        if let Some(min) = self.min_length {
            if count < min {
                return false;
            }
        }

        if let Some(max) = self.max_length {
            if count > max {
                return false;
            }
        }

        true
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::uast::schema::{SourceSpan, UastKind};

    fn make_node(kind: UastKind, text: Option<&str>) -> UastNode {
        let mut node = UastNode::new(kind, "rust", SourceSpan::empty());
        if let Some(t) = text {
            node = node.with_text(t);
        }
        node
    }

    #[test]
    fn test_regex_constraint() {
        let constraint = RegexConstraint::new(r"^my_").unwrap();
        let matcher = PatternMatcher::new();

        let node = make_node(UastKind::Identifier, Some("my_function"));
        assert!(constraint.evaluate(&node, &matcher));

        let node = make_node(UastKind::Identifier, Some("other_function"));
        assert!(!constraint.evaluate(&node, &matcher));
    }

    #[test]
    fn test_kind_constraint() {
        let constraint = KindConstraint::new("FunctionDeclaration");
        let matcher = PatternMatcher::new();

        let node = make_node(UastKind::FunctionDeclaration, None);
        assert!(constraint.evaluate(&node, &matcher));

        let node = make_node(UastKind::IfStatement, None);
        assert!(!constraint.evaluate(&node, &matcher));
    }

    #[test]
    fn test_not_constraint() {
        let inner = Box::new(KindConstraint::new("FunctionDeclaration"));
        let constraint = NotConstraint::new(inner);
        let matcher = PatternMatcher::new();

        let node = make_node(UastKind::IfStatement, None);
        assert!(constraint.evaluate(&node, &matcher));

        let node = make_node(UastKind::FunctionDeclaration, None);
        assert!(!constraint.evaluate(&node, &matcher));
    }

    #[test]
    fn test_all_constraint() {
        let constraints: Vec<Box<dyn Constraint>> = vec![
            Box::new(KindConstraint::new("FunctionDeclaration")),
            Box::new(RegexConstraint::new(r"^test").unwrap()),
        ];
        let constraint = AllConstraint::new(constraints);
        let matcher = PatternMatcher::new();

        let mut node = make_node(UastKind::FunctionDeclaration, None);
        node = node.with_name("test_function");
        // Note: regex checks text, not name, so this won't match
        // Let's fix by using text
        let mut node = make_node(UastKind::FunctionDeclaration, Some("test_value"));
        assert!(constraint.evaluate(&node, &matcher));
    }

    #[test]
    fn test_any_constraint() {
        let constraints: Vec<Box<dyn Constraint>> = vec![
            Box::new(KindConstraint::new("FunctionDeclaration")),
            Box::new(KindConstraint::new("MethodDeclaration")),
        ];
        let constraint = AnyConstraint::new(constraints);
        let matcher = PatternMatcher::new();

        let node = make_node(UastKind::FunctionDeclaration, None);
        assert!(constraint.evaluate(&node, &matcher));

        let node = make_node(UastKind::MethodDeclaration, None);
        assert!(constraint.evaluate(&node, &matcher));

        let node = make_node(UastKind::IfStatement, None);
        assert!(!constraint.evaluate(&node, &matcher));
    }

    #[test]
    fn test_has_constraint() {
        let child = make_node(UastKind::Identifier, Some("x"));
        let root = UastNode::new(UastKind::FunctionDeclaration, "rust", SourceSpan::empty())
            .with_child(child);

        let constraint = HasConstraint::new(
            PatternNode::Kind(UastKind::Identifier),
            StopBehavior::End,
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&root, &matcher));
    }

    #[test]
    fn test_not_has_constraint() {
        let child = make_node(UastKind::Identifier, Some("x"));
        let root = UastNode::new(UastKind::FunctionDeclaration, "rust", SourceSpan::empty())
            .with_child(child);

        let constraint = NotHasConstraint::new(
            PatternNode::Kind(UastKind::IfStatement),
            StopBehavior::End,
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&root, &matcher));
    }

    #[test]
    fn test_length_constraint_exact() {
        let constraint = LengthConstraint::exact(1);
        let matcher = PatternMatcher::new();
        let node = make_node(UastKind::Identifier, None);

        assert!(constraint.evaluate(&node, &matcher));
    }

    #[test]
    fn test_length_constraint_range() {
        let constraint = LengthConstraint::between(0, 5);
        let matcher = PatternMatcher::new();
        let node = make_node(UastKind::Identifier, None);

        assert!(constraint.evaluate(&node, &matcher));
    }
}
