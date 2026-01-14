//! Match constraints for pattern matching.
//!
//! This module provides constraint types that can be applied to filter or validate
//! pattern matches based on various criteria.

use super::matcher::PatternMatcher;
use super::pattern::PatternNode;
use crate::uast::schema::UastNode;
use regex::Regex;
use std::collections::HashMap;
use std::fmt::Debug;

// ============================================================================
// Match Context - For ancestor/sibling access during constraint evaluation
// ============================================================================

/// Context passed during matching for ancestor/sibling access.
///
/// This enables constraints like `inside`, `not_inside`, `precedes`, and `follows`
/// to access the node's ancestors and siblings without storing parent pointers
/// in `UastNode` (which would create ownership issues in Rust).
#[derive(Debug, Clone)]
pub struct MatchContext<'a> {
    /// Ancestor chain from root to parent (not including current node).
    /// Index 0 is the root, last element is the immediate parent.
    pub ancestors: Vec<&'a UastNode>,
    /// Index of current node within parent's children (for sibling access).
    pub sibling_index: usize,
}

impl<'a> MatchContext<'a> {
    /// Create a new match context.
    pub fn new(ancestors: Vec<&'a UastNode>, sibling_index: usize) -> Self {
        Self {
            ancestors,
            sibling_index,
        }
    }

    /// Get the immediate parent, if any.
    pub fn parent(&self) -> Option<&'a UastNode> {
        self.ancestors.last().copied()
    }

    /// Get siblings that come after the current node.
    pub fn following_siblings(&self) -> &[UastNode] {
        if let Some(parent) = self.parent() {
            if self.sibling_index + 1 < parent.children.len() {
                return &parent.children[self.sibling_index + 1..];
            }
        }
        &[]
    }

    /// Get siblings that come before the current node.
    pub fn preceding_siblings(&self) -> &[UastNode] {
        if let Some(parent) = self.parent() {
            if self.sibling_index > 0 {
                return &parent.children[..self.sibling_index];
            }
        }
        &[]
    }
}

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
    ///
    /// # Arguments
    /// * `node` - The node to evaluate the constraint against
    /// * `matcher` - The pattern matcher for sub-pattern matching
    /// * `source` - Source code for lazy text extraction from byte ranges
    /// * `context` - Match context providing access to ancestors and siblings
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool;

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
    fn evaluate(
        &self,
        node: &UastNode,
        _matcher: &PatternMatcher,
        source: &str,
        _context: &MatchContext,
    ) -> bool {
        if let Some(text) = node.get_text(source) {
            if self.pattern.is_match(text) {
                return true;
            }
        }
        if let Some(name) = node.get_name(source) {
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
    fn evaluate(
        &self,
        node: &UastNode,
        _matcher: &PatternMatcher,
        _source: &str,
        _context: &MatchContext,
    ) -> bool {
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
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        !self.inner.evaluate(node, matcher, source, context)
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
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        self.constraints
            .iter()
            .all(|c| c.evaluate(node, matcher, source, context))
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
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        self.constraints
            .iter()
            .any(|c| c.evaluate(node, matcher, source, context))
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
    fn evaluate(
        &self,
        _node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        // Check if any ancestor matches the pattern
        let ancestors_to_check: Vec<_> = match self.stop_by {
            StopBehavior::Neighbor => {
                // Only check immediate parent
                context.ancestors.last().into_iter().collect()
            }
            StopBehavior::End => {
                // Check all ancestors
                context.ancestors.iter().collect()
            }
        };

        for ancestor in ancestors_to_check {
            let mut env = HashMap::new();
            if matcher.try_match_node(ancestor, &self.ancestor_pattern, &mut env, source) {
                return true;
            }
        }
        false
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
    fn evaluate(
        &self,
        _node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        // Check that NO ancestor matches the pattern
        let ancestors_to_check: Vec<_> = match self.stop_by {
            StopBehavior::Neighbor => {
                // Only check immediate parent
                context.ancestors.last().into_iter().collect()
            }
            StopBehavior::End => {
                // Check all ancestors
                context.ancestors.iter().collect()
            }
        };

        for ancestor in ancestors_to_check {
            let mut env = HashMap::new();
            if matcher.try_match_node(ancestor, &self.ancestor_pattern, &mut env, source) {
                return false; // Found a matching ancestor, constraint fails
            }
        }
        true // No matching ancestor found, constraint passes
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

/// Requires the node to have a descendant matching a pattern.
///
/// Supports nested constraints for deep matching like:
/// ```yaml
/// has:
///   kind: IfStatement
///   has:
///     kind: IfStatement
/// ```
/// This finds a node that has a descendant IfStatement which ITSELF
/// has a descendant IfStatement.
#[derive(Debug, Clone)]
pub struct HasConstraint {
    /// The pattern a descendant must match.
    pub descendant_pattern: PatternNode,
    /// How far to search down the tree.
    pub stop_by: StopBehavior,
    /// Optional regex that matching descendants' source text must match.
    pub regex: Option<Regex>,
    /// Optional nested constraint that matching descendants must also satisfy.
    pub nested_constraint: Option<Box<HasConstraint>>,
    /// Optional pattern that matching descendants must NOT have as descendants.
    pub not_has_pattern: Option<PatternNode>,
    /// Stop behavior for the not_has check.
    pub not_has_stop_by: Option<StopBehavior>,
}

impl HasConstraint {
    /// Create a new has constraint.
    pub fn new(descendant_pattern: PatternNode, stop_by: StopBehavior) -> Self {
        HasConstraint {
            descendant_pattern,
            stop_by,
            regex: None,
            nested_constraint: None,
            not_has_pattern: None,
            not_has_stop_by: None,
        }
    }

    /// Create a new has constraint with a regex filter.
    pub fn with_regex(
        descendant_pattern: PatternNode,
        stop_by: StopBehavior,
        regex: Option<Regex>,
    ) -> Self {
        HasConstraint {
            descendant_pattern,
            stop_by,
            regex,
            nested_constraint: None,
            not_has_pattern: None,
            not_has_stop_by: None,
        }
    }

    /// Create a new has constraint with a nested constraint.
    pub fn with_nested(
        descendant_pattern: PatternNode,
        stop_by: StopBehavior,
        nested: HasConstraint,
    ) -> Self {
        HasConstraint {
            descendant_pattern,
            stop_by,
            regex: None,
            nested_constraint: Some(Box::new(nested)),
            not_has_pattern: None,
            not_has_stop_by: None,
        }
    }

    /// Create a new has constraint with a not-has constraint.
    pub fn with_not_has(
        descendant_pattern: PatternNode,
        stop_by: StopBehavior,
        not_has_pattern: PatternNode,
        not_has_stop_by: StopBehavior,
    ) -> Self {
        HasConstraint {
            descendant_pattern,
            stop_by,
            regex: None,
            nested_constraint: None,
            not_has_pattern: Some(not_has_pattern),
            not_has_stop_by: Some(not_has_stop_by),
        }
    }
}

impl Constraint for HasConstraint {
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        _context: &MatchContext,
    ) -> bool {
        let stop_at_neighbor = self.stop_by == StopBehavior::Neighbor;
        self.has_matching_descendant_with_nested(node, matcher, stop_at_neighbor, source)
    }

    fn clone_box(&self) -> Box<dyn Constraint> {
        Box::new(self.clone())
    }
}

impl HasConstraint {
    /// Check if the node has a descendant matching the pattern.
    /// If there's a nested constraint, the matching descendant must also satisfy it.
    /// If there's a not_has constraint, the matching descendant must NOT have certain descendants.
    fn has_matching_descendant_with_nested(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        stop_at_neighbor: bool,
        source: &str,
    ) -> bool {
        for child in &node.children {
            let mut env = HashMap::new();
            if matcher.try_match_node(child, &self.descendant_pattern, &mut env, source) {
                // Pattern matches - now check regex filter if present
                if let Some(ref regex) = self.regex {
                    let node_text = child.get_text(source).unwrap_or("");
                    if !regex.is_match(node_text) {
                        // Regex doesn't match, try next child
                        if !stop_at_neighbor
                            && self.has_matching_descendant_with_nested(child, matcher, false, source)
                        {
                            return true;
                        }
                        continue;
                    }
                }

                // Pattern (and regex if present) matches - now check nested/not_has constraints
                if let Some(ref nested) = self.nested_constraint {
                    // The matching descendant must also satisfy the nested constraint
                    let empty_context = MatchContext::new(vec![], 0);
                    if nested.evaluate(child, matcher, source, &empty_context) {
                        return true;
                    }
                    // Nested constraint not satisfied, keep searching
                } else if let Some(ref not_has_pattern) = self.not_has_pattern {
                    // The matching descendant must NOT have descendants matching not_has_pattern
                    let stop_by = self.not_has_stop_by.unwrap_or(StopBehavior::End);
                    if !self.has_descendant_matching(child, matcher, not_has_pattern, stop_by == StopBehavior::Neighbor, source) {
                        // No forbidden descendant found - this is what we want!
                        return true;
                    }
                    // Forbidden descendant found, keep searching for another match
                } else {
                    // No nested constraint, pattern match is sufficient
                    return true;
                }
            }

            // Recurse into children (unless stop_at_neighbor)
            if !stop_at_neighbor
                && self.has_matching_descendant_with_nested(child, matcher, false, source)
            {
                return true;
            }
        }
        false
    }

    /// Check if a node has any descendant matching a pattern.
    fn has_descendant_matching(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        pattern: &PatternNode,
        stop_at_neighbor: bool,
        source: &str,
    ) -> bool {
        for child in &node.children {
            let mut env = HashMap::new();
            if matcher.try_match_node(child, pattern, &mut env, source) {
                return true;
            }
            // Recurse into children (unless stop_at_neighbor)
            if !stop_at_neighbor && self.has_descendant_matching(child, matcher, pattern, false, source) {
                return true;
            }
        }
        false
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
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        _context: &MatchContext,
    ) -> bool {
        let stop_at_neighbor = self.stop_by == StopBehavior::Neighbor;
        !matcher.has_matching_descendant(node, &self.descendant_pattern, stop_at_neighbor, source)
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
    fn evaluate(
        &self,
        _node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        // Get following siblings
        let following = context.following_siblings();

        if self.immediate {
            // Check only the immediate next sibling
            if let Some(next) = following.first() {
                let mut env = HashMap::new();
                return matcher.try_match_node(next, &self.sibling_pattern, &mut env, source);
            }
            false // No following sibling
        } else {
            // Check any following sibling
            for sibling in following {
                let mut env = HashMap::new();
                if matcher.try_match_node(sibling, &self.sibling_pattern, &mut env, source) {
                    return true;
                }
            }
            false
        }
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
    fn evaluate(
        &self,
        _node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        context: &MatchContext,
    ) -> bool {
        // Get preceding siblings
        let preceding = context.preceding_siblings();

        if self.immediate {
            // Check only the immediate previous sibling
            if let Some(prev) = preceding.last() {
                let mut env = HashMap::new();
                return matcher.try_match_node(prev, &self.sibling_pattern, &mut env, source);
            }
            false // No preceding sibling
        } else {
            // Check any preceding sibling
            for sibling in preceding {
                let mut env = HashMap::new();
                if matcher.try_match_node(sibling, &self.sibling_pattern, &mut env, source) {
                    return true;
                }
            }
            false
        }
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
    fn evaluate(
        &self,
        node: &UastNode,
        matcher: &PatternMatcher,
        source: &str,
        _context: &MatchContext,
    ) -> bool {
        let mut env = HashMap::new();
        matcher.try_match_node(node, &self.pattern, &mut env, source)
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
    fn evaluate(
        &self,
        _node: &UastNode,
        _matcher: &PatternMatcher,
        _source: &str,
        _context: &MatchContext,
    ) -> bool {
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

    // Empty source for tests that don't use text/name extraction
    const EMPTY_SOURCE: &str = "";

    fn make_node(kind: UastKind, text: Option<&str>) -> UastNode {
        let mut node = UastNode::new(kind, "rust", SourceSpan::empty());
        if let Some(t) = text {
            node = node.with_text(t);
        }
        node
    }

    fn empty_context() -> MatchContext<'static> {
        MatchContext::new(vec![], 0)
    }

    #[test]
    fn test_regex_constraint() {
        let constraint = RegexConstraint::new(r"^my_").unwrap();
        let matcher = PatternMatcher::new();
        let ctx = empty_context();

        let node = make_node(UastKind::Identifier, Some("my_function"));
        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));

        let node = make_node(UastKind::Identifier, Some("other_function"));
        assert!(!constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_regex_constraint_with_lazy_extraction() {
        // Test regex with lazy text extraction
        let source = "my_test_function";
        let constraint = RegexConstraint::new(r"^my_").unwrap();
        let matcher = PatternMatcher::new();
        let ctx = empty_context();

        let node = UastNode::new(UastKind::Identifier, "rust", SourceSpan::empty())
            .with_text_range(0, 16);
        assert!(constraint.evaluate(&node, &matcher, source, &ctx));

        let other_source = "other_function";
        let other_node = UastNode::new(UastKind::Identifier, "rust", SourceSpan::empty())
            .with_text_range(0, 14);
        assert!(!constraint.evaluate(&other_node, &matcher, other_source, &ctx));
    }

    #[test]
    fn test_kind_constraint() {
        let constraint = KindConstraint::new("FunctionDeclaration");
        let matcher = PatternMatcher::new();
        let ctx = empty_context();

        let node = make_node(UastKind::FunctionDeclaration, None);
        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));

        let node = make_node(UastKind::IfStatement, None);
        assert!(!constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_not_constraint() {
        let inner = Box::new(KindConstraint::new("FunctionDeclaration"));
        let constraint = NotConstraint::new(inner);
        let matcher = PatternMatcher::new();
        let ctx = empty_context();

        let node = make_node(UastKind::IfStatement, None);
        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));

        let node = make_node(UastKind::FunctionDeclaration, None);
        assert!(!constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_all_constraint() {
        let constraints: Vec<Box<dyn Constraint>> = vec![
            Box::new(KindConstraint::new("FunctionDeclaration")),
            Box::new(RegexConstraint::new(r"^test").unwrap()),
        ];
        let constraint = AllConstraint::new(constraints);
        let matcher = PatternMatcher::new();
        let ctx = empty_context();

        let node = make_node(UastKind::FunctionDeclaration, Some("test_value"));
        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_any_constraint() {
        let constraints: Vec<Box<dyn Constraint>> = vec![
            Box::new(KindConstraint::new("FunctionDeclaration")),
            Box::new(KindConstraint::new("MethodDeclaration")),
        ];
        let constraint = AnyConstraint::new(constraints);
        let matcher = PatternMatcher::new();
        let ctx = empty_context();

        let node = make_node(UastKind::FunctionDeclaration, None);
        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));

        let node = make_node(UastKind::MethodDeclaration, None);
        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));

        let node = make_node(UastKind::IfStatement, None);
        assert!(!constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
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
        let ctx = empty_context();

        assert!(constraint.evaluate(&root, &matcher, EMPTY_SOURCE, &ctx));
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
        let ctx = empty_context();

        assert!(constraint.evaluate(&root, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_length_constraint_exact() {
        let constraint = LengthConstraint::exact(1);
        let matcher = PatternMatcher::new();
        let node = make_node(UastKind::Identifier, None);
        let ctx = empty_context();

        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_length_constraint_range() {
        let constraint = LengthConstraint::between(0, 5);
        let matcher = PatternMatcher::new();
        let node = make_node(UastKind::Identifier, None);
        let ctx = empty_context();

        assert!(constraint.evaluate(&node, &matcher, EMPTY_SOURCE, &ctx));
    }

    // ========== New tests for InsideConstraint ==========

    #[test]
    fn test_inside_constraint_matches_ancestor() {
        // Build tree: FunctionDeclaration > Block > IfStatement
        let if_node = make_node(UastKind::IfStatement, None);
        let block = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(if_node.clone());
        let func = UastNode::new(UastKind::FunctionDeclaration, "rust", SourceSpan::empty())
            .with_child(block.clone());

        // Create context as if we're at the IfStatement
        let ctx = MatchContext::new(vec![&func, &block], 0);

        let constraint = InsideConstraint::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            StopBehavior::End,
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&if_node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_inside_constraint_neighbor_only() {
        // Build tree: FunctionDeclaration > Block > IfStatement
        let if_node = make_node(UastKind::IfStatement, None);
        let block = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(if_node.clone());
        let func = UastNode::new(UastKind::FunctionDeclaration, "rust", SourceSpan::empty())
            .with_child(block.clone());

        // Create context as if we're at the IfStatement
        let ctx = MatchContext::new(vec![&func, &block], 0);

        // With Neighbor, should only check immediate parent (Block)
        let constraint = InsideConstraint::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            StopBehavior::Neighbor,
        );
        let matcher = PatternMatcher::new();

        // Should fail because immediate parent is Block, not FunctionDeclaration
        assert!(!constraint.evaluate(&if_node, &matcher, EMPTY_SOURCE, &ctx));

        // Should pass for Block
        let block_constraint = InsideConstraint::new(
            PatternNode::Kind(UastKind::Block),
            StopBehavior::Neighbor,
        );
        assert!(block_constraint.evaluate(&if_node, &matcher, EMPTY_SOURCE, &ctx));
    }

    #[test]
    fn test_not_inside_constraint() {
        // Build tree: FunctionDeclaration > Block > IfStatement
        let if_node = make_node(UastKind::IfStatement, None);
        let block = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(if_node.clone());
        let func = UastNode::new(UastKind::FunctionDeclaration, "rust", SourceSpan::empty())
            .with_child(block.clone());

        let ctx = MatchContext::new(vec![&func, &block], 0);

        // Should fail because FunctionDeclaration IS an ancestor
        let constraint = NotInsideConstraint::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            StopBehavior::End,
        );
        let matcher = PatternMatcher::new();

        assert!(!constraint.evaluate(&if_node, &matcher, EMPTY_SOURCE, &ctx));

        // Should pass because WhileStatement is NOT an ancestor
        let not_while = NotInsideConstraint::new(
            PatternNode::Kind(UastKind::WhileStatement),
            StopBehavior::End,
        );
        assert!(not_while.evaluate(&if_node, &matcher, EMPTY_SOURCE, &ctx));
    }

    // ========== New tests for PrecedesConstraint ==========

    #[test]
    fn test_precedes_constraint_immediate() {
        // Build tree with siblings: [A, B, C]
        let a = make_node(UastKind::Identifier, Some("a"));
        let b = make_node(UastKind::Identifier, Some("b"));
        let c = make_node(UastKind::NumberLiteral, Some("42"));
        let parent = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(a.clone())
            .with_child(b.clone())
            .with_child(c.clone());

        // Context for node A (sibling_index = 0)
        let ctx_a = MatchContext::new(vec![&parent], 0);

        // A immediately precedes B (Identifier)
        let constraint = PrecedesConstraint::new(
            PatternNode::Kind(UastKind::Identifier),
            true, // immediate
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&a, &matcher, EMPTY_SOURCE, &ctx_a));

        // A does NOT immediately precede NumberLiteral (C is at index 2, not 1)
        let num_constraint = PrecedesConstraint::new(
            PatternNode::Kind(UastKind::NumberLiteral),
            true, // immediate
        );
        assert!(!num_constraint.evaluate(&a, &matcher, EMPTY_SOURCE, &ctx_a));
    }

    #[test]
    fn test_precedes_constraint_any() {
        // Build tree with siblings: [A, B, C]
        let a = make_node(UastKind::Identifier, Some("a"));
        let b = make_node(UastKind::Identifier, Some("b"));
        let c = make_node(UastKind::NumberLiteral, Some("42"));
        let parent = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(a.clone())
            .with_child(b.clone())
            .with_child(c.clone());

        // Context for node A (sibling_index = 0)
        let ctx_a = MatchContext::new(vec![&parent], 0);

        // A precedes some NumberLiteral (C)
        let constraint = PrecedesConstraint::new(
            PatternNode::Kind(UastKind::NumberLiteral),
            false, // any following sibling
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&a, &matcher, EMPTY_SOURCE, &ctx_a));
    }

    // ========== New tests for FollowsConstraint ==========

    #[test]
    fn test_follows_constraint_immediate() {
        // Build tree with siblings: [A, B, C]
        let a = make_node(UastKind::Identifier, Some("a"));
        let b = make_node(UastKind::Identifier, Some("b"));
        let c = make_node(UastKind::NumberLiteral, Some("42"));
        let parent = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(a.clone())
            .with_child(b.clone())
            .with_child(c.clone());

        // Context for node B (sibling_index = 1)
        let ctx_b = MatchContext::new(vec![&parent], 1);

        // B immediately follows A (Identifier)
        let constraint = FollowsConstraint::new(
            PatternNode::Kind(UastKind::Identifier),
            true, // immediate
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&b, &matcher, EMPTY_SOURCE, &ctx_b));

        // Context for node C (sibling_index = 2)
        let ctx_c = MatchContext::new(vec![&parent], 2);

        // C immediately follows B (Identifier), not A
        assert!(constraint.evaluate(&c, &matcher, EMPTY_SOURCE, &ctx_c));
    }

    #[test]
    fn test_follows_constraint_any() {
        // Build tree with siblings: [A, B, C]
        let a = make_node(UastKind::NumberLiteral, Some("1"));
        let b = make_node(UastKind::Identifier, Some("b"));
        let c = make_node(UastKind::Identifier, Some("c"));
        let parent = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(a.clone())
            .with_child(b.clone())
            .with_child(c.clone());

        // Context for node C (sibling_index = 2)
        let ctx_c = MatchContext::new(vec![&parent], 2);

        // C follows some NumberLiteral (A)
        let constraint = FollowsConstraint::new(
            PatternNode::Kind(UastKind::NumberLiteral),
            false, // any preceding sibling
        );
        let matcher = PatternMatcher::new();

        assert!(constraint.evaluate(&c, &matcher, EMPTY_SOURCE, &ctx_c));
    }

    #[test]
    fn test_match_context_helpers() {
        let a = make_node(UastKind::Identifier, Some("a"));
        let b = make_node(UastKind::Identifier, Some("b"));
        let c = make_node(UastKind::Identifier, Some("c"));
        let parent = UastNode::new(UastKind::Block, "rust", SourceSpan::empty())
            .with_child(a)
            .with_child(b)
            .with_child(c);

        // Context for middle sibling (index 1)
        let ctx = MatchContext::new(vec![&parent], 1);

        assert!(ctx.parent().is_some());
        assert_eq!(ctx.preceding_siblings().len(), 1);
        assert_eq!(ctx.following_siblings().len(), 1);
    }
}
