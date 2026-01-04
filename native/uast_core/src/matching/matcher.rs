//! Pattern matching engine for UAST trees.
//!
//! This module implements the core pattern matching algorithm that traverses
//! UAST trees and finds nodes matching patterns.

use super::constraints::Constraint;
use super::pattern::{LiteralPattern, MetavarQuantifier, Pattern, PatternNode};
use crate::uast::mappings::get_native_types_for_uast;
use crate::uast::schema::{SourceSpan, UastKind, UastNode};
use std::collections::HashMap;

/// Result of a successful pattern match.
#[derive(Debug, Clone)]
pub struct MatchResult {
    /// The matched node.
    pub node: UastNode,
    /// Captured metavariables: name -> captured node(s).
    pub captures: HashMap<String, CapturedValue>,
    /// Source span of the match.
    pub span: SourceSpan,
}

/// A captured value from a metavariable.
#[derive(Debug, Clone)]
pub enum CapturedValue {
    /// A single captured node.
    Single(UastNode),
    /// Multiple captured nodes (from $$ or $$$ metavars).
    Multiple(Vec<UastNode>),
}

impl CapturedValue {
    /// Get as a single node (first node if multiple).
    pub fn as_single(&self) -> Option<&UastNode> {
        match self {
            CapturedValue::Single(node) => Some(node),
            CapturedValue::Multiple(nodes) => nodes.first(),
        }
    }

    /// Get all nodes.
    pub fn as_vec(&self) -> Vec<&UastNode> {
        match self {
            CapturedValue::Single(node) => vec![node],
            CapturedValue::Multiple(nodes) => nodes.iter().collect(),
        }
    }

    /// Get the count of captured nodes.
    pub fn len(&self) -> usize {
        match self {
            CapturedValue::Single(_) => 1,
            CapturedValue::Multiple(nodes) => nodes.len(),
        }
    }

    /// Check if empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

/// The pattern matcher.
#[derive(Debug, Default)]
pub struct PatternMatcher {
    /// Optional constraints for metavariables.
    constraints: HashMap<String, Vec<Box<dyn Constraint>>>,
}

impl PatternMatcher {
    /// Create a new pattern matcher.
    pub fn new() -> Self {
        PatternMatcher {
            constraints: HashMap::new(),
        }
    }

    /// Add a constraint for a metavariable.
    pub fn add_constraint(&mut self, var_name: String, constraint: Box<dyn Constraint>) {
        self.constraints
            .entry(var_name)
            .or_insert_with(Vec::new)
            .push(constraint);
    }

    /// Try to match a pattern against a node.
    ///
    /// Returns `Some(MatchResult)` if the pattern matches, `None` otherwise.
    pub fn try_match(&self, node: &UastNode, pattern: &Pattern) -> Option<MatchResult> {
        let mut env = HashMap::new();

        if self.try_match_node(node, &pattern.root, &mut env) {
            Some(MatchResult {
                node: node.clone(),
                captures: env,
                span: node.span,
            })
        } else {
            None
        }
    }

    /// Find all matches in a tree (depth-first traversal).
    pub fn find_all(&self, root: &UastNode, pattern: &Pattern) -> Vec<MatchResult> {
        let mut results = Vec::new();
        self.visit(root, pattern, &mut results);
        results
    }

    /// Visit all nodes collecting matches.
    fn visit(&self, node: &UastNode, pattern: &Pattern, results: &mut Vec<MatchResult>) {
        // Try to match at this node
        if let Some(result) = self.try_match(node, pattern) {
            results.push(result);
        }

        // Recurse to children
        for child in &node.children {
            self.visit(child, pattern, results);
        }
    }

    /// Core matching logic for a node against a pattern.
    pub fn try_match_node(
        &self,
        node: &UastNode,
        pattern: &PatternNode,
        env: &mut HashMap<String, CapturedValue>,
    ) -> bool {
        match pattern {
            PatternNode::Kind(expected_kind) => {
                node.kind == *expected_kind
            }

            PatternNode::Metavar {
                name,
                quantifier,
                is_anonymous,
            } => {
                // Check constraints for this metavar
                if !self.check_constraints(name, node) {
                    return false;
                }

                // Anonymous metavars match but don't capture
                if *is_anonymous {
                    return true;
                }

                // Check for existing capture with same name
                if let Some(existing) = env.get(name) {
                    // Must match the same structure
                    return self.nodes_equal(node, existing.as_single().unwrap());
                }

                // Capture the node
                match quantifier {
                    MetavarQuantifier::Single => {
                        env.insert(name.clone(), CapturedValue::Single(node.clone()));
                    }
                    MetavarQuantifier::OneOrMore | MetavarQuantifier::ZeroOrMore => {
                        env.insert(name.clone(), CapturedValue::Multiple(vec![node.clone()]));
                    }
                }

                true
            }

            PatternNode::Wildcard => true,

            PatternNode::Native(native_pattern) => {
                // For native patterns, check if the node's native_type matches
                // or if the UAST kind maps to this native type
                if let Some(native_type) = &node.native_type {
                    if native_type == native_pattern {
                        return true;
                    }
                }

                // Try reverse lookup: does this native pattern map from our kind?
                let native_types = get_native_types_for_uast(node.kind.as_str(), &node.language);
                native_types.contains(&native_pattern.as_str())
            }

            PatternNode::Sequence(patterns) => {
                self.try_match_children(&node.children, patterns, 0, 0, env)
            }

            PatternNode::AnyOf(alternatives) => {
                for alt in alternatives {
                    let mut temp_env = env.clone();
                    if self.try_match_node(node, alt, &mut temp_env) {
                        // Merge captures
                        env.extend(temp_env);
                        return true;
                    }
                }
                false
            }

            PatternNode::AllOf(patterns) => {
                for p in patterns {
                    if !self.try_match_node(node, p, env) {
                        return false;
                    }
                }
                true
            }

            PatternNode::Not(inner) => {
                let mut temp_env = HashMap::new();
                !self.try_match_node(node, inner, &mut temp_env)
            }

            PatternNode::Structural {
                kind,
                properties,
                children,
            } => {
                // Check kind if specified
                if let Some(expected_kind) = kind {
                    if node.kind != *expected_kind {
                        return false;
                    }
                }

                // Check properties
                for (prop_name, prop_pattern) in properties {
                    if !self.check_property(node, prop_name, prop_pattern, env) {
                        return false;
                    }
                }

                // Check children
                if !children.is_empty() {
                    if !self.try_match_children(&node.children, children, 0, 0, env) {
                        return false;
                    }
                }

                true
            }

            PatternNode::Literal(lit) => self.match_literal(node, lit),
        }
    }

    /// Match children against pattern children with ellipsis support.
    fn try_match_children(
        &self,
        nodes: &[UastNode],
        patterns: &[PatternNode],
        node_idx: usize,
        pattern_idx: usize,
        env: &mut HashMap<String, CapturedValue>,
    ) -> bool {
        // Base case: all patterns consumed
        if pattern_idx >= patterns.len() {
            // Allow trailing nodes if last pattern was a multi-match
            if pattern_idx > 0 {
                if let Some(last) = patterns.last() {
                    if last.is_multiple_metavar() {
                        return true;
                    }
                }
            }
            return node_idx >= nodes.len();
        }

        let pattern = &patterns[pattern_idx];

        // Handle multi-match metavariables
        if let PatternNode::Metavar {
            name,
            quantifier,
            is_anonymous,
        } = pattern
        {
            if quantifier.is_multiple() {
                // Try different amounts of greedy capture
                let min_required = quantifier.min_count();
                let remaining_patterns = patterns.len() - pattern_idx - 1;
                let max_capture = if nodes.len() > node_idx + remaining_patterns {
                    nodes.len() - node_idx - remaining_patterns
                } else {
                    0
                };

                if max_capture < min_required {
                    return false;
                }

                // Try from max down to min (greedy)
                for capture_count in (min_required..=max_capture).rev() {
                    let mut temp_env = env.clone();
                    let captured: Vec<UastNode> =
                        nodes[node_idx..node_idx + capture_count].to_vec();

                    if !*is_anonymous {
                        temp_env.insert(name.clone(), CapturedValue::Multiple(captured));
                    }

                    if self.try_match_children(
                        nodes,
                        patterns,
                        node_idx + capture_count,
                        pattern_idx + 1,
                        &mut temp_env,
                    ) {
                        env.extend(temp_env);
                        return true;
                    }
                }

                return false;
            }
        }

        // Regular single-node pattern
        if node_idx >= nodes.len() {
            return false;
        }

        if !self.try_match_node(&nodes[node_idx], pattern, env) {
            return false;
        }

        self.try_match_children(nodes, patterns, node_idx + 1, pattern_idx + 1, env)
    }

    /// Check constraints for a metavariable.
    fn check_constraints(&self, var_name: &str, node: &UastNode) -> bool {
        if let Some(constraints) = self.constraints.get(var_name) {
            for constraint in constraints {
                if !constraint.evaluate(node, self) {
                    return false;
                }
            }
        }
        true
    }

    /// Check a property pattern.
    fn check_property(
        &self,
        node: &UastNode,
        prop_name: &str,
        pattern: &PatternNode,
        env: &mut HashMap<String, CapturedValue>,
    ) -> bool {
        // Check standard properties
        match prop_name {
            "name" | "Name" => {
                if let Some(name) = &node.name {
                    match pattern {
                        PatternNode::Literal(LiteralPattern::String(expected)) => {
                            return name == expected;
                        }
                        PatternNode::Metavar { name: var_name, is_anonymous, .. } => {
                            if !*is_anonymous {
                                // Capture as string - create a synthetic node
                                let synthetic = UastNode::new(
                                    UastKind::Identifier,
                                    &node.language,
                                    node.span,
                                )
                                .with_text(name.clone());
                                env.insert(var_name.clone(), CapturedValue::Single(synthetic));
                            }
                            return true;
                        }
                        PatternNode::Wildcard => return true,
                        _ => {}
                    }
                }
                // Property doesn't exist - only OK if pattern allows optional
                matches!(
                    pattern,
                    PatternNode::Metavar {
                        quantifier: MetavarQuantifier::ZeroOrMore,
                        ..
                    }
                )
            }
            "text" | "Text" => {
                if let Some(text) = &node.text {
                    match pattern {
                        PatternNode::Literal(LiteralPattern::String(expected)) => {
                            return text == expected;
                        }
                        PatternNode::Metavar { .. } | PatternNode::Wildcard => return true,
                        _ => {}
                    }
                }
                matches!(
                    pattern,
                    PatternNode::Metavar {
                        quantifier: MetavarQuantifier::ZeroOrMore,
                        ..
                    }
                )
            }
            _ => {
                // Check custom properties
                if node.properties.contains_key(prop_name) {
                    // For now, just check if property exists
                    return true;
                }
                matches!(
                    pattern,
                    PatternNode::Metavar {
                        quantifier: MetavarQuantifier::ZeroOrMore,
                        ..
                    }
                )
            }
        }
    }

    /// Match a literal pattern.
    fn match_literal(&self, node: &UastNode, lit: &LiteralPattern) -> bool {
        match lit {
            LiteralPattern::String(expected) => {
                node.text.as_ref().map_or(false, |t| t == expected)
                    || node.name.as_ref().map_or(false, |n| n == expected)
            }
            LiteralPattern::Integer(expected) => {
                if let Some(text) = &node.text {
                    text.parse::<i64>().map_or(false, |n| n == *expected)
                } else {
                    false
                }
            }
            LiteralPattern::Float(expected) => {
                if let Some(text) = &node.text {
                    text.parse::<f64>().map_or(false, |f| (f - expected).abs() < f64::EPSILON)
                } else {
                    false
                }
            }
            LiteralPattern::Boolean(expected) => {
                if let Some(text) = &node.text {
                    text.eq_ignore_ascii_case(if *expected { "true" } else { "false" })
                } else {
                    false
                }
            }
            LiteralPattern::Null => {
                node.text
                    .as_ref()
                    .map_or(false, |t| t.eq_ignore_ascii_case("null") || t.eq_ignore_ascii_case("nil"))
            }
        }
    }

    /// Check if two nodes are structurally equal.
    pub fn nodes_equal(&self, a: &UastNode, b: &UastNode) -> bool {
        if a.kind != b.kind {
            return false;
        }

        // Compare text if available
        if a.text.is_some() && b.text.is_some() && a.text != b.text {
            return false;
        }

        // Compare name if available
        if a.name.is_some() && b.name.is_some() && a.name != b.name {
            return false;
        }

        // Compare children
        if a.children.len() != b.children.len() {
            return false;
        }

        for (ca, cb) in a.children.iter().zip(b.children.iter()) {
            if !self.nodes_equal(ca, cb) {
                return false;
            }
        }

        true
    }

    /// Check if a node has a descendant matching a pattern.
    pub fn has_matching_descendant(
        &self,
        node: &UastNode,
        pattern: &PatternNode,
        stop_at_neighbor: bool,
    ) -> bool {
        for child in &node.children {
            let mut env = HashMap::new();
            if self.try_match_node(child, pattern, &mut env) {
                return true;
            }

            if !stop_at_neighbor && self.has_matching_descendant(child, pattern, false) {
                return true;
            }
        }
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_node(kind: UastKind, name: Option<&str>) -> UastNode {
        let mut node = UastNode::new(kind, "rust", SourceSpan::empty());
        if let Some(n) = name {
            node = node.with_name(n);
        }
        node
    }

    #[test]
    fn test_match_kind() {
        let node = make_node(UastKind::FunctionDeclaration, Some("test"));
        let pattern = PatternNode::Kind(UastKind::FunctionDeclaration);
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
    }

    #[test]
    fn test_match_kind_mismatch() {
        let node = make_node(UastKind::IfStatement, None);
        let pattern = PatternNode::Kind(UastKind::FunctionDeclaration);
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(!matcher.try_match_node(&node, &pattern, &mut env));
    }

    #[test]
    fn test_match_wildcard() {
        let node = make_node(UastKind::FunctionDeclaration, Some("any"));
        let pattern = PatternNode::Wildcard;
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
    }

    #[test]
    fn test_match_metavar_capture() {
        let node = make_node(UastKind::FunctionDeclaration, Some("my_func"));
        let pattern = PatternNode::metavar("FUNC", MetavarQuantifier::Single);
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
        assert!(env.contains_key("FUNC"));
    }

    #[test]
    fn test_match_metavar_anonymous() {
        let node = make_node(UastKind::FunctionDeclaration, Some("test"));
        let pattern = PatternNode::anonymous(MetavarQuantifier::Single);
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
        assert!(!env.contains_key("_")); // Anonymous should not be captured
    }

    #[test]
    fn test_match_any_of() {
        let node = make_node(UastKind::IfStatement, None);
        let pattern = PatternNode::any_of(vec![
            PatternNode::Kind(UastKind::FunctionDeclaration),
            PatternNode::Kind(UastKind::IfStatement),
        ]);
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
    }

    #[test]
    fn test_match_all_of() {
        let node = make_node(UastKind::FunctionDeclaration, Some("test"));
        let pattern = PatternNode::all_of(vec![
            PatternNode::Kind(UastKind::FunctionDeclaration),
            PatternNode::metavar("NAME", MetavarQuantifier::Single),
        ]);
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
    }

    #[test]
    fn test_match_not() {
        let node = make_node(UastKind::IfStatement, None);
        let pattern = PatternNode::not(PatternNode::Kind(UastKind::FunctionDeclaration));
        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_node(&node, &pattern, &mut env));
    }

    #[test]
    fn test_find_all() {
        let child1 = make_node(UastKind::FunctionDeclaration, Some("func1"));
        let child2 = make_node(UastKind::IfStatement, None);
        let child3 = make_node(UastKind::FunctionDeclaration, Some("func2"));

        let root = UastNode::new(UastKind::SourceFile, "rust", SourceSpan::empty())
            .with_child(child1)
            .with_child(child2)
            .with_child(child3);

        let pattern = Pattern::new(
            PatternNode::Kind(UastKind::FunctionDeclaration),
            "FunctionDeclaration".to_string(),
            "rust".to_string(),
        );

        let matcher = PatternMatcher::new();
        let matches = matcher.find_all(&root, &pattern);

        assert_eq!(matches.len(), 2);
    }

    #[test]
    fn test_captured_value() {
        let node = make_node(UastKind::Identifier, Some("x"));
        let single = CapturedValue::Single(node.clone());
        let multiple = CapturedValue::Multiple(vec![node.clone(), node.clone()]);

        assert_eq!(single.len(), 1);
        assert_eq!(multiple.len(), 2);
        assert!(single.as_single().is_some());
        assert_eq!(multiple.as_vec().len(), 2);
    }

    #[test]
    fn test_match_children_with_multi_metavar() {
        let child1 = make_node(UastKind::Identifier, Some("a"));
        let child2 = make_node(UastKind::Identifier, Some("b"));
        let child3 = make_node(UastKind::Identifier, Some("c"));

        let root = UastNode::new(UastKind::CallExpression, "rust", SourceSpan::empty())
            .with_child(child1)
            .with_child(child2)
            .with_child(child3);

        let patterns = vec![
            PatternNode::metavar("ARGS", MetavarQuantifier::ZeroOrMore),
        ];

        let matcher = PatternMatcher::new();
        let mut env = HashMap::new();

        assert!(matcher.try_match_children(&root.children, &patterns, 0, 0, &mut env));
        assert!(env.contains_key("ARGS"));

        if let Some(CapturedValue::Multiple(nodes)) = env.get("ARGS") {
            assert_eq!(nodes.len(), 3);
        } else {
            panic!("Expected Multiple capture");
        }
    }
}
