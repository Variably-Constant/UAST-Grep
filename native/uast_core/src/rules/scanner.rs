//! Rule scanner for UAST-Grep.
//!
//! This module implements the core scanning logic that matches rules against
//! UAST trees and collects scan results.

use super::fix::Fix;
use super::rule::{ConstraintYaml, RulePatternYaml, RuleYaml, Severity};
use crate::matching::{
    parse_simple_pattern, CapturedValue, Constraint, HasConstraint, InsideConstraint,
    KindConstraint, NotConstraint, NotHasConstraint, Pattern, PatternMatcher,
    PatternNode, RegexConstraint, StopBehavior,
};
use crate::uast::schema::{SourceSpan, UastNode};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use thiserror::Error;

/// Error type for scanning failures.
#[derive(Error, Debug)]
pub enum ScanError {
    /// Pattern parsing error.
    #[error("Pattern error in rule '{rule_id}': {message}")]
    Pattern { rule_id: String, message: String },

    /// Regex compilation error.
    #[error("Regex error in rule '{rule_id}': {message}")]
    Regex { rule_id: String, message: String },

    /// Generic scan error.
    #[error("Scan error: {0}")]
    Generic(String),
}

/// Result of scanning a single file or source.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScanResult {
    /// The rule ID that matched.
    pub rule_id: String,

    /// Severity of the match.
    pub severity: Severity,

    /// The interpolated message.
    pub message: String,

    /// Source location of the match.
    pub location: SourceSpan,

    /// Optional fix for this issue.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fix: Option<Fix>,

    /// File path (if known).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file_path: Option<String>,

    /// Captured metavariables (for debugging/reporting).
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub captures: HashMap<String, String>,
}

impl ScanResult {
    /// Create a new scan result.
    pub fn new(
        rule_id: String,
        severity: Severity,
        message: String,
        location: SourceSpan,
    ) -> Self {
        ScanResult {
            rule_id,
            severity,
            message,
            location,
            fix: None,
            file_path: None,
            captures: HashMap::new(),
        }
    }

    /// Add a fix to this result.
    pub fn with_fix(mut self, fix: Fix) -> Self {
        self.fix = Some(fix);
        self
    }

    /// Set the file path.
    pub fn with_file_path(mut self, path: impl Into<String>) -> Self {
        self.file_path = Some(path.into());
        self
    }

    /// Add a captured variable.
    pub fn with_capture(mut self, name: String, value: String) -> Self {
        self.captures.insert(name, value);
        self
    }
}

/// A compiled rule ready for matching.
#[derive(Debug)]
pub struct CompiledRule {
    /// The original rule definition.
    pub rule: RuleYaml,

    /// The compiled primary pattern.
    pub pattern: Pattern,

    /// Additional constraint patterns.
    pub constraints: HashMap<String, Vec<Box<dyn Constraint>>>,

    /// Inside constraint if present.
    pub inside_constraint: Option<Box<dyn Constraint>>,

    /// Has constraint if present.
    pub has_constraint: Option<Box<dyn Constraint>>,
}

impl CompiledRule {
    /// Compile a rule from YAML.
    pub fn compile(rule: RuleYaml) -> Result<Self, ScanError> {
        // Compile the main pattern
        let pattern = compile_rule_pattern(&rule.rule, &rule.language, &rule.id)?;

        // Compile metavariable constraints
        let constraints = if let Some(ref constraint_map) = rule.constraints {
            compile_constraints(constraint_map, &rule.language, &rule.id)?
        } else {
            HashMap::new()
        };

        // Compile relational constraints from the pattern
        let inside_constraint = compile_inside_constraint(&rule.rule, &rule.language, &rule.id)?;
        let has_constraint = compile_has_constraint(&rule.rule, &rule.language, &rule.id)?;

        Ok(CompiledRule {
            rule,
            pattern,
            constraints,
            inside_constraint,
            has_constraint,
        })
    }

    /// Get the rule ID.
    pub fn id(&self) -> &str {
        &self.rule.id
    }

    /// Get the target language.
    pub fn language(&self) -> &str {
        &self.rule.language
    }
}

/// Compile a rule pattern YAML into a Pattern.
fn compile_rule_pattern(
    yaml: &RulePatternYaml,
    language: &str,
    rule_id: &str,
) -> Result<Pattern, ScanError> {
    // Handle simple pattern
    if let Some(ref pattern_str) = yaml.pattern {
        return parse_simple_pattern(pattern_str, language).map_err(|e| ScanError::Pattern {
            rule_id: rule_id.to_string(),
            message: e.to_string(),
        });
    }

    // Handle any (OR)
    if let Some(ref alternatives) = yaml.any {
        let compiled: Result<Vec<Pattern>, ScanError> = alternatives
            .iter()
            .map(|alt| compile_rule_pattern(alt, language, rule_id))
            .collect();
        let patterns = compiled?;

        if patterns.is_empty() {
            return Err(ScanError::Pattern {
                rule_id: rule_id.to_string(),
                message: "Empty 'any' pattern list".to_string(),
            });
        }

        // Create an AnyOf pattern node
        let roots: Vec<PatternNode> = patterns.into_iter().map(|p| p.root).collect();
        return Ok(Pattern::new(
            PatternNode::AnyOf(roots),
            "(any)".to_string(),
            language.to_string(),
        ));
    }

    // Handle all (AND)
    if let Some(ref all_patterns) = yaml.all {
        let compiled: Result<Vec<Pattern>, ScanError> = all_patterns
            .iter()
            .map(|p| compile_rule_pattern(p, language, rule_id))
            .collect();
        let patterns = compiled?;

        if patterns.is_empty() {
            return Err(ScanError::Pattern {
                rule_id: rule_id.to_string(),
                message: "Empty 'all' pattern list".to_string(),
            });
        }

        // Create an AllOf pattern node
        let roots: Vec<PatternNode> = patterns.into_iter().map(|p| p.root).collect();
        return Ok(Pattern::new(
            PatternNode::AllOf(roots),
            "(all)".to_string(),
            language.to_string(),
        ));
    }

    // Handle not
    if let Some(ref not_pattern) = yaml.not {
        let inner = compile_rule_pattern(not_pattern, language, rule_id)?;
        return Ok(Pattern::new(
            PatternNode::Not(Box::new(inner.root)),
            "(not)".to_string(),
            language.to_string(),
        ));
    }

    // Handle kind
    if let Some(ref kind) = yaml.kind {
        let kind_enum = crate::uast::schema::UastKind::from_str(kind);
        return Ok(Pattern::new(
            PatternNode::Kind(kind_enum),
            kind.clone(),
            language.to_string(),
        ));
    }

    // Handle regex
    if let Some(ref regex_str) = yaml.regex {
        // For regex patterns, we match against source text
        // Create a pattern that matches anything and apply regex constraint
        return Ok(Pattern::new(
            PatternNode::Wildcard,
            format!("regex:{}", regex_str),
            language.to_string(),
        ));
    }

    Err(ScanError::Pattern {
        rule_id: rule_id.to_string(),
        message: "Rule pattern must have pattern, any, all, not, kind, or regex".to_string(),
    })
}

/// Compile metavariable constraints.
fn compile_constraints(
    constraints: &HashMap<String, ConstraintYaml>,
    language: &str,
    rule_id: &str,
) -> Result<HashMap<String, Vec<Box<dyn Constraint>>>, ScanError> {
    let mut result: HashMap<String, Vec<Box<dyn Constraint>>> = HashMap::new();

    for (var_name, constraint) in constraints {
        let mut var_constraints: Vec<Box<dyn Constraint>> = Vec::new();

        // Kind constraint
        if let Some(ref kind) = constraint.kind {
            var_constraints.push(Box::new(KindConstraint::new(kind)));
        }

        // Regex constraint
        if let Some(ref regex) = constraint.regex {
            let regex_constraint =
                RegexConstraint::new(regex).map_err(|e| ScanError::Regex {
                    rule_id: rule_id.to_string(),
                    message: e.to_string(),
                })?;
            var_constraints.push(Box::new(regex_constraint));
        }

        // Not-kind constraint
        if let Some(ref not_kind) = constraint.not_kind {
            var_constraints.push(Box::new(NotConstraint::new(Box::new(KindConstraint::new(
                not_kind,
            )))));
        }

        // Not-regex constraint
        if let Some(ref not_regex) = constraint.not_regex {
            let regex_constraint =
                RegexConstraint::new(not_regex).map_err(|e| ScanError::Regex {
                    rule_id: rule_id.to_string(),
                    message: e.to_string(),
                })?;
            var_constraints.push(Box::new(NotConstraint::new(Box::new(regex_constraint))));
        }

        if !var_constraints.is_empty() {
            result.insert(var_name.clone(), var_constraints);
        }
    }

    Ok(result)
}

/// Compile an inside constraint from the pattern YAML.
fn compile_inside_constraint(
    yaml: &RulePatternYaml,
    language: &str,
    rule_id: &str,
) -> Result<Option<Box<dyn Constraint>>, ScanError> {
    if let Some(ref inside) = yaml.inside {
        let pattern_node = if let Some(ref pattern_str) = inside.pattern {
            let pattern =
                parse_simple_pattern(pattern_str, language).map_err(|e| ScanError::Pattern {
                    rule_id: rule_id.to_string(),
                    message: format!("inside pattern: {}", e),
                })?;
            pattern.root
        } else if let Some(ref kind) = inside.kind {
            PatternNode::Kind(crate::uast::schema::UastKind::from_str(kind))
        } else {
            return Err(ScanError::Pattern {
                rule_id: rule_id.to_string(),
                message: "inside constraint must have pattern or kind".to_string(),
            });
        };

        let stop_by = match inside.stop_by.as_str() {
            "neighbor" => StopBehavior::Neighbor,
            _ => StopBehavior::End,
        };

        return Ok(Some(Box::new(InsideConstraint::new(pattern_node, stop_by))));
    }

    Ok(None)
}

/// Compile a has constraint from the pattern YAML.
fn compile_has_constraint(
    yaml: &RulePatternYaml,
    language: &str,
    rule_id: &str,
) -> Result<Option<Box<dyn Constraint>>, ScanError> {
    if let Some(ref has) = yaml.has {
        let pattern_node = if let Some(ref pattern_str) = has.pattern {
            let pattern =
                parse_simple_pattern(pattern_str, language).map_err(|e| ScanError::Pattern {
                    rule_id: rule_id.to_string(),
                    message: format!("has pattern: {}", e),
                })?;
            pattern.root
        } else if let Some(ref kind) = has.kind {
            PatternNode::Kind(crate::uast::schema::UastKind::from_str(kind))
        } else {
            return Err(ScanError::Pattern {
                rule_id: rule_id.to_string(),
                message: "has constraint must have pattern or kind".to_string(),
            });
        };

        let stop_by = match has.stop_by.as_str() {
            "neighbor" => StopBehavior::Neighbor,
            _ => StopBehavior::End,
        };

        return Ok(Some(Box::new(HasConstraint::new(pattern_node, stop_by))));
    }

    // Also check for notHas
    if let Some(ref not_has) = yaml.not_has {
        let pattern_node = if let Some(ref pattern_str) = not_has.pattern {
            let pattern = parse_simple_pattern(pattern_str, language).map_err(|e| {
                ScanError::Pattern {
                    rule_id: rule_id.to_string(),
                    message: format!("notHas pattern: {}", e),
                }
            })?;
            pattern.root
        } else if let Some(ref kind) = not_has.kind {
            PatternNode::Kind(crate::uast::schema::UastKind::from_str(kind))
        } else {
            return Err(ScanError::Pattern {
                rule_id: rule_id.to_string(),
                message: "notHas constraint must have pattern or kind".to_string(),
            });
        };

        let stop_by = match not_has.stop_by.as_str() {
            "neighbor" => StopBehavior::Neighbor,
            _ => StopBehavior::End,
        };

        return Ok(Some(Box::new(NotHasConstraint::new(pattern_node, stop_by))));
    }

    Ok(None)
}

/// The main rule scanner.
#[derive(Debug)]
pub struct Scanner {
    /// Compiled rules indexed by language.
    rules_by_language: HashMap<String, Vec<CompiledRule>>,
    /// Rules that match any language.
    universal_rules: Vec<CompiledRule>,
}

impl Default for Scanner {
    fn default() -> Self {
        Self::new()
    }
}

impl Scanner {
    /// Create a new empty scanner.
    pub fn new() -> Self {
        Scanner {
            rules_by_language: HashMap::new(),
            universal_rules: Vec::new(),
        }
    }

    /// Add a rule to the scanner.
    pub fn add_rule(&mut self, rule: RuleYaml) -> Result<(), ScanError> {
        let compiled = CompiledRule::compile(rule)?;
        let language = compiled.language().to_lowercase();

        if language == "*" || language == "any" || language == "all" {
            self.universal_rules.push(compiled);
        } else {
            self.rules_by_language
                .entry(language)
                .or_insert_with(Vec::new)
                .push(compiled);
        }

        Ok(())
    }

    /// Add multiple rules to the scanner.
    pub fn add_rules(&mut self, rules: Vec<RuleYaml>) -> Result<(), ScanError> {
        for rule in rules {
            self.add_rule(rule)?;
        }
        Ok(())
    }

    /// Get the total number of rules.
    pub fn rule_count(&self) -> usize {
        let lang_count: usize = self.rules_by_language.values().map(|v| v.len()).sum();
        lang_count + self.universal_rules.len()
    }

    /// Scan a UAST tree against all applicable rules.
    pub fn scan_tree(&self, tree: &UastNode, language: &str) -> Vec<ScanResult> {
        let mut results = Vec::new();
        let language_lower = language.to_lowercase();

        // Collect applicable rules
        let mut applicable_rules: Vec<&CompiledRule> = Vec::new();
        if let Some(rules) = self.rules_by_language.get(&language_lower) {
            applicable_rules.extend(rules.iter());
        }
        applicable_rules.extend(self.universal_rules.iter());

        // Apply each rule
        for compiled_rule in applicable_rules {
            let rule_results = self.apply_rule(compiled_rule, tree);
            results.extend(rule_results);
        }

        results
    }

    /// Scan source code (requires external parsing).
    ///
    /// This method is for scanning when you already have a parsed UAST tree.
    pub fn scan_source(
        &self,
        tree: &UastNode,
        language: &str,
        file_path: Option<&Path>,
    ) -> Vec<ScanResult> {
        let mut results = self.scan_tree(tree, language);

        // Add file path to all results
        if let Some(path) = file_path {
            let path_str = path.display().to_string();
            for result in &mut results {
                result.file_path = Some(path_str.clone());
            }
        }

        results
    }

    /// Apply a single rule to a tree.
    fn apply_rule(&self, compiled_rule: &CompiledRule, tree: &UastNode) -> Vec<ScanResult> {
        let mut results = Vec::new();

        // Create a pattern matcher with the rule's constraints
        let mut matcher = PatternMatcher::new();

        // Add metavariable constraints
        for (var_name, constraints) in &compiled_rule.constraints {
            for constraint in constraints {
                matcher.add_constraint(var_name.clone(), constraint.clone());
            }
        }

        // Find all matches
        let matches = matcher.find_all(tree, &compiled_rule.pattern);

        for match_result in matches {
            // Check additional relational constraints
            if !self.check_relational_constraints(compiled_rule, &match_result.node, &matcher) {
                continue;
            }

            // Interpolate message with captures
            let message =
                interpolate_message(&compiled_rule.rule.message, &match_result.captures);

            // Create fix if present
            let fix = compiled_rule.rule.fix.as_ref().map(|fix_template| {
                Fix::new(
                    match_result.span,
                    interpolate_message(fix_template, &match_result.captures),
                )
            });

            let mut result = ScanResult::new(
                compiled_rule.rule.id.clone(),
                compiled_rule.rule.severity,
                message,
                match_result.span,
            );

            if let Some(f) = fix {
                result = result.with_fix(f);
            }

            // Add captures for debugging
            for (name, value) in &match_result.captures {
                let text = match value {
                    CapturedValue::Single(node) => {
                        node.text.clone().unwrap_or_default()
                    }
                    CapturedValue::Multiple(nodes) => nodes
                        .iter()
                        .filter_map(|n| n.text.clone())
                        .collect::<Vec<_>>()
                        .join(", "),
                };
                result = result.with_capture(name.clone(), text);
            }

            results.push(result);
        }

        results
    }

    /// Check relational constraints (inside, has).
    fn check_relational_constraints(
        &self,
        compiled_rule: &CompiledRule,
        node: &UastNode,
        matcher: &PatternMatcher,
    ) -> bool {
        // Check inside constraint
        if let Some(ref constraint) = compiled_rule.inside_constraint {
            if !constraint.evaluate(node, matcher) {
                return false;
            }
        }

        // Check has constraint
        if let Some(ref constraint) = compiled_rule.has_constraint {
            if !constraint.evaluate(node, matcher) {
                return false;
            }
        }

        true
    }
}

/// Interpolate metavariable captures into a message template.
fn interpolate_message(
    template: &str,
    captures: &HashMap<String, CapturedValue>,
) -> String {
    let mut result = template.to_string();

    for (name, value) in captures {
        let text = match value {
            CapturedValue::Single(node) => {
                node.text.clone().or_else(|| node.name.clone()).unwrap_or_default()
            }
            CapturedValue::Multiple(nodes) => nodes
                .iter()
                .filter_map(|n| n.text.clone().or_else(|| n.name.clone()))
                .collect::<Vec<_>>()
                .join(", "),
        };

        // Replace $NAME and \u{2200}NAME variants
        result = result.replace(&format!("${}", name), &text);
        result = result.replace(&format!("\u{2200}{}", name), &text);
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::uast::schema::UastKind;

    fn make_test_node(kind: UastKind, name: Option<&str>, text: Option<&str>) -> UastNode {
        let mut node = UastNode::new(kind, "rust", SourceSpan::empty());
        if let Some(n) = name {
            node = node.with_name(n);
        }
        if let Some(t) = text {
            node = node.with_text(t);
        }
        node
    }

    #[test]
    fn test_compile_simple_rule() {
        let rule = RuleYaml {
            id: "test".to_string(),
            language: "rust".to_string(),
            severity: Severity::Warning,
            message: "Test".to_string(),
            note: None,
            url: None,
            rule: RulePatternYaml {
                pattern: Some("FunctionDeclaration".to_string()),
                ..Default::default()
            },
            constraints: None,
            fix: None,
            files: None,
            ignores: None,
            tags: None,
            enabled: true,
        };

        let compiled = CompiledRule::compile(rule).unwrap();
        assert_eq!(compiled.id(), "test");
    }

    #[test]
    fn test_scanner_basic() {
        let mut scanner = Scanner::new();

        let rule = RuleYaml {
            id: "find-functions".to_string(),
            language: "rust".to_string(),
            severity: Severity::Info,
            message: "Found a function".to_string(),
            note: None,
            url: None,
            rule: RulePatternYaml {
                pattern: Some("FunctionDeclaration".to_string()),
                ..Default::default()
            },
            constraints: None,
            fix: None,
            files: None,
            ignores: None,
            tags: None,
            enabled: true,
        };

        scanner.add_rule(rule).unwrap();
        assert_eq!(scanner.rule_count(), 1);

        // Create a test tree
        let func = make_test_node(UastKind::FunctionDeclaration, Some("test_func"), None);
        let root = UastNode::new(UastKind::SourceFile, "rust", SourceSpan::empty())
            .with_child(func);

        let results = scanner.scan_tree(&root, "rust");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].rule_id, "find-functions");
    }

    #[test]
    fn test_scanner_multiple_rules() {
        let mut scanner = Scanner::new();

        let rule1 = RuleYaml {
            id: "find-functions".to_string(),
            language: "rust".to_string(),
            severity: Severity::Info,
            message: "Found function".to_string(),
            note: None,
            url: None,
            rule: RulePatternYaml {
                pattern: Some("FunctionDeclaration".to_string()),
                ..Default::default()
            },
            constraints: None,
            fix: None,
            files: None,
            ignores: None,
            tags: None,
            enabled: true,
        };

        let rule2 = RuleYaml {
            id: "find-if".to_string(),
            language: "rust".to_string(),
            severity: Severity::Warning,
            message: "Found if".to_string(),
            note: None,
            url: None,
            rule: RulePatternYaml {
                pattern: Some("IfStatement".to_string()),
                ..Default::default()
            },
            constraints: None,
            fix: None,
            files: None,
            ignores: None,
            tags: None,
            enabled: true,
        };

        scanner.add_rule(rule1).unwrap();
        scanner.add_rule(rule2).unwrap();

        // Create a test tree
        let func = make_test_node(UastKind::FunctionDeclaration, Some("test"), None);
        let if_stmt = make_test_node(UastKind::IfStatement, None, None);
        let root = UastNode::new(UastKind::SourceFile, "rust", SourceSpan::empty())
            .with_child(func)
            .with_child(if_stmt);

        let results = scanner.scan_tree(&root, "rust");
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_interpolate_message() {
        use crate::matching::CapturedValue;

        let mut captures = HashMap::new();
        captures.insert(
            "NAME".to_string(),
            CapturedValue::Single(UastNode::new(UastKind::Identifier, "rust", SourceSpan::empty()).with_text("foo")),
        );

        let message = interpolate_message("Found variable $NAME in code", &captures);
        assert_eq!(message, "Found variable foo in code");
    }

    #[test]
    fn test_scan_result_serialization() {
        let result = ScanResult::new(
            "test-rule".to_string(),
            Severity::Warning,
            "Test message".to_string(),
            SourceSpan::new(1, 0, 1, 10, 0, 10),
        );

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("test-rule"));
        assert!(json.contains("warning"));
    }
}
