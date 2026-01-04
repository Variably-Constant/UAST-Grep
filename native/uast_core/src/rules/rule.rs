//! Rule definitions for UAST-Grep YAML rules.
//!
//! This module defines the core rule structures that can be deserialized from YAML files.
//! The structure mirrors the C# implementation in UAST.Core.Rules.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Severity level for rule matches.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum Severity {
    /// Hint-level (lowest priority).
    Hint,
    /// Information-level.
    Info,
    /// Warning-level (default).
    #[default]
    Warning,
    /// Error-level (highest priority).
    Error,
}

impl Severity {
    /// Get the display name for this severity.
    pub fn as_str(&self) -> &'static str {
        match self {
            Severity::Hint => "hint",
            Severity::Info => "info",
            Severity::Warning => "warning",
            Severity::Error => "error",
        }
    }

    /// Get a numeric level for sorting (higher = more severe).
    pub fn level(&self) -> u8 {
        match self {
            Severity::Hint => 0,
            Severity::Info => 1,
            Severity::Warning => 2,
            Severity::Error => 3,
        }
    }
}

/// A YAML rule definition.
///
/// This is the top-level structure for a single rule in a YAML file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RuleYaml {
    /// Unique identifier for the rule (e.g., "no-write-host").
    pub id: String,

    /// Target language (e.g., "powershell", "csharp", "rust").
    pub language: String,

    /// Severity level.
    #[serde(default)]
    pub severity: Severity,

    /// Human-readable message describing the issue.
    /// May contain metavariable interpolations like $NAME or \u{2200}NAME.
    pub message: String,

    /// Optional extended explanation or notes.
    #[serde(default)]
    pub note: Option<String>,

    /// Optional URL to documentation about this rule.
    #[serde(default)]
    pub url: Option<String>,

    /// The pattern rule definition.
    pub rule: RulePatternYaml,

    /// Additional constraints on captured metavariables.
    /// Key is the metavariable name (without $ or \u{2200}).
    #[serde(default)]
    pub constraints: Option<HashMap<String, ConstraintYaml>>,

    /// Optional fix template for auto-fix.
    /// May contain metavariable interpolations.
    #[serde(default)]
    pub fix: Option<String>,

    /// Optional glob patterns for files to include.
    #[serde(default)]
    pub files: Option<Vec<String>>,

    /// Optional glob patterns for files to exclude.
    #[serde(default)]
    pub ignores: Option<Vec<String>>,

    /// Optional tags for categorizing rules.
    #[serde(default)]
    pub tags: Option<Vec<String>>,

    /// Whether this rule is enabled (default true).
    #[serde(default = "default_enabled")]
    pub enabled: bool,
}

fn default_enabled() -> bool {
    true
}

/// Represents a pattern within a rule.
///
/// Supports simple patterns, composites (any/all/not), and relations (inside/has).
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RulePatternYaml {
    /// A simple pattern string (e.g., "Write-Host $$$ARGS").
    #[serde(default)]
    pub pattern: Option<String>,

    /// Match any of these patterns (OR).
    #[serde(default)]
    pub any: Option<Vec<RulePatternYaml>>,

    /// Match all of these patterns (AND).
    #[serde(default)]
    pub all: Option<Vec<RulePatternYaml>>,

    /// Negate this pattern (NOT).
    #[serde(default)]
    pub not: Option<Box<RulePatternYaml>>,

    /// Require the match to be inside an ancestor matching this pattern.
    #[serde(default)]
    pub inside: Option<RelationYaml>,

    /// Require the match to NOT be inside an ancestor matching this pattern.
    #[serde(default, rename = "notInside")]
    pub not_inside: Option<RelationYaml>,

    /// Require the match to have a descendant matching this pattern.
    #[serde(default)]
    pub has: Option<RelationYaml>,

    /// Require the match to NOT have a descendant matching this pattern.
    #[serde(default, rename = "notHas")]
    pub not_has: Option<RelationYaml>,

    /// Require a sibling after this to match (precedes).
    #[serde(default)]
    pub precedes: Option<RelationYaml>,

    /// Require a sibling before this to match (follows).
    #[serde(default)]
    pub follows: Option<RelationYaml>,

    /// Match by node kind directly (e.g., "CommandExpression").
    #[serde(default)]
    pub kind: Option<String>,

    /// Match by regex on source text.
    #[serde(default)]
    pub regex: Option<String>,
}

impl RulePatternYaml {
    /// Check if this pattern has any defined fields.
    pub fn is_empty(&self) -> bool {
        self.pattern.is_none()
            && self.any.is_none()
            && self.all.is_none()
            && self.not.is_none()
            && self.inside.is_none()
            && self.not_inside.is_none()
            && self.has.is_none()
            && self.not_has.is_none()
            && self.precedes.is_none()
            && self.follows.is_none()
            && self.kind.is_none()
            && self.regex.is_none()
    }
}

/// Represents constraint options for a captured metavariable.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConstraintYaml {
    /// Required node kind (e.g., "HashtableAst").
    #[serde(default)]
    pub kind: Option<String>,

    /// Regex pattern the source text must match.
    #[serde(default)]
    pub regex: Option<String>,

    /// A pattern the captured value must match.
    #[serde(default)]
    pub pattern: Option<RulePatternYaml>,

    /// Regex pattern the source text must NOT match.
    #[serde(default, rename = "notRegex")]
    pub not_regex: Option<String>,

    /// Node kind that must NOT match.
    #[serde(default, rename = "notKind")]
    pub not_kind: Option<String>,
}

/// Represents a relational constraint (inside, has, precedes, follows).
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RelationYaml {
    /// The pattern to match in the relation.
    #[serde(default)]
    pub pattern: Option<String>,

    /// Node kind to match.
    #[serde(default)]
    pub kind: Option<String>,

    /// How far to search: "end" (default) or "neighbor".
    #[serde(default = "default_stop_by")]
    pub stop_by: String,

    /// Whether the relation must be immediate (for precedes/follows).
    #[serde(default)]
    pub immediate: bool,
}

fn default_stop_by() -> String {
    "end".to_string()
}

/// Configuration file schema for UAST-Grep.yaml.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConfigYaml {
    /// Paths to rule files to load.
    #[serde(default)]
    pub rules: Option<Vec<String>>,

    /// Glob patterns for files to include.
    #[serde(default)]
    pub files: Option<Vec<String>>,

    /// Glob patterns for files to exclude.
    #[serde(default)]
    pub ignores: Option<Vec<String>>,

    /// Rules to disable by ID.
    #[serde(default, rename = "disableRules")]
    pub disable_rules: Option<Vec<String>>,

    /// Rule overrides by ID.
    #[serde(default, rename = "ruleOverrides")]
    pub rule_overrides: Option<HashMap<String, RuleOverrideYaml>>,
}

/// Allows overriding rule properties in config.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct RuleOverrideYaml {
    /// Override severity level.
    #[serde(default)]
    pub severity: Option<Severity>,

    /// Override enabled state.
    #[serde(default)]
    pub enabled: Option<bool>,

    /// Override fix template.
    #[serde(default)]
    pub fix: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity_default() {
        let s: Severity = Default::default();
        assert_eq!(s, Severity::Warning);
    }

    #[test]
    fn test_severity_level() {
        assert!(Severity::Error.level() > Severity::Warning.level());
        assert!(Severity::Warning.level() > Severity::Info.level());
        assert!(Severity::Info.level() > Severity::Hint.level());
    }

    #[test]
    fn test_deserialize_simple_rule() {
        let yaml = r#"
id: no-write-host
language: powershell
severity: warning
message: "Avoid using Write-Host"
rule:
  pattern: "Write-Host $$$ARGS"
"#;
        let rule: RuleYaml = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(rule.id, "no-write-host");
        assert_eq!(rule.language, "powershell");
        assert_eq!(rule.severity, Severity::Warning);
        assert!(rule.rule.pattern.is_some());
        assert!(rule.enabled);
    }

    #[test]
    fn test_deserialize_rule_with_constraints() {
        let yaml = r#"
id: test-rule
language: rust
severity: error
message: "Found $VAR"
rule:
  pattern: "$VAR"
constraints:
  VAR:
    kind: "identifier"
    regex: "^test_"
"#;
        let rule: RuleYaml = serde_yaml::from_str(yaml).unwrap();
        assert!(rule.constraints.is_some());
        let constraints = rule.constraints.unwrap();
        assert!(constraints.contains_key("VAR"));
        let var_constraint = &constraints["VAR"];
        assert_eq!(var_constraint.kind.as_deref(), Some("identifier"));
        assert_eq!(var_constraint.regex.as_deref(), Some("^test_"));
    }

    #[test]
    fn test_deserialize_rule_with_fix() {
        let yaml = r#"
id: prefer-const
language: javascript
severity: info
message: "Use const instead of let for $VAR"
rule:
  pattern: "let $VAR = $VALUE"
fix: "const $VAR = $VALUE"
"#;
        let rule: RuleYaml = serde_yaml::from_str(yaml).unwrap();
        assert_eq!(rule.fix.as_deref(), Some("const $VAR = $VALUE"));
    }

    #[test]
    fn test_deserialize_any_pattern() {
        let yaml = r#"
id: console-methods
language: javascript
severity: warning
message: "Avoid console logging"
rule:
  any:
    - pattern: "console.log($$$ARGS)"
    - pattern: "console.warn($$$ARGS)"
    - pattern: "console.error($$$ARGS)"
"#;
        let rule: RuleYaml = serde_yaml::from_str(yaml).unwrap();
        assert!(rule.rule.any.is_some());
        assert_eq!(rule.rule.any.as_ref().unwrap().len(), 3);
    }

    #[test]
    fn test_deserialize_config() {
        let yaml = r#"
rules:
  - rules/*.yaml
  - custom-rules/*.yaml
files:
  - "**/*.ps1"
  - "**/*.psm1"
ignores:
  - "**/test/**"
disableRules:
  - rule-1
  - rule-2
"#;
        let config: ConfigYaml = serde_yaml::from_str(yaml).unwrap();
        assert!(config.rules.is_some());
        assert_eq!(config.rules.as_ref().unwrap().len(), 2);
        assert!(config.disable_rules.is_some());
        assert_eq!(config.disable_rules.as_ref().unwrap().len(), 2);
    }
}
