//! YAML Rules Engine for UAST-Grep
//!
//! This module provides a complete YAML-based rules engine for scanning code
//! against user-defined patterns. It supports:
//!
//! - YAML rule definitions with flexible pattern matching
//! - Multi-document YAML files (multiple rules per file)
//! - Directory-based rule loading
//! - Metavariable constraints (kind, regex, pattern)
//! - Relational constraints (inside, has, precedes, follows)
//! - Auto-fix support with metavariable interpolation
//!
//! # Example YAML Rule
//!
//! ```yaml
//! id: no-write-host
//! language: powershell
//! severity: warning
//! message: "Avoid using Write-Host, use Write-Output instead"
//!
//! rule:
//!   pattern: "Write-Host $$$ARGS"
//!
//! fix: "Write-Output $$$ARGS"
//! ```
//!
//! # Usage
//!
//! ```rust,ignore
//! use uast_core::rules::{RuleLoader, Scanner};
//!
//! // Load rules
//! let mut loader = RuleLoader::new();
//! loader.load_file(Path::new("rules/my-rules.yaml"))?;
//!
//! // Create scanner
//! let mut scanner = Scanner::new();
//! scanner.add_rules(loader.all_rules().into_iter().cloned().collect())?;
//!
//! // Scan a UAST tree
//! let results = scanner.scan_tree(&tree, "powershell");
//! ```
//!
//! # Architecture
//!
//! The rules engine integrates with the existing pattern matching infrastructure:
//!
//! ```text
//! ┌─────────────────┐     ┌─────────────────┐
//! │   RuleLoader    │────>│    Scanner      │
//! │  (YAML parsing) │     │ (rule matching) │
//! └─────────────────┘     └────────┬────────┘
//!                                  │
//!                         ┌────────▼────────┐
//!                         │ PatternMatcher  │
//!                         │ (from matching/)│
//!                         └─────────────────┘
//! ```

mod fix;
mod parser;
mod rule;
mod scanner;

// Re-export main types
pub use fix::{apply_fix, apply_fixes, interpolate_fix, Fix, FixResult};

pub use parser::{
    parse_config, parse_rules_from_directory, parse_rules_from_file, parse_rules_from_string,
    RuleLoader, RuleParseError, RuleParseResult,
};

pub use rule::{
    ConfigYaml, ConstraintYaml, RelationYaml, RuleOverrideYaml, RulePatternYaml, RuleYaml,
    Severity,
};

pub use scanner::{CompiledRule, ScanError, ScanResult, Scanner};

#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::uast::schema::{SourceSpan, UastKind, UastNode};

    fn make_test_tree() -> UastNode {
        // Create a simple AST: SourceFile -> FunctionDeclaration -> Block -> CallExpression
        let call = UastNode::new(UastKind::CallExpression, "rust", SourceSpan::new(3, 4, 3, 20, 30, 46))
            .with_name("println");

        let block = UastNode::new(UastKind::Block, "rust", SourceSpan::new(2, 0, 4, 1, 20, 50))
            .with_child(call);

        let func = UastNode::new(UastKind::FunctionDeclaration, "rust", SourceSpan::new(1, 0, 5, 1, 0, 60))
            .with_name("main")
            .with_child(block);

        UastNode::new(UastKind::SourceFile, "rust", SourceSpan::new(1, 0, 5, 1, 0, 60))
            .with_child(func)
    }

    #[test]
    fn test_full_rule_loading_and_scanning() {
        let yaml = r#"
id: find-functions
language: rust
severity: info
message: "Found function declaration"
rule:
  pattern: FunctionDeclaration
---
id: find-calls
language: rust
severity: warning
message: "Found call expression"
rule:
  pattern: CallExpression
"#;

        // Load rules
        let rules = parse_rules_from_string(yaml).unwrap();
        assert_eq!(rules.len(), 2);

        // Create scanner
        let mut scanner = Scanner::new();
        scanner.add_rules(rules).unwrap();
        assert_eq!(scanner.rule_count(), 2);

        // Scan
        let tree = make_test_tree();
        let results = scanner.scan_tree(&tree, "rust");

        // Should find 1 function and 1 call
        assert_eq!(results.len(), 2);

        let func_results: Vec<_> = results.iter().filter(|r| r.rule_id == "find-functions").collect();
        let call_results: Vec<_> = results.iter().filter(|r| r.rule_id == "find-calls").collect();

        assert_eq!(func_results.len(), 1);
        assert_eq!(call_results.len(), 1);
    }

    #[test]
    fn test_rule_with_fix() {
        let yaml = r#"
id: prefer-writeln
language: rust
severity: warning
message: "Use writeln! instead of println!"
rule:
  kind: CallExpression
fix: "writeln!"
"#;

        let rules = parse_rules_from_string(yaml).unwrap();
        let mut scanner = Scanner::new();
        scanner.add_rules(rules).unwrap();

        let tree = make_test_tree();
        let results = scanner.scan_tree(&tree, "rust");

        assert!(!results.is_empty());
        let result = &results[0];
        assert!(result.fix.is_some());
        assert_eq!(result.fix.as_ref().unwrap().replacement, "writeln!");
    }

    #[test]
    fn test_any_pattern() {
        let yaml = r#"
id: find-declarations
language: rust
severity: info
message: "Found declaration"
rule:
  any:
    - pattern: FunctionDeclaration
    - pattern: TypeDeclaration
"#;

        let rules = parse_rules_from_string(yaml).unwrap();
        let mut scanner = Scanner::new();
        scanner.add_rules(rules).unwrap();

        let tree = make_test_tree();
        let results = scanner.scan_tree(&tree, "rust");

        // Should find the function declaration
        assert!(!results.is_empty());
    }

    #[test]
    fn test_language_filtering() {
        let yaml = r#"
id: python-only
language: python
severity: info
message: "Python rule"
rule:
  pattern: FunctionDeclaration
"#;

        let rules = parse_rules_from_string(yaml).unwrap();
        let mut scanner = Scanner::new();
        scanner.add_rules(rules).unwrap();

        let tree = make_test_tree(); // Rust tree
        let results = scanner.scan_tree(&tree, "rust");

        // Python rule should not match Rust code
        assert!(results.is_empty());
    }

    #[test]
    fn test_universal_rule() {
        let yaml = r#"
id: any-language
language: "*"
severity: info
message: "Universal rule"
rule:
  pattern: FunctionDeclaration
"#;

        let rules = parse_rules_from_string(yaml).unwrap();
        let mut scanner = Scanner::new();
        scanner.add_rules(rules).unwrap();

        let tree = make_test_tree();
        let results = scanner.scan_tree(&tree, "rust");

        // Universal rule should match any language
        assert!(!results.is_empty());
    }
}
