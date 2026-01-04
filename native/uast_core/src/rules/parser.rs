//! YAML rule parser for UAST-Grep.
//!
//! This module handles loading and parsing YAML rule files, supporting:
//! - Single-rule YAML files
//! - Multi-document YAML files (multiple rules separated by ---)
//! - Recursive directory loading

use super::rule::{ConfigYaml, RuleYaml};
use std::fs;
use std::io;
use std::path::Path;
use thiserror::Error;

/// Error type for rule parsing failures.
#[derive(Error, Debug)]
pub enum RuleParseError {
    /// I/O error reading file.
    #[error("I/O error: {0}")]
    Io(#[from] io::Error),

    /// YAML parsing error.
    #[error("YAML parse error: {0}")]
    Yaml(#[from] serde_yaml::Error),

    /// Rule validation error.
    #[error("Rule validation error in '{rule_id}': {message}")]
    Validation { rule_id: String, message: String },

    /// Generic parsing error.
    #[error("Parse error: {0}")]
    Generic(String),
}

/// Result type for rule parsing.
pub type RuleParseResult<T> = Result<T, RuleParseError>;

/// Parse all rules from a YAML string.
///
/// Supports multi-document YAML files (multiple rules separated by ---).
///
/// # Arguments
///
/// * `yaml` - The YAML content to parse
///
/// # Returns
///
/// A vector of parsed rules, or an error.
pub fn parse_rules_from_string(yaml: &str) -> RuleParseResult<Vec<RuleYaml>> {
    let mut rules = Vec::new();

    // Use serde_yaml's document iterator for multi-doc support
    for doc in serde_yaml::Deserializer::from_str(yaml) {
        let rule: RuleYaml = serde::Deserialize::deserialize(doc)?;
        validate_rule(&rule)?;
        rules.push(rule);
    }

    Ok(rules)
}

/// Parse all rules from a YAML file.
///
/// # Arguments
///
/// * `path` - Path to the YAML file
///
/// # Returns
///
/// A vector of parsed rules, or an error.
pub fn parse_rules_from_file(path: &Path) -> RuleParseResult<Vec<RuleYaml>> {
    let content = fs::read_to_string(path)?;
    parse_rules_from_string(&content)
}

/// Parse all rules from a directory (recursively).
///
/// Loads all `.yaml` and `.yml` files in the directory tree.
///
/// # Arguments
///
/// * `dir` - Path to the directory
///
/// # Returns
///
/// A vector of all parsed rules, or an error.
pub fn parse_rules_from_directory(dir: &Path) -> RuleParseResult<Vec<RuleYaml>> {
    let mut rules = Vec::new();

    if !dir.is_dir() {
        return Err(RuleParseError::Generic(format!(
            "Not a directory: {}",
            dir.display()
        )));
    }

    for entry in walkdir::WalkDir::new(dir)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();
        if path.is_file() {
            if let Some(ext) = path.extension() {
                let ext = ext.to_string_lossy().to_lowercase();
                if ext == "yaml" || ext == "yml" {
                    let file_rules = parse_rules_from_file(path)?;
                    rules.extend(file_rules);
                }
            }
        }
    }

    Ok(rules)
}

/// Parse a configuration file.
///
/// # Arguments
///
/// * `path` - Path to the config file
///
/// # Returns
///
/// The parsed configuration, or an error.
pub fn parse_config(path: &Path) -> RuleParseResult<ConfigYaml> {
    let content = fs::read_to_string(path)?;
    let config: ConfigYaml = serde_yaml::from_str(&content)?;
    Ok(config)
}

/// Validate a parsed rule.
fn validate_rule(rule: &RuleYaml) -> RuleParseResult<()> {
    // Rule must have an ID
    if rule.id.is_empty() {
        return Err(RuleParseError::Validation {
            rule_id: "(empty)".to_string(),
            message: "Rule must have an 'id' field".to_string(),
        });
    }

    // Rule must have a language
    if rule.language.is_empty() {
        return Err(RuleParseError::Validation {
            rule_id: rule.id.clone(),
            message: "Rule must have a 'language' field".to_string(),
        });
    }

    // Rule must have a message
    if rule.message.is_empty() {
        return Err(RuleParseError::Validation {
            rule_id: rule.id.clone(),
            message: "Rule must have a 'message' field".to_string(),
        });
    }

    // Rule pattern must have at least one condition
    if rule.rule.is_empty() {
        return Err(RuleParseError::Validation {
            rule_id: rule.id.clone(),
            message: "Rule must have at least one pattern condition (pattern, any, all, not, kind, or regex)".to_string(),
        });
    }

    Ok(())
}

/// Rule loader that caches loaded rules and handles configuration.
#[derive(Debug, Default)]
pub struct RuleLoader {
    /// All loaded rules.
    rules: Vec<RuleYaml>,
    /// Configuration if loaded.
    config: Option<ConfigYaml>,
    /// Set of disabled rule IDs.
    disabled_rules: std::collections::HashSet<String>,
}

impl RuleLoader {
    /// Create a new rule loader.
    pub fn new() -> Self {
        Self::default()
    }

    /// Load configuration from a file.
    pub fn load_config(&mut self, path: &Path) -> RuleParseResult<()> {
        let config = parse_config(path)?;

        // Process disable list
        if let Some(disabled) = &config.disable_rules {
            for id in disabled {
                self.disabled_rules.insert(id.clone());
            }
        }

        self.config = Some(config);
        Ok(())
    }

    /// Load rules from a file.
    pub fn load_file(&mut self, path: &Path) -> RuleParseResult<usize> {
        let rules = parse_rules_from_file(path)?;
        let count = rules.len();
        self.rules.extend(rules);
        Ok(count)
    }

    /// Load rules from a directory (recursively).
    pub fn load_directory(&mut self, dir: &Path) -> RuleParseResult<usize> {
        let rules = parse_rules_from_directory(dir)?;
        let count = rules.len();
        self.rules.extend(rules);
        Ok(count)
    }

    /// Load rules from a YAML string.
    pub fn load_string(&mut self, yaml: &str) -> RuleParseResult<usize> {
        let rules = parse_rules_from_string(yaml)?;
        let count = rules.len();
        self.rules.extend(rules);
        Ok(count)
    }

    /// Get all enabled rules for a specific language.
    pub fn rules_for_language(&self, language: &str) -> Vec<&RuleYaml> {
        self.rules
            .iter()
            .filter(|r| {
                r.enabled
                    && !self.disabled_rules.contains(&r.id)
                    && (r.language.eq_ignore_ascii_case(language) || r.language == "*")
            })
            .collect()
    }

    /// Get all enabled rules.
    pub fn all_rules(&self) -> Vec<&RuleYaml> {
        self.rules
            .iter()
            .filter(|r| r.enabled && !self.disabled_rules.contains(&r.id))
            .collect()
    }

    /// Get total number of loaded rules (including disabled).
    pub fn total_rules(&self) -> usize {
        self.rules.len()
    }

    /// Get the loaded configuration.
    pub fn config(&self) -> Option<&ConfigYaml> {
        self.config.as_ref()
    }

    /// Disable a rule by ID.
    pub fn disable_rule(&mut self, id: &str) {
        self.disabled_rules.insert(id.to_string());
    }

    /// Enable a rule by ID.
    pub fn enable_rule(&mut self, id: &str) {
        self.disabled_rules.remove(id);
    }

    /// Clear all loaded rules.
    pub fn clear(&mut self) {
        self.rules.clear();
        self.disabled_rules.clear();
        self.config = None;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_single_rule() {
        let yaml = r#"
id: test-rule
language: rust
severity: warning
message: "Test message"
rule:
  pattern: "println!($$$ARGS)"
"#;
        let rules = parse_rules_from_string(yaml).unwrap();
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].id, "test-rule");
    }

    #[test]
    fn test_parse_multi_document() {
        let yaml = r#"
id: rule-1
language: rust
message: "Rule 1"
rule:
  pattern: "foo"
---
id: rule-2
language: rust
message: "Rule 2"
rule:
  pattern: "bar"
"#;
        let rules = parse_rules_from_string(yaml).unwrap();
        assert_eq!(rules.len(), 2);
        assert_eq!(rules[0].id, "rule-1");
        assert_eq!(rules[1].id, "rule-2");
    }

    #[test]
    fn test_validation_empty_id() {
        let yaml = r#"
id: ""
language: rust
message: "Test"
rule:
  pattern: "x"
"#;
        let result = parse_rules_from_string(yaml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("'id' field"));
    }

    #[test]
    fn test_validation_empty_rule() {
        let yaml = r#"
id: test
language: rust
message: "Test"
rule: {}
"#;
        let result = parse_rules_from_string(yaml);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("pattern condition"));
    }

    #[test]
    fn test_rule_loader() {
        let mut loader = RuleLoader::new();

        let yaml1 = r#"
id: rule-1
language: rust
message: "Rust rule"
rule:
  pattern: "test"
"#;
        let yaml2 = r#"
id: rule-2
language: python
message: "Python rule"
rule:
  pattern: "test"
"#;

        loader.load_string(yaml1).unwrap();
        loader.load_string(yaml2).unwrap();

        assert_eq!(loader.total_rules(), 2);
        assert_eq!(loader.rules_for_language("rust").len(), 1);
        assert_eq!(loader.rules_for_language("python").len(), 1);
        assert_eq!(loader.all_rules().len(), 2);

        // Test disabling
        loader.disable_rule("rule-1");
        assert_eq!(loader.rules_for_language("rust").len(), 0);
        assert_eq!(loader.all_rules().len(), 1);

        // Test enabling
        loader.enable_rule("rule-1");
        assert_eq!(loader.all_rules().len(), 2);
    }

    #[test]
    fn test_parse_config() {
        let yaml = r#"
rules:
  - rules/*.yaml
files:
  - "**/*.rs"
disableRules:
  - some-rule
"#;
        let config: ConfigYaml = serde_yaml::from_str(yaml).unwrap();
        assert!(config.rules.is_some());
        assert!(config.disable_rules.is_some());
    }
}
