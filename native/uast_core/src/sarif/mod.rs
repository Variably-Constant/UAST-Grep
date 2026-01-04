//! SARIF 2.1.0 Output Format for UAST-Grep
//!
//! This module implements the Static Analysis Results Interchange Format (SARIF)
//! version 2.1.0 for exporting scan results. SARIF is the OASIS standard for
//! static analysis tools, supported by GitHub Code Scanning, Azure DevOps,
//! VS Code SARIF Viewer, and other tools.
//!
//! # Specification
//!
//! Based on OASIS SARIF 2.1.0:
//! <https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html>
//!
//! # Usage
//!
//! ```rust,ignore
//! use uast_core::sarif::{SarifWriter, SarifLog};
//! use uast_core::rules::{ScanResult, Scanner};
//!
//! // After scanning...
//! let writer = SarifWriter::new("UAST-Grep", "1.0.0");
//! let sarif_log = writer.from_scan_results(&results, &rules);
//! let json = writer.to_json_pretty(&sarif_log)?;
//! ```

mod schema;
mod writer;

pub use schema::*;
pub use writer::*;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rules::{ScanResult, Severity};
    use crate::uast::schema::SourceSpan;

    #[test]
    fn test_sarif_log_serialization() {
        let log = SarifLog::new(vec![Run::new(
            Tool::new(ToolComponent::new("UAST-Grep", "1.0.0")),
            vec![],
        )]);

        let json = serde_json::to_string_pretty(&log).unwrap();
        assert!(json.contains(r#""$schema""#));
        assert!(json.contains(r#""version": "2.1.0""#));
        assert!(json.contains(r#""name": "UAST-Grep""#));
    }

    #[test]
    fn test_sarif_writer_empty_results() {
        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&[], &[]);

        assert_eq!(log.version, "2.1.0");
        assert_eq!(log.runs.len(), 1);
        assert!(log.runs[0].results.is_empty());
    }

    #[test]
    fn test_sarif_writer_with_results() {
        let results = vec![ScanResult::new(
            "no-write-host".to_string(),
            Severity::Warning,
            "Avoid using Write-Host".to_string(),
            SourceSpan::new(10, 4, 10, 20, 100, 116),
        )
        .with_file_path("/path/to/script.ps1")];

        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&results, &[]);

        assert_eq!(log.runs[0].results.len(), 1);
        let result = &log.runs[0].results[0];
        assert_eq!(result.rule_id, "no-write-host");
        assert_eq!(result.level, "warning");
        assert_eq!(result.locations.len(), 1);
    }

    #[test]
    fn test_severity_mapping() {
        assert_eq!(severity_to_sarif_level(Severity::Error), "error");
        assert_eq!(severity_to_sarif_level(Severity::Warning), "warning");
        assert_eq!(severity_to_sarif_level(Severity::Info), "note");
        assert_eq!(severity_to_sarif_level(Severity::Hint), "note");
    }

    #[test]
    fn test_sarif_with_fix() {
        use crate::rules::Fix;

        let mut result = ScanResult::new(
            "prefer-output".to_string(),
            Severity::Warning,
            "Use Write-Output".to_string(),
            SourceSpan::new(5, 0, 5, 20, 50, 70),
        );
        result.fix = Some(Fix::new(SourceSpan::new(5, 0, 5, 20, 50, 70), "Write-Output"));
        result.file_path = Some("/path/to/script.ps1".to_string()); // Required for SARIF fix output

        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&[result], &[]);

        assert!(log.runs[0].results[0].fixes.is_some());
        let fixes = log.runs[0].results[0].fixes.as_ref().unwrap();
        assert_eq!(fixes.len(), 1);
    }

    #[test]
    fn test_sarif_json_format() {
        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&[], &[]);

        let json = writer.to_json(&log);
        assert!(json.is_ok());
        let json = json.unwrap();

        // Verify it's valid JSON by parsing
        let parsed: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed["version"], "2.1.0");
    }

    #[test]
    fn test_sarif_with_rule_descriptors() {
        use crate::rules::RuleYaml;

        let rules = vec![RuleYaml {
            id: "no-write-host".to_string(),
            language: "powershell".to_string(),
            severity: Severity::Warning,
            message: "Avoid Write-Host for output".to_string(),
            note: Some("Use Write-Output or Write-Information instead".to_string()),
            url: Some("https://example.com/rules/no-write-host".to_string()),
            rule: Default::default(),
            constraints: None,
            fix: Some("Write-Output $$$ARGS".to_string()),
            files: None,
            ignores: None,
            tags: Some(vec!["best-practice".to_string(), "output".to_string()]),
            enabled: true,
        }];

        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&[], &rules);

        let driver = &log.runs[0].tool.driver;
        assert_eq!(driver.rules.len(), 1);

        let rule = &driver.rules[0];
        assert_eq!(rule.id, "no-write-host");
        assert!(rule.help_uri.is_some());
    }

    #[test]
    fn test_sarif_multiple_files() {
        let results = vec![
            ScanResult::new(
                "rule-1".to_string(),
                Severity::Error,
                "Error 1".to_string(),
                SourceSpan::new(1, 0, 1, 10, 0, 10),
            )
            .with_file_path("/path/file1.ps1"),
            ScanResult::new(
                "rule-1".to_string(),
                Severity::Error,
                "Error 2".to_string(),
                SourceSpan::new(5, 0, 5, 15, 40, 55),
            )
            .with_file_path("/path/file2.ps1"),
            ScanResult::new(
                "rule-2".to_string(),
                Severity::Warning,
                "Warning 1".to_string(),
                SourceSpan::new(10, 0, 10, 20, 100, 120),
            )
            .with_file_path("/path/file1.ps1"),
        ];

        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&results, &[]);

        assert_eq!(log.runs[0].results.len(), 3);

        // Verify artifacts are collected
        if let Some(artifacts) = &log.runs[0].artifacts {
            assert!(artifacts.len() >= 1);
        }
    }

    #[test]
    fn test_sarif_region_coordinates() {
        // Test that SARIF region uses 1-indexed lines and columns
        let result = ScanResult::new(
            "test-rule".to_string(),
            Severity::Info,
            "Test".to_string(),
            SourceSpan::new(10, 5, 12, 15, 100, 150),
        )
        .with_file_path("/test.ps1");

        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&[result], &[]);

        let location = &log.runs[0].results[0].locations[0];
        let region = location.physical_location.region.as_ref().unwrap();

        // SARIF uses 1-indexed lines and columns
        assert_eq!(region.start_line, 10);
        assert_eq!(region.start_column, Some(6)); // SARIF columns are 1-indexed, SourceSpan is 0-indexed
        assert_eq!(region.end_line, Some(12));
        assert_eq!(region.end_column, Some(16));
    }
}
