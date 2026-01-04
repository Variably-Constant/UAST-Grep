//! SARIF Writer - Converts scan results to SARIF format.
//!
//! This module provides the `SarifWriter` for converting UAST-Grep scan results
//! into SARIF 2.1.0 format.

use super::schema::*;
use crate::rules::{Fix as RuleFix, RuleYaml, ScanResult, Severity};
use std::collections::HashSet;

/// Convert a Severity to SARIF level string.
///
/// SARIF levels: "none", "note", "warning", "error"
/// - Error -> "error"
/// - Warning -> "warning"
/// - Info -> "note"
/// - Hint -> "note"
pub fn severity_to_sarif_level(severity: Severity) -> &'static str {
    match severity {
        Severity::Error => "error",
        Severity::Warning => "warning",
        Severity::Info => "note",
        Severity::Hint => "note",
    }
}

/// Writer for converting scan results to SARIF format.
#[derive(Debug, Clone)]
pub struct SarifWriter {
    /// The tool name.
    tool_name: String,

    /// The tool version.
    tool_version: String,

    /// Optional information URI for the tool.
    information_uri: Option<String>,

    /// Optional organization name.
    organization: Option<String>,
}

impl SarifWriter {
    /// Create a new SARIF writer with tool name and version.
    pub fn new(tool_name: impl Into<String>, tool_version: impl Into<String>) -> Self {
        Self {
            tool_name: tool_name.into(),
            tool_version: tool_version.into(),
            information_uri: None,
            organization: None,
        }
    }

    /// Set the tool information URI.
    pub fn with_information_uri(mut self, uri: impl Into<String>) -> Self {
        self.information_uri = Some(uri.into());
        self
    }

    /// Set the organization name.
    pub fn with_organization(mut self, org: impl Into<String>) -> Self {
        self.organization = Some(org.into());
        self
    }

    /// Convert scan results to a SARIF log.
    ///
    /// # Arguments
    ///
    /// * `results` - The scan results to convert
    /// * `rules` - The rule definitions (for rule metadata in the SARIF output)
    ///
    /// # Returns
    ///
    /// A `SarifLog` containing the converted results.
    pub fn from_scan_results(&self, results: &[ScanResult], rules: &[RuleYaml]) -> SarifLog {
        // Build rule descriptors from rule definitions
        let rule_descriptors: Vec<ReportingDescriptor> = rules
            .iter()
            .map(|rule| self.rule_to_descriptor(rule))
            .collect();

        // Build tool component
        let mut driver = ToolComponent::new(&self.tool_name, &self.tool_version);
        if let Some(ref uri) = self.information_uri {
            driver = driver.with_information_uri(uri);
        }
        if let Some(ref org) = self.organization {
            driver = driver.with_organization(org);
        }
        driver = driver.with_rules(rule_descriptors);

        let tool = Tool::new(driver);

        // Convert results
        let sarif_results: Vec<Result> = results
            .iter()
            .map(|r| self.scan_result_to_sarif(r))
            .collect();

        // Collect unique file paths for artifacts
        let artifacts = self.collect_artifacts(results);

        // Build run
        let mut run = Run::new(tool, sarif_results);
        if !artifacts.is_empty() {
            run = run.with_artifacts(artifacts);
        }

        SarifLog::new(vec![run])
    }

    /// Convert a scan result to a SARIF result.
    fn scan_result_to_sarif(&self, result: &ScanResult) -> Result {
        let level = severity_to_sarif_level(result.severity);
        let message = Message::new(&result.message);

        let mut sarif_result = Result::new(&result.rule_id, level, message);

        // Add location
        if let Some(ref file_path) = result.file_path {
            let location = self.create_location(file_path, &result.location);
            sarif_result = sarif_result.with_location(location);
        }

        // Add fix if present
        if let Some(ref fix) = result.fix {
            if let Some(sarif_fix) = self.create_fix(fix, result.file_path.as_deref()) {
                sarif_result = sarif_result.with_fixes(vec![sarif_fix]);
            }
        }

        // Add captures as properties
        if !result.captures.is_empty() {
            let mut props = PropertyBag::new();
            for (key, value) in &result.captures {
                props.additional.insert(key.clone(), serde_json::Value::String(value.clone()));
            }
            sarif_result = sarif_result.with_properties(props);
        }

        sarif_result
    }

    /// Create a SARIF location from a file path and source span.
    fn create_location(
        &self,
        file_path: &str,
        span: &crate::uast::schema::SourceSpan,
    ) -> Location {
        let artifact_location = ArtifactLocation::new(file_path);

        // SARIF uses 1-indexed lines and columns
        // Our SourceSpan: start_line is 1-indexed, start_column is 0-indexed
        // Convert to SARIF: columns should be 1-indexed
        let mut region = Region::new(span.start_line)
            .with_start_column(span.start_column + 1) // Convert 0-indexed to 1-indexed
            .with_end_line(span.end_line)
            .with_end_column(span.end_column + 1); // Convert 0-indexed to 1-indexed

        // Add byte range
        let length = span.end_offset.saturating_sub(span.start_offset);
        region = region.with_byte_range(span.start_offset, length);

        let physical_location = PhysicalLocation::new(artifact_location).with_region(region);

        Location::new(physical_location)
    }

    /// Create a SARIF fix from a rule fix.
    fn create_fix(&self, fix: &RuleFix, file_path: Option<&str>) -> Option<SarifFix> {
        let file_path = file_path?;

        let artifact_location = ArtifactLocation::new(file_path);

        // Create the region to delete (the span being replaced)
        let span = &fix.span;
        let deleted_region = Region::new(span.start_line)
            .with_start_column(span.start_column + 1)
            .with_end_line(span.end_line)
            .with_end_column(span.end_column + 1)
            .with_byte_range(span.start_offset, span.end_offset.saturating_sub(span.start_offset));

        // Create the replacement
        let inserted_content = ArtifactContent::new(&fix.replacement);
        let replacement = Replacement::new(deleted_region).with_inserted_content(inserted_content);

        let artifact_change = ArtifactChange::new(artifact_location, vec![replacement]);

        Some(SarifFix::new(vec![artifact_change]))
    }

    /// Convert a rule definition to a SARIF reporting descriptor.
    fn rule_to_descriptor(&self, rule: &RuleYaml) -> ReportingDescriptor {
        let mut descriptor = ReportingDescriptor::new(&rule.id)
            .with_name(&rule.id)
            .with_short_description(&rule.message)
            .with_default_level(severity_to_sarif_level(rule.severity));

        // Add help text if note is provided
        if let Some(ref note) = rule.note {
            descriptor = descriptor.with_help(note);
        }

        // Add help URI if provided
        if let Some(ref url) = rule.url {
            descriptor = descriptor.with_help_uri(url);
        }

        // Add tags as properties
        if let Some(ref tags) = rule.tags {
            let props = PropertyBag::new().with_tags(tags.clone());
            descriptor = descriptor.with_properties(props);
        }

        descriptor
    }

    /// Collect unique file paths as artifacts.
    fn collect_artifacts(&self, results: &[ScanResult]) -> Vec<Artifact> {
        let mut seen: HashSet<String> = HashSet::new();
        let mut artifacts = Vec::new();

        for result in results {
            if let Some(ref path) = result.file_path {
                if !seen.contains(path) {
                    seen.insert(path.clone());
                    let artifact_location = ArtifactLocation::new(path);
                    artifacts.push(Artifact::new(artifact_location));
                }
            }
        }

        artifacts
    }

    /// Serialize the SARIF log to a JSON string.
    pub fn to_json(&self, log: &SarifLog) -> std::result::Result<String, serde_json::Error> {
        serde_json::to_string(log)
    }

    /// Serialize the SARIF log to a pretty-printed JSON string.
    pub fn to_json_pretty(&self, log: &SarifLog) -> std::result::Result<String, serde_json::Error> {
        serde_json::to_string_pretty(log)
    }
}

impl Default for SarifWriter {
    fn default() -> Self {
        Self::new("UAST-Grep", "1.0.0")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::uast::schema::SourceSpan;

    #[test]
    fn test_sarif_writer_creation() {
        let writer = SarifWriter::new("TestTool", "2.0.0")
            .with_information_uri("https://example.com")
            .with_organization("Test Org");

        assert_eq!(writer.tool_name, "TestTool");
        assert_eq!(writer.tool_version, "2.0.0");
        assert_eq!(writer.information_uri, Some("https://example.com".to_string()));
        assert_eq!(writer.organization, Some("Test Org".to_string()));
    }

    #[test]
    fn test_severity_mapping() {
        assert_eq!(severity_to_sarif_level(Severity::Error), "error");
        assert_eq!(severity_to_sarif_level(Severity::Warning), "warning");
        assert_eq!(severity_to_sarif_level(Severity::Info), "note");
        assert_eq!(severity_to_sarif_level(Severity::Hint), "note");
    }

    #[test]
    fn test_create_location() {
        let writer = SarifWriter::default();
        let span = SourceSpan::new(10, 5, 10, 20, 100, 115);

        let location = writer.create_location("/path/to/file.ps1", &span);

        assert_eq!(
            location.physical_location.artifact_location.uri,
            Some("/path/to/file.ps1".to_string())
        );

        let region = location.physical_location.region.unwrap();
        assert_eq!(region.start_line, 10);
        assert_eq!(region.start_column, Some(6)); // 0-indexed 5 -> 1-indexed 6
        assert_eq!(region.end_line, Some(10));
        assert_eq!(region.end_column, Some(21)); // 0-indexed 20 -> 1-indexed 21
        assert_eq!(region.byte_offset, Some(100));
        assert_eq!(region.byte_length, Some(15));
    }

    #[test]
    fn test_empty_results() {
        let writer = SarifWriter::new("Test", "1.0.0");
        let log = writer.from_scan_results(&[], &[]);

        assert_eq!(log.version, "2.1.0");
        assert_eq!(log.runs.len(), 1);
        assert!(log.runs[0].results.is_empty());
        assert_eq!(log.runs[0].tool.driver.name, "Test");
    }

    #[test]
    fn test_with_results() {
        let results = vec![
            ScanResult::new(
                "rule-1".to_string(),
                Severity::Error,
                "Error message".to_string(),
                SourceSpan::new(1, 0, 1, 10, 0, 10),
            )
            .with_file_path("/file.ps1"),
        ];

        let writer = SarifWriter::default();
        let log = writer.from_scan_results(&results, &[]);

        assert_eq!(log.runs[0].results.len(), 1);
        let result = &log.runs[0].results[0];
        assert_eq!(result.rule_id, "rule-1");
        assert_eq!(result.level, "error");
        assert_eq!(result.message.text, Some("Error message".to_string()));
    }

    #[test]
    fn test_artifacts_collection() {
        let results = vec![
            ScanResult::new(
                "r1".to_string(),
                Severity::Info,
                "Msg".to_string(),
                SourceSpan::empty(),
            )
            .with_file_path("/a.ps1"),
            ScanResult::new(
                "r2".to_string(),
                Severity::Info,
                "Msg".to_string(),
                SourceSpan::empty(),
            )
            .with_file_path("/b.ps1"),
            ScanResult::new(
                "r3".to_string(),
                Severity::Info,
                "Msg".to_string(),
                SourceSpan::empty(),
            )
            .with_file_path("/a.ps1"), // Duplicate
        ];

        let writer = SarifWriter::default();
        let log = writer.from_scan_results(&results, &[]);

        let artifacts = log.runs[0].artifacts.as_ref().unwrap();
        assert_eq!(artifacts.len(), 2); // Only unique paths
    }

    #[test]
    fn test_json_output() {
        let writer = SarifWriter::new("UAST-Grep", "1.0.0");
        let log = writer.from_scan_results(&[], &[]);

        let json = writer.to_json(&log).unwrap();
        assert!(json.contains(r#""$schema""#));
        assert!(json.contains(r#""version":"2.1.0""#));

        let pretty = writer.to_json_pretty(&log).unwrap();
        assert!(pretty.contains("\"version\": \"2.1.0\""));
    }
}
