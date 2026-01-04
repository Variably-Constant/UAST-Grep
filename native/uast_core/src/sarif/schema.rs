//! SARIF 2.1.0 Schema Types
//!
//! This module defines the SARIF 2.1.0 schema types for serialization.
//! Based on the OASIS SARIF 2.1.0 specification:
//! <https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html>

use serde::{Deserialize, Serialize};

/// The SARIF schema URL for version 2.1.0.
pub const SARIF_SCHEMA_URL: &str =
    "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json";

/// The SARIF version string.
pub const SARIF_VERSION: &str = "2.1.0";

// ============================================================================
// Top-Level Log
// ============================================================================

/// The top-level SARIF log object.
///
/// A SARIF log contains one or more runs, where each run represents a single
/// invocation of an analysis tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifLog {
    /// The URI of the JSON schema for this log.
    #[serde(rename = "$schema")]
    pub schema: String,

    /// The SARIF format version.
    pub version: String,

    /// The set of runs contained in this log.
    pub runs: Vec<Run>,
}

impl SarifLog {
    /// Create a new SARIF log with the given runs.
    pub fn new(runs: Vec<Run>) -> Self {
        Self {
            schema: SARIF_SCHEMA_URL.to_string(),
            version: SARIF_VERSION.to_string(),
            runs,
        }
    }
}

// ============================================================================
// Run
// ============================================================================

/// A single run of an analysis tool.
///
/// A run contains the results of executing a tool on a set of analysis targets.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Run {
    /// Information about the tool that produced this run.
    pub tool: Tool,

    /// The set of results produced by the tool.
    pub results: Vec<Result>,

    /// Information about the invocation of the tool.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub invocations: Option<Vec<Invocation>>,

    /// An array of artifact objects representing files scanned.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub artifacts: Option<Vec<Artifact>>,

    /// The analysis target (e.g., project directory).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub original_uri_base_ids: Option<serde_json::Value>,
}

impl Run {
    /// Create a new run with the given tool and results.
    pub fn new(tool: Tool, results: Vec<Result>) -> Self {
        Self {
            tool,
            results,
            invocations: None,
            artifacts: None,
            original_uri_base_ids: None,
        }
    }

    /// Set the invocations for this run.
    pub fn with_invocations(mut self, invocations: Vec<Invocation>) -> Self {
        self.invocations = Some(invocations);
        self
    }

    /// Set the artifacts for this run.
    pub fn with_artifacts(mut self, artifacts: Vec<Artifact>) -> Self {
        self.artifacts = Some(artifacts);
        self
    }
}

// ============================================================================
// Tool
// ============================================================================

/// Information about an analysis tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Tool {
    /// The tool component that was run.
    pub driver: ToolComponent,

    /// Tool extensions/plugins.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extensions: Option<Vec<ToolComponent>>,
}

impl Tool {
    /// Create a new tool with the given driver component.
    pub fn new(driver: ToolComponent) -> Self {
        Self {
            driver,
            extensions: None,
        }
    }
}

/// A component of an analysis tool (driver or extension).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ToolComponent {
    /// The name of the tool component.
    pub name: String,

    /// The version of the tool component.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,

    /// The semantic version of the tool component.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_version: Option<String>,

    /// A URI where information about the tool can be found.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub information_uri: Option<String>,

    /// The rules/checks defined by this tool component.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub rules: Vec<ReportingDescriptor>,

    /// Notifications that may be emitted by the tool.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub notifications: Option<Vec<ReportingDescriptor>>,

    /// The organization that produced the tool.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub organization: Option<String>,

    /// A brief description of the tool component.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub short_description: Option<MultiformatMessageString>,

    /// A comprehensive description of the tool component.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub full_description: Option<MultiformatMessageString>,
}

impl ToolComponent {
    /// Create a new tool component with name and version.
    pub fn new(name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            version: Some(version.into()),
            semantic_version: None,
            information_uri: None,
            rules: Vec::new(),
            notifications: None,
            organization: None,
            short_description: None,
            full_description: None,
        }
    }

    /// Set the information URI.
    pub fn with_information_uri(mut self, uri: impl Into<String>) -> Self {
        self.information_uri = Some(uri.into());
        self
    }

    /// Add rules to the tool component.
    pub fn with_rules(mut self, rules: Vec<ReportingDescriptor>) -> Self {
        self.rules = rules;
        self
    }

    /// Set the organization name.
    pub fn with_organization(mut self, org: impl Into<String>) -> Self {
        self.organization = Some(org.into());
        self
    }
}

// ============================================================================
// Reporting Descriptor (Rule Definition)
// ============================================================================

/// Metadata for a reportable condition (rule/check).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportingDescriptor {
    /// A stable identifier for the rule.
    pub id: String,

    /// A human-readable name for the rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// A brief description of the rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub short_description: Option<MultiformatMessageString>,

    /// A comprehensive description of the rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub full_description: Option<MultiformatMessageString>,

    /// Help text for the rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help: Option<MultiformatMessageString>,

    /// A URI where documentation for the rule can be found.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub help_uri: Option<String>,

    /// Default configuration for the rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_configuration: Option<ReportingConfiguration>,

    /// Properties associated with the rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<PropertyBag>,
}

impl ReportingDescriptor {
    /// Create a new reporting descriptor with an ID.
    pub fn new(id: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: None,
            short_description: None,
            full_description: None,
            help: None,
            help_uri: None,
            default_configuration: None,
            properties: None,
        }
    }

    /// Set the rule name.
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }

    /// Set the short description.
    pub fn with_short_description(mut self, text: impl Into<String>) -> Self {
        self.short_description = Some(MultiformatMessageString::new(text));
        self
    }

    /// Set the full description.
    pub fn with_full_description(mut self, text: impl Into<String>) -> Self {
        self.full_description = Some(MultiformatMessageString::new(text));
        self
    }

    /// Set the help text.
    pub fn with_help(mut self, text: impl Into<String>) -> Self {
        self.help = Some(MultiformatMessageString::new(text));
        self
    }

    /// Set the help URI.
    pub fn with_help_uri(mut self, uri: impl Into<String>) -> Self {
        self.help_uri = Some(uri.into());
        self
    }

    /// Set the default severity level.
    pub fn with_default_level(mut self, level: impl Into<String>) -> Self {
        self.default_configuration = Some(ReportingConfiguration {
            level: Some(level.into()),
            enabled: None,
            rank: None,
            parameters: None,
        });
        self
    }

    /// Set properties (tags, etc.).
    pub fn with_properties(mut self, properties: PropertyBag) -> Self {
        self.properties = Some(properties);
        self
    }
}

/// Default configuration for a reporting descriptor.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportingConfiguration {
    /// The default severity level: "none", "note", "warning", "error".
    #[serde(skip_serializing_if = "Option::is_none")]
    pub level: Option<String>,

    /// Whether the rule is enabled by default.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub enabled: Option<bool>,

    /// The rank of the rule (0.0 to 100.0).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rank: Option<f64>,

    /// Rule-specific parameters.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<PropertyBag>,
}

// ============================================================================
// Result
// ============================================================================

/// A result produced by an analysis tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Result {
    /// The stable identifier of the rule that was evaluated.
    pub rule_id: String,

    /// The severity level: "none", "note", "warning", "error".
    pub level: String,

    /// A message describing the result.
    pub message: Message,

    /// The locations associated with the result.
    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    pub locations: Vec<Location>,

    /// Proposed fixes for the result.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fixes: Option<Vec<SarifFix>>,

    /// The code flows through which the result was detected.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub code_flows: Option<Vec<CodeFlow>>,

    /// Related locations that provide context.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub related_locations: Option<Vec<Location>>,

    /// A unique identifier for this result.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub guid: Option<String>,

    /// A stable fingerprint for this result.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fingerprints: Option<serde_json::Value>,

    /// Properties associated with the result.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<PropertyBag>,

    /// The index of the rule in the tool's rules array.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rule_index: Option<i32>,
}

impl Result {
    /// Create a new result.
    pub fn new(rule_id: impl Into<String>, level: impl Into<String>, message: Message) -> Self {
        Self {
            rule_id: rule_id.into(),
            level: level.into(),
            message,
            locations: Vec::new(),
            fixes: None,
            code_flows: None,
            related_locations: None,
            guid: None,
            fingerprints: None,
            properties: None,
            rule_index: None,
        }
    }

    /// Add a location to the result.
    pub fn with_location(mut self, location: Location) -> Self {
        self.locations.push(location);
        self
    }

    /// Set the locations for the result.
    pub fn with_locations(mut self, locations: Vec<Location>) -> Self {
        self.locations = locations;
        self
    }

    /// Set the fixes for the result.
    pub fn with_fixes(mut self, fixes: Vec<SarifFix>) -> Self {
        self.fixes = Some(fixes);
        self
    }

    /// Set properties for the result.
    pub fn with_properties(mut self, properties: PropertyBag) -> Self {
        self.properties = Some(properties);
        self
    }
}

// ============================================================================
// Location
// ============================================================================

/// A location within a programming artifact.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Location {
    /// The physical location (file + region).
    pub physical_location: PhysicalLocation,

    /// Logical locations (e.g., function name, class name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logical_locations: Option<Vec<LogicalLocation>>,

    /// A user-visible description of the location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<Message>,

    /// A unique identifier for this location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<i32>,
}

impl Location {
    /// Create a new location with a physical location.
    pub fn new(physical_location: PhysicalLocation) -> Self {
        Self {
            physical_location,
            logical_locations: None,
            message: None,
            id: None,
        }
    }
}

/// A physical location in a file.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PhysicalLocation {
    /// The artifact (file) containing the location.
    pub artifact_location: ArtifactLocation,

    /// The region within the artifact.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub region: Option<Region>,

    /// The context region (larger surrounding region).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context_region: Option<Region>,
}

impl PhysicalLocation {
    /// Create a new physical location.
    pub fn new(artifact_location: ArtifactLocation) -> Self {
        Self {
            artifact_location,
            region: None,
            context_region: None,
        }
    }

    /// Set the region for this location.
    pub fn with_region(mut self, region: Region) -> Self {
        self.region = Some(region);
        self
    }
}

/// A location within an artifact (file).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ArtifactLocation {
    /// A string containing a valid relative or absolute URI.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,

    /// The index of the artifact in the run's artifacts array.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub index: Option<i32>,

    /// A short description of the artifact location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<Message>,

    /// The URI base ID for relative URIs.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri_base_id: Option<String>,
}

impl ArtifactLocation {
    /// Create a new artifact location with a URI.
    pub fn new(uri: impl Into<String>) -> Self {
        Self {
            uri: Some(uri.into()),
            index: None,
            description: None,
            uri_base_id: None,
        }
    }

    /// Create an artifact location with an index reference.
    pub fn with_index(index: i32) -> Self {
        Self {
            uri: None,
            index: Some(index),
            description: None,
            uri_base_id: None,
        }
    }
}

/// A region within an artifact.
///
/// All coordinates are 1-indexed per SARIF specification.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Region {
    /// The 1-indexed line number of the first character.
    pub start_line: u32,

    /// The 1-indexed column number of the first character.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_column: Option<u32>,

    /// The 1-indexed line number of the last character.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_line: Option<u32>,

    /// The 1-indexed column number of the character after the last.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_column: Option<u32>,

    /// The 0-indexed byte offset of the first character.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub char_offset: Option<u32>,

    /// The length in characters.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub char_length: Option<u32>,

    /// The 0-indexed byte offset of the first byte.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_offset: Option<u32>,

    /// The length in bytes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub byte_length: Option<u32>,

    /// A message for this region.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<Message>,

    /// The source code in this region.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub snippet: Option<ArtifactContent>,
}

impl Region {
    /// Create a new region with start line.
    pub fn new(start_line: u32) -> Self {
        Self {
            start_line,
            start_column: None,
            end_line: None,
            end_column: None,
            char_offset: None,
            char_length: None,
            byte_offset: None,
            byte_length: None,
            message: None,
            snippet: None,
        }
    }

    /// Set the start column (1-indexed).
    pub fn with_start_column(mut self, column: u32) -> Self {
        self.start_column = Some(column);
        self
    }

    /// Set the end line.
    pub fn with_end_line(mut self, line: u32) -> Self {
        self.end_line = Some(line);
        self
    }

    /// Set the end column (1-indexed, points after the last character).
    pub fn with_end_column(mut self, column: u32) -> Self {
        self.end_column = Some(column);
        self
    }

    /// Set byte offset and length.
    pub fn with_byte_range(mut self, offset: u32, length: u32) -> Self {
        self.byte_offset = Some(offset);
        self.byte_length = Some(length);
        self
    }
}

/// A logical location (e.g., function, class).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct LogicalLocation {
    /// The name of the logical location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// A fully qualified name for the logical location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub fully_qualified_name: Option<String>,

    /// The kind of logical location (e.g., "function", "class").
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,
}

// ============================================================================
// Fix
// ============================================================================

/// A proposed fix for a result (SARIF schema).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SarifFix {
    /// A description of the fix.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<Message>,

    /// The changes to make.
    pub artifact_changes: Vec<ArtifactChange>,
}

impl SarifFix {
    /// Create a new fix with changes.
    pub fn new(changes: Vec<ArtifactChange>) -> Self {
        Self {
            description: None,
            artifact_changes: changes,
        }
    }

    /// Set the description for this fix.
    pub fn with_description(mut self, message: Message) -> Self {
        self.description = Some(message);
        self
    }
}

/// A change to an artifact (file).
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ArtifactChange {
    /// The location of the artifact to change.
    pub artifact_location: ArtifactLocation,

    /// The replacements to make.
    pub replacements: Vec<Replacement>,
}

impl ArtifactChange {
    /// Create a new artifact change.
    pub fn new(artifact_location: ArtifactLocation, replacements: Vec<Replacement>) -> Self {
        Self {
            artifact_location,
            replacements,
        }
    }
}

/// A replacement within an artifact.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Replacement {
    /// The region to be replaced.
    pub deleted_region: Region,

    /// The content to insert.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inserted_content: Option<ArtifactContent>,
}

impl Replacement {
    /// Create a new replacement.
    pub fn new(deleted_region: Region) -> Self {
        Self {
            deleted_region,
            inserted_content: None,
        }
    }

    /// Set the inserted content.
    pub fn with_inserted_content(mut self, content: ArtifactContent) -> Self {
        self.inserted_content = Some(content);
        self
    }
}

/// The content of an artifact or snippet.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ArtifactContent {
    /// The text content.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,

    /// The binary content (base64 encoded).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub binary: Option<String>,

    /// A rendered representation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rendered: Option<MultiformatMessageString>,
}

impl ArtifactContent {
    /// Create new artifact content with text.
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: Some(text.into()),
            binary: None,
            rendered: None,
        }
    }
}

// ============================================================================
// Invocation
// ============================================================================

/// Information about a tool invocation.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Invocation {
    /// Whether the invocation completed successfully.
    pub execution_successful: bool,

    /// The command line used.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub command_line: Option<String>,

    /// The arguments passed.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arguments: Option<Vec<String>>,

    /// The working directory.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub working_directory: Option<ArtifactLocation>,

    /// The start time (ISO 8601 format).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub start_time_utc: Option<String>,

    /// The end time (ISO 8601 format).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub end_time_utc: Option<String>,

    /// The exit code.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exit_code: Option<i32>,

    /// Tool execution notifications.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_execution_notifications: Option<Vec<Notification>>,
}

impl Invocation {
    /// Create a new successful invocation.
    pub fn successful() -> Self {
        Self {
            execution_successful: true,
            command_line: None,
            arguments: None,
            working_directory: None,
            start_time_utc: None,
            end_time_utc: None,
            exit_code: None,
            tool_execution_notifications: None,
        }
    }

    /// Create a new failed invocation.
    pub fn failed() -> Self {
        Self {
            execution_successful: false,
            ..Self::successful()
        }
    }
}

// ============================================================================
// Artifact
// ============================================================================

/// An artifact (typically a file) scanned by the tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Artifact {
    /// The location of the artifact.
    pub location: ArtifactLocation,

    /// The length of the artifact in bytes.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub length: Option<i64>,

    /// The MIME type of the artifact.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mime_type: Option<String>,

    /// The encoding of the artifact.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub encoding: Option<String>,

    /// The source language of the artifact.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source_language: Option<String>,

    /// Hashes of the artifact.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hashes: Option<serde_json::Value>,

    /// Properties associated with the artifact.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<PropertyBag>,
}

impl Artifact {
    /// Create a new artifact with a location.
    pub fn new(location: ArtifactLocation) -> Self {
        Self {
            location,
            length: None,
            mime_type: None,
            encoding: None,
            source_language: None,
            hashes: None,
            properties: None,
        }
    }
}

// ============================================================================
// Message Types
// ============================================================================

/// A message with plain text.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Message {
    /// The plain text message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub text: Option<String>,

    /// A markdown-formatted message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub markdown: Option<String>,

    /// A message ID for localization.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,

    /// Arguments for message string formatting.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub arguments: Option<Vec<String>>,
}

impl Message {
    /// Create a new message with text.
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: Some(text.into()),
            markdown: None,
            id: None,
            arguments: None,
        }
    }

    /// Create an empty message.
    pub fn empty() -> Self {
        Self {
            text: None,
            markdown: None,
            id: None,
            arguments: None,
        }
    }
}

/// A message string with multiple formats.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MultiformatMessageString {
    /// The plain text message.
    pub text: String,

    /// A markdown-formatted version.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub markdown: Option<String>,
}

impl MultiformatMessageString {
    /// Create a new multiformat message.
    pub fn new(text: impl Into<String>) -> Self {
        Self {
            text: text.into(),
            markdown: None,
        }
    }

    /// Add a markdown version.
    pub fn with_markdown(mut self, markdown: impl Into<String>) -> Self {
        self.markdown = Some(markdown.into());
        self
    }
}

// ============================================================================
// Code Flow
// ============================================================================

/// A code flow representing an execution path.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CodeFlow {
    /// A message describing the code flow.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<Message>,

    /// The thread flows comprising this code flow.
    pub thread_flows: Vec<ThreadFlow>,
}

/// A thread flow within a code flow.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadFlow {
    /// The thread ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,

    /// A message describing this thread flow.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<Message>,

    /// The locations in this thread flow.
    pub locations: Vec<ThreadFlowLocation>,
}

/// A location within a thread flow.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ThreadFlowLocation {
    /// The location.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,

    /// The execution order.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub execution_order: Option<i32>,

    /// The nesting level.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub nesting_level: Option<i32>,
}

// ============================================================================
// Notification
// ============================================================================

/// A notification from the tool.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Notification {
    /// The notification message.
    pub message: Message,

    /// The severity level.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub level: Option<String>,

    /// The associated rule.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub associated_rule: Option<ReportingDescriptorReference>,

    /// Exception details if this is an exception notification.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub exception: Option<Exception>,
}

/// A reference to a reporting descriptor.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportingDescriptorReference {
    /// The rule ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<String>,

    /// The index of the rule in the rules array.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub index: Option<i32>,
}

/// Exception information.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Exception {
    /// The exception kind (type name).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub kind: Option<String>,

    /// The exception message.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<String>,

    /// The stack trace.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stack: Option<Stack>,

    /// Inner exceptions.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inner_exceptions: Option<Vec<Exception>>,
}

/// A call stack.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Stack {
    /// The stack frames.
    pub frames: Vec<StackFrame>,

    /// A message about the stack.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub message: Option<Message>,
}

/// A stack frame.
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct StackFrame {
    /// The location of this frame.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub location: Option<Location>,

    /// The module containing this frame.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub module: Option<String>,

    /// The thread ID.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub thread_id: Option<i32>,

    /// Parameters to the function.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<Vec<String>>,
}

// ============================================================================
// Property Bag
// ============================================================================

/// A property bag for custom properties.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PropertyBag {
    /// Tags associated with this entity.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,

    /// Additional properties.
    #[serde(flatten)]
    pub additional: std::collections::HashMap<String, serde_json::Value>,
}

impl PropertyBag {
    /// Create an empty property bag.
    pub fn new() -> Self {
        Self::default()
    }

    /// Add tags.
    pub fn with_tags(mut self, tags: Vec<String>) -> Self {
        self.tags = Some(tags);
        self
    }

    /// Add a custom property.
    pub fn with_property(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.additional.insert(key.into(), value);
        self
    }
}
