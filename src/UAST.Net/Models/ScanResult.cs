using System.Text.Json.Serialization;

namespace UAST.Net;

/// <summary>
/// Represents a rule scan result (a finding).
/// </summary>
public sealed class ScanResult
{
    /// <summary>
    /// The ID of the rule that matched.
    /// </summary>
    [JsonPropertyName("rule_id")]
    public string RuleId { get; init; } = "";

    /// <summary>
    /// Severity level: "error", "warning", "info", or "hint".
    /// </summary>
    [JsonPropertyName("severity")]
    public string Severity { get; init; } = "warning";

    /// <summary>
    /// The message describing the finding.
    /// </summary>
    [JsonPropertyName("message")]
    public string Message { get; init; } = "";

    /// <summary>
    /// Source location of the finding.
    /// </summary>
    [JsonPropertyName("location")]
    public ScanLocation Location { get; init; } = new();

    /// <summary>
    /// File path where the finding was detected.
    /// </summary>
    [JsonPropertyName("file_path")]
    public string? FilePath { get; init; }

    /// <summary>
    /// Captured values from the pattern.
    /// </summary>
    [JsonPropertyName("captures")]
    public Dictionary<string, CapturedValue>? Captures { get; init; }

    /// <summary>
    /// Suggested fix, if available.
    /// </summary>
    [JsonPropertyName("fix")]
    public ScanFix? Fix { get; init; }
}

/// <summary>
/// Source location for a scan finding.
/// </summary>
public sealed class ScanLocation
{
    [JsonPropertyName("startLine")]
    public int StartLine { get; init; }

    [JsonPropertyName("startColumn")]
    public int StartColumn { get; init; }

    [JsonPropertyName("endLine")]
    public int EndLine { get; init; }

    [JsonPropertyName("endColumn")]
    public int EndColumn { get; init; }

    [JsonPropertyName("startOffset")]
    public int StartOffset { get; init; }

    [JsonPropertyName("endOffset")]
    public int EndOffset { get; init; }

    /// <summary>
    /// Convert to SourceSpan struct.
    /// </summary>
    public SourceSpan ToSourceSpan() => new()
    {
        StartLine = StartLine,
        StartColumn = StartColumn,
        EndLine = EndLine,
        EndColumn = EndColumn,
        StartOffset = StartOffset,
        EndOffset = EndOffset
    };
}

/// <summary>
/// A suggested fix for a scan finding.
/// </summary>
public sealed class ScanFix
{
    /// <summary>
    /// The replacement text.
    /// </summary>
    [JsonPropertyName("replacement")]
    public string Replacement { get; init; } = "";

    /// <summary>
    /// Start byte offset for the replacement.
    /// </summary>
    [JsonPropertyName("start_byte")]
    public int StartByte { get; init; }

    /// <summary>
    /// End byte offset for the replacement.
    /// </summary>
    [JsonPropertyName("end_byte")]
    public int EndByte { get; init; }
}

/// <summary>
/// Severity levels for scan results.
/// </summary>
public static class Severity
{
    public const string Error = "error";
    public const string Warning = "warning";
    public const string Info = "info";
    public const string Hint = "hint";

    /// <summary>
    /// Parse a severity string to a numeric level (higher = more severe).
    /// </summary>
    public static int ToLevel(string severity) => severity.ToLowerInvariant() switch
    {
        "error" => 4,
        "warning" => 3,
        "info" => 2,
        "hint" => 1,
        _ => 0
    };
}
