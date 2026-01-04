using System.Text.Json.Serialization;

namespace UAST.Net;

/// <summary>
/// Represents a pattern match result.
/// </summary>
public sealed class MatchResult
{
    /// <summary>
    /// The kind of node that matched.
    /// </summary>
    [JsonPropertyName("nodeKind")]
    public string NodeKind { get; init; } = "";

    /// <summary>
    /// Source location of the match.
    /// </summary>
    [JsonPropertyName("span")]
    public MatchSpan Span { get; init; } = new();

    /// <summary>
    /// Captured values from metavariables in the pattern.
    /// Keys are capture names (without $), values contain the captured nodes.
    /// </summary>
    [JsonPropertyName("captures")]
    public Dictionary<string, CapturedValue> Captures { get; init; } = [];
}

/// <summary>
/// Source span for a match (JSON-compatible).
/// </summary>
public sealed class MatchSpan
{
    [JsonPropertyName("startLine")]
    public int StartLine { get; init; }

    [JsonPropertyName("startColumn")]
    public int StartColumn { get; init; }

    [JsonPropertyName("endLine")]
    public int EndLine { get; init; }

    [JsonPropertyName("endColumn")]
    public int EndColumn { get; init; }

    /// <summary>
    /// Convert to SourceSpan struct.
    /// </summary>
    public SourceSpan ToSourceSpan() => new()
    {
        StartLine = StartLine,
        StartColumn = StartColumn,
        EndLine = EndLine,
        EndColumn = EndColumn
    };
}

/// <summary>
/// A captured value from a pattern match.
/// </summary>
public sealed class CapturedValue
{
    /// <summary>
    /// Number of nodes captured.
    /// </summary>
    [JsonPropertyName("count")]
    public int Count { get; init; }

    /// <summary>
    /// The captured nodes.
    /// </summary>
    [JsonPropertyName("nodes")]
    public List<CapturedNode> Nodes { get; init; } = [];
}

/// <summary>
/// A single captured node from a pattern match.
/// </summary>
public sealed class CapturedNode
{
    /// <summary>
    /// The node kind.
    /// </summary>
    [JsonPropertyName("kind")]
    public string Kind { get; init; } = "";

    /// <summary>
    /// The source text of the node.
    /// </summary>
    [JsonPropertyName("text")]
    public string? Text { get; init; }

    /// <summary>
    /// The name of the node (for identifiers, etc.).
    /// </summary>
    [JsonPropertyName("name")]
    public string? Name { get; init; }
}
