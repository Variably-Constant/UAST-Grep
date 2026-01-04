using UAST.Net.Native;

namespace UAST.Net;

/// <summary>
/// Represents a query match from tree-sitter pattern matching.
/// </summary>
public sealed class QueryMatch
{
    /// <summary>
    /// The index of the pattern that matched (for multi-pattern queries).
    /// </summary>
    public int PatternIndex { get; init; }

    /// <summary>
    /// Captured nodes from the query.
    /// Keys are capture names (from @name in the query).
    /// </summary>
    public IReadOnlyDictionary<string, UastNode> Captures { get; init; } = new Dictionary<string, UastNode>();

    internal static unsafe QueryMatch FromNative(UastNativeBindings.UastMatch match, TreeHandle tree)
    {
        var captures = new Dictionary<string, UastNode>();

        for (int i = 0; i < match.CaptureCount; i++)
        {
            var capture = match.Captures[i];
            var name = capture.GetName();
            if (!string.IsNullOrEmpty(name))
            {
                captures[name] = new UastNode(capture.Node, tree);
            }
        }

        return new QueryMatch
        {
            PatternIndex = (int)match.PatternIndex,
            Captures = captures
        };
    }

    /// <summary>
    /// Get a captured node by name.
    /// </summary>
    /// <param name="name">Capture name (without @).</param>
    /// <returns>The captured node, or null if not found.</returns>
    public UastNode? GetCapture(string name) =>
        Captures.TryGetValue(name, out var node) ? node : null;

    /// <summary>
    /// Check if a capture exists.
    /// </summary>
    /// <param name="name">Capture name (without @).</param>
    /// <returns>True if the capture exists.</returns>
    public bool HasCapture(string name) =>
        Captures.ContainsKey(name);
}
