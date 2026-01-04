namespace UAST.Native;

/// <summary>
/// A query match result containing captured nodes.
/// </summary>
public sealed class QueryMatch
{
    /// <summary>
    /// The index of the pattern that matched (for multi-pattern queries).
    /// </summary>
    public uint PatternIndex { get; init; }

    /// <summary>
    /// The captured nodes in this match.
    /// </summary>
    public IReadOnlyList<QueryCapture> Captures { get; init; } = [];

    internal static unsafe QueryMatch FromNative(UastNative.UastMatch match, SafeTreeHandle tree)
    {
        var captures = new List<QueryCapture>((int)match.CaptureCount);
        for (int i = 0; i < match.CaptureCount; i++)
        {
            var capture = match.Captures[i];
            captures.Add(new QueryCapture
            {
                Name = capture.GetName(),
                Node = new NativeNode(capture.Node, tree)
            });
        }

        return new QueryMatch
        {
            PatternIndex = match.PatternIndex,
            Captures = captures
        };
    }

    /// <summary>
    /// Get a capture by name.
    /// </summary>
    /// <param name="name">The capture name (without @ prefix).</param>
    /// <returns>The first capture with the given name, or null if not found.</returns>
    public QueryCapture? GetCapture(string name)
    {
        foreach (var capture in Captures)
        {
            if (capture.Name == name)
                return capture;
        }
        return null;
    }

    /// <summary>
    /// Get all captures with the given name.
    /// </summary>
    /// <param name="name">The capture name (without @ prefix).</param>
    /// <returns>All captures with the given name.</returns>
    public IEnumerable<QueryCapture> GetCaptures(string name)
    {
        foreach (var capture in Captures)
        {
            if (capture.Name == name)
                yield return capture;
        }
    }
}

/// <summary>
/// A single capture within a query match.
/// </summary>
public sealed class QueryCapture
{
    /// <summary>
    /// The name of the capture (from the query pattern, without @ prefix).
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The captured node.
    /// </summary>
    public required NativeNode Node { get; init; }

    public override string ToString() => $"@{Name}: {Node}";
}
