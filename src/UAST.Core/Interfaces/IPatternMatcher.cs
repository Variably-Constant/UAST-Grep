using UAST.Core.Matching;
using UAST.Core.Schema;

namespace UAST.Core.Interfaces;

/// <summary>
/// Interface for pattern matching against UAST trees.
/// </summary>
public interface IPatternMatcher
{
    /// <summary>
    /// Finds all matches of a pattern in a UAST tree.
    /// </summary>
    /// <param name="tree">The UAST tree to search.</param>
    /// <param name="pattern">The pattern to match.</param>
    /// <returns>All matches found.</returns>
    IEnumerable<MatchResult> Match(UastNode tree, Pattern pattern);

    /// <summary>
    /// Checks if a pattern matches at a specific node.
    /// </summary>
    /// <param name="node">The node to check.</param>
    /// <param name="pattern">The pattern to match.</param>
    /// <param name="captures">Output dictionary of captured metavariables.</param>
    /// <returns>True if the pattern matches.</returns>
    bool TryMatch(UastNode node, Pattern pattern, out IReadOnlyDictionary<string, UastNode> captures);
}

/// <summary>
/// Represents a pattern match result.
/// </summary>
public record MatchResult(
    /// <summary>
    /// The node that matched.
    /// </summary>
    UastNode MatchedNode,

    /// <summary>
    /// Captured metavariable bindings.
    /// </summary>
    IReadOnlyDictionary<string, UastNode> Captures,

    /// <summary>
    /// The source span of the match.
    /// </summary>
    SourceSpan Span
)
{
    /// <summary>
    /// Gets the source text of the match.
    /// </summary>
    public string? GetSourceText() => MatchedNode.RawSource;
}
