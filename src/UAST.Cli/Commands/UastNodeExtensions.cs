using UAST.Core.Schema;
using UAST.Native;

namespace UAST.Cli.Commands;

/// <summary>
/// Extension methods for UastNode to support native tree-sitter operations.
/// </summary>
public static class UastNodeExtensions
{
    /// <summary>
    /// Gets all descendant nodes of the specified native tree-sitter type.
    /// </summary>
    /// <param name="node">The root node to search from.</param>
    /// <param name="nativeType">The native tree-sitter node type (e.g., "function_definition").</param>
    /// <returns>All matching descendant nodes.</returns>
    public static IEnumerable<UastNode> DescendantsByNativeType(this UastNode node, string nativeType)
    {
        var results = new List<UastNode>();
        CollectDescendantsByNativeType(node, nativeType, results);
        return results;
    }

    /// <summary>
    /// Gets all descendant nodes matching any of the specified native types.
    /// </summary>
    /// <param name="node">The root node to search from.</param>
    /// <param name="nativeTypes">The native tree-sitter node types to match.</param>
    /// <returns>All matching descendant nodes.</returns>
    public static IEnumerable<UastNode> DescendantsByNativeTypes(this UastNode node, params string[] nativeTypes)
    {
        var typeSet = new HashSet<string>(nativeTypes, StringComparer.Ordinal);
        var results = new List<UastNode>();
        CollectDescendantsByNativeTypes(node, typeSet, results);
        return results;
    }

    /// <summary>
    /// Executes a tree-sitter S-expression query against the node's source.
    /// This requires the Rust FFI backend to be available.
    /// </summary>
    /// <param name="node">The root node to query.</param>
    /// <param name="query">The tree-sitter S-expression query string.</param>
    /// <param name="language">The language for parsing.</param>
    /// <returns>Query matches with captured nodes.</returns>
    public static IEnumerable<NativeQueryMatch> QueryNative(this UastNode node, string query, string language)
    {
        // Get the raw source from the node
        var source = node.RawSource;
        if (string.IsNullOrEmpty(source))
        {
            yield break;
        }

        // Use the native parser to execute the query
        using var parser = NativeParser.Create(language);
        using var tree = parser.Parse(source);

        foreach (var match in tree.Query(query))
        {
            yield return new NativeQueryMatch
            {
                PatternIndex = match.PatternIndex,
                Captures = match.Captures.Select(c => new NativeQueryCapture
                {
                    Name = c.Name,
                    NodeKind = c.Node.Kind,
                    Text = c.Node.Text,
                    StartLine = (int)c.Node.StartRow + 1,
                    StartColumn = (int)c.Node.StartColumn,
                    EndLine = (int)c.Node.EndRow + 1,
                    EndColumn = (int)c.Node.EndColumn
                }).ToList()
            };
        }
    }

    /// <summary>
    /// Recursively collects descendants matching the specified native type.
    /// </summary>
    private static void CollectDescendantsByNativeType(UastNode node, string nativeType, List<UastNode> results)
    {
        var nodeNativeType = GetNativeNodeType(node);
        if (nodeNativeType != null && string.Equals(nodeNativeType, nativeType, StringComparison.Ordinal))
        {
            results.Add(node);
        }

        foreach (var child in node.Children)
        {
            CollectDescendantsByNativeType(child, nativeType, results);
        }
    }

    /// <summary>
    /// Recursively collects descendants matching any of the specified native types.
    /// </summary>
    private static void CollectDescendantsByNativeTypes(UastNode node, HashSet<string> nativeTypes, List<UastNode> results)
    {
        var nodeNativeType = GetNativeNodeType(node);
        if (nodeNativeType != null && nativeTypes.Contains(nodeNativeType))
        {
            results.Add(node);
        }

        foreach (var child in node.Children)
        {
            CollectDescendantsByNativeTypes(child, nativeTypes, results);
        }
    }

    /// <summary>
    /// Gets the native tree-sitter node type from a UAST node.
    /// </summary>
    private static string? GetNativeNodeType(UastNode node)
    {
        // Check for UnknownNode which has NativeNodeType directly
        if (node is UnknownNode unknown)
            return unknown.NativeNodeType;

        // Check for UnknownExpression
        if (node is Core.Schema.Expressions.UnknownExpression unknownExpr)
            return unknownExpr.NativeNodeType;

        // Check Extensions
        if (node.Extensions?.TryGetValue("NativeNodeType", out var nativeType) == true)
            return nativeType as string;

        return null;
    }
}

/// <summary>
/// Represents a match from a native tree-sitter query.
/// </summary>
public sealed class NativeQueryMatch
{
    /// <summary>
    /// The index of the pattern that matched (for multi-pattern queries).
    /// </summary>
    public uint PatternIndex { get; init; }

    /// <summary>
    /// The captured nodes in this match.
    /// </summary>
    public IReadOnlyList<NativeQueryCapture> Captures { get; init; } = [];
}

/// <summary>
/// A single capture within a native query match.
/// </summary>
public sealed class NativeQueryCapture
{
    /// <summary>
    /// The name of the capture (from the query pattern, without @ prefix).
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The node kind/type.
    /// </summary>
    public required string NodeKind { get; init; }

    /// <summary>
    /// The source text of the captured node.
    /// </summary>
    public required string Text { get; init; }

    /// <summary>
    /// 1-indexed start line.
    /// </summary>
    public int StartLine { get; init; }

    /// <summary>
    /// 0-indexed start column.
    /// </summary>
    public int StartColumn { get; init; }

    /// <summary>
    /// 1-indexed end line.
    /// </summary>
    public int EndLine { get; init; }

    /// <summary>
    /// 0-indexed end column.
    /// </summary>
    public int EndColumn { get; init; }

    public override string ToString() => $"@{Name}: {NodeKind} ({(Text.Length > 50 ? Text[..47] + "..." : Text)})";
}
