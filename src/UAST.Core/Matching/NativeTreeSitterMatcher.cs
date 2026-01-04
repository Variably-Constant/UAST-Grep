using UAST.Core.Schema;

namespace UAST.Core.Matching;

/// <summary>
/// High-performance native tree-sitter pattern matcher.
/// Works directly with tree-sitter node types without UAST conversion.
/// This provides maximum performance by skipping the UAST mapping layer.
///
/// Performance benefits:
/// - No UAST conversion overhead (saves ~27s on large codebases)
/// - Direct string comparison on node types
/// - Minimal memory allocation
/// - Simple depth-first traversal
/// </summary>
public sealed class NativeTreeSitterMatcher
{
    private readonly NativePatternParser _parser = new();

    /// <summary>
    /// Matches a pattern against a tree-sitter AST using native node types.
    /// </summary>
    /// <param name="root">The root UAST node (with Extensions containing NativeNodeType).</param>
    /// <param name="patternText">The pattern text to match.</param>
    /// <returns>All matching nodes with their captures.</returns>
    public IEnumerable<NativeMatchResult> Match(UastNode root, string patternText)
    {
        var pattern = _parser.Parse(patternText);
        return Match(root, pattern);
    }

    /// <summary>
    /// Matches a compiled pattern against a tree-sitter AST.
    /// </summary>
    /// <param name="root">The root UAST node.</param>
    /// <param name="pattern">The compiled pattern.</param>
    /// <returns>All matching nodes with their captures.</returns>
    public IEnumerable<NativeMatchResult> Match(UastNode root, NativePattern pattern)
    {
        var results = new List<NativeMatchResult>();
        Visit(root, pattern, results);
        return results;
    }

    /// <summary>
    /// Counts matches without collecting full results.
    /// More efficient when only count is needed.
    /// </summary>
    public int CountMatches(UastNode root, NativePattern pattern)
    {
        int count = 0;
        CountVisit(root, pattern, ref count);
        return count;
    }

    /// <summary>
    /// Visits all nodes and collects matches.
    /// </summary>
    private void Visit(UastNode node, NativePattern pattern, List<NativeMatchResult> results)
    {
        var captures = new Dictionary<string, UastNode>();
        if (TryMatch(node, pattern, captures))
        {
            results.Add(new NativeMatchResult(
                node,
                GetNativeNodeType(node) ?? node.NodeKind,
                node.Span,
                captures.Count > 0 ? captures : null
            ));
        }

        // Visit children
        foreach (var child in node.Children)
        {
            Visit(child, pattern, results);
        }
    }

    /// <summary>
    /// Visits all nodes and counts matches without collecting results.
    /// </summary>
    private void CountVisit(UastNode node, NativePattern pattern, ref int count)
    {
        if (TryMatch(node, pattern, null))
        {
            count++;
        }

        foreach (var child in node.Children)
        {
            CountVisit(child, pattern, ref count);
        }
    }

    /// <summary>
    /// Tries to match a single node against a pattern.
    /// </summary>
    private bool TryMatch(UastNode node, NativePattern pattern, Dictionary<string, UastNode>? captures)
    {
        return pattern.PatternType switch
        {
            NativePatternType.NodeType => TryMatchNodeType(node, pattern),
            NativePatternType.SExpression => TryMatchSExpression(node, pattern, captures),
            NativePatternType.MetaVariable => TryMatchMetaVariable(node, pattern, captures),
            NativePatternType.Wildcard => true,
            NativePatternType.Ellipsis => true, // Ellipsis matches anything in this context
            NativePatternType.AtLeastOne => true,
            _ => false
        };
    }

    /// <summary>
    /// Matches by native node type.
    /// </summary>
    private bool TryMatchNodeType(UastNode node, NativePattern pattern)
    {
        var nativeType = GetNativeNodeType(node);
        if (nativeType == null) return false;

        return string.Equals(nativeType, pattern.NodeType, StringComparison.Ordinal);
    }

    /// <summary>
    /// Matches an S-expression pattern with field constraints.
    /// </summary>
    private bool TryMatchSExpression(UastNode node, NativePattern pattern, Dictionary<string, UastNode>? captures)
    {
        // First check the node type
        var nativeType = GetNativeNodeType(node);
        if (nativeType == null || !string.Equals(nativeType, pattern.NodeType, StringComparison.Ordinal))
        {
            return false;
        }

        // Check field constraints
        if (pattern.FieldConstraints != null && pattern.FieldConstraints.Count > 0)
        {
            foreach (var (fieldName, fieldPattern) in pattern.FieldConstraints)
            {
                var fieldChild = GetFieldChild(node, fieldName);
                if (fieldChild == null)
                {
                    return false;
                }

                if (!TryMatch(fieldChild, fieldPattern, captures))
                {
                    return false;
                }
            }
        }

        // Add captures
        if (captures != null && pattern.Captures != null)
        {
            foreach (var (captureName, _) in pattern.Captures)
            {
                captures[captureName] = node;
            }
        }

        return true;
    }

    /// <summary>
    /// Matches a metavariable pattern (captures the node).
    /// </summary>
    private bool TryMatchMetaVariable(UastNode node, NativePattern pattern, Dictionary<string, UastNode>? captures)
    {
        // Metavariables always match
        if (captures != null && !pattern.IsAnonymous && pattern.MetaVarName != null)
        {
            // Check if already captured
            if (captures.TryGetValue(pattern.MetaVarName, out var existing))
            {
                // Must match the same structure
                return NodesEqual(node, existing);
            }
            captures[pattern.MetaVarName] = node;
        }
        return true;
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
        if (node is Schema.Expressions.UnknownExpression unknownExpr)
            return unknownExpr.NativeNodeType;

        // Check Extensions
        if (node.Extensions?.TryGetValue("NativeNodeType", out var nativeType) == true)
            return nativeType as string;

        return null;
    }

    /// <summary>
    /// Gets a child node by field name from the Extensions dictionary.
    /// </summary>
    private static UastNode? GetFieldChild(UastNode node, string fieldName)
    {
        if (node.Extensions == null)
            return null;

        if (!node.Extensions.TryGetValue("NativeFields", out var fieldsObj))
            return null;

        if (fieldsObj is not Dictionary<string, object> fields)
            return null;

        if (!fields.TryGetValue(fieldName, out var fieldInfo))
            return null;

        // Try to find a matching child by the field info
        if (fieldInfo is Dictionary<string, object> fieldDict)
        {
            if (fieldDict.TryGetValue("Type", out var fieldType) && fieldType is string typeStr)
            {
                // Find child with matching native type
                foreach (var child in node.Children)
                {
                    var childType = GetNativeNodeType(child);
                    if (childType == typeStr)
                        return child;
                }
            }
        }

        return null;
    }

    /// <summary>
    /// Checks if two nodes are structurally equal.
    /// </summary>
    private static bool NodesEqual(UastNode a, UastNode b)
    {
        if (ReferenceEquals(a, b))
            return true;

        // Compare raw source if available
        if (a.RawSource != null && b.RawSource != null)
        {
            return a.RawSource == b.RawSource;
        }

        // Compare native type
        var aType = GetNativeNodeType(a);
        var bType = GetNativeNodeType(b);
        if (aType != bType)
            return false;

        // Compare children count
        if (a.Children.Count != b.Children.Count)
            return false;

        // Compare children
        for (int i = 0; i < a.Children.Count; i++)
        {
            if (!NodesEqual(a.Children[i], b.Children[i]))
                return false;
        }

        return true;
    }
}

/// <summary>
/// Represents a match result from native tree-sitter matching.
/// </summary>
/// <param name="Node">The matched UAST node.</param>
/// <param name="NativeType">The native tree-sitter node type.</param>
/// <param name="Span">The source location span.</param>
/// <param name="Captures">Captured metavariable values, if any.</param>
public readonly record struct NativeMatchResult(
    UastNode Node,
    string NativeType,
    SourceSpan Span,
    IReadOnlyDictionary<string, UastNode>? Captures
);
