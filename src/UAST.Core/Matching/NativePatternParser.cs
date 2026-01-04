using System.Text;
using System.Text.RegularExpressions;

namespace UAST.Core.Matching;

/// <summary>
/// Parses pattern strings into native tree-sitter query structures.
/// Supports:
/// - Node type matching: "function_definition" → matches nodes with that type
/// - Metavariables: "$NAME" captures a node, "∀NAME" also captures
/// - Ellipsis: "$$$" or "∀∀∀" matches zero or more nodes
/// - S-expression patterns: "(function_definition name: (identifier))"
/// </summary>
public class NativePatternParser
{
    /// <summary>
    /// Metavariable prefix (Unicode ForAll symbol).
    /// </summary>
    public const char MetaVarPrefix = '\u2200';

    /// <summary>
    /// Alternative ASCII metavariable prefix.
    /// </summary>
    public const char MetaVarPrefixAlt = '$';

    /// <summary>
    /// Parses a pattern string into a NativePattern.
    /// </summary>
    /// <param name="patternSource">The pattern source text.</param>
    /// <returns>A compiled NativePattern object.</returns>
    public NativePattern Parse(string patternSource)
    {
        patternSource = patternSource.Trim();

        // Check if it's an S-expression pattern
        if (patternSource.StartsWith('('))
        {
            return ParseSExpression(patternSource);
        }

        // Check for metavariable pattern
        if (patternSource.Length > 0 && (patternSource[0] == MetaVarPrefix || patternSource[0] == MetaVarPrefixAlt))
        {
            return ParseMetaVariable(patternSource);
        }

        // Check for wildcard
        if (patternSource == "*" || patternSource == "..." || patternSource == "$$$" || patternSource == "∀∀∀")
        {
            return new NativePattern
            {
                PatternType = NativePatternType.Wildcard,
                SourceText = patternSource
            };
        }

        // Otherwise treat as node type pattern
        return new NativePattern
        {
            PatternType = NativePatternType.NodeType,
            NodeType = patternSource,
            SourceText = patternSource
        };
    }

    /// <summary>
    /// Parses a metavariable pattern (e.g., $NAME, ∀NAME, $$$NODES).
    /// </summary>
    private NativePattern ParseMetaVariable(string source)
    {
        char prefixChar = source[0];
        int prefixCount = 0;
        int i = 0;

        // Count prefix characters
        while (i < source.Length && source[i] == prefixChar)
        {
            prefixCount++;
            i++;
        }

        // Get the name
        var name = source[i..];
        bool isAnonymous = name == "_" || string.IsNullOrEmpty(name);
        bool isMultiple = prefixCount >= 3;
        bool isAtLeastOne = prefixCount == 2;

        return new NativePattern
        {
            PatternType = isMultiple ? NativePatternType.Ellipsis :
                         isAtLeastOne ? NativePatternType.AtLeastOne :
                         NativePatternType.MetaVariable,
            MetaVarName = isAnonymous ? null : name,
            IsAnonymous = isAnonymous,
            SourceText = source
        };
    }

    /// <summary>
    /// Parses an S-expression pattern like "(function_definition name: (identifier))".
    /// </summary>
    private NativePattern ParseSExpression(string source)
    {
        // Extract the main node type
        var nodeTypeMatch = Regex.Match(source, @"^\((\w+)");
        if (!nodeTypeMatch.Success)
        {
            return new NativePattern
            {
                PatternType = NativePatternType.Invalid,
                SourceText = source
            };
        }

        var nodeType = nodeTypeMatch.Groups[1].Value;
        var fieldConstraints = new Dictionary<string, NativePattern>();
        var captures = new Dictionary<string, string>();

        // Extract field constraints like "name: (identifier)"
        var fieldMatches = Regex.Matches(source, @"(\w+):\s*\((\w+)\)");
        foreach (Match match in fieldMatches)
        {
            var fieldName = match.Groups[1].Value;
            var fieldType = match.Groups[2].Value;
            fieldConstraints[fieldName] = new NativePattern
            {
                PatternType = NativePatternType.NodeType,
                NodeType = fieldType,
                SourceText = $"({fieldType})"
            };
        }

        // Extract captures like "@name" or "@name.identifier"
        var captureMatches = Regex.Matches(source, @"@(\w+(?:\.\w+)?)");
        foreach (Match match in captureMatches)
        {
            var captureName = match.Groups[1].Value;
            captures[captureName] = captureName;
        }

        return new NativePattern
        {
            PatternType = NativePatternType.SExpression,
            NodeType = nodeType,
            FieldConstraints = fieldConstraints,
            Captures = captures,
            SourceText = source
        };
    }

    /// <summary>
    /// Converts a pattern to a tree-sitter S-expression query string.
    /// This is useful for debugging and for languages that support native queries.
    /// </summary>
    public string ToTreeSitterQuery(NativePattern pattern)
    {
        return pattern.PatternType switch
        {
            NativePatternType.NodeType => $"({pattern.NodeType})",
            NativePatternType.SExpression => pattern.SourceText,
            NativePatternType.Wildcard => "(_)",
            NativePatternType.MetaVariable => $"({pattern.MetaVarName ?? "_"}) @{pattern.MetaVarName ?? "capture"}",
            NativePatternType.Ellipsis => "(_)*",
            _ => "(ERROR)"
        };
    }
}

/// <summary>
/// Represents a compiled native pattern for tree-sitter matching.
/// </summary>
public class NativePattern
{
    /// <summary>
    /// The type of pattern.
    /// </summary>
    public required NativePatternType PatternType { get; init; }

    /// <summary>
    /// The node type to match (for NodeType and SExpression patterns).
    /// </summary>
    public string? NodeType { get; init; }

    /// <summary>
    /// Metavariable name (for MetaVariable patterns).
    /// </summary>
    public string? MetaVarName { get; init; }

    /// <summary>
    /// Whether this is an anonymous metavariable.
    /// </summary>
    public bool IsAnonymous { get; init; }

    /// <summary>
    /// Field constraints for S-expression patterns.
    /// Maps field name to expected pattern.
    /// </summary>
    public IReadOnlyDictionary<string, NativePattern>? FieldConstraints { get; init; }

    /// <summary>
    /// Named captures from the pattern.
    /// </summary>
    public IReadOnlyDictionary<string, string>? Captures { get; init; }

    /// <summary>
    /// The original source text.
    /// </summary>
    public required string SourceText { get; init; }

    /// <summary>
    /// Child patterns (for complex S-expressions).
    /// </summary>
    public IReadOnlyList<NativePattern>? Children { get; init; }
}

/// <summary>
/// The type of native pattern.
/// </summary>
public enum NativePatternType
{
    /// <summary>
    /// Invalid pattern.
    /// </summary>
    Invalid,

    /// <summary>
    /// Matches a specific node type (e.g., "function_definition").
    /// </summary>
    NodeType,

    /// <summary>
    /// Captures a single node ($NAME or ∀NAME).
    /// </summary>
    MetaVariable,

    /// <summary>
    /// Matches zero or more nodes ($$$, ∀∀∀).
    /// </summary>
    Ellipsis,

    /// <summary>
    /// Matches at least one node ($$, ∀∀).
    /// </summary>
    AtLeastOne,

    /// <summary>
    /// Matches any single node.
    /// </summary>
    Wildcard,

    /// <summary>
    /// Full S-expression pattern with field constraints.
    /// </summary>
    SExpression
}
