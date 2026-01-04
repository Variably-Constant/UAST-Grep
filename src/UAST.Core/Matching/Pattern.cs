using UAST.Core.Schema;

namespace UAST.Core.Matching;

/// <summary>
/// Represents a compiled pattern for matching against UAST trees.
/// </summary>
public class Pattern
{
    /// <summary>
    /// The root pattern node.
    /// </summary>
    public required PatternNode Root { get; init; }

    /// <summary>
    /// The source pattern text.
    /// </summary>
    public required string SourceText { get; init; }

    /// <summary>
    /// The language this pattern is for.
    /// </summary>
    public required string Language { get; init; }

    /// <summary>
    /// Metavariable names used in the pattern.
    /// </summary>
    public IReadOnlySet<string> MetaVariables { get; init; } = new HashSet<string>();
}

/// <summary>
/// Base class for pattern nodes.
/// </summary>
public abstract class PatternNode
{
    /// <summary>
    /// The expected node kind (null means any).
    /// </summary>
    public string? NodeKind { get; init; }
}

/// <summary>
/// Specifies the kind of metavariable matching.
/// </summary>
public enum MetaVarKind
{
    /// <summary>Matches exactly one node (∀VAR).</summary>
    Single,

    /// <summary>Matches zero or more nodes (∀∀∀VAR).</summary>
    Multiple,

    /// <summary>Matches one or more nodes (∀∀VAR) - future extension.</summary>
    AtLeastOne
}

/// <summary>
/// A metavariable pattern that matches any node and captures it.
/// Uses the ∀ (ForAll) prefix in pattern syntax.
/// </summary>
public class MetaVariable : PatternNode
{
    /// <summary>
    /// The placeholder prefix used to identify metavariables after preprocessing.
    /// </summary>
    public const string Placeholder = "__UAST_METAVAR_";

    /// <summary>
    /// The metavariable name (e.g., "NAME" from ∀NAME).
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The kind of matching: Single, Multiple (zero or more), or AtLeastOne.
    /// </summary>
    public MetaVarKind Kind { get; init; } = MetaVarKind.Single;

    /// <summary>
    /// Whether this matches multiple nodes (∀∀∀ syntax for zero-or-more).
    /// </summary>
    [Obsolete("Use Kind == MetaVarKind.Multiple instead")]
    public bool IsMultiple
    {
        get => Kind == MetaVarKind.Multiple;
        init => Kind = value ? MetaVarKind.Multiple : MetaVarKind.Single;
    }

    /// <summary>
    /// If true, this is an anonymous metavariable (∀_) - matches but doesn't capture.
    /// </summary>
    public bool IsAnonymous { get; init; }

    /// <summary>
    /// Optional constraint on what this metavariable can match.
    /// </summary>
    public Constraint? Constraint { get; init; }

    /// <summary>
    /// Optional type constraint on the captured node (legacy, use Constraint instead).
    /// </summary>
    public string? TypeConstraint { get; init; }

    public override string ToString()
    {
        var prefix = Kind switch
        {
            MetaVarKind.Multiple => "∀∀∀",
            MetaVarKind.AtLeastOne => "∀∀",
            _ => "∀"
        };
        return IsAnonymous ? $"{prefix}_" : $"{prefix}{Name}";
    }
}

/// <summary>
/// A pattern that matches a specific node structure.
/// </summary>
public class StructuralPattern : PatternNode
{
    /// <summary>
    /// Required properties and their patterns.
    /// </summary>
    public Dictionary<string, PatternNode> Properties { get; init; } = new();

    /// <summary>
    /// Child patterns (for ordered children).
    /// </summary>
    public List<PatternNode> Children { get; init; } = [];
}

/// <summary>
/// A pattern that matches a literal value.
/// </summary>
public class LiteralPattern : PatternNode
{
    /// <summary>
    /// The expected literal value.
    /// </summary>
    public required object? Value { get; init; }
}

/// <summary>
/// A pattern that matches any node (wildcard).
/// </summary>
public class WildcardPattern : PatternNode
{
    /// <summary>
    /// Singleton instance.
    /// </summary>
    public static readonly WildcardPattern Instance = new();

    private WildcardPattern() { }
}

/// <summary>
/// A pattern that matches one of several alternatives (OR).
/// </summary>
public class AlternativePattern : PatternNode
{
    /// <summary>
    /// The alternative patterns.
    /// </summary>
    public required IReadOnlyList<PatternNode> Alternatives { get; init; }
}

/// <summary>
/// A pattern that matches a sequence of nodes.
/// </summary>
public class SequencePattern : PatternNode
{
    /// <summary>
    /// The sequence of patterns.
    /// </summary>
    public required IReadOnlyList<PatternNode> Elements { get; init; }
}

/// <summary>
/// A pattern that requires all sub-patterns to match (AND).
/// </summary>
public class ConjunctionPattern : PatternNode
{
    /// <summary>
    /// All patterns that must match.
    /// </summary>
    public required IReadOnlyList<PatternNode> Patterns { get; init; }
}

/// <summary>
/// A pattern that negates another pattern (NOT).
/// </summary>
public class NegationPattern : PatternNode
{
    /// <summary>
    /// The pattern that must not match.
    /// </summary>
    public required PatternNode Inner { get; init; }
}
