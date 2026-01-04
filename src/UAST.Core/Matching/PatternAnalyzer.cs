using System.Collections.Frozen;
using UAST.Core.Rules;

namespace UAST.Core.Matching;

/// <summary>
/// Analyzes patterns to extract target NodeKinds for lazy conversion optimization.
/// This enables the parser to skip converting nodes that can never match.
/// </summary>
public static class PatternAnalyzer
{
    private static readonly IReadOnlySet<string> EmptyKinds = new HashSet<string>().ToFrozenSet();

    /// <summary>
    /// Extracts all NodeKind values that a compiled pattern could potentially match.
    /// Used by lazy conversion to filter which nodes need full conversion.
    /// </summary>
    public static IReadOnlySet<string> ExtractTargetKinds(CompiledPattern compiledPattern)
    {
        // Handle simple compiled pattern which wraps a Pattern
        if (compiledPattern is SimpleCompiledPattern simple)
        {
            return ExtractTargetKinds(simple.Pattern);
        }

        // For other pattern types, return empty (means convert all)
        return EmptyKinds;
    }

    /// <summary>
    /// Extracts all NodeKind values that a pattern could potentially match.
    /// Used by lazy conversion to filter which nodes need full conversion.
    /// </summary>
    public static IReadOnlySet<string> ExtractTargetKinds(Pattern pattern)
    {
        var kinds = new HashSet<string>(StringComparer.Ordinal);
        CollectKinds(pattern.Root, kinds);

        // If no specific kinds found, return empty set (means convert all)
        return kinds.Count > 0 ? kinds.ToFrozenSet(StringComparer.Ordinal) : EmptyKinds;
    }

    private static void CollectKinds(PatternNode node, HashSet<string> kinds)
    {
        switch (node)
        {
            case StructuralPattern sp:
                if (!string.IsNullOrEmpty(sp.NodeKind))
                {
                    kinds.Add(sp.NodeKind);
                }
                foreach (var (_, propPattern) in sp.Properties)
                {
                    CollectKinds(propPattern, kinds);
                }
                foreach (var child in sp.Children)
                {
                    CollectKinds(child, kinds);
                }
                break;

            case MetaVariable mv:
                // Add type constraint if present
                if (!string.IsNullOrEmpty(mv.TypeConstraint))
                {
                    kinds.Add(mv.TypeConstraint);
                }
                break;

            case AlternativePattern ap:
                foreach (var alt in ap.Alternatives)
                {
                    CollectKinds(alt, kinds);
                }
                break;

            case ConjunctionPattern cp:
                foreach (var pattern in cp.Patterns)
                {
                    CollectKinds(pattern, kinds);
                }
                break;

            case NegationPattern np:
                CollectKinds(np.Inner, kinds);
                break;

            case SequencePattern seq:
                foreach (var element in seq.Elements)
                {
                    CollectKinds(element, kinds);
                }
                break;

            // LiteralPattern and WildcardPattern don't specify NodeKinds
        }
    }

    /// <summary>
    /// Gets the primary NodeKind from a pattern if it has exactly one.
    /// Returns null if the pattern could match multiple kinds.
    /// </summary>
    public static string? GetPrimaryKind(Pattern pattern)
    {
        if (pattern.Root is StructuralPattern sp && !string.IsNullOrEmpty(sp.NodeKind))
        {
            return sp.NodeKind;
        }

        if (pattern.Root is MetaVariable mv && !string.IsNullOrEmpty(mv.TypeConstraint))
        {
            return mv.TypeConstraint;
        }

        return null;
    }
}
