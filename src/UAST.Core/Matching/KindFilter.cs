using UAST.Core.Schema;

namespace UAST.Core.Matching;

/// <summary>
/// Provides kind-based pre-filtering to optimize pattern matching.
/// Inspired by ast-grep's optimization strategy: quickly reject nodes that cannot possibly match
/// based on their NodeKind before attempting expensive structural matching.
/// </summary>
public class KindFilter
{
    private readonly HashSet<string> _potentialKinds;
    private readonly bool _acceptsAnyKind;

    /// <summary>
    /// Creates a kind filter from a pattern.
    /// </summary>
    /// <param name="pattern">The pattern to analyze.</param>
    public KindFilter(Pattern pattern)
    {
        _potentialKinds = ExtractPotentialKinds(pattern);
        _acceptsAnyKind = _potentialKinds.Count == 0;
    }

    /// <summary>
    /// Creates a kind filter from explicit kinds.
    /// </summary>
    /// <param name="kinds">The kinds that could match.</param>
    public KindFilter(IEnumerable<string> kinds)
    {
        _potentialKinds = new HashSet<string>(kinds, StringComparer.OrdinalIgnoreCase);
        _acceptsAnyKind = _potentialKinds.Count == 0;
    }

    /// <summary>
    /// The potential node kinds that could match.
    /// Empty set means any kind could match (pattern uses only metavariables).
    /// </summary>
    public IReadOnlySet<string> PotentialKinds => _potentialKinds;

    /// <summary>
    /// Whether this filter accepts any node kind.
    /// </summary>
    public bool AcceptsAnyKind => _acceptsAnyKind;

    /// <summary>
    /// Quick rejection test: returns false if the node definitely cannot match.
    /// Returns true if the node could potentially match (requires full structural check).
    /// </summary>
    /// <param name="node">The node to test.</param>
    /// <returns>True if the node could match, false if it definitely cannot.</returns>
    public bool CouldMatch(UastNode node)
    {
        // If no specific kinds required, any node could match
        if (_acceptsAnyKind)
            return true;

        // Quick rejection based on node kind
        return _potentialKinds.Contains(node.NodeKind);
    }

    /// <summary>
    /// Filters an enumerable to only nodes that could potentially match.
    /// </summary>
    /// <param name="nodes">The nodes to filter.</param>
    /// <returns>Nodes that pass the pre-filter.</returns>
    public IEnumerable<UastNode> Filter(IEnumerable<UastNode> nodes)
    {
        if (_acceptsAnyKind)
            return nodes;

        return nodes.Where(CouldMatch);
    }

    /// <summary>
    /// Filters descendants of a root node that could potentially match.
    /// </summary>
    /// <param name="root">The root node.</param>
    /// <returns>Filtered descendants.</returns>
    public IEnumerable<UastNode> FilterDescendants(UastNode root)
    {
        return Filter(root.DescendantsAndSelf());
    }

    private static HashSet<string> ExtractPotentialKinds(Pattern pattern)
    {
        var kinds = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
        CollectKinds(pattern.Root, kinds);
        return kinds;
    }

    private static void CollectKinds(PatternNode? node, HashSet<string> kinds)
    {
        if (node == null)
            return;

        switch (node)
        {
            case StructuralPattern sp:
                // Structural patterns specify an explicit node kind
                if (!string.IsNullOrEmpty(sp.NodeKind))
                {
                    kinds.Add(sp.NodeKind);
                }
                // Also check children
                foreach (var child in sp.Children)
                {
                    CollectKinds(child, kinds);
                }
                break;

            case LiteralPattern:
                // Literal patterns match specific values - typically literals
                kinds.Add("LiteralExpression");
                break;

            case MetaVariable:
                // Metavariables can match anything - don't add any kinds
                // This is intentional: if pattern is just a metavariable,
                // we can't do kind-based filtering
                break;

            case AlternativePattern alt:
                // For alternatives, collect from all options
                foreach (var option in alt.Alternatives)
                {
                    CollectKinds(option, kinds);
                }
                break;

            case SequencePattern seq:
                // For sequences, collect from all elements
                foreach (var element in seq.Elements)
                {
                    CollectKinds(element, kinds);
                }
                break;
        }
    }
}

/// <summary>
/// Extension methods for kind-based filtering.
/// </summary>
public static class KindFilterExtensions
{
    /// <summary>
    /// Creates a kind filter from a pattern and applies it to find candidate nodes.
    /// </summary>
    /// <param name="root">The root node to search.</param>
    /// <param name="pattern">The pattern to match.</param>
    /// <returns>Nodes that could potentially match the pattern.</returns>
    public static IEnumerable<UastNode> GetCandidates(this UastNode root, Pattern pattern)
    {
        var filter = new KindFilter(pattern);
        return filter.FilterDescendants(root);
    }

    /// <summary>
    /// Finds all nodes of a specific kind.
    /// </summary>
    /// <param name="root">The root node to search.</param>
    /// <param name="kind">The node kind to find.</param>
    /// <returns>All matching nodes.</returns>
    public static IEnumerable<UastNode> FindByKind(this UastNode root, string kind)
    {
        return root.DescendantsAndSelf().Where(n =>
            n.NodeKind.Equals(kind, StringComparison.OrdinalIgnoreCase));
    }

    /// <summary>
    /// Finds all nodes of any of the specified kinds.
    /// </summary>
    /// <param name="root">The root node to search.</param>
    /// <param name="kinds">The node kinds to find.</param>
    /// <returns>All matching nodes.</returns>
    public static IEnumerable<UastNode> FindByKinds(this UastNode root, params string[] kinds)
    {
        var kindSet = new HashSet<string>(kinds, StringComparer.OrdinalIgnoreCase);
        return root.DescendantsAndSelf().Where(n => kindSet.Contains(n.NodeKind));
    }
}
