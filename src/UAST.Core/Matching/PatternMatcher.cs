using System.Buffers;
using System.Collections.Concurrent;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using Microsoft.Extensions.ObjectPool;
using UAST.Core.Interfaces;
using UAST.Core.Schema;
using Unsafe = System.Runtime.CompilerServices.Unsafe;

namespace UAST.Core.Matching;

/// <summary>
/// High-performance implementation of pattern matching against UAST trees.
/// Uses depth-first traversal to find all matches.
/// Implements IConstraintMatcher for constraint evaluation.
///
/// Performance optimizations:
/// - Index-based child matching (no Skip().ToList() allocations)
/// - ObjectPool for temporary dictionaries (reduced GC pressure)
/// - Property accessor caching (no repeated reflection)
/// - Node equality caching
/// - Span-based string comparisons (SIMD-accelerated via SequenceEqual)
/// - CollectionsMarshal for single-lookup dictionary operations
/// - SkipLocalsInit for reduced stack initialization
/// - Aggressive inlining for hot paths
/// </summary>
[SkipLocalsInit] // Skip zeroing of local variables for faster method entry
public sealed class PatternMatcher : IPatternMatcher, IConstraintMatcher
{
    // Object pool for temporary dictionaries - reduces GC pressure significantly
    private static readonly ObjectPool<Dictionary<string, object>> _dictPool =
        new DefaultObjectPoolProvider().Create(new DictionaryPoolPolicy());

    // Cache for node equality comparisons using identity-based keys
    private readonly ConcurrentDictionary<(int, int), bool> _equalityCache = new();

    // Cache for property accessors - avoids repeated reflection
    private static readonly ConcurrentDictionary<(Type, string), PropertyInfo?> _propertyCache = new();

    /// <inheritdoc/>
    public IEnumerable<MatchResult> Match(UastNode tree, Pattern pattern)
    {
        var results = new List<MatchResult>();
        Visit(tree, pattern, results);
        return results;
    }

    /// <inheritdoc/>
    public bool TryMatch(UastNode node, Pattern pattern, out IReadOnlyDictionary<string, UastNode> captures)
    {
        var env = _dictPool.Get();
        try
        {
            if (TryMatchNode(node, pattern.Root, env))
            {
                captures = ConvertToNodeDictionary(env);
                return true;
            }
            captures = new Dictionary<string, UastNode>();
            return false;
        }
        finally
        {
            _dictPool.Return(env);
        }
    }

    /// <summary>
    /// Converts the mixed-type environment to a UastNode dictionary.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static IReadOnlyDictionary<string, UastNode> ConvertToNodeDictionary(Dictionary<string, object> env)
    {
        var result = new Dictionary<string, UastNode>(env.Count);
        foreach (var (key, value) in env)
        {
            switch (value)
            {
                case UastNode node:
                    result[key] = node;
                    break;
                case List<UastNode> nodes when nodes.Count > 0:
                    result[key] = nodes[0];
                    break;
            }
        }
        return result;
    }

    /// <summary>
    /// Visits all nodes in the tree, collecting matches.
    /// </summary>
    private void Visit(UastNode node, Pattern pattern, List<MatchResult> results)
    {
        var env = _dictPool.Get();
        try
        {
            if (TryMatchNode(node, pattern.Root, env))
            {
                results.Add(new MatchResult(node, ConvertToNodeDictionary(env), node.Span));
            }
        }
        finally
        {
            _dictPool.Return(env);
        }

        // Continue to children - use indexed loop to avoid enumerator allocation
        var children = node.Children;
        for (int i = 0; i < children.Count; i++)
        {
            Visit(children[i], pattern, results);
        }
    }

    /// <summary>
    /// Tries to match a node against a pattern.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool TryMatchNode(UastNode node, PatternNode pattern, Dictionary<string, object> env)
    {
        return pattern switch
        {
            MetaVariable mv => TryMatchMetaVar(node, mv, env),
            StructuralPattern sp => TryMatchStructural(node, sp, env),
            LiteralPattern lp => TryMatchLiteral(node, lp),
            WildcardPattern => true,
            AlternativePattern ap => TryMatchAlternative(node, ap, env),
            SequencePattern seq => TryMatchSequence(node, seq, env),
            ConjunctionPattern conj => TryMatchConjunction(node, conj, env),
            NegationPattern neg => TryMatchNegation(node, neg, env),
            _ => false
        };
    }

    /// <summary>
    /// Matches a metavariable pattern.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool TryMatchMetaVar(UastNode node, MetaVariable mv, Dictionary<string, object> env)
    {
        // Check type constraint if present (legacy) - use Span comparison
        if (mv.TypeConstraint != null && !NodeKindEquals(node.NodeKind, mv.TypeConstraint))
        {
            return false;
        }

        // Check constraint if present
        if (mv.Constraint != null && !mv.Constraint.Evaluate(node, env, this))
        {
            return false;
        }

        // Anonymous metavariables match but don't capture
        if (mv.IsAnonymous)
        {
            return true;
        }

        // Check for existing capture with same name - use single-lookup pattern
        ref var existingRef = ref CollectionsMarshal.GetValueRefOrNullRef(env, mv.Name);
        if (!Unsafe.IsNullRef(ref existingRef))
        {
            // Already captured - must match same structure
            if (existingRef is UastNode existingNode)
            {
                return NodesEqualCached(node, existingNode);
            }
            return false;
        }

        // Capture the node
        env[mv.Name] = node;
        return true;
    }

    /// <summary>
    /// Span-based case-insensitive string comparison for NodeKind.
    /// Avoids string allocations from ToLower/ToUpper.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool NodeKindEquals(string? a, string? b)
    {
        if (a is null || b is null)
            return a == b;

        return a.AsSpan().Equals(b.AsSpan(), StringComparison.OrdinalIgnoreCase);
    }

    /// <summary>
    /// Matches a structural pattern.
    /// </summary>
    private bool TryMatchStructural(UastNode node, StructuralPattern sp, Dictionary<string, object> env)
    {
        // Check node kind matches - use Span comparison
        if (sp.NodeKind != null && !NodeKindEquals(sp.NodeKind, node.NodeKind))
        {
            return false;
        }

        // Check each property pattern
        foreach (var (propName, propPattern) in sp.Properties)
        {
            var propValue = GetPropertyValueCached(node, propName);
            if (propValue == null)
            {
                // Property doesn't exist - only OK if pattern is optional (multi-match)
                if (propPattern is not MetaVariable { Kind: MetaVarKind.Multiple })
                {
                    return false;
                }
            }
            else if (propValue is UastNode propNode)
            {
                if (!TryMatchNode(propNode, propPattern, env))
                {
                    return false;
                }
            }
            else if (!TryMatchValue(propValue, propPattern))
            {
                return false;
            }
        }

        // Check children if specified - use index-based matching
        if (sp.Children.Count > 0)
        {
            if (!TryMatchChildrenIndexed(node.Children, 0, sp.Children, 0, env))
            {
                return false;
            }
        }

        return true;
    }

    /// <summary>
    /// Matches children against pattern children with ellipsis (∀∀∀) support.
    /// Uses index-based approach to avoid allocations from Skip().ToList().
    /// </summary>
    private bool TryMatchChildrenIndexed(
        IReadOnlyList<UastNode> nodes,
        int nodeStartIdx,
        IReadOnlyList<PatternNode> patterns,
        int patternStartIdx,
        Dictionary<string, object> env)
    {
        int nodeIdx = nodeStartIdx;
        int patternIdx = patternStartIdx;
        int nodeCount = nodes.Count;
        int patternCount = patterns.Count;

        while (patternIdx < patternCount)
        {
            var pattern = patterns[patternIdx];

            if (pattern is MetaVariable mv && mv.Kind == MetaVarKind.Multiple)
            {
                // Ellipsis matching: greedy capture of zero or more nodes
                var capturedNodes = new List<UastNode>();

                if (patternIdx + 1 >= patternCount)
                {
                    // Ellipsis at end - capture all remaining nodes
                    while (nodeIdx < nodeCount)
                    {
                        capturedNodes.Add(nodes[nodeIdx++]);
                    }
                }
                else
                {
                    // Find first position where remaining patterns match
                    int matchPoint = FindMatchPoint(nodes, nodeIdx, patterns, patternIdx + 1, env);

                    if (matchPoint < 0)
                    {
                        // No match found - capture all remaining and fail at next pattern
                        while (nodeIdx < nodeCount)
                        {
                            capturedNodes.Add(nodes[nodeIdx++]);
                        }
                    }
                    else
                    {
                        // Capture nodes up to match point
                        while (nodeIdx < matchPoint)
                        {
                            capturedNodes.Add(nodes[nodeIdx++]);
                        }
                    }
                }

                // Store captured nodes (unless anonymous)
                if (!mv.IsAnonymous)
                {
                    env[mv.Name] = capturedNodes;
                }

                patternIdx++;
            }
            else if (pattern is MetaVariable mvOne && mvOne.Kind == MetaVarKind.AtLeastOne)
            {
                // At least one matching: must capture at least one node
                if (nodeIdx >= nodeCount)
                {
                    return false;
                }

                var capturedNodes = new List<UastNode> { nodes[nodeIdx++] };

                if (patternIdx + 1 >= patternCount)
                {
                    // Capture all remaining
                    while (nodeIdx < nodeCount)
                    {
                        capturedNodes.Add(nodes[nodeIdx++]);
                    }
                }
                else
                {
                    // Find first position where remaining patterns match
                    int matchPoint = FindMatchPoint(nodes, nodeIdx, patterns, patternIdx + 1, env);

                    if (matchPoint >= 0)
                    {
                        while (nodeIdx < matchPoint)
                        {
                            capturedNodes.Add(nodes[nodeIdx++]);
                        }
                    }
                    else
                    {
                        while (nodeIdx < nodeCount)
                        {
                            capturedNodes.Add(nodes[nodeIdx++]);
                        }
                    }
                }

                if (!mvOne.IsAnonymous)
                {
                    env[mvOne.Name] = capturedNodes;
                }

                patternIdx++;
            }
            else
            {
                // Single node pattern - must match exactly one node
                if (nodeIdx >= nodeCount)
                {
                    return false;
                }

                if (!TryMatchNode(nodes[nodeIdx], pattern, env))
                {
                    return false;
                }

                nodeIdx++;
                patternIdx++;
            }
        }

        // All patterns matched; allow trailing nodes only if last pattern was ellipsis
        return nodeIdx == nodeCount ||
               (patternCount > 0 && patterns[patternCount - 1] is MetaVariable { Kind: MetaVarKind.Multiple or MetaVarKind.AtLeastOne });
    }

    /// <summary>
    /// Finds the first position in nodes (starting from nodeStartIdx) where
    /// the remaining patterns (starting from patternStartIdx) match.
    /// Returns -1 if no match point found.
    /// Uses pooled dictionaries for speculative matching.
    /// </summary>
    private int FindMatchPoint(
        IReadOnlyList<UastNode> nodes,
        int nodeStartIdx,
        IReadOnlyList<PatternNode> patterns,
        int patternStartIdx,
        Dictionary<string, object> env)
    {
        int nodeCount = nodes.Count;

        for (int tryIdx = nodeStartIdx; tryIdx <= nodeCount; tryIdx++)
        {
            // Get pooled dictionary for speculative match
            var testEnv = _dictPool.Get();
            try
            {
                // Copy existing env into pooled dict
                foreach (var kvp in env)
                {
                    testEnv[kvp.Key] = kvp.Value;
                }

                if (TryMatchChildrenIndexed(nodes, tryIdx, patterns, patternStartIdx, testEnv))
                {
                    // Found a valid match point - merge successful captures back
                    foreach (var (key, value) in testEnv)
                    {
                        if (!env.ContainsKey(key))
                        {
                            env[key] = value;
                        }
                    }
                    return tryIdx;
                }
            }
            finally
            {
                _dictPool.Return(testEnv);
            }
        }

        return -1;
    }

    /// <summary>
    /// Matches a literal pattern.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TryMatchLiteral(UastNode node, LiteralPattern lp)
    {
        if (node is Schema.Expressions.LiteralExpression lit)
        {
            return Equals(lit.Value, lp.Value);
        }
        return false;
    }

    /// <summary>
    /// Matches a value against a pattern.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TryMatchValue(object value, PatternNode pattern)
    {
        return pattern switch
        {
            LiteralPattern lp => Equals(value, lp.Value),
            WildcardPattern => true,
            MetaVariable => true, // Metavar in value position always matches
            _ => false
        };
    }

    /// <summary>
    /// Matches an alternative pattern (any of the alternatives).
    /// Uses pooled dictionary for speculative matching.
    /// </summary>
    private bool TryMatchAlternative(UastNode node, AlternativePattern ap, Dictionary<string, object> env)
    {
        var alternatives = ap.Alternatives;
        for (int i = 0; i < alternatives.Count; i++)
        {
            var testEnv = _dictPool.Get();
            try
            {
                foreach (var kvp in env)
                {
                    testEnv[kvp.Key] = kvp.Value;
                }

                if (TryMatchNode(node, alternatives[i], testEnv))
                {
                    // Merge captures from successful alternative
                    foreach (var (key, value) in testEnv)
                    {
                        env[key] = value;
                    }
                    return true;
                }
            }
            finally
            {
                _dictPool.Return(testEnv);
            }
        }
        return false;
    }

    /// <summary>
    /// Matches a conjunction pattern (all must match).
    /// </summary>
    private bool TryMatchConjunction(UastNode node, ConjunctionPattern conj, Dictionary<string, object> env)
    {
        var patterns = conj.Patterns;
        for (int i = 0; i < patterns.Count; i++)
        {
            if (!TryMatchNode(node, patterns[i], env))
            {
                return false;
            }
        }
        return true;
    }

    /// <summary>
    /// Matches a negation pattern (must not match).
    /// Uses pooled dictionary for speculative matching.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool TryMatchNegation(UastNode node, NegationPattern neg, Dictionary<string, object> env)
    {
        var testEnv = _dictPool.Get();
        try
        {
            foreach (var kvp in env)
            {
                testEnv[kvp.Key] = kvp.Value;
            }
            return !TryMatchNode(node, neg.Inner, testEnv);
        }
        finally
        {
            _dictPool.Return(testEnv);
        }
    }

    /// <summary>
    /// Matches a sequence pattern.
    /// </summary>
    private bool TryMatchSequence(UastNode node, SequencePattern seq, Dictionary<string, object> env)
    {
        var children = node.Children;
        var elements = seq.Elements;

        if (elements.Count > children.Count)
        {
            return false;
        }

        for (int i = 0; i < elements.Count; i++)
        {
            if (!TryMatchNode(children[i], elements[i], env))
            {
                return false;
            }
        }
        return true;
    }

    /// <summary>
    /// Gets a property value from a node by name with caching.
    /// Caches PropertyInfo lookups to avoid repeated reflection.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static object? GetPropertyValueCached(UastNode node, string propName)
    {
        var type = node.GetType();
        var key = (type, propName);

        var prop = _propertyCache.GetOrAdd(key, k => k.Item1.GetProperty(k.Item2));
        return prop?.GetValue(node);
    }

    /// <summary>
    /// Checks if two nodes are structurally equal with caching.
    /// </summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private bool NodesEqualCached(UastNode a, UastNode b)
    {
        // Reference equality fast path
        if (ReferenceEquals(a, b))
            return true;

        // Use identity hash codes for cache key
        var key = (RuntimeHelpers.GetHashCode(a), RuntimeHelpers.GetHashCode(b));

        // Check cache first
        if (_equalityCache.TryGetValue(key, out var cached))
        {
            return cached;
        }

        // Compute and cache
        bool result = NodesEqual(a, b);
        _equalityCache.TryAdd(key, result);
        return result;
    }

    /// <summary>
    /// Checks if two nodes are structurally equal.
    /// Uses Span-based comparison for raw source.
    /// </summary>
    private static bool NodesEqual(UastNode a, UastNode b)
    {
        // Use Span comparison for NodeKind
        if (!NodeKindEquals(a.NodeKind, b.NodeKind))
            return false;

        // Compare raw source if available (fast path) - use Span
        if (a.RawSource != null && b.RawSource != null)
        {
            return a.RawSource.AsSpan().SequenceEqual(b.RawSource.AsSpan());
        }

        // Compare children
        var aChildren = a.Children;
        var bChildren = b.Children;

        if (aChildren.Count != bChildren.Count)
            return false;

        for (int i = 0; i < aChildren.Count; i++)
        {
            if (!NodesEqual(aChildren[i], bChildren[i]))
                return false;
        }

        return true;
    }

    #region IConstraintMatcher Implementation

    /// <inheritdoc/>
    bool IConstraintMatcher.TryMatch(UastNode node, PatternNode pattern, Dictionary<string, object> env)
    {
        return TryMatchNode(node, pattern, env);
    }

    /// <inheritdoc/>
    string IConstraintMatcher.GetSourceText(UastNode node)
    {
        return node.RawSource ?? string.Empty;
    }

    /// <inheritdoc/>
    bool IConstraintMatcher.HasMatchingDescendant(UastNode node, PatternNode pattern, Dictionary<string, object> env, StopBehavior stopBy)
    {
        return HasMatchingDescendantInternal(node, pattern, env, stopBy);
    }

    private bool HasMatchingDescendantInternal(UastNode node, PatternNode pattern, Dictionary<string, object> env, StopBehavior stopBy)
    {
        var children = node.Children;
        for (int i = 0; i < children.Count; i++)
        {
            var child = children[i];
            var tempEnv = _dictPool.Get();
            try
            {
                foreach (var kvp in env)
                {
                    tempEnv[kvp.Key] = kvp.Value;
                }

                if (TryMatchNode(child, pattern, tempEnv))
                {
                    return true;
                }
            }
            finally
            {
                _dictPool.Return(tempEnv);
            }

            // If StopBy is Neighbor, only check immediate children
            if (stopBy == StopBehavior.Neighbor)
            {
                continue;
            }

            // Recursively check descendants
            if (HasMatchingDescendantInternal(child, pattern, env, stopBy))
            {
                return true;
            }
        }
        return false;
    }

    #endregion
}

/// <summary>
/// Pool policy for Dictionary&lt;string, object&gt; that clears on return.
/// </summary>
internal sealed class DictionaryPoolPolicy : PooledObjectPolicy<Dictionary<string, object>>
{
    public override Dictionary<string, object> Create() => new(8); // Pre-sized for typical captures

    public override bool Return(Dictionary<string, object> obj)
    {
        obj.Clear();
        return true;
    }
}
