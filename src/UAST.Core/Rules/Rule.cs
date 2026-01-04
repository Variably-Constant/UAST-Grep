using UAST.Core.Interfaces;
using UAST.Core.Matching;
using UAST.Core.Schema;

namespace UAST.Core.Rules;

/// <summary>
/// Represents a compiled rule ready for matching.
/// </summary>
public class Rule
{
    /// <summary>
    /// Unique identifier for the rule.
    /// </summary>
    public required string Id { get; init; }

    /// <summary>
    /// Target language for this rule.
    /// </summary>
    public required string Language { get; init; }

    /// <summary>
    /// Severity level of matches.
    /// </summary>
    public Severity Severity { get; init; } = Severity.Warning;

    /// <summary>
    /// Human-readable message describing the issue.
    /// May contain metavariable interpolations.
    /// </summary>
    public required string Message { get; init; }

    /// <summary>
    /// The compiled pattern for matching.
    /// </summary>
    public required CompiledPattern Pattern { get; init; }

    /// <summary>
    /// Additional constraints on captured metavariables.
    /// </summary>
    public Dictionary<string, Constraint>? Constraints { get; init; }

    /// <summary>
    /// Optional fix template for auto-fix.
    /// </summary>
    public string? Fix { get; init; }

    /// <summary>
    /// Optional URL to documentation.
    /// </summary>
    public string? Url { get; init; }

    /// <summary>
    /// Optional extended notes.
    /// </summary>
    public string? Note { get; init; }

    /// <summary>
    /// Glob patterns for files to include.
    /// </summary>
    public List<string>? Files { get; init; }

    /// <summary>
    /// Glob patterns for files to exclude.
    /// </summary>
    public List<string>? Ignores { get; init; }

    /// <summary>
    /// Tags for categorizing rules.
    /// </summary>
    public List<string>? Tags { get; init; }

    /// <summary>
    /// Whether this rule is enabled.
    /// </summary>
    public bool Enabled { get; init; } = true;

    /// <summary>
    /// Interpolates the message with captured metavariable values.
    /// </summary>
    public string InterpolateMessage(IReadOnlyDictionary<string, object> captures)
    {
        var result = Message;

        foreach (var (name, value) in captures)
        {
            var placeholder = $"∀{name}";
            var replacement = value switch
            {
                UastNode node => node.RawSource ?? "",
                IEnumerable<UastNode> nodes => string.Join(", ", nodes.Select(n => n.RawSource ?? "")),
                _ => value?.ToString() ?? ""
            };
            result = result.Replace(placeholder, replacement);
        }

        return result;
    }
}

/// <summary>
/// Base class for compiled patterns that can match against UAST trees.
/// </summary>
public abstract class CompiledPattern
{
    /// <summary>
    /// Matches this pattern against a UAST tree.
    /// </summary>
    /// <param name="tree">The tree to search.</param>
    /// <param name="matcher">The pattern matcher to use.</param>
    /// <returns>All matches found.</returns>
    public abstract IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher);

    /// <summary>
    /// Tries to match this pattern at a specific node.
    /// </summary>
    public abstract bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures);
}

/// <summary>
/// A simple pattern that matches a single pattern.
/// </summary>
public class SimpleCompiledPattern : CompiledPattern
{
    /// <summary>
    /// The compiled pattern.
    /// </summary>
    public required Pattern Pattern { get; init; }

    /// <summary>
    /// Additional constraints from the rule.
    /// </summary>
    public Dictionary<string, Constraint>? MetaVarConstraints { get; init; }

    /// <summary>
    /// Relational constraint: must be inside an ancestor matching this.
    /// </summary>
    public Constraint? InsideConstraint { get; init; }

    /// <summary>
    /// Relational constraint: must have a descendant matching this.
    /// </summary>
    public Constraint? HasConstraint { get; init; }

    public override IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher)
    {
        var patternMatcher = matcher as PatternMatcher ?? new PatternMatcher();
        var results = new List<RuleMatch>();

        VisitForMatches(tree, patternMatcher, results);
        return results;
    }

    private void VisitForMatches(UastNode node, PatternMatcher matcher, List<RuleMatch> results)
    {
        if (TryMatch(node, matcher, out var captures))
        {
            results.Add(new RuleMatch(node, captures, node.Span));
        }

        foreach (var child in node.Children)
        {
            VisitForMatches(child, matcher, results);
        }
    }

    public override bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures)
    {
        captures = new Dictionary<string, object>();

        if (!matcher.TryMatch(node, Pattern, out var nodeCaptures))
        {
            return false;
        }

        // Convert captures
        foreach (var (key, value) in nodeCaptures)
        {
            captures[key] = value;
        }

        // Check metavariable constraints
        if (MetaVarConstraints != null && matcher is PatternMatcher pm)
        {
            foreach (var (varName, constraint) in MetaVarConstraints)
            {
                if (captures.TryGetValue(varName, out var capturedValue) && capturedValue is UastNode capturedNode)
                {
                    if (!constraint.Evaluate(capturedNode, captures, pm))
                    {
                        return false;
                    }
                }
            }
        }

        // Check relational constraints
        if (InsideConstraint != null && matcher is PatternMatcher pm2)
        {
            if (!InsideConstraint.Evaluate(node, captures, pm2))
            {
                return false;
            }
        }

        if (HasConstraint != null && matcher is PatternMatcher pm3)
        {
            if (!HasConstraint.Evaluate(node, captures, pm3))
            {
                return false;
            }
        }

        return true;
    }
}

/// <summary>
/// Matches if any of the sub-patterns match (OR).
/// </summary>
public class AnyCompiledPattern : CompiledPattern
{
    public required IReadOnlyList<CompiledPattern> Patterns { get; init; }

    public override IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher)
    {
        var seen = new HashSet<(int, int)>();
        var results = new List<RuleMatch>();

        foreach (var pattern in Patterns)
        {
            foreach (var match in pattern.Match(tree, matcher))
            {
                // Deduplicate by position
                var key = (match.Span.StartOffset, match.Span.EndOffset);
                if (seen.Add(key))
                {
                    results.Add(match);
                }
            }
        }

        return results;
    }

    public override bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures)
    {
        foreach (var pattern in Patterns)
        {
            if (pattern.TryMatch(node, matcher, out captures))
            {
                return true;
            }
        }
        captures = new Dictionary<string, object>();
        return false;
    }
}

/// <summary>
/// Matches if all of the sub-patterns match (AND).
/// </summary>
public class AllCompiledPattern : CompiledPattern
{
    public required IReadOnlyList<CompiledPattern> Patterns { get; init; }

    public override IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher)
    {
        if (Patterns.Count == 0)
            yield break;

        // Get matches from first pattern
        var firstMatches = Patterns[0].Match(tree, matcher).ToList();

        foreach (var match in firstMatches)
        {
            // Check that remaining patterns also match at this node
            bool allMatch = true;
            var combinedCaptures = new Dictionary<string, object>(match.Captures);

            for (int i = 1; i < Patterns.Count; i++)
            {
                if (!Patterns[i].TryMatch(match.MatchedNode, matcher, out var additionalCaptures))
                {
                    allMatch = false;
                    break;
                }
                foreach (var (key, value) in additionalCaptures)
                {
                    combinedCaptures[key] = value;
                }
            }

            if (allMatch)
            {
                yield return new RuleMatch(match.MatchedNode, combinedCaptures, match.Span);
            }
        }
    }

    public override bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures)
    {
        captures = new Dictionary<string, object>();

        foreach (var pattern in Patterns)
        {
            if (!pattern.TryMatch(node, matcher, out var patternCaptures))
            {
                return false;
            }
            foreach (var (key, value) in patternCaptures)
            {
                captures[key] = value;
            }
        }

        return true;
    }
}

/// <summary>
/// Matches if the sub-pattern does not match (NOT).
/// </summary>
public class NotCompiledPattern : CompiledPattern
{
    public required CompiledPattern Inner { get; init; }

    public override IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher)
    {
        // NOT pattern doesn't make sense for finding matches
        // It's typically used as part of a larger pattern
        yield break;
    }

    public override bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures)
    {
        captures = new Dictionary<string, object>();
        return !Inner.TryMatch(node, matcher, out _);
    }
}

/// <summary>
/// Matches by node kind.
/// </summary>
public class KindCompiledPattern : CompiledPattern
{
    public required string Kind { get; init; }

    public override IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher)
    {
        return tree.DescendantsAndSelf()
            .Where(n => string.Equals(n.NodeKind, Kind, StringComparison.OrdinalIgnoreCase))
            .Select(n => new RuleMatch(n, new Dictionary<string, object>(), n.Span));
    }

    public override bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures)
    {
        captures = new Dictionary<string, object>();
        return string.Equals(node.NodeKind, Kind, StringComparison.OrdinalIgnoreCase);
    }
}

/// <summary>
/// Matches by regex on source text.
/// </summary>
public class RegexCompiledPattern : CompiledPattern
{
    public required System.Text.RegularExpressions.Regex Regex { get; init; }

    public override IEnumerable<RuleMatch> Match(UastNode tree, IPatternMatcher matcher)
    {
        return tree.DescendantsAndSelf()
            .Where(n => n.RawSource != null && Regex.IsMatch(n.RawSource))
            .Select(n => new RuleMatch(n, new Dictionary<string, object>(), n.Span));
    }

    public override bool TryMatch(UastNode node, IPatternMatcher matcher, out Dictionary<string, object> captures)
    {
        captures = new Dictionary<string, object>();
        return node.RawSource != null && Regex.IsMatch(node.RawSource);
    }
}

/// <summary>
/// Represents a match from a rule.
/// </summary>
public record RuleMatch(
    /// <summary>The matched node.</summary>
    UastNode MatchedNode,

    /// <summary>Captured metavariables.</summary>
    IReadOnlyDictionary<string, object> Captures,

    /// <summary>Source span of the match.</summary>
    SourceSpan Span
);
