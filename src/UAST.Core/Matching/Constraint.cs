using System.Text.RegularExpressions;
using UAST.Core.Schema;

namespace UAST.Core.Matching;

/// <summary>
/// Base class for all constraints that can be applied to pattern matches.
/// Constraints filter or validate matches based on various criteria.
/// </summary>
public abstract class Constraint
{
    /// <summary>
    /// Evaluates this constraint against a node.
    /// </summary>
    /// <param name="node">The node to evaluate.</param>
    /// <param name="env">The current match environment with captured variables.</param>
    /// <param name="matcher">The pattern matcher for recursive matching.</param>
    /// <returns>True if the constraint is satisfied.</returns>
    public abstract bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher);
}

/// <summary>
/// Interface for constraint evaluation that requires pattern matching.
/// </summary>
public interface IConstraintMatcher
{
    /// <summary>
    /// Tries to match a node against a pattern.
    /// </summary>
    bool TryMatch(UastNode node, PatternNode pattern, Dictionary<string, object> env);

    /// <summary>
    /// Gets the raw source text for a node.
    /// </summary>
    string GetSourceText(UastNode node);

    /// <summary>
    /// Checks if a node has a descendant matching a pattern.
    /// </summary>
    bool HasMatchingDescendant(UastNode node, PatternNode pattern, Dictionary<string, object> env, StopBehavior stopBy);
}

/// <summary>
/// Specifies how far to traverse when checking relational constraints.
/// </summary>
public enum StopBehavior
{
    /// <summary>Only check immediate parent/children.</summary>
    Neighbor,

    /// <summary>Check all ancestors/descendants until end.</summary>
    End
}

/// <summary>
/// Matches node's source text against a regular expression.
/// </summary>
public class RegexConstraint : Constraint
{
    /// <summary>
    /// The regex pattern to match against the source text.
    /// </summary>
    public required Regex Pattern { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        var sourceText = matcher.GetSourceText(node);
        return Pattern.IsMatch(sourceText);
    }

    /// <summary>
    /// Creates a RegexConstraint from a pattern string.
    /// </summary>
    public static RegexConstraint FromPattern(string pattern, RegexOptions options = RegexOptions.None)
    {
        return new RegexConstraint { Pattern = new Regex(pattern, options | RegexOptions.Compiled) };
    }
}

/// <summary>
/// Matches nodes by their node kind.
/// </summary>
public class KindConstraint : Constraint
{
    /// <summary>
    /// The required node kind.
    /// </summary>
    public required string Kind { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        return string.Equals(node.NodeKind, Kind, StringComparison.OrdinalIgnoreCase);
    }
}

/// <summary>
/// Negates another constraint.
/// </summary>
public class NotConstraint : Constraint
{
    /// <summary>
    /// The constraint to negate.
    /// </summary>
    public required Constraint Inner { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        return !Inner.Evaluate(node, env, matcher);
    }
}

/// <summary>
/// Requires all inner constraints to pass (AND).
/// </summary>
public class AllConstraint : Constraint
{
    /// <summary>
    /// All constraints that must pass.
    /// </summary>
    public required IReadOnlyList<Constraint> Constraints { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        return Constraints.All(c => c.Evaluate(node, env, matcher));
    }
}

/// <summary>
/// Requires any inner constraint to pass (OR).
/// </summary>
public class AnyConstraint : Constraint
{
    /// <summary>
    /// Any of these constraints must pass.
    /// </summary>
    public required IReadOnlyList<Constraint> Constraints { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        return Constraints.Any(c => c.Evaluate(node, env, matcher));
    }
}

/// <summary>
/// Requires the node to appear inside an ancestor matching a pattern.
/// </summary>
public class InsideConstraint : Constraint
{
    /// <summary>
    /// The pattern the ancestor must match.
    /// </summary>
    public required PatternNode AncestorPattern { get; init; }

    /// <summary>
    /// How far to search up the tree.
    /// </summary>
    public StopBehavior StopBy { get; init; } = StopBehavior.End;

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        var current = node.Parent;
        while (current != null)
        {
            var tempEnv = new Dictionary<string, object>(env);
            if (matcher.TryMatch(current, AncestorPattern, tempEnv))
                return true;

            if (StopBy == StopBehavior.Neighbor)
                break;

            current = current.Parent;
        }
        return false;
    }
}

/// <summary>
/// Requires the node to NOT appear inside an ancestor matching a pattern.
/// </summary>
public class NotInsideConstraint : Constraint
{
    /// <summary>
    /// The pattern the ancestor must NOT match.
    /// </summary>
    public required PatternNode AncestorPattern { get; init; }

    /// <summary>
    /// How far to search up the tree.
    /// </summary>
    public StopBehavior StopBy { get; init; } = StopBehavior.End;

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        var insideConstraint = new InsideConstraint
        {
            AncestorPattern = AncestorPattern,
            StopBy = StopBy
        };
        return !insideConstraint.Evaluate(node, env, matcher);
    }
}

/// <summary>
/// Requires the node to have a descendant matching a pattern.
/// </summary>
public class HasConstraint : Constraint
{
    /// <summary>
    /// The pattern a descendant must match.
    /// </summary>
    public required PatternNode DescendantPattern { get; init; }

    /// <summary>
    /// How far to search down the tree.
    /// </summary>
    public StopBehavior StopBy { get; init; } = StopBehavior.End;

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        return matcher.HasMatchingDescendant(node, DescendantPattern, env, StopBy);
    }
}

/// <summary>
/// Requires the node to NOT have a descendant matching a pattern.
/// </summary>
public class NotHasConstraint : Constraint
{
    /// <summary>
    /// The pattern no descendant should match.
    /// </summary>
    public required PatternNode DescendantPattern { get; init; }

    /// <summary>
    /// How far to search down the tree.
    /// </summary>
    public StopBehavior StopBy { get; init; } = StopBehavior.End;

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        return !matcher.HasMatchingDescendant(node, DescendantPattern, env, StopBy);
    }
}

/// <summary>
/// Requires a sibling after this node to match a pattern.
/// </summary>
public class PrecedesConstraint : Constraint
{
    /// <summary>
    /// The pattern a following sibling must match.
    /// </summary>
    public required PatternNode SiblingPattern { get; init; }

    /// <summary>
    /// Whether the sibling must be immediately after (true) or anywhere after (false).
    /// </summary>
    public bool Immediate { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        var parent = node.Parent;
        if (parent == null) return false;

        var siblings = parent.Children;
        var idx = -1;

        // Find this node in siblings
        for (int i = 0; i < siblings.Count; i++)
        {
            if (ReferenceEquals(siblings[i], node))
            {
                idx = i;
                break;
            }
        }

        if (idx < 0) return false;

        // Check siblings after this node
        if (Immediate)
        {
            if (idx + 1 >= siblings.Count) return false;
            var tempEnv = new Dictionary<string, object>(env);
            return matcher.TryMatch(siblings[idx + 1], SiblingPattern, tempEnv);
        }
        else
        {
            for (int i = idx + 1; i < siblings.Count; i++)
            {
                var tempEnv = new Dictionary<string, object>(env);
                if (matcher.TryMatch(siblings[i], SiblingPattern, tempEnv))
                    return true;
            }
            return false;
        }
    }
}

/// <summary>
/// Requires a sibling before this node to match a pattern.
/// </summary>
public class FollowsConstraint : Constraint
{
    /// <summary>
    /// The pattern a preceding sibling must match.
    /// </summary>
    public required PatternNode SiblingPattern { get; init; }

    /// <summary>
    /// Whether the sibling must be immediately before (true) or anywhere before (false).
    /// </summary>
    public bool Immediate { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        var parent = node.Parent;
        if (parent == null) return false;

        var siblings = parent.Children;
        var idx = -1;

        // Find this node in siblings
        for (int i = 0; i < siblings.Count; i++)
        {
            if (ReferenceEquals(siblings[i], node))
            {
                idx = i;
                break;
            }
        }

        if (idx < 0) return false;

        // Check siblings before this node
        if (Immediate)
        {
            if (idx - 1 < 0) return false;
            var tempEnv = new Dictionary<string, object>(env);
            return matcher.TryMatch(siblings[idx - 1], SiblingPattern, tempEnv);
        }
        else
        {
            for (int i = idx - 1; i >= 0; i--)
            {
                var tempEnv = new Dictionary<string, object>(env);
                if (matcher.TryMatch(siblings[i], SiblingPattern, tempEnv))
                    return true;
            }
            return false;
        }
    }
}

/// <summary>
/// Matches the node against a pattern.
/// </summary>
public class PatternConstraint : Constraint
{
    /// <summary>
    /// The pattern to match.
    /// </summary>
    public required PatternNode Pattern { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        var tempEnv = new Dictionary<string, object>(env);
        return matcher.TryMatch(node, Pattern, tempEnv);
    }
}

/// <summary>
/// Matches based on the length of captured collection.
/// </summary>
public class LengthConstraint : Constraint
{
    /// <summary>
    /// Minimum length (inclusive).
    /// </summary>
    public int? MinLength { get; init; }

    /// <summary>
    /// Maximum length (inclusive).
    /// </summary>
    public int? MaxLength { get; init; }

    /// <summary>
    /// Exact length required.
    /// </summary>
    public int? ExactLength { get; init; }

    public override bool Evaluate(UastNode node, Dictionary<string, object> env, IConstraintMatcher matcher)
    {
        // This constraint is typically used on captured collections
        // For a single node, count is 1
        int count = 1;

        if (ExactLength.HasValue && count != ExactLength.Value)
            return false;

        if (MinLength.HasValue && count < MinLength.Value)
            return false;

        if (MaxLength.HasValue && count > MaxLength.Value)
            return false;

        return true;
    }
}
