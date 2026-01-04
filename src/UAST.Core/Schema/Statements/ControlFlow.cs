using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Statements;

/// <summary>
/// Represents an if statement with optional else.
/// </summary>
public class IfStatement : StatementNode
{
    /// <summary>
    /// The condition expression.
    /// </summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>
    /// The then branch.
    /// </summary>
    public required StatementNode ThenBranch { get; init; }

    /// <summary>
    /// The else branch (if any). Can be another IfStatement for elseif chains.
    /// </summary>
    public StatementNode? ElseBranch { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Condition, ThenBranch };
        if (ElseBranch != null) children.Add(ElseBranch);
        return children;
    }
}

/// <summary>
/// Represents a while loop.
/// </summary>
public class WhileStatement : StatementNode
{
    /// <summary>
    /// The loop condition.
    /// </summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>
    /// The loop body.
    /// </summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Condition, Body];
}

/// <summary>
/// Represents a do-while loop.
/// </summary>
public class DoWhileStatement : StatementNode
{
    /// <summary>
    /// The loop body.
    /// </summary>
    public required StatementNode Body { get; init; }

    /// <summary>
    /// The loop condition.
    /// </summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>
    /// Whether this is do-until (inverted condition).
    /// </summary>
    public bool IsUntil { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body, Condition];
}

/// <summary>
/// Represents a for loop with initializer, condition, and increment.
/// </summary>
public class ForStatement : StatementNode
{
    /// <summary>
    /// The initializer (if any).
    /// </summary>
    public StatementNode? Initializer { get; init; }

    /// <summary>
    /// The condition (if any).
    /// </summary>
    public ExpressionNode? Condition { get; init; }

    /// <summary>
    /// The increment expression (if any).
    /// </summary>
    public ExpressionNode? Increment { get; init; }

    /// <summary>
    /// The loop body.
    /// </summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Initializer != null) children.Add(Initializer);
        if (Condition != null) children.Add(Condition);
        if (Increment != null) children.Add(Increment);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Represents a foreach/for-in loop.
/// </summary>
public class ForEachStatement : StatementNode
{
    /// <summary>
    /// The loop variable name.
    /// </summary>
    public required string VariableName { get; init; }

    /// <summary>
    /// The loop variable type (if declared).
    /// </summary>
    public TypeReference? VariableType { get; init; }

    /// <summary>
    /// The collection being iterated.
    /// </summary>
    public required ExpressionNode Iterable { get; init; }

    /// <summary>
    /// The loop body.
    /// </summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (VariableType != null) children.Add(VariableType);
        children.Add(Iterable);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Represents a switch/match statement.
/// </summary>
public class SwitchStatement : StatementNode
{
    /// <summary>
    /// The value being switched on.
    /// </summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>
    /// The switch cases.
    /// </summary>
    public required IReadOnlyList<SwitchCase> Cases { get; init; }

    /// <summary>
    /// PowerShell-specific flags (like -Regex, -Wildcard, -Exact).
    /// </summary>
    public IReadOnlyList<string> Flags { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(Cases);
        return children;
    }
}

/// <summary>
/// Represents a case in a switch statement.
/// </summary>
public class SwitchCase : UastNode
{
    /// <summary>
    /// The patterns for this case. Empty means default case.
    /// </summary>
    public IReadOnlyList<ExpressionNode> Patterns { get; init; } = [];

    /// <summary>
    /// The case body.
    /// </summary>
    public required IReadOnlyList<StatementNode> Body { get; init; }

    /// <summary>
    /// Language-specific pattern nodes (e.g., Swift's IsPattern, AsPattern, WildcardPattern).
    /// These are in addition to the standard expression patterns.
    /// </summary>
    public IReadOnlyList<UastNode> LanguagePatterns { get; init; } = [];

    /// <summary>
    /// Whether this is the default case.
    /// </summary>
    public bool IsDefault => Patterns.Count == 0;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Patterns);
        children.AddRange(LanguagePatterns);
        children.AddRange(Body);
        return children;
    }
}
