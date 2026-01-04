using UAST.Core.Schema.Expressions;

namespace UAST.Core.Schema.Statements;

/// <summary>
/// Represents a lock statement (C#).
/// </summary>
public class LockStatement : StatementNode
{
    /// <summary>
    /// The object to lock on.
    /// </summary>
    public required ExpressionNode LockObject { get; init; }

    /// <summary>
    /// The lock body.
    /// </summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [LockObject, Body];
}

/// <summary>
/// Represents a yield statement (C# iterator).
/// </summary>
public class YieldStatement : StatementNode
{
    /// <summary>
    /// The yielded value (null for yield break).
    /// </summary>
    public ExpressionNode? Value { get; init; }

    /// <summary>
    /// Whether this is yield break (vs yield return).
    /// </summary>
    public bool IsBreak { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Value != null ? [Value] : [];
    }
}

/// <summary>
/// Represents a goto statement.
/// </summary>
public class GotoStatement : StatementNode
{
    /// <summary>
    /// The label to jump to.
    /// </summary>
    public required string Label { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a labeled statement.
/// </summary>
public class LabeledStatement : StatementNode
{
    /// <summary>
    /// The label.
    /// </summary>
    public required string Label { get; init; }

    /// <summary>
    /// The statement that follows the label.
    /// </summary>
    public required StatementNode Statement { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Statement];
}

/// <summary>
/// Represents a checked/unchecked statement.
/// </summary>
public class CheckedStatement : StatementNode
{
    /// <summary>
    /// Whether this is checked (vs unchecked).
    /// </summary>
    public bool IsChecked { get; init; }

    /// <summary>
    /// The checked/unchecked block body.
    /// </summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Represents an unsafe statement.
/// </summary>
public class UnsafeStatement : StatementNode
{
    /// <summary>
    /// The unsafe block body.
    /// </summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Represents a fixed statement (for pinning pointers).
/// </summary>
public class FixedStatement : StatementNode
{
    /// <summary>
    /// The variables being fixed.
    /// </summary>
    public required IReadOnlyList<ExpressionNode> Variables { get; init; }

    /// <summary>
    /// The fixed block body.
    /// </summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Variables);
        children.Add(Body);
        return children;
    }
}
