using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Statements;

/// <summary>
/// Represents a try-catch-finally statement.
/// </summary>
public class TryStatement : StatementNode
{
    /// <summary>
    /// The try block.
    /// </summary>
    public required BlockNode TryBlock { get; init; }

    /// <summary>
    /// The catch clauses.
    /// </summary>
    public IReadOnlyList<CatchClause> CatchClauses { get; init; } = [];

    /// <summary>
    /// The finally block (if any).
    /// </summary>
    public BlockNode? FinallyBlock { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { TryBlock };
        children.AddRange(CatchClauses);
        if (FinallyBlock != null) children.Add(FinallyBlock);
        return children;
    }
}

/// <summary>
/// Represents a catch clause in a try statement.
/// </summary>
public class CatchClause : UastNode
{
    /// <summary>
    /// The exception variable name (if any).
    /// </summary>
    public string? ExceptionVariable { get; init; }

    /// <summary>
    /// The exception type(s) being caught.
    /// </summary>
    public IReadOnlyList<TypeReference> ExceptionTypes { get; init; } = [];

    /// <summary>
    /// The catch block body.
    /// </summary>
    public required BlockNode Body { get; init; }

    /// <summary>
    /// A filter condition (C# "when" clause).
    /// </summary>
    public Expressions.ExpressionNode? Filter { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(ExceptionTypes);
        if (Filter != null) children.Add(Filter);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Represents a trap statement (PowerShell-specific).
/// </summary>
public class TrapStatement : StatementNode
{
    /// <summary>
    /// The exception type being trapped (if any).
    /// </summary>
    public TypeReference? ExceptionType { get; init; }

    /// <summary>
    /// The trap body.
    /// </summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (ExceptionType != null) children.Add(ExceptionType);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Represents a using statement (C# resource management).
/// </summary>
public class UsingStatement : StatementNode
{
    /// <summary>
    /// The resource being managed.
    /// </summary>
    public required Expressions.ExpressionNode Resource { get; init; }

    /// <summary>
    /// The using block body.
    /// </summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Resource, Body];
}
