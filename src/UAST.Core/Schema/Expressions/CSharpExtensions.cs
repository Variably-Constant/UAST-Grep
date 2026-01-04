namespace UAST.Core.Schema.Expressions;

/// <summary>
/// Represents a C# LINQ query expression.
/// </summary>
public class LinqExpression : ExpressionNode
{
    /// <summary>
    /// The source collection (from clause).
    /// </summary>
    public required ExpressionNode Source { get; init; }

    /// <summary>
    /// The query clauses (where, orderby, etc.).
    /// </summary>
    public IReadOnlyList<LinqClause> Clauses { get; init; } = [];

    /// <summary>
    /// The final select or group clause expression.
    /// </summary>
    public ExpressionNode? SelectOrGroup { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Source };
        children.AddRange(Clauses);
        if (SelectOrGroup != null) children.Add(SelectOrGroup);
        return children;
    }
}

/// <summary>
/// Represents a clause in a LINQ expression.
/// </summary>
public class LinqClause : UastNode
{
    /// <summary>
    /// The type of clause (where, orderby, select, group, join, let, etc.).
    /// </summary>
    public required string ClauseType { get; init; }

    /// <summary>
    /// The clause expression.
    /// </summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>
    /// Optional ordering direction (ascending/descending for orderby).
    /// </summary>
    public string? Ordering { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Represents a tuple expression (a, b, c).
/// </summary>
public class TupleExpression : ExpressionNode
{
    /// <summary>
    /// The tuple elements.
    /// </summary>
    public required IReadOnlyList<ExpressionNode> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Elements.Cast<UastNode>().ToList();
    }
}
