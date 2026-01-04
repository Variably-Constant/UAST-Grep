namespace UAST.Core.Schema.Expressions;

/// <summary>
/// Represents an array literal expression.
/// </summary>
public class ArrayExpression : ExpressionNode
{
    /// <summary>
    /// The array elements.
    /// </summary>
    public required IReadOnlyList<ExpressionNode> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Elements.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents an object/dictionary literal expression.
/// </summary>
public class ObjectExpression : ExpressionNode
{
    /// <summary>
    /// The object properties.
    /// </summary>
    public required IReadOnlyList<PropertyNode> Properties { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Properties.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a property in an object literal.
/// </summary>
public class PropertyNode : UastNode
{
    /// <summary>
    /// The property key.
    /// </summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>
    /// The property value.
    /// </summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>
    /// Whether this is a computed property ([expr]: value).
    /// </summary>
    public bool IsComputed { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Value];
}

/// <summary>
/// Represents a lambda/anonymous function expression.
/// </summary>
public class LambdaExpression : ExpressionNode
{
    /// <summary>
    /// The lambda parameters.
    /// </summary>
    public required IReadOnlyList<Declarations.ParameterNode> Parameters { get; init; }

    /// <summary>
    /// The lambda body (expression or block).
    /// </summary>
    public required UastNode Body { get; init; }

    /// <summary>
    /// Whether this is an async lambda.
    /// </summary>
    public bool IsAsync { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Represents an interpolated string expression.
/// </summary>
public class InterpolatedStringExpression : ExpressionNode
{
    /// <summary>
    /// The parts of the interpolated string (text and expressions).
    /// </summary>
    public required IReadOnlyList<InterpolatedStringPart> Parts { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Parts.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a part of an interpolated string.
/// </summary>
public class InterpolatedStringPart : UastNode
{
    /// <summary>
    /// The text content (for literal parts).
    /// </summary>
    public string? Text { get; init; }

    /// <summary>
    /// The interpolated expression (for expression parts).
    /// </summary>
    public ExpressionNode? Expression { get; init; }

    /// <summary>
    /// Whether this is a literal text part.
    /// </summary>
    public bool IsText => Text != null;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Expression != null ? [Expression] : [];
    }
}

/// <summary>
/// Represents a range expression (1..10).
/// </summary>
public class RangeExpression : ExpressionNode
{
    /// <summary>
    /// The start of the range.
    /// </summary>
    public ExpressionNode? Start { get; init; }

    /// <summary>
    /// The end of the range.
    /// </summary>
    public ExpressionNode? End { get; init; }

    /// <summary>
    /// Whether the end is inclusive.
    /// </summary>
    public bool IsInclusive { get; init; } = true;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Start != null) children.Add(Start);
        if (End != null) children.Add(End);
        return children;
    }
}
