namespace UAST.Core.Schema.Expressions;

/// <summary>
/// Represents a binary operation.
/// </summary>
public class BinaryExpression : ExpressionNode
{
    /// <summary>
    /// The left operand.
    /// </summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>
    /// The operator.
    /// </summary>
    public required BinaryOperator Operator { get; init; }

    /// <summary>
    /// The right operand.
    /// </summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Represents a unary operation.
/// </summary>
public class UnaryExpression : ExpressionNode
{
    /// <summary>
    /// The operator.
    /// </summary>
    public required UnaryOperator Operator { get; init; }

    /// <summary>
    /// The operand.
    /// </summary>
    public required ExpressionNode Operand { get; init; }

    /// <summary>
    /// Whether this is a prefix operator (default) or postfix.
    /// </summary>
    public bool IsPrefix { get; init; } = true;

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// Represents an assignment expression.
/// </summary>
public class AssignmentExpression : ExpressionNode
{
    /// <summary>
    /// The assignment target.
    /// </summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>
    /// The assignment operator.
    /// </summary>
    public AssignmentOperator Operator { get; init; } = AssignmentOperator.Assign;

    /// <summary>
    /// The value being assigned.
    /// </summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Target, Value];
}

/// <summary>
/// Represents a conditional (ternary) expression.
/// </summary>
public class ConditionalExpression : ExpressionNode
{
    /// <summary>
    /// The condition.
    /// </summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>
    /// The expression if condition is true.
    /// </summary>
    public required ExpressionNode ThenExpression { get; init; }

    /// <summary>
    /// The expression if condition is false.
    /// </summary>
    public required ExpressionNode ElseExpression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Condition, ThenExpression, ElseExpression];
}

/// <summary>
/// Represents a type cast/conversion expression.
/// </summary>
public class CastExpression : ExpressionNode
{
    /// <summary>
    /// The target type.
    /// </summary>
    public required Types.TypeReference Type { get; init; }

    /// <summary>
    /// The expression being cast.
    /// </summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type, Expression];
}

/// <summary>
/// Represents a type check expression (is/instanceof).
/// </summary>
public class TypeCheckExpression : ExpressionNode
{
    /// <summary>
    /// The expression being checked.
    /// </summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>
    /// The type to check against.
    /// </summary>
    public required Types.TypeReference Type { get; init; }

    /// <summary>
    /// Whether this is a negated check (is not).
    /// </summary>
    public bool IsNegated { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Type];
}
