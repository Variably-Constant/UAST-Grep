namespace UAST.Core.Schema.Expressions;

/// <summary>
/// Represents a function/method call.
/// </summary>
public class CallExpression : ExpressionNode
{
    /// <summary>
    /// The callee (function/method being called).
    /// </summary>
    public required ExpressionNode Callee { get; init; }

    /// <summary>
    /// The arguments.
    /// </summary>
    public required IReadOnlyList<ArgumentNode> Arguments { get; init; }

    /// <summary>
    /// Type arguments for generic calls.
    /// </summary>
    public IReadOnlyList<Types.TypeReference> TypeArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Callee };
        children.AddRange(TypeArguments);
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Represents a function argument.
/// </summary>
public class ArgumentNode : UastNode
{
    /// <summary>
    /// The argument name (for named arguments).
    /// </summary>
    public string? Name { get; init; }

    /// <summary>
    /// The argument value.
    /// </summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>
    /// Whether this is a spread argument.
    /// </summary>
    public bool IsSpread { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Represents member access (object.member).
/// </summary>
public class MemberExpression : ExpressionNode
{
    /// <summary>
    /// The object being accessed.
    /// </summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>
    /// The member name.
    /// </summary>
    public required string Member { get; init; }

    /// <summary>
    /// Whether this is null-safe access (?./).
    /// </summary>
    public bool IsNullSafe { get; init; }

    /// <summary>
    /// Whether this is static member access.
    /// </summary>
    public bool IsStatic { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Object];
}

/// <summary>
/// Represents index/subscript access (object[index]).
/// </summary>
public class IndexExpression : ExpressionNode
{
    /// <summary>
    /// The object being indexed.
    /// </summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>
    /// The index expression.
    /// </summary>
    public required ExpressionNode Index { get; init; }

    /// <summary>
    /// Whether this is null-safe access.
    /// </summary>
    public bool IsNullSafe { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Object, Index];
}

/// <summary>
/// Represents a new/instantiation expression.
/// </summary>
public class NewExpression : ExpressionNode
{
    /// <summary>
    /// The type being instantiated.
    /// </summary>
    public required Types.TypeReference Type { get; init; }

    /// <summary>
    /// The constructor arguments.
    /// </summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    /// <summary>
    /// Object initializer (if any).
    /// </summary>
    public ObjectExpression? Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Arguments);
        if (Initializer != null) children.Add(Initializer);
        return children;
    }
}

/// <summary>
/// Represents an invocation of a member method.
/// </summary>
public class InvokeMemberExpression : ExpressionNode
{
    /// <summary>
    /// The object on which to invoke the method.
    /// </summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>
    /// The method name.
    /// </summary>
    public required string MethodName { get; init; }

    /// <summary>
    /// The arguments.
    /// </summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    /// <summary>
    /// Whether this is static invocation.
    /// </summary>
    public bool IsStatic { get; init; }

    /// <summary>
    /// Whether this is null-safe invocation.
    /// </summary>
    public bool IsNullSafe { get; init; }

    /// <summary>
    /// Type arguments for generic method calls.
    /// </summary>
    public IReadOnlyList<Types.TypeReference> TypeArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Object };
        children.AddRange(TypeArguments);
        children.AddRange(Arguments);
        return children;
    }
}
