namespace UAST.Core.Schema.Expressions;

/// <summary>
/// Represents a PowerShell pipeline expression.
/// </summary>
public class PipelineExpression : ExpressionNode
{
    /// <summary>
    /// The pipeline elements/commands.
    /// </summary>
    public required IReadOnlyList<ExpressionNode> Commands { get; init; }

    /// <summary>
    /// Whether this pipeline runs in the background (&).
    /// </summary>
    public bool IsBackground { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Commands.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a PowerShell pipeline chain expression (&& or ||).
/// </summary>
public class PipelineChainExpression : ExpressionNode
{
    /// <summary>
    /// The left pipeline.
    /// </summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>
    /// The chain operator (&& or ||).
    /// </summary>
    public required string Operator { get; init; }

    /// <summary>
    /// The right pipeline.
    /// </summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Represents a PowerShell command invocation.
/// </summary>
public class CommandExpression : ExpressionNode
{
    /// <summary>
    /// The command name.
    /// </summary>
    public required string CommandName { get; init; }

    /// <summary>
    /// The named parameters.
    /// </summary>
    public IReadOnlyList<CommandParameterNode> Parameters { get; init; } = [];

    /// <summary>
    /// The positional arguments.
    /// </summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    /// <summary>
    /// The redirection operators.
    /// </summary>
    public IReadOnlyList<RedirectionNode> Redirections { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.AddRange(Arguments);
        children.AddRange(Redirections);
        return children;
    }
}

/// <summary>
/// Represents a command parameter (-Name Value).
/// </summary>
public class CommandParameterNode : UastNode
{
    /// <summary>
    /// The parameter name (without dash).
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The parameter value (if any).
    /// </summary>
    public ExpressionNode? Value { get; init; }

    /// <summary>
    /// Whether the value is colon-separated (-Name:Value).
    /// </summary>
    public bool IsColonSeparated { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Value != null ? [Value] : [];
    }
}

/// <summary>
/// Represents a redirection operator.
/// </summary>
public class RedirectionNode : UastNode
{
    /// <summary>
    /// The stream number (1=stdout, 2=stderr, etc.).
    /// </summary>
    public int StreamNumber { get; init; } = 1;

    /// <summary>
    /// The redirection operator (>, >>, 2>, etc.).
    /// </summary>
    public required string Operator { get; init; }

    /// <summary>
    /// The target (file path or stream).
    /// </summary>
    public required ExpressionNode Target { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Target];
}

/// <summary>
/// Represents a PowerShell scriptblock expression.
/// </summary>
public class ScriptBlockExpression : ExpressionNode
{
    /// <summary>
    /// The script parameters.
    /// </summary>
    public IReadOnlyList<Declarations.ParameterNode> Parameters { get; init; } = [];

    /// <summary>
    /// The begin block.
    /// </summary>
    public Statements.BlockNode? Begin { get; init; }

    /// <summary>
    /// The process block.
    /// </summary>
    public Statements.BlockNode? Process { get; init; }

    /// <summary>
    /// The end block.
    /// </summary>
    public Statements.BlockNode? End { get; init; }

    /// <summary>
    /// The clean block (PS7+).
    /// </summary>
    public Statements.BlockNode? Clean { get; init; }

    /// <summary>
    /// The dynamicparam block.
    /// </summary>
    public Statements.BlockNode? DynamicParam { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        if (Begin != null) children.Add(Begin);
        if (Process != null) children.Add(Process);
        if (End != null) children.Add(End);
        if (Clean != null) children.Add(Clean);
        if (DynamicParam != null) children.Add(DynamicParam);
        return children;
    }
}

/// <summary>
/// Represents a splatting expression (@hash or @array).
/// </summary>
public class SplattingExpression : ExpressionNode
{
    /// <summary>
    /// The variable being splatted.
    /// </summary>
    public required VariableExpression Variable { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Variable];
}

/// <summary>
/// Represents a sub-expression $(...).
/// </summary>
public class SubExpression : ExpressionNode
{
    /// <summary>
    /// The statements inside the sub-expression.
    /// </summary>
    public required IReadOnlyList<Statements.StatementNode> Statements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Statements.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents an array sub-expression @(...).
/// </summary>
public class ArraySubExpression : ExpressionNode
{
    /// <summary>
    /// The statements inside.
    /// </summary>
    public required IReadOnlyList<Statements.StatementNode> Statements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Statements.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a using expression ($using:var).
/// </summary>
public class UsingExpression : ExpressionNode
{
    /// <summary>
    /// The variable being referenced from the parent scope.
    /// </summary>
    public required VariableExpression Variable { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Variable];
}

/// <summary>
/// Represents a type expression [type].
/// </summary>
public class TypeExpression : ExpressionNode
{
    /// <summary>
    /// The type reference.
    /// </summary>
    public required Types.TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Represents a data statement.
/// </summary>
public class DataStatement : Statements.StatementNode
{
    /// <summary>
    /// The data section name (if any).
    /// </summary>
    public string? Name { get; init; }

    /// <summary>
    /// Supported commands in the data section.
    /// </summary>
    public IReadOnlyList<string> SupportedCommands { get; init; } = [];

    /// <summary>
    /// The data body.
    /// </summary>
    public required Statements.BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}
