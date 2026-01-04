namespace UAST.Core.Schema.Statements;

/// <summary>
/// Base class for all statement nodes.
/// </summary>
public abstract class StatementNode : UastNode
{
}

/// <summary>
/// Represents a block of statements.
/// </summary>
public class BlockNode : StatementNode
{
    /// <summary>
    /// The statements in this block.
    /// </summary>
    public required IReadOnlyList<StatementNode> Statements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Statements.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents an expression used as a statement.
/// </summary>
public class ExpressionStatement : StatementNode
{
    /// <summary>
    /// The expression.
    /// </summary>
    public required Expressions.ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Represents a return statement.
/// </summary>
public class ReturnStatement : StatementNode
{
    /// <summary>
    /// The value being returned (if any).
    /// </summary>
    public Expressions.ExpressionNode? Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Value != null ? [Value] : [];
    }
}

/// <summary>
/// Represents a break statement.
/// </summary>
public class BreakStatement : StatementNode
{
    /// <summary>
    /// The label to break to (if any).
    /// </summary>
    public string? Label { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a continue statement.
/// </summary>
public class ContinueStatement : StatementNode
{
    /// <summary>
    /// The label to continue to (if any).
    /// </summary>
    public string? Label { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a throw statement.
/// </summary>
public class ThrowStatement : StatementNode
{
    /// <summary>
    /// The exception being thrown (if any, for re-throw).
    /// </summary>
    public Expressions.ExpressionNode? Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Expression != null ? [Expression] : [];
    }
}

/// <summary>
/// Represents an exit statement (PowerShell).
/// </summary>
public class ExitStatement : StatementNode
{
    /// <summary>
    /// The exit code (if any).
    /// </summary>
    public Expressions.ExpressionNode? ExitCode { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return ExitCode != null ? [ExitCode] : [];
    }
}

/// <summary>
/// Represents an empty statement.
/// </summary>
public class EmptyStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}
