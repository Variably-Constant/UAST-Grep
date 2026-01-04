namespace UAST.Core.Schema.Expressions;

/// <summary>
/// Base class for all expression nodes.
/// </summary>
public abstract class ExpressionNode : UastNode
{
}

/// <summary>
/// Represents a literal value (string, number, boolean, null, etc.).
/// </summary>
public class LiteralExpression : ExpressionNode
{
    /// <summary>
    /// The literal value.
    /// </summary>
    public required object? Value { get; init; }

    /// <summary>
    /// The kind of literal.
    /// </summary>
    public required LiteralKind Kind { get; init; }

    /// <summary>
    /// The raw text representation.
    /// </summary>
    public required string RawText { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents an identifier reference.
/// </summary>
public class IdentifierExpression : ExpressionNode
{
    /// <summary>
    /// The identifier name.
    /// </summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a variable reference (PowerShell $var, etc.).
/// </summary>
public class VariableExpression : ExpressionNode
{
    /// <summary>
    /// The variable name (without sigil).
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The scope qualifier (global, local, script, etc.).
    /// </summary>
    public string? Scope { get; init; }

    /// <summary>
    /// Whether this is splatted (@var).
    /// </summary>
    public bool IsSplatted { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a parenthesized expression.
/// </summary>
public class ParenthesizedExpression : ExpressionNode
{
    /// <summary>
    /// The inner expression.
    /// </summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Represents an unknown or unmappable expression from the source language.
/// Used as a fallback when an expression construct cannot map to a standard UAST node.
/// </summary>
public class UnknownExpression : ExpressionNode
{
    /// <summary>
    /// The native node type name from the source language's AST.
    /// </summary>
    public required string NativeNodeType { get; init; }

    /// <summary>
    /// Semantic roles that describe this expression's purpose.
    /// </summary>
    public IReadOnlyList<string> Roles { get; init; } = [];

    /// <summary>
    /// Child nodes that were successfully mapped.
    /// </summary>
    public IReadOnlyList<UastNode> ChildNodes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() => ChildNodes;
}
