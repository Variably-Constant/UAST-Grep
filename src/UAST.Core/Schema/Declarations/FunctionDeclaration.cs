using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Declarations;

/// <summary>
/// Represents a function, method, or cmdlet declaration.
/// Maps to: PowerShell FunctionDefinitionAst, C# MethodDeclarationSyntax,
/// Python FunctionDef, JavaScript FunctionDeclaration, etc.
/// </summary>
public class FunctionDeclaration : DeclarationNode
{
    /// <summary>
    /// The function parameters.
    /// </summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>
    /// The declared return type (if any).
    /// </summary>
    public TypeReference? ReturnType { get; init; }

    /// <summary>
    /// The function body.
    /// </summary>
    public BlockNode? Body { get; init; }

    /// <summary>
    /// Whether this is an async function.
    /// </summary>
    public bool IsAsync { get; init; }

    /// <summary>
    /// Whether this is a generator function.
    /// </summary>
    public bool IsGenerator { get; init; }

    /// <summary>
    /// Type parameters for generic functions.
    /// </summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Represents a function/method parameter.
/// </summary>
public class ParameterNode : UastNode
{
    /// <summary>
    /// The parameter name.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The declared type (if any).
    /// </summary>
    public TypeReference? Type { get; init; }

    /// <summary>
    /// The default value (if any).
    /// </summary>
    public Expressions.ExpressionNode? DefaultValue { get; init; }

    /// <summary>
    /// Whether this is a rest/params parameter (..., *args, params).
    /// </summary>
    public bool IsRest { get; init; }

    /// <summary>
    /// Whether this parameter is optional.
    /// </summary>
    public bool IsOptional { get; init; }

    /// <summary>
    /// Attributes applied to this parameter.
    /// </summary>
    public IReadOnlyList<AttributeNode> Attributes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        if (Type != null) children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

/// <summary>
/// Represents a type parameter in a generic declaration.
/// </summary>
public class TypeParameterNode : UastNode
{
    /// <summary>
    /// The type parameter name.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// Constraints on the type parameter.
    /// </summary>
    public IReadOnlyList<TypeReference> Constraints { get; init; } = [];

    /// <summary>
    /// Variance (in/out for C#, etc.).
    /// </summary>
    public string? Variance { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Constraints.Cast<UastNode>().ToList();
    }
}
