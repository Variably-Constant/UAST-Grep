using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Declarations;

/// <summary>
/// Represents a type declaration (class, struct, interface, enum, etc.).
/// Maps to: PowerShell TypeDefinitionAst, C# ClassDeclarationSyntax, Python ClassDef, etc.
/// </summary>
public class TypeDeclaration : DeclarationNode
{
    /// <summary>
    /// The kind of type (class, struct, interface, etc.).
    /// </summary>
    public TypeDeclarationKind Kind { get; init; }

    /// <summary>
    /// Base types/interfaces this type extends/implements.
    /// </summary>
    public IReadOnlyList<TypeReference> BaseTypes { get; init; } = [];

    /// <summary>
    /// Type parameters for generic types.
    /// </summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    /// <summary>
    /// Members of this type (methods, properties, fields, etc.).
    /// </summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Represents an enum member declaration.
/// </summary>
public class EnumMemberDeclaration : DeclarationNode
{
    /// <summary>
    /// The explicit value assigned to this enum member (if any).
    /// </summary>
    public Expressions.ExpressionNode? Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        if (Value != null) children.Add(Value);
        return children;
    }
}
