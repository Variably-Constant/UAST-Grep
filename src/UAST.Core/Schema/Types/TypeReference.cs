namespace UAST.Core.Schema.Types;

/// <summary>
/// Base class for type references.
/// </summary>
public abstract class TypeReference : UastNode
{
}

/// <summary>
/// Represents a named type reference.
/// </summary>
public class NamedTypeReference : TypeReference
{
    /// <summary>
    /// The type name.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The namespace or containing type.
    /// </summary>
    public string? Namespace { get; init; }

    /// <summary>
    /// Type arguments for generic types.
    /// </summary>
    public IReadOnlyList<TypeReference> TypeArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return TypeArguments.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents an array type reference.
/// </summary>
public class ArrayTypeReference : TypeReference
{
    /// <summary>
    /// The element type.
    /// </summary>
    public required TypeReference ElementType { get; init; }

    /// <summary>
    /// The array size (if fixed).
    /// </summary>
    public int? Size { get; init; }

    /// <summary>
    /// The number of dimensions.
    /// </summary>
    public int Dimensions { get; init; } = 1;

    protected override IReadOnlyList<UastNode> GetChildren() => [ElementType];
}

/// <summary>
/// Represents a nullable type reference.
/// </summary>
public class NullableTypeReference : TypeReference
{
    /// <summary>
    /// The inner type.
    /// </summary>
    public required TypeReference InnerType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [InnerType];
}

/// <summary>
/// Represents a union type reference (TypeScript, Python).
/// </summary>
public class UnionTypeReference : TypeReference
{
    /// <summary>
    /// The types in the union.
    /// </summary>
    public required IReadOnlyList<TypeReference> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Types.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents an intersection type reference (TypeScript).
/// </summary>
public class IntersectionTypeReference : TypeReference
{
    /// <summary>
    /// The types in the intersection.
    /// </summary>
    public required IReadOnlyList<TypeReference> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Types.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a function type reference.
/// </summary>
public class FunctionTypeReference : TypeReference
{
    /// <summary>
    /// The parameter types.
    /// </summary>
    public required IReadOnlyList<TypeReference> ParameterTypes { get; init; }

    /// <summary>
    /// The return type.
    /// </summary>
    public required TypeReference ReturnType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(ParameterTypes);
        children.Add(ReturnType);
        return children;
    }
}

/// <summary>
/// Represents a tuple type reference.
/// </summary>
public class TupleTypeReference : TypeReference
{
    /// <summary>
    /// The element types.
    /// </summary>
    public required IReadOnlyList<TypeReference> ElementTypes { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return ElementTypes.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a literal type (TypeScript specific).
/// </summary>
public class LiteralTypeReference : TypeReference
{
    /// <summary>
    /// The literal value.
    /// </summary>
    public required object? Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}
