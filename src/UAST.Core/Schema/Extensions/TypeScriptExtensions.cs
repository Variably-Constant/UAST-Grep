using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region TypeScript Type System Extensions

/// <summary>
/// TypeScript conditional type: T extends U ? X : Y
/// </summary>
public class ConditionalTypeReference : TypeReference
{
    /// <summary>The type being checked (T)</summary>
    public required TypeReference CheckType { get; init; }

    /// <summary>The type being extended (U)</summary>
    public required TypeReference ExtendsType { get; init; }

    /// <summary>The type if condition is true (X)</summary>
    public required TypeReference TrueType { get; init; }

    /// <summary>The type if condition is false (Y)</summary>
    public required TypeReference FalseType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        [CheckType, ExtendsType, TrueType, FalseType];
}

/// <summary>
/// TypeScript mapped type: { [K in T]: V }
/// </summary>
public class MappedTypeReference : TypeReference
{
    /// <summary>The type parameter name (K)</summary>
    public required string TypeParameter { get; init; }

    /// <summary>The constraint type (T)</summary>
    public required TypeReference ConstraintType { get; init; }

    /// <summary>The value type (V)</summary>
    public required TypeReference ValueType { get; init; }

    /// <summary>Readonly modifier (+readonly, -readonly, or none)</summary>
    public MappedTypeModifier ReadonlyModifier { get; init; }

    /// <summary>Optional modifier (+?, -?, or none)</summary>
    public MappedTypeModifier OptionalModifier { get; init; }

    /// <summary>The 'as' clause for key remapping (optional)</summary>
    public TypeReference? NameType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { ConstraintType, ValueType };
        if (NameType != null) children.Add(NameType);
        return children;
    }
}

/// <summary>
/// Modifier for mapped type readonly/optional properties.
/// </summary>
public enum MappedTypeModifier
{
    /// <summary>No modifier</summary>
    None,
    /// <summary>Add modifier (+readonly, +?)</summary>
    Add,
    /// <summary>Remove modifier (-readonly, -?)</summary>
    Remove
}

/// <summary>
/// TypeScript index type query: keyof T
/// </summary>
public class KeyofTypeReference : TypeReference
{
    /// <summary>The type to get keys from</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// TypeScript indexed access type: T[K]
/// </summary>
public class IndexedAccessTypeReference : TypeReference
{
    /// <summary>The object type (T)</summary>
    public required TypeReference ObjectType { get; init; }

    /// <summary>The index type (K)</summary>
    public required TypeReference IndexType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [ObjectType, IndexType];
}

/// <summary>
/// TypeScript infer type in conditional: infer T
/// </summary>
public class InferTypeReference : TypeReference
{
    /// <summary>The type parameter name to infer</summary>
    public required string TypeParameter { get; init; }

    /// <summary>Optional constraint: infer T extends U</summary>
    public TypeReference? Constraint { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Constraint != null ? [Constraint] : [];
    }
}

/// <summary>
/// TypeScript template literal type: `hello ${T}`
/// </summary>
public class TemplateLiteralTypeReference : TypeReference
{
    /// <summary>The parts of the template literal type</summary>
    public required IReadOnlyList<TemplateLiteralTypePart> Parts { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Base class for template literal type parts.
/// </summary>
public abstract class TemplateLiteralTypePart : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Text portion of a template literal type.
/// </summary>
public class TemplateLiteralTypeText : TemplateLiteralTypePart
{
    /// <summary>The literal text content</summary>
    public required string Text { get; init; }
}

/// <summary>
/// Type interpolation in a template literal type: ${T}
/// </summary>
public class TemplateLiteralTypeSpan : TemplateLiteralTypePart
{
    /// <summary>The interpolated type</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// TypeScript type predicate: x is string
/// </summary>
public class TypePredicateReference : TypeReference
{
    /// <summary>The parameter name being narrowed</summary>
    public required string ParameterName { get; init; }

    /// <summary>Whether this is 'this is Type'</summary>
    public bool IsThis { get; init; }

    /// <summary>The predicate type</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Whether this is an assertion: asserts x is string</summary>
    public bool Asserts { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// TypeScript readonly type modifier: readonly T[]
/// </summary>
public class ReadonlyTypeReference : TypeReference
{
    /// <summary>The underlying type</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// TypeScript import type: import("module").Type
/// </summary>
public class ImportTypeReference : TypeReference
{
    /// <summary>The module path</summary>
    public required string ModulePath { get; init; }

    /// <summary>The qualifier (e.g., "Type" in import("module").Type)</summary>
    public string? Qualifier { get; init; }

    /// <summary>Type arguments if generic</summary>
    public IReadOnlyList<TypeReference> TypeArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        TypeArguments.Cast<UastNode>().ToList();
}

#endregion

#region TypeScript Expression Extensions

/// <summary>
/// TypeScript type assertion: x as Type or &lt;Type&gt;x
/// Note: Go has a TypeAssertionExpression as well, this is for TypeScript's different semantics.
/// </summary>
public class TsTypeAssertionExpression : ExpressionNode
{
    /// <summary>The expression being asserted</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The asserted type</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The assertion style (as or angle bracket)</summary>
    public TypeAssertionStyle Style { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Type];
}

/// <summary>
/// Style of TypeScript type assertion.
/// </summary>
public enum TypeAssertionStyle
{
    /// <summary>'as' syntax: value as Type</summary>
    As,
    /// <summary>Angle bracket syntax: &lt;Type&gt;value</summary>
    AngleBracket
}

/// <summary>
/// TypeScript satisfies expression: x satisfies Type
/// </summary>
public class SatisfiesExpression : ExpressionNode
{
    /// <summary>The expression being checked</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The type to satisfy</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Type];
}

/// <summary>
/// TypeScript non-null assertion: x!
/// </summary>
public class NonNullExpression : ExpressionNode
{
    /// <summary>The expression being asserted non-null</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// TypeScript decorator: @decorator or @decorator(args)
/// </summary>
public class TypeScriptDecoratorNode : UastNode
{
    /// <summary>The decorator expression (identifier or call)</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region TypeScript Declaration Extensions

/// <summary>
/// TypeScript class declaration with decorator support.
/// </summary>
public class TsClassDeclaration : TypeDeclaration
{
    /// <summary>Decorators applied to this class</summary>
    public IReadOnlyList<TypeScriptDecoratorNode> Decorators { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Decorators);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// TypeScript interface declaration.
/// </summary>
public class InterfaceDeclaration : TypeDeclaration
{
    /// <summary>Extended interfaces</summary>
    public IReadOnlyList<TypeReference> Extends { get; init; } = [];

    /// <summary>Interface members</summary>
    public IReadOnlyList<TsTypeMember> InterfaceMembers { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Extends);
        children.AddRange(InterfaceMembers);
        return children;
    }
}

/// <summary>
/// Base class for TypeScript type members (properties, methods, signatures).
/// </summary>
public abstract class TsTypeMember : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// TypeScript property signature in interface/type.
/// </summary>
public class PropertySignature : TsTypeMember
{
    /// <summary>The property name</summary>
    public required string Name { get; init; }

    /// <summary>The property type</summary>
    public TypeReference? Type { get; init; }

    /// <summary>Whether this property is optional</summary>
    public bool IsOptional { get; init; }

    /// <summary>Whether this property is readonly</summary>
    public bool IsReadonly { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Type != null ? [Type] : [];
    }
}

/// <summary>
/// TypeScript method signature in interface/type.
/// Note: Go has a MethodSignature as well, this is TypeScript-specific.
/// </summary>
public class TsMethodSignature : TsTypeMember
{
    /// <summary>The method name</summary>
    public required string Name { get; init; }

    /// <summary>Type parameters</summary>
    public IReadOnlyList<TypeScriptTypeParameterDeclaration> TypeParameters { get; init; } = [];

    /// <summary>Parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>Return type</summary>
    public TypeReference? ReturnType { get; init; }

    /// <summary>Whether this method is optional</summary>
    public bool IsOptional { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        return children;
    }
}

/// <summary>
/// TypeScript index signature: [key: string]: Type
/// </summary>
public class IndexSignature : TsTypeMember
{
    /// <summary>The parameter name (e.g., "key")</summary>
    public required string ParameterName { get; init; }

    /// <summary>The key type (e.g., string or number)</summary>
    public required TypeReference KeyType { get; init; }

    /// <summary>The value type</summary>
    public required TypeReference ValueType { get; init; }

    /// <summary>Whether this is readonly</summary>
    public bool IsReadonly { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [KeyType, ValueType];
}

/// <summary>
/// TypeScript call signature: (x: T): R
/// </summary>
public class CallSignature : TsTypeMember
{
    /// <summary>Type parameters</summary>
    public IReadOnlyList<TypeScriptTypeParameterDeclaration> TypeParameters { get; init; } = [];

    /// <summary>Parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>Return type</summary>
    public TypeReference? ReturnType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        return children;
    }
}

/// <summary>
/// TypeScript construct signature: new (x: T): R
/// </summary>
public class ConstructSignature : TsTypeMember
{
    /// <summary>Type parameters</summary>
    public IReadOnlyList<TypeScriptTypeParameterDeclaration> TypeParameters { get; init; } = [];

    /// <summary>Parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>Return type</summary>
    public TypeReference? ReturnType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        return children;
    }
}

/// <summary>
/// TypeScript type alias declaration: type Name = Type
/// </summary>
public class TypeAliasDeclaration : DeclarationNode
{
    /// <summary>Type parameters</summary>
    public IReadOnlyList<TypeScriptTypeParameterDeclaration> TypeParameters { get; init; } = [];

    /// <summary>The aliased type</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.Add(Type);
        return children;
    }
}

/// <summary>
/// TypeScript type parameter with constraint and default.
/// </summary>
public class TypeScriptTypeParameterDeclaration : UastNode
{
    /// <summary>The type parameter name</summary>
    public required string Name { get; init; }

    /// <summary>Constraint: extends T</summary>
    public TypeReference? Constraint { get; init; }

    /// <summary>Default type: = T</summary>
    public TypeReference? Default { get; init; }

    /// <summary>Whether this is a const type parameter</summary>
    public bool IsConst { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Constraint != null) children.Add(Constraint);
        if (Default != null) children.Add(Default);
        return children;
    }
}

/// <summary>
/// TypeScript enum declaration.
/// </summary>
public class EnumDeclaration : TypeDeclaration
{
    /// <summary>Whether this is a const enum</summary>
    public bool IsConst { get; init; }

    /// <summary>Enum members</summary>
    public IReadOnlyList<EnumMemberNode> EnumMembers { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(EnumMembers);
        return children;
    }
}

/// <summary>
/// TypeScript enum member.
/// </summary>
public class EnumMemberNode : UastNode
{
    /// <summary>The member name</summary>
    public required string Name { get; init; }

    /// <summary>The initializer expression (optional)</summary>
    public ExpressionNode? Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Initializer != null ? [Initializer] : [];
    }
}

/// <summary>
/// TypeScript namespace/module declaration.
/// </summary>
public class TsNamespaceDeclaration : DeclarationNode
{
    /// <summary>Whether this is a module (vs namespace)</summary>
    public bool IsModule { get; init; }

    /// <summary>Whether this is declare global</summary>
    public bool IsGlobal { get; init; }

    /// <summary>Namespace members</summary>
    public IReadOnlyList<DeclarationNode> NamespaceMembers { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(NamespaceMembers);
        return children;
    }
}

/// <summary>
/// TypeScript ambient declaration: declare ...
/// </summary>
public class TsAmbientDeclaration : DeclarationNode
{
    /// <summary>The declared entity</summary>
    public required DeclarationNode Declaration { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Declaration);
        return children;
    }
}

#endregion
