using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region PHP Trait Extensions

/// <summary>
/// PHP trait declaration: trait Name { ... }
/// Traits are a mechanism for code reuse in single inheritance languages.
/// </summary>
public class PhpTraitDeclaration : DeclarationNode
{
    /// <summary>The trait body containing methods and properties</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Other traits used by this trait (via use statement)</summary>
    public IReadOnlyList<PhpTraitUse> TraitUses { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TraitUses);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// PHP trait use statement: use TraitA, TraitB { ... }
/// </summary>
public class PhpTraitUse : DeclarationNode
{
    /// <summary>The traits being used</summary>
    public required IReadOnlyList<TypeReference> Traits { get; init; }

    /// <summary>Trait adaptations (conflict resolution, visibility changes)</summary>
    public IReadOnlyList<PhpTraitAdaptation> Adaptations { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Traits);
        children.AddRange(Adaptations);
        return children;
    }
}

/// <summary>
/// Base class for trait adaptations (insteadof, as).
/// </summary>
public abstract class PhpTraitAdaptation : UastNode
{
    /// <summary>The method being adapted</summary>
    public required string MethodName { get; init; }

    /// <summary>The trait the method comes from (optional)</summary>
    public TypeReference? TraitReference { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return TraitReference != null ? [TraitReference] : [];
    }
}

/// <summary>
/// PHP trait precedence declaration: TraitA::method insteadof TraitB
/// </summary>
public class PhpTraitPrecedence : PhpTraitAdaptation
{
    /// <summary>The traits being excluded</summary>
    public required IReadOnlyList<TypeReference> ExcludedTraits { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (TraitReference != null) children.Add(TraitReference);
        children.AddRange(ExcludedTraits);
        return children;
    }
}

/// <summary>
/// PHP trait alias declaration: method as visibility newName
/// </summary>
public class PhpTraitAlias : PhpTraitAdaptation
{
    /// <summary>The new name for the method (optional)</summary>
    public string? NewName { get; init; }

    /// <summary>The new visibility modifier (optional)</summary>
    public Visibility? NewVisibility { get; init; }
}

#endregion

#region PHP Attribute Extensions (PHP 8.0+)

/// <summary>
/// PHP attribute: #[Attribute(args)]
/// </summary>
public class PhpAttribute : UastNode
{
    /// <summary>The attribute name</summary>
    public required string Name { get; init; }

    /// <summary>Arguments passed to the attribute</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    /// <summary>Whether this uses named arguments</summary>
    public bool HasNamedArguments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arguments.Cast<UastNode>().ToList();
}

/// <summary>
/// PHP attribute group: #[Attr1, Attr2(arg)]
/// </summary>
public class PhpAttributeGroup : DeclarationNode
{
    /// <summary>The attributes in this group</summary>
    public required IReadOnlyList<PhpAttribute> Attributes { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(base.Attributes);
        children.AddRange(base.Modifiers);
        children.AddRange(Attributes);
        return children;
    }
}

#endregion

#region PHP Match Expression (PHP 8.0+)

/// <summary>
/// PHP match expression: match($expr) { pattern => result, ... }
/// Unlike switch, match is an expression and uses strict comparison.
/// </summary>
public class PhpMatchExpression : ExpressionNode
{
    /// <summary>The expression being matched</summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>The match arms</summary>
    public required IReadOnlyList<PhpMatchArm> Arms { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(Arms);
        return children;
    }
}

/// <summary>
/// PHP match arm: pattern1, pattern2 => result
/// </summary>
public class PhpMatchArm : UastNode
{
    /// <summary>The patterns (conditions) for this arm. Empty means default arm.</summary>
    public IReadOnlyList<ExpressionNode> Patterns { get; init; } = [];

    /// <summary>The result expression</summary>
    public required ExpressionNode Body { get; init; }

    /// <summary>Whether this is the default arm</summary>
    public bool IsDefault => Patterns.Count == 0;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Patterns);
        children.Add(Body);
        return children;
    }
}

#endregion

#region PHP Named Arguments (PHP 8.0+)

/// <summary>
/// PHP named argument: name: value
/// </summary>
public class PhpNamedArgument : ExpressionNode
{
    /// <summary>The argument name</summary>
    public required string Name { get; init; }

    /// <summary>The argument value</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion

#region PHP Arrow Functions (PHP 7.4+)

/// <summary>
/// PHP arrow function: fn($x) => $x * 2
/// Single-expression functions with implicit capture.
/// </summary>
public class PhpArrowFunction : ExpressionNode
{
    /// <summary>The function parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The return type (optional)</summary>
    public TypeReference? ReturnType { get; init; }

    /// <summary>The body expression (automatically returned)</summary>
    public required ExpressionNode Body { get; init; }

    /// <summary>Whether this is a static arrow function</summary>
    public bool IsStatic { get; init; }

    /// <summary>Whether this returns by reference</summary>
    public bool ReturnsByReference { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        children.Add(Body);
        return children;
    }
}

#endregion

#region PHP Nullsafe Operator (PHP 8.0+)

/// <summary>
/// PHP nullsafe property access: $obj?->property
/// </summary>
public class PhpNullsafePropertyAccess : ExpressionNode
{
    /// <summary>The object expression</summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>The property name</summary>
    public required string Property { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Object];
}

/// <summary>
/// PHP nullsafe method call: $obj?->method()
/// </summary>
public class PhpNullsafeMethodCall : ExpressionNode
{
    /// <summary>The object expression</summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>The method name</summary>
    public required string MethodName { get; init; }

    /// <summary>The method arguments</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Object };
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region PHP Enums (PHP 8.1+)

/// <summary>
/// PHP enum declaration: enum Name [: BackingType] { ... }
/// </summary>
public class PhpEnumDeclaration : DeclarationNode
{
    /// <summary>The backing type for backed enums (string or int)</summary>
    public TypeReference? BackingType { get; init; }

    /// <summary>Whether this is a backed enum</summary>
    public bool IsBacked => BackingType != null;

    /// <summary>The enum cases</summary>
    public required IReadOnlyList<PhpEnumCase> Cases { get; init; }

    /// <summary>The enum methods</summary>
    public IReadOnlyList<FunctionDeclaration> Methods { get; init; } = [];

    /// <summary>Implemented interfaces</summary>
    public IReadOnlyList<TypeReference> Implements { get; init; } = [];

    /// <summary>Traits used by this enum</summary>
    public IReadOnlyList<PhpTraitUse> TraitUses { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (BackingType != null) children.Add(BackingType);
        children.AddRange(Implements);
        children.AddRange(TraitUses);
        children.AddRange(Cases);
        children.AddRange(Methods);
        return children;
    }
}

/// <summary>
/// PHP enum case: case Name [= value]
/// </summary>
public class PhpEnumCase : UastNode
{
    /// <summary>The case name</summary>
    public required string Name { get; init; }

    /// <summary>The backing value for backed enums</summary>
    public ExpressionNode? Value { get; init; }

    /// <summary>Attributes applied to this case</summary>
    public IReadOnlyList<PhpAttributeGroup> Attributes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        if (Value != null) children.Add(Value);
        return children;
    }
}

#endregion

#region PHP Constructor Property Promotion (PHP 8.0+)

/// <summary>
/// PHP promoted constructor parameter: public int $x
/// The parameter is automatically promoted to a property.
/// </summary>
public class PhpPromotedParameter : UastNode
{
    /// <summary>The parameter/property name</summary>
    public required string Name { get; init; }

    /// <summary>The type</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The default value</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>The visibility modifier</summary>
    public Visibility Visibility { get; init; } = Visibility.Public;

    /// <summary>Whether this is readonly</summary>
    public bool IsReadonly { get; init; }

    /// <summary>Attributes applied to this parameter</summary>
    public IReadOnlyList<PhpAttributeGroup> Attributes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        if (Type != null) children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

#endregion

#region PHP Union and Intersection Types (PHP 8.0+, 8.1+)

/// <summary>
/// PHP union type: Type1|Type2
/// </summary>
public class PhpUnionType : TypeReference
{
    /// <summary>The types in the union</summary>
    public required IReadOnlyList<TypeReference> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Types.Cast<UastNode>().ToList();
}

/// <summary>
/// PHP intersection type (PHP 8.1+): Type1&amp;Type2
/// </summary>
public class PhpIntersectionType : TypeReference
{
    /// <summary>The types in the intersection</summary>
    public required IReadOnlyList<TypeReference> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Types.Cast<UastNode>().ToList();
}

/// <summary>
/// PHP Disjunctive Normal Form (DNF) type (PHP 8.2+): (A&amp;B)|C
/// </summary>
public class PhpDnfType : TypeReference
{
    /// <summary>The types in DNF form (unions of intersections)</summary>
    public required IReadOnlyList<TypeReference> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Types.Cast<UastNode>().ToList();
}

#endregion

#region PHP Readonly Properties (PHP 8.1+)

/// <summary>
/// PHP readonly property declaration: readonly int $prop
/// </summary>
public class PhpReadonlyProperty : DeclarationNode
{
    /// <summary>The property type</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The default value (for promoted properties)</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>Whether this is a static property</summary>
    public bool IsStatic { get; init; }

    /// <summary>Attributes applied to this property</summary>
    public new IReadOnlyList<PhpAttributeGroup> Attributes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

/// <summary>
/// PHP readonly class (PHP 8.2+): readonly class Name { ... }
/// All properties in a readonly class are implicitly readonly.
/// </summary>
public class PhpReadonlyClass : DeclarationNode
{
    /// <summary>The class body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Base class</summary>
    public TypeReference? Extends { get; init; }

    /// <summary>Implemented interfaces</summary>
    public IReadOnlyList<TypeReference> Implements { get; init; } = [];

    /// <summary>Traits used</summary>
    public IReadOnlyList<PhpTraitUse> TraitUses { get; init; } = [];

    /// <summary>Whether this is also final</summary>
    public bool IsFinal { get; init; }

    /// <summary>Whether this is also abstract</summary>
    public bool IsAbstract { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Extends != null) children.Add(Extends);
        children.AddRange(Implements);
        children.AddRange(TraitUses);
        children.Add(Body);
        return children;
    }
}

#endregion

#region PHP First-Class Callables (PHP 8.1+)

/// <summary>
/// PHP first-class callable syntax: $obj->method(...) or Class::method(...)
/// Creates a Closure from a callable.
/// </summary>
public class PhpFirstClassCallable : ExpressionNode
{
    /// <summary>The callable expression (method or function reference)</summary>
    public required ExpressionNode Callable { get; init; }

    /// <summary>Whether this is a static method reference</summary>
    public bool IsStatic { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Callable];
}

/// <summary>
/// PHP method reference for first-class callable: $obj->method
/// </summary>
public class PhpMethodReference : ExpressionNode
{
    /// <summary>The object expression</summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>The method name</summary>
    public required string MethodName { get; init; }

    /// <summary>Whether this is a nullsafe reference</summary>
    public bool IsNullsafe { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Object];
}

/// <summary>
/// PHP static method reference: Class::method
/// </summary>
public class PhpStaticMethodReference : ExpressionNode
{
    /// <summary>The class type</summary>
    public required TypeReference Class { get; init; }

    /// <summary>The method name</summary>
    public required string MethodName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Class];
}

#endregion

#region PHP Additional Modern Features

/// <summary>
/// PHP new in initializers (PHP 8.1+): new Class() as default parameter value
/// </summary>
public class PhpNewInitializer : ExpressionNode
{
    /// <summary>The type being instantiated</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Constructor arguments</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// PHP named argument unpacking (PHP 8.0+): func(...$args)
/// </summary>
public class PhpSpreadArgument : ExpressionNode
{
    /// <summary>The expression being spread</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this is a named spread</summary>
    public string? Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// PHP nullsafe operator in chained calls: $a?->b?->c
/// Represents a chain of nullsafe operations.
/// </summary>
public class PhpNullsafeChain : ExpressionNode
{
    /// <summary>The chain of expressions (each may be nullsafe)</summary>
    public required IReadOnlyList<ExpressionNode> Chain { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Chain.Cast<UastNode>().ToList();
}

/// <summary>
/// PHP throw expression (PHP 8.0+): $x ?? throw new Exception()
/// throw can now be used as an expression.
/// </summary>
public class PhpThrowExpression : ExpressionNode
{
    /// <summary>The exception being thrown</summary>
    public required ExpressionNode Exception { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Exception];
}

/// <summary>
/// PHP mixed type (PHP 8.0+): represents any type
/// </summary>
public class PhpMixedType : TypeReference
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// PHP never type (PHP 8.1+): function never returns
/// </summary>
public class PhpNeverType : TypeReference
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// PHP void type (PHP 7.1+): function returns nothing
/// </summary>
public class PhpVoidType : TypeReference
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// PHP false/true/null as standalone types (PHP 8.0+/8.2+)
/// </summary>
public class PhpLiteralType : TypeReference
{
    /// <summary>The literal value (false, true, or null)</summary>
    public required PhpLiteralTypeValue Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// PHP literal type values that can be used as types.
/// </summary>
public enum PhpLiteralTypeValue
{
    /// <summary>The false type</summary>
    False,
    /// <summary>The true type (PHP 8.2+)</summary>
    True,
    /// <summary>The null type</summary>
    Null
}

/// <summary>
/// PHP anonymous class: new class { ... }
/// </summary>
public class PhpAnonymousClass : ExpressionNode
{
    /// <summary>Constructor arguments</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    /// <summary>Base class</summary>
    public TypeReference? Extends { get; init; }

    /// <summary>Implemented interfaces</summary>
    public IReadOnlyList<TypeReference> Implements { get; init; } = [];

    /// <summary>The class body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Arguments);
        if (Extends != null) children.Add(Extends);
        children.AddRange(Implements);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// PHP constant expression in class/enum (PHP 8.1+): const CONST = expr
/// </summary>
public class PhpClassConstant : DeclarationNode
{
    /// <summary>The constant type (PHP 8.3+)</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The constant value</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>Whether this is a final constant</summary>
    public bool IsFinal { get; init; }

    /// <summary>Attributes applied to this constant</summary>
    public new IReadOnlyList<PhpAttributeGroup> Attributes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        children.Add(Value);
        return children;
    }
}

#endregion
