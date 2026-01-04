using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Record Types

/// <summary>
/// Represents a C# record declaration (record class or record struct).
/// </summary>
public class CsRecordDeclaration : TypeDeclaration
{
    /// <summary>Whether this is a record struct (vs record class).</summary>
    public bool IsRecordStruct { get; init; }

    /// <summary>Primary constructor parameters (positional records).</summary>
    public IReadOnlyList<ParameterNode> PrimaryConstructorParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        children.AddRange(PrimaryConstructorParameters);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Pattern Matching

/// <summary>
/// Base class for C# pattern expressions.
/// </summary>
public abstract class CsPatternExpression : ExpressionNode
{
}

/// <summary>
/// Represents a C# 'is' pattern expression: expr is Pattern
/// </summary>
public class CsIsPatternExpression : ExpressionNode
{
    /// <summary>The expression being tested.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The pattern to match against.</summary>
    public required CsPatternExpression Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Pattern];
}

/// <summary>
/// Represents a type pattern: is Type or is Type name
/// </summary>
public class CsTypePattern : CsPatternExpression
{
    /// <summary>The type to match.</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Optional designation (variable name).</summary>
    public string? Designation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Represents a declaration pattern: is Type variable
/// </summary>
public class CsDeclarationPattern : CsPatternExpression
{
    /// <summary>The type to match.</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The variable name to bind.</summary>
    public required string VariableName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Represents a constant pattern: is 42, is "hello", is null
/// </summary>
public class CsConstantPattern : CsPatternExpression
{
    /// <summary>The constant expression.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Represents a var pattern: is var x
/// </summary>
public class CsVarPattern : CsPatternExpression
{
    /// <summary>The variable name or tuple of names.</summary>
    public required string Designation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a discard pattern: _
/// </summary>
public class CsDiscardPattern : CsPatternExpression
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Represents a relational pattern: is &gt; 0, is &lt;= 100
/// </summary>
public class CsRelationalPattern : CsPatternExpression
{
    /// <summary>The relational operator (&lt;, &lt;=, &gt;, &gt;=).</summary>
    public required string Operator { get; init; }

    /// <summary>The value to compare against.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Represents a logical pattern: not, and, or patterns
/// </summary>
public class CsLogicalPattern : CsPatternExpression
{
    /// <summary>The logical operator (and, or, not).</summary>
    public required string Operator { get; init; }

    /// <summary>Left pattern (null for unary 'not').</summary>
    public CsPatternExpression? Left { get; init; }

    /// <summary>Right pattern.</summary>
    public required CsPatternExpression Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Left != null ? [Left, Right] : [Right];
    }
}

/// <summary>
/// Represents a property pattern: { Name: "value", Age: > 18 }
/// </summary>
public class CsPropertyPattern : CsPatternExpression
{
    /// <summary>Optional type to match before property patterns.</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The property sub-patterns.</summary>
    public required IReadOnlyList<CsPropertySubpattern> Subpatterns { get; init; }

    /// <summary>Optional designation (variable name).</summary>
    public string? Designation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        children.AddRange(Subpatterns);
        return children;
    }
}

/// <summary>
/// Represents a single property subpattern: Name: "value"
/// </summary>
public class CsPropertySubpattern : UastNode
{
    /// <summary>The property name.</summary>
    public required string Name { get; init; }

    /// <summary>The pattern to match against the property value.</summary>
    public required CsPatternExpression Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Represents a positional pattern: (x, y) or Type(x, y)
/// </summary>
public class CsPositionalPattern : CsPatternExpression
{
    /// <summary>Optional type for the positional pattern.</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The positional subpatterns.</summary>
    public required IReadOnlyList<CsPatternExpression> Subpatterns { get; init; }

    /// <summary>Optional designation (variable name).</summary>
    public string? Designation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        children.AddRange(Subpatterns);
        return children;
    }
}

/// <summary>
/// Represents a parenthesized pattern: (pattern)
/// </summary>
public class CsParenthesizedPattern : CsPatternExpression
{
    /// <summary>The inner pattern.</summary>
    public required CsPatternExpression Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Represents a list pattern: [1, 2, .., 5] or [first, .., last]
/// </summary>
public class CsListPattern : CsPatternExpression
{
    /// <summary>The patterns for each element.</summary>
    public required IReadOnlyList<CsPatternExpression> Patterns { get; init; }

    /// <summary>Optional designation (variable name).</summary>
    public string? Designation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Patterns.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Represents a slice pattern: .. or ..rest
/// </summary>
public class CsSlicePattern : CsPatternExpression
{
    /// <summary>Optional pattern for the sliced elements.</summary>
    public CsPatternExpression? Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Pattern != null ? [Pattern] : [];
    }
}

#endregion

#region Switch Expression

/// <summary>
/// Represents a C# switch expression: value switch { pattern => result, ... }
/// </summary>
public class CsSwitchExpression : ExpressionNode
{
    /// <summary>The governing expression.</summary>
    public required ExpressionNode GoverningExpression { get; init; }

    /// <summary>The switch arms.</summary>
    public required IReadOnlyList<CsSwitchExpressionArm> Arms { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { GoverningExpression };
        children.AddRange(Arms);
        return children;
    }
}

/// <summary>
/// Represents a single arm of a switch expression: pattern when guard => expression
/// </summary>
public class CsSwitchExpressionArm : UastNode
{
    /// <summary>The pattern to match.</summary>
    public required CsPatternExpression Pattern { get; init; }

    /// <summary>Optional when clause.</summary>
    public ExpressionNode? WhenClause { get; init; }

    /// <summary>The result expression.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        if (WhenClause != null) children.Add(WhenClause);
        children.Add(Expression);
        return children;
    }
}

#endregion

#region With Expression

/// <summary>
/// Represents a C# with expression for non-destructive mutation: record with { Property = value }
/// </summary>
public class CsWithExpression : ExpressionNode
{
    /// <summary>The expression to copy.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The property initializers.</summary>
    public required IReadOnlyList<CsPropertyInitializer> Initializers { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Expression };
        children.AddRange(Initializers);
        return children;
    }
}

/// <summary>
/// Represents a property initializer in a with expression: Property = value
/// </summary>
public class CsPropertyInitializer : UastNode
{
    /// <summary>The property name.</summary>
    public required string Name { get; init; }

    /// <summary>The value to assign.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion

#region Init-Only Properties

/// <summary>
/// Represents a C# property with init-only setter.
/// </summary>
public class CsInitOnlyProperty : DeclarationNode
{
    /// <summary>The property type.</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Whether this property is required.</summary>
    public bool IsRequired { get; init; }

    /// <summary>The property getter body (if any).</summary>
    public ExpressionNode? Getter { get; init; }

    /// <summary>The property initializer (if any).</summary>
    public ExpressionNode? Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Type);
        if (Getter != null) children.Add(Getter);
        if (Initializer != null) children.Add(Initializer);
        return children;
    }
}

#endregion

#region Primary Constructors

/// <summary>
/// Represents a class or struct with a primary constructor.
/// </summary>
public class CsPrimaryConstructorDeclaration : TypeDeclaration
{
    /// <summary>The primary constructor parameters.</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        children.AddRange(BaseTypes);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Required Members

/// <summary>
/// Represents a C# required modifier on a property or field.
/// </summary>
public class CsRequiredMemberModifier : ModifierNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Collection Expressions

/// <summary>
/// Represents a C# collection expression: [1, 2, 3] or [..existing, newItem]
/// </summary>
public class CsCollectionExpression : ExpressionNode
{
    /// <summary>The collection elements.</summary>
    public required IReadOnlyList<CsCollectionElement> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Elements.Cast<UastNode>().ToList();
    }
}

/// <summary>
/// Base class for collection expression elements.
/// </summary>
public abstract class CsCollectionElement : UastNode
{
}

/// <summary>
/// Represents a single element in a collection expression.
/// </summary>
public class CsExpressionElement : CsCollectionElement
{
    /// <summary>The element expression.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Represents a spread element in a collection expression: ..existing
/// </summary>
public class CsSpreadElement : CsCollectionElement
{
    /// <summary>The expression to spread.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region File-Scoped Namespaces

/// <summary>
/// Represents a C# file-scoped namespace declaration: namespace Foo;
/// </summary>
public class CsFileScopedNamespaceDeclaration : DeclarationNode
{
    /// <summary>The member declarations.</summary>
    public required IReadOnlyList<DeclarationNode> NamespaceMembers { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(NamespaceMembers);
        return children;
    }
}

#endregion

#region Global Using

/// <summary>
/// Represents a C# global using directive: global using System;
/// </summary>
public class CsGlobalUsingDirective : UastNode
{
    /// <summary>The namespace or type being imported.</summary>
    public required string Target { get; init; }

    /// <summary>Whether this is a static using.</summary>
    public bool IsStatic { get; init; }

    /// <summary>Optional alias for the using.</summary>
    public string? Alias { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Raw String Literals

/// <summary>
/// Represents a C# raw string literal: """content"""
/// </summary>
public class CsRawStringLiteral : ExpressionNode
{
    /// <summary>The string content.</summary>
    public required string Content { get; init; }

    /// <summary>The number of quote characters used (3 or more).</summary>
    public int QuoteCount { get; init; } = 3;

    /// <summary>Whether this is an interpolated raw string: $"""..."""</summary>
    public bool IsInterpolated { get; init; }

    /// <summary>Interpolated parts (for interpolated raw strings).</summary>
    public IReadOnlyList<InterpolatedStringPart> Parts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Parts.Cast<UastNode>().ToList();
    }
}

#endregion

#region File-Scoped Types

/// <summary>
/// Represents a C# file-scoped type declaration: file class Foo { }
/// </summary>
public class CsFileScopedTypeDeclaration : TypeDeclaration
{
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

#endregion

#region Type Alias (using directive)

/// <summary>
/// Represents a C# using alias directive: using Point = (int X, int Y);
/// </summary>
public class CsUsingAliasDirective : UastNode
{
    /// <summary>The alias name.</summary>
    public required string Alias { get; init; }

    /// <summary>The type being aliased.</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Whether this is a global using alias.</summary>
    public bool IsGlobal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

#endregion

#region Lambda Default Parameters

/// <summary>
/// Represents a lambda parameter with a default value.
/// </summary>
public class CsLambdaParameterWithDefault : ParameterNode
{
    /// <summary>The default value expression.</summary>
    public new required ExpressionNode DefaultValue { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        if (Type != null) children.Add(Type);
        children.Add(DefaultValue);
        return children;
    }
}

#endregion

#region Extended Property Patterns

/// <summary>
/// Represents an extended property pattern: { A.B.C: pattern }
/// </summary>
public class CsExtendedPropertyPattern : CsPatternExpression
{
    /// <summary>The property path (e.g., ["A", "B", "C"]).</summary>
    public required IReadOnlyList<string> PropertyPath { get; init; }

    /// <summary>The pattern to match.</summary>
    public required CsPatternExpression Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

#endregion

#region Generic Attributes

/// <summary>
/// Represents a generic attribute: [Attribute&lt;T&gt;]
/// </summary>
public class CsGenericAttribute : AttributeNode
{
    /// <summary>Type arguments for the generic attribute.</summary>
    public required IReadOnlyList<TypeReference> TypeArguments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(TypeArguments);
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region Scoped Ref

/// <summary>
/// Represents a scoped ref or scoped ref readonly parameter/local.
/// </summary>
public class CsScopedRefExpression : ExpressionNode
{
    /// <summary>The inner expression.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this is scoped ref readonly.</summary>
    public bool IsReadonly { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Nameof Extended Scope

/// <summary>
/// Represents a nameof expression with extended scope (accessing method parameters).
/// </summary>
public class CsNameofExpression : ExpressionNode
{
    /// <summary>The expression inside nameof.</summary>
    public required ExpressionNode Argument { get; init; }

    /// <summary>The resolved name (if available).</summary>
    public string? ResolvedName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Argument];
}

#endregion
