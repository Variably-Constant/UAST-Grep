using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Null Safety

/// <summary>
/// Dart nullable type: Type?
/// </summary>
public class DartNullableType : TypeReference
{
    /// <summary>The non-nullable inner type.</summary>
    public required TypeReference InnerType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [InnerType];
}

/// <summary>
/// Dart null assertion expression: expr!
/// </summary>
public class DartNullAssertionExpression : ExpressionNode
{
    /// <summary>The expression being asserted as non-null.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Dart null-aware access expression: expr?.member
/// </summary>
public class DartNullAwareAccessExpression : ExpressionNode
{
    /// <summary>The receiver expression being null-checked.</summary>
    public required ExpressionNode Receiver { get; init; }

    /// <summary>The member being accessed.</summary>
    public required string Member { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Receiver];
}

/// <summary>
/// Dart null-aware index expression: expr?[index]
/// </summary>
public class DartNullAwareIndexExpression : ExpressionNode
{
    /// <summary>The receiver expression being null-checked.</summary>
    public required ExpressionNode Receiver { get; init; }

    /// <summary>The index expression.</summary>
    public required ExpressionNode Index { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Receiver, Index];
}

/// <summary>
/// Dart if-null expression: expr ?? default
/// </summary>
public class DartIfNullExpression : ExpressionNode
{
    /// <summary>The left expression (may be null).</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right expression (default value if left is null).</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Dart if-null assignment expression: variable ??= value
/// </summary>
public class DartIfNullAssignmentExpression : ExpressionNode
{
    /// <summary>The target being assigned if null.</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>The value to assign.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Target, Value];
}

/// <summary>
/// Dart late variable modifier: late final/var
/// </summary>
public class DartLateVariable : VariableDeclaration
{
    /// <summary>Whether the variable is also final.</summary>
    public bool IsFinal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        if (Initializer != null) children.Add(Initializer);
        return children;
    }
}

/// <summary>
/// Dart required named parameter: required String name
/// </summary>
public class DartRequiredParameter : ParameterNode
{
    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

#endregion

#region Extension Methods

/// <summary>
/// Dart extension declaration: extension Name on Type { }
/// </summary>
public class DartExtensionDeclaration : DeclarationNode
{
    /// <summary>The type being extended.</summary>
    public required TypeReference OnType { get; init; }

    /// <summary>Members of the extension.</summary>
    public IReadOnlyList<DeclarationNode> ExtensionMembers { get; init; } = [];

    /// <summary>Type parameters of the extension.</summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { OnType };
        children.AddRange(TypeParameters);
        children.AddRange(ExtensionMembers);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

#endregion

#region Mixins

/// <summary>
/// Dart mixin declaration: mixin Name on Base { }
/// </summary>
public class DartMixinDeclaration : DeclarationNode
{
    /// <summary>Types that this mixin can be applied to (on clause).</summary>
    public IReadOnlyList<TypeReference> OnTypes { get; init; } = [];

    /// <summary>Interfaces that this mixin implements.</summary>
    public IReadOnlyList<TypeReference> Implements { get; init; } = [];

    /// <summary>Members of the mixin.</summary>
    public IReadOnlyList<DeclarationNode> MixinMembers { get; init; } = [];

    /// <summary>Type parameters of the mixin.</summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(OnTypes);
        children.AddRange(Implements);
        children.AddRange(TypeParameters);
        children.AddRange(MixinMembers);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Dart with clause: class A with B, C
/// </summary>
public class DartWithClause : UastNode
{
    /// <summary>The mixins being applied.</summary>
    public required IReadOnlyList<TypeReference> Mixins { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Mixins.Cast<UastNode>().ToList();
}

#endregion

#region Cascade Operator

/// <summary>
/// Dart cascade expression: object..method()..property = value
/// </summary>
public class DartCascadeExpression : ExpressionNode
{
    /// <summary>The target object for the cascade.</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>The cascaded sections.</summary>
    public required IReadOnlyList<DartCascadeSection> Sections { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Target };
        children.AddRange(Sections);
        return children;
    }
}

/// <summary>
/// Dart null-aware cascade expression: object?..method()
/// </summary>
public class DartNullAwareCascadeExpression : ExpressionNode
{
    /// <summary>The target object for the cascade.</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>The cascaded sections.</summary>
    public required IReadOnlyList<DartCascadeSection> Sections { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Target };
        children.AddRange(Sections);
        return children;
    }
}

/// <summary>
/// A section in a Dart cascade expression.
/// </summary>
public class DartCascadeSection : UastNode
{
    /// <summary>The expression in this cascade section.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Spread Operator

/// <summary>
/// Dart spread expression: [...list] or [...?nullableList]
/// </summary>
public class DartSpreadExpression : ExpressionNode
{
    /// <summary>The expression being spread.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this is a null-aware spread (...?).</summary>
    public bool IsNullAware { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Dart collection if: [if (condition) element]
/// </summary>
public class DartCollectionIfElement : ExpressionNode
{
    /// <summary>The condition.</summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>The element if condition is true.</summary>
    public required ExpressionNode ThenElement { get; init; }

    /// <summary>The element if condition is false (optional).</summary>
    public ExpressionNode? ElseElement { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Condition, ThenElement };
        if (ElseElement != null) children.Add(ElseElement);
        return children;
    }
}

/// <summary>
/// Dart collection for: [for (var i in list) i * 2]
/// </summary>
public class DartCollectionForElement : ExpressionNode
{
    /// <summary>The loop variable.</summary>
    public required string VariableName { get; init; }

    /// <summary>The iterable expression.</summary>
    public required ExpressionNode Iterable { get; init; }

    /// <summary>The element expression to generate.</summary>
    public required ExpressionNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Iterable, Body];
}

#endregion

#region Constructors

/// <summary>
/// Dart factory constructor: factory ClassName.name() { }
/// </summary>
public class DartFactoryConstructor : FunctionDeclaration
{
    /// <summary>Whether this redirects to another constructor.</summary>
    public bool IsRedirecting { get; init; }

    /// <summary>The constructor being redirected to, if any.</summary>
    public ExpressionNode? RedirectTarget { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        if (RedirectTarget != null) children.Add(RedirectTarget);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Dart named constructor: ClassName.named() { }
/// </summary>
public class DartNamedConstructor : FunctionDeclaration
{
    /// <summary>The class name.</summary>
    public required string ClassName { get; init; }

    /// <summary>The constructor name suffix.</summary>
    public required string ConstructorName { get; init; }

    /// <summary>Initializer list entries.</summary>
    public IReadOnlyList<DartInitializerListEntry> InitializerList { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        children.AddRange(InitializerList);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Dart const constructor: const ClassName()
/// </summary>
public class DartConstConstructor : FunctionDeclaration
{
    /// <summary>Initializer list entries.</summary>
    public IReadOnlyList<DartInitializerListEntry> InitializerList { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(Parameters);
        children.AddRange(InitializerList);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Dart initializer list entry: : field = value
/// </summary>
public class DartInitializerListEntry : UastNode
{
    /// <summary>The field being initialized.</summary>
    public required string FieldName { get; init; }

    /// <summary>The initialization value.</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>Whether this is a super() call.</summary>
    public bool IsSuperCall { get; init; }

    /// <summary>Whether this is an assert() in the initializer list.</summary>
    public bool IsAssert { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Dart field formal parameter: this.field
/// </summary>
public class DartFieldFormalParameter : ParameterNode
{
    /// <summary>The field name being initialized.</summary>
    public required string FieldName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

/// <summary>
/// Dart super formal parameter: super.field
/// </summary>
public class DartSuperFormalParameter : ParameterNode
{
    /// <summary>The super parameter name being forwarded.</summary>
    public required string SuperParameterName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

#endregion

#region Async/Await/Stream

/// <summary>
/// Dart await expression: await expr
/// </summary>
public class DartAwaitExpression : ExpressionNode
{
    /// <summary>The expression being awaited.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Dart async function modifier.
/// </summary>
public class DartAsyncModifier : UastNode
{
    /// <summary>Whether this is async* (generator).</summary>
    public bool IsGenerator { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart sync* generator modifier.
/// </summary>
public class DartSyncGeneratorModifier : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart yield statement: yield value;
/// </summary>
public class DartYieldStatement : StatementNode
{
    /// <summary>The value being yielded.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Dart yield* statement: yield* iterable;
/// </summary>
public class DartYieldEachStatement : StatementNode
{
    /// <summary>The iterable being yielded.</summary>
    public required ExpressionNode Iterable { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Iterable];
}

/// <summary>
/// Dart await for statement: await for (var x in stream) { }
/// </summary>
public class DartAwaitForStatement : StatementNode
{
    /// <summary>The loop variable name.</summary>
    public required string VariableName { get; init; }

    /// <summary>The variable type, if specified.</summary>
    public TypeReference? VariableType { get; init; }

    /// <summary>The stream expression.</summary>
    public required ExpressionNode Stream { get; init; }

    /// <summary>The loop body.</summary>
    public required StatementNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (VariableType != null) children.Add(VariableType);
        children.Add(Stream);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Pattern Matching (Dart 3)

/// <summary>
/// Dart switch expression (Dart 3): value switch { pattern => result }
/// </summary>
public class DartSwitchExpression : ExpressionNode
{
    /// <summary>The value being switched on.</summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>The switch expression cases.</summary>
    public required IReadOnlyList<DartSwitchExpressionCase> Cases { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(Cases);
        return children;
    }
}

/// <summary>
/// Dart switch expression case: pattern => expression
/// </summary>
public class DartSwitchExpressionCase : UastNode
{
    /// <summary>The pattern to match.</summary>
    public required DartPattern Pattern { get; init; }

    /// <summary>Optional guard expression (when clause).</summary>
    public ExpressionNode? Guard { get; init; }

    /// <summary>The result expression.</summary>
    public required ExpressionNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        if (Guard != null) children.Add(Guard);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Base class for Dart patterns (Dart 3).
/// </summary>
public abstract class DartPattern : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart constant pattern: 42, 'hello', const Point(0, 0)
/// </summary>
public class DartConstantPattern : DartPattern
{
    /// <summary>The constant expression.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Dart variable pattern: var x, final int y
/// </summary>
public class DartVariablePattern : DartPattern
{
    /// <summary>The variable name.</summary>
    public required string Name { get; init; }

    /// <summary>The type annotation, if any.</summary>
    public TypeReference? Type { get; init; }

    /// <summary>Whether this is a final binding.</summary>
    public bool IsFinal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Type != null ? [Type] : [];
    }
}

/// <summary>
/// Dart wildcard pattern: _
/// </summary>
public class DartWildcardPattern : DartPattern
{
    /// <summary>The type annotation, if any.</summary>
    public TypeReference? Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Type != null ? [Type] : [];
    }
}

/// <summary>
/// Dart list pattern: [a, b, ...rest]
/// </summary>
public class DartListPattern : DartPattern
{
    /// <summary>The element patterns.</summary>
    public required IReadOnlyList<DartPattern> Elements { get; init; }

    /// <summary>The rest pattern variable name, if any.</summary>
    public string? RestPattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Dart map pattern: {'key': value, ...}
/// </summary>
public class DartMapPattern : DartPattern
{
    /// <summary>The map entry patterns.</summary>
    public required IReadOnlyList<DartMapPatternEntry> Entries { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Entries.Cast<UastNode>().ToList();
}

/// <summary>
/// Dart map pattern entry: 'key': pattern
/// </summary>
public class DartMapPatternEntry : UastNode
{
    /// <summary>The key expression.</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The value pattern.</summary>
    public required DartPattern Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Value];
}

/// <summary>
/// Dart record pattern: (a, b: c)
/// </summary>
public class DartRecordPattern : DartPattern
{
    /// <summary>The positional field patterns.</summary>
    public IReadOnlyList<DartPattern> PositionalFields { get; init; } = [];

    /// <summary>The named field patterns.</summary>
    public IReadOnlyList<DartNamedPatternField> NamedFields { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(PositionalFields);
        children.AddRange(NamedFields);
        return children;
    }
}

/// <summary>
/// Dart named pattern field: name: pattern
/// </summary>
public class DartNamedPatternField : UastNode
{
    /// <summary>The field name.</summary>
    public required string Name { get; init; }

    /// <summary>The pattern for this field.</summary>
    public required DartPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Dart object pattern: ClassName(field: pattern, ...)
/// </summary>
public class DartObjectPattern : DartPattern
{
    /// <summary>The type being matched.</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The field patterns.</summary>
    public required IReadOnlyList<DartNamedPatternField> Fields { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Fields);
        return children;
    }
}

/// <summary>
/// Dart logical-or pattern: pattern1 || pattern2
/// </summary>
public class DartLogicalOrPattern : DartPattern
{
    /// <summary>The left pattern.</summary>
    public required DartPattern Left { get; init; }

    /// <summary>The right pattern.</summary>
    public required DartPattern Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Dart logical-and pattern: pattern1 && pattern2
/// </summary>
public class DartLogicalAndPattern : DartPattern
{
    /// <summary>The left pattern.</summary>
    public required DartPattern Left { get; init; }

    /// <summary>The right pattern.</summary>
    public required DartPattern Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Dart relational pattern: &gt; 5, == 0, != null
/// </summary>
public class DartRelationalPattern : DartPattern
{
    /// <summary>The relational operator.</summary>
    public required string Operator { get; init; }

    /// <summary>The operand expression.</summary>
    public required ExpressionNode Operand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// Dart cast pattern: pattern as Type
/// </summary>
public class DartCastPattern : DartPattern
{
    /// <summary>The pattern being cast.</summary>
    public required DartPattern Pattern { get; init; }

    /// <summary>The target type.</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Type];
}

/// <summary>
/// Dart null-check pattern: pattern?
/// </summary>
public class DartNullCheckPattern : DartPattern
{
    /// <summary>The pattern being null-checked.</summary>
    public required DartPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Dart null-assert pattern: pattern!
/// </summary>
public class DartNullAssertPattern : DartPattern
{
    /// <summary>The pattern being null-asserted.</summary>
    public required DartPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Dart if-case statement: if (expr case pattern) { }
/// </summary>
public class DartIfCaseStatement : StatementNode
{
    /// <summary>The expression being matched.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The pattern to match.</summary>
    public required DartPattern Pattern { get; init; }

    /// <summary>Optional guard expression.</summary>
    public ExpressionNode? Guard { get; init; }

    /// <summary>The then branch.</summary>
    public required StatementNode ThenBranch { get; init; }

    /// <summary>The else branch, if any.</summary>
    public StatementNode? ElseBranch { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Expression, Pattern };
        if (Guard != null) children.Add(Guard);
        children.Add(ThenBranch);
        if (ElseBranch != null) children.Add(ElseBranch);
        return children;
    }
}

#endregion

#region Records (Dart 3)

/// <summary>
/// Dart record type: (int, String, {String name})
/// </summary>
public class DartRecordType : TypeReference
{
    /// <summary>Positional field types.</summary>
    public IReadOnlyList<TypeReference> PositionalTypes { get; init; } = [];

    /// <summary>Named field types.</summary>
    public IReadOnlyList<DartNamedRecordField> NamedTypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(PositionalTypes);
        children.AddRange(NamedTypes);
        return children;
    }
}

/// <summary>
/// Dart named record field: {String name}
/// </summary>
public class DartNamedRecordField : UastNode
{
    /// <summary>The field name.</summary>
    public required string Name { get; init; }

    /// <summary>The field type.</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Dart record expression: (1, 'hello', name: 'world')
/// </summary>
public class DartRecordExpression : ExpressionNode
{
    /// <summary>Positional field values.</summary>
    public IReadOnlyList<ExpressionNode> PositionalFields { get; init; } = [];

    /// <summary>Named field values.</summary>
    public IReadOnlyList<DartNamedRecordFieldValue> NamedFields { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(PositionalFields);
        children.AddRange(NamedFields);
        return children;
    }
}

/// <summary>
/// Dart named record field value: name: value
/// </summary>
public class DartNamedRecordFieldValue : UastNode
{
    /// <summary>The field name.</summary>
    public required string Name { get; init; }

    /// <summary>The field value.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion

#region Sealed Classes (Dart 3)

/// <summary>
/// Dart sealed class declaration: sealed class Name { }
/// </summary>
public class DartSealedClassDeclaration : TypeDeclaration
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

/// <summary>
/// Dart final class declaration: final class Name { }
/// </summary>
public class DartFinalClassDeclaration : TypeDeclaration
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

/// <summary>
/// Dart base class declaration: base class Name { }
/// </summary>
public class DartBaseClassDeclaration : TypeDeclaration
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

/// <summary>
/// Dart interface class declaration: interface class Name { }
/// </summary>
public class DartInterfaceClassDeclaration : TypeDeclaration
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

#region Other Dart Constructs

/// <summary>
/// Dart part directive: part 'file.dart';
/// </summary>
public class DartPartDirective : StatementNode
{
    /// <summary>The part URI.</summary>
    public required string Uri { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart part of directive: part of library;
/// </summary>
public class DartPartOfDirective : StatementNode
{
    /// <summary>The library name or URI.</summary>
    public required string Library { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart show combinator: import 'lib.dart' show Class1, Class2;
/// </summary>
public class DartShowCombinator : UastNode
{
    /// <summary>The names being shown.</summary>
    public required IReadOnlyList<string> Names { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart hide combinator: import 'lib.dart' hide Class1, Class2;
/// </summary>
public class DartHideCombinator : UastNode
{
    /// <summary>The names being hidden.</summary>
    public required IReadOnlyList<string> Names { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart assert statement: assert(condition, message);
/// </summary>
public class DartAssertStatement : StatementNode
{
    /// <summary>The assertion condition.</summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>The optional message.</summary>
    public ExpressionNode? Message { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Condition };
        if (Message != null) children.Add(Message);
        return children;
    }
}

/// <summary>
/// Dart rethrow statement: rethrow;
/// </summary>
public class DartRethrowStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart symbol literal: #symbolName
/// </summary>
public class DartSymbolLiteral : ExpressionNode
{
    /// <summary>The symbol name.</summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Dart string interpolation: 'Hello $name' or 'Value: ${expr}'
/// </summary>
public class DartStringInterpolation : ExpressionNode
{
    /// <summary>The parts of the interpolated string.</summary>
    public required IReadOnlyList<DartStringInterpolationPart> Parts { get; init; }

    /// <summary>Whether this is a raw string (r prefix).</summary>
    public bool IsRaw { get; init; }

    /// <summary>Whether this is a multi-line string (triple quotes).</summary>
    public bool IsMultiLine { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Base class for Dart string interpolation parts.
/// </summary>
public abstract class DartStringInterpolationPart : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Literal text part in Dart string interpolation.
/// </summary>
public class DartStringLiteralPart : DartStringInterpolationPart
{
    /// <summary>The literal text.</summary>
    public required string Text { get; init; }
}

/// <summary>
/// Expression part in Dart string interpolation: $var or ${expr}
/// </summary>
public class DartStringExpressionPart : DartStringInterpolationPart
{
    /// <summary>The interpolated expression.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this uses braces: ${expr} vs $var</summary>
    public bool IsBraced { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Dart labeled statement: label: statement
/// </summary>
public class DartLabeledStatement : StatementNode
{
    /// <summary>The label name.</summary>
    public required string Label { get; init; }

    /// <summary>The labeled statement.</summary>
    public required StatementNode Statement { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Statement];
}

/// <summary>
/// Dart getter declaration: T get name => value;
/// </summary>
public class DartGetterDeclaration : DeclarationNode
{
    /// <summary>The return type.</summary>
    public TypeReference? ReturnType { get; init; }

    /// <summary>The getter body.</summary>
    public UastNode? Body { get; init; }

    /// <summary>Whether this is an external getter.</summary>
    public bool IsExternal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (ReturnType != null) children.Add(ReturnType);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Dart setter declaration: set name(T value) { }
/// </summary>
public class DartSetterDeclaration : DeclarationNode
{
    /// <summary>The parameter.</summary>
    public required ParameterNode Parameter { get; init; }

    /// <summary>The setter body.</summary>
    public UastNode? Body { get; init; }

    /// <summary>Whether this is an external setter.</summary>
    public bool IsExternal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Parameter);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Dart operator declaration: T operator +(T other) { }
/// </summary>
public class DartOperatorDeclaration : FunctionDeclaration
{
    /// <summary>The operator being overloaded.</summary>
    public required string Operator { get; init; }

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

#endregion
