using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Swift Optional Types

/// <summary>
/// Swift optional type: Type?
/// </summary>
public class OptionalType : TypeReference
{
    /// <summary>The wrapped type</summary>
    public required TypeReference WrappedType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [WrappedType];
}

/// <summary>
/// Swift implicitly unwrapped optional type: Type!
/// </summary>
public class ImplicitlyUnwrappedOptionalType : TypeReference
{
    /// <summary>The wrapped type</summary>
    public required TypeReference WrappedType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [WrappedType];
}

/// <summary>
/// Swift optional chaining expression: expr?.member
/// </summary>
public class OptionalChainingExpression : ExpressionNode
{
    /// <summary>The base expression being chained</summary>
    public required ExpressionNode Base { get; init; }

    /// <summary>The member being accessed</summary>
    public required string Member { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Base];
}

/// <summary>
/// Swift force unwrap expression: expr!
/// </summary>
public class ForceUnwrapExpression : ExpressionNode
{
    /// <summary>The expression being force unwrapped</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Swift nil coalescing expression: expr ?? default
/// </summary>
public class NilCoalescingExpression : ExpressionNode
{
    /// <summary>The optional expression (left side)</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The default value (right side)</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Swift optional binding condition: if let/var x = expr or guard let/var x = expr
/// </summary>
public class OptionalBindingCondition : UastNode
{
    /// <summary>Whether this is a 'let' (true) or 'var' (false) binding</summary>
    public bool IsLet { get; init; } = true;

    /// <summary>The pattern for the binding (usually an identifier)</summary>
    public required string Pattern { get; init; }

    /// <summary>The initializer expression</summary>
    public required ExpressionNode Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Initializer];
}

#endregion

#region Swift Guard & Control Flow

/// <summary>
/// Swift guard statement: guard condition else { }
/// </summary>
public class GuardStatement : StatementNode
{
    /// <summary>The conditions that must be true</summary>
    public required IReadOnlyList<UastNode> Conditions { get; init; }

    /// <summary>The else body (executed when conditions fail)</summary>
    public required BlockNode ElseBody { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Conditions);
        children.Add(ElseBody);
        return children;
    }
}

/// <summary>
/// Swift defer statement: defer { }
/// </summary>
public class DeferStatement : StatementNode
{
    /// <summary>The body to execute when scope exits</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Swift repeat-while statement: repeat { } while condition
/// </summary>
public class RepeatWhileStatement : StatementNode
{
    /// <summary>The body to execute</summary>
    public required StatementNode Body { get; init; }

    /// <summary>The condition to check after each iteration</summary>
    public required ExpressionNode Condition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body, Condition];
}

/// <summary>
/// Swift fallthrough statement: fallthrough (in switch cases)
/// </summary>
public class FallthroughStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Swift Extensions & Protocols

/// <summary>
/// Swift extension declaration: extension Type: Protocol { }
/// </summary>
public class ExtensionDeclaration : DeclarationNode
{
    /// <summary>The type being extended</summary>
    public required TypeReference ExtendedType { get; init; }

    /// <summary>Protocols this extension conforms to</summary>
    public IReadOnlyList<TypeReference> Conformances { get; init; } = [];

    /// <summary>Members of the extension</summary>
    public IReadOnlyList<DeclarationNode> ExtensionMembers { get; init; } = [];

    /// <summary>Optional where clause constraints</summary>
    public SwiftWhereClause? WhereClause { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { ExtendedType };
        children.AddRange(Conformances);
        children.AddRange(ExtensionMembers);
        if (WhereClause != null) children.Add(WhereClause);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Swift protocol declaration: protocol Name { }
/// </summary>
public class ProtocolDeclaration : DeclarationNode
{
    /// <summary>Inherited protocols</summary>
    public IReadOnlyList<TypeReference> Conformances { get; init; } = [];

    /// <summary>Protocol members (requirements)</summary>
    public IReadOnlyList<DeclarationNode> ProtocolMembers { get; init; } = [];

    /// <summary>Associated types defined in the protocol</summary>
    public IReadOnlyList<AssociatedTypeDeclaration> AssociatedTypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Conformances);
        children.AddRange(ProtocolMembers);
        children.AddRange(AssociatedTypes);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Swift associated type declaration: associatedtype Name
/// </summary>
public class AssociatedTypeDeclaration : DeclarationNode
{
    /// <summary>Type constraint (e.g., : Equatable)</summary>
    public TypeReference? Constraint { get; init; }

    /// <summary>Default type</summary>
    public TypeReference? Default { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Constraint != null) children.Add(Constraint);
        if (Default != null) children.Add(Default);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Swift protocol composition: Protocol1 &amp; Protocol2
/// </summary>
public class ProtocolComposition : TypeReference
{
    /// <summary>The protocols being composed</summary>
    public required IReadOnlyList<TypeReference> Protocols { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Protocols.Cast<UastNode>().ToList();
}

/// <summary>
/// Swift where clause for generic constraints.
/// </summary>
public class SwiftWhereClause : UastNode
{
    /// <summary>The constraints in the where clause</summary>
    public required IReadOnlyList<SwiftConstraint> Constraints { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Constraints.Cast<UastNode>().ToList();
}

/// <summary>
/// A single constraint in a Swift where clause.
/// </summary>
public class SwiftConstraint : UastNode
{
    /// <summary>The constrained type</summary>
    public required TypeReference ConstrainedType { get; init; }

    /// <summary>The constraint type (conformance or same-type)</summary>
    public required SwiftConstraintKind Kind { get; init; }

    /// <summary>The constraining type</summary>
    public required TypeReference ConstrainingType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [ConstrainedType, ConstrainingType];
}

/// <summary>
/// Kind of Swift generic constraint.
/// </summary>
public enum SwiftConstraintKind
{
    /// <summary>Conformance constraint: T: Protocol</summary>
    Conformance,
    /// <summary>Same-type constraint: T == U</summary>
    SameType
}

#endregion

#region Swift Property Wrappers & Attributes

/// <summary>
/// Swift property wrapper: @Wrapper var x
/// </summary>
public class PropertyWrapper : UastNode
{
    /// <summary>The wrapper type (e.g., State, Binding)</summary>
    public required TypeReference WrapperType { get; init; }

    /// <summary>The wrapped property declaration</summary>
    public required VariableDeclaration WrappedProperty { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [WrapperType, WrappedProperty];
}

/// <summary>
/// Swift attribute: @attribute or @attribute(args)
/// </summary>
public class SwiftAttribute : AttributeNode
{
    // Inherits Name and Arguments from AttributeNode
    // SwiftAttribute extends the base AttributeNode for Swift-specific features

    /// <summary>Optional additional Swift-specific arguments (expressions)</summary>
    public IReadOnlyList<ExpressionNode> ExpressionArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Arguments.Cast<UastNode>());
        children.AddRange(ExpressionArguments.Cast<UastNode>());
        return children;
    }
}

/// <summary>
/// Swift availability condition: #available(iOS 15, *)
/// </summary>
public class AvailabilityCondition : UastNode
{
    /// <summary>The platform requirements</summary>
    public required IReadOnlyList<PlatformRequirement> Platforms { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Platforms.Cast<UastNode>().ToList();
}

/// <summary>
/// A platform requirement in an availability condition.
/// </summary>
public class PlatformRequirement : UastNode
{
    /// <summary>The platform name (iOS, macOS, watchOS, tvOS, etc.) or * for "all others"</summary>
    public required string Platform { get; init; }

    /// <summary>The minimum version (null for wildcard *)</summary>
    public string? Version { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Swift Closures

/// <summary>
/// Swift closure: { params in body } or { $0, $1 in body }
/// </summary>
public class SwiftClosure : ExpressionNode
{
    /// <summary>The closure parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The capture list (e.g., [weak self, x])</summary>
    public IReadOnlyList<CaptureListItem> CaptureList { get; init; } = [];

    /// <summary>The closure body</summary>
    public required UastNode Body { get; init; }

    /// <summary>Whether the closure is async</summary>
    public bool IsAsync { get; init; }

    /// <summary>Whether the closure throws</summary>
    public bool Throws { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.AddRange(CaptureList);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// An item in a Swift closure capture list: [weak self, x]
/// </summary>
public class CaptureListItem : UastNode
{
    /// <summary>The capture specifier (weak, unowned, etc.)</summary>
    public CaptureSpecifier Specifier { get; init; } = CaptureSpecifier.None;

    /// <summary>The captured expression</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Capture specifier for Swift closure capture lists.
/// </summary>
public enum CaptureSpecifier
{
    /// <summary>No specifier (strong capture)</summary>
    None,
    /// <summary>Weak reference: [weak self]</summary>
    Weak,
    /// <summary>Unowned reference: [unowned self]</summary>
    Unowned,
    /// <summary>Unowned safe reference: [unowned(safe) self]</summary>
    UnownedSafe,
    /// <summary>Unowned unsafe reference: [unowned(unsafe) self]</summary>
    UnownedUnsafe
}

#endregion

#region Swift Pattern Matching

/// <summary>
/// Base class for Swift patterns.
/// </summary>
public abstract class SwiftPattern : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Swift 'is' pattern: case is Type
/// </summary>
public class IsPattern : SwiftPattern
{
    /// <summary>The type to check against</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Swift 'as' pattern: case let x as Type
/// </summary>
public class AsPattern : SwiftPattern
{
    /// <summary>The pattern to match</summary>
    public required SwiftPattern SubPattern { get; init; }

    /// <summary>The type to cast to</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [SubPattern, Type];
}

/// <summary>
/// Swift expression pattern: case .enumCase or case value
/// </summary>
public class ExpressionPattern : SwiftPattern
{
    /// <summary>The expression to match</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Swift value binding pattern: let/var pattern
/// </summary>
public class ValueBindingPattern : SwiftPattern
{
    /// <summary>Whether this is a 'let' (true) or 'var' (false) binding</summary>
    public bool IsLet { get; init; } = true;

    /// <summary>The nested pattern</summary>
    public required SwiftPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Swift identifier pattern: a simple name binding.
/// </summary>
public class IdentifierPattern : SwiftPattern
{
    /// <summary>The identifier name</summary>
    public required string Name { get; init; }

    /// <summary>Optional type annotation</summary>
    public TypeReference? Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Type != null ? [Type] : [];
    }
}

/// <summary>
/// Swift tuple pattern: (pattern1, pattern2)
/// </summary>
public class SwiftTuplePattern : SwiftPattern
{
    /// <summary>The element patterns</summary>
    public required IReadOnlyList<SwiftPattern> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Swift wildcard pattern: _
/// </summary>
public class WildcardPattern : SwiftPattern
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Swift enum case pattern: .case(associated)
/// </summary>
public class EnumCasePattern : SwiftPattern
{
    /// <summary>The enum type (optional, can be inferred)</summary>
    public TypeReference? EnumType { get; init; }

    /// <summary>The case name</summary>
    public required string CaseName { get; init; }

    /// <summary>Associated value patterns</summary>
    public IReadOnlyList<SwiftPattern> AssociatedValuePatterns { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (EnumType != null) children.Add(EnumType);
        children.AddRange(AssociatedValuePatterns);
        return children;
    }
}

#endregion

#region Swift Try Expressions

/// <summary>
/// Swift try expression: try/try?/try! expr
/// </summary>
public class TryExpression : ExpressionNode
{
    /// <summary>The expression being tried</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The kind of try (normal, optional, or forced)</summary>
    public TryKind Kind { get; init; } = TryKind.Try;

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Kind of Swift try expression.
/// </summary>
public enum TryKind
{
    /// <summary>Normal try: try expr</summary>
    Try,
    /// <summary>Optional try: try? expr</summary>
    OptionalTry,
    /// <summary>Forced try: try! expr</summary>
    ForcedTry
}

/// <summary>
/// Swift throw expression: throw error
/// </summary>
public class ThrowExpression : ExpressionNode
{
    /// <summary>The error being thrown</summary>
    public required ExpressionNode Error { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Error];
}

#endregion

#region Swift Type Declarations

/// <summary>
/// Swift typealias declaration: typealias Name = Type
/// </summary>
public class SwiftTypealiasDeclaration : DeclarationNode
{
    /// <summary>The aliased type</summary>
    public required TypeReference AliasedType { get; init; }

    /// <summary>Generic parameters</summary>
    public IReadOnlyList<SwiftGenericParameter> GenericParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { AliasedType };
        children.AddRange(GenericParameters);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Swift generic parameter declaration.
/// </summary>
public class SwiftGenericParameter : UastNode
{
    /// <summary>The type parameter name</summary>
    public required string Name { get; init; }

    /// <summary>Inherited types/protocols (: SomeProtocol)</summary>
    public IReadOnlyList<TypeReference> InheritedTypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() => InheritedTypes.Cast<UastNode>().ToList();
}

#endregion

#region Swift Property Observers

/// <summary>
/// Swift property observers: willSet and/or didSet
/// </summary>
public class WillSetDidSet : UastNode
{
    /// <summary>The willSet body (optional)</summary>
    public BlockNode? WillSetBody { get; init; }

    /// <summary>The parameter name for willSet (defaults to newValue)</summary>
    public string WillSetParameterName { get; init; } = "newValue";

    /// <summary>The didSet body (optional)</summary>
    public BlockNode? DidSetBody { get; init; }

    /// <summary>The parameter name for didSet (defaults to oldValue)</summary>
    public string DidSetParameterName { get; init; } = "oldValue";

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (WillSetBody != null) children.Add(WillSetBody);
        if (DidSetBody != null) children.Add(DidSetBody);
        return children;
    }
}

/// <summary>
/// Swift computed property: get/set property
/// </summary>
public class ComputedProperty : DeclarationNode
{
    /// <summary>The property type</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The getter body</summary>
    public required BlockNode Getter { get; init; }

    /// <summary>The setter body (optional, makes property read-only if absent)</summary>
    public BlockNode? Setter { get; init; }

    /// <summary>The parameter name for setter (defaults to newValue)</summary>
    public string SetterParameterName { get; init; } = "newValue";

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Getter };
        if (Type != null) children.Add(Type);
        if (Setter != null) children.Add(Setter);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Swift subscript declaration: subscript(params) -> Type { get set }
/// </summary>
public class SubscriptDeclaration : DeclarationNode
{
    /// <summary>The subscript parameters</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    /// <summary>The return type</summary>
    public required TypeReference ReturnType { get; init; }

    /// <summary>The getter body</summary>
    public BlockNode? Getter { get; init; }

    /// <summary>The setter body</summary>
    public BlockNode? Setter { get; init; }

    /// <summary>Generic parameters</summary>
    public IReadOnlyList<SwiftGenericParameter> GenericParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.Add(ReturnType);
        if (Getter != null) children.Add(Getter);
        if (Setter != null) children.Add(Setter);
        children.AddRange(GenericParameters);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

#endregion

#region Swift Other Expressions

/// <summary>
/// Swift inout expression: &amp;variable
/// </summary>
public class InoutExpression : ExpressionNode
{
    /// <summary>The expression being passed by reference</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Swift key path expression: \Type.path
/// </summary>
public class KeyPathExpression : ExpressionNode
{
    /// <summary>The root type (optional)</summary>
    public TypeReference? Root { get; init; }

    /// <summary>The path components</summary>
    public required IReadOnlyList<KeyPathComponent> Components { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Root != null) children.Add(Root);
        children.AddRange(Components);
        return children;
    }
}

/// <summary>
/// A component in a Swift key path.
/// </summary>
public class KeyPathComponent : UastNode
{
    /// <summary>The component kind</summary>
    public required KeyPathComponentKind Kind { get; init; }

    /// <summary>The component name (for property access)</summary>
    public string? Name { get; init; }

    /// <summary>The subscript arguments (for subscript access)</summary>
    public IReadOnlyList<ExpressionNode> SubscriptArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() => SubscriptArguments.Cast<UastNode>().ToList();
}

/// <summary>
/// Kind of key path component.
/// </summary>
public enum KeyPathComponentKind
{
    /// <summary>Property access: .propertyName</summary>
    Property,
    /// <summary>Subscript access: [index]</summary>
    Subscript,
    /// <summary>Optional chaining: ?</summary>
    OptionalChaining,
    /// <summary>Force unwrap: !</summary>
    ForceUnwrap,
    /// <summary>Self: .self</summary>
    Self
}

/// <summary>
/// Swift selector expression: #selector(method)
/// </summary>
public class SelectorExpression : ExpressionNode
{
    /// <summary>The expression referencing the method</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this is a getter selector: #selector(getter:)</summary>
    public bool IsGetter { get; init; }

    /// <summary>Whether this is a setter selector: #selector(setter:)</summary>
    public bool IsSetter { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Swift string interpolation expression.
/// </summary>
public class SwiftStringInterpolation : ExpressionNode
{
    /// <summary>The segments of the string</summary>
    public required IReadOnlyList<StringInterpolationSegment> Segments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => Segments.Cast<UastNode>().ToList();
}

/// <summary>
/// A segment in a Swift string interpolation.
/// </summary>
public abstract class StringInterpolationSegment : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Literal text segment in string interpolation.
/// </summary>
public class StringLiteralSegment : StringInterpolationSegment
{
    /// <summary>The literal text</summary>
    public required string Text { get; init; }
}

/// <summary>
/// Interpolated expression segment: \(expression)
/// </summary>
public class InterpolatedExpressionSegment : StringInterpolationSegment
{
    /// <summary>The interpolated expression</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Swift Result Builder

/// <summary>
/// Swift result builder attribute: @resultBuilder
/// </summary>
public class ResultBuilderDeclaration : DeclarationNode
{
    /// <summary>The struct/class declaration that is the result builder</summary>
    public required TypeDeclaration BuilderType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { BuilderType };
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

#endregion

#region Swift Async/Await

/// <summary>
/// Swift await expression: await expr
/// </summary>
public class SwiftAwaitExpression : ExpressionNode
{
    /// <summary>The expression being awaited</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Swift async let binding: async let x = expr
/// </summary>
public class AsyncLetBinding : StatementNode
{
    /// <summary>The variable name</summary>
    public required string Name { get; init; }

    /// <summary>The type annotation (optional)</summary>
    public TypeReference? Type { get; init; }

    /// <summary>The initializer expression</summary>
    public required ExpressionNode Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Initializer };
        if (Type != null) children.Add(Type);
        return children;
    }
}

/// <summary>
/// Swift Task group: withTaskGroup, withThrowingTaskGroup
/// </summary>
public class TaskGroupExpression : ExpressionNode
{
    /// <summary>The child task type</summary>
    public TypeReference? ChildTaskType { get; init; }

    /// <summary>The result type</summary>
    public TypeReference? ResultType { get; init; }

    /// <summary>The body closure</summary>
    public required ExpressionNode Body { get; init; }

    /// <summary>Whether this is a throwing task group</summary>
    public bool IsThrowing { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (ChildTaskType != null) children.Add(ChildTaskType);
        if (ResultType != null) children.Add(ResultType);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Swift Actors

/// <summary>
/// Swift actor declaration: actor Name { }
/// </summary>
public class ActorDeclaration : DeclarationNode
{
    /// <summary>Inherited types</summary>
    public IReadOnlyList<TypeReference> InheritedTypes { get; init; } = [];

    /// <summary>Actor members</summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    /// <summary>Generic parameters</summary>
    public IReadOnlyList<SwiftGenericParameter> GenericParameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(InheritedTypes);
        children.AddRange(Members);
        children.AddRange(GenericParameters);
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Swift isolated parameter: isolated actor
/// </summary>
public class IsolatedParameter : UastNode
{
    /// <summary>The actor expression</summary>
    public required ExpressionNode Actor { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Actor];
}

/// <summary>
/// Swift nonisolated modifier.
/// </summary>
public class NonisolatedModifier : UastNode
{
    /// <summary>Whether this is nonisolated(unsafe)</summary>
    public bool IsUnsafe { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion
