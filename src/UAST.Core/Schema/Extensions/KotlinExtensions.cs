using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Coroutines

/// <summary>
/// Kotlin coroutine scope call: launch, async, runBlocking, withContext.
/// </summary>
public class CoroutineScopeCall : ExpressionNode
{
    /// <summary>The type of coroutine scope function being called.</summary>
    public required CoroutineScopeType ScopeType { get; init; }

    /// <summary>The lambda or block passed to the coroutine scope.</summary>
    public required UastNode Lambda { get; init; }

    /// <summary>Optional dispatcher or context argument.</summary>
    public ExpressionNode? Context { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Lambda };
        if (Context != null) children.Add(Context);
        return children;
    }
}

/// <summary>
/// Type of Kotlin coroutine scope function.
/// </summary>
public enum CoroutineScopeType
{
    /// <summary>launch { } - Fire-and-forget coroutine returning Job</summary>
    Launch,
    /// <summary>async { } - Coroutine returning Deferred&lt;T&gt;</summary>
    Async,
    /// <summary>runBlocking { } - Blocking coroutine builder</summary>
    RunBlocking,
    /// <summary>withContext(dispatcher) { } - Context switch</summary>
    WithContext
}

/// <summary>
/// Kotlin await expression: deferred.await().
/// </summary>
public class AwaitExpression : ExpressionNode
{
    /// <summary>The deferred expression being awaited.</summary>
    public required ExpressionNode Deferred { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Deferred];
}

/// <summary>
/// Kotlin suspend modifier on a function.
/// </summary>
public class SuspendModifier : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Data Classes & Special Classes

/// <summary>
/// Kotlin data class declaration: data class User(val name: String, val age: Int)
/// </summary>
public class DataClassDeclaration : TypeDeclaration
{
    /// <summary>The primary constructor with property declarations.</summary>
    public KotlinPrimaryConstructor? PrimaryConstructor { get; init; }

    /// <summary>Properties declared in the primary constructor.</summary>
    public IReadOnlyList<VariableDeclaration> DataProperties { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        if (PrimaryConstructor != null) children.Add(PrimaryConstructor);
        children.AddRange(DataProperties);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Kotlin sealed class or sealed interface declaration.
/// </summary>
public class SealedClassDeclaration : TypeDeclaration
{
    /// <summary>Whether this is a sealed interface (true) or sealed class (false).</summary>
    public bool IsInterface { get; init; }

    /// <summary>Known subclasses of this sealed type.</summary>
    public IReadOnlyList<string> Subclasses { get; init; } = [];

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
/// Kotlin object declaration (singleton): object Singleton { }
/// </summary>
public class ObjectDeclaration : DeclarationNode
{
    /// <summary>Supertypes implemented by this object.</summary>
    public IReadOnlyList<TypeReference> Supertypes { get; init; } = [];

    /// <summary>Members of the object.</summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Supertypes);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Kotlin companion object: companion object { }
/// </summary>
public class CompanionObjectDeclaration : DeclarationNode
{
    /// <summary>Members of the companion object.</summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Kotlin enum entry with optional body: enum entry NAME(args) { body }
/// </summary>
public class KotlinEnumEntry : DeclarationNode
{
    /// <summary>Constructor arguments for the enum entry.</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    /// <summary>Optional body of the enum entry.</summary>
    public BlockNode? Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Arguments);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Kotlin value class declaration: @JvmInline value class Wrapper(val value: T)
/// </summary>
public class ValueClassDeclaration : TypeDeclaration
{
    /// <summary>The single property wrapped by this value class.</summary>
    public required VariableDeclaration Property { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.Add(Property);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Scope Functions

/// <summary>
/// Kotlin scope function call: let, run, with, apply, also.
/// </summary>
public class ScopeFunctionCall : ExpressionNode
{
    /// <summary>The receiver expression (null for 'with').</summary>
    public ExpressionNode? Receiver { get; init; }

    /// <summary>The type of scope function being called.</summary>
    public required ScopeFunctionType ScopeFunction { get; init; }

    /// <summary>The lambda passed to the scope function.</summary>
    public required UastNode Lambda { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Receiver != null) children.Add(Receiver);
        children.Add(Lambda);
        return children;
    }
}

/// <summary>
/// Type of Kotlin scope function.
/// </summary>
public enum ScopeFunctionType
{
    /// <summary>let { it -> ... } - Object reference as 'it', returns lambda result</summary>
    Let,
    /// <summary>run { this... } - Object reference as 'this', returns lambda result</summary>
    Run,
    /// <summary>with(obj) { this... } - Object reference as 'this', returns lambda result</summary>
    With,
    /// <summary>apply { this... } - Object reference as 'this', returns context object</summary>
    Apply,
    /// <summary>also { it -> ... } - Object reference as 'it', returns context object</summary>
    Also
}

/// <summary>
/// Kotlin implicit 'it' parameter in lambdas.
/// </summary>
public class ItParameter : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Null Safety

/// <summary>
/// Kotlin nullable type: Type?
/// </summary>
public class KotlinNullableType : TypeReference
{
    /// <summary>The non-nullable inner type.</summary>
    public required TypeReference InnerType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [InnerType];
}

/// <summary>
/// Kotlin safe call expression: expr?.member
/// </summary>
public class SafeCallExpression : ExpressionNode
{
    /// <summary>The receiver expression being null-checked.</summary>
    public required ExpressionNode Receiver { get; init; }

    /// <summary>The member being accessed.</summary>
    public required string Member { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Receiver];
}

/// <summary>
/// Kotlin elvis expression: expr ?: default
/// </summary>
public class ElvisExpression : ExpressionNode
{
    /// <summary>The left expression (may be null).</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right expression (default value if left is null).</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Kotlin not-null assertion expression: expr!!
/// </summary>
public class NotNullAssertionExpression : ExpressionNode
{
    /// <summary>The expression being asserted as non-null.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Kotlin safe cast expression: expr as? Type
/// </summary>
public class SafeCastExpression : ExpressionNode
{
    /// <summary>The expression being cast.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The target type.</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Type];
}

#endregion

#region Properties & Delegation

/// <summary>
/// Kotlin delegated property: var x by delegate
/// </summary>
public class DelegatedProperty : VariableDeclaration
{
    /// <summary>The delegate expression.</summary>
    public required ExpressionNode Delegate { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        children.Add(Delegate);
        return children;
    }
}

/// <summary>
/// Kotlin lazy property: by lazy { initializer }
/// </summary>
public class LazyProperty : VariableDeclaration
{
    /// <summary>The lazy initializer lambda.</summary>
    public required new UastNode Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Type != null) children.Add(Type);
        children.Add(Initializer);
        return children;
    }
}

/// <summary>
/// Kotlin backing field reference in property accessors.
/// </summary>
public class BackingFieldExpression : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Kotlin lateinit property: lateinit var x: Type
/// </summary>
public class LateinitProperty : VariableDeclaration
{
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

#endregion

#region When Expression

/// <summary>
/// Kotlin when expression: when (subject) { branches }
/// </summary>
public class WhenExpression : ExpressionNode
{
    /// <summary>The subject expression (null for when without subject).</summary>
    public ExpressionNode? Subject { get; init; }

    /// <summary>The branches of the when expression.</summary>
    public required IReadOnlyList<WhenBranch> Branches { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Subject != null) children.Add(Subject);
        children.AddRange(Branches);
        return children;
    }
}

/// <summary>
/// Kotlin when branch: conditions -> body
/// </summary>
public class WhenBranch : UastNode
{
    /// <summary>The conditions for this branch (empty for else branch).</summary>
    public IReadOnlyList<WhenCondition> Conditions { get; init; } = [];

    /// <summary>The body of this branch.</summary>
    public required UastNode Body { get; init; }

    /// <summary>Whether this is the else branch.</summary>
    public bool IsElse { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Conditions);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Base class for Kotlin when conditions.
/// </summary>
public abstract class WhenCondition : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Kotlin when condition with expression: value ->
/// </summary>
public class WhenExpressionCondition : WhenCondition
{
    /// <summary>The expression to match against.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Kotlin when condition with range: in range or !in range
/// </summary>
public class WhenRangeCondition : WhenCondition
{
    /// <summary>The range expression.</summary>
    public required ExpressionNode Range { get; init; }

    /// <summary>Whether this is a negated check (!in).</summary>
    public bool IsNegated { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Range];
}

/// <summary>
/// Kotlin when condition with type check: is Type or !is Type
/// </summary>
public class WhenTypeCondition : WhenCondition
{
    /// <summary>The type to check against.</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Whether this is a negated check (!is).</summary>
    public bool IsNegated { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

#endregion

#region Constructors & Initialization

/// <summary>
/// Kotlin primary constructor: class Name(val x: T)
/// </summary>
public class KotlinPrimaryConstructor : DeclarationNode
{
    /// <summary>Parameters of the primary constructor.</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Parameters);
        return children;
    }
}

/// <summary>
/// Kotlin init block: init { }
/// Init blocks are class members that run during object initialization.
/// </summary>
public class InitBlock : DeclarationNode
{
    /// <summary>The body of the init block.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Functions

/// <summary>
/// Kotlin infix function call: a infixFun b
/// </summary>
public class InfixFunctionCall : ExpressionNode
{
    /// <summary>The left operand.</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The infix function name.</summary>
    public required string FunctionName { get; init; }

    /// <summary>The right operand.</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Kotlin operator function declaration: operator fun plus(...)
/// </summary>
public class OperatorFunction : FunctionDeclaration
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

/// <summary>
/// Kotlin extension function: fun Type.name()
/// </summary>
public class ExtensionFunction : FunctionDeclaration
{
    /// <summary>The receiver type being extended.</summary>
    public required TypeReference ReceiverType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.Add(ReceiverType);
        children.AddRange(Parameters);
        if (ReturnType != null) children.Add(ReturnType);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Kotlin inline function: inline fun
/// </summary>
public class InlineFunction : FunctionDeclaration
{
    /// <summary>Whether the function is inline.</summary>
    public bool IsInline { get; init; }

    /// <summary>Whether a parameter is crossinline.</summary>
    public bool IsCrossinline { get; init; }

    /// <summary>Whether a parameter is noinline.</summary>
    public bool IsNoinline { get; init; }

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
/// Kotlin reified type parameter: reified T
/// </summary>
public class ReifiedTypeParameter : TypeParameterNode
{
    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Constraints.Cast<UastNode>().ToList();
    }
}

#endregion

#region Destructuring

/// <summary>
/// Kotlin destructuring declaration: val (a, b) = pair
/// </summary>
public class DestructuringDeclaration : StatementNode
{
    /// <summary>The component variables being declared.</summary>
    public required IReadOnlyList<VariableDeclaration> Components { get; init; }

    /// <summary>The expression being destructured.</summary>
    public required ExpressionNode Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Components);
        children.Add(Initializer);
        return children;
    }
}

#endregion

#region Labels

/// <summary>
/// Kotlin label expression: label@ or return@label
/// </summary>
public class LabelExpression : ExpressionNode
{
    /// <summary>The label name (without @).</summary>
    public required string Label { get; init; }

    /// <summary>The expression being labeled (for label@ expressions).</summary>
    public ExpressionNode? Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Expression != null ? [Expression] : [];
    }
}

#endregion

#region String Templates

/// <summary>
/// Kotlin string template: "text $var ${expr}"
/// </summary>
public class StringTemplate : ExpressionNode
{
    /// <summary>The parts of the string template.</summary>
    public required IReadOnlyList<StringTemplatePart> Parts { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Base class for string template parts.
/// </summary>
public abstract class StringTemplatePart : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Literal text in a string template.
/// </summary>
public class StringLiteralPart : StringTemplatePart
{
    /// <summary>The literal text content.</summary>
    public required string Text { get; init; }
}

/// <summary>
/// Expression interpolation in a string template: $var or ${expr}
/// </summary>
public class StringExpressionPart : StringTemplatePart
{
    /// <summary>The interpolated expression.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether the expression uses braces: ${expr} vs $var</summary>
    public bool IsBraced { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion
