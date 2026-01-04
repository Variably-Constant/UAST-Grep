using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Case Classes

/// <summary>
/// Scala case class declaration: case class Point(x: Int, y: Int)
/// Case classes automatically get copy, equals, hashCode, toString, and pattern matching support.
/// </summary>
public class CaseClassDeclaration : TypeDeclaration
{
    /// <summary>The parameters of the case class constructor.</summary>
    public IReadOnlyList<ParameterNode> CaseParameters { get; init; } = [];

    /// <summary>Whether copy method is auto-generated.</summary>
    public bool HasCopyMethod { get; init; } = true;

    /// <summary>Whether the case class is a product type for pattern matching.</summary>
    public bool IsProduct { get; init; } = true;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        children.AddRange(CaseParameters);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Scala case object declaration: case object None extends Option[Nothing]
/// A singleton case class.
/// </summary>
public class CaseObjectDeclaration : DeclarationNode
{
    /// <summary>Supertypes extended by this case object.</summary>
    public IReadOnlyList<TypeReference> Supertypes { get; init; } = [];

    /// <summary>Members of the case object.</summary>
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

#endregion

#region Traits

/// <summary>
/// Scala trait declaration: trait Ordered[A] { def compare(that: A): Int }
/// Traits are similar to interfaces but can have concrete implementations.
/// </summary>
public class TraitDeclaration : TypeDeclaration
{
    /// <summary>Whether the trait is sealed (can only be extended in same file).</summary>
    public bool IsSealed { get; init; }

    /// <summary>Self-type annotation if present: trait Foo { self: Bar => }</summary>
    public TypeReference? SelfType { get; init; }

    /// <summary>Self-type alias name (e.g., 'self' in self: Bar =>).</summary>
    public string? SelfTypeAlias { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        if (SelfType != null) children.Add(SelfType);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Scala sealed trait: sealed trait Option[+A]
/// A trait that can only be extended in the same compilation unit.
/// </summary>
public class SealedTraitDeclaration : TraitDeclaration
{
    /// <summary>Known subtypes of this sealed trait.</summary>
    public IReadOnlyList<string> KnownSubtypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        if (SelfType != null) children.Add(SelfType);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Objects (Singletons)

/// <summary>
/// Scala object declaration (singleton): object MySingleton { ... }
/// </summary>
public class ScalaObjectDeclaration : DeclarationNode
{
    /// <summary>Supertypes extended/mixed by this object.</summary>
    public IReadOnlyList<TypeReference> Supertypes { get; init; } = [];

    /// <summary>Members of the object.</summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    /// <summary>Whether this is a package object.</summary>
    public bool IsPackageObject { get; init; }

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
/// Scala companion object: object MyClass { ... } paired with class MyClass
/// </summary>
public class CompanionObject : DeclarationNode
{
    /// <summary>Name of the companion class/trait.</summary>
    public required string CompanionClassName { get; init; }

    /// <summary>Members of the companion object (factory methods, apply, unapply, etc.).</summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    /// <summary>Whether this companion has an apply method (for factory pattern).</summary>
    public bool HasApplyMethod { get; init; }

    /// <summary>Whether this companion has an unapply method (for pattern matching).</summary>
    public bool HasUnapplyMethod { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Pattern Matching

/// <summary>
/// Scala match expression: expr match { case patterns => body }
/// </summary>
public class ScalaMatchExpression : ExpressionNode
{
    /// <summary>The expression being matched (scrutinee).</summary>
    public required ExpressionNode Scrutinee { get; init; }

    /// <summary>The case clauses of the match expression.</summary>
    public required IReadOnlyList<ScalaCaseClause> Cases { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Scrutinee };
        children.AddRange(Cases);
        return children;
    }
}

/// <summary>
/// Scala case clause: case pattern if guard => body
/// </summary>
public class ScalaCaseClause : UastNode
{
    /// <summary>The pattern to match against.</summary>
    public required ScalaPattern Pattern { get; init; }

    /// <summary>Optional guard condition.</summary>
    public ExpressionNode? Guard { get; init; }

    /// <summary>The body to execute if pattern matches.</summary>
    public required UastNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        if (Guard != null) children.Add(Guard);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Base class for Scala patterns in match expressions.
/// </summary>
public abstract class ScalaPattern : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Wildcard pattern: case _ => ...
/// </summary>
public class ScalaWildcardPattern : ScalaPattern { }

/// <summary>
/// Variable binding pattern: case x => ...
/// </summary>
public class ScalaVariablePattern : ScalaPattern
{
    /// <summary>The variable name being bound.</summary>
    public required string VariableName { get; init; }

    /// <summary>Optional type ascription for the variable.</summary>
    public TypeReference? TypeAnnotation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return TypeAnnotation != null ? [TypeAnnotation] : [];
    }
}

/// <summary>
/// Literal pattern: case 42 => ... or case "hello" => ...
/// </summary>
public class ScalaLiteralPattern : ScalaPattern
{
    /// <summary>The literal value to match.</summary>
    public required ExpressionNode Literal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Literal];
}

/// <summary>
/// Type pattern: case x: String => ...
/// </summary>
public class ScalaTypePattern : ScalaPattern
{
    /// <summary>The variable name (can be _ for anonymous).</summary>
    public required string VariableName { get; init; }

    /// <summary>The type to match against.</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

/// <summary>
/// Constructor/extractor pattern: case Some(x) => ... or case Point(x, y) => ...
/// </summary>
public class ScalaConstructorPattern : ScalaPattern
{
    /// <summary>The constructor/extractor name.</summary>
    public required string ConstructorName { get; init; }

    /// <summary>The nested patterns for constructor arguments.</summary>
    public IReadOnlyList<ScalaPattern> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arguments.Cast<UastNode>().ToList();
}

/// <summary>
/// Tuple pattern: case (x, y, z) => ...
/// </summary>
public class ScalaTuplePattern : ScalaPattern
{
    /// <summary>The element patterns.</summary>
    public required IReadOnlyList<ScalaPattern> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Sequence pattern: case List(1, 2, _*) => ... or case x :: xs => ...
/// </summary>
public class ScalaSequencePattern : ScalaPattern
{
    /// <summary>The element patterns (including vararg patterns).</summary>
    public required IReadOnlyList<ScalaPattern> Elements { get; init; }

    /// <summary>Whether this pattern includes a vararg/rest element.</summary>
    public bool HasVarArg { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Alternative pattern: case A | B | C => ...
/// </summary>
public class ScalaAlternativePattern : ScalaPattern
{
    /// <summary>The alternative patterns.</summary>
    public required IReadOnlyList<ScalaPattern> Alternatives { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Alternatives.Cast<UastNode>().ToList();
}

/// <summary>
/// Pattern with bound variable: case x @ Some(_) => ...
/// </summary>
public class ScalaBoundPattern : ScalaPattern
{
    /// <summary>The variable name to bind the match to.</summary>
    public required string VariableName { get; init; }

    /// <summary>The pattern to match.</summary>
    public required ScalaPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

#endregion

#region For-Comprehensions

/// <summary>
/// Scala for-comprehension: for { generators } yield expr
/// </summary>
public class ForComprehension : ExpressionNode
{
    /// <summary>The generators and guards in the for comprehension.</summary>
    public required IReadOnlyList<ForEnumerator> Enumerators { get; init; }

    /// <summary>The body expression (after yield).</summary>
    public required UastNode Body { get; init; }

    /// <summary>Whether this is a for-yield (returns collection) or for-do (side effects only).</summary>
    public bool IsYield { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Enumerators);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Base class for for-comprehension enumerators.
/// </summary>
public abstract class ForEnumerator : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Generator in for-comprehension: x <- collection
/// </summary>
public class ForGenerator : ForEnumerator
{
    /// <summary>The pattern being bound (usually a variable name).</summary>
    public required ScalaPattern Pattern { get; init; }

    /// <summary>The expression generating values.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Expression];
}

/// <summary>
/// Guard in for-comprehension: if condition
/// </summary>
public class ForGuard : ForEnumerator
{
    /// <summary>The guard condition.</summary>
    public required ExpressionNode Condition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Condition];
}

/// <summary>
/// Value definition in for-comprehension: x = expr
/// </summary>
public class ForValueDefinition : ForEnumerator
{
    /// <summary>The pattern being bound.</summary>
    public required ScalaPattern Pattern { get; init; }

    /// <summary>The value expression.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Value];
}

#endregion

#region Implicits, Given, and Using (Scala 2 & 3)

/// <summary>
/// Scala 2 implicit definition: implicit val/def/class
/// </summary>
public class ImplicitDefinition : DeclarationNode
{
    /// <summary>The underlying declaration (val, def, or class).</summary>
    public required DeclarationNode Declaration { get; init; }

    /// <summary>The type of implicit (val, def, class, object).</summary>
    public required ImplicitKind Kind { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Declaration);
        return children;
    }
}

/// <summary>
/// Type of implicit definition in Scala 2.
/// </summary>
public enum ImplicitKind
{
    /// <summary>implicit val x: T = ...</summary>
    Val,
    /// <summary>implicit def convert(x: A): B = ...</summary>
    Def,
    /// <summary>implicit class RichInt(val x: Int) { ... }</summary>
    Class,
    /// <summary>implicit object Foo extends Ordering[Int] { ... }</summary>
    Object
}

/// <summary>
/// Scala 3 given instance: given Ordering[Int] with { ... }
/// </summary>
public class GivenDefinition : DeclarationNode
{
    /// <summary>Optional name for the given instance.</summary>
    public string? GivenName { get; init; }

    /// <summary>The type being provided.</summary>
    public required TypeReference GivenType { get; init; }

    /// <summary>The implementation body (if inline).</summary>
    public BlockNode? Body { get; init; }

    /// <summary>The value expression (if alias).</summary>
    public ExpressionNode? Alias { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(GivenType);
        if (Body != null) children.Add(Body);
        if (Alias != null) children.Add(Alias);
        return children;
    }
}

/// <summary>
/// Scala 3 using clause in function parameters: def foo(using ord: Ordering[T]): Unit
/// </summary>
public class UsingClause : UastNode
{
    /// <summary>The using parameters.</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parameters.Cast<UastNode>().ToList();
}

/// <summary>
/// Implicit parameter clause: (implicit ord: Ordering[T])
/// </summary>
public class ImplicitParameterClause : UastNode
{
    /// <summary>The implicit parameters.</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parameters.Cast<UastNode>().ToList();
}

#endregion

#region Extension Methods

/// <summary>
/// Scala 3 extension methods: extension (s: String) def words: List[String] = ...
/// </summary>
public class ExtensionDefinition : DeclarationNode
{
    /// <summary>The parameter being extended.</summary>
    public required ParameterNode ExtendedParameter { get; init; }

    /// <summary>Type parameters for the extension.</summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    /// <summary>The extension methods.</summary>
    public required IReadOnlyList<FunctionDeclaration> Methods { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(ExtendedParameter);
        children.AddRange(TypeParameters);
        children.AddRange(Methods);
        return children;
    }
}

/// <summary>
/// Scala 2 implicit class (for extension methods): implicit class RichString(val s: String) { ... }
/// </summary>
public class ImplicitClass : TypeDeclaration
{
    /// <summary>The constructor parameter being wrapped.</summary>
    public required ParameterNode WrappedParameter { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.Add(WrappedParameter);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Type Classes

/// <summary>
/// Type class pattern in Scala: trait Functor[F[_]] { def map[A, B](fa: F[A])(f: A => B): F[B] }
/// </summary>
public class TypeClassDeclaration : TraitDeclaration
{
    /// <summary>Higher-kinded type parameters (e.g., F[_]).</summary>
    public IReadOnlyList<HigherKindedTypeParameter> HigherKindedTypeParams { get; init; } = [];

    /// <summary>Laws that implementations should satisfy (documentation only).</summary>
    public IReadOnlyList<string> Laws { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(HigherKindedTypeParams);
        children.AddRange(BaseTypes);
        if (SelfType != null) children.Add(SelfType);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Higher-kinded type parameter: F[_], F[+_], F[-_], F[_, _]
/// </summary>
public class HigherKindedTypeParameter : TypeParameterNode
{
    /// <summary>The arity of the type constructor (e.g., 1 for F[_], 2 for F[_, _]).</summary>
    public int Arity { get; init; } = 1;

    /// <summary>Variance annotations for each position (Invariant, Covariant, Contravariant).</summary>
    public IReadOnlyList<Variance> PositionVariances { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Constraints.Cast<UastNode>().ToList();
}

/// <summary>
/// Variance annotation for type parameters.
/// </summary>
public enum Variance
{
    /// <summary>Invariant (no annotation)</summary>
    Invariant,
    /// <summary>Covariant (+)</summary>
    Covariant,
    /// <summary>Contravariant (-)</summary>
    Contravariant
}

#endregion

#region By-Name Parameters and Lazy Vals

/// <summary>
/// By-name parameter: def foo(x: => Int): Unit
/// The parameter is evaluated lazily each time it's referenced.
/// </summary>
public class ByNameParameter : ParameterNode
{
    /// <summary>The underlying type (without the => prefix).</summary>
    public required TypeReference UnderlyingType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.Add(UnderlyingType);
        if (DefaultValue != null) children.Add(DefaultValue);
        return children;
    }
}

/// <summary>
/// Lazy val declaration: lazy val x = expensiveComputation()
/// The value is computed on first access and memoized.
/// </summary>
public class LazyValDeclaration : VariableDeclaration
{
    /// <summary>Whether the lazy val has been initialized (runtime info if available).</summary>
    public bool? IsInitialized { get; init; }

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

#region Partial Functions

/// <summary>
/// Partial function literal: { case x if x > 0 => x * 2 }
/// </summary>
public class PartialFunctionLiteral : ExpressionNode
{
    /// <summary>The case clauses of the partial function.</summary>
    public required IReadOnlyList<ScalaCaseClause> Cases { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Cases.Cast<UastNode>().ToList();
}

/// <summary>
/// Application of partial function methods: pf.isDefinedAt(x), pf.lift
/// </summary>
public class PartialFunctionApplication : ExpressionNode
{
    /// <summary>The partial function being queried.</summary>
    public required ExpressionNode PartialFunction { get; init; }

    /// <summary>The method being called (isDefinedAt, lift, orElse, andThen, etc.).</summary>
    public required string Method { get; init; }

    /// <summary>Arguments to the method.</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { PartialFunction };
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region Scala 3 Enums

/// <summary>
/// Scala 3 enum declaration: enum Color { case Red, Green, Blue }
/// </summary>
public class Scala3EnumDeclaration : TypeDeclaration
{
    /// <summary>The enum cases.</summary>
    public required IReadOnlyList<Scala3EnumCase> EnumCases { get; init; }

    /// <summary>Whether this is an ADT (has case classes with parameters).</summary>
    public bool IsADT { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.AddRange(BaseTypes);
        children.AddRange(EnumCases);
        children.AddRange(Members);
        return children;
    }
}

/// <summary>
/// Scala 3 enum case: case Red or case Circle(radius: Double)
/// </summary>
public class Scala3EnumCase : DeclarationNode
{
    /// <summary>Parameters for parameterized enum cases.</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>Parent types extended by this case.</summary>
    public IReadOnlyList<TypeReference> Extends { get; init; } = [];

    /// <summary>Whether this case is a singleton (no parameters).</summary>
    public bool IsSingleton => Parameters.Count == 0;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Parameters);
        children.AddRange(Extends);
        return children;
    }
}

#endregion

#region Opaque Types (Scala 3)

/// <summary>
/// Scala 3 opaque type alias: opaque type UserId = String
/// </summary>
public class OpaqueTypeDeclaration : DeclarationNode
{
    /// <summary>Type parameters for the opaque type.</summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    /// <summary>The underlying type (visible only within the companion).</summary>
    public required TypeReference UnderlyingType { get; init; }

    /// <summary>Upper bound visible to external code.</summary>
    public TypeReference? UpperBound { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(TypeParameters);
        children.Add(UnderlyingType);
        if (UpperBound != null) children.Add(UpperBound);
        return children;
    }
}

#endregion

#region Context Functions and Dependent Types (Scala 3)

/// <summary>
/// Scala 3 context function type: (using Context) ?=> Result
/// </summary>
public class ContextFunctionType : TypeReference
{
    /// <summary>The context parameter types.</summary>
    public required IReadOnlyList<TypeReference> ContextParameters { get; init; }

    /// <summary>The result type.</summary>
    public required TypeReference ResultType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(ContextParameters);
        children.Add(ResultType);
        return children;
    }
}

/// <summary>
/// Dependent function type: (x: Int) => Vector[x.type]
/// </summary>
public class DependentFunctionType : TypeReference
{
    /// <summary>The parameter declarations (not just types).</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    /// <summary>The result type (may depend on parameters).</summary>
    public required TypeReference ResultType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.Add(ResultType);
        return children;
    }
}

#endregion

#region String Interpolation

/// <summary>
/// Scala string interpolation: s"Hello, $name!" or f"$value%.2f"
/// </summary>
public class ScalaStringInterpolation : ExpressionNode
{
    /// <summary>The interpolation prefix (s, f, raw, custom).</summary>
    public required string Prefix { get; init; }

    /// <summary>The parts of the interpolated string.</summary>
    public required IReadOnlyList<InterpolationPart> Parts { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Base class for interpolation parts.
/// </summary>
public abstract class InterpolationPart : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Literal text in string interpolation.
/// </summary>
public class InterpolationLiteralPart : InterpolationPart
{
    /// <summary>The literal text content.</summary>
    public required string Text { get; init; }
}

/// <summary>
/// Expression in string interpolation: $expr or ${expr}
/// </summary>
public class InterpolationExpressionPart : InterpolationPart
{
    /// <summary>The interpolated expression.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Format specifier for f-strings (e.g., "%.2f").</summary>
    public string? FormatSpecifier { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Type Bounds and Variance

/// <summary>
/// Type bound: T <: Upper or T >: Lower or T : TypeClass
/// </summary>
public class TypeBound : UastNode
{
    /// <summary>The type parameter name.</summary>
    public required string TypeParameterName { get; init; }

    /// <summary>The kind of bound.</summary>
    public required BoundKind Kind { get; init; }

    /// <summary>The bound type.</summary>
    public required TypeReference BoundType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [BoundType];
}

/// <summary>
/// Kind of type bound.
/// </summary>
public enum BoundKind
{
    /// <summary>Upper bound: T <: Upper</summary>
    Upper,
    /// <summary>Lower bound: T >: Lower</summary>
    Lower,
    /// <summary>Context bound: T : TypeClass (requires given TypeClass[T])</summary>
    Context,
    /// <summary>View bound (deprecated): T <% Viewable</summary>
    View
}

#endregion

#region Apply/Unapply

/// <summary>
/// Apply method call: MyClass(args) desugars to MyClass.apply(args)
/// </summary>
public class ApplyCall : ExpressionNode
{
    /// <summary>The object with apply method.</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>Arguments to apply.</summary>
    public required IReadOnlyList<ArgumentNode> Arguments { get; init; }

    /// <summary>Whether this is an explicit .apply() call or syntactic sugar.</summary>
    public bool IsExplicit { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Target };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Unapply method for pattern matching extractors.
/// </summary>
public class UnapplyMethod : FunctionDeclaration
{
    /// <summary>Whether this is an unapplySeq (for variable-length patterns).</summary>
    public bool IsUnapplySeq { get; init; }

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

#region Infix and Symbolic Methods

/// <summary>
/// Infix method call: a method b (instead of a.method(b))
/// </summary>
public class InfixCall : ExpressionNode
{
    /// <summary>The left operand.</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The method name (can be symbolic like +, ::, etc.).</summary>
    public required string MethodName { get; init; }

    /// <summary>The right operand.</summary>
    public required ExpressionNode Right { get; init; }

    /// <summary>Whether this is a right-associative method (ends with :).</summary>
    public bool IsRightAssociative { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Symbolic method definition: def +(other: Int): Int = ...
/// </summary>
public class SymbolicMethod : FunctionDeclaration
{
    /// <summary>The symbolic operator name.</summary>
    public required string Symbol { get; init; }

    /// <summary>Whether this is marked as @infix.</summary>
    public bool IsInfix { get; init; }

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
