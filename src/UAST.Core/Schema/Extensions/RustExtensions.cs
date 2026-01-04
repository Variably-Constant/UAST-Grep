using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Rust Borrow and Dereference Expressions

/// <summary>
/// Rust borrow expression: &amp;value or &amp;mut value
/// Represents borrowing a reference to a value.
/// </summary>
public class BorrowExpression : ExpressionNode
{
    /// <summary>
    /// Whether this is a mutable borrow (&amp;mut).
    /// </summary>
    public bool IsMutable { get; init; }

    /// <summary>
    /// The lifetime annotation if present (e.g., 'a in &amp;'a T).
    /// </summary>
    public string? Lifetime { get; init; }

    /// <summary>
    /// The expression being borrowed.
    /// </summary>
    public required ExpressionNode Inner { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Inner];
}

/// <summary>
/// Rust dereference expression: *ptr
/// Represents dereferencing a pointer or reference.
/// </summary>
public class DereferenceExpression : ExpressionNode
{
    /// <summary>
    /// The expression being dereferenced.
    /// </summary>
    public required ExpressionNode Inner { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Inner];
}

#endregion

#region Rust Macro Expressions

/// <summary>
/// The type of delimiter used in a macro invocation.
/// </summary>
public enum MacroInvocationType
{
    /// <summary>Macro invoked with parentheses: macro!()</summary>
    Parentheses,

    /// <summary>Macro invoked with brackets: macro![]</summary>
    Brackets,

    /// <summary>Macro invoked with braces: macro!{}</summary>
    Braces
}

/// <summary>
/// Rust macro invocation: println!(), vec![], etc.
/// Represents a macro call with its tokens.
/// </summary>
public class MacroExpression : ExpressionNode
{
    /// <summary>
    /// The name of the macro (without the ! suffix).
    /// </summary>
    public required string MacroName { get; init; }

    /// <summary>
    /// The delimiter type used in the invocation.
    /// </summary>
    public MacroInvocationType InvocationType { get; init; }

    /// <summary>
    /// The raw token stream inside the macro.
    /// </summary>
    public string? RawTokens { get; init; }

    /// <summary>
    /// Parsed arguments if the macro uses standard argument syntax.
    /// </summary>
    public IReadOnlyList<ExpressionNode> ParsedArguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        ParsedArguments.Cast<UastNode>().ToList();
}

#endregion

#region Rust Block Expressions

/// <summary>
/// Rust unsafe block: unsafe { }
/// Represents an unsafe code block.
/// </summary>
public class UnsafeBlockExpression : ExpressionNode
{
    /// <summary>
    /// The body of the unsafe block.
    /// </summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Rust async block: async { } or async move { }
/// Represents an async block expression.
/// </summary>
public class AsyncBlockExpression : ExpressionNode
{
    /// <summary>
    /// Whether this is an async move block (captures by value).
    /// </summary>
    public bool IsMove { get; init; }

    /// <summary>
    /// The body of the async block.
    /// </summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

#endregion

#region Rust Try and Await Expressions

/// <summary>
/// Rust try expression: expr?
/// Represents the ? operator for error propagation.
/// </summary>
public class RustTryExpression : ExpressionNode
{
    /// <summary>
    /// The expression being tried.
    /// </summary>
    public required ExpressionNode Inner { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Inner];
}

/// <summary>
/// Rust await expression: future.await
/// Represents awaiting a future.
/// </summary>
public class RustAwaitExpression : ExpressionNode
{
    /// <summary>
    /// The future being awaited.
    /// </summary>
    public required ExpressionNode Future { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Future];
}

#endregion

#region Rust Closure Expressions

/// <summary>
/// A parameter in a Rust closure.
/// </summary>
public class ClosureParameter : UastNode
{
    /// <summary>
    /// The parameter name.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// The optional type annotation.
    /// </summary>
    public TypeReference? Type { get; init; }

    /// <summary>
    /// Whether this parameter is mutable.
    /// </summary>
    public bool IsMutable { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Type != null ? [Type] : [];
    }
}

/// <summary>
/// Rust closure expression: |x, y| expr or move |x| { ... }
/// Represents a closure/lambda expression.
/// </summary>
public class RustClosureExpression : ExpressionNode
{
    /// <summary>
    /// Whether this is a move closure (captures by value).
    /// </summary>
    public bool IsMove { get; init; }

    /// <summary>
    /// Whether this is an async closure.
    /// </summary>
    public bool IsAsync { get; init; }

    /// <summary>
    /// The closure parameters.
    /// </summary>
    public IReadOnlyList<ClosureParameter> Parameters { get; init; } = [];

    /// <summary>
    /// The optional return type annotation.
    /// </summary>
    public TypeReference? ReturnType { get; init; }

    /// <summary>
    /// The closure body (expression or block).
    /// </summary>
    public required UastNode Body { get; init; }

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

#region Rust Type References

/// <summary>
/// Rust lifetime type reference: &amp;'a T
/// Represents a reference type with a lifetime annotation.
/// </summary>
public class LifetimeTypeReference : TypeReference
{
    /// <summary>
    /// The lifetime name (e.g., 'a, 'static).
    /// </summary>
    public required string Lifetime { get; init; }

    /// <summary>
    /// The inner type being referenced.
    /// </summary>
    public required TypeReference InnerType { get; init; }

    /// <summary>
    /// Whether this is a mutable reference.
    /// </summary>
    public bool IsMutable { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [InnerType];
}

#endregion

#region Rust Impl Declaration

/// <summary>
/// Rust impl block: impl Trait for Type { } or impl Type { }
/// Represents an implementation block.
/// </summary>
public class ImplDeclaration : DeclarationNode
{
    /// <summary>
    /// The trait being implemented (null for inherent impls).
    /// </summary>
    public TypeReference? TraitType { get; init; }

    /// <summary>
    /// The type implementing the trait.
    /// </summary>
    public required TypeReference TargetType { get; init; }

    /// <summary>
    /// Type parameters for generic impls.
    /// </summary>
    public IReadOnlyList<TypeParameterNode> TypeParameters { get; init; } = [];

    /// <summary>
    /// Where clause constraints.
    /// </summary>
    public string? WhereClause { get; init; }

    /// <summary>
    /// Members of this impl block.
    /// </summary>
    public IReadOnlyList<DeclarationNode> Members { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (TraitType != null) children.Add(TraitType);
        children.Add(TargetType);
        children.AddRange(TypeParameters);
        children.AddRange(Members);
        return children;
    }
}

#endregion

#region Rust Match Expression

/// <summary>
/// Rust match expression: match expr { arms }
/// Represents pattern matching with multiple arms.
/// </summary>
public class MatchExpression : ExpressionNode
{
    /// <summary>
    /// The expression being matched.
    /// </summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>
    /// The match arms.
    /// </summary>
    public required IReadOnlyList<MatchArm> Arms { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(Arms);
        return children;
    }
}

/// <summary>
/// A match arm: pattern => body with optional guard.
/// </summary>
public class MatchArm : UastNode
{
    /// <summary>
    /// The pattern to match.
    /// </summary>
    public required RustPatternNode Pattern { get; init; }

    /// <summary>
    /// The optional guard expression (if pattern matches).
    /// </summary>
    public ExpressionNode? Guard { get; init; }

    /// <summary>
    /// The body expression if the pattern matches.
    /// </summary>
    public required ExpressionNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        if (Guard != null) children.Add(Guard);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Rust Pattern Types

/// <summary>
/// Base class for Rust patterns.
/// </summary>
public abstract class RustPatternNode : UastNode
{
}

/// <summary>
/// Rust identifier pattern: x or ref x or mut x
/// Binds a value to a name.
/// </summary>
public class RustIdentifierPattern : RustPatternNode
{
    /// <summary>
    /// The identifier name.
    /// </summary>
    public required string Name { get; init; }

    /// <summary>
    /// Whether this is a mutable binding.
    /// </summary>
    public bool IsMutable { get; init; }

    /// <summary>
    /// Whether this is a ref binding.
    /// </summary>
    public bool IsRef { get; init; }

    /// <summary>
    /// Optional subpattern for @ patterns: x @ pattern
    /// </summary>
    public RustPatternNode? Subpattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Subpattern != null ? [Subpattern] : [];
    }
}

/// <summary>
/// Rust tuple pattern: (a, b, c)
/// Destructures a tuple.
/// </summary>
public class RustTuplePattern : RustPatternNode
{
    /// <summary>
    /// The element patterns.
    /// </summary>
    public required IReadOnlyList<RustPatternNode> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Rust struct pattern: Point { x, y } or Point { x: a, .. }
/// Destructures a struct.
/// </summary>
public class RustStructPattern : RustPatternNode
{
    /// <summary>
    /// The struct type name.
    /// </summary>
    public required string TypeName { get; init; }

    /// <summary>
    /// The field patterns.
    /// </summary>
    public IReadOnlyList<RustFieldPattern> Fields { get; init; } = [];

    /// <summary>
    /// Whether this pattern uses .. to ignore remaining fields.
    /// </summary>
    public bool HasRest { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Fields.Cast<UastNode>().ToList();
}

/// <summary>
/// A Rust field pattern within a struct pattern.
/// </summary>
public class RustFieldPattern : UastNode
{
    /// <summary>
    /// The field name.
    /// </summary>
    public required string FieldName { get; init; }

    /// <summary>
    /// The pattern for this field (null for shorthand: x instead of x: x).
    /// </summary>
    public RustPatternNode? Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Pattern != null ? [Pattern] : [];
    }
}

/// <summary>
/// Rust wildcard pattern: _
/// Matches anything and discards the value.
/// </summary>
public class RustWildcardPattern : RustPatternNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Rust literal pattern: 42, "hello", true
/// Matches a specific literal value.
/// </summary>
public class RustLiteralPattern : RustPatternNode
{
    /// <summary>
    /// The literal expression.
    /// </summary>
    public required LiteralExpression Literal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Literal];
}

/// <summary>
/// Rust range pattern: 1..=10 or 'a'..='z'
/// Matches values within a range.
/// </summary>
public class RustRangePattern : RustPatternNode
{
    /// <summary>
    /// The start of the range (null for open-ended).
    /// </summary>
    public ExpressionNode? Start { get; init; }

    /// <summary>
    /// The end of the range (null for open-ended).
    /// </summary>
    public ExpressionNode? End { get; init; }

    /// <summary>
    /// Whether the range is inclusive (..= vs ..).
    /// </summary>
    public bool IsInclusive { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Start != null) children.Add(Start);
        if (End != null) children.Add(End);
        return children;
    }
}

/// <summary>
/// Rust or pattern: pattern1 | pattern2 | pattern3
/// Matches if any alternative matches.
/// </summary>
public class RustOrPattern : RustPatternNode
{
    /// <summary>
    /// The alternative patterns.
    /// </summary>
    public required IReadOnlyList<RustPatternNode> Alternatives { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Alternatives.Cast<UastNode>().ToList();
}

#endregion
