using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;

namespace UAST.Core.Schema.Extensions;

#region Python Comprehension Extensions

/// <summary>
/// Python list comprehension: [expr for x in iter if cond]
/// </summary>
public class ListComprehension : ExpressionNode
{
    /// <summary>The element expression that produces each value.</summary>
    public required ExpressionNode Element { get; init; }

    /// <summary>The generators (for ... in ... if ...) that produce values.</summary>
    public required IReadOnlyList<ComprehensionGenerator> Generators { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Element };
        children.AddRange(Generators);
        return children;
    }
}

/// <summary>
/// Python set comprehension: {expr for x in iter}
/// </summary>
public class SetComprehension : ExpressionNode
{
    /// <summary>The element expression that produces each value.</summary>
    public required ExpressionNode Element { get; init; }

    /// <summary>The generators (for ... in ... if ...) that produce values.</summary>
    public required IReadOnlyList<ComprehensionGenerator> Generators { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Element };
        children.AddRange(Generators);
        return children;
    }
}

/// <summary>
/// Python dictionary comprehension: {k: v for x in iter}
/// </summary>
public class DictComprehension : ExpressionNode
{
    /// <summary>The key expression.</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The value expression.</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>The generators (for ... in ... if ...) that produce values.</summary>
    public required IReadOnlyList<ComprehensionGenerator> Generators { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Key, Value };
        children.AddRange(Generators);
        return children;
    }
}

/// <summary>
/// Python generator expression: (expr for x in iter)
/// </summary>
public class GeneratorExpression : ExpressionNode
{
    /// <summary>The element expression that produces each value.</summary>
    public required ExpressionNode Element { get; init; }

    /// <summary>The generators (for ... in ... if ...) that produce values.</summary>
    public required IReadOnlyList<ComprehensionGenerator> Generators { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Element };
        children.AddRange(Generators);
        return children;
    }
}

/// <summary>
/// Python comprehension generator: for target in iter [if cond]*
/// </summary>
public class ComprehensionGenerator : UastNode
{
    /// <summary>The target variable(s) bound in the iteration.</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>The iterable expression.</summary>
    public required ExpressionNode Iterable { get; init; }

    /// <summary>Optional filter conditions (if clauses).</summary>
    public IReadOnlyList<ExpressionNode> Conditions { get; init; } = [];

    /// <summary>Whether this is an async for comprehension.</summary>
    public bool IsAsync { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Target, Iterable };
        children.AddRange(Conditions);
        return children;
    }
}

#endregion

#region Python Decorator Extensions

/// <summary>
/// Python decorator: @decorator or @decorator(args)
/// </summary>
public class PythonDecorator : UastNode
{
    /// <summary>The decorator expression (identifier, attribute, or call).</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Arguments if the decorator is called with parameters.</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Expression };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Python decorated definition: decorators + function/class
/// </summary>
public class DecoratedDefinition : StatementNode
{
    /// <summary>The decorators applied to the definition.</summary>
    public required IReadOnlyList<PythonDecorator> Decorators { get; init; }

    /// <summary>The decorated function or class definition.</summary>
    public required DeclarationNode Definition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Decorators);
        children.Add(Definition);
        return children;
    }
}

#endregion

#region Python Special Expression Extensions

/// <summary>
/// Python walrus (assignment) expression: (x := value)
/// </summary>
public class WalrusExpression : ExpressionNode
{
    /// <summary>The target variable being assigned.</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>The value being assigned.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Target, Value];
}

/// <summary>
/// Python f-string expression: f"text {expr}"
/// </summary>
public class FStringExpression : ExpressionNode
{
    /// <summary>The parts of the f-string (text and interpolations).</summary>
    public required IReadOnlyList<FStringPart> Parts { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Base class for f-string parts.
/// </summary>
public abstract class FStringPart : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Literal text portion of an f-string.
/// </summary>
public class FStringText : FStringPart
{
    /// <summary>The literal text content.</summary>
    public required string Text { get; init; }
}

/// <summary>
/// Interpolation in an f-string: {expr} or {expr:format} or {expr!r}
/// </summary>
public class FStringInterpolation : FStringPart
{
    /// <summary>The interpolated expression.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The conversion specifier (!s, !r, !a) if any.</summary>
    public string? Conversion { get; init; }

    /// <summary>The format specifier if any.</summary>
    public string? FormatSpec { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Python starred expression: *args (for unpacking)
/// </summary>
public class StarredExpression : ExpressionNode
{
    /// <summary>The expression being unpacked.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Python double-starred expression: **kwargs (for dictionary unpacking)
/// </summary>
public class DoubleStarExpression : ExpressionNode
{
    /// <summary>The expression being unpacked.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Python yield expression: yield value or yield from expr
/// </summary>
public class PyYieldExpression : ExpressionNode
{
    /// <summary>The value being yielded (if any).</summary>
    public ExpressionNode? Value { get; init; }

    /// <summary>Whether this is 'yield from' (delegating to a sub-generator).</summary>
    public bool IsFrom { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Value != null ? [Value] : [];
    }
}

/// <summary>
/// Python await expression: await expr
/// </summary>
public class PyAwaitExpression : ExpressionNode
{
    /// <summary>The awaited expression.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Python Pattern Matching Extensions (Python 3.10+)

/// <summary>
/// Python match statement: match subject:
/// </summary>
public class PyMatchStatement : StatementNode
{
    /// <summary>The subject expression being matched.</summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>The case clauses.</summary>
    public required IReadOnlyList<PyMatchCase> Cases { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(Cases);
        return children;
    }
}

/// <summary>
/// Python match case: case pattern [if guard]: body
/// </summary>
public class PyMatchCase : UastNode
{
    /// <summary>The pattern to match.</summary>
    public required PythonPattern Pattern { get; init; }

    /// <summary>Optional guard condition.</summary>
    public ExpressionNode? Guard { get; init; }

    /// <summary>The body to execute if matched.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        if (Guard != null) children.Add(Guard);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Base class for Python match patterns.
/// </summary>
public abstract class PythonPattern : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Python capture pattern: matches anything and binds to a name.
/// </summary>
public class PyCapturePattern : PythonPattern
{
    /// <summary>The name to bind the matched value to.</summary>
    public required string Name { get; init; }
}

/// <summary>
/// Python wildcard pattern: _ (matches anything without binding).
/// </summary>
public class PyWildcardPattern : PythonPattern
{
}

/// <summary>
/// Python literal pattern: matches a literal value.
/// </summary>
public class PyLiteralPattern : PythonPattern
{
    /// <summary>The literal value to match.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Python class pattern: ClassName(args, kwarg=pattern)
/// </summary>
public class PyClassPattern : PythonPattern
{
    /// <summary>The class name to match.</summary>
    public required string ClassName { get; init; }

    /// <summary>Positional sub-patterns.</summary>
    public IReadOnlyList<PythonPattern> PositionalPatterns { get; init; } = [];

    /// <summary>Keyword sub-patterns (attribute name to pattern).</summary>
    public IReadOnlyList<PyKeywordPattern> KeywordPatterns { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(PositionalPatterns);
        children.AddRange(KeywordPatterns);
        return children;
    }
}

/// <summary>
/// Python keyword pattern within a class pattern: attr=pattern
/// </summary>
public class PyKeywordPattern : UastNode
{
    /// <summary>The attribute name.</summary>
    public required string Name { get; init; }

    /// <summary>The pattern for the attribute value.</summary>
    public required PythonPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Python sequence pattern: [a, b, *rest]
/// </summary>
public class PySequencePattern : PythonPattern
{
    /// <summary>The sub-patterns in the sequence.</summary>
    public required IReadOnlyList<PythonPattern> Patterns { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Patterns.Cast<UastNode>().ToList();
}

/// <summary>
/// Python star pattern within a sequence: *rest
/// </summary>
public class PyStarPattern : PythonPattern
{
    /// <summary>The name to bind the remaining elements to (null for wildcard *).</summary>
    public string? Name { get; init; }
}

/// <summary>
/// Python mapping pattern: {"key": pattern}
/// </summary>
public class PyMappingPattern : PythonPattern
{
    /// <summary>The key-pattern pairs.</summary>
    public required IReadOnlyList<PyMappingPatternItem> Items { get; init; }

    /// <summary>Optional double-star rest pattern (**rest).</summary>
    public string? RestName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Items.Cast<UastNode>().ToList();
}

/// <summary>
/// A key-pattern pair in a mapping pattern.
/// </summary>
public class PyMappingPatternItem : UastNode
{
    /// <summary>The key expression (typically a literal).</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The pattern for the value.</summary>
    public required PythonPattern Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Pattern];
}

/// <summary>
/// Python or pattern: pattern1 | pattern2
/// </summary>
public class PyOrPattern : PythonPattern
{
    /// <summary>The alternative patterns.</summary>
    public required IReadOnlyList<PythonPattern> Patterns { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Patterns.Cast<UastNode>().ToList();
}

/// <summary>
/// Python as pattern: pattern as name
/// </summary>
public class PyAsPattern : PythonPattern
{
    /// <summary>The pattern to match.</summary>
    public required PythonPattern Pattern { get; init; }

    /// <summary>The name to bind the matched value to.</summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern];
}

/// <summary>
/// Python value pattern: matches a dotted name (constant).
/// </summary>
public class PyValuePattern : PythonPattern
{
    /// <summary>The dotted name to match against.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion

#region Python Other Statement Extensions

/// <summary>
/// Python with item: context manager with optional alias.
/// </summary>
public class PyWithItem : UastNode
{
    /// <summary>The context manager expression.</summary>
    public required ExpressionNode ContextExpr { get; init; }

    /// <summary>Optional alias (as clause).</summary>
    public ExpressionNode? OptionalVars { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return OptionalVars != null ? [ContextExpr, OptionalVars] : [ContextExpr];
    }
}

/// <summary>
/// Python with statement: with expr [as name]: body
/// </summary>
public class PyWithStatement : StatementNode
{
    /// <summary>The context manager items.</summary>
    public required IReadOnlyList<PyWithItem> Items { get; init; }

    /// <summary>The body of the with statement.</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Whether this is an async with statement.</summary>
    public bool IsAsync { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Items);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Python except handler: except Type as name: body
/// </summary>
public class PyExceptHandler : UastNode
{
    /// <summary>The exception type(s) to catch (null for bare except).</summary>
    public ExpressionNode? Type { get; init; }

    /// <summary>The name to bind the exception to.</summary>
    public string? Name { get; init; }

    /// <summary>The handler body.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Type != null) children.Add(Type);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Python global statement: global x, y
/// </summary>
public class GlobalStatement : StatementNode
{
    /// <summary>The names declared global.</summary>
    public required IReadOnlyList<string> Names { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Python nonlocal statement: nonlocal x, y
/// </summary>
public class NonlocalStatement : StatementNode
{
    /// <summary>The names declared nonlocal.</summary>
    public required IReadOnlyList<string> Names { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Python assert statement: assert condition [, message]
/// </summary>
public class PyAssertStatement : StatementNode
{
    /// <summary>The condition to assert.</summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>Optional message expression.</summary>
    public ExpressionNode? Message { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Message != null ? [Condition, Message] : [Condition];
    }
}

/// <summary>
/// Python import statement: import module [as alias]
/// </summary>
public class PyImportStatement : StatementNode
{
    /// <summary>The modules being imported.</summary>
    public required IReadOnlyList<PyImportAlias> Names { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Names.Cast<UastNode>().ToList();
}

/// <summary>
/// Python from-import statement: from module import name [as alias]
/// </summary>
public class PyFromImportStatement : StatementNode
{
    /// <summary>The module being imported from.</summary>
    public required string Module { get; init; }

    /// <summary>The relative import level (number of leading dots).</summary>
    public int Level { get; init; }

    /// <summary>The names being imported (empty for 'import *').</summary>
    public IReadOnlyList<PyImportAlias> Names { get; init; } = [];

    /// <summary>Whether this is 'from module import *'.</summary>
    public bool IsWildcard { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Names.Cast<UastNode>().ToList();
}

/// <summary>
/// An import name with optional alias.
/// </summary>
public class PyImportAlias : UastNode
{
    /// <summary>The module or name being imported.</summary>
    public required string Name { get; init; }

    /// <summary>The alias (as clause).</summary>
    public string? Alias { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Python del statement: del x, y
/// </summary>
public class PyDelStatement : StatementNode
{
    /// <summary>The targets to delete.</summary>
    public required IReadOnlyList<ExpressionNode> Targets { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Targets.Cast<UastNode>().ToList();
}

/// <summary>
/// Python pass statement: pass
/// </summary>
public class PyPassStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Python ellipsis expression: ...
/// </summary>
public class PyEllipsisExpression : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion
