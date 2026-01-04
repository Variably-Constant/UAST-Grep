using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;

namespace UAST.Core.Schema.Extensions;

#region Lua Metatable Extensions

/// <summary>
/// Lua metatable metamethod declaration.
/// Represents metamethods like __index, __newindex, __call, __add, etc.
/// </summary>
public class LuaMetamethod : UastNode
{
    /// <summary>The metamethod name (e.g., "__index", "__call")</summary>
    public required string Name { get; init; }

    /// <summary>The metamethod kind</summary>
    public required LuaMetamethodKind Kind { get; init; }

    /// <summary>The function body implementing the metamethod</summary>
    public required UastNode Body { get; init; }

    /// <summary>Whether this is assigned as a function or a table</summary>
    public bool IsTableValue { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Lua metamethod kinds for metatables.
/// </summary>
public enum LuaMetamethodKind
{
    /// <summary>__index - table access fallback</summary>
    Index,
    /// <summary>__newindex - table assignment fallback</summary>
    NewIndex,
    /// <summary>__call - function call on table</summary>
    Call,
    /// <summary>__add - addition operator</summary>
    Add,
    /// <summary>__sub - subtraction operator</summary>
    Sub,
    /// <summary>__mul - multiplication operator</summary>
    Mul,
    /// <summary>__div - division operator</summary>
    Div,
    /// <summary>__mod - modulo operator</summary>
    Mod,
    /// <summary>__pow - exponentiation operator</summary>
    Pow,
    /// <summary>__unm - unary minus operator</summary>
    Unm,
    /// <summary>__idiv - integer division operator (Lua 5.3+)</summary>
    IDiv,
    /// <summary>__band - bitwise AND (Lua 5.3+)</summary>
    BAnd,
    /// <summary>__bor - bitwise OR (Lua 5.3+)</summary>
    BOr,
    /// <summary>__bxor - bitwise XOR (Lua 5.3+)</summary>
    BXor,
    /// <summary>__bnot - bitwise NOT (Lua 5.3+)</summary>
    BNot,
    /// <summary>__shl - left shift (Lua 5.3+)</summary>
    Shl,
    /// <summary>__shr - right shift (Lua 5.3+)</summary>
    Shr,
    /// <summary>__concat - concatenation operator</summary>
    Concat,
    /// <summary>__len - length operator</summary>
    Len,
    /// <summary>__eq - equality comparison</summary>
    Eq,
    /// <summary>__lt - less than comparison</summary>
    Lt,
    /// <summary>__le - less than or equal comparison</summary>
    Le,
    /// <summary>__tostring - string conversion</summary>
    ToString,
    /// <summary>__gc - garbage collection finalizer</summary>
    Gc,
    /// <summary>__mode - weak table mode</summary>
    Mode,
    /// <summary>__metatable - metatable protection</summary>
    Metatable,
    /// <summary>__pairs - custom pairs iterator (Lua 5.2+)</summary>
    Pairs,
    /// <summary>__ipairs - custom ipairs iterator (Lua 5.2+)</summary>
    IPairs,
    /// <summary>__close - to-be-closed variable (Lua 5.4+)</summary>
    Close,
    /// <summary>Unknown or custom metamethod</summary>
    Unknown
}

/// <summary>
/// Lua setmetatable call expression.
/// Represents: setmetatable(table, metatable)
/// </summary>
public class LuaSetMetatable : ExpressionNode
{
    /// <summary>The table receiving the metatable</summary>
    public required ExpressionNode Table { get; init; }

    /// <summary>The metatable being set</summary>
    public required ExpressionNode Metatable { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Table, Metatable];
}

/// <summary>
/// Lua getmetatable call expression.
/// Represents: getmetatable(object)
/// </summary>
public class LuaGetMetatable : ExpressionNode
{
    /// <summary>The object whose metatable is being retrieved</summary>
    public required ExpressionNode Object { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Object];
}

#endregion

#region Lua Coroutine Extensions

/// <summary>
/// Lua coroutine.create expression.
/// Creates a new coroutine from a function.
/// </summary>
public class LuaCoroutineCreate : ExpressionNode
{
    /// <summary>The function to wrap in a coroutine</summary>
    public required ExpressionNode Function { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Function];
}

/// <summary>
/// Lua coroutine.resume expression.
/// Resumes a suspended coroutine.
/// </summary>
public class LuaCoroutineResume : ExpressionNode
{
    /// <summary>The coroutine to resume</summary>
    public required ExpressionNode Coroutine { get; init; }

    /// <summary>Arguments to pass to the coroutine</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Coroutine };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Lua coroutine.yield expression.
/// Suspends the current coroutine and returns values.
/// </summary>
public class LuaCoroutineYield : ExpressionNode
{
    /// <summary>Values to yield from the coroutine</summary>
    public IReadOnlyList<ExpressionNode> Values { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Values.Cast<UastNode>().ToList();
}

/// <summary>
/// Lua coroutine.wrap expression.
/// Creates a coroutine and returns a function that resumes it.
/// </summary>
public class LuaCoroutineWrap : ExpressionNode
{
    /// <summary>The function to wrap</summary>
    public required ExpressionNode Function { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Function];
}

/// <summary>
/// Lua coroutine.status expression.
/// Returns the status of a coroutine.
/// </summary>
public class LuaCoroutineStatus : ExpressionNode
{
    /// <summary>The coroutine to check</summary>
    public required ExpressionNode Coroutine { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Coroutine];
}

/// <summary>
/// Lua coroutine.running expression.
/// Returns the running coroutine plus a boolean.
/// </summary>
public class LuaCoroutineRunning : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Lua Multiple Returns Extensions

/// <summary>
/// Lua multiple return expression.
/// Represents: return a, b, c
/// </summary>
public class LuaMultipleReturn : ExpressionNode
{
    /// <summary>The values being returned</summary>
    public required IReadOnlyList<ExpressionNode> Values { get; init; }

    /// <summary>Number of return values</summary>
    public int ValueCount => Values.Count;

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Values.Cast<UastNode>().ToList();
}

/// <summary>
/// Lua multiple assignment expression.
/// Represents: a, b, c = 1, 2, 3
/// </summary>
public class LuaMultipleAssignment : ExpressionNode
{
    /// <summary>The target variables</summary>
    public required IReadOnlyList<ExpressionNode> Targets { get; init; }

    /// <summary>The values being assigned</summary>
    public required IReadOnlyList<ExpressionNode> Values { get; init; }

    /// <summary>Number of targets</summary>
    public int TargetCount => Targets.Count;

    /// <summary>Number of values</summary>
    public int ValueCount => Values.Count;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Targets);
        children.AddRange(Values);
        return children;
    }
}

#endregion

#region Lua Table Constructor Extensions

/// <summary>
/// Lua table constructor expression with enhanced field tracking.
/// Represents: { field1 = value1, [expr] = value2, value3 }
/// </summary>
public class LuaTableConstructor : ExpressionNode
{
    /// <summary>Named fields (key = value)</summary>
    public IReadOnlyList<LuaTableField> NamedFields { get; init; } = [];

    /// <summary>Computed fields ([expr] = value)</summary>
    public IReadOnlyList<LuaTableField> ComputedFields { get; init; } = [];

    /// <summary>Array-style elements (positional values)</summary>
    public IReadOnlyList<ExpressionNode> ArrayElements { get; init; } = [];

    /// <summary>Whether this table is used as an array (sequential integer keys)</summary>
    public bool IsArray => NamedFields.Count == 0 && ComputedFields.Count == 0;

    /// <summary>Whether this table is used as a dictionary (all named/computed keys)</summary>
    public bool IsDictionary => ArrayElements.Count == 0;

    /// <summary>Whether this table has mixed array and dictionary elements</summary>
    public bool IsMixed => ArrayElements.Count > 0 && (NamedFields.Count > 0 || ComputedFields.Count > 0);

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(NamedFields);
        children.AddRange(ComputedFields);
        children.AddRange(ArrayElements);
        return children;
    }
}

/// <summary>
/// Lua table field (key-value pair).
/// </summary>
public class LuaTableField : UastNode
{
    /// <summary>The field key (identifier for named, expression for computed)</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The field value</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>Whether this is a computed key ([expr] = value)</summary>
    public bool IsComputed { get; init; }

    /// <summary>Whether this uses shorthand syntax (Lua 5.4+ {x, y} for {x = x, y = y})</summary>
    public bool IsShorthand { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Value];
}

#endregion

#region Lua Local Function Extensions

/// <summary>
/// Lua local function declaration.
/// Represents: local function name(...) ... end
/// </summary>
public class LuaLocalFunction : DeclarationNode
{
    /// <summary>The function parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The function body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Whether this function accepts varargs (...)</summary>
    public bool HasVarargs { get; init; }

    /// <summary>Whether this is a recursive function (references itself)</summary>
    public bool IsRecursive { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Parameters);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Lua Method Call Extensions

/// <summary>
/// Lua method call expression using colon syntax.
/// Represents: obj:method(args) which passes obj as implicit self
/// </summary>
public class LuaMethodCall : ExpressionNode
{
    /// <summary>The receiver object</summary>
    public required ExpressionNode Receiver { get; init; }

    /// <summary>The method name</summary>
    public required string MethodName { get; init; }

    /// <summary>The arguments (not including implicit self)</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    /// <summary>Whether this uses colon syntax (obj:method vs obj.method)</summary>
    public bool IsColonSyntax { get; init; } = true;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Receiver };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Lua method definition using colon syntax.
/// Represents: function obj:method(...) ... end which has implicit self parameter
/// </summary>
public class LuaMethodDefinition : DeclarationNode
{
    /// <summary>The object or class containing this method</summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>The method parameters (not including implicit self)</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The method body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Whether this uses colon syntax (has implicit self)</summary>
    public bool IsColonSyntax { get; init; } = true;

    /// <summary>Whether this function accepts varargs (...)</summary>
    public bool HasVarargs { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Object);
        children.AddRange(Parameters);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Lua Varargs Extensions

/// <summary>
/// Lua vararg expression.
/// Represents: ... in function body or return statements
/// </summary>
public class LuaVarargExpression : ExpressionNode
{
    /// <summary>Whether this vararg is in a function parameter position</summary>
    public bool IsParameter { get; init; }

    /// <summary>Whether this vararg is in a return statement</summary>
    public bool IsInReturn { get; init; }

    /// <summary>Whether this vararg is in a function call argument position</summary>
    public bool IsInCall { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Lua select function call for vararg manipulation.
/// Represents: select(index, ...) or select('#', ...)
/// </summary>
public class LuaSelectExpression : ExpressionNode
{
    /// <summary>The selector (index or '#')</summary>
    public required ExpressionNode Selector { get; init; }

    /// <summary>The vararg or list of values</summary>
    public IReadOnlyList<ExpressionNode> Values { get; init; } = [];

    /// <summary>Whether this is select('#', ...) to get count</summary>
    public bool IsCountSelector { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Selector };
        children.AddRange(Values);
        return children;
    }
}

#endregion

#region Lua Long String and Comment Extensions

/// <summary>
/// Lua long string literal.
/// Represents: [[...]], [=[...]=], [==[...]==], etc.
/// </summary>
public class LuaLongString : ExpressionNode
{
    /// <summary>The string content (without delimiters)</summary>
    public required string Content { get; init; }

    /// <summary>The level of equals signs used (0 for [[]], 1 for [=[]=], etc.)</summary>
    public int Level { get; init; }

    /// <summary>The raw text including delimiters</summary>
    public string? RawText { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Lua long comment (block comment).
/// Represents: --[[...]], --[=[...]=], etc.
/// </summary>
public class LuaLongComment : StatementNode
{
    /// <summary>The comment content (without delimiters)</summary>
    public required string Content { get; init; }

    /// <summary>The level of equals signs used</summary>
    public int Level { get; init; }

    /// <summary>Whether this is a documentation comment (starts with ---)</summary>
    public bool IsDocComment { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Lua single-line comment.
/// Represents: -- comment text
/// </summary>
public class LuaLineComment : StatementNode
{
    /// <summary>The comment text (without -- prefix)</summary>
    public required string Text { get; init; }

    /// <summary>Whether this is a documentation comment (starts with ---)</summary>
    public bool IsDocComment { get; init; }

    /// <summary>Whether this comment is at end of a code line</summary>
    public bool IsTrailingComment { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Lua Control Flow Extensions

/// <summary>
/// Lua goto statement.
/// Represents: goto label
/// </summary>
public class LuaGotoStatement : StatementNode
{
    /// <summary>The label name to jump to</summary>
    public required string Label { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Lua label statement.
/// Represents: ::label::
/// </summary>
public class LuaLabelStatement : StatementNode
{
    /// <summary>The label name</summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Lua repeat-until loop.
/// Represents: repeat ... until condition
/// </summary>
public class LuaRepeatStatement : StatementNode
{
    /// <summary>The loop body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>The until condition (loop exits when true)</summary>
    public required ExpressionNode Condition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body, Condition];
}

/// <summary>
/// Lua numeric for loop.
/// Represents: for var = start, stop[, step] do ... end
/// </summary>
public class LuaNumericFor : StatementNode
{
    /// <summary>The loop variable name</summary>
    public required string Variable { get; init; }

    /// <summary>The start value</summary>
    public required ExpressionNode Start { get; init; }

    /// <summary>The stop value</summary>
    public required ExpressionNode Stop { get; init; }

    /// <summary>The step value (optional, defaults to 1)</summary>
    public ExpressionNode? Step { get; init; }

    /// <summary>The loop body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Start, Stop };
        if (Step != null) children.Add(Step);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Lua generic for loop (iterator-based).
/// Represents: for var1, var2, ... in explist do ... end
/// </summary>
public class LuaGenericFor : StatementNode
{
    /// <summary>The loop variable names</summary>
    public required IReadOnlyList<string> Variables { get; init; }

    /// <summary>The iterator expressions</summary>
    public required IReadOnlyList<ExpressionNode> Iterators { get; init; }

    /// <summary>The loop body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Iterators);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Lua Special Expressions

/// <summary>
/// Lua require expression for module loading.
/// Represents: require("module")
/// </summary>
public class LuaRequireExpression : ExpressionNode
{
    /// <summary>The module path/name</summary>
    public required ExpressionNode ModulePath { get; init; }

    /// <summary>The resolved module name if it's a string literal</summary>
    public string? ResolvedModuleName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [ModulePath];
}

/// <summary>
/// Lua rawget expression.
/// Represents: rawget(table, key) - bypasses __index metamethod
/// </summary>
public class LuaRawGet : ExpressionNode
{
    /// <summary>The table to access</summary>
    public required ExpressionNode Table { get; init; }

    /// <summary>The key to retrieve</summary>
    public required ExpressionNode Key { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Table, Key];
}

/// <summary>
/// Lua rawset expression.
/// Represents: rawset(table, key, value) - bypasses __newindex metamethod
/// </summary>
public class LuaRawSet : ExpressionNode
{
    /// <summary>The table to modify</summary>
    public required ExpressionNode Table { get; init; }

    /// <summary>The key to set</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The value to assign</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Table, Key, Value];
}

/// <summary>
/// Lua rawequal expression.
/// Represents: rawequal(a, b) - bypasses __eq metamethod
/// </summary>
public class LuaRawEqual : ExpressionNode
{
    /// <summary>The first operand</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The second operand</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Lua rawlen expression.
/// Represents: rawlen(value) - bypasses __len metamethod
/// </summary>
public class LuaRawLen : ExpressionNode
{
    /// <summary>The value to get length of</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Lua type expression.
/// Represents: type(value) - returns type name as string
/// </summary>
public class LuaTypeExpression : ExpressionNode
{
    /// <summary>The value to check type of</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Lua pcall expression for protected call.
/// Represents: pcall(f, arg1, ...) - calls function in protected mode
/// </summary>
public class LuaPcall : ExpressionNode
{
    /// <summary>The function to call</summary>
    public required ExpressionNode Function { get; init; }

    /// <summary>Arguments to pass to the function</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Function };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Lua xpcall expression for protected call with error handler.
/// Represents: xpcall(f, msgh, arg1, ...) - calls function with custom error handler
/// </summary>
public class LuaXpcall : ExpressionNode
{
    /// <summary>The function to call</summary>
    public required ExpressionNode Function { get; init; }

    /// <summary>The error handler function</summary>
    public required ExpressionNode ErrorHandler { get; init; }

    /// <summary>Arguments to pass to the function</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Function, ErrorHandler };
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region Lua 5.4 Extensions

/// <summary>
/// Lua to-be-closed variable declaration (Lua 5.4+).
/// Represents: local x &lt;close&gt; = value
/// </summary>
public class LuaToBeClosedVariable : StatementNode
{
    /// <summary>The variable name</summary>
    public required string Name { get; init; }

    /// <summary>The initial value (must have __close metamethod)</summary>
    public required ExpressionNode Initializer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Initializer];
}

/// <summary>
/// Lua const variable declaration (Lua 5.4+).
/// Represents: local x &lt;const&gt; = value
/// </summary>
public class LuaConstVariable : StatementNode
{
    /// <summary>The variable name</summary>
    public required string Name { get; init; }

    /// <summary>The constant value</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion
