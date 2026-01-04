using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Pipe Operator Extensions

/// <summary>
/// Elixir pipe operator expression: left |> right
/// The left value is passed as the first argument to the right function.
/// </summary>
public class ElixirPipeExpression : ExpressionNode
{
    /// <summary>The left side of the pipe (value being piped)</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right side of the pipe (function receiving the piped value)</summary>
    public required ExpressionNode Right { get; init; }

    /// <summary>The full pipe chain if this is part of a longer chain</summary>
    public IReadOnlyList<ExpressionNode> PipeChain { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Left, Right };
        children.AddRange(PipeChain);
        return children;
    }
}

#endregion

#region Pattern Matching Extensions

/// <summary>
/// Elixir pattern matching in function head.
/// Example: def handle_call({:get, key}, _from, state) do ... end
/// </summary>
public class ElixirPatternMatchingFunction : DeclarationNode
{
    /// <summary>The function name</summary>
    public required string FunctionName { get; init; }

    /// <summary>The pattern clauses (multiple function heads with patterns)</summary>
    public required IReadOnlyList<ElixirFunctionClause> Clauses { get; init; }

    /// <summary>Whether this is a private function (defp)</summary>
    public bool IsPrivate { get; init; }

    /// <summary>Whether this is a macro (defmacro/defmacrop)</summary>
    public bool IsMacro { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Clauses);
        return children;
    }
}

/// <summary>
/// A single function clause with pattern matching and optional guard.
/// Example: def foo({:ok, value}) when is_binary(value), do: value
/// </summary>
public class ElixirFunctionClause : UastNode
{
    /// <summary>The patterns for each parameter</summary>
    public required IReadOnlyList<ElixirPattern> Patterns { get; init; }

    /// <summary>Optional guard clause (when ...)</summary>
    public ElixirGuardClause? Guard { get; init; }

    /// <summary>The function body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Patterns);
        if (Guard != null) children.Add(Guard);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Base class for Elixir patterns used in pattern matching.
/// </summary>
public abstract class ElixirPattern : UastNode
{
}

/// <summary>
/// Elixir literal pattern: matches a specific value.
/// Example: :ok, 42, "hello"
/// </summary>
public class ElixirLiteralPattern : ElixirPattern
{
    /// <summary>The literal value to match</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Elixir variable pattern: binds a value to a variable.
/// Example: x, _ignored, value
/// </summary>
public class ElixirVariablePattern : ElixirPattern
{
    /// <summary>The variable name</summary>
    public required string Name { get; init; }

    /// <summary>Whether this is an ignored variable (starts with _)</summary>
    public bool IsIgnored { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Elixir tuple pattern: matches a tuple structure.
/// Example: {a, b, c}, {:ok, result}
/// </summary>
public class ElixirTuplePattern : ElixirPattern
{
    /// <summary>The patterns for each tuple element</summary>
    public required IReadOnlyList<ElixirPattern> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Elixir list pattern: matches a list structure with optional head/tail.
/// Example: [head | tail], [a, b, c], []
/// </summary>
public class ElixirListPattern : ElixirPattern
{
    /// <summary>The patterns for list elements</summary>
    public IReadOnlyList<ElixirPattern> Elements { get; init; } = [];

    /// <summary>The head pattern in [head | tail] syntax</summary>
    public ElixirPattern? Head { get; init; }

    /// <summary>The tail pattern in [head | tail] syntax</summary>
    public ElixirPattern? Tail { get; init; }

    /// <summary>Whether this uses head|tail cons syntax</summary>
    public bool IsConsSyntax => Head != null || Tail != null;

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Elements);
        if (Head != null) children.Add(Head);
        if (Tail != null) children.Add(Tail);
        return children;
    }
}

/// <summary>
/// Elixir map pattern: matches a map structure.
/// Example: %{key: value}, %{^key => value}
/// </summary>
public class ElixirMapPattern : ElixirPattern
{
    /// <summary>The key-value pattern pairs</summary>
    public required IReadOnlyList<ElixirMapPatternPair> Pairs { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Pairs.Cast<UastNode>().ToList();
}

/// <summary>
/// A key-value pair in a map pattern.
/// </summary>
public class ElixirMapPatternPair : UastNode
{
    /// <summary>The key expression or pattern</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The value pattern</summary>
    public required ElixirPattern Value { get; init; }

    /// <summary>Whether the key uses the pin operator (^key)</summary>
    public bool IsPinned { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Value];
}

/// <summary>
/// Elixir struct pattern: matches a struct structure.
/// Example: %User{name: name, age: age}
/// </summary>
public class ElixirStructPattern : ElixirPattern
{
    /// <summary>The struct module name</summary>
    public required string StructName { get; init; }

    /// <summary>The field patterns</summary>
    public required IReadOnlyList<ElixirMapPatternPair> Fields { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Fields.Cast<UastNode>().ToList();
}

/// <summary>
/// Elixir binary pattern: matches binary data.
/// Example: &lt;&lt;header::size(16), rest::binary&gt;&gt;
/// </summary>
public class ElixirBinaryPattern : ElixirPattern
{
    /// <summary>The binary segments</summary>
    public required IReadOnlyList<ElixirBinarySegment> Segments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Segments.Cast<UastNode>().ToList();
}

/// <summary>
/// A segment in a binary pattern.
/// Example: x::size(8), rest::binary
/// </summary>
public class ElixirBinarySegment : UastNode
{
    /// <summary>The pattern for this segment</summary>
    public required ElixirPattern Pattern { get; init; }

    /// <summary>The size specifier (if any)</summary>
    public ExpressionNode? Size { get; init; }

    /// <summary>The type specifier (integer, binary, utf8, etc.)</summary>
    public string? Type { get; init; }

    /// <summary>The signedness (signed, unsigned)</summary>
    public string? Signedness { get; init; }

    /// <summary>The endianness (big, little, native)</summary>
    public string? Endianness { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        if (Size != null) children.Add(Size);
        return children;
    }
}

#endregion

#region Guard Extensions

/// <summary>
/// Elixir guard clause: when condition
/// Example: when is_binary(value) and byte_size(value) > 0
/// </summary>
public class ElixirGuardClause : UastNode
{
    /// <summary>The guard condition expression</summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>All guard expressions if using 'and' or 'or'</summary>
    public IReadOnlyList<ExpressionNode> Conditions { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Condition };
        children.AddRange(Conditions);
        return children;
    }
}

/// <summary>
/// Elixir guard function call (restricted set of functions allowed in guards).
/// Example: is_binary/1, is_list/1, length/1, etc.
/// </summary>
public class ElixirGuardCall : ExpressionNode
{
    /// <summary>The guard function name</summary>
    public required string FunctionName { get; init; }

    /// <summary>The arguments to the guard function</summary>
    public required IReadOnlyList<ExpressionNode> Arguments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arguments.Cast<UastNode>().ToList();
}

#endregion

#region Sigil Extensions

/// <summary>
/// Elixir sigil expression: ~r//, ~s"", ~w[], etc.
/// Sigils are custom string literals with special semantics.
/// </summary>
public class ElixirSigil : ExpressionNode
{
    /// <summary>The sigil letter (r, s, w, c, etc.)</summary>
    public required char SigilLetter { get; init; }

    /// <summary>Whether this is an uppercase sigil (no interpolation)</summary>
    public bool IsUppercase { get; init; }

    /// <summary>The sigil content</summary>
    public required string Content { get; init; }

    /// <summary>The delimiter used (/, ", [, {, etc.)</summary>
    public char Delimiter { get; init; }

    /// <summary>Modifiers at the end (for regex: i, m, s, u, etc.)</summary>
    public string Modifiers { get; init; } = "";

    /// <summary>For interpolated sigils, the parts</summary>
    public IReadOnlyList<ExpressionNode> InterpolatedParts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        InterpolatedParts.Cast<UastNode>().ToList();
}

/// <summary>
/// Elixir sigil types enumeration.
/// </summary>
public enum ElixirSigilType
{
    /// <summary>~r// - Regular expression</summary>
    Regex,
    /// <summary>~s"" - String without escaping</summary>
    String,
    /// <summary>~c'' - Charlist</summary>
    Charlist,
    /// <summary>~w[] - Word list</summary>
    WordList,
    /// <summary>~D[] - Date</summary>
    Date,
    /// <summary>~T[] - Time</summary>
    Time,
    /// <summary>~N[] - Naive DateTime</summary>
    NaiveDateTime,
    /// <summary>~U[] - UTC DateTime</summary>
    UtcDateTime,
    /// <summary>Custom sigil</summary>
    Custom
}

#endregion

#region Protocol Extensions

/// <summary>
/// Elixir protocol definition: defprotocol Name do ... end
/// </summary>
public class ElixirProtocol : DeclarationNode
{
    /// <summary>The protocol functions that implementers must define</summary>
    public required IReadOnlyList<ElixirProtocolFunction> Functions { get; init; }

    /// <summary>Documentation for the protocol</summary>
    public string? Documentation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Functions);
        return children;
    }
}

/// <summary>
/// A function specification within a protocol.
/// Example: def size(data)
/// </summary>
public class ElixirProtocolFunction : UastNode
{
    /// <summary>The function name</summary>
    public required string Name { get; init; }

    /// <summary>The parameter names</summary>
    public required IReadOnlyList<string> Parameters { get; init; }

    /// <summary>Documentation for this function</summary>
    public string? Documentation { get; init; }

    /// <summary>Type specification if provided</summary>
    public string? TypeSpec { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Elixir protocol implementation: defimpl Protocol, for: Type do ... end
/// </summary>
public class ElixirProtocolImpl : DeclarationNode
{
    /// <summary>The protocol being implemented</summary>
    public required string ProtocolName { get; init; }

    /// <summary>The type implementing the protocol</summary>
    public required string ForType { get; init; }

    /// <summary>The implemented functions</summary>
    public required IReadOnlyList<FunctionDeclaration> Functions { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Functions);
        return children;
    }
}

#endregion

#region With Expression Extensions

/// <summary>
/// Elixir with expression for handling multiple pattern matches.
/// Example: with {:ok, a} &lt;- foo(), {:ok, b} &lt;- bar(a), do: a + b
/// </summary>
public class ElixirWithExpression : ExpressionNode
{
    /// <summary>The pattern match clauses</summary>
    public required IReadOnlyList<ElixirWithClause> Clauses { get; init; }

    /// <summary>The do block (success path)</summary>
    public required UastNode DoBody { get; init; }

    /// <summary>The else clauses for handling failures</summary>
    public IReadOnlyList<ElixirWithElseClause> ElseClauses { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Clauses);
        children.Add(DoBody);
        children.AddRange(ElseClauses);
        return children;
    }
}

/// <summary>
/// A clause in a with expression: pattern &lt;- expression
/// </summary>
public class ElixirWithClause : UastNode
{
    /// <summary>The pattern to match</summary>
    public required ElixirPattern Pattern { get; init; }

    /// <summary>The expression that should match the pattern</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Optional guard for this clause</summary>
    public ElixirGuardClause? Guard { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern, Expression };
        if (Guard != null) children.Add(Guard);
        return children;
    }
}

/// <summary>
/// An else clause in a with expression for handling non-matching patterns.
/// </summary>
public class ElixirWithElseClause : UastNode
{
    /// <summary>The pattern to match the failure</summary>
    public required ElixirPattern Pattern { get; init; }

    /// <summary>The body to execute when this pattern matches</summary>
    public required UastNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Body];
}

#endregion

#region Comprehension Extensions

/// <summary>
/// Elixir for comprehension: for x &lt;- list, y &lt;- other, do: {x, y}
/// </summary>
public class ElixirComprehension : ExpressionNode
{
    /// <summary>The generators (x &lt;- collection)</summary>
    public required IReadOnlyList<ElixirGenerator> Generators { get; init; }

    /// <summary>The filters (boolean expressions)</summary>
    public IReadOnlyList<ExpressionNode> Filters { get; init; } = [];

    /// <summary>The body expression</summary>
    public required ExpressionNode Body { get; init; }

    /// <summary>Whether this comprehension collects into a specific data type</summary>
    public string? IntoType { get; init; }

    /// <summary>The uniq option (remove duplicates)</summary>
    public bool IsUnique { get; init; }

    /// <summary>The reduce option for aggregation</summary>
    public ExpressionNode? Reduce { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Generators);
        children.AddRange(Filters);
        children.Add(Body);
        if (Reduce != null) children.Add(Reduce);
        return children;
    }
}

/// <summary>
/// A generator in a comprehension: pattern &lt;- collection
/// </summary>
public class ElixirGenerator : UastNode
{
    /// <summary>The pattern to bind each element</summary>
    public required ElixirPattern Pattern { get; init; }

    /// <summary>The collection to iterate over</summary>
    public required ExpressionNode Collection { get; init; }

    /// <summary>Whether this is a binary generator (&lt;&lt;x &lt;- binary&gt;&gt;)</summary>
    public bool IsBinaryGenerator { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Collection];
}

#endregion

#region Pin Operator Extensions

/// <summary>
/// Elixir pin operator: ^variable
/// Matches the existing value of a variable instead of rebinding.
/// </summary>
public class ElixirPinExpression : ExpressionNode
{
    /// <summary>The pinned variable name</summary>
    public required string VariableName { get; init; }

    /// <summary>The pinned expression (for nested pins)</summary>
    public ExpressionNode? Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Expression != null ? [Expression] : [];
    }
}

#endregion

#region Struct Extensions

/// <summary>
/// Elixir struct definition: defstruct [:field1, :field2]
/// </summary>
public class ElixirStructDefinition : DeclarationNode
{
    /// <summary>The struct fields with optional defaults</summary>
    public required IReadOnlyList<ElixirStructField> Fields { get; init; }

    /// <summary>Whether @enforce_keys is used</summary>
    public IReadOnlyList<string> EnforcedKeys { get; init; } = [];

    /// <summary>Whether @derive is used</summary>
    public IReadOnlyList<string> DerivedProtocols { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Fields);
        return children;
    }
}

/// <summary>
/// A field in a struct definition.
/// </summary>
public class ElixirStructField : UastNode
{
    /// <summary>The field name (atom)</summary>
    public required string Name { get; init; }

    /// <summary>The default value (if any)</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>Whether this field is enforced (required)</summary>
    public bool IsEnforced { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return DefaultValue != null ? [DefaultValue] : [];
    }
}

/// <summary>
/// Elixir struct literal: %ModuleName{field: value}
/// </summary>
public class ElixirStructLiteral : ExpressionNode
{
    /// <summary>The struct module name</summary>
    public required string StructName { get; init; }

    /// <summary>The field-value pairs</summary>
    public required IReadOnlyList<PropertyNode> Fields { get; init; }

    /// <summary>Base struct being updated (for update syntax)</summary>
    public ExpressionNode? BaseStruct { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (BaseStruct != null) children.Add(BaseStruct);
        children.AddRange(Fields);
        return children;
    }
}

#endregion

#region Behaviour Extensions

/// <summary>
/// Elixir behaviour definition: @callback function_name(args) :: return_type
/// </summary>
public class ElixirBehaviour : DeclarationNode
{
    /// <summary>The callback specifications</summary>
    public required IReadOnlyList<ElixirCallback> Callbacks { get; init; }

    /// <summary>Optional callbacks that can be overridden</summary>
    public IReadOnlyList<ElixirCallback> OptionalCallbacks { get; init; } = [];

    /// <summary>Macros defined by the behaviour</summary>
    public IReadOnlyList<ElixirMacroCallback> MacroCallbacks { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Callbacks);
        children.AddRange(OptionalCallbacks);
        children.AddRange(MacroCallbacks);
        return children;
    }
}

/// <summary>
/// A callback specification in a behaviour.
/// Example: @callback handle_call(request, from, state) :: {:reply, reply, state}
/// </summary>
public class ElixirCallback : UastNode
{
    /// <summary>The callback function name</summary>
    public required string Name { get; init; }

    /// <summary>The parameter types or names</summary>
    public required IReadOnlyList<string> Parameters { get; init; }

    /// <summary>The return type specification</summary>
    public string? ReturnType { get; init; }

    /// <summary>Documentation for this callback</summary>
    public string? Documentation { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// A macro callback specification in a behaviour.
/// </summary>
public class ElixirMacroCallback : UastNode
{
    /// <summary>The macro callback name</summary>
    public required string Name { get; init; }

    /// <summary>The parameter types or names</summary>
    public required IReadOnlyList<string> Parameters { get; init; }

    /// <summary>The return type specification</summary>
    public string? ReturnType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Use of a behaviour: @behaviour ModuleName
/// </summary>
public class ElixirBehaviourUse : StatementNode
{
    /// <summary>The behaviour module being used</summary>
    public required string BehaviourModule { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Macro Extensions

/// <summary>
/// Elixir macro definition: defmacro name(args) do ... end
/// </summary>
public class ElixirMacroDefinition : DeclarationNode
{
    /// <summary>The macro parameters</summary>
    public required IReadOnlyList<ParameterNode> Parameters { get; init; }

    /// <summary>The macro body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Whether this is a private macro (defmacrop)</summary>
    public bool IsPrivate { get; init; }

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

/// <summary>
/// Elixir quote expression: quote do ... end
/// Captures code as AST.
/// </summary>
public class ElixirQuoteExpression : ExpressionNode
{
    /// <summary>The quoted code block</summary>
    public required UastNode Body { get; init; }

    /// <summary>Quote options (bind_quoted, unquote, etc.)</summary>
    public IReadOnlyList<ElixirQuoteOption> Options { get; init; } = [];

    /// <summary>Whether hygiene is enabled</summary>
    public bool Hygiene { get; init; } = true;

    /// <summary>The context for variable hygiene</summary>
    public string? Context { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Body };
        children.AddRange(Options);
        return children;
    }
}

/// <summary>
/// An option passed to quote.
/// </summary>
public class ElixirQuoteOption : UastNode
{
    /// <summary>The option name (e.g., bind_quoted, unquote, location, context)</summary>
    public required string Name { get; init; }

    /// <summary>The option value</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Elixir unquote expression: unquote(expr)
/// Injects a value into quoted code.
/// </summary>
public class ElixirUnquoteExpression : ExpressionNode
{
    /// <summary>The expression to unquote</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Elixir unquote_splicing expression: unquote_splicing(list)
/// Injects a list into quoted code as individual elements.
/// </summary>
public class ElixirUnquoteSplicingExpression : ExpressionNode
{
    /// <summary>The list expression to splice</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Module Attribute Extensions

/// <summary>
/// Elixir module attribute: @attribute value
/// </summary>
public class ElixirModuleAttribute : DeclarationNode
{
    /// <summary>The attribute name (without @)</summary>
    public required string AttributeName { get; init; }

    /// <summary>The attribute value (can be nil for attribute access)</summary>
    public ExpressionNode? Value { get; init; }

    /// <summary>Whether this is an attribute definition (vs access)</summary>
    public bool IsDefinition { get; init; }

    /// <summary>Whether this is a compile-time attribute</summary>
    public bool IsCompileTime { get; init; }

    /// <summary>Common attribute types</summary>
    public ElixirAttributeType AttributeType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        if (Value != null) children.Add(Value);
        return children;
    }
}

/// <summary>
/// Common Elixir module attribute types.
/// </summary>
public enum ElixirAttributeType
{
    /// <summary>Custom user-defined attribute</summary>
    Custom,
    /// <summary>@moduledoc - Module documentation</summary>
    Moduledoc,
    /// <summary>@doc - Function documentation</summary>
    Doc,
    /// <summary>@spec - Type specification</summary>
    Spec,
    /// <summary>@type - Type definition</summary>
    Type,
    /// <summary>@typep - Private type definition</summary>
    TypePrivate,
    /// <summary>@opaque - Opaque type definition</summary>
    Opaque,
    /// <summary>@callback - Behaviour callback</summary>
    Callback,
    /// <summary>@macrocallback - Macro callback</summary>
    MacroCallback,
    /// <summary>@optional_callbacks - Optional behaviour callbacks</summary>
    OptionalCallbacks,
    /// <summary>@behaviour - Behaviour use</summary>
    Behaviour,
    /// <summary>@impl - Implementation marker</summary>
    Impl,
    /// <summary>@derive - Protocol derivation</summary>
    Derive,
    /// <summary>@enforce_keys - Required struct fields</summary>
    EnforceKeys,
    /// <summary>@deprecated - Deprecation warning</summary>
    Deprecated,
    /// <summary>@compile - Compiler options</summary>
    Compile,
    /// <summary>@dialyzer - Dialyzer options</summary>
    Dialyzer,
    /// <summary>@external_resource - External file dependency</summary>
    ExternalResource,
    /// <summary>@on_load - Load callback</summary>
    OnLoad,
    /// <summary>@on_definition - Definition callback</summary>
    OnDefinition,
    /// <summary>@before_compile - Before compile callback</summary>
    BeforeCompile,
    /// <summary>@after_compile - After compile callback</summary>
    AfterCompile,
    /// <summary>@vsn - Module version</summary>
    Vsn
}

/// <summary>
/// Elixir @spec type specification.
/// Example: @spec add(integer(), integer()) :: integer()
/// </summary>
public class ElixirTypeSpec : StatementNode
{
    /// <summary>The function name</summary>
    public required string FunctionName { get; init; }

    /// <summary>The parameter types</summary>
    public required IReadOnlyList<string> ParameterTypes { get; init; }

    /// <summary>The return type</summary>
    public required string ReturnType { get; init; }

    /// <summary>Type guards (when clauses)</summary>
    public IReadOnlyList<string> TypeGuards { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Elixir @type definition.
/// Example: @type my_type :: atom() | integer()
/// </summary>
public class ElixirTypeDefinition : StatementNode
{
    /// <summary>The type name</summary>
    public required string TypeName { get; init; }

    /// <summary>Type parameters if generic</summary>
    public IReadOnlyList<string> TypeParameters { get; init; } = [];

    /// <summary>The type definition expression</summary>
    public required string Definition { get; init; }

    /// <summary>Whether this is a private type (@typep)</summary>
    public bool IsPrivate { get; init; }

    /// <summary>Whether this is an opaque type (@opaque)</summary>
    public bool IsOpaque { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Elixir Atom Expression

/// <summary>
/// Elixir atom literal: :atom or :"complex atom"
/// </summary>
public class ElixirAtom : ExpressionNode
{
    /// <summary>The atom value (without the colon)</summary>
    public required string Value { get; init; }

    /// <summary>Whether this is a quoted atom (:"...")</summary>
    public bool IsQuoted { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Elixir module alias: Module.Nested.Name
/// </summary>
public class ElixirAlias : ExpressionNode
{
    /// <summary>The full module path segments</summary>
    public required IReadOnlyList<string> Segments { get; init; }

    /// <summary>The full alias name</summary>
    public string FullName => string.Join(".", Segments);

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Elixir capture expression: &amp;function/arity or &amp;Module.function/arity
/// </summary>
public class ElixirCaptureExpression : ExpressionNode
{
    /// <summary>The module (if qualified)</summary>
    public string? Module { get; init; }

    /// <summary>The function name</summary>
    public required string FunctionName { get; init; }

    /// <summary>The arity</summary>
    public required int Arity { get; init; }

    /// <summary>Whether this is a local capture (no module)</summary>
    public bool IsLocal => Module == null;

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Elixir anonymous function shorthand: &amp;(&amp;1 + &amp;2)
/// </summary>
public class ElixirCaptureShorthand : ExpressionNode
{
    /// <summary>The captured expression</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The highest numbered capture placeholder used</summary>
    public int MaxPlaceholder { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Elixir capture placeholder: &amp;1, &amp;2, etc.
/// </summary>
public class ElixirCapturePlaceholder : ExpressionNode
{
    /// <summary>The placeholder number (1-based)</summary>
    public required int Number { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Exception Extensions

/// <summary>
/// Elixir exception definition: defexception [:message]
/// </summary>
public class ElixirExceptionDefinition : DeclarationNode
{
    /// <summary>The exception fields</summary>
    public required IReadOnlyList<ElixirStructField> Fields { get; init; }

    /// <summary>Custom exception message function if defined</summary>
    public FunctionDeclaration? MessageFunction { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Fields);
        if (MessageFunction != null) children.Add(MessageFunction);
        return children;
    }
}

#endregion
