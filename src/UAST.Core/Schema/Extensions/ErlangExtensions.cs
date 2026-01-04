using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Message Passing

/// <summary>
/// Erlang receive block expression: receive Pattern -> Body end.
/// Handles message passing pattern matching with optional timeout.
/// </summary>
public class ErlangReceiveBlock : ExpressionNode
{
    /// <summary>The receive clauses (pattern -> body).</summary>
    public required IReadOnlyList<ErlangReceiveClause> Clauses { get; init; }

    /// <summary>Optional after clause for timeout handling.</summary>
    public ErlangAfterClause? AfterClause { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Clauses);
        if (AfterClause != null) children.Add(AfterClause);
        return children;
    }
}

/// <summary>
/// Erlang receive clause: Pattern [when Guard] -> Body.
/// </summary>
public class ErlangReceiveClause : UastNode
{
    /// <summary>The pattern to match against incoming messages.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>Optional guard expressions.</summary>
    public IReadOnlyList<ExpressionNode> Guards { get; init; } = [];

    /// <summary>The body to execute when pattern matches.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        children.AddRange(Guards);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Erlang after clause in receive: after Timeout -> Body.
/// </summary>
public class ErlangAfterClause : UastNode
{
    /// <summary>The timeout expression (in milliseconds or 'infinity').</summary>
    public required ExpressionNode Timeout { get; init; }

    /// <summary>The body to execute on timeout.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Timeout, Body];
}

/// <summary>
/// Erlang send expression: Pid ! Message.
/// The ! operator sends a message to a process.
/// </summary>
public class ErlangSendExpression : ExpressionNode
{
    /// <summary>The process identifier or registered name to send to.</summary>
    public required ExpressionNode Pid { get; init; }

    /// <summary>The message to send.</summary>
    public required ExpressionNode Message { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pid, Message];
}

#endregion

#region Guards

/// <summary>
/// Erlang guard expression: when Guard1, Guard2; Guard3.
/// Guards are expressions that must evaluate to true for a clause to match.
/// </summary>
public class ErlangGuardExpression : ExpressionNode
{
    /// <summary>Guard sequences (semicolon-separated groups).</summary>
    public required IReadOnlyList<ErlangGuardSequence> Sequences { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Sequences.Cast<UastNode>().ToList();
}

/// <summary>
/// Erlang guard sequence: Guard1, Guard2.
/// Multiple guards in a sequence are AND-ed together.
/// </summary>
public class ErlangGuardSequence : UastNode
{
    /// <summary>The guards in this sequence (comma-separated, AND-ed).</summary>
    public required IReadOnlyList<ExpressionNode> Guards { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Guards.Cast<UastNode>().ToList();
}

/// <summary>
/// Erlang guard test: is_atom(X), is_list(Y), etc.
/// Type test functions used in guards.
/// </summary>
public class ErlangGuardTest : ExpressionNode
{
    /// <summary>The guard test function name (is_atom, is_list, etc.).</summary>
    public required string TestName { get; init; }

    /// <summary>The argument being tested.</summary>
    public required ExpressionNode Argument { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Argument];
}

#endregion

#region Pattern Matching

/// <summary>
/// Erlang function clause with pattern matching.
/// Functions can have multiple clauses with different patterns.
/// </summary>
public class ErlangFunctionClause : UastNode
{
    /// <summary>The function name.</summary>
    public required string Name { get; init; }

    /// <summary>The patterns for parameters.</summary>
    public required IReadOnlyList<ExpressionNode> Patterns { get; init; }

    /// <summary>Optional guard expressions.</summary>
    public IReadOnlyList<ExpressionNode> Guards { get; init; } = [];

    /// <summary>The function body.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Patterns);
        children.AddRange(Guards);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Erlang case expression: case Expr of Pattern -> Body end.
/// </summary>
public class ErlangCaseExpression : ExpressionNode
{
    /// <summary>The expression being matched.</summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>The case clauses.</summary>
    public required IReadOnlyList<ErlangCaseClause> Clauses { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(Clauses);
        return children;
    }
}

/// <summary>
/// Erlang case clause: Pattern [when Guard] -> Body.
/// </summary>
public class ErlangCaseClause : UastNode
{
    /// <summary>The pattern to match.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>Optional guard expressions.</summary>
    public IReadOnlyList<ExpressionNode> Guards { get; init; } = [];

    /// <summary>The body to execute.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Pattern };
        children.AddRange(Guards);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Erlang if expression: if Guard1 -> Body1; Guard2 -> Body2 end.
/// Note: Erlang if requires guards only, no pattern matching.
/// </summary>
public class ErlangIfExpression : ExpressionNode
{
    /// <summary>The if clauses.</summary>
    public required IReadOnlyList<ErlangIfClause> Clauses { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Clauses.Cast<UastNode>().ToList();
}

/// <summary>
/// Erlang if clause: Guard -> Body.
/// </summary>
public class ErlangIfClause : UastNode
{
    /// <summary>The guard condition.</summary>
    public required ExpressionNode Guard { get; init; }

    /// <summary>The body to execute.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Guard, Body];
}

/// <summary>
/// Erlang pattern match expression: Pattern = Expression.
/// </summary>
public class ErlangMatchExpression : ExpressionNode
{
    /// <summary>The pattern to match against.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>The expression whose value is matched.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Expression];
}

#endregion

#region List Comprehensions

/// <summary>
/// Erlang list comprehension: [Expr || Generator1, Filter1, ...].
/// </summary>
public class ErlangListComprehension : ExpressionNode
{
    /// <summary>The expression to generate for each element.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The generators and filters.</summary>
    public required IReadOnlyList<ErlangComprehensionQualifier> Qualifiers { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Expression };
        children.AddRange(Qualifiers);
        return children;
    }
}

/// <summary>
/// Base class for comprehension qualifiers (generators and filters).
/// </summary>
public abstract class ErlangComprehensionQualifier : UastNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Erlang list generator: Pattern &lt;- List.
/// </summary>
public class ErlangListGenerator : ErlangComprehensionQualifier
{
    /// <summary>The pattern to bind each element to.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>The list to iterate over.</summary>
    public required ExpressionNode List { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, List];
}

/// <summary>
/// Erlang binary generator: Pattern &lt;= Binary.
/// </summary>
public class ErlangBinaryGenerator : ErlangComprehensionQualifier
{
    /// <summary>The binary pattern to match.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>The binary to iterate over.</summary>
    public required ExpressionNode Binary { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Binary];
}

/// <summary>
/// Erlang comprehension filter: boolean expression.
/// </summary>
public class ErlangComprehensionFilter : ErlangComprehensionQualifier
{
    /// <summary>The filter expression (must evaluate to boolean).</summary>
    public required ExpressionNode Condition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Condition];
}

/// <summary>
/// Erlang binary comprehension: &lt;&lt;Expr || Generator1, Filter1, ...&gt;&gt;.
/// </summary>
public class ErlangBinaryComprehension : ExpressionNode
{
    /// <summary>The binary expression to generate.</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The generators and filters.</summary>
    public required IReadOnlyList<ErlangComprehensionQualifier> Qualifiers { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Expression };
        children.AddRange(Qualifiers);
        return children;
    }
}

#endregion

#region Binary Pattern Matching

/// <summary>
/// Erlang binary expression: &lt;&lt;Segment1, Segment2, ...&gt;&gt;.
/// Used for constructing and pattern matching binary data.
/// </summary>
public class ErlangBinaryExpression : ExpressionNode
{
    /// <summary>The binary segments.</summary>
    public required IReadOnlyList<ErlangBinarySegment> Segments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Segments.Cast<UastNode>().ToList();
}

/// <summary>
/// Erlang binary segment: Value/Type-Specifiers or Value:Size/Type-Specifiers.
/// </summary>
public class ErlangBinarySegment : UastNode
{
    /// <summary>The value expression.</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>Optional size specifier.</summary>
    public ExpressionNode? Size { get; init; }

    /// <summary>Type specifiers (integer, float, binary, bytes, bitstring, bits, utf8, utf16, utf32).</summary>
    public IReadOnlyList<string> TypeSpecifiers { get; init; } = [];

    /// <summary>Signedness specifier (signed, unsigned).</summary>
    public ErlangBinarySignedness? Signedness { get; init; }

    /// <summary>Endianness specifier (big, little, native).</summary>
    public ErlangBinaryEndianness? Endianness { get; init; }

    /// <summary>Unit size.</summary>
    public int? Unit { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Value };
        if (Size != null) children.Add(Size);
        return children;
    }
}

/// <summary>
/// Erlang binary signedness specifier.
/// </summary>
public enum ErlangBinarySignedness
{
    /// <summary>Signed integer.</summary>
    Signed,
    /// <summary>Unsigned integer.</summary>
    Unsigned
}

/// <summary>
/// Erlang binary endianness specifier.
/// </summary>
public enum ErlangBinaryEndianness
{
    /// <summary>Big endian byte order.</summary>
    Big,
    /// <summary>Little endian byte order.</summary>
    Little,
    /// <summary>Native byte order.</summary>
    Native
}

#endregion

#region Records

/// <summary>
/// Erlang record definition: -record(name, {field1, field2 = default}).
/// </summary>
public class ErlangRecordDefinition : DeclarationNode
{
    /// <summary>The record fields.</summary>
    public required IReadOnlyList<ErlangRecordField> Fields { get; init; }

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
/// Erlang record field definition: FieldName or FieldName = Default.
/// </summary>
public class ErlangRecordField : UastNode
{
    /// <summary>The field name.</summary>
    public required string Name { get; init; }

    /// <summary>Optional default value.</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>Optional type specification.</summary>
    public TypeReference? Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (DefaultValue != null) children.Add(DefaultValue);
        if (Type != null) children.Add(Type);
        return children;
    }
}

/// <summary>
/// Erlang record creation expression: #record_name{field1 = Value1}.
/// </summary>
public class ErlangRecordExpression : ExpressionNode
{
    /// <summary>The record type name.</summary>
    public required string RecordName { get; init; }

    /// <summary>The field assignments.</summary>
    public required IReadOnlyList<ErlangRecordFieldAssignment> Fields { get; init; }

    /// <summary>Optional base record expression for record update.</summary>
    public ExpressionNode? BaseRecord { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (BaseRecord != null) children.Add(BaseRecord);
        children.AddRange(Fields);
        return children;
    }
}

/// <summary>
/// Erlang record field assignment: field_name = value.
/// </summary>
public class ErlangRecordFieldAssignment : UastNode
{
    /// <summary>The field name.</summary>
    public required string FieldName { get; init; }

    /// <summary>The value to assign.</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Erlang record field access: Record#record_name.field.
/// </summary>
public class ErlangRecordAccess : ExpressionNode
{
    /// <summary>The record expression.</summary>
    public required ExpressionNode Record { get; init; }

    /// <summary>The record type name.</summary>
    public required string RecordName { get; init; }

    /// <summary>The field name being accessed.</summary>
    public required string FieldName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Record];
}

#endregion

#region Behaviours

/// <summary>
/// Erlang behaviour declaration: -behaviour(gen_server).
/// </summary>
public class ErlangBehaviourDeclaration : DeclarationNode
{
    /// <summary>The behaviour module name.</summary>
    public required string BehaviourModule { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Erlang callback specification: -callback function(Args) -> Return.
/// </summary>
public class ErlangCallbackSpec : DeclarationNode
{
    /// <summary>The function type specifications.</summary>
    public required IReadOnlyList<ErlangFunctionType> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Types);
        return children;
    }
}

/// <summary>
/// Erlang function type specification.
/// </summary>
public class ErlangFunctionType : TypeReference
{
    /// <summary>The parameter types.</summary>
    public required IReadOnlyList<TypeReference> ParameterTypes { get; init; }

    /// <summary>The return type.</summary>
    public required TypeReference ReturnType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(ParameterTypes);
        children.Add(ReturnType);
        return children;
    }
}

#endregion

#region Funs (Anonymous Functions)

/// <summary>
/// Erlang fun expression: fun(Args) -> Body end.
/// Anonymous functions in Erlang.
/// </summary>
public class ErlangFunExpression : ExpressionNode
{
    /// <summary>The function clauses.</summary>
    public required IReadOnlyList<ErlangFunClause> Clauses { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Clauses.Cast<UastNode>().ToList();
}

/// <summary>
/// Erlang fun clause: (Pattern1, Pattern2) [when Guard] -> Body.
/// </summary>
public class ErlangFunClause : UastNode
{
    /// <summary>The parameter patterns.</summary>
    public required IReadOnlyList<ExpressionNode> Patterns { get; init; }

    /// <summary>Optional guard expressions.</summary>
    public IReadOnlyList<ExpressionNode> Guards { get; init; } = [];

    /// <summary>The clause body.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Patterns);
        children.AddRange(Guards);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Erlang fun reference: fun module:function/arity or fun function/arity.
/// </summary>
public class ErlangFunReference : ExpressionNode
{
    /// <summary>Optional module name.</summary>
    public string? Module { get; init; }

    /// <summary>The function name.</summary>
    public required string Function { get; init; }

    /// <summary>The function arity.</summary>
    public required int Arity { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Atoms

/// <summary>
/// Erlang atom literal: atom or 'quoted atom'.
/// Atoms are constants with their name as their value.
/// </summary>
public class ErlangAtom : ExpressionNode
{
    /// <summary>The atom value (without quotes).</summary>
    public required string Value { get; init; }

    /// <summary>Whether the atom was quoted ('atom').</summary>
    public bool IsQuoted { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Tuples

/// <summary>
/// Erlang tuple expression: {Element1, Element2, ...}.
/// </summary>
public class ErlangTuple : ExpressionNode
{
    /// <summary>The tuple elements.</summary>
    public required IReadOnlyList<ExpressionNode> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

#endregion

#region Maps

/// <summary>
/// Erlang map expression: #{Key1 => Value1, Key2 := Value2}.
/// </summary>
public class ErlangMapExpression : ExpressionNode
{
    /// <summary>The map associations.</summary>
    public required IReadOnlyList<ErlangMapAssociation> Associations { get; init; }

    /// <summary>Optional base map for update syntax.</summary>
    public ExpressionNode? BaseMap { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (BaseMap != null) children.Add(BaseMap);
        children.AddRange(Associations);
        return children;
    }
}

/// <summary>
/// Erlang map association: Key => Value (create) or Key := Value (update).
/// </summary>
public class ErlangMapAssociation : UastNode
{
    /// <summary>The key expression.</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The value expression.</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>The association type (arrow or exact match).</summary>
    public required ErlangMapAssociationType AssociationType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Value];
}

/// <summary>
/// Type of Erlang map association.
/// </summary>
public enum ErlangMapAssociationType
{
    /// <summary>Arrow association (=>) - creates or updates key.</summary>
    Arrow,
    /// <summary>Exact association (:=) - updates existing key only.</summary>
    Exact
}

#endregion

#region Try/Catch/After

/// <summary>
/// Erlang try expression: try Exprs [of Clauses] catch Catches [after After] end.
/// </summary>
public class ErlangTryExpression : ExpressionNode
{
    /// <summary>The expressions to try.</summary>
    public required BlockNode TryBody { get; init; }

    /// <summary>Optional of clauses for pattern matching the result.</summary>
    public IReadOnlyList<ErlangCaseClause> OfClauses { get; init; } = [];

    /// <summary>The catch clauses.</summary>
    public IReadOnlyList<ErlangCatchClause> CatchClauses { get; init; } = [];

    /// <summary>Optional after block (finally).</summary>
    public BlockNode? AfterBody { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { TryBody };
        children.AddRange(OfClauses);
        children.AddRange(CatchClauses);
        if (AfterBody != null) children.Add(AfterBody);
        return children;
    }
}

/// <summary>
/// Erlang catch clause: Class:Pattern:Stacktrace [when Guard] -> Body.
/// </summary>
public class ErlangCatchClause : UastNode
{
    /// <summary>The exception class (error, exit, throw, or pattern).</summary>
    public ExpressionNode? ExceptionClass { get; init; }

    /// <summary>The exception pattern.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>Optional stacktrace variable (OTP 21+).</summary>
    public string? StacktraceVariable { get; init; }

    /// <summary>Optional guard expressions.</summary>
    public IReadOnlyList<ExpressionNode> Guards { get; init; } = [];

    /// <summary>The catch body.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (ExceptionClass != null) children.Add(ExceptionClass);
        children.Add(Pattern);
        children.AddRange(Guards);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Erlang catch expression: catch Expr.
/// Catches any exception and returns a value.
/// </summary>
public class ErlangCatchExpression : ExpressionNode
{
    /// <summary>The expression to catch exceptions from.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Module Attributes

/// <summary>
/// Erlang module attribute: -module(name).
/// </summary>
public class ErlangModuleAttribute : DeclarationNode
{
    /// <summary>The module name.</summary>
    public required string ModuleName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        return children;
    }
}

/// <summary>
/// Erlang export attribute: -export([func/arity, ...]).
/// </summary>
public class ErlangExportAttribute : DeclarationNode
{
    /// <summary>The exported function references.</summary>
    public required IReadOnlyList<ErlangFunctionExport> Exports { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Exports);
        return children;
    }
}

/// <summary>
/// Erlang function export: function_name/arity.
/// </summary>
public class ErlangFunctionExport : UastNode
{
    /// <summary>The function name.</summary>
    public required string FunctionName { get; init; }

    /// <summary>The function arity.</summary>
    public required int Arity { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Erlang import attribute: -import(module, [func/arity, ...]).
/// </summary>
public class ErlangImportAttribute : DeclarationNode
{
    /// <summary>The module to import from.</summary>
    public required string ImportModule { get; init; }

    /// <summary>The imported function references.</summary>
    public required IReadOnlyList<ErlangFunctionExport> Imports { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Imports);
        return children;
    }
}

/// <summary>
/// Erlang type specification: -spec function(Args) -> Return.
/// </summary>
public class ErlangSpecAttribute : DeclarationNode
{
    /// <summary>The function name.</summary>
    public required string FunctionName { get; init; }

    /// <summary>The function type specifications.</summary>
    public required IReadOnlyList<ErlangFunctionType> Types { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Types);
        return children;
    }
}

/// <summary>
/// Erlang type definition: -type name() :: Definition.
/// </summary>
public class ErlangTypeDefinition : DeclarationNode
{
    /// <summary>Optional type parameters.</summary>
    public IReadOnlyList<string> TypeParameters { get; init; } = [];

    /// <summary>The type definition body.</summary>
    public required TypeReference Definition { get; init; }

    /// <summary>Whether this is an opaque type (-opaque).</summary>
    public bool IsOpaque { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Definition);
        return children;
    }
}

#endregion

#region Remote Calls

/// <summary>
/// Erlang remote function call: module:function(args).
/// </summary>
public class ErlangRemoteCall : ExpressionNode
{
    /// <summary>The module expression.</summary>
    public required ExpressionNode Module { get; init; }

    /// <summary>The function expression.</summary>
    public required ExpressionNode Function { get; init; }

    /// <summary>The call arguments.</summary>
    public required IReadOnlyList<ExpressionNode> Arguments { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Module, Function };
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region Maybe Expression (OTP 25+)

/// <summary>
/// Erlang maybe expression: maybe Exprs end.
/// Short-circuit evaluation for pattern matching with ?= operator.
/// </summary>
public class ErlangMaybeExpression : ExpressionNode
{
    /// <summary>The maybe body expressions.</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Optional else clauses.</summary>
    public IReadOnlyList<ErlangCaseClause> ElseClauses { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Body };
        children.AddRange(ElseClauses);
        return children;
    }
}

/// <summary>
/// Erlang conditional match: Pattern ?= Expression.
/// Used in maybe expressions for short-circuit matching.
/// </summary>
public class ErlangConditionalMatch : ExpressionNode
{
    /// <summary>The pattern to match.</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>The expression to match against.</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Pattern, Expression];
}

#endregion

#region Begin/End Block

/// <summary>
/// Erlang begin-end block: begin Expr1, Expr2, ... end.
/// Groups multiple expressions into a single expression.
/// </summary>
public class ErlangBeginEndBlock : ExpressionNode
{
    /// <summary>The block body.</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

#endregion
