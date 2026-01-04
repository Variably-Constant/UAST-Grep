using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Goroutines & Channels

/// <summary>
/// Go statement that starts a goroutine: go func()
/// </summary>
public class GoStatement : StatementNode
{
    /// <summary>The function call to execute as a goroutine</summary>
    public required ExpressionNode FunctionCall { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [FunctionCall];
}

/// <summary>
/// Go channel type: chan T, chan&lt;- T, &lt;-chan T
/// </summary>
public class GoChannelType : TypeReference
{
    /// <summary>The element type of the channel</summary>
    public required TypeReference ElementType { get; init; }

    /// <summary>The direction of the channel (send-only, receive-only, or bidirectional)</summary>
    public ChannelDirection Direction { get; init; } = ChannelDirection.Bidirectional;

    protected override IReadOnlyList<UastNode> GetChildren() => [ElementType];
}

/// <summary>
/// Direction of a Go channel.
/// </summary>
public enum ChannelDirection
{
    /// <summary>Bidirectional channel: chan T</summary>
    Bidirectional,
    /// <summary>Send-only channel: chan&lt;- T</summary>
    SendOnly,
    /// <summary>Receive-only channel: &lt;-chan T</summary>
    ReceiveOnly
}

/// <summary>
/// Go send statement: channel &lt;- value
/// </summary>
public class GoSendStatement : StatementNode
{
    /// <summary>The channel to send to</summary>
    public required ExpressionNode Channel { get; init; }

    /// <summary>The value to send</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Channel, Value];
}

/// <summary>
/// Go receive expression: &lt;-channel
/// </summary>
public class GoReceiveExpression : ExpressionNode
{
    /// <summary>The channel to receive from</summary>
    public required ExpressionNode Channel { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Channel];
}

/// <summary>
/// Go make expression: make(type, args) for channels/slices/maps
/// </summary>
public class GoMakeExpression : ExpressionNode
{
    /// <summary>The type to create (channel, slice, or map type)</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Additional arguments (size, capacity, etc.)</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region Defer & Panic

/// <summary>
/// Go defer statement: defer func()
/// </summary>
public class GoDeferStatement : StatementNode
{
    /// <summary>The function call to defer</summary>
    public required ExpressionNode Call { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Call];
}

/// <summary>
/// Go panic expression: panic(value)
/// </summary>
public class GoPanicExpression : ExpressionNode
{
    /// <summary>The value to panic with</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Go recover expression: recover()
/// </summary>
public class GoRecoverExpression : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Select Statement

/// <summary>
/// Go select statement: select { cases }
/// </summary>
public class GoSelectStatement : StatementNode
{
    /// <summary>The select cases</summary>
    public required IReadOnlyList<GoSelectCase> Cases { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Cases.Cast<UastNode>().ToList();
}

/// <summary>
/// Go select case: case &lt;-ch: or case ch &lt;- v: or default:
/// </summary>
public class GoSelectCase : UastNode
{
    /// <summary>The communication clause (null for default case)</summary>
    public GoCommClause? Communication { get; init; }

    /// <summary>The statements to execute</summary>
    public required IReadOnlyList<StatementNode> Body { get; init; }

    /// <summary>Whether this is the default case</summary>
    public bool IsDefault { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Communication != null) children.Add(Communication);
        children.AddRange(Body);
        return children;
    }
}

/// <summary>
/// Go communication clause details for select cases.
/// </summary>
public class GoCommClause : UastNode
{
    /// <summary>The channel involved in the communication</summary>
    public required ExpressionNode Channel { get; init; }

    /// <summary>The value being sent or assigned (null for receive-only)</summary>
    public ExpressionNode? Value { get; init; }

    /// <summary>Whether this is a receive operation (vs send)</summary>
    public bool IsReceive { get; init; }

    /// <summary>Variable(s) to assign receive result to (for k, v := &lt;-ch syntax)</summary>
    public IReadOnlyList<string> AssignedVariables { get; init; } = [];

    /// <summary>Whether this uses short variable declaration (:= vs =)</summary>
    public bool IsShortDeclaration { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Channel };
        if (Value != null) children.Add(Value);
        return children;
    }
}

#endregion

#region Slices & Composite Literals

/// <summary>
/// Go slice expression: arr[low:high:max]
/// </summary>
public class GoSliceExpression : ExpressionNode
{
    /// <summary>The expression being sliced</summary>
    public required ExpressionNode Operand { get; init; }

    /// <summary>The low bound (optional)</summary>
    public ExpressionNode? Low { get; init; }

    /// <summary>The high bound (optional)</summary>
    public ExpressionNode? High { get; init; }

    /// <summary>The max capacity (optional, for 3-index slicing)</summary>
    public ExpressionNode? Max { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Operand };
        if (Low != null) children.Add(Low);
        if (High != null) children.Add(High);
        if (Max != null) children.Add(Max);
        return children;
    }
}

/// <summary>
/// Go composite literal: Type{elements}
/// </summary>
public class GoCompositeLiteral : ExpressionNode
{
    /// <summary>The type of the composite literal</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The elements of the composite literal</summary>
    public IReadOnlyList<GoKeyedElement> Elements { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Elements);
        return children;
    }
}

/// <summary>
/// Go keyed element in composite literal: key: value
/// </summary>
public class GoKeyedElement : UastNode
{
    /// <summary>The key (field name or index) - null for unkeyed elements</summary>
    public ExpressionNode? Key { get; init; }

    /// <summary>The value</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Key != null) children.Add(Key);
        children.Add(Value);
        return children;
    }
}

#endregion

#region Type System

/// <summary>
/// Go interface type: interface { methods }
/// </summary>
public class GoInterfaceType : TypeReference
{
    /// <summary>Method signatures in the interface</summary>
    public IReadOnlyList<GoMethodSpec> Methods { get; init; } = [];

    /// <summary>Embedded interface types</summary>
    public IReadOnlyList<TypeReference> EmbeddedTypes { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Methods);
        children.AddRange(EmbeddedTypes);
        return children;
    }
}

/// <summary>
/// Go method signature in an interface.
/// </summary>
public class GoMethodSpec : UastNode
{
    /// <summary>The method name</summary>
    public required string Name { get; init; }

    /// <summary>The method parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The return type(s)</summary>
    public IReadOnlyList<TypeReference> Results { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.AddRange(Results);
        return children;
    }
}

/// <summary>
/// Go struct type: struct { fields }
/// </summary>
public class GoStructType : TypeReference
{
    /// <summary>Fields in the struct</summary>
    public required IReadOnlyList<GoStructField> Fields { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Fields.Cast<UastNode>().ToList();
}

/// <summary>
/// Go struct field.
/// </summary>
public class GoStructField : UastNode
{
    /// <summary>The field names (multiple for grouped fields like "x, y int")</summary>
    public IReadOnlyList<string> Names { get; init; } = [];

    /// <summary>The field type</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The struct tag (optional)</summary>
    public GoStructTag? Tag { get; init; }

    /// <summary>Whether this is an embedded/anonymous field</summary>
    public bool IsEmbedded { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        if (Tag != null) children.Add(Tag);
        return children;
    }
}

/// <summary>
/// Go struct tag: `json:"name"`
/// </summary>
public class GoStructTag : UastNode
{
    /// <summary>The raw tag string including backticks</summary>
    public required string Raw { get; init; }

    /// <summary>Parsed tag key-value pairs (e.g., "json" -> "name,omitempty")</summary>
    public IReadOnlyDictionary<string, string> Pairs { get; init; } = new Dictionary<string, string>();

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Go type assertion: x.(Type)
/// </summary>
public class GoTypeAssertion : ExpressionNode
{
    /// <summary>The expression to assert type on</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>The asserted type</summary>
    public required TypeReference Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression, Type];
}

/// <summary>
/// Go type switch: switch x.(type) { cases }
/// </summary>
public class GoTypeSwitchStatement : StatementNode
{
    /// <summary>The expression to switch on (before .(type))</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Optional variable to assign the typed value to</summary>
    public string? AssignedVariable { get; init; }

    /// <summary>The type switch cases</summary>
    public required IReadOnlyList<GoTypeSwitchCase> Cases { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Expression };
        children.AddRange(Cases);
        return children;
    }
}

/// <summary>
/// Go type switch case: case Type1, Type2:
/// </summary>
public class GoTypeSwitchCase : UastNode
{
    /// <summary>The types to match (empty for default)</summary>
    public IReadOnlyList<TypeReference> Types { get; init; } = [];

    /// <summary>The statements to execute</summary>
    public required IReadOnlyList<StatementNode> Body { get; init; }

    /// <summary>Whether this is the default case</summary>
    public bool IsDefault { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Types);
        children.AddRange(Body);
        return children;
    }
}

#endregion

#region Other Go-Specific Constructs

/// <summary>
/// Go short variable declaration: x := value
/// </summary>
public class GoShortVarDecl : StatementNode
{
    /// <summary>The variable names being declared</summary>
    public required IReadOnlyList<string> Names { get; init; }

    /// <summary>The values being assigned</summary>
    public required IReadOnlyList<ExpressionNode> Values { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Values.Cast<UastNode>().ToList();
}

/// <summary>
/// Go range clause: for k, v := range expr
/// </summary>
public class GoRangeClause : UastNode
{
    /// <summary>The key variable (optional)</summary>
    public string? Key { get; init; }

    /// <summary>The value variable (optional)</summary>
    public string? Value { get; init; }

    /// <summary>The expression to range over</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this uses short variable declaration (:= vs =)</summary>
    public bool IsShortDeclaration { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Go labeled statement: label: statement
/// </summary>
public class GoLabeledStatement : StatementNode
{
    /// <summary>The label name</summary>
    public required string Label { get; init; }

    /// <summary>The labeled statement</summary>
    public required StatementNode Statement { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Statement];
}

/// <summary>
/// Go goto statement: goto label
/// </summary>
public class GoGotoStatement : StatementNode
{
    /// <summary>The target label</summary>
    public required string Label { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Go fallthrough statement in switch cases.
/// </summary>
public class GoFallthroughStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Go blank identifier: _ (used to discard values)
/// </summary>
public class GoBlankIdentifier : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Go ellipsis: ... (used in variadic parameters/arguments)
/// </summary>
public class GoEllipsis : UastNode
{
    /// <summary>The element type (for variadic parameter types like ...int)</summary>
    public TypeReference? ElementType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return ElementType != null ? [ElementType] : [];
    }
}

/// <summary>
/// Go iota expression (special constant generator in const blocks).
/// </summary>
public class GoIotaExpression : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Go type declaration that wraps Go-specific type nodes (struct, interface).
/// This allows FindNode to discover GoStructType and GoInterfaceType via the TypeDefinition property.
/// </summary>
public class GoTypeDeclaration : DeclarationNode
{
    /// <summary>The actual type (GoStructType or GoInterfaceType)</summary>
    public required TypeReference TypeDefinition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(TypeDefinition);
        return children;
    }
}

/// <summary>
/// Go function declaration with receiver (method).
/// Extends the base FunctionDeclaration to include Go-specific receiver information.
/// </summary>
public class GoMethodDeclaration : DeclarationNode
{
    /// <summary>The receiver parameter</summary>
    public required GoReceiverParameter Receiver { get; init; }

    /// <summary>The method parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The return type(s)</summary>
    public IReadOnlyList<TypeReference> Results { get; init; } = [];

    /// <summary>The method body</summary>
    public BlockNode? Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Receiver };
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Parameters);
        children.AddRange(Results);
        if (Body != null) children.Add(Body);
        return children;
    }
}

/// <summary>
/// Go method receiver parameter: (r *ReceiverType)
/// </summary>
public class GoReceiverParameter : UastNode
{
    /// <summary>The receiver variable name (optional)</summary>
    public string? Name { get; init; }

    /// <summary>The receiver type</summary>
    public required TypeReference Type { get; init; }

    /// <summary>Whether the receiver is a pointer type</summary>
    public bool IsPointer { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Type];
}

#endregion
