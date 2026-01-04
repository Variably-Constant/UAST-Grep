using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Threading Macro Extensions

/// <summary>
/// Clojure thread-first macro: (-> x (f a) (g b))
/// Threads x as the first argument through each form.
/// </summary>
public class ClojureThreadFirst : ExpressionNode
{
    /// <summary>The initial value being threaded</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>The forms to thread through</summary>
    public IReadOnlyList<ClojureThreadForm> Forms { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        children.AddRange(Forms);
        return children;
    }
}

/// <summary>
/// Clojure thread-last macro: (->> x (f a) (g b))
/// Threads x as the last argument through each form.
/// </summary>
public class ClojureThreadLast : ExpressionNode
{
    /// <summary>The initial value being threaded</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>The forms to thread through</summary>
    public IReadOnlyList<ClojureThreadForm> Forms { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        children.AddRange(Forms);
        return children;
    }
}

/// <summary>
/// Clojure thread-as macro: (as-> expr name forms...)
/// Binds expr to name and threads it through forms.
/// </summary>
public class ClojureThreadAs : ExpressionNode
{
    /// <summary>The initial value being threaded</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>The binding name for the threaded value</summary>
    public required string BindingName { get; init; }

    /// <summary>The forms to thread through</summary>
    public IReadOnlyList<ExpressionNode> Forms { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        children.AddRange(Forms);
        return children;
    }
}

/// <summary>
/// Clojure conditional thread macro: (cond-> x test1 form1 test2 form2)
/// Threads x through forms where the corresponding test is truthy.
/// </summary>
public class ClojureCondThread : ExpressionNode
{
    /// <summary>The initial value being threaded</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>The test-form pairs</summary>
    public IReadOnlyList<ClojureCondThreadClause> Clauses { get; init; } = [];

    /// <summary>Whether this is cond->> (thread-last) vs cond-> (thread-first)</summary>
    public bool IsThreadLast { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        children.AddRange(Clauses);
        return children;
    }
}

/// <summary>
/// A test-form pair for cond-> or cond->>
/// </summary>
public class ClojureCondThreadClause : UastNode
{
    /// <summary>The test expression</summary>
    public required ExpressionNode Test { get; init; }

    /// <summary>The form to thread through if test is truthy</summary>
    public required ExpressionNode Form { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Test, Form];
}

/// <summary>
/// A form in a threading macro (function call or symbol).
/// </summary>
public class ClojureThreadForm : UastNode
{
    /// <summary>The function or symbol being called</summary>
    public required ExpressionNode Callee { get; init; }

    /// <summary>Additional arguments (not including the threaded value)</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    /// <summary>Whether this is a bare symbol (no parens)</summary>
    public bool IsBareSymbol { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Callee };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Clojure some-threading macros: (some-> x form1 form2) or (some->> x form1 form2)
/// Like -> or ->> but short-circuits on nil.
/// </summary>
public class ClojureSomeThread : ExpressionNode
{
    /// <summary>The initial value being threaded</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>The forms to thread through</summary>
    public IReadOnlyList<ClojureThreadForm> Forms { get; init; } = [];

    /// <summary>Whether this is some->> (thread-last) vs some-> (thread-first)</summary>
    public bool IsThreadLast { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        children.AddRange(Forms);
        return children;
    }
}

#endregion

#region Destructuring Extensions

/// <summary>
/// Clojure destructuring pattern in let, fn, defn, etc.
/// Supports sequential ([a b c]), associative ({:keys [a b]}), and nested patterns.
/// </summary>
public class ClojureDestructuringPattern : UastNode
{
    /// <summary>The type of destructuring</summary>
    public required ClojureDestructuringType Type { get; init; }

    /// <summary>The bindings in this pattern</summary>
    public IReadOnlyList<ClojureDestructuringBinding> Bindings { get; init; } = [];

    /// <summary>Rest binding (e.g., &amp; rest in [a b &amp; rest])</summary>
    public string? RestBinding { get; init; }

    /// <summary>As binding (e.g., :as all in [a b :as all])</summary>
    public string? AsBinding { get; init; }

    /// <summary>Or default (e.g., :or {a 1} in {:keys [a] :or {a 1}})</summary>
    public ExpressionNode? OrDefault { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Bindings);
        if (OrDefault != null) children.Add(OrDefault);
        return children;
    }
}

/// <summary>
/// Clojure destructuring types.
/// </summary>
public enum ClojureDestructuringType
{
    /// <summary>Sequential destructuring: [a b c]</summary>
    Sequential,
    /// <summary>Associative destructuring: {:keys [a b]}</summary>
    Associative,
    /// <summary>Nested destructuring pattern</summary>
    Nested
}

/// <summary>
/// A single binding in a destructuring pattern.
/// </summary>
public class ClojureDestructuringBinding : UastNode
{
    /// <summary>The bound name or nested pattern</summary>
    public required string Name { get; init; }

    /// <summary>For associative destructuring, the key type</summary>
    public ClojureDestructuringKeyType KeyType { get; init; }

    /// <summary>The source key (for associative) or index (for sequential)</summary>
    public string? SourceKey { get; init; }

    /// <summary>Default value if key/index is missing</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>Nested destructuring pattern (if this binding destructures further)</summary>
    public ClojureDestructuringPattern? NestedPattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (DefaultValue != null) children.Add(DefaultValue);
        if (NestedPattern != null) children.Add(NestedPattern);
        return children;
    }
}

/// <summary>
/// Key types for associative destructuring.
/// </summary>
public enum ClojureDestructuringKeyType
{
    /// <summary>Regular key binding: {a :a}</summary>
    Regular,
    /// <summary>:keys shorthand: {:keys [a b]}</summary>
    Keys,
    /// <summary>:strs shorthand: {:strs [a b]}</summary>
    Strs,
    /// <summary>:syms shorthand: {:syms [a b]}</summary>
    Syms,
    /// <summary>Namespaced keys: {:ns/keys [a b]} or {::keys [a b]}</summary>
    NamespacedKeys
}

#endregion

#region Metadata Extensions

/// <summary>
/// Clojure metadata annotation: ^{:doc "..."} or ^:keyword
/// </summary>
public class ClojureMetadata : UastNode
{
    /// <summary>The target expression with metadata attached</summary>
    public required ExpressionNode Target { get; init; }

    /// <summary>The metadata map or keyword</summary>
    public required ExpressionNode MetadataValue { get; init; }

    /// <summary>Whether this is shorthand keyword metadata (^:keyword)</summary>
    public bool IsKeywordShorthand { get; init; }

    /// <summary>Whether this is type hint metadata (^String)</summary>
    public bool IsTypeHint { get; init; }

    /// <summary>The type hint if IsTypeHint is true</summary>
    public string? TypeHint { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Target, MetadataValue];
}

/// <summary>
/// Old-style metadata with #^ prefix (deprecated but still valid).
/// </summary>
public class ClojureOldMetadata : ClojureMetadata
{
    // Inherits all properties from ClojureMetadata
}

#endregion

#region Reader Macro Extensions

/// <summary>
/// Clojure var quote: #'symbol
/// Returns the Var object for a symbol.
/// </summary>
public class ClojureVarQuote : ExpressionNode
{
    /// <summary>The symbol being quoted as a var</summary>
    public required string SymbolName { get; init; }

    /// <summary>The namespace qualifier (if any)</summary>
    public string? Namespace { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Clojure anonymous function shorthand: #(+ %1 %2)
/// </summary>
public class ClojureAnonFn : ExpressionNode
{
    /// <summary>The body expression</summary>
    public required ExpressionNode Body { get; init; }

    /// <summary>Highest parameter index used (%1, %2, etc.)</summary>
    public int MaxParameterIndex { get; init; }

    /// <summary>Whether % (single arg shorthand) is used</summary>
    public bool UsesSingleArgShorthand { get; init; }

    /// <summary>Whether %&amp; (rest args) is used</summary>
    public bool UsesRestArgs { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Clojure set literal: #{1 2 3}
/// </summary>
public class ClojureSetLiteral : ExpressionNode
{
    /// <summary>The elements of the set</summary>
    public IReadOnlyList<ExpressionNode> Elements { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Clojure regex literal: #"pattern"
/// </summary>
public class ClojureRegexLiteral : ExpressionNode
{
    /// <summary>The regex pattern</summary>
    public required string Pattern { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Namespaced Keyword Extensions

/// <summary>
/// Clojure namespaced keyword: :ns/keyword or ::keyword (auto-resolved)
/// </summary>
public class ClojureNamespacedKeyword : ExpressionNode
{
    /// <summary>The keyword name (without namespace prefix)</summary>
    public required string Name { get; init; }

    /// <summary>The namespace (null for auto-resolved ::keyword)</summary>
    public string? Namespace { get; init; }

    /// <summary>Whether this is auto-resolved (::keyword or ::alias/keyword)</summary>
    public bool IsAutoResolved { get; init; }

    /// <summary>The alias used for auto-resolution (if ::alias/keyword)</summary>
    public string? Alias { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Clojure namespaced map: #:ns{:key val} or #::{:key val}
/// </summary>
public class ClojureNamespacedMap : ExpressionNode
{
    /// <summary>The namespace to apply to unqualified keys</summary>
    public string? Namespace { get; init; }

    /// <summary>Whether this uses current namespace (::{})</summary>
    public bool IsCurrentNamespace { get; init; }

    /// <summary>The key-value pairs</summary>
    public IReadOnlyList<ClojureMapEntry> Entries { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Entries.Cast<UastNode>().ToList();
}

/// <summary>
/// A key-value entry in a Clojure map.
/// </summary>
public class ClojureMapEntry : UastNode
{
    /// <summary>The key expression</summary>
    public required ExpressionNode Key { get; init; }

    /// <summary>The value expression</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Key, Value];
}

#endregion

#region Protocol Extensions

/// <summary>
/// Clojure protocol definition: (defprotocol Name (method [this arg]))
/// </summary>
public class ClojureProtocol : DeclarationNode
{
    /// <summary>The protocol methods</summary>
    public IReadOnlyList<ClojureProtocolMethod> Methods { get; init; } = [];

    /// <summary>Documentation string</summary>
    public string? DocString { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Methods);
        return children;
    }
}

/// <summary>
/// A method signature in a protocol.
/// </summary>
public class ClojureProtocolMethod : UastNode
{
    /// <summary>The method name</summary>
    public required string Name { get; init; }

    /// <summary>The method arities (parameter lists)</summary>
    public IReadOnlyList<ClojureMethodArity> Arities { get; init; } = [];

    /// <summary>Documentation string</summary>
    public string? DocString { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arities.Cast<UastNode>().ToList();
}

/// <summary>
/// A method arity (parameter list) in a protocol or function.
/// </summary>
public class ClojureMethodArity : UastNode
{
    /// <summary>The parameters for this arity</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parameters.Cast<UastNode>().ToList();
}

/// <summary>
/// Clojure extend-type: (extend-type Type Protocol (method [this] ...))
/// </summary>
public class ClojureExtendType : StatementNode
{
    /// <summary>The type being extended</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The protocol implementations</summary>
    public IReadOnlyList<ClojureProtocolImpl> Implementations { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Implementations);
        return children;
    }
}

/// <summary>
/// Clojure extend-protocol: (extend-protocol Protocol Type1 (method ...) Type2 (method ...))
/// </summary>
public class ClojureExtendProtocol : StatementNode
{
    /// <summary>The protocol being implemented</summary>
    public required string ProtocolName { get; init; }

    /// <summary>The type implementations</summary>
    public IReadOnlyList<ClojureTypeImpl> TypeImplementations { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        TypeImplementations.Cast<UastNode>().ToList();
}

/// <summary>
/// A protocol implementation in extend-type.
/// </summary>
public class ClojureProtocolImpl : UastNode
{
    /// <summary>The protocol being implemented</summary>
    public required string ProtocolName { get; init; }

    /// <summary>The method implementations</summary>
    public IReadOnlyList<FunctionDeclaration> Methods { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Methods.Cast<UastNode>().ToList();
}

/// <summary>
/// A type implementation in extend-protocol.
/// </summary>
public class ClojureTypeImpl : UastNode
{
    /// <summary>The type being extended</summary>
    public required TypeReference Type { get; init; }

    /// <summary>The method implementations</summary>
    public IReadOnlyList<FunctionDeclaration> Methods { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Type };
        children.AddRange(Methods);
        return children;
    }
}

/// <summary>
/// Clojure reify: (reify Protocol (method [this] ...))
/// Creates an anonymous instance implementing protocols.
/// </summary>
public class ClojureReify : ExpressionNode
{
    /// <summary>The protocol implementations</summary>
    public IReadOnlyList<ClojureProtocolImpl> Implementations { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Implementations.Cast<UastNode>().ToList();
}

#endregion

#region Multimethod Extensions

/// <summary>
/// Clojure multimethod definition: (defmulti name dispatch-fn)
/// </summary>
public class ClojureDefmulti : DeclarationNode
{
    /// <summary>The dispatch function</summary>
    public required ExpressionNode DispatchFn { get; init; }

    /// <summary>Default dispatch value (if specified with :default)</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>Hierarchy (if specified with :hierarchy)</summary>
    public ExpressionNode? Hierarchy { get; init; }

    /// <summary>Documentation string</summary>
    public string? DocString { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(DispatchFn);
        if (DefaultValue != null) children.Add(DefaultValue);
        if (Hierarchy != null) children.Add(Hierarchy);
        return children;
    }
}

/// <summary>
/// Clojure method implementation: (defmethod multi-name dispatch-val [args] body)
/// </summary>
public class ClojureDefmethod : DeclarationNode
{
    /// <summary>The multimethod being implemented</summary>
    public required string MultimethodName { get; init; }

    /// <summary>The dispatch value for this implementation</summary>
    public required ExpressionNode DispatchValue { get; init; }

    /// <summary>The method parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The method body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(DispatchValue);
        children.AddRange(Parameters);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Atom/Ref/Agent Extensions

/// <summary>
/// Clojure atom creation: (atom initial-value)
/// </summary>
public class ClojureAtom : ExpressionNode
{
    /// <summary>The initial value</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>Validator function (if specified with :validator)</summary>
    public ExpressionNode? Validator { get; init; }

    /// <summary>Metadata (if specified with :meta)</summary>
    public ExpressionNode? Meta { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        if (Validator != null) children.Add(Validator);
        if (Meta != null) children.Add(Meta);
        return children;
    }
}

/// <summary>
/// Clojure ref creation: (ref initial-value)
/// </summary>
public class ClojureRef : ExpressionNode
{
    /// <summary>The initial value</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>Validator function</summary>
    public ExpressionNode? Validator { get; init; }

    /// <summary>Minimum history size</summary>
    public int? MinHistory { get; init; }

    /// <summary>Maximum history size</summary>
    public int? MaxHistory { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        if (Validator != null) children.Add(Validator);
        return children;
    }
}

/// <summary>
/// Clojure agent creation: (agent initial-value)
/// </summary>
public class ClojureAgent : ExpressionNode
{
    /// <summary>The initial value</summary>
    public required ExpressionNode InitialValue { get; init; }

    /// <summary>Validator function</summary>
    public ExpressionNode? Validator { get; init; }

    /// <summary>Error handler function</summary>
    public ExpressionNode? ErrorHandler { get; init; }

    /// <summary>Error mode (:fail or :continue)</summary>
    public string? ErrorMode { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { InitialValue };
        if (Validator != null) children.Add(Validator);
        if (ErrorHandler != null) children.Add(ErrorHandler);
        return children;
    }
}

/// <summary>
/// Clojure deref expression: @ref or (deref ref)
/// </summary>
public class ClojureDeref : ExpressionNode
{
    /// <summary>The reference being dereferenced</summary>
    public required ExpressionNode Reference { get; init; }

    /// <summary>Timeout in milliseconds (for blocking deref)</summary>
    public ExpressionNode? TimeoutMs { get; init; }

    /// <summary>Timeout value (returned if timeout occurs)</summary>
    public ExpressionNode? TimeoutVal { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Reference };
        if (TimeoutMs != null) children.Add(TimeoutMs);
        if (TimeoutVal != null) children.Add(TimeoutVal);
        return children;
    }
}

/// <summary>
/// Clojure swap! expression: (swap! atom f args...)
/// </summary>
public class ClojureSwap : ExpressionNode
{
    /// <summary>The atom to swap</summary>
    public required ExpressionNode Atom { get; init; }

    /// <summary>The function to apply</summary>
    public required ExpressionNode Function { get; init; }

    /// <summary>Additional arguments to the function</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Atom, Function };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Clojure reset! expression: (reset! atom new-value)
/// </summary>
public class ClojureReset : ExpressionNode
{
    /// <summary>The atom to reset</summary>
    public required ExpressionNode Atom { get; init; }

    /// <summary>The new value</summary>
    public required ExpressionNode NewValue { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Atom, NewValue];
}

/// <summary>
/// Clojure compare-and-set!: (compare-and-set! atom oldval newval)
/// </summary>
public class ClojureCompareAndSet : ExpressionNode
{
    /// <summary>The atom</summary>
    public required ExpressionNode Atom { get; init; }

    /// <summary>Expected old value</summary>
    public required ExpressionNode OldValue { get; init; }

    /// <summary>New value to set</summary>
    public required ExpressionNode NewValue { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Atom, OldValue, NewValue];
}

/// <summary>
/// Clojure dosync block: (dosync body...)
/// Executes body in a transaction for coordinated ref updates.
/// </summary>
public class ClojureDosync : StatementNode
{
    /// <summary>The body expressions</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Clojure alter: (alter ref f args...)
/// Atomically applies f to ref's value within a transaction.
/// </summary>
public class ClojureAlter : ExpressionNode
{
    /// <summary>The ref to alter</summary>
    public required ExpressionNode Ref { get; init; }

    /// <summary>The function to apply</summary>
    public required ExpressionNode Function { get; init; }

    /// <summary>Additional arguments</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Ref, Function };
        children.AddRange(Arguments);
        return children;
    }
}

/// <summary>
/// Clojure send/send-off for agents: (send agent f args...)
/// </summary>
public class ClojureSend : ExpressionNode
{
    /// <summary>The agent</summary>
    public required ExpressionNode Agent { get; init; }

    /// <summary>The function to apply</summary>
    public required ExpressionNode Function { get; init; }

    /// <summary>Additional arguments</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    /// <summary>Whether this is send-off (for blocking operations)</summary>
    public bool IsSendOff { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Agent, Function };
        children.AddRange(Arguments);
        return children;
    }
}

#endregion

#region Quoting Extensions

/// <summary>
/// Clojure quote: 'expr or (quote expr)
/// </summary>
public class ClojureQuote : ExpressionNode
{
    /// <summary>The quoted expression</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this uses the ' reader macro vs (quote ...)</summary>
    public bool IsReaderMacro { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Clojure syntax quote: `expr
/// </summary>
public class ClojureSyntaxQuote : ExpressionNode
{
    /// <summary>The syntax-quoted expression</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Clojure unquote: ~expr (inside syntax quote)
/// </summary>
public class ClojureUnquote : ExpressionNode
{
    /// <summary>The unquoted expression</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Clojure unquote-splicing: ~@expr (inside syntax quote)
/// </summary>
public class ClojureUnquoteSplicing : ExpressionNode
{
    /// <summary>The spliced expression</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Special Literal Extensions

/// <summary>
/// Clojure tagged literal: #tag value
/// </summary>
public class ClojureTaggedLiteral : ExpressionNode
{
    /// <summary>The tag symbol</summary>
    public required string Tag { get; init; }

    /// <summary>The tagged value</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Clojure symbolic value: ##Inf, ##-Inf, ##NaN
/// </summary>
public class ClojureSymbolicValue : ExpressionNode
{
    /// <summary>The symbolic value type</summary>
    public required ClojureSymbolicValueType ValueType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Types of symbolic values in Clojure.
/// </summary>
public enum ClojureSymbolicValueType
{
    /// <summary>Positive infinity: ##Inf</summary>
    PositiveInfinity,
    /// <summary>Negative infinity: ##-Inf</summary>
    NegativeInfinity,
    /// <summary>Not a number: ##NaN</summary>
    NaN
}

/// <summary>
/// Clojure ratio literal: 1/3, 22/7
/// </summary>
public class ClojureRatio : ExpressionNode
{
    /// <summary>The numerator</summary>
    public required long Numerator { get; init; }

    /// <summary>The denominator</summary>
    public required long Denominator { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Clojure character literal: \a, \newline, \u0041
/// </summary>
public class ClojureCharLiteral : ExpressionNode
{
    /// <summary>The character value</summary>
    public required char Value { get; init; }

    /// <summary>The raw representation</summary>
    public required string RawRepresentation { get; init; }

    /// <summary>Whether this is a named character (\newline, \space, etc.)</summary>
    public bool IsNamedChar { get; init; }

    /// <summary>Whether this is a unicode escape (\uXXXX)</summary>
    public bool IsUnicodeEscape { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Reader Conditional Extensions

/// <summary>
/// Clojure reader conditional: #?(:clj x :cljs y)
/// </summary>
public class ClojureReaderConditional : ExpressionNode
{
    /// <summary>The platform-expression pairs</summary>
    public IReadOnlyList<ClojureReaderConditionalClause> Clauses { get; init; } = [];

    /// <summary>Whether this is a splicing reader conditional #?@</summary>
    public bool IsSplicing { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Clauses.Cast<UastNode>().ToList();
}

/// <summary>
/// A clause in a reader conditional.
/// </summary>
public class ClojureReaderConditionalClause : UastNode
{
    /// <summary>The platform keyword (:clj, :cljs, :cljr, :default)</summary>
    public required string Platform { get; init; }

    /// <summary>The expression for this platform</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Record and Type Extensions

/// <summary>
/// Clojure defrecord: (defrecord Name [fields] Protocol (method ...))
/// </summary>
public class ClojureDefrecord : DeclarationNode
{
    /// <summary>The record fields</summary>
    public IReadOnlyList<ClojureRecordField> Fields { get; init; } = [];

    /// <summary>The protocol implementations</summary>
    public IReadOnlyList<ClojureProtocolImpl> Implementations { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Fields);
        children.AddRange(Implementations);
        return children;
    }
}

/// <summary>
/// Clojure deftype: (deftype Name [fields] Protocol (method ...))
/// </summary>
public class ClojureDeftype : DeclarationNode
{
    /// <summary>The type fields</summary>
    public IReadOnlyList<ClojureRecordField> Fields { get; init; } = [];

    /// <summary>The protocol implementations</summary>
    public IReadOnlyList<ClojureProtocolImpl> Implementations { get; init; } = [];

    /// <summary>Whether fields are mutable (volatile-mutable or unsynchronized-mutable)</summary>
    public bool HasMutableFields { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Fields);
        children.AddRange(Implementations);
        return children;
    }
}

/// <summary>
/// A field in a record or type definition.
/// </summary>
public class ClojureRecordField : UastNode
{
    /// <summary>The field name</summary>
    public required string Name { get; init; }

    /// <summary>Type hint (if any)</summary>
    public TypeReference? TypeHint { get; init; }

    /// <summary>Whether this field is mutable</summary>
    public bool IsMutable { get; init; }

    /// <summary>Mutability type (volatile-mutable or unsynchronized-mutable)</summary>
    public ClojureMutabilityType MutabilityType { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return TypeHint != null ? [TypeHint] : [];
    }
}

/// <summary>
/// Clojure field mutability types.
/// </summary>
public enum ClojureMutabilityType
{
    /// <summary>Immutable (default)</summary>
    Immutable,
    /// <summary>^:volatile-mutable</summary>
    VolatileMutable,
    /// <summary>^:unsynchronized-mutable</summary>
    UnsynchronizedMutable
}

#endregion

#region Macro Extensions

/// <summary>
/// Clojure macro definition: (defmacro name [args] body)
/// </summary>
public class ClojureDefmacro : DeclarationNode
{
    /// <summary>The macro parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The macro body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Documentation string</summary>
    public string? DocString { get; init; }

    /// <summary>Attribute map</summary>
    public ExpressionNode? AttrMap { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Parameters);
        children.Add(Body);
        if (AttrMap != null) children.Add(AttrMap);
        return children;
    }
}

/// <summary>
/// Clojure gensym: auto## in syntax-quote expands to unique symbol
/// </summary>
public class ClojureGensym : ExpressionNode
{
    /// <summary>The base name before ##</summary>
    public required string BaseName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Lazy Sequence Extensions

/// <summary>
/// Clojure lazy-seq: (lazy-seq body)
/// </summary>
public class ClojureLazySeq : ExpressionNode
{
    /// <summary>The body that produces the sequence</summary>
    public required ExpressionNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Clojure delay: (delay body)
/// </summary>
public class ClojureDelay : ExpressionNode
{
    /// <summary>The delayed body</summary>
    public required ExpressionNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Clojure future: (future body)
/// </summary>
public class ClojureFuture : ExpressionNode
{
    /// <summary>The body to execute asynchronously</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Body];
}

/// <summary>
/// Clojure promise: (promise)
/// </summary>
public class ClojurePromise : ExpressionNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion
