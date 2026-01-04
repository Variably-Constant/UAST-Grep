using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Ruby Block Extensions

/// <summary>
/// Ruby block expression: do |args| ... end or { |args| ... }
/// Blocks are anonymous functions passed to methods.
/// </summary>
public class RubyBlock : ExpressionNode
{
    /// <summary>Block parameters inside |...|</summary>
    public IReadOnlyList<RubyBlockParameter> Parameters { get; init; } = [];

    /// <summary>The block body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Whether this is a do...end block (true) or brace block (false)</summary>
    public bool IsDoEnd { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Ruby block parameter with optional default value.
/// Example: |x, y = 10|
/// </summary>
public class RubyBlockParameter : UastNode
{
    /// <summary>The parameter name</summary>
    public required string Name { get; init; }

    /// <summary>Optional default value expression</summary>
    public ExpressionNode? DefaultValue { get; init; }

    /// <summary>Whether this is a splat parameter (*args)</summary>
    public bool IsSplat { get; init; }

    /// <summary>Whether this is a double-splat parameter (**kwargs)</summary>
    public bool IsDoubleSplat { get; init; }

    /// <summary>Whether this is a block parameter (&amp;block)</summary>
    public bool IsBlockParameter { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return DefaultValue != null ? [DefaultValue] : [];
    }
}

/// <summary>
/// Ruby block argument: passing a block with &amp; prefix.
/// Example: method(&amp;block) or method(&amp;:symbol)
/// </summary>
public class RubyBlockArgument : ExpressionNode
{
    /// <summary>The expression being passed as a block</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

#endregion

#region Ruby Symbol Extensions

/// <summary>
/// Ruby symbol literal: :symbol or :"dynamic symbol"
/// </summary>
public class RubySymbolLiteral : ExpressionNode
{
    /// <summary>The symbol name (without the colon)</summary>
    public required string Name { get; init; }

    /// <summary>Whether this is a dynamic symbol (:"...")</summary>
    public bool IsDynamic { get; init; }

    /// <summary>For interpolated symbols, the parts</summary>
    public IReadOnlyList<ExpressionNode> InterpolatedParts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        InterpolatedParts.Cast<UastNode>().ToList();
}

/// <summary>
/// Ruby symbol array literal: %i[a b c] or %I[a b c]
/// </summary>
public class RubySymbolArray : ExpressionNode
{
    /// <summary>The symbols in the array</summary>
    public required IReadOnlyList<RubySymbolLiteral> Elements { get; init; }

    /// <summary>Whether interpolation is enabled (%I vs %i)</summary>
    public bool IsInterpolated { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

#endregion

#region Ruby Range Extensions

/// <summary>
/// Ruby range expression: start..end or start...end
/// </summary>
public class RubyRange : ExpressionNode
{
    /// <summary>The start expression (can be null for beginless ranges)</summary>
    public ExpressionNode? Start { get; init; }

    /// <summary>The end expression (can be null for endless ranges)</summary>
    public ExpressionNode? End { get; init; }

    /// <summary>Whether this is an exclusive range (...) vs inclusive (..)</summary>
    public bool IsExclusive { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Start != null) children.Add(Start);
        if (End != null) children.Add(End);
        return children;
    }
}

#endregion

#region Ruby Statement Modifier Extensions

/// <summary>
/// Ruby statement modifier: expr if/unless/while/until condition
/// </summary>
public class RubyStatementModifier : StatementNode
{
    /// <summary>The statement being modified</summary>
    public required StatementNode Statement { get; init; }

    /// <summary>The modifier type</summary>
    public required RubyModifierType Modifier { get; init; }

    /// <summary>The condition expression</summary>
    public required ExpressionNode Condition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Statement, Condition];
}

/// <summary>
/// Ruby statement modifier types.
/// </summary>
public enum RubyModifierType
{
    /// <summary>if modifier: expr if condition</summary>
    If,
    /// <summary>unless modifier: expr unless condition</summary>
    Unless,
    /// <summary>while modifier: expr while condition</summary>
    While,
    /// <summary>until modifier: expr until condition</summary>
    Until,
    /// <summary>rescue modifier: expr rescue fallback</summary>
    Rescue
}

#endregion

#region Ruby Heredoc Extensions

/// <summary>
/// Ruby heredoc string literal: &lt;&lt;EOF ... EOF
/// </summary>
public class RubyHeredoc : ExpressionNode
{
    /// <summary>The heredoc delimiter (e.g., EOF, SQL)</summary>
    public required string Delimiter { get; init; }

    /// <summary>The heredoc content</summary>
    public required string Content { get; init; }

    /// <summary>Whether this is an indented heredoc (&lt;&lt;-EOF or &lt;&lt;~EOF)</summary>
    public bool IsIndented { get; init; }

    /// <summary>Whether this uses squiggly heredoc (&lt;&lt;~EOF)</summary>
    public bool IsSquiggly { get; init; }

    /// <summary>The interpolation type</summary>
    public RubyHeredocInterpolation Interpolation { get; init; }

    /// <summary>For interpolated heredocs, the parts</summary>
    public IReadOnlyList<ExpressionNode> Parts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Ruby heredoc interpolation types.
/// </summary>
public enum RubyHeredocInterpolation
{
    /// <summary>No interpolation (&lt;&lt;'EOF')</summary>
    None,
    /// <summary>Single-quoted semantics</summary>
    Single,
    /// <summary>Double-quoted semantics with interpolation (&lt;&lt;EOF or &lt;&lt;"EOF")</summary>
    Double,
    /// <summary>Command execution with interpolation (&lt;&lt;`EOF`)</summary>
    Command
}

#endregion

#region Ruby Special Literal Extensions

/// <summary>
/// Ruby regex literal: /pattern/flags
/// </summary>
public class RubyRegexLiteral : ExpressionNode
{
    /// <summary>The regex pattern</summary>
    public required string Pattern { get; init; }

    /// <summary>The regex flags (i, m, x, o, etc.)</summary>
    public string Flags { get; init; } = "";

    /// <summary>Whether the pattern contains interpolation</summary>
    public bool HasInterpolation { get; init; }

    /// <summary>For interpolated regex, the parts</summary>
    public IReadOnlyList<ExpressionNode> Parts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Ruby percent literal: %w[], %W[], %i[], %I[], %q(), %Q(), etc.
/// </summary>
public class RubyPercentLiteral : ExpressionNode
{
    /// <summary>The type of percent literal</summary>
    public required RubyPercentLiteralType Type { get; init; }

    /// <summary>The elements (for array types) or content (for string types)</summary>
    public IReadOnlyList<ExpressionNode> Elements { get; init; } = [];

    /// <summary>For string types, the raw content</summary>
    public string? Content { get; init; }

    /// <summary>The delimiter character used</summary>
    public char Delimiter { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Ruby percent literal types.
/// </summary>
public enum RubyPercentLiteralType
{
    /// <summary>%w[] - array of strings (no interpolation)</summary>
    Words,
    /// <summary>%W[] - array of strings (with interpolation)</summary>
    InterpolatedWords,
    /// <summary>%i[] - array of symbols (no interpolation)</summary>
    Symbols,
    /// <summary>%I[] - array of symbols (with interpolation)</summary>
    InterpolatedSymbols,
    /// <summary>%q() - single-quoted string</summary>
    String,
    /// <summary>%Q() - double-quoted string (with interpolation)</summary>
    InterpolatedString,
    /// <summary>%r() - regular expression</summary>
    Regex,
    /// <summary>%x() - command execution</summary>
    Command
}

#endregion

#region Ruby Method Parameter Extensions

/// <summary>
/// Ruby splat parameter: *args
/// </summary>
public class RubySplatParameter : UastNode
{
    /// <summary>The parameter name (without the *)</summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Ruby keyword splat parameter: **kwargs
/// </summary>
public class RubyKeywordSplatParameter : UastNode
{
    /// <summary>The parameter name (without the **)</summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Ruby block parameter in method signature: &amp;block
/// </summary>
public class RubyBlockParameterDecl : UastNode
{
    /// <summary>The parameter name (without the &amp;)</summary>
    public required string Name { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Ruby keyword parameter: name: or name: default
/// </summary>
public class RubyKeywordParameter : UastNode
{
    /// <summary>The parameter name</summary>
    public required string Name { get; init; }

    /// <summary>The default value (null if required keyword parameter)</summary>
    public ExpressionNode? Default { get; init; }

    /// <summary>Whether this is a required keyword parameter (no default)</summary>
    public bool IsRequired { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Default != null ? [Default] : [];
    }
}

#endregion

#region Ruby Exception Handling Extensions

/// <summary>
/// Ruby begin/rescue/else/ensure/end block.
/// </summary>
public class RubyBeginRescueEnsure : StatementNode
{
    /// <summary>The main body (between begin and rescue/else/ensure)</summary>
    public required BlockNode Body { get; init; }

    /// <summary>The rescue clauses</summary>
    public IReadOnlyList<RubyRescueClause> Rescues { get; init; } = [];

    /// <summary>The else body (executed if no exception)</summary>
    public BlockNode? ElseBody { get; init; }

    /// <summary>The ensure body (always executed, like finally)</summary>
    public BlockNode? EnsureBody { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Body };
        children.AddRange(Rescues);
        if (ElseBody != null) children.Add(ElseBody);
        if (EnsureBody != null) children.Add(EnsureBody);
        return children;
    }
}

/// <summary>
/// Ruby rescue clause: rescue Type1, Type2 =&gt; var
/// </summary>
public class RubyRescueClause : UastNode
{
    /// <summary>The exception types being rescued</summary>
    public IReadOnlyList<TypeReference> ExceptionTypes { get; init; } = [];

    /// <summary>The variable to capture the exception (after =&gt;)</summary>
    public string? Variable { get; init; }

    /// <summary>The rescue block body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(ExceptionTypes);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Ruby retry statement: restarts the begin block.
/// </summary>
public class RubyRetryStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Ruby redo statement: restarts the current iteration.
/// </summary>
public class RubyRedoStatement : StatementNode
{
    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Ruby Declaration Extensions

/// <summary>
/// Ruby module declaration: module Name ... end
/// </summary>
public class RubyModuleDeclaration : DeclarationNode
{
    /// <summary>The module body containing methods, classes, constants</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Included modules (via include)</summary>
    public IReadOnlyList<TypeReference> Includes { get; init; } = [];

    /// <summary>Extended modules (via extend)</summary>
    public IReadOnlyList<TypeReference> Extends { get; init; } = [];

    /// <summary>Prepended modules (via prepend)</summary>
    public IReadOnlyList<TypeReference> Prepends { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.AddRange(Includes);
        children.AddRange(Extends);
        children.AddRange(Prepends);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Ruby singleton class: class &lt;&lt; self or class &lt;&lt; object
/// </summary>
public class RubySingletonClass : DeclarationNode
{
    /// <summary>The object whose singleton class is being opened</summary>
    public required ExpressionNode Object { get; init; }

    /// <summary>The singleton class body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Attributes);
        children.AddRange(Modifiers);
        children.Add(Object);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Ruby alias statement: alias new_name old_name
/// </summary>
public class RubyAliasStatement : StatementNode
{
    /// <summary>The new name (symbol or method name)</summary>
    public required string NewName { get; init; }

    /// <summary>The old name (symbol or method name)</summary>
    public required string OldName { get; init; }

    /// <summary>Whether these are global variables ($old, $new)</summary>
    public bool IsGlobalAlias { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

#endregion

#region Ruby Special Expressions

/// <summary>
/// Ruby defined? expression: defined?(expr)
/// </summary>
public class RubyDefinedExpression : ExpressionNode
{
    /// <summary>The expression being tested</summary>
    public required ExpressionNode Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Ruby super call: super or super(args)
/// </summary>
public class RubySuperCall : ExpressionNode
{
    /// <summary>The arguments to pass to super (empty for bare super)</summary>
    public IReadOnlyList<ArgumentNode> Arguments { get; init; } = [];

    /// <summary>Whether parentheses were used (distinguishes super from super())</summary>
    public bool HasParens { get; init; }

    /// <summary>Whether this is a bare super (passes all args implicitly)</summary>
    public bool IsBareSuperCall => !HasParens && Arguments.Count == 0;

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arguments.Cast<UastNode>().ToList();
}

/// <summary>
/// Ruby yield expression: yield or yield(args)
/// </summary>
public class RubyYieldExpression : ExpressionNode
{
    /// <summary>The arguments to yield</summary>
    public IReadOnlyList<ExpressionNode> Arguments { get; init; } = [];

    /// <summary>Whether parentheses were used</summary>
    public bool HasParens { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Arguments.Cast<UastNode>().ToList();
}

/// <summary>
/// Ruby lambda literal: -&gt;(args) { body } or lambda { body }
/// </summary>
public class RubyLambda : ExpressionNode
{
    /// <summary>The lambda parameters</summary>
    public IReadOnlyList<ParameterNode> Parameters { get; init; } = [];

    /// <summary>The lambda body</summary>
    public required UastNode Body { get; init; }

    /// <summary>Whether this uses arrow syntax (-&gt;) vs lambda keyword</summary>
    public bool IsArrowSyntax { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Parameters);
        children.Add(Body);
        return children;
    }
}

/// <summary>
/// Ruby safe navigation operator: obj&amp;.method
/// </summary>
public class RubySafeNavigationExpression : ExpressionNode
{
    /// <summary>The receiver expression</summary>
    public required ExpressionNode Receiver { get; init; }

    /// <summary>The member being accessed</summary>
    public required string Member { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Receiver];
}

/// <summary>
/// Ruby case-when expression (pattern matching in Ruby 3.0+)
/// </summary>
public class RubyCaseInExpression : ExpressionNode
{
    /// <summary>The subject being matched</summary>
    public required ExpressionNode Subject { get; init; }

    /// <summary>The in clauses (pattern matching branches)</summary>
    public required IReadOnlyList<RubyInClause> InClauses { get; init; }

    /// <summary>The else expression (if any)</summary>
    public ExpressionNode? ElseExpression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Subject };
        children.AddRange(InClauses);
        if (ElseExpression != null) children.Add(ElseExpression);
        return children;
    }
}

/// <summary>
/// Ruby in clause for pattern matching: in pattern [if/unless guard] then expr
/// </summary>
public class RubyInClause : UastNode
{
    /// <summary>The pattern to match</summary>
    public required ExpressionNode Pattern { get; init; }

    /// <summary>Optional guard condition</summary>
    public ExpressionNode? Guard { get; init; }

    /// <summary>Whether the guard is an 'if' (true) or 'unless' (false)</summary>
    public bool GuardIsIf { get; init; } = true;

    /// <summary>The body expression</summary>
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

#region Ruby Metaprogramming Extensions

/// <summary>
/// Ruby attr_* declarations: attr_reader, attr_writer, attr_accessor
/// </summary>
public class RubyAttrDeclaration : StatementNode
{
    /// <summary>The type of attr declaration</summary>
    public required RubyAttrType Type { get; init; }

    /// <summary>The attribute names (as symbols)</summary>
    public required IReadOnlyList<string> Names { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Ruby attr declaration types.
/// </summary>
public enum RubyAttrType
{
    /// <summary>attr_reader :name - generates getter only</summary>
    Reader,
    /// <summary>attr_writer :name - generates setter only</summary>
    Writer,
    /// <summary>attr_accessor :name - generates getter and setter</summary>
    Accessor
}

/// <summary>
/// Ruby method visibility modifier: public, private, protected
/// Can be used as statement (affects all following methods) or with symbol arguments.
/// </summary>
public class RubyVisibilityModifier : StatementNode
{
    /// <summary>The visibility level</summary>
    public required RubyVisibilityLevel Level { get; init; }

    /// <summary>Specific method names (if used with arguments)</summary>
    public IReadOnlyList<string> MethodNames { get; init; } = [];

    /// <summary>Whether this modifies all following methods (no arguments)</summary>
    public bool IsBlockModifier => MethodNames.Count == 0;

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Ruby visibility levels.
/// </summary>
public enum RubyVisibilityLevel
{
    /// <summary>Public methods</summary>
    Public,
    /// <summary>Protected methods</summary>
    Protected,
    /// <summary>Private methods</summary>
    Private
}

#endregion
