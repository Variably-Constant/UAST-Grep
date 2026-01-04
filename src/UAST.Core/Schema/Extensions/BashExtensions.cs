using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Core.Schema.Extensions;

#region Here-Documents

/// <summary>
/// Bash here-document: &lt;&lt;EOF, &lt;&lt;'EOF', &lt;&lt;-EOF, &lt;&lt;&lt;"string"
/// </summary>
public class BashHeredoc : ExpressionNode
{
    /// <summary>The delimiter word (e.g., EOF, END, SQL)</summary>
    public required string Delimiter { get; init; }

    /// <summary>The heredoc content</summary>
    public required string Content { get; init; }

    /// <summary>The type of heredoc (affects quoting/expansion behavior)</summary>
    public BashHeredocType Type { get; init; } = BashHeredocType.Unquoted;

    /// <summary>Whether this is a tab-stripping heredoc (&lt;&lt;-)</summary>
    public bool IsTabStripped { get; init; }

    /// <summary>For interpolated heredocs, the expansion parts</summary>
    public IReadOnlyList<BashExpansionPart> Parts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

/// <summary>
/// Bash heredoc types based on delimiter quoting.
/// </summary>
public enum BashHeredocType
{
    /// <summary>Unquoted delimiter (&lt;&lt;EOF) - variable and command expansion performed</summary>
    Unquoted,
    /// <summary>Single-quoted delimiter (&lt;&lt;'EOF') - no expansion</summary>
    SingleQuoted,
    /// <summary>Double-quoted delimiter (&lt;&lt;"EOF") - expansion performed</summary>
    DoubleQuoted,
    /// <summary>Backslash-escaped delimiter (&lt;&lt;\EOF) - no expansion</summary>
    Escaped
}

/// <summary>
/// Bash here-string: &lt;&lt;&lt;"string"
/// </summary>
public class BashHerestring : ExpressionNode
{
    /// <summary>The string expression</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

#endregion

#region Process Substitution

/// <summary>
/// Bash process substitution: &lt;(command) or &gt;(command)
/// </summary>
public class BashProcessSubstitution : ExpressionNode
{
    /// <summary>The command to execute</summary>
    public required UastNode Command { get; init; }

    /// <summary>The direction of substitution</summary>
    public ProcessSubstitutionDirection Direction { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Command];
}

/// <summary>
/// Direction of process substitution.
/// </summary>
public enum ProcessSubstitutionDirection
{
    /// <summary>Input substitution: &lt;(cmd) - command output becomes a readable file</summary>
    Input,
    /// <summary>Output substitution: &gt;(cmd) - writable file becomes command input</summary>
    Output
}

#endregion

#region Parameter Expansion

/// <summary>
/// Bash parameter expansion: ${var}, ${var:-default}, ${var:+alt}, ${#var}, etc.
/// </summary>
public class BashParameterExpansion : ExpressionNode
{
    /// <summary>The variable name being expanded</summary>
    public required string VariableName { get; init; }

    /// <summary>The type of parameter expansion</summary>
    public ParameterExpansionType ExpansionType { get; init; } = ParameterExpansionType.Simple;

    /// <summary>The operator used (e.g., -, :-, +, :+, =, :=, ?, :?, #, ##, %, %%, /, //)</summary>
    public string? Operator { get; init; }

    /// <summary>The word/pattern argument for the expansion</summary>
    public ExpressionNode? Argument { get; init; }

    /// <summary>For substring expansion, the offset</summary>
    public ExpressionNode? Offset { get; init; }

    /// <summary>For substring expansion, the length</summary>
    public ExpressionNode? Length { get; init; }

    /// <summary>Whether the colon is present (affects null vs unset behavior)</summary>
    public bool HasColon { get; init; }

    /// <summary>For case modification, the type of modification</summary>
    public CaseModificationType? CaseModification { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Argument != null) children.Add(Argument);
        if (Offset != null) children.Add(Offset);
        if (Length != null) children.Add(Length);
        return children;
    }
}

/// <summary>
/// Types of bash parameter expansion.
/// </summary>
public enum ParameterExpansionType
{
    /// <summary>Simple expansion: ${var}</summary>
    Simple,
    /// <summary>Default value: ${var:-word} or ${var-word}</summary>
    DefaultValue,
    /// <summary>Assign default: ${var:=word} or ${var=word}</summary>
    AssignDefault,
    /// <summary>Error if unset: ${var:?word} or ${var?word}</summary>
    ErrorIfUnset,
    /// <summary>Alternate value: ${var:+word} or ${var+word}</summary>
    AlternateValue,
    /// <summary>String length: ${#var}</summary>
    Length,
    /// <summary>Substring: ${var:offset} or ${var:offset:length}</summary>
    Substring,
    /// <summary>Remove shortest prefix: ${var#pattern}</summary>
    RemoveShortestPrefix,
    /// <summary>Remove longest prefix: ${var##pattern}</summary>
    RemoveLongestPrefix,
    /// <summary>Remove shortest suffix: ${var%pattern}</summary>
    RemoveShortestSuffix,
    /// <summary>Remove longest suffix: ${var%%pattern}</summary>
    RemoveLongestSuffix,
    /// <summary>Replace first match: ${var/pattern/replacement}</summary>
    ReplaceFirst,
    /// <summary>Replace all matches: ${var//pattern/replacement}</summary>
    ReplaceAll,
    /// <summary>Replace prefix: ${var/#pattern/replacement}</summary>
    ReplacePrefix,
    /// <summary>Replace suffix: ${var/%pattern/replacement}</summary>
    ReplaceSuffix,
    /// <summary>Case modification: ${var^}, ${var^^}, ${var,}, ${var,,}</summary>
    CaseModification,
    /// <summary>Indirect expansion: ${!var}</summary>
    Indirect,
    /// <summary>Array keys/indices: ${!array[@]} or ${!array[*]}</summary>
    ArrayKeys,
    /// <summary>Variable name matching: ${!prefix*} or ${!prefix@}</summary>
    NameMatching
}

/// <summary>
/// Types of case modification in parameter expansion.
/// </summary>
public enum CaseModificationType
{
    /// <summary>Uppercase first character: ${var^}</summary>
    UppercaseFirst,
    /// <summary>Uppercase all: ${var^^}</summary>
    UppercaseAll,
    /// <summary>Lowercase first character: ${var,}</summary>
    LowercaseFirst,
    /// <summary>Lowercase all: ${var,,}</summary>
    LowercaseAll
}

/// <summary>
/// Represents a part in an interpolated string/heredoc.
/// </summary>
public class BashExpansionPart : UastNode
{
    /// <summary>The type of this part</summary>
    public ExpansionPartType PartType { get; init; }

    /// <summary>For literal parts, the text content</summary>
    public string? Text { get; init; }

    /// <summary>For expansion parts, the expression</summary>
    public ExpressionNode? Expression { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Expression != null ? [Expression] : [];
    }
}

/// <summary>
/// Types of expansion parts.
/// </summary>
public enum ExpansionPartType
{
    /// <summary>Literal text content</summary>
    Literal,
    /// <summary>Variable expansion ($var or ${var})</summary>
    Variable,
    /// <summary>Command substitution $(cmd) or `cmd`</summary>
    Command,
    /// <summary>Arithmetic expansion $((expr))</summary>
    Arithmetic
}

#endregion

#region Arrays

/// <summary>
/// Bash array declaration: arr=(elem1 elem2 elem3)
/// </summary>
public class BashArrayDeclaration : StatementNode
{
    /// <summary>The array variable name</summary>
    public required string Name { get; init; }

    /// <summary>The array elements</summary>
    public required IReadOnlyList<BashArrayElement> Elements { get; init; }

    /// <summary>Whether this is an associative array (declare -A)</summary>
    public bool IsAssociative { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Bash array element: value or [index]=value
/// </summary>
public class BashArrayElement : UastNode
{
    /// <summary>The element value</summary>
    public required ExpressionNode Value { get; init; }

    /// <summary>The index/key (null for sequential elements)</summary>
    public ExpressionNode? Index { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Index != null) children.Add(Index);
        children.Add(Value);
        return children;
    }
}

/// <summary>
/// Bash array access: ${arr[index]}, ${arr[@]}, ${arr[*]}
/// </summary>
public class BashArrayAccess : ExpressionNode
{
    /// <summary>The array variable name</summary>
    public required string ArrayName { get; init; }

    /// <summary>The index expression</summary>
    public required ExpressionNode Index { get; init; }

    /// <summary>Whether this is an "all elements" access (@ or *)</summary>
    public bool IsAllElements { get; init; }

    /// <summary>Whether @ was used (true) vs * (false) for all elements</summary>
    public bool UsesAt { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Index];
}

/// <summary>
/// Bash array length: ${#arr[@]} or ${#arr[*]}
/// </summary>
public class BashArrayLength : ExpressionNode
{
    /// <summary>The array variable name</summary>
    public required string ArrayName { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Bash array slice: ${arr[@]:offset:length}
/// </summary>
public class BashArraySlice : ExpressionNode
{
    /// <summary>The array variable name</summary>
    public required string ArrayName { get; init; }

    /// <summary>The offset expression</summary>
    public required ExpressionNode Offset { get; init; }

    /// <summary>The length expression (optional)</summary>
    public ExpressionNode? Length { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Offset };
        if (Length != null) children.Add(Length);
        return children;
    }
}

#endregion

#region Test Expressions

/// <summary>
/// Bash test expression: [[ condition ]]
/// </summary>
public class BashTestExpression : ExpressionNode
{
    /// <summary>The condition being tested</summary>
    public required ExpressionNode Condition { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Condition];
}

/// <summary>
/// Bash file test: -e file, -f file, -d file, etc.
/// </summary>
public class BashFileTest : ExpressionNode
{
    /// <summary>The test operator (-e, -f, -d, -r, -w, -x, etc.)</summary>
    public required string Operator { get; init; }

    /// <summary>The file path expression</summary>
    public required ExpressionNode File { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [File];
}

/// <summary>
/// Bash string test: -z str, -n str, str == str, str =~ regex, etc.
/// </summary>
public class BashStringTest : ExpressionNode
{
    /// <summary>The test operator (-z, -n, ==, !=, =~, &lt;, &gt;)</summary>
    public required string Operator { get; init; }

    /// <summary>The left operand (or only operand for unary tests)</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right operand (null for unary tests)</summary>
    public ExpressionNode? Right { get; init; }

    /// <summary>Whether this is a regex match (=~)</summary>
    public bool IsRegexMatch { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Left };
        if (Right != null) children.Add(Right);
        return children;
    }
}

/// <summary>
/// Bash arithmetic test: -eq, -ne, -lt, -le, -gt, -ge
/// </summary>
public class BashArithmeticTest : ExpressionNode
{
    /// <summary>The test operator (-eq, -ne, -lt, -le, -gt, -ge)</summary>
    public required string Operator { get; init; }

    /// <summary>The left operand</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right operand</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Bash compound test with logical operators: [[ a && b ]], [[ a || b ]], [[ ! a ]]
/// </summary>
public class BashCompoundTest : ExpressionNode
{
    /// <summary>The logical operator (&&, ||, !)</summary>
    public required string Operator { get; init; }

    /// <summary>The left operand (or only operand for unary !)</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right operand (null for unary !)</summary>
    public ExpressionNode? Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode> { Left };
        if (Right != null) children.Add(Right);
        return children;
    }
}

#endregion

#region Arithmetic Expansion

/// <summary>
/// Bash arithmetic expansion: $((expression)) or ((expression))
/// </summary>
public class BashArithmeticExpansion : ExpressionNode
{
    /// <summary>The arithmetic expression</summary>
    public required ExpressionNode Expression { get; init; }

    /// <summary>Whether this is a statement form (( )) vs expansion form $(( ))</summary>
    public bool IsStatement { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Expression];
}

/// <summary>
/// Bash arithmetic binary expression: a + b, a - b, etc.
/// </summary>
public class BashArithmeticBinary : ExpressionNode
{
    /// <summary>The operator (+, -, *, /, %, **, &, |, ^, &lt;&lt;, &gt;&gt;, etc.)</summary>
    public required string Operator { get; init; }

    /// <summary>The left operand</summary>
    public required ExpressionNode Left { get; init; }

    /// <summary>The right operand</summary>
    public required ExpressionNode Right { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Left, Right];
}

/// <summary>
/// Bash arithmetic unary expression: ++a, --a, a++, a--, !a, ~a
/// </summary>
public class BashArithmeticUnary : ExpressionNode
{
    /// <summary>The operator (++, --, !, ~, -, +)</summary>
    public required string Operator { get; init; }

    /// <summary>The operand</summary>
    public required ExpressionNode Operand { get; init; }

    /// <summary>Whether the operator is a prefix (true) or postfix (false)</summary>
    public bool IsPrefix { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Operand];
}

/// <summary>
/// Bash arithmetic ternary expression: condition ? true_val : false_val
/// </summary>
public class BashArithmeticTernary : ExpressionNode
{
    /// <summary>The condition</summary>
    public required ExpressionNode Condition { get; init; }

    /// <summary>The value if true</summary>
    public required ExpressionNode TrueValue { get; init; }

    /// <summary>The value if false</summary>
    public required ExpressionNode FalseValue { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Condition, TrueValue, FalseValue];
}

/// <summary>
/// Bash arithmetic assignment: var=expr, var+=expr, var-=expr, etc.
/// </summary>
public class BashArithmeticAssignment : ExpressionNode
{
    /// <summary>The variable name</summary>
    public required string VariableName { get; init; }

    /// <summary>The assignment operator (=, +=, -=, *=, /=, %=, &lt;&lt;=, &gt;&gt;=, &=, |=, ^=)</summary>
    public required string Operator { get; init; }

    /// <summary>The value expression</summary>
    public required ExpressionNode Value { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Value];
}

/// <summary>
/// Bash arithmetic comma expression: expr1, expr2, expr3
/// </summary>
public class BashArithmeticComma : ExpressionNode
{
    /// <summary>The expressions in the comma sequence</summary>
    public required IReadOnlyList<ExpressionNode> Expressions { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Expressions.Cast<UastNode>().ToList();
}

#endregion

#region Command Substitution

/// <summary>
/// Bash command substitution: $(command) or `command`
/// </summary>
public class BashCommandSubstitution : ExpressionNode
{
    /// <summary>The command to execute</summary>
    public required UastNode Command { get; init; }

    /// <summary>Whether backtick syntax was used (true) vs $() syntax (false)</summary>
    public bool IsBacktickSyntax { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Command];
}

#endregion

#region Brace Expansion

/// <summary>
/// Bash brace expansion: {a,b,c} or {1..10} or {01..10..2}
/// </summary>
public class BashBraceExpansion : ExpressionNode
{
    /// <summary>The type of brace expansion</summary>
    public BraceExpansionType Type { get; init; }

    /// <summary>For list expansion, the items</summary>
    public IReadOnlyList<ExpressionNode> Items { get; init; } = [];

    /// <summary>For sequence expansion, the start value</summary>
    public ExpressionNode? Start { get; init; }

    /// <summary>For sequence expansion, the end value</summary>
    public ExpressionNode? End { get; init; }

    /// <summary>For sequence expansion, the optional increment</summary>
    public ExpressionNode? Increment { get; init; }

    /// <summary>Optional prefix before the brace expansion</summary>
    public ExpressionNode? Prefix { get; init; }

    /// <summary>Optional suffix after the brace expansion</summary>
    public ExpressionNode? Suffix { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        if (Prefix != null) children.Add(Prefix);
        children.AddRange(Items);
        if (Start != null) children.Add(Start);
        if (End != null) children.Add(End);
        if (Increment != null) children.Add(Increment);
        if (Suffix != null) children.Add(Suffix);
        return children;
    }
}

/// <summary>
/// Types of brace expansion.
/// </summary>
public enum BraceExpansionType
{
    /// <summary>List expansion: {a,b,c}</summary>
    List,
    /// <summary>Numeric sequence: {1..10}</summary>
    NumericSequence,
    /// <summary>Alphabetic sequence: {a..z}</summary>
    AlphabeticSequence
}

#endregion

#region Glob Patterns

/// <summary>
/// Bash glob pattern: *, ?, [...], **
/// </summary>
public class BashGlobPattern : ExpressionNode
{
    /// <summary>The raw pattern string</summary>
    public required string Pattern { get; init; }

    /// <summary>The type of glob pattern</summary>
    public GlobPatternType Type { get; init; }

    /// <summary>For character class patterns, the characters/ranges</summary>
    public IReadOnlyList<GlobCharacterRange> CharacterRanges { get; init; } = [];

    /// <summary>Whether the pattern is negated ([!...] or [^...])</summary>
    public bool IsNegated { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        CharacterRanges.Cast<UastNode>().ToList();
}

/// <summary>
/// Types of glob patterns.
/// </summary>
public enum GlobPatternType
{
    /// <summary>Match any string: *</summary>
    Star,
    /// <summary>Match any single character: ?</summary>
    Question,
    /// <summary>Match characters in a class: [abc] or [a-z]</summary>
    CharacterClass,
    /// <summary>Match any path including directories: **</summary>
    GlobStar
}

/// <summary>
/// Character range in a glob character class.
/// </summary>
public class GlobCharacterRange : UastNode
{
    /// <summary>The start character (or single character if not a range)</summary>
    public required char Start { get; init; }

    /// <summary>The end character (null if not a range)</summary>
    public char? End { get; init; }

    /// <summary>Whether this is a range (a-z) vs single character</summary>
    public bool IsRange => End.HasValue;

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Bash extended glob pattern: ?(pattern), *(pattern), +(pattern), @(pattern), !(pattern)
/// </summary>
public class BashExtendedGlob : ExpressionNode
{
    /// <summary>The type of extended glob</summary>
    public ExtendedGlobType Type { get; init; }

    /// <summary>The pattern alternatives</summary>
    public required IReadOnlyList<ExpressionNode> Patterns { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Patterns.Cast<UastNode>().ToList();
}

/// <summary>
/// Types of extended glob patterns.
/// </summary>
public enum ExtendedGlobType
{
    /// <summary>Zero or one match: ?(pattern)</summary>
    ZeroOrOne,
    /// <summary>Zero or more matches: *(pattern)</summary>
    ZeroOrMore,
    /// <summary>One or more matches: +(pattern)</summary>
    OneOrMore,
    /// <summary>Exactly one match: @(pattern)</summary>
    ExactlyOne,
    /// <summary>Negated match: !(pattern)</summary>
    Negated
}

#endregion

#region Coprocess

/// <summary>
/// Bash coprocess: coproc [NAME] command
/// </summary>
public class BashCoprocess : StatementNode
{
    /// <summary>The optional coprocess name</summary>
    public string? Name { get; init; }

    /// <summary>The command to run as a coprocess</summary>
    public required UastNode Command { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Command];
}

#endregion

#region Function Definition

/// <summary>
/// Bash function definition: function name { } or name() { }
/// </summary>
public class BashFunctionDefinition : DeclarationNode
{
    /// <summary>The function body</summary>
    public required BlockNode Body { get; init; }

    /// <summary>Whether the 'function' keyword was used</summary>
    public bool UsesFunctionKeyword { get; init; }

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

#region Special Variables

/// <summary>
/// Bash special variable: $?, $!, $$, $0, $1-$9, $*, $@, $#, $-
/// </summary>
public class BashSpecialVariable : ExpressionNode
{
    /// <summary>The variable identifier (without $)</summary>
    public required string Identifier { get; init; }

    /// <summary>The type of special variable</summary>
    public SpecialVariableType Type { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Types of bash special variables.
/// </summary>
public enum SpecialVariableType
{
    /// <summary>Exit status of last command: $?</summary>
    ExitStatus,
    /// <summary>PID of last background command: $!</summary>
    BackgroundPid,
    /// <summary>Current shell PID: $$</summary>
    ShellPid,
    /// <summary>Script name: $0</summary>
    ScriptName,
    /// <summary>Positional parameter: $1-$9</summary>
    PositionalParameter,
    /// <summary>All positional parameters as single word: $*</summary>
    AllParametersSingle,
    /// <summary>All positional parameters as separate words: $@</summary>
    AllParametersSeparate,
    /// <summary>Number of positional parameters: $#</summary>
    ParameterCount,
    /// <summary>Current shell options: $-</summary>
    ShellOptions,
    /// <summary>PID of current shell (for subshells): $BASHPID</summary>
    BashPid,
    /// <summary>Parent PID: $PPID</summary>
    ParentPid,
    /// <summary>Seconds since shell start: $SECONDS</summary>
    Seconds,
    /// <summary>Random number: $RANDOM</summary>
    Random,
    /// <summary>Line number: $LINENO</summary>
    LineNumber,
    /// <summary>Other shell variable</summary>
    Other
}

#endregion

#region Pipeline and List Constructs

/// <summary>
/// Bash pipeline: cmd1 | cmd2 | cmd3
/// </summary>
public class BashPipeline : ExpressionNode
{
    /// <summary>The commands in the pipeline</summary>
    public required IReadOnlyList<UastNode> Commands { get; init; }

    /// <summary>Whether negation is applied to the pipeline exit status (! pipeline)</summary>
    public bool IsNegated { get; init; }

    /// <summary>Whether to use pipefail (affects exit status behavior)</summary>
    public bool UsesTime { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Commands.Cast<UastNode>().ToList();
}

/// <summary>
/// Bash command list with operators: cmd1 && cmd2 || cmd3 ; cmd4
/// </summary>
public class BashCommandList : ExpressionNode
{
    /// <summary>The commands and operators in order</summary>
    public required IReadOnlyList<BashListElement> Elements { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Elements.Cast<UastNode>().ToList();
}

/// <summary>
/// Element in a bash command list.
/// </summary>
public class BashListElement : UastNode
{
    /// <summary>The command</summary>
    public required UastNode Command { get; init; }

    /// <summary>The operator following this command (&&, ||, ;, &amp;) - null for last command</summary>
    public string? Operator { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Command];
}

/// <summary>
/// Bash subshell: ( command )
/// </summary>
public class BashSubshell : ExpressionNode
{
    /// <summary>The command(s) to execute in a subshell</summary>
    public required UastNode Command { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Command];
}

/// <summary>
/// Bash command group: { command; }
/// </summary>
public class BashCommandGroup : ExpressionNode
{
    /// <summary>The command(s) to execute in the current shell</summary>
    public required UastNode Command { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Command];
}

#endregion

#region Redirections

/// <summary>
/// Bash file descriptor redirection: n&gt;file, n&gt;&gt;file, n&lt;file, etc.
/// </summary>
public class BashRedirection : UastNode
{
    /// <summary>The source file descriptor (default 1 for output, 0 for input)</summary>
    public int? SourceFd { get; init; }

    /// <summary>The redirection operator</summary>
    public required RedirectionOperator Operator { get; init; }

    /// <summary>The target (file path, fd, or here-doc delimiter)</summary>
    public required ExpressionNode Target { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [Target];
}

/// <summary>
/// Bash redirection operators.
/// </summary>
public enum RedirectionOperator
{
    /// <summary>Redirect output: &gt;</summary>
    Output,
    /// <summary>Append output: &gt;&gt;</summary>
    Append,
    /// <summary>Redirect input: &lt;</summary>
    Input,
    /// <summary>Here-document: &lt;&lt;</summary>
    Heredoc,
    /// <summary>Here-string: &lt;&lt;&lt;</summary>
    Herestring,
    /// <summary>Duplicate output fd: &gt;&amp;</summary>
    DuplicateOutput,
    /// <summary>Duplicate input fd: &lt;&amp;</summary>
    DuplicateInput,
    /// <summary>Open for read/write: &lt;&gt;</summary>
    ReadWrite,
    /// <summary>Redirect both stdout and stderr: &amp;&gt;</summary>
    OutputAll,
    /// <summary>Append both stdout and stderr: &amp;&gt;&gt;</summary>
    AppendAll,
    /// <summary>Pipe stdout and stderr: |&amp;</summary>
    PipeAll,
    /// <summary>Clobber (force overwrite): &gt;|</summary>
    Clobber
}

/// <summary>
/// Bash file descriptor manipulation: exec n&gt;file, n&lt;&amp;-, etc.
/// </summary>
public class BashFdManipulation : StatementNode
{
    /// <summary>The file descriptor number</summary>
    public required int Fd { get; init; }

    /// <summary>The manipulation type</summary>
    public FdManipulationType Type { get; init; }

    /// <summary>The target (file, fd, or null for close)</summary>
    public ExpressionNode? Target { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Target != null ? [Target] : [];
    }
}

/// <summary>
/// Types of file descriptor manipulation.
/// </summary>
public enum FdManipulationType
{
    /// <summary>Open for output</summary>
    OpenOutput,
    /// <summary>Open for append</summary>
    OpenAppend,
    /// <summary>Open for input</summary>
    OpenInput,
    /// <summary>Open for read/write</summary>
    OpenReadWrite,
    /// <summary>Duplicate from another fd</summary>
    Duplicate,
    /// <summary>Close the fd</summary>
    Close,
    /// <summary>Move fd (close source after dup)</summary>
    Move
}

#endregion

#region Select Statement

/// <summary>
/// Bash select statement: select name in words; do commands; done
/// </summary>
public class BashSelectStatement : StatementNode
{
    /// <summary>The loop variable name</summary>
    public required string VariableName { get; init; }

    /// <summary>The words to select from</summary>
    public required IReadOnlyList<ExpressionNode> Words { get; init; }

    /// <summary>The loop body</summary>
    public required BlockNode Body { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        var children = new List<UastNode>();
        children.AddRange(Words);
        children.Add(Body);
        return children;
    }
}

#endregion

#region Trap

/// <summary>
/// Bash trap command: trap 'commands' SIGNAL
/// </summary>
public class BashTrap : StatementNode
{
    /// <summary>The command to execute (or special values like - or "")</summary>
    public ExpressionNode? Command { get; init; }

    /// <summary>The signals to trap</summary>
    public required IReadOnlyList<string> Signals { get; init; }

    /// <summary>Whether this resets the trap to default (-)</summary>
    public bool IsReset { get; init; }

    /// <summary>Whether this ignores the signal ("")</summary>
    public bool IsIgnore { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren()
    {
        return Command != null ? [Command] : [];
    }
}

#endregion

#region ANSI-C Quoting

/// <summary>
/// Bash ANSI-C quoted string: $'string with \n escapes'
/// </summary>
public class BashAnsiCString : ExpressionNode
{
    /// <summary>The raw string content (with escapes unprocessed)</summary>
    public required string RawContent { get; init; }

    /// <summary>The interpreted string content (with escapes processed)</summary>
    public required string InterpretedContent { get; init; }

    protected override IReadOnlyList<UastNode> GetChildren() => [];
}

/// <summary>
/// Bash locale-translated string: $"string"
/// </summary>
public class BashLocalizedString : ExpressionNode
{
    /// <summary>The string content</summary>
    public required string Content { get; init; }

    /// <summary>For interpolated strings, the parts</summary>
    public IReadOnlyList<BashExpansionPart> Parts { get; init; } = [];

    protected override IReadOnlyList<UastNode> GetChildren() =>
        Parts.Cast<UastNode>().ToList();
}

#endregion
