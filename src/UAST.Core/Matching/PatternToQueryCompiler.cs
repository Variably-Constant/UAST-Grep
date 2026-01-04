using System.Collections.Frozen;
using System.Text;

namespace UAST.Core.Matching;

/// <summary>
/// Compiles UAST Pattern objects into tree-sitter S-expression query strings.
/// This enables native code pattern matching by leveraging tree-sitter's built-in query engine.
///
/// Tree-sitter query format:
/// - Node types: (function_definition)
/// - Field access: (function_definition name: (identifier) @name)
/// - Captures: @capture_name after any node
/// - Wildcards: (_) matches any node, (_)* matches zero or more
/// - Alternation: [(type_a) (type_b)] matches either
/// - Negation: (#not-match? @capture "pattern") as a predicate
///
/// Performance: Compiled queries run in native C code, providing significant
/// speedups over managed pattern matching, especially for large codebases.
/// </summary>
public sealed class PatternToQueryCompiler
{
    private int _captureIndex;
    private readonly HashSet<string> _usedCaptures = new(StringComparer.Ordinal);

    /// <summary>
    /// Mapping from UAST node kinds to tree-sitter node types.
    /// This is a comprehensive mapping covering PowerShell, C#, Python, and other languages.
    /// </summary>
    private static readonly FrozenDictionary<string, string> UastToTreeSitter = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    {
        // Declarations
        ["FunctionDeclaration"] = "function_definition",
        ["TypeDeclaration"] = "class_definition",
        ["VariableDeclaration"] = "variable_declaration",
        ["PropertyDeclaration"] = "property_declaration",
        ["ParameterNode"] = "parameter",
        ["TypeParameterNode"] = "type_parameter",
        ["AttributeNode"] = "attribute",
        ["ModifierNode"] = "modifier",
        ["EnumMemberDeclaration"] = "enum_member",

        // Statements
        ["BlockNode"] = "block",
        ["ExpressionStatement"] = "expression_statement",
        ["IfStatement"] = "if_statement",
        ["WhileStatement"] = "while_statement",
        ["DoWhileStatement"] = "do_statement",
        ["ForStatement"] = "for_statement",
        ["ForEachStatement"] = "foreach_statement",
        ["SwitchStatement"] = "switch_statement",
        ["SwitchCase"] = "case_clause",
        ["TryStatement"] = "try_statement",
        ["CatchClause"] = "catch_clause",
        ["ReturnStatement"] = "return_statement",
        ["BreakStatement"] = "break_statement",
        ["ContinueStatement"] = "continue_statement",
        ["ThrowStatement"] = "throw_statement",
        ["UsingStatement"] = "using_statement",
        ["LockStatement"] = "lock_statement",
        ["YieldStatement"] = "yield_statement",
        ["GotoStatement"] = "goto_statement",
        ["LabeledStatement"] = "labeled_statement",
        ["CheckedStatement"] = "checked_statement",
        ["UnsafeStatement"] = "unsafe_statement",
        ["FixedStatement"] = "fixed_statement",
        ["EmptyStatement"] = "empty_statement",

        // Expressions
        ["LiteralExpression"] = "literal",
        ["IdentifierExpression"] = "identifier",
        ["VariableExpression"] = "variable",
        ["BinaryExpression"] = "binary_expression",
        ["UnaryExpression"] = "unary_expression",
        ["AssignmentExpression"] = "assignment_expression",
        ["CallExpression"] = "call_expression",
        ["MemberExpression"] = "member_expression",
        ["IndexExpression"] = "index_expression",
        ["NewExpression"] = "object_creation_expression",
        ["ArrayExpression"] = "array_expression",
        ["ObjectExpression"] = "object_expression",
        ["PropertyNode"] = "property",
        ["ConditionalExpression"] = "conditional_expression",
        ["LambdaExpression"] = "lambda_expression",
        ["CastExpression"] = "cast_expression",
        ["TypeCheckExpression"] = "is_expression",
        ["InterpolatedStringExpression"] = "interpolated_string_expression",
        ["InterpolatedStringPart"] = "interpolated_string_content",
        ["TupleExpression"] = "tuple_expression",
        ["ParenthesizedExpression"] = "parenthesized_expression",
        ["AwaitExpression"] = "await_expression",
        ["TypeExpression"] = "type_expression",
        ["InvokeMemberExpression"] = "invocation_expression",
        ["ArgumentNode"] = "argument",

        // PowerShell-specific
        ["CommandExpression"] = "command",
        ["PipelineExpression"] = "pipeline",
        ["HashtableExpression"] = "hash_literal_expression",
        ["ScriptBlockExpression"] = "script_block_expression",
        ["SplatExpression"] = "splatted_variable",
        ["ExpandableStringExpression"] = "expandable_string_literal",
        ["SubExpressionNode"] = "sub_expression",

        // C#-specific
        ["LinqExpression"] = "query_expression",
        ["PointerExpression"] = "pointer_expression",
        ["StackAllocExpression"] = "stackalloc_expression",
        ["RefExpression"] = "ref_expression",

        // Types
        ["NamedTypeReference"] = "type_identifier",
        ["ArrayTypeReference"] = "array_type",
        ["GenericTypeReference"] = "generic_name",
        ["TupleTypeReference"] = "tuple_type",
        ["UnionTypeReference"] = "union_type",
        ["FunctionTypeReference"] = "function_type",
        ["NullableTypeReference"] = "nullable_type",

        // Generic fallbacks
        ["UnknownExpression"] = "_"
    }.ToFrozenDictionary(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Mapping from UAST property names to tree-sitter field names.
    /// </summary>
    private static readonly FrozenDictionary<string, string> PropertyToField = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
    {
        ["Name"] = "name",
        ["CommandName"] = "name",
        ["Value"] = "value",
        ["Operator"] = "operator",
        ["Left"] = "left",
        ["Right"] = "right",
        ["Condition"] = "condition",
        ["Consequence"] = "consequence",
        ["Alternative"] = "alternative",
        ["Body"] = "body",
        ["Parameters"] = "parameters",
        ["Arguments"] = "arguments",
        ["ReturnType"] = "return_type",
        ["Type"] = "type",
        ["Target"] = "object",
        ["Member"] = "name",
        ["Index"] = "index",
        ["Initializer"] = "initializer",
        ["Iterator"] = "iterator",
        ["Iterable"] = "right",
        ["Expression"] = "expression",
        ["Operand"] = "operand",
        ["Handler"] = "handler",
        ["Finally"] = "finally_clause"
    }.ToFrozenDictionary(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Compiles a UAST Pattern into a tree-sitter query string.
    /// </summary>
    /// <param name="pattern">The UAST pattern to compile.</param>
    /// <returns>A compilation result containing the query string and metadata.</returns>
    public QueryCompilationResult Compile(Pattern pattern)
    {
        _captureIndex = 0;
        _usedCaptures.Clear();

        var sb = new StringBuilder();
        var rootCapture = CompileNode(pattern.Root, sb, isRoot: true);

        return new QueryCompilationResult
        {
            Query = sb.ToString(),
            RootCapture = rootCapture,
            MetaVariableMappings = BuildMetaVariableMappings(pattern),
            Success = true
        };
    }

    /// <summary>
    /// Compiles a PatternNode into part of a tree-sitter query.
    /// </summary>
    /// <param name="node">The pattern node to compile.</param>
    /// <param name="sb">The StringBuilder to append to.</param>
    /// <param name="isRoot">Whether this is the root pattern node.</param>
    /// <returns>The capture name if one was generated.</returns>
    private string? CompileNode(PatternNode node, StringBuilder sb, bool isRoot = false)
    {
        return node switch
        {
            StructuralPattern sp => CompileStructural(sp, sb, isRoot),
            MetaVariable mv => CompileMetaVariable(mv, sb),
            LiteralPattern lp => CompileLiteral(lp, sb),
            WildcardPattern => CompileWildcard(sb),
            AlternativePattern ap => CompileAlternative(ap, sb),
            SequencePattern seq => CompileSequence(seq, sb),
            ConjunctionPattern conj => CompileConjunction(conj, sb),
            NegationPattern neg => CompileNegation(neg, sb),
            _ => CompileUnknown(node, sb)
        };
    }

    /// <summary>
    /// Compiles a structural pattern into a tree-sitter node pattern.
    /// </summary>
    private string? CompileStructural(StructuralPattern sp, StringBuilder sb, bool isRoot)
    {
        var nodeType = sp.NodeKind != null
            ? MapNodeKind(sp.NodeKind)
            : "_";

        sb.Append('(');
        sb.Append(nodeType);

        // Add field constraints from properties
        foreach (var (propName, propPattern) in sp.Properties)
        {
            var fieldName = MapPropertyToField(propName);
            sb.Append(' ');
            sb.Append(fieldName);
            sb.Append(": ");
            CompileNode(propPattern, sb);
        }

        // Add child patterns
        foreach (var child in sp.Children)
        {
            sb.Append(' ');
            CompileNode(child, sb);
        }

        sb.Append(')');

        // Add capture for root node
        string? capture = null;
        if (isRoot)
        {
            capture = "@match";
            sb.Append(' ');
            sb.Append(capture);
        }

        return capture;
    }

    /// <summary>
    /// Compiles a metavariable into a capture pattern.
    /// </summary>
    private string? CompileMetaVariable(MetaVariable mv, StringBuilder sb)
    {
        string capture;

        if (mv.IsAnonymous)
        {
            // Anonymous metavariables still need unique capture names internally
            capture = $"@_anon{_captureIndex++}";
        }
        else
        {
            // Convert metavariable name to lowercase for tree-sitter conventions
            capture = $"@{mv.Name.ToLowerInvariant()}";
        }

        // Handle type constraint if present
        if (mv.TypeConstraint != null)
        {
            var nodeType = MapNodeKind(mv.TypeConstraint);
            sb.Append('(');
            sb.Append(nodeType);
            sb.Append(')');
        }
        else if (mv.Kind == MetaVarKind.Multiple)
        {
            // Zero or more nodes
            sb.Append("(_)*");
        }
        else if (mv.Kind == MetaVarKind.AtLeastOne)
        {
            // One or more nodes
            sb.Append("(_)+");
        }
        else
        {
            // Single node wildcard
            sb.Append("(_)");
        }

        // Add capture
        sb.Append(' ');
        sb.Append(capture);
        _usedCaptures.Add(capture);

        return capture;
    }

    /// <summary>
    /// Compiles a literal pattern into a tree-sitter pattern with predicate.
    /// </summary>
    private string? CompileLiteral(LiteralPattern lp, StringBuilder sb)
    {
        var capture = $"@_lit{_captureIndex++}";

        // Literals match any node but we add a predicate to check the value
        sb.Append("(_) ");
        sb.Append(capture);

        // Add a predicate to match the literal value
        if (lp.Value != null)
        {
            var escaped = EscapeString(lp.Value.ToString() ?? "");
            sb.Append($" (#eq? {capture} \"{escaped}\")");
        }

        _usedCaptures.Add(capture);
        return capture;
    }

    /// <summary>
    /// Compiles a wildcard pattern.
    /// </summary>
    private string? CompileWildcard(StringBuilder sb)
    {
        sb.Append("(_)");
        return null;
    }

    /// <summary>
    /// Compiles an alternative pattern using tree-sitter's choice syntax.
    /// </summary>
    private string? CompileAlternative(AlternativePattern ap, StringBuilder sb)
    {
        sb.Append('[');

        bool first = true;
        foreach (var alt in ap.Alternatives)
        {
            if (!first)
                sb.Append(' ');
            first = false;

            CompileNode(alt, sb);
        }

        sb.Append(']');

        var capture = $"@alt{_captureIndex++}";
        sb.Append(' ');
        sb.Append(capture);
        _usedCaptures.Add(capture);

        return capture;
    }

    /// <summary>
    /// Compiles a sequence pattern.
    /// </summary>
    private string? CompileSequence(SequencePattern seq, StringBuilder sb)
    {
        // Sequences become adjacent siblings in tree-sitter
        foreach (var element in seq.Elements)
        {
            CompileNode(element, sb);
            sb.Append(' ');
        }

        return null;
    }

    /// <summary>
    /// Compiles a conjunction pattern (all must match).
    /// Tree-sitter doesn't have direct AND support, so we use predicates.
    /// </summary>
    private string? CompileConjunction(ConjunctionPattern conj, StringBuilder sb)
    {
        // For conjunctions, we compile the first pattern and add predicates for others
        if (conj.Patterns.Count == 0)
        {
            sb.Append("(_)");
            return null;
        }

        var capture = CompileNode(conj.Patterns[0], sb);

        // Additional patterns become predicates (limited support in tree-sitter)
        // Note: Tree-sitter predicates are post-match filters, not pattern components
        for (int i = 1; i < conj.Patterns.Count; i++)
        {
            // We can only add predicates, not full pattern matching here
            // This is a limitation of tree-sitter queries
            sb.Append(" ; conjunction-member");
        }

        return capture;
    }

    /// <summary>
    /// Compiles a negation pattern using tree-sitter's #not-match? predicate.
    /// </summary>
    private string? CompileNegation(NegationPattern neg, StringBuilder sb)
    {
        // Compile the inner pattern first to get its structure
        var innerSb = new StringBuilder();
        CompileNode(neg.Inner, innerSb);

        // Wrap with negation predicate
        var capture = $"@not{_captureIndex++}";
        sb.Append("(_) ");
        sb.Append(capture);

        // Add negation predicate
        var innerQuery = innerSb.ToString();
        var escaped = EscapeString(innerQuery);
        sb.Append($" (#not-match? {capture} \"{escaped}\")");

        _usedCaptures.Add(capture);
        return capture;
    }

    /// <summary>
    /// Handles unknown pattern types.
    /// </summary>
    private string? CompileUnknown(PatternNode node, StringBuilder sb)
    {
        // Fallback to wildcard for unknown patterns
        sb.Append($"(_) ; unknown: {node.GetType().Name}");
        return null;
    }

    /// <summary>
    /// Maps a UAST node kind to a tree-sitter node type.
    /// </summary>
    private static string MapNodeKind(string uastKind)
    {
        return UastToTreeSitter.TryGetValue(uastKind, out var tsType)
            ? tsType
            : uastKind.ToLowerInvariant().Replace("node", "").Replace("expression", "_expression");
    }

    /// <summary>
    /// Maps a UAST property name to a tree-sitter field name.
    /// </summary>
    private static string MapPropertyToField(string propertyName)
    {
        return PropertyToField.TryGetValue(propertyName, out var fieldName)
            ? fieldName
            : propertyName.ToLowerInvariant();
    }

    /// <summary>
    /// Escapes a string for use in tree-sitter query predicates.
    /// </summary>
    private static string EscapeString(string value)
    {
        return value
            .Replace("\\", "\\\\")
            .Replace("\"", "\\\"")
            .Replace("\n", "\\n")
            .Replace("\r", "\\r")
            .Replace("\t", "\\t");
    }

    /// <summary>
    /// Builds a mapping from metavariable names to their tree-sitter capture names.
    /// </summary>
    private Dictionary<string, string> BuildMetaVariableMappings(Pattern pattern)
    {
        var mappings = new Dictionary<string, string>(StringComparer.Ordinal);

        foreach (var metaVar in pattern.MetaVariables)
        {
            var captureName = $"@{metaVar.ToLowerInvariant()}";
            if (_usedCaptures.Contains(captureName))
            {
                mappings[metaVar] = captureName;
            }
        }

        return mappings;
    }
}

/// <summary>
/// Result of compiling a UAST pattern to a tree-sitter query.
/// </summary>
public class QueryCompilationResult
{
    /// <summary>
    /// The compiled tree-sitter query string.
    /// </summary>
    public required string Query { get; init; }

    /// <summary>
    /// The capture name for the root match (typically @match).
    /// </summary>
    public string? RootCapture { get; init; }

    /// <summary>
    /// Mapping from original metavariable names to tree-sitter capture names.
    /// </summary>
    public IReadOnlyDictionary<string, string> MetaVariableMappings { get; init; }
        = new Dictionary<string, string>();

    /// <summary>
    /// Whether compilation succeeded.
    /// </summary>
    public bool Success { get; init; }

    /// <summary>
    /// Error message if compilation failed.
    /// </summary>
    public string? ErrorMessage { get; init; }

    /// <summary>
    /// Warnings generated during compilation.
    /// </summary>
    public IReadOnlyList<string> Warnings { get; init; } = [];
}

/// <summary>
/// Extension methods for pattern-to-query compilation.
/// </summary>
public static class PatternToQueryExtensions
{
    private static readonly PatternToQueryCompiler DefaultCompiler = new();

    /// <summary>
    /// Compiles this pattern to a tree-sitter query string.
    /// </summary>
    /// <param name="pattern">The pattern to compile.</param>
    /// <returns>The compilation result.</returns>
    public static QueryCompilationResult ToTreeSitterQuery(this Pattern pattern)
    {
        return DefaultCompiler.Compile(pattern);
    }

    /// <summary>
    /// Gets just the query string from a pattern.
    /// </summary>
    /// <param name="pattern">The pattern to compile.</param>
    /// <returns>The tree-sitter query string.</returns>
    public static string ToQueryString(this Pattern pattern)
    {
        return DefaultCompiler.Compile(pattern).Query;
    }
}
