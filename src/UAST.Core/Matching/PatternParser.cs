using System.Text;
using System.Text.Json;
using UAST.Core.Interfaces;
using UAST.Core.Schema;

namespace UAST.Core.Matching;

/// <summary>
/// Parses pattern strings into Pattern objects.
/// Handles metavariable preprocessing and pattern tree construction.
/// </summary>
public class PatternParser
{
    /// <summary>
    /// The metavariable prefix character (∀ = ForAll, U+2200).
    /// </summary>
    public const char MetaVarPrefix = '\u2200';

    /// <summary>
    /// Alternative ASCII-friendly metavariable prefix.
    /// </summary>
    public const char MetaVarPrefixAlt = '$';

    private readonly Dictionary<string, ILanguageMapper> _mappers = new(StringComparer.OrdinalIgnoreCase);

    /// <summary>
    /// Registers a language mapper for pattern parsing.
    /// </summary>
    public void RegisterMapper(ILanguageMapper mapper)
    {
        _mappers[mapper.LanguageName] = mapper;
        foreach (var ext in mapper.FileExtensions)
        {
            _mappers[ext.TrimStart('.')] = mapper;
        }
    }

    /// <summary>
    /// Parses a pattern string for a specific language.
    /// </summary>
    /// <param name="patternSource">The pattern source text.</param>
    /// <param name="language">The target language.</param>
    /// <returns>A compiled Pattern object.</returns>
    public Pattern Parse(string patternSource, string language)
    {
        // 1. Extract metavariable names
        var metaVars = new HashSet<string>();
        var preprocessed = PreprocessMetaVariables(patternSource, metaVars);

        // 2. Parse the preprocessed pattern as the target language
        if (!_mappers.TryGetValue(language, out var mapper))
        {
            throw new ArgumentException($"No mapper registered for language: {language}");
        }

        UastNode ast;
        try
        {
            ast = mapper.Parse(preprocessed);
        }
        catch (Exception ex)
        {
            throw new PatternParseException($"Failed to parse pattern as {language}: {ex.Message}", ex);
        }

        // 3. Convert the AST to a pattern tree
        var patternRoot = ConvertToPattern(ast, metaVars);

        return new Pattern
        {
            Root = patternRoot,
            SourceText = patternSource,
            Language = language,
            MetaVariables = metaVars
        };
    }

    /// <summary>
    /// Parses a simple pattern without requiring a language mapper.
    /// Useful for simple patterns like function names or variable patterns.
    /// </summary>
    public Pattern ParseSimple(string patternSource, string language)
    {
        var metaVars = new HashSet<string>();
        var patternRoot = ParseSimplePattern(patternSource, metaVars);

        return new Pattern
        {
            Root = patternRoot,
            SourceText = patternSource,
            Language = language,
            MetaVariables = metaVars
        };
    }

    /// <summary>
    /// Preprocesses pattern source to replace metavariables with valid identifiers.
    /// </summary>
    private string PreprocessMetaVariables(string source, HashSet<string> metaVars)
    {
        var result = new StringBuilder();
        var i = 0;

        while (i < source.Length)
        {
            var c = source[i];

            // Check for metavariable prefix
            if (c == MetaVarPrefix || (c == MetaVarPrefixAlt && i + 1 < source.Length && char.IsLetter(source[i + 1])))
            {
                // Count prefix characters for multiple match (∀∀∀)
                int prefixCount = 0;
                while (i < source.Length && source[i] == c)
                {
                    prefixCount++;
                    i++;
                }

                // Collect the metavariable name
                var name = new StringBuilder();
                while (i < source.Length && (char.IsLetterOrDigit(source[i]) || source[i] == '_'))
                {
                    name.Append(source[i]);
                    i++;
                }

                if (name.Length > 0)
                {
                    var varName = name.ToString();
                    metaVars.Add(varName);

                    // Replace with placeholder identifier
                    var placeholder = prefixCount >= 3
                        ? $"{MetaVariable.Placeholder}MULTI_{varName}__"
                        : $"{MetaVariable.Placeholder}{varName}__";
                    result.Append(placeholder);
                }
                else
                {
                    // Not a valid metavariable, keep the prefix
                    result.Append(c, prefixCount);
                }
            }
            else
            {
                result.Append(c);
                i++;
            }
        }

        return result.ToString();
    }

    /// <summary>
    /// Converts a UAST node to a pattern node.
    /// </summary>
    private PatternNode ConvertToPattern(UastNode node, HashSet<string> metaVars)
    {
        // Check if this node is a metavariable placeholder
        if (TryExtractMetaVariable(node, out var metaVar))
        {
            return metaVar;
        }

        // Convert to structural pattern
        var structural = new StructuralPattern
        {
            NodeKind = node.NodeKind
        };

        // Add property patterns based on node type
        AddPropertyPatterns(node, structural, metaVars);

        // Convert children
        foreach (var child in node.Children)
        {
            structural.Children.Add(ConvertToPattern(child, metaVars));
        }

        return structural;
    }

    /// <summary>
    /// Checks if a node represents a metavariable placeholder.
    /// </summary>
    private bool TryExtractMetaVariable(UastNode node, out MetaVariable? metaVar)
    {
        metaVar = null;

        string? name = null;

        // Check various node types for metavariable placeholders
        if (node is Schema.Expressions.IdentifierExpression ident)
        {
            name = ident.Name;
        }
        else if (node is Schema.Expressions.VariableExpression varExpr)
        {
            name = varExpr.Name;
        }
        else if (node is Schema.Expressions.CommandExpression cmd)
        {
            name = cmd.CommandName;
        }

        if (name != null && name.StartsWith(MetaVariable.Placeholder))
        {
            var suffix = name[MetaVariable.Placeholder.Length..];
            bool isMultiple = suffix.StartsWith("MULTI_");
            if (isMultiple)
            {
                suffix = suffix["MULTI_".Length..];
            }
            suffix = suffix.TrimEnd('_');

            metaVar = new MetaVariable
            {
                Name = suffix,
                Kind = isMultiple ? MetaVarKind.Multiple : MetaVarKind.Single
            };
            return true;
        }

        return false;
    }

    /// <summary>
    /// Adds property patterns based on node type.
    /// </summary>
    private void AddPropertyPatterns(UastNode node, StructuralPattern pattern, HashSet<string> metaVars)
    {
        // Add relevant properties based on node type
        switch (node)
        {
            case Schema.Declarations.FunctionDeclaration func:
                if (!string.IsNullOrEmpty(func.Name) && !func.Name.StartsWith(MetaVariable.Placeholder))
                {
                    pattern.Properties["Name"] = new LiteralPattern { Value = func.Name };
                }
                break;

            case Schema.Expressions.CommandExpression cmd:
                if (!string.IsNullOrEmpty(cmd.CommandName) && !cmd.CommandName.StartsWith(MetaVariable.Placeholder))
                {
                    pattern.Properties["CommandName"] = new LiteralPattern { Value = cmd.CommandName };
                }
                break;

            case Schema.Expressions.LiteralExpression lit:
                pattern.Properties["Value"] = new LiteralPattern { Value = lit.Value };
                pattern.Properties["Kind"] = new LiteralPattern { Value = lit.Kind };
                break;

            case Schema.Expressions.BinaryExpression bin:
                pattern.Properties["Operator"] = new LiteralPattern { Value = bin.Operator };
                break;

            case Schema.Expressions.VariableExpression var:
                if (!string.IsNullOrEmpty(var.Name) && !var.Name.StartsWith(MetaVariable.Placeholder))
                {
                    pattern.Properties["Name"] = new LiteralPattern { Value = var.Name };
                }
                break;
        }
    }

    // Known UAST node kinds for simple pattern matching
    private static readonly HashSet<string> KnownNodeKinds = new(StringComparer.OrdinalIgnoreCase)
    {
        // Declarations
        "FunctionDeclaration", "TypeDeclaration", "VariableDeclaration", "PropertyDeclaration",
        "ParameterNode", "TypeParameterNode", "AttributeNode", "ModifierNode",
        "EnumMemberDeclaration",

        // Statements
        "BlockNode", "ExpressionStatement", "IfStatement", "WhileStatement", "DoWhileStatement",
        "ForStatement", "ForEachStatement", "SwitchStatement", "SwitchCase",
        "TryStatement", "CatchClause", "ReturnStatement", "BreakStatement", "ContinueStatement",
        "ThrowStatement", "UsingStatement", "LockStatement", "YieldStatement",
        "GotoStatement", "LabeledStatement", "CheckedStatement", "UnsafeStatement",
        "FixedStatement", "EmptyStatement",

        // Expressions
        "LiteralExpression", "IdentifierExpression", "VariableExpression", "BinaryExpression",
        "UnaryExpression", "AssignmentExpression", "CallExpression", "MemberExpression",
        "IndexExpression", "NewExpression", "ArrayExpression", "ObjectExpression",
        "PropertyNode", "ConditionalExpression", "LambdaExpression", "CastExpression",
        "TypeCheckExpression", "InterpolatedStringExpression", "InterpolatedStringPart",
        "TupleExpression", "ParenthesizedExpression", "AwaitExpression", "TypeExpression",
        "InvokeMemberExpression", "ArgumentNode",

        // PowerShell-specific
        "CommandExpression", "PipelineExpression", "HashtableExpression", "ScriptBlockExpression",
        "SplatExpression", "ExpandableStringExpression", "SubExpressionNode",

        // C#-specific
        "LinqExpression", "PointerExpression", "StackAllocExpression", "RefExpression",

        // Types
        "NamedTypeReference", "ArrayTypeReference", "GenericTypeReference",
        "TupleTypeReference", "UnionTypeReference", "FunctionTypeReference",
        "NullableTypeReference",

        // Unknown/fallback
        "UnknownExpression"
    };

    /// <summary>
    /// Parses a simple pattern string without full language parsing.
    /// </summary>
    private PatternNode ParseSimplePattern(string source, HashSet<string> metaVars)
    {
        source = source.Trim();

        // Check for metavariable
        if (source.Length > 0 && (source[0] == MetaVarPrefix || source[0] == MetaVarPrefixAlt))
        {
            int prefixCount = 0;
            int i = 0;
            while (i < source.Length && source[i] == source[0])
            {
                prefixCount++;
                i++;
            }

            var name = source[i..];
            if (!string.IsNullOrEmpty(name))
            {
                // Check for anonymous metavariable (∀_ or ∀∀∀_)
                bool isAnonymous = name == "_";

                // Determine kind based on prefix count
                var kind = prefixCount switch
                {
                    >= 3 => MetaVarKind.Multiple,
                    2 => MetaVarKind.AtLeastOne,
                    _ => MetaVarKind.Single
                };

                if (!isAnonymous)
                {
                    metaVars.Add(name);
                }

                return new MetaVariable
                {
                    Name = isAnonymous ? "_" : name,
                    Kind = kind,
                    IsAnonymous = isAnonymous
                };
            }
        }

        // Check for wildcard
        if (source == "*" || source == "...")
        {
            return WildcardPattern.Instance;
        }

        // Check if this is a known UAST node kind - match by NodeKind
        if (KnownNodeKinds.Contains(source))
        {
            return new StructuralPattern
            {
                NodeKind = source
            };
        }

        // Otherwise treat as literal
        return new LiteralPattern { Value = source };
    }
}

/// <summary>
/// Exception thrown when pattern parsing fails.
/// </summary>
public class PatternParseException : Exception
{
    public PatternParseException(string message) : base(message) { }
    public PatternParseException(string message, Exception inner) : base(message, inner) { }
}
