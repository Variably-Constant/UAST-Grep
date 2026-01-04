using UAST.Core.Configuration;
using UAST.Core.Interfaces;
using UAST.Core.Schema;
using UAST.Core.Schema.Statements;
using UAST.Native;

namespace UAST.Parsers.Adapters;

/// <summary>
/// Adapter that wraps native parsing with UAST conversion.
/// Allows gradual migration of individual mappers from legacy to native backend.
///
/// Usage:
///   1. Create a mapper that extends this class
///   2. Provide the legacy mapper in the constructor
///   3. Override ConvertNode() for language-specific conversion
///   4. The adapter automatically routes to legacy or native based on ParserConfiguration
/// </summary>
public abstract class NativeMapperAdapter : ILanguageMapper
{
    private readonly string _languageName;
    private readonly ILanguageMapper? _legacyMapper;
    private readonly List<ParseError> _errors = [];

    /// <summary>
    /// Creates a new adapter for the specified language.
    /// </summary>
    /// <param name="languageName">The tree-sitter language name (e.g., "python", "javascript").</param>
    /// <param name="legacyMapper">Optional legacy mapper for fallback when native is not available.</param>
    protected NativeMapperAdapter(string languageName, ILanguageMapper? legacyMapper = null)
    {
        _languageName = languageName ?? throw new ArgumentNullException(nameof(languageName));
        _legacyMapper = legacyMapper;
    }

    /// <summary>
    /// The name of the language this mapper handles.
    /// </summary>
    public abstract string LanguageName { get; }

    /// <summary>
    /// The file extensions this mapper handles (e.g., ".py", ".js").
    /// </summary>
    public abstract IReadOnlyList<string> FileExtensions { get; }

    /// <summary>
    /// Parses source code and returns a UAST tree.
    /// Automatically routes to legacy or native backend based on configuration.
    /// </summary>
    public UastNode Parse(string source, string? filePath = null)
    {
        _errors.Clear();

        // Use legacy mapper if native is not available or not configured
        if (ParserConfiguration.EffectiveBackend == ParserBackend.Legacy)
        {
            if (_legacyMapper != null)
            {
                return _legacyMapper.Parse(source, filePath);
            }
            throw new InvalidOperationException(
                $"Native backend not available and no legacy mapper provided for {LanguageName}");
        }

        // Use native parser
        try
        {
            using var parser = NativeParser.Create(_languageName);
            using var tree = parser.Parse(source);

            if (tree.HasError)
            {
                // Add parse error but continue - tree-sitter does error recovery
                _errors.Add(new ParseError(
                    $"Parse errors detected in {filePath ?? "input"}",
                    new SourceSpan(1, 0, 1, 0, 0, 0),
                    "PARSE_ERROR",
                    ParseErrorSeverity.Warning));
            }

            // Convert to UAST
            return ConvertToUast(tree, source, filePath);
        }
        catch (UastNativeException ex)
        {
            _errors.Add(new ParseError(
                ex.Message,
                new SourceSpan(1, 0, 1, 0, 0, 0),
                $"NATIVE_{ex.ErrorCode}",
                ParseErrorSeverity.Error));

            // Fall back to legacy if available
            if (_legacyMapper != null)
            {
                return _legacyMapper.Parse(source, filePath);
            }

            throw;
        }
    }

    /// <summary>
    /// Parses a file and returns a UAST tree.
    /// </summary>
    public UastNode ParseFile(string filePath)
    {
        var source = File.ReadAllText(filePath);
        return Parse(source, filePath);
    }

    /// <summary>
    /// Gets any parse errors from the last parse operation.
    /// </summary>
    public IReadOnlyList<ParseError> GetErrors()
    {
        // Combine our errors with legacy mapper errors if using legacy
        if (ParserConfiguration.EffectiveBackend == ParserBackend.Legacy && _legacyMapper != null)
        {
            return _legacyMapper.GetErrors();
        }
        return _errors;
    }

    /// <summary>
    /// Convert the native tree to UAST nodes.
    /// Override in derived classes to customize conversion.
    /// </summary>
    /// <param name="tree">The parsed native tree.</param>
    /// <param name="source">The original source code.</param>
    /// <param name="filePath">Optional file path for error reporting.</param>
    /// <returns>The root UAST node.</returns>
    protected virtual UastNode ConvertToUast(ParsedNativeTree tree, string source, string? filePath)
    {
        var root = tree.RootNode;
        var statements = new List<StatementNode>();

        // Recursively convert named children to statements
        foreach (var node in tree.WalkNamed())
        {
            // Only process top-level nodes (depth 1 from root)
            if (node.ParentId == root.Id)
            {
                var uastNode = ConvertNode(node, source);
                if (uastNode is StatementNode stmt)
                {
                    statements.Add(stmt);
                }
                else if (uastNode != null)
                {
                    // Wrap non-statements in an expression statement if needed
                    statements.Add(CreateWrappedStatement(uastNode, node, source));
                }
            }
        }

        var block = new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = root.ToSourceSpan(),
            RawSource = source,
            Statements = statements
        };

        // Set parent references
        block.SetParentReferences();

        return block;
    }

    /// <summary>
    /// Convert a single native node to a UAST node.
    /// Override to handle language-specific node types.
    /// </summary>
    /// <param name="node">The native tree-sitter node.</param>
    /// <param name="source">The original source code for extracting text.</param>
    /// <returns>The converted UAST node, or null to skip this node.</returns>
    protected abstract UastNode? ConvertNode(NativeNode node, string source);

    /// <summary>
    /// Gets the text of a node from the source code.
    /// </summary>
    /// <param name="node">The native node.</param>
    /// <param name="source">The source code.</param>
    /// <returns>The text span of the node.</returns>
    protected static string GetNodeText(NativeNode node, string source)
    {
        var start = (int)node.StartByte;
        var end = (int)node.EndByte;

        if (start >= 0 && end <= source.Length && start < end)
        {
            return source[start..end];
        }
        return string.Empty;
    }

    /// <summary>
    /// Creates an UnknownNode for native nodes that don't have a specific mapping.
    /// </summary>
    /// <param name="node">The native node.</param>
    /// <param name="source">The source code.</param>
    /// <param name="roles">Optional semantic roles for the unknown node.</param>
    /// <returns>An UnknownNode wrapping the native node.</returns>
    protected UnknownNode CreateUnknownNode(NativeNode node, string source, params string[] roles)
    {
        return new UnknownNode
        {
            NodeKind = "UnknownNode",
            Language = LanguageName,
            Span = node.ToSourceSpan(),
            RawSource = GetNodeText(node, source),
            NativeNodeType = node.Kind,
            Roles = roles.Length > 0 ? roles : []
        };
    }

    /// <summary>
    /// Creates a statement wrapper for a UAST node that isn't already a statement.
    /// </summary>
    private StatementNode CreateWrappedStatement(UastNode node, NativeNode nativeNode, string source)
    {
        // Return as-is if already a statement
        if (node is StatementNode stmt)
            return stmt;

        // Wrap expressions in ExpressionStatement
        if (node is UAST.Core.Schema.Expressions.ExpressionNode expr)
        {
            return new ExpressionStatement
            {
                NodeKind = "ExpressionStatement",
                Language = LanguageName,
                Span = node.Span,
                RawSource = node.RawSource,
                Expression = expr
            };
        }

        // For other node types, create an empty statement as placeholder
        return new EmptyStatement
        {
            NodeKind = "EmptyStatement",
            Language = LanguageName,
            Span = node.Span,
            RawSource = node.RawSource
        };
    }
}
