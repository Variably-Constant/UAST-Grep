namespace UAST.Native;

/// <summary>
/// High-level wrapper for the native Rust parser.
/// Provides a clean API for parsing and iterating nodes.
/// </summary>
public sealed class NativeParser : IDisposable
{
    private readonly SafeParserHandle _parser;
    private bool _disposed;

    public string Language { get; }

    private NativeParser(SafeParserHandle parser, string language)
    {
        _parser = parser;
        Language = language;
    }

    /// <summary>
    /// Create a parser for the specified language.
    /// </summary>
    public static NativeParser Create(string language)
    {
        var parser = SafeParserHandle.Create(language);
        return new NativeParser(parser, language);
    }

    /// <summary>
    /// Create a parser for a file extension.
    /// </summary>
    public static NativeParser CreateForExtension(string extension)
    {
        var parser = SafeParserHandle.CreateForExtension(extension);
        return new NativeParser(parser, extension);
    }

    /// <summary>
    /// Parse source code and return a tree.
    /// </summary>
    public ParsedNativeTree Parse(string source)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        var tree = SafeTreeHandle.Parse(_parser, source);
        return new ParsedNativeTree(tree, source);
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _parser.Dispose();
            _disposed = true;
        }
    }
}

/// <summary>
/// A parsed tree with methods for traversal and querying.
/// </summary>
public sealed class ParsedNativeTree : IDisposable
{
    private readonly SafeTreeHandle _tree;
    private readonly string _source;
    private bool _disposed;

    internal ParsedNativeTree(SafeTreeHandle tree, string source)
    {
        _tree = tree;
        _source = source;
    }

    /// <summary>
    /// Original source code.
    /// </summary>
    public string Source => _source;

    /// <summary>
    /// Whether the parse had errors.
    /// </summary>
    public bool HasError => _tree.HasError;

    /// <summary>
    /// Total number of nodes.
    /// </summary>
    public uint NodeCount => _tree.NodeCount;

    /// <summary>
    /// Get the root node.
    /// </summary>
    public NativeNode RootNode
    {
        get
        {
            ObjectDisposedException.ThrowIf(_disposed, this);
            var node = _tree.GetRootNode();
            return new NativeNode(node, _tree);
        }
    }

    /// <summary>
    /// Walk all nodes in the tree.
    /// </summary>
    public IEnumerable<NativeNode> Walk()
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        return WalkInternal(namedOnly: false);
    }

    /// <summary>
    /// Walk only named nodes (skipping syntax tokens).
    /// </summary>
    public IEnumerable<NativeNode> WalkNamed()
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        return WalkInternal(namedOnly: true);
    }

    private unsafe IEnumerable<NativeNode> WalkInternal(bool namedOnly)
    {
        var nodes = new List<NativeNode>();

        // Callback that collects nodes
        UastNative.NodeCallback callback = (node, _) =>
        {
            nodes.Add(new NativeNode(*node, _tree));
            return true; // continue
        };

        var result = namedOnly
            ? UastNative.TreeWalkNamed(_tree.DangerousGetHandle(), callback, IntPtr.Zero)
            : UastNative.TreeWalk(_tree.DangerousGetHandle(), callback, IntPtr.Zero);

        result.ThrowIfFailed("Tree walk");

        return nodes;
    }

    /// <summary>
    /// Execute a tree-sitter query and return matches.
    /// </summary>
    public IEnumerable<QueryMatch> Query(string pattern)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        var matches = new List<QueryMatch>();
        var patternBytes = System.Text.Encoding.UTF8.GetBytes(pattern);

        unsafe
        {
            fixed (byte* patternPtr = patternBytes)
            {
                UastNative.MatchCallback callback = (match, _) =>
                {
                    matches.Add(QueryMatch.FromNative(*match, _tree));
                    return true;
                };

                var result = UastNative.QueryExecute(
                    _tree.DangerousGetHandle(),
                    (IntPtr)patternPtr,
                    (uint)patternBytes.Length,
                    callback,
                    IntPtr.Zero);

                result.ThrowIfFailed("Query execution");
            }
        }

        return matches;
    }

    public void Dispose()
    {
        if (!_disposed)
        {
            _tree.Dispose();
            _disposed = true;
        }
    }
}
