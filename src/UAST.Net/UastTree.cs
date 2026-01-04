using UAST.Net.Native;

namespace UAST.Net;

/// <summary>
/// Represents a parsed syntax tree.
/// Provides methods for traversal, querying, and node access.
/// </summary>
public sealed class UastTree : IDisposable
{
    private readonly TreeHandle _tree;
    private readonly string _source;
    private readonly string _language;
    private bool _disposed;

    internal UastTree(TreeHandle tree, string source, string language)
    {
        _tree = tree;
        _source = source;
        _language = language;
    }

    /// <summary>
    /// The original source code.
    /// </summary>
    public string Source => _source;

    /// <summary>
    /// The language of the source code.
    /// </summary>
    public string Language => _language;

    /// <summary>
    /// Whether the parse had any errors.
    /// </summary>
    public bool HasError => _tree.HasError;

    /// <summary>
    /// Total number of nodes in the tree.
    /// </summary>
    public int NodeCount => (int)_tree.NodeCount;

    /// <summary>
    /// Get the root node of the tree.
    /// </summary>
    public UastNode RootNode
    {
        get
        {
            ObjectDisposedException.ThrowIf(_disposed, this);
            var node = _tree.GetRootNode();
            return new UastNode(node, _tree);
        }
    }

    /// <summary>
    /// Walk all nodes in the tree depth-first.
    /// </summary>
    /// <returns>An enumerable of all nodes.</returns>
    public IEnumerable<UastNode> Walk()
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        return WalkInternal(namedOnly: false);
    }

    /// <summary>
    /// Walk only named nodes (skipping syntax tokens like punctuation).
    /// </summary>
    /// <returns>An enumerable of named nodes.</returns>
    public IEnumerable<UastNode> WalkNamed()
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        return WalkInternal(namedOnly: true);
    }

    private unsafe IEnumerable<UastNode> WalkInternal(bool namedOnly)
    {
        var nodes = new List<UastNode>();

        UastNativeBindings.NodeCallback callback = (node, _) =>
        {
            nodes.Add(new UastNode(*node, _tree));
            return true;
        };

        var result = namedOnly
            ? UastNativeBindings.TreeWalkNamed(_tree.DangerousGetHandle(), callback, IntPtr.Zero)
            : UastNativeBindings.TreeWalk(_tree.DangerousGetHandle(), callback, IntPtr.Zero);

        if (result != UastNativeBindings.UastResult.Ok)
        {
            throw new UastException(
                (UastErrorCode)(int)result,
                "Tree walk failed");
        }

        return nodes;
    }

    /// <summary>
    /// Execute a tree-sitter query on the tree.
    /// </summary>
    /// <param name="pattern">Query pattern in tree-sitter S-expression syntax.</param>
    /// <returns>An enumerable of query matches.</returns>
    public IEnumerable<QueryMatch> Query(string pattern)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        var matches = new List<QueryMatch>();
        var patternBytes = System.Text.Encoding.UTF8.GetBytes(pattern);

        unsafe
        {
            fixed (byte* patternPtr = patternBytes)
            {
                UastNativeBindings.MatchCallback callback = (match, _) =>
                {
                    matches.Add(QueryMatch.FromNative(*match, _tree));
                    return true;
                };

                var result = UastNativeBindings.QueryExecuteSimple(
                    _tree.DangerousGetHandle(),
                    (IntPtr)patternPtr,
                    (uint)patternBytes.Length,
                    callback,
                    IntPtr.Zero);

                if (result != UastNativeBindings.UastResult.Ok)
                {
                    throw UastException.QueryFailed(pattern);
                }
            }
        }

        return matches;
    }

    /// <summary>
    /// Find all nodes of a specific kind.
    /// </summary>
    /// <param name="kind">The node kind to search for (e.g., "function_item").</param>
    /// <returns>An enumerable of matching nodes.</returns>
    public IEnumerable<UastNode> FindByKind(string kind)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);
        return WalkNamed().Where(n => n.Kind == kind);
    }

    /// <summary>
    /// Get text for a specific byte range.
    /// </summary>
    /// <param name="startByte">Start byte offset.</param>
    /// <param name="endByte">End byte offset.</param>
    /// <returns>The text in the range.</returns>
    public string GetText(int startByte, int endByte) =>
        _tree.GetNodeText((uint)startByte, (uint)endByte);

    /// <summary>
    /// Disposes the tree and frees native resources.
    /// </summary>
    public void Dispose()
    {
        if (!_disposed)
        {
            _tree.Dispose();
            _disposed = true;
        }
    }
}
