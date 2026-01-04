using UAST.Net.Native;

namespace UAST.Net;

/// <summary>
/// Represents a node in the syntax tree.
/// </summary>
public readonly struct UastNode
{
    private readonly UastNativeBindings.UastNode _node;
    private readonly TreeHandle _tree;

    internal UastNode(UastNativeBindings.UastNode node, TreeHandle tree)
    {
        _node = node;
        _tree = tree;
    }

    /// <summary>
    /// The kind of node (e.g., "function_item", "identifier").
    /// </summary>
    public string Kind => _node.GetKind();

    /// <summary>
    /// The field name if this node is a named field (e.g., "name", "body").
    /// Null if not a named field.
    /// </summary>
    public string? FieldName => _node.GetFieldName();

    /// <summary>
    /// The source location of this node.
    /// </summary>
    public SourceSpan Span => SourceSpan.FromNative(_node.Range);

    /// <summary>
    /// Total number of children (named and anonymous).
    /// </summary>
    public int ChildCount => (int)_node.ChildCount;

    /// <summary>
    /// Number of named children only.
    /// </summary>
    public int NamedChildCount => (int)_node.NamedChildCount;

    /// <summary>
    /// True if this is a named node (not syntax like punctuation).
    /// </summary>
    public bool IsNamed => _node.IsNamed;

    /// <summary>
    /// True if this node was inserted by error recovery.
    /// </summary>
    public bool IsMissing => _node.IsMissing;

    /// <summary>
    /// True if this node or any descendant has a parse error.
    /// </summary>
    public bool HasError => _node.HasError;

    /// <summary>
    /// Internal node ID.
    /// </summary>
    public ulong Id => _node.Id;

    /// <summary>
    /// Parent node ID, or 0 if this is the root.
    /// </summary>
    public ulong ParentId => _node.ParentId;

    /// <summary>
    /// Get the source text for this node.
    /// </summary>
    public string Text => _tree.GetNodeText(_node.Range.StartByte, _node.Range.EndByte);

    /// <summary>
    /// 1-indexed start line number.
    /// </summary>
    public int StartLine => (int)_node.Range.StartPoint.Row + 1;

    /// <summary>
    /// 0-indexed start column.
    /// </summary>
    public int StartColumn => (int)_node.Range.StartPoint.Column;

    /// <summary>
    /// 1-indexed end line number.
    /// </summary>
    public int EndLine => (int)_node.Range.EndPoint.Row + 1;

    /// <summary>
    /// 0-indexed end column.
    /// </summary>
    public int EndColumn => (int)_node.Range.EndPoint.Column;

    /// <summary>
    /// Byte offset from start of file.
    /// </summary>
    public int StartOffset => (int)_node.Range.StartByte;

    /// <summary>
    /// Byte offset from start of file.
    /// </summary>
    public int EndOffset => (int)_node.Range.EndByte;

    /// <summary>
    /// Returns a string representation of the node.
    /// </summary>
    public override string ToString() =>
        $"[{Kind}] {Span} ({(IsNamed ? "named" : "anonymous")})";
}
