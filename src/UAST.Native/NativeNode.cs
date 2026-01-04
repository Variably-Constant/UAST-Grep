using UAST.Core.Schema;

namespace UAST.Native;

/// <summary>
/// A node from the native tree.
/// </summary>
public readonly struct NativeNode
{
    private readonly UastNative.UastNode _node;
    private readonly SafeTreeHandle _tree;

    internal NativeNode(UastNative.UastNode node, SafeTreeHandle tree)
    {
        _node = node;
        _tree = tree;
    }

    /// <summary>
    /// The node type/kind (e.g., "function_declaration", "identifier").
    /// </summary>
    public string Kind => _node.GetKind();

    /// <summary>
    /// The field name if this node is a named field, otherwise null.
    /// </summary>
    public string? FieldName => _node.GetFieldName();

    /// <summary>
    /// Byte offset of the start of this node.
    /// </summary>
    public uint StartByte => _node.Range.StartByte;

    /// <summary>
    /// Byte offset of the end of this node (exclusive).
    /// </summary>
    public uint EndByte => _node.Range.EndByte;

    /// <summary>
    /// Zero-indexed row of the start position.
    /// </summary>
    public uint StartRow => _node.Range.StartPoint.Row;

    /// <summary>
    /// Zero-indexed column of the start position.
    /// </summary>
    public uint StartColumn => _node.Range.StartPoint.Column;

    /// <summary>
    /// Zero-indexed row of the end position.
    /// </summary>
    public uint EndRow => _node.Range.EndPoint.Row;

    /// <summary>
    /// Zero-indexed column of the end position.
    /// </summary>
    public uint EndColumn => _node.Range.EndPoint.Column;

    /// <summary>
    /// Total number of child nodes.
    /// </summary>
    public uint ChildCount => _node.ChildCount;

    /// <summary>
    /// Number of named (non-anonymous) child nodes.
    /// </summary>
    public uint NamedChildCount => _node.NamedChildCount;

    /// <summary>
    /// Whether this is a named node (vs anonymous syntax token).
    /// </summary>
    public bool IsNamed => _node.IsNamed;

    /// <summary>
    /// Whether this node represents a missing token inserted for error recovery.
    /// </summary>
    public bool IsMissing => _node.IsMissing;

    /// <summary>
    /// Whether this node or any of its descendants has a parse error.
    /// </summary>
    public bool HasError => _node.HasError;

    /// <summary>
    /// Unique identifier for this node within the tree.
    /// </summary>
    public ulong Id => _node.Id;

    /// <summary>
    /// Unique identifier of the parent node, or 0 if this is the root.
    /// </summary>
    public ulong ParentId => _node.ParentId;

    /// <summary>
    /// Get the source text for this node.
    /// </summary>
    public string Text => _tree.GetNodeText(_node);

    /// <summary>
    /// Convert to a SourceSpan for UAST integration.
    /// Note: SourceSpan uses 1-indexed lines and 0-indexed columns.
    /// </summary>
    public SourceSpan ToSourceSpan() => new(
        StartLine: (int)StartRow + 1,        // 1-indexed line
        StartColumn: (int)StartColumn,        // 0-indexed column
        EndLine: (int)EndRow + 1,            // 1-indexed line
        EndColumn: (int)EndColumn,            // 0-indexed column
        StartOffset: (int)StartByte,
        EndOffset: (int)EndByte);

    public override string ToString() => $"{Kind} [{StartRow}:{StartColumn}-{EndRow}:{EndColumn}]";
}
