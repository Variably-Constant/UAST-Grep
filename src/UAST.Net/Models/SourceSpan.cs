namespace UAST.Net;

/// <summary>
/// Represents a source location in a file.
/// </summary>
public readonly record struct SourceSpan
{
    /// <summary>
    /// 1-indexed start line number.
    /// </summary>
    public int StartLine { get; init; }

    /// <summary>
    /// 0-indexed start column (byte offset within line).
    /// </summary>
    public int StartColumn { get; init; }

    /// <summary>
    /// 1-indexed end line number.
    /// </summary>
    public int EndLine { get; init; }

    /// <summary>
    /// 0-indexed end column (byte offset within line).
    /// </summary>
    public int EndColumn { get; init; }

    /// <summary>
    /// Byte offset from start of file.
    /// </summary>
    public int StartOffset { get; init; }

    /// <summary>
    /// Byte offset from start of file.
    /// </summary>
    public int EndOffset { get; init; }

    /// <summary>
    /// Length in bytes.
    /// </summary>
    public int Length => EndOffset - StartOffset;

    /// <summary>
    /// Create a span from tree-sitter range data.
    /// </summary>
    internal static SourceSpan FromNative(Native.UastNativeBindings.UastRange range)
    {
        return new SourceSpan
        {
            // tree-sitter uses 0-indexed rows, we convert to 1-indexed lines
            StartLine = (int)range.StartPoint.Row + 1,
            StartColumn = (int)range.StartPoint.Column,
            EndLine = (int)range.EndPoint.Row + 1,
            EndColumn = (int)range.EndPoint.Column,
            StartOffset = (int)range.StartByte,
            EndOffset = (int)range.EndByte
        };
    }

    /// <summary>
    /// Returns a string representation in the format "line:column-line:column".
    /// </summary>
    public override string ToString() =>
        $"{StartLine}:{StartColumn}-{EndLine}:{EndColumn}";
}
