namespace UAST.Core.Schema;

/// <summary>
/// Represents the source location of a UAST node.
/// All positions are preserved for bidirectional mapping to source.
/// </summary>
public readonly record struct SourceSpan(
    int StartLine,      // 1-indexed line number
    int StartColumn,    // 0-indexed column
    int EndLine,        // 1-indexed line number
    int EndColumn,      // 0-indexed column
    int StartOffset,    // Byte offset from start of file
    int EndOffset       // Byte offset from start of file
)
{
    /// <summary>
    /// Creates an empty/unknown span.
    /// </summary>
    public static readonly SourceSpan Empty = new(0, 0, 0, 0, 0, 0);

    /// <summary>
    /// The length in bytes of the span.
    /// </summary>
    public int Length => EndOffset - StartOffset;

    /// <summary>
    /// Whether this span is valid (non-empty).
    /// </summary>
    public bool IsValid => StartLine > 0;

    /// <summary>
    /// Creates a new span that encompasses both this span and another.
    /// </summary>
    public SourceSpan Merge(SourceSpan other)
    {
        if (!IsValid) return other;
        if (!other.IsValid) return this;

        return new SourceSpan(
            Math.Min(StartLine, other.StartLine),
            StartLine < other.StartLine ? StartColumn :
                (StartLine == other.StartLine ? Math.Min(StartColumn, other.StartColumn) : other.StartColumn),
            Math.Max(EndLine, other.EndLine),
            EndLine > other.EndLine ? EndColumn :
                (EndLine == other.EndLine ? Math.Max(EndColumn, other.EndColumn) : other.EndColumn),
            Math.Min(StartOffset, other.StartOffset),
            Math.Max(EndOffset, other.EndOffset)
        );
    }

    public override string ToString() => $"{StartLine}:{StartColumn}-{EndLine}:{EndColumn}";
}
