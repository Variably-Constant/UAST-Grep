using System.Runtime.InteropServices;
using Microsoft.Win32.SafeHandles;

namespace UAST.Native;

/// <summary>
/// Safe handle for a Rust parser instance.
/// Ensures the parser is properly freed when disposed.
/// </summary>
public sealed class SafeParserHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    public SafeParserHandle() : base(ownsHandle: true) { }

    public SafeParserHandle(IntPtr handle, bool ownsHandle) : base(ownsHandle)
    {
        SetHandle(handle);
    }

    protected override bool ReleaseHandle()
    {
        UastNative.ParserFree(handle);
        return true;
    }

    /// <summary>
    /// Create a parser for the specified language.
    /// </summary>
    public static SafeParserHandle Create(string language)
    {
        var handle = UastNative.ParserNew(language);
        if (handle == IntPtr.Zero)
        {
            throw new UastNativeException(
                UastNative.UastResult.UnknownLanguage,
                $"Failed to create parser for language: {language}");
        }
        return new SafeParserHandle(handle, ownsHandle: true);
    }

    /// <summary>
    /// Create a parser for a file extension.
    /// </summary>
    public static SafeParserHandle CreateForExtension(string extension)
    {
        var handle = UastNative.ParserForExtension(extension);
        if (handle == IntPtr.Zero)
        {
            throw new UastNativeException(
                UastNative.UastResult.UnknownLanguage,
                $"Failed to create parser for extension: {extension}");
        }
        return new SafeParserHandle(handle, ownsHandle: true);
    }
}

/// <summary>
/// Safe handle for a parsed syntax tree.
/// </summary>
public sealed class SafeTreeHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    private readonly byte[] _sourceBytes;

    public SafeTreeHandle() : base(ownsHandle: true)
    {
        _sourceBytes = [];
    }

    private SafeTreeHandle(IntPtr handle, byte[] sourceBytes) : base(ownsHandle: true)
    {
        SetHandle(handle);
        _sourceBytes = sourceBytes;
    }

    protected override bool ReleaseHandle()
    {
        UastNative.TreeFree(handle);
        return true;
    }

    /// <summary>
    /// Parse source code using the given parser.
    /// </summary>
    public static SafeTreeHandle Parse(SafeParserHandle parser, string source)
    {
        if (parser.IsInvalid || parser.IsClosed)
        {
            throw new ObjectDisposedException(nameof(parser));
        }

        // Convert to UTF-8 and pin for native call
        var sourceBytes = System.Text.Encoding.UTF8.GetBytes(source);

        unsafe
        {
            fixed (byte* sourcePtr = sourceBytes)
            {
                var handle = UastNative.Parse(
                    parser.DangerousGetHandle(),
                    (IntPtr)sourcePtr,
                    (uint)sourceBytes.Length);

                if (handle == IntPtr.Zero)
                {
                    throw new UastNativeException(
                        UastNative.UastResult.ParseFailed,
                        "Failed to parse source code");
                }

                return new SafeTreeHandle(handle, sourceBytes);
            }
        }
    }

    /// <summary>
    /// Get the root node of the tree.
    /// </summary>
    public unsafe UastNative.UastNode GetRootNode()
    {
        if (IsInvalid || IsClosed)
        {
            throw new ObjectDisposedException(nameof(SafeTreeHandle));
        }

        UastNative.UastNode node;
        var result = UastNative.TreeRootNode(DangerousGetHandle(), &node);
        result.ThrowIfFailed("Get root node");
        return node;
    }

    /// <summary>
    /// Check if the tree has parse errors.
    /// </summary>
    public bool HasError
    {
        get
        {
            if (IsInvalid || IsClosed) return true;
            return UastNative.TreeHasError(DangerousGetHandle());
        }
    }

    /// <summary>
    /// Get the total node count.
    /// </summary>
    public uint NodeCount
    {
        get
        {
            if (IsInvalid || IsClosed) return 0;
            return UastNative.TreeNodeCount(DangerousGetHandle());
        }
    }

    /// <summary>
    /// Get the source bytes (for extracting node text).
    /// </summary>
    public ReadOnlySpan<byte> SourceBytes => _sourceBytes;

    /// <summary>
    /// Extract text for a node.
    /// </summary>
    public string GetNodeText(UastNative.UastNode node)
    {
        var start = (int)node.Range.StartByte;
        var length = (int)(node.Range.EndByte - node.Range.StartByte);

        if (start < 0 || start + length > _sourceBytes.Length)
        {
            return string.Empty;
        }

        return System.Text.Encoding.UTF8.GetString(_sourceBytes, start, length);
    }
}
