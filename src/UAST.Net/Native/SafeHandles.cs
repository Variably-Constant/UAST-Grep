using Microsoft.Win32.SafeHandles;

namespace UAST.Net.Native;

/// <summary>
/// Safe handle for a Rust parser instance.
/// Ensures the parser is properly freed when disposed.
/// </summary>
internal sealed class ParserHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    public ParserHandle() : base(ownsHandle: true) { }

    private ParserHandle(IntPtr handle) : base(ownsHandle: true)
    {
        SetHandle(handle);
    }

    protected override bool ReleaseHandle()
    {
        UastNativeBindings.ParserFree(handle);
        return true;
    }

    /// <summary>
    /// Create a parser for the specified language.
    /// </summary>
    internal static ParserHandle Create(string language)
    {
        var handle = UastNativeBindings.ParserNew(language);
        if (handle == IntPtr.Zero)
        {
            throw new UastException(
                UastErrorCode.UnknownLanguage,
                $"Failed to create parser for language: {language}. " +
                "The language may not be registered. Call UastParser.RegisterLanguage() first.");
        }
        return new ParserHandle(handle);
    }

    /// <summary>
    /// Create a parser for a file extension.
    /// </summary>
    internal static ParserHandle CreateForExtension(string extension)
    {
        var handle = UastNativeBindings.ParserForExtension(extension);
        if (handle == IntPtr.Zero)
        {
            throw new UastException(
                UastErrorCode.UnknownLanguage,
                $"Failed to create parser for extension: {extension}. " +
                "No language is registered for this extension.");
        }
        return new ParserHandle(handle);
    }

    /// <summary>
    /// Create a parser from a raw language pointer.
    /// </summary>
    internal static ParserHandle CreateWithLanguage(IntPtr languagePtr, string name)
    {
        var handle = UastNativeBindings.ParserNewWithLanguage(languagePtr, name);
        if (handle == IntPtr.Zero)
        {
            throw new UastException(
                UastErrorCode.InternalError,
                $"Failed to create parser from language pointer for: {name}");
        }
        return new ParserHandle(handle);
    }
}

/// <summary>
/// Safe handle for a parsed syntax tree.
/// </summary>
internal sealed class TreeHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    private readonly byte[] _sourceBytes;

    public TreeHandle() : base(ownsHandle: true)
    {
        _sourceBytes = [];
    }

    private TreeHandle(IntPtr handle, byte[] sourceBytes) : base(ownsHandle: true)
    {
        SetHandle(handle);
        _sourceBytes = sourceBytes;
    }

    protected override bool ReleaseHandle()
    {
        UastNativeBindings.TreeFree(handle);
        return true;
    }

    /// <summary>
    /// Parse source code using the given parser.
    /// </summary>
    internal static TreeHandle Parse(ParserHandle parser, string source)
    {
        if (parser.IsInvalid || parser.IsClosed)
        {
            throw new ObjectDisposedException(nameof(parser));
        }

        var sourceBytes = System.Text.Encoding.UTF8.GetBytes(source);

        unsafe
        {
            fixed (byte* sourcePtr = sourceBytes)
            {
                var handle = UastNativeBindings.Parse(
                    parser.DangerousGetHandle(),
                    (IntPtr)sourcePtr,
                    (uint)sourceBytes.Length);

                if (handle == IntPtr.Zero)
                {
                    throw new UastException(
                        UastErrorCode.ParseFailed,
                        "Failed to parse source code");
                }

                return new TreeHandle(handle, sourceBytes);
            }
        }
    }

    /// <summary>
    /// Get the root node of the tree.
    /// </summary>
    internal unsafe UastNativeBindings.UastNode GetRootNode()
    {
        ThrowIfDisposed();

        UastNativeBindings.UastNode node;
        var result = UastNativeBindings.TreeRootNode(DangerousGetHandle(), &node);
        ThrowIfFailed(result, "Get root node");
        return node;
    }

    /// <summary>
    /// Check if the tree has parse errors.
    /// </summary>
    internal bool HasError
    {
        get
        {
            if (IsInvalid || IsClosed) return true;
            return UastNativeBindings.TreeHasError(DangerousGetHandle());
        }
    }

    /// <summary>
    /// Get the total node count.
    /// </summary>
    internal uint NodeCount
    {
        get
        {
            if (IsInvalid || IsClosed) return 0;
            return UastNativeBindings.TreeNodeCount(DangerousGetHandle());
        }
    }

    /// <summary>
    /// Get the source bytes.
    /// </summary>
    internal ReadOnlySpan<byte> SourceBytes => _sourceBytes;

    /// <summary>
    /// Extract text for a node by byte range.
    /// </summary>
    internal string GetNodeText(uint startByte, uint endByte)
    {
        var start = (int)startByte;
        var length = (int)(endByte - startByte);

        if (start < 0 || start + length > _sourceBytes.Length)
        {
            return string.Empty;
        }

        return System.Text.Encoding.UTF8.GetString(_sourceBytes, start, length);
    }

    private void ThrowIfDisposed()
    {
        if (IsInvalid || IsClosed)
        {
            throw new ObjectDisposedException(nameof(TreeHandle));
        }
    }

    private static void ThrowIfFailed(UastNativeBindings.UastResult result, string operation)
    {
        if (result != UastNativeBindings.UastResult.Ok)
        {
            throw new UastException(
                (UastErrorCode)(int)result,
                $"{operation} failed: {result}");
        }
    }
}

/// <summary>
/// Safe handle for a compiled query.
/// </summary>
internal sealed class QueryHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    public QueryHandle() : base(ownsHandle: true) { }

    private QueryHandle(IntPtr handle) : base(ownsHandle: true)
    {
        SetHandle(handle);
    }

    protected override bool ReleaseHandle()
    {
        UastNativeBindings.QueryFree(handle);
        return true;
    }

    /// <summary>
    /// Compile a query pattern for a tree's language.
    /// </summary>
    internal static QueryHandle Compile(TreeHandle tree, string pattern)
    {
        if (tree.IsInvalid || tree.IsClosed)
        {
            throw new ObjectDisposedException(nameof(tree));
        }

        var patternBytes = System.Text.Encoding.UTF8.GetBytes(pattern);

        unsafe
        {
            fixed (byte* patternPtr = patternBytes)
            {
                var handle = UastNativeBindings.QueryNew(
                    tree.DangerousGetHandle(),
                    (IntPtr)patternPtr,
                    (uint)patternBytes.Length);

                if (handle == IntPtr.Zero)
                {
                    throw new UastException(
                        UastErrorCode.QueryFailed,
                        $"Failed to compile query pattern: {pattern}");
                }

                return new QueryHandle(handle);
            }
        }
    }

    /// <summary>
    /// Get the number of patterns in the query.
    /// </summary>
    internal uint PatternCount
    {
        get
        {
            if (IsInvalid || IsClosed) return 0;
            return UastNativeBindings.QueryPatternCount(DangerousGetHandle());
        }
    }

    /// <summary>
    /// Get the number of capture names in the query.
    /// </summary>
    internal uint CaptureCount
    {
        get
        {
            if (IsInvalid || IsClosed) return 0;
            return UastNativeBindings.QueryCaptureCount(DangerousGetHandle());
        }
    }
}

/// <summary>
/// Safe handle for a rule loader.
/// </summary>
internal sealed class RuleLoaderHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    public RuleLoaderHandle() : base(ownsHandle: true) { }

    private RuleLoaderHandle(IntPtr handle) : base(ownsHandle: true)
    {
        SetHandle(handle);
    }

    protected override bool ReleaseHandle()
    {
        UastNativeBindings.RuleLoaderFree(handle);
        return true;
    }

    /// <summary>
    /// Create a new rule loader.
    /// </summary>
    internal static RuleLoaderHandle Create()
    {
        var handle = UastNativeBindings.RuleLoaderNew();
        if (handle == IntPtr.Zero)
        {
            throw new UastException(
                UastErrorCode.InternalError,
                "Failed to create rule loader");
        }
        return new RuleLoaderHandle(handle);
    }

    /// <summary>
    /// Get the number of loaded rules.
    /// </summary>
    internal uint RuleCount
    {
        get
        {
            if (IsInvalid || IsClosed) return 0;
            return UastNativeBindings.RuleLoaderCount(DangerousGetHandle());
        }
    }
}

/// <summary>
/// Safe handle for a scanner.
/// </summary>
internal sealed class ScannerHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    public ScannerHandle() : base(ownsHandle: true) { }

    private ScannerHandle(IntPtr handle) : base(ownsHandle: true)
    {
        SetHandle(handle);
    }

    protected override bool ReleaseHandle()
    {
        UastNativeBindings.ScannerFree(handle);
        return true;
    }

    /// <summary>
    /// Create a new scanner.
    /// </summary>
    internal static ScannerHandle Create()
    {
        var handle = UastNativeBindings.ScannerNew();
        if (handle == IntPtr.Zero)
        {
            throw new UastException(
                UastErrorCode.InternalError,
                "Failed to create scanner");
        }
        return new ScannerHandle(handle);
    }

    /// <summary>
    /// Get the number of rules in the scanner.
    /// </summary>
    internal uint RuleCount
    {
        get
        {
            if (IsInvalid || IsClosed) return 0;
            return UastNativeBindings.ScannerRuleCount(DangerousGetHandle());
        }
    }
}

/// <summary>
/// Safe handle for a compiled pattern.
/// </summary>
internal sealed class PatternHandle : SafeHandleZeroOrMinusOneIsInvalid
{
    private readonly nuint _handleId;

    public PatternHandle() : base(ownsHandle: true)
    {
        _handleId = 0;
    }

    private PatternHandle(nuint handleId) : base(ownsHandle: true)
    {
        _handleId = handleId;
        SetHandle((IntPtr)(nint)handleId);
    }

    protected override bool ReleaseHandle()
    {
        UastNativeBindings.PatternFree(_handleId);
        return true;
    }

    /// <summary>
    /// Compile a pattern for pattern matching.
    /// </summary>
    internal static PatternHandle Compile(string pattern, string language)
    {
        var result = UastNativeBindings.PatternCompile(pattern, language, out var handleId);
        if (result != 0 || handleId == 0)
        {
            throw new UastException(
                UastErrorCode.InternalError,
                $"Failed to compile pattern: {pattern}");
        }
        return new PatternHandle(handleId);
    }

    internal nuint HandleId => _handleId;
}
