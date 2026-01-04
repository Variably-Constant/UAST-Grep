using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

namespace UAST.Native;

/// <summary>
/// P/Invoke declarations for the Rust uast_core library.
/// </summary>
public static partial class UastNative
{
    private const string LibraryName = "uast_core";

    // ========================================================================
    // Result codes (must match Rust enum)
    // ========================================================================

    public enum UastResult : int
    {
        Ok = 0,
        NullPointer = 1,
        InvalidUtf8 = 2,
        UnknownLanguage = 3,
        ParseFailed = 4,
        QueryFailed = 5,
        InternalError = 99
    }

    // ========================================================================
    // Structs (must match Rust layout exactly)
    // ========================================================================

    [StructLayout(LayoutKind.Sequential)]
    public struct UastPoint
    {
        public uint Row;
        public uint Column;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct UastRange
    {
        public uint StartByte;
        public uint EndByte;
        public UastPoint StartPoint;
        public UastPoint EndPoint;
    }

    [StructLayout(LayoutKind.Sequential)]
    public unsafe struct UastNode
    {
        public byte* Kind;
        public uint KindLen;
        public byte* FieldName;
        public uint FieldNameLen;
        public UastRange Range;
        public uint ChildCount;
        public uint NamedChildCount;
        [MarshalAs(UnmanagedType.I1)]
        public bool IsNamed;
        [MarshalAs(UnmanagedType.I1)]
        public bool IsMissing;
        [MarshalAs(UnmanagedType.I1)]
        public bool HasError;
        public ulong Id;
        public ulong ParentId;

        /// <summary>
        /// Get the node kind as a managed string.
        /// </summary>
        public readonly string GetKind()
        {
            if (Kind == null || KindLen == 0) return string.Empty;
            return System.Text.Encoding.UTF8.GetString(Kind, (int)KindLen);
        }

        /// <summary>
        /// Get the field name as a managed string, or null if not a named field.
        /// </summary>
        public readonly string? GetFieldName()
        {
            if (FieldName == null || FieldNameLen == 0) return null;
            return System.Text.Encoding.UTF8.GetString(FieldName, (int)FieldNameLen);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public unsafe struct UastCapture
    {
        public byte* Name;
        public uint NameLen;
        public UastNode Node;

        public readonly string GetName()
        {
            if (Name == null || NameLen == 0) return string.Empty;
            return System.Text.Encoding.UTF8.GetString(Name, (int)NameLen);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    public unsafe struct UastMatch
    {
        public uint PatternIndex;
        public UastCapture* Captures;
        public uint CaptureCount;
    }

    // ========================================================================
    // Callbacks
    // ========================================================================

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.I1)]
    public unsafe delegate bool NodeCallback(UastNode* node, IntPtr userData);

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.I1)]
    public unsafe delegate bool MatchCallback(UastMatch* match, IntPtr userData);

    // ========================================================================
    // Initialization
    // ========================================================================

    [LibraryImport(LibraryName, EntryPoint = "uast_version")]
    public static partial IntPtr Version();

    [LibraryImport(LibraryName, EntryPoint = "uast_init")]
    public static partial UastResult Init();

    [LibraryImport(LibraryName, EntryPoint = "uast_language_count")]
    public static partial uint LanguageCount();

    // ========================================================================
    // Language Registration (for dynamic grammar loading)
    // ========================================================================

    /// <summary>
    /// Register a language from a raw pointer (obtained from DynamicLanguage.cs).
    /// </summary>
    /// <param name="name">Language name (e.g., "kotlin")</param>
    /// <param name="languagePtr">TSLanguage* pointer from grammar DLL</param>
    /// <returns>True if registration succeeded</returns>
    [LibraryImport(LibraryName, EntryPoint = "uast_register_language", StringMarshalling = StringMarshalling.Utf8)]
    [return: MarshalAs(UnmanagedType.I1)]
    public static partial bool RegisterLanguage(string name, IntPtr languagePtr);

    /// <summary>
    /// Register file extensions for a language.
    /// </summary>
    [LibraryImport(LibraryName, EntryPoint = "uast_register_extensions", StringMarshalling = StringMarshalling.Utf8)]
    public static partial void RegisterExtensions(string languageName, IntPtr extensions, uint count);

    /// <summary>
    /// Check if a language is registered.
    /// </summary>
    [LibraryImport(LibraryName, EntryPoint = "uast_has_language", StringMarshalling = StringMarshalling.Utf8)]
    [return: MarshalAs(UnmanagedType.I1)]
    public static partial bool HasLanguage(string name);

    /// <summary>
    /// Unregister a language.
    /// </summary>
    [LibraryImport(LibraryName, EntryPoint = "uast_unregister_language", StringMarshalling = StringMarshalling.Utf8)]
    [return: MarshalAs(UnmanagedType.I1)]
    public static partial bool UnregisterLanguage(string name);

    // ========================================================================
    // Parser Management
    // ========================================================================

    /// <summary>
    /// Create a parser from a language pointer directly (bypass registry).
    /// </summary>
    [LibraryImport(LibraryName, EntryPoint = "uast_parser_new_with_language", StringMarshalling = StringMarshalling.Utf8)]
    public static partial IntPtr ParserNewWithLanguage(IntPtr languagePtr, string name);

    [LibraryImport(LibraryName, EntryPoint = "uast_parser_new", StringMarshalling = StringMarshalling.Utf8)]
    public static partial IntPtr ParserNew(string language);

    [LibraryImport(LibraryName, EntryPoint = "uast_parser_for_extension", StringMarshalling = StringMarshalling.Utf8)]
    public static partial IntPtr ParserForExtension(string extension);

    [LibraryImport(LibraryName, EntryPoint = "uast_parser_free")]
    public static partial void ParserFree(IntPtr parser);

    // ========================================================================
    // Parsing
    // ========================================================================

    [LibraryImport(LibraryName, EntryPoint = "uast_parse")]
    public static partial IntPtr Parse(IntPtr parser, IntPtr source, uint sourceLen);

    [LibraryImport(LibraryName, EntryPoint = "uast_tree_free")]
    public static partial void TreeFree(IntPtr tree);

    [LibraryImport(LibraryName, EntryPoint = "uast_tree_root_node")]
    public static unsafe partial UastResult TreeRootNode(IntPtr tree, UastNode* outNode);

    [LibraryImport(LibraryName, EntryPoint = "uast_tree_has_error")]
    [return: MarshalAs(UnmanagedType.I1)]
    public static partial bool TreeHasError(IntPtr tree);

    [LibraryImport(LibraryName, EntryPoint = "uast_tree_node_count")]
    public static partial uint TreeNodeCount(IntPtr tree);

    // ========================================================================
    // Tree Walking
    // ========================================================================

    [LibraryImport(LibraryName, EntryPoint = "uast_tree_walk")]
    public static partial UastResult TreeWalk(IntPtr tree, NodeCallback callback, IntPtr userData);

    [LibraryImport(LibraryName, EntryPoint = "uast_tree_walk_named")]
    public static partial UastResult TreeWalkNamed(IntPtr tree, NodeCallback callback, IntPtr userData);

    // ========================================================================
    // Query Execution
    // ========================================================================

    [LibraryImport(LibraryName, EntryPoint = "uast_query_execute")]
    public static partial UastResult QueryExecute(
        IntPtr tree,
        IntPtr pattern,
        uint patternLen,
        MatchCallback callback,
        IntPtr userData);

    // ========================================================================
    // Memory Management
    // ========================================================================

    [LibraryImport(LibraryName, EntryPoint = "uast_free_string")]
    public static partial void FreeString(IntPtr str);
}

/// <summary>
/// Helper extension methods for working with native types.
/// </summary>
public static class UastNativeExtensions
{
    /// <summary>
    /// Get the version string.
    /// </summary>
    public static string GetVersion()
    {
        var ptr = UastNative.Version();
        return Marshal.PtrToStringUTF8(ptr) ?? "unknown";
    }

    /// <summary>
    /// Check if a result indicates success.
    /// </summary>
    public static bool IsSuccess(this UastNative.UastResult result)
        => result == UastNative.UastResult.Ok;

    /// <summary>
    /// Throw if result is not success.
    /// </summary>
    public static void ThrowIfFailed(this UastNative.UastResult result, string operation)
    {
        if (!result.IsSuccess())
        {
            throw new UastNativeException(result, $"{operation} failed: {result}");
        }
    }
}

/// <summary>
/// Exception thrown when a native operation fails.
/// </summary>
public class UastNativeException : Exception
{
    public UastNative.UastResult ErrorCode { get; }

    public UastNativeException(UastNative.UastResult errorCode, string message)
        : base(message)
    {
        ErrorCode = errorCode;
    }
}
