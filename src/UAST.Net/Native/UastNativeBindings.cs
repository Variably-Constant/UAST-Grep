using System.Runtime.InteropServices;

namespace UAST.Net.Native;

/// <summary>
/// P/Invoke declarations for the Rust uast_core native library.
/// This provides the low-level FFI interface for all UAST operations.
/// </summary>
internal static partial class UastNativeBindings
{
    private const string LibName = "uast_core";

    // ========================================================================
    // Result codes (must match Rust enum UastResult)
    // ========================================================================

    internal enum UastResult : int
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
    // FFI Structs (must match Rust layout exactly)
    // ========================================================================

    [StructLayout(LayoutKind.Sequential)]
    internal struct UastPoint
    {
        public uint Row;
        public uint Column;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal struct UastRange
    {
        public uint StartByte;
        public uint EndByte;
        public UastPoint StartPoint;
        public UastPoint EndPoint;
    }

    [StructLayout(LayoutKind.Sequential)]
    internal unsafe struct UastNode
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

        public readonly string GetKind()
        {
            if (Kind == null || KindLen == 0) return string.Empty;
            return System.Text.Encoding.UTF8.GetString(Kind, (int)KindLen);
        }

        public readonly string? GetFieldName()
        {
            if (FieldName == null || FieldNameLen == 0) return null;
            return System.Text.Encoding.UTF8.GetString(FieldName, (int)FieldNameLen);
        }
    }

    [StructLayout(LayoutKind.Sequential)]
    internal unsafe struct UastCapture
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
    internal unsafe struct UastMatch
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
    internal unsafe delegate bool NodeCallback(UastNode* node, IntPtr userData);

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    [return: MarshalAs(UnmanagedType.I1)]
    internal unsafe delegate bool MatchCallback(UastMatch* match, IntPtr userData);

    // ========================================================================
    // Initialization & Version
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_version")]
    internal static partial IntPtr Version();

    [LibraryImport(LibName, EntryPoint = "uast_init")]
    internal static partial UastResult Init();

    [LibraryImport(LibName, EntryPoint = "uast_language_count")]
    internal static partial uint LanguageCount();

    // ========================================================================
    // Language Registration
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_register_language", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial UastResult RegisterLanguage(string name, IntPtr languagePtr);

    [LibraryImport(LibName, EntryPoint = "uast_has_language", StringMarshalling = StringMarshalling.Utf8)]
    [return: MarshalAs(UnmanagedType.I1)]
    internal static partial bool HasLanguage(string name);

    [LibraryImport(LibName, EntryPoint = "uast_unregister_language", StringMarshalling = StringMarshalling.Utf8)]
    [return: MarshalAs(UnmanagedType.I1)]
    internal static partial bool UnregisterLanguage(string name);

    // ========================================================================
    // Parser Management
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_parser_new", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial IntPtr ParserNew(string language);

    [LibraryImport(LibName, EntryPoint = "uast_parser_new_with_language", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial IntPtr ParserNewWithLanguage(IntPtr languagePtr, string name);

    [LibraryImport(LibName, EntryPoint = "uast_parser_for_extension", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial IntPtr ParserForExtension(string extension);

    [LibraryImport(LibName, EntryPoint = "uast_parser_free")]
    internal static partial void ParserFree(IntPtr parser);

    // ========================================================================
    // Parsing
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_parse")]
    internal static partial IntPtr Parse(IntPtr parser, IntPtr source, uint sourceLen);

    [LibraryImport(LibName, EntryPoint = "uast_tree_free")]
    internal static partial void TreeFree(IntPtr tree);

    [LibraryImport(LibName, EntryPoint = "uast_tree_root_node")]
    internal static unsafe partial UastResult TreeRootNode(IntPtr tree, UastNode* outNode);

    [LibraryImport(LibName, EntryPoint = "uast_tree_has_error")]
    [return: MarshalAs(UnmanagedType.I1)]
    internal static partial bool TreeHasError(IntPtr tree);

    [LibraryImport(LibName, EntryPoint = "uast_tree_node_count")]
    internal static partial uint TreeNodeCount(IntPtr tree);

    // ========================================================================
    // Tree Walking
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_tree_walk")]
    internal static partial UastResult TreeWalk(IntPtr tree, NodeCallback callback, IntPtr userData);

    [LibraryImport(LibName, EntryPoint = "uast_tree_walk_named")]
    internal static partial UastResult TreeWalkNamed(IntPtr tree, NodeCallback callback, IntPtr userData);

    [LibraryImport(LibName, EntryPoint = "uast_node_text")]
    internal static unsafe partial UastResult NodeText(
        IntPtr tree,
        UastNode* node,
        out IntPtr outText,
        out uint outLen);

    // ========================================================================
    // Query Compilation and Execution
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_query_new")]
    internal static partial IntPtr QueryNew(IntPtr tree, IntPtr pattern, uint patternLen);

    [LibraryImport(LibName, EntryPoint = "uast_query_free")]
    internal static partial void QueryFree(IntPtr query);

    [LibraryImport(LibName, EntryPoint = "uast_query_pattern_count")]
    internal static partial uint QueryPatternCount(IntPtr query);

    [LibraryImport(LibName, EntryPoint = "uast_query_capture_count")]
    internal static partial uint QueryCaptureCount(IntPtr query);

    [LibraryImport(LibName, EntryPoint = "uast_query_execute")]
    internal static partial UastResult QueryExecute(
        IntPtr tree,
        IntPtr query,
        MatchCallback callback,
        IntPtr userData);

    [LibraryImport(LibName, EntryPoint = "uast_query_execute_simple")]
    internal static partial UastResult QueryExecuteSimple(
        IntPtr tree,
        IntPtr pattern,
        uint patternLen,
        MatchCallback callback,
        IntPtr userData);

    [LibraryImport(LibName, EntryPoint = "uast_query_match_count")]
    internal static partial uint QueryMatchCount(IntPtr tree, IntPtr query);

    // ========================================================================
    // UAST Conversion
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_parse_uast", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int ParseUast(
        string language,
        string source,
        string? sourcePath,
        out IntPtr outJson);

    [LibraryImport(LibName, EntryPoint = "uast_parse_to_typed_json", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int ParseToTypedJson(
        string language,
        string source,
        string? sourcePath,
        [MarshalAs(UnmanagedType.I1)] bool includeAllText,
        [MarshalAs(UnmanagedType.I1)] bool includeNativeTypes,
        out IntPtr outJson);

    [LibraryImport(LibName, EntryPoint = "uast_free_json")]
    internal static partial void FreeJson(IntPtr jsonPtr);

    [LibraryImport(LibName, EntryPoint = "uast_get_kind_for_native", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int GetKindForNative(
        string language,
        string nativeType,
        out IntPtr outKind,
        out uint outLen);

    [LibraryImport(LibName, EntryPoint = "uast_get_native_types_for_kind", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int GetNativeTypesForKind(
        string language,
        string uastKind,
        out IntPtr outTypes);

    [LibraryImport(LibName, EntryPoint = "uast_is_uast_pattern", StringMarshalling = StringMarshalling.Utf8)]
    [return: MarshalAs(UnmanagedType.I1)]
    internal static partial bool IsUastPattern(string pattern);

    // ========================================================================
    // Pattern Matching
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_pattern_compile", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int PatternCompile(
        string pattern,
        string language,
        out nuint outHandle);

    [LibraryImport(LibName, EntryPoint = "uast_pattern_free")]
    internal static partial void PatternFree(nuint handle);

    [LibraryImport(LibName, EntryPoint = "uast_pattern_to_query", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int PatternToQuery(
        string pattern,
        string language,
        out IntPtr outQuery);

    [LibraryImport(LibName, EntryPoint = "uast_pattern_match", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int PatternMatch(
        string pattern,
        string language,
        string uastJson,
        out IntPtr outMatchesJson);

    // ========================================================================
    // Rule Loading
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_new")]
    internal static partial IntPtr RuleLoaderNew();

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_load_string", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int RuleLoaderLoadString(IntPtr loader, string yaml);

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_load_file", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int RuleLoaderLoadFile(IntPtr loader, string path);

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_load_directory", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial int RuleLoaderLoadDirectory(IntPtr loader, string path);

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_count")]
    internal static partial uint RuleLoaderCount(IntPtr loader);

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_rules_json", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial IntPtr RuleLoaderRulesJson(IntPtr loader, string language);

    [LibraryImport(LibName, EntryPoint = "uast_rule_loader_free")]
    internal static partial void RuleLoaderFree(IntPtr loader);

    // ========================================================================
    // Scanner
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_scanner_new")]
    internal static partial IntPtr ScannerNew();

    [LibraryImport(LibName, EntryPoint = "uast_scanner_add_rules", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial UastResult ScannerAddRules(IntPtr scanner, string yaml);

    [LibraryImport(LibName, EntryPoint = "uast_scanner_rule_count")]
    internal static partial uint ScannerRuleCount(IntPtr scanner);

    [LibraryImport(LibName, EntryPoint = "uast_scanner_scan_json")]
    internal static partial IntPtr ScannerScanJson(
        IntPtr scanner,
        IntPtr uastJson,
        uint uastJsonLen,
        IntPtr language);

    [LibraryImport(LibName, EntryPoint = "uast_scanner_free")]
    internal static partial void ScannerFree(IntPtr scanner);

    // ========================================================================
    // Fixes
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_apply_fixes", StringMarshalling = StringMarshalling.Utf8)]
    internal static partial IntPtr ApplyFixes(
        string source,
        IntPtr fixesJson,
        uint fixesJsonLen);

    // ========================================================================
    // SARIF
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_scan_results_to_sarif")]
    internal static partial IntPtr ScanResultsToSarif(
        IntPtr resultsJson,
        uint resultsJsonLen,
        IntPtr rulesJson,
        uint rulesJsonLen,
        IntPtr toolName,
        IntPtr toolVersion);

    [LibraryImport(LibName, EntryPoint = "uast_scan_results_to_sarif_simple")]
    internal static partial IntPtr ScanResultsToSarifSimple(
        IntPtr resultsJson,
        uint resultsJsonLen);

    [LibraryImport(LibName, EntryPoint = "uast_scanner_scan_to_sarif")]
    internal static partial IntPtr ScannerScanToSarif(
        IntPtr scanner,
        IntPtr uastJson,
        uint uastJsonLen,
        IntPtr language,
        IntPtr filePath,
        IntPtr toolName,
        IntPtr toolVersion);

    [LibraryImport(LibName, EntryPoint = "uast_sarif_version")]
    internal static partial IntPtr SarifVersion();

    // ========================================================================
    // Memory Management
    // ========================================================================

    [LibraryImport(LibName, EntryPoint = "uast_free_string")]
    internal static partial void FreeString(IntPtr str);
}
