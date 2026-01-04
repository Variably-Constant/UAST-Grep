using System.Collections.Concurrent;
using System.Runtime.InteropServices;

namespace UAST.Native;

/// <summary>
/// Static class to manage language registration with the native Rust parser.
/// Bridges between DynamicLanguage (legacy) and the native uast_core registry.
///
/// This enables using dynamically loaded tree-sitter grammars with the native
/// Rust parser backend, allowing gradual migration from legacy to native.
/// </summary>
public static class NativeLanguageRegistry
{
    private static readonly ConcurrentDictionary<string, bool> _registeredLanguages = new(StringComparer.OrdinalIgnoreCase);
    private static bool _initialized;
    private static readonly object _initLock = new();

    /// <summary>
    /// Gets the names of all languages registered with the native backend.
    /// </summary>
    public static IEnumerable<string> RegisteredLanguages => _registeredLanguages.Keys;

    /// <summary>
    /// Gets the count of registered languages.
    /// </summary>
    public static int Count => _registeredLanguages.Count;

    /// <summary>
    /// Ensures the native library is initialized.
    /// </summary>
    private static void EnsureInitialized()
    {
        if (_initialized) return;

        lock (_initLock)
        {
            if (_initialized) return;

            var result = UastNative.Init();
            if (!result.IsSuccess())
            {
                throw new UastNativeException(result, "Failed to initialize native parser library");
            }

            _initialized = true;
        }
    }

    /// <summary>
    /// Checks if a language is registered with the native backend.
    /// </summary>
    /// <param name="languageName">The language name (e.g., "python", "javascript").</param>
    /// <returns>True if the language is registered.</returns>
    public static bool IsRegistered(string languageName)
    {
        if (string.IsNullOrEmpty(languageName))
            return false;

        // Check local cache first
        if (_registeredLanguages.ContainsKey(languageName))
            return true;

        // Check native registry
        try
        {
            EnsureInitialized();
            return UastNative.HasLanguage(languageName);
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Registers a language from a DynamicLanguage instance.
    /// This bridges the legacy dynamic loading with the native Rust registry.
    /// </summary>
    /// <param name="language">The DynamicLanguage instance with the loaded grammar.</param>
    /// <returns>True if registration succeeded, false otherwise.</returns>
    /// <exception cref="ArgumentNullException">If language is null.</exception>
    /// <exception cref="ArgumentException">If the language handle is invalid.</exception>
    public static bool RegisterFromDynamicLanguage(DynamicLanguage language)
    {
        if (language == null)
            throw new ArgumentNullException(nameof(language));

        if (language.Handle == IntPtr.Zero)
            throw new ArgumentException("Language handle is null", nameof(language));

        var name = language.Name;

        // Already registered?
        if (_registeredLanguages.ContainsKey(name))
            return true;

        try
        {
            EnsureInitialized();

            // Register with native library
            var success = UastNative.RegisterLanguage(name, language.Handle);

            if (success)
            {
                _registeredLanguages.TryAdd(name, true);
            }

            return success;
        }
        catch (Exception ex)
        {
            throw new UastNativeException(
                UastNative.UastResult.InternalError,
                $"Failed to register language '{name}': {ex.Message}");
        }
    }

    /// <summary>
    /// Registers all grammars found in a directory.
    /// Grammars should follow the naming convention: tree-sitter-{name}.dll (or .so/.dylib)
    /// </summary>
    /// <param name="grammarDir">Directory containing tree-sitter grammar libraries.</param>
    /// <returns>The number of successfully registered languages.</returns>
    /// <exception cref="DirectoryNotFoundException">If the directory doesn't exist.</exception>
    public static int RegisterAllFromDirectory(string grammarDir)
    {
        if (!Directory.Exists(grammarDir))
            throw new DirectoryNotFoundException($"Grammar directory not found: {grammarDir}");

        var extension = GetNativeExtension();
        var prefix = RuntimeInformation.IsOSPlatform(OSPlatform.Windows) ? "" : "lib";
        var pattern = $"{prefix}tree-sitter-*{extension}";

        var registered = 0;
        var files = Directory.GetFiles(grammarDir, pattern);

        foreach (var file in files)
        {
            var fileName = Path.GetFileNameWithoutExtension(file);

            // Remove "lib" prefix on Unix
            if (fileName.StartsWith("lib"))
                fileName = fileName[3..];

            // Extract language name from "tree-sitter-{name}"
            if (!fileName.StartsWith("tree-sitter-"))
                continue;

            var name = fileName["tree-sitter-".Length..];
            if (string.IsNullOrEmpty(name))
                continue;

            try
            {
                // Load the grammar
                var language = new DynamicLanguage(name, file);

                // Register with native backend
                if (RegisterFromDynamicLanguage(language))
                {
                    registered++;
                }
            }
            catch (DynamicLanguageException)
            {
                // Skip languages that fail to load
                continue;
            }
            catch (UastNativeException)
            {
                // Skip languages that fail to register
                continue;
            }
        }

        return registered;
    }

    /// <summary>
    /// Unregisters a language from the native backend.
    /// </summary>
    /// <param name="languageName">The language name to unregister.</param>
    /// <returns>True if the language was unregistered, false if it wasn't registered.</returns>
    public static bool Unregister(string languageName)
    {
        if (string.IsNullOrEmpty(languageName))
            return false;

        _registeredLanguages.TryRemove(languageName, out _);

        try
        {
            EnsureInitialized();
            return UastNative.UnregisterLanguage(languageName);
        }
        catch
        {
            return false;
        }
    }

    /// <summary>
    /// Clears all registered languages from the cache.
    /// Note: This only clears the local cache; languages may still be registered in native.
    /// </summary>
    public static void ClearCache()
    {
        _registeredLanguages.Clear();
    }

    /// <summary>
    /// Gets the native library count of registered languages.
    /// </summary>
    public static uint GetNativeLanguageCount()
    {
        try
        {
            EnsureInitialized();
            return UastNative.LanguageCount();
        }
        catch
        {
            return 0;
        }
    }

    private static string GetNativeExtension()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
            return ".dll";
        if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
            return ".dylib";
        return ".so";
    }
}

/// <summary>
/// Exception thrown when registering a language fails.
/// </summary>
public class LanguageRegistrationException : Exception
{
    /// <summary>
    /// The name of the language that failed to register.
    /// </summary>
    public string LanguageName { get; }

    public LanguageRegistrationException(string languageName, string message)
        : base(message)
    {
        LanguageName = languageName;
    }

    public LanguageRegistrationException(string languageName, string message, Exception inner)
        : base(message, inner)
    {
        LanguageName = languageName;
    }
}

/// <summary>
/// Represents a DynamicLanguage loaded from a tree-sitter grammar DLL.
/// This is a minimal interface for use with NativeLanguageRegistry.
/// </summary>
/// <remarks>
/// Note: This is separate from UAST.Parsers.Dynamic.DynamicLanguage to avoid
/// circular dependencies. The two classes have compatible interfaces.
/// </remarks>
public sealed class DynamicLanguage : IDisposable
{
    private IntPtr _libraryHandle;
    private IntPtr _language;
    private bool _disposed;

    /// <summary>The language name (e.g., "kotlin", "lua").</summary>
    public string Name { get; }

    /// <summary>The path to the loaded native library.</summary>
    public string LibraryPath { get; }

    /// <summary>The native language pointer for use with tree-sitter API.</summary>
    public IntPtr Handle => _language;

    /// <summary>
    /// Loads a tree-sitter language grammar from a native library.
    /// </summary>
    /// <param name="name">The language name (e.g., "kotlin"). Used to find the entry point.</param>
    /// <param name="libraryPath">Full path to the native library (.dll/.so/.dylib).</param>
    /// <exception cref="DynamicLanguageException">If the library cannot be loaded.</exception>
    public DynamicLanguage(string name, string libraryPath)
    {
        Name = name ?? throw new ArgumentNullException(nameof(name));
        LibraryPath = libraryPath ?? throw new ArgumentNullException(nameof(libraryPath));

        if (!File.Exists(libraryPath))
        {
            throw new DynamicLanguageException($"Grammar library not found: {libraryPath}");
        }

        // Load the native library
        if (!NativeLibrary.TryLoad(libraryPath, out _libraryHandle))
        {
            throw new DynamicLanguageException($"Failed to load native library: {libraryPath}");
        }

        // Find the language entry point
        var entryPoint = FindEntryPoint(name, _libraryHandle);
        if (entryPoint == IntPtr.Zero)
        {
            NativeLibrary.Free(_libraryHandle);
            _libraryHandle = IntPtr.Zero;
            throw new DynamicLanguageException(
                $"Entry point not found in {Path.GetFileName(libraryPath)}. " +
                $"Expected: tree_sitter_{name.ToLowerInvariant().Replace("-", "_")}");
        }

        // Call the entry point to get the language pointer
        try
        {
            var getLanguage = Marshal.GetDelegateForFunctionPointer<GetLanguageDelegate>(entryPoint);
            _language = getLanguage();

            if (_language == IntPtr.Zero)
            {
                throw new DynamicLanguageException($"Language entry point returned null for {name}");
            }
        }
        catch (Exception ex) when (ex is not DynamicLanguageException)
        {
            NativeLibrary.Free(_libraryHandle);
            _libraryHandle = IntPtr.Zero;
            throw new DynamicLanguageException($"Failed to initialize language {name}: {ex.Message}", ex);
        }
    }

    public void Dispose()
    {
        if (_disposed) return;
        _disposed = true;

        if (_libraryHandle != IntPtr.Zero)
        {
            NativeLibrary.Free(_libraryHandle);
            _libraryHandle = IntPtr.Zero;
        }

        _language = IntPtr.Zero;
    }

    [UnmanagedFunctionPointer(CallingConvention.Cdecl)]
    private delegate IntPtr GetLanguageDelegate();

    /// <summary>
    /// Known symbol name aliases for grammars that don't follow naming conventions.
    /// </summary>
    private static readonly Dictionary<string, string[]> SymbolAliases = new(StringComparer.OrdinalIgnoreCase)
    {
        ["sfapex"] = ["tree_sitter_apex"],
        ["cobol"] = ["tree_sitter_COBOL", "tree_sitter_cobol85"],
        ["fsharp"] = ["tree_sitter_fsharp", "tree_sitter_f_sharp"],
        ["c-sharp"] = ["tree_sitter_c_sharp", "tree_sitter_csharp"],
        ["markdown-inline"] = ["tree_sitter_markdown_inline"],
        ["ocaml-type"] = ["tree_sitter_ocaml_type"],
    };

    private static IntPtr FindEntryPoint(string name, IntPtr libraryHandle)
    {
        // Try the standard name first
        var standardName = $"tree_sitter_{name.ToLowerInvariant().Replace("-", "_")}";
        if (NativeLibrary.TryGetExport(libraryHandle, standardName, out var entryPoint))
        {
            return entryPoint;
        }

        // Try known aliases
        if (SymbolAliases.TryGetValue(name, out var aliases))
        {
            foreach (var alias in aliases)
            {
                if (NativeLibrary.TryGetExport(libraryHandle, alias, out entryPoint))
                {
                    return entryPoint;
                }
            }
        }

        // Try with uppercase first letter
        var upperName = $"tree_sitter_{char.ToUpperInvariant(name[0])}{name[1..].ToLowerInvariant().Replace("-", "_")}";
        if (NativeLibrary.TryGetExport(libraryHandle, upperName, out entryPoint))
        {
            return entryPoint;
        }

        return IntPtr.Zero;
    }
}

/// <summary>
/// Exception thrown when loading a dynamic language fails.
/// </summary>
public class DynamicLanguageException : Exception
{
    public DynamicLanguageException(string message) : base(message) { }
    public DynamicLanguageException(string message, Exception inner) : base(message, inner) { }
}
