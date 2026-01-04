namespace UAST.Core.Configuration;

/// <summary>
/// Controls which parser backend to use.
/// </summary>
public enum ParserBackend
{
    /// <summary>
    /// Original C# P/Invoke to tree-sitter (legacy)
    /// </summary>
    Legacy,

    /// <summary>
    /// New Rust-based parser core
    /// </summary>
    Native,

    /// <summary>
    /// Auto-detect based on availability
    /// </summary>
    Auto
}

/// <summary>
/// Global configuration for parser backend selection.
/// </summary>
public static class ParserConfiguration
{
    private static ParserBackend _backend = ParserBackend.Auto;
    private static bool? _nativeAvailable;

    /// <summary>
    /// Gets or sets the preferred parser backend.
    /// </summary>
    public static ParserBackend Backend
    {
        get => _backend;
        set
        {
            _backend = value;
            // Clear cached availability when backend changes
            if (value != ParserBackend.Auto)
            {
                _nativeAvailable = null;
            }
        }
    }

    /// <summary>
    /// Check if native backend is available.
    /// Caches the result after first check.
    /// </summary>
    public static bool IsNativeAvailable
    {
        get
        {
            if (_nativeAvailable.HasValue)
                return _nativeAvailable.Value;

            _nativeAvailable = CheckNativeAvailability();
            return _nativeAvailable.Value;
        }
    }

    /// <summary>
    /// Get the effective backend to use based on configuration and availability.
    /// </summary>
    public static ParserBackend EffectiveBackend
    {
        get
        {
            if (_backend == ParserBackend.Auto)
            {
                return IsNativeAvailable ? ParserBackend.Native : ParserBackend.Legacy;
            }
            return _backend;
        }
    }

    /// <summary>
    /// Resets the native availability cache, forcing a re-check on next access.
    /// Useful for testing or after loading new native libraries.
    /// </summary>
    public static void ResetNativeAvailabilityCache()
    {
        _nativeAvailable = null;
    }

    /// <summary>
    /// Checks if the native uast_core library can be loaded.
    /// </summary>
    private static bool CheckNativeAvailability()
    {
        try
        {
            // Try to load the native library by calling a simple function
            // This uses lazy loading - the actual load happens when we call UastNative methods
            var version = GetNativeVersion();
            return !string.IsNullOrEmpty(version);
        }
        catch (DllNotFoundException)
        {
            return false;
        }
        catch (BadImageFormatException)
        {
            return false;
        }
        catch (Exception)
        {
            // Any other exception means native is not available
            return false;
        }
    }

    /// <summary>
    /// Gets the native library version string, or null if not available.
    /// </summary>
    private static string? GetNativeVersion()
    {
        // Use reflection to avoid hard dependency on UAST.Native assembly
        // This allows UAST.Core to be used without UAST.Native being present
        try
        {
            var nativeAssembly = System.Reflection.Assembly.Load("UAST.Native");
            var extensionsType = nativeAssembly.GetType("UAST.Native.UastNativeExtensions");
            if (extensionsType == null)
                return null;

            var getVersionMethod = extensionsType.GetMethod("GetVersion",
                System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static);
            if (getVersionMethod == null)
                return null;

            return getVersionMethod.Invoke(null, null) as string;
        }
        catch
        {
            return null;
        }
    }
}
