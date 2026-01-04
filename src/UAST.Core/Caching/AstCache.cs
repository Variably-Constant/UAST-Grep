using System.Collections.Concurrent;
using UAST.Core.Interfaces;
using UAST.Core.Schema;

namespace UAST.Core.Caching;

/// <summary>
/// Provides file-based caching for parsed AST trees.
/// Uses file modification timestamps to invalidate cache entries.
/// </summary>
public class AstCache
{
    private readonly ConcurrentDictionary<string, CachedAst> _cache = new();
    private readonly int _maxEntries;

    /// <summary>
    /// Creates a new AST cache with the specified maximum entries.
    /// </summary>
    /// <param name="maxEntries">Maximum number of cached entries (default: 1000).</param>
    public AstCache(int maxEntries = 1000)
    {
        _maxEntries = maxEntries;
    }

    /// <summary>
    /// Gets a cached AST or parses the file if not cached or stale.
    /// </summary>
    /// <param name="filePath">The file path to parse.</param>
    /// <param name="mapper">The language mapper to use.</param>
    /// <returns>The parsed UAST tree.</returns>
    public UastNode GetOrParse(string filePath, ILanguageMapper mapper)
    {
        var normalizedPath = Path.GetFullPath(filePath);
        var lastModified = File.GetLastWriteTimeUtc(filePath);

        if (_cache.TryGetValue(normalizedPath, out var cached))
        {
            if (cached.LastModified == lastModified)
            {
                return cached.Ast;
            }
        }

        // Evict if at capacity
        if (_cache.Count >= _maxEntries)
        {
            EvictOldest();
        }

        var ast = mapper.ParseFile(filePath);
        _cache[normalizedPath] = new CachedAst(ast, lastModified, DateTime.UtcNow);
        return ast;
    }

    /// <summary>
    /// Gets a cached AST or parses the source if not cached.
    /// Uses content hash for caching since there's no file.
    /// </summary>
    /// <param name="source">The source code to parse.</param>
    /// <param name="mapper">The language mapper to use.</param>
    /// <param name="cacheKey">Optional cache key (defaults to content hash).</param>
    /// <returns>The parsed UAST tree.</returns>
    public UastNode GetOrParseSource(string source, ILanguageMapper mapper, string? cacheKey = null)
    {
        var key = cacheKey ?? ComputeHash(source);

        if (_cache.TryGetValue(key, out var cached))
        {
            return cached.Ast;
        }

        if (_cache.Count >= _maxEntries)
        {
            EvictOldest();
        }

        var ast = mapper.Parse(source);
        _cache[key] = new CachedAst(ast, DateTime.UtcNow, DateTime.UtcNow);
        return ast;
    }

    /// <summary>
    /// Invalidates the cache entry for a specific file.
    /// </summary>
    /// <param name="filePath">The file path to invalidate.</param>
    public void Invalidate(string filePath)
    {
        var normalizedPath = Path.GetFullPath(filePath);
        _cache.TryRemove(normalizedPath, out _);
    }

    /// <summary>
    /// Invalidates all cache entries.
    /// </summary>
    public void Clear()
    {
        _cache.Clear();
    }

    /// <summary>
    /// Gets the current cache size.
    /// </summary>
    public int Count => _cache.Count;

    /// <summary>
    /// Gets cache statistics.
    /// </summary>
    public CacheStats GetStats()
    {
        return new CacheStats(
            _cache.Count,
            _maxEntries,
            _cache.Values.Sum(c => c.Ast.Descendants().Count())
        );
    }

    private void EvictOldest()
    {
        // Simple LRU-like eviction: remove the oldest accessed entry
        var oldest = _cache
            .OrderBy(kv => kv.Value.LastAccessed)
            .FirstOrDefault();

        if (!string.IsNullOrEmpty(oldest.Key))
        {
            _cache.TryRemove(oldest.Key, out _);
        }
    }

    private static string ComputeHash(string source)
    {
        // Simple hash for source content
        var hash = source.GetHashCode();
        return $"source:{hash:X8}:{source.Length}";
    }

    private record CachedAst(UastNode Ast, DateTime LastModified, DateTime LastAccessed);
}

/// <summary>
/// Cache statistics.
/// </summary>
public record CacheStats(
    int CurrentSize,
    int MaxSize,
    int TotalNodes
);
