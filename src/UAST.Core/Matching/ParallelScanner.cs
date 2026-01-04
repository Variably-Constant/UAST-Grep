using System.Collections.Concurrent;
using System.Collections.Frozen;
using UAST.Core.Caching;
using UAST.Core.Interfaces;
using UAST.Core.Rules;
using UAST.Core.Schema;

namespace UAST.Core.Matching;

/// <summary>
/// Provides parallel file scanning capabilities for pattern matching.
///
/// Performance optimizations:
/// - Thread-local PatternMatcher instances (avoids per-rule allocation)
/// - FrozenDictionary for mapper lookups (optimized for read-heavy access)
/// - Result pre-sizing based on file count
/// - Optimized LINQ-free hot paths
/// </summary>
public sealed class ParallelScanner
{
    private readonly Dictionary<string, ILanguageMapper> _mapperBuilder = new();
    private FrozenDictionary<string, ILanguageMapper>? _mappers;
    private readonly AstCache? _cache;

    // Thread-local matcher to avoid allocation per rule/file
    private static readonly ThreadLocal<PatternMatcher> _threadLocalMatcher =
        new(() => new PatternMatcher());

    // Thread-local native matcher for native mode scanning
    private static readonly ThreadLocal<NativeTreeSitterMatcher> _threadLocalNativeMatcher =
        new(() => new NativeTreeSitterMatcher());

    /// <summary>
    /// Creates a new parallel scanner with optional caching.
    /// </summary>
    /// <param name="cache">Optional AST cache for improved performance.</param>
    public ParallelScanner(AstCache? cache = null)
    {
        _cache = cache;
    }

    /// <summary>
    /// Registers a language mapper.
    /// </summary>
    /// <param name="mapper">The mapper to register.</param>
    public void RegisterMapper(ILanguageMapper mapper)
    {
        foreach (var ext in mapper.FileExtensions)
        {
            _mapperBuilder[ext.ToLowerInvariant()] = mapper;
        }
        // Invalidate frozen dictionary so it gets rebuilt on next lookup
        _mappers = null;
    }

    /// <summary>
    /// Freezes the mapper dictionary for optimal read performance.
    /// Call this after all mappers are registered and before scanning.
    /// </summary>
    public void FreezeMappers()
    {
        _mappers = _mapperBuilder.ToFrozenDictionary(StringComparer.OrdinalIgnoreCase);
    }

    /// <summary>
    /// Scans multiple files in parallel for pattern matches.
    /// </summary>
    /// <param name="files">The files to scan.</param>
    /// <param name="rules">The rules to check.</param>
    /// <param name="maxDegreeOfParallelism">Maximum parallel tasks (0 = ProcessorCount).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Diagnostic results for all matches.</returns>
    public async Task<IEnumerable<DiagnosticResult>> ScanParallelAsync(
        IEnumerable<string> files,
        IEnumerable<Rule> rules,
        int maxDegreeOfParallelism = 0,
        CancellationToken cancellationToken = default)
    {
        var results = new ConcurrentBag<DiagnosticResult>();
        var rulesList = rules.ToList();

        var options = new ParallelOptions
        {
            MaxDegreeOfParallelism = maxDegreeOfParallelism > 0
                ? maxDegreeOfParallelism
                : Environment.ProcessorCount,
            CancellationToken = cancellationToken
        };

        await Parallel.ForEachAsync(files, options, async (file, ct) =>
        {
            ct.ThrowIfCancellationRequested();

            try
            {
                var fileResults = await ScanFileAsync(file, rulesList, ct);
                foreach (var result in fileResults)
                {
                    results.Add(result);
                }
            }
            catch (Exception ex) when (ex is not OperationCanceledException)
            {
                results.Add(new DiagnosticResult
                {
                    RuleId = "SCANNER_ERROR",
                    Message = $"Failed to scan {file}: {ex.Message}",
                    FilePath = file,
                    Span = SourceSpan.Empty,
                    Severity = DiagnosticSeverity.Error
                });
            }
        });

        return results.OrderBy(r => r.FilePath).ThenBy(r => r.Span.StartLine);
    }

    /// <summary>
    /// Scans a single file for pattern matches.
    /// Uses thread-local PatternMatcher and lazy parsing for optimal performance.
    /// </summary>
    public Task<IEnumerable<DiagnosticResult>> ScanFileAsync(
        string file,
        IEnumerable<Rule> rules,
        CancellationToken cancellationToken = default)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var mapper = GetMapper(file);
        if (mapper == null)
        {
            return Task.FromResult<IEnumerable<DiagnosticResult>>([]);
        }

        var results = new List<DiagnosticResult>();
        var languageName = mapper.LanguageName;
        var matcher = _threadLocalMatcher.Value!;

        // Check if we can use lazy parsing
        var lazyMapper = mapper as ILazyLanguageMapper;

        // Collect all target kinds from applicable rules for lazy parsing
        HashSet<string>? allTargetKinds = null;
        if (lazyMapper != null && _cache == null)
        {
            foreach (var rule in rules)
            {
                if (!MatchesLanguage(rule, languageName))
                    continue;

                var ruleKinds = PatternAnalyzer.ExtractTargetKinds(rule.Pattern);
                if (ruleKinds.Count > 0)
                {
                    allTargetKinds ??= new HashSet<string>(StringComparer.Ordinal);
                    foreach (var kind in ruleKinds)
                    {
                        allTargetKinds.Add(kind);
                    }
                }
            }
        }

        // Parse tree - use lazy parsing if available and not using cache
        UastNode tree;
        if (_cache != null)
        {
            tree = _cache.GetOrParse(file, mapper);
        }
        else if (lazyMapper != null && allTargetKinds != null && allTargetKinds.Count > 0)
        {
            tree = lazyMapper.ParseFileLazy(file, allTargetKinds);
        }
        else
        {
            tree = mapper.ParseFile(file);
        }

        // Avoid LINQ Where().ToList() - filter inline
        foreach (var rule in rules)
        {
            if (!MatchesLanguage(rule, languageName))
                continue;

            cancellationToken.ThrowIfCancellationRequested();

            var matches = rule.Pattern.Match(tree, matcher);

            foreach (var match in matches)
            {
                results.Add(CreateDiagnostic(rule, match, file));
            }
        }

        return Task.FromResult<IEnumerable<DiagnosticResult>>(results);
    }

    /// <summary>
    /// Counts matches across files without full result details.
    /// Uses thread-local PatternMatcher and lazy parsing for optimal performance.
    /// </summary>
    public async Task<int> CountMatchesAsync(
        IEnumerable<string> files,
        Pattern pattern,
        int maxDegreeOfParallelism = 0,
        CancellationToken cancellationToken = default)
    {
        var count = 0;
        var options = new ParallelOptions
        {
            MaxDegreeOfParallelism = maxDegreeOfParallelism > 0
                ? maxDegreeOfParallelism
                : Environment.ProcessorCount,
            CancellationToken = cancellationToken
        };

        // Pre-compute target kinds for lazy parsing
        var targetKinds = PatternAnalyzer.ExtractTargetKinds(pattern);

        await Parallel.ForEachAsync(files, options, async (file, ct) =>
        {
            ct.ThrowIfCancellationRequested();

            try
            {
                var mapper = GetMapper(file);
                if (mapper == null) return;

                // Use lazy parsing if available and not using cache
                UastNode tree;
                if (_cache != null)
                {
                    tree = _cache.GetOrParse(file, mapper);
                }
                else if (mapper is ILazyLanguageMapper lazyMapper && targetKinds.Count > 0)
                {
                    tree = lazyMapper.ParseFileLazy(file, targetKinds);
                }
                else
                {
                    tree = mapper.ParseFile(file);
                }

                // Reuse thread-local matcher
                var matcher = _threadLocalMatcher.Value!;
                var matchCount = 0;

                // Avoid LINQ .Count() allocation - count manually
                foreach (var _ in matcher.Match(tree, pattern))
                {
                    matchCount++;
                }

                Interlocked.Add(ref count, matchCount);
            }
            catch
            {
                // Ignore errors in count mode
            }
        });

        return count;
    }

    #region Native Mode Scanning (Bypasses UAST Conversion)

    /// <summary>
    /// Scans files in parallel using native tree-sitter matching.
    /// Bypasses UAST conversion for maximum performance.
    /// </summary>
    /// <param name="files">The files to scan.</param>
    /// <param name="pattern">The native pattern to match (e.g., "function_definition").</param>
    /// <param name="maxDegreeOfParallelism">Maximum parallel tasks (0 = ProcessorCount).</param>
    /// <param name="cancellationToken">Cancellation token.</param>
    /// <returns>Native match results for all matches.</returns>
    public async Task<IEnumerable<NativeMatchResult>> ScanNativeAsync(
        IEnumerable<string> files,
        NativePattern pattern,
        int maxDegreeOfParallelism = 0,
        CancellationToken cancellationToken = default)
    {
        var results = new ConcurrentBag<NativeMatchResult>();

        var options = new ParallelOptions
        {
            MaxDegreeOfParallelism = maxDegreeOfParallelism > 0
                ? maxDegreeOfParallelism
                : Environment.ProcessorCount,
            CancellationToken = cancellationToken
        };

        await Parallel.ForEachAsync(files, options, (file, ct) =>
        {
            ct.ThrowIfCancellationRequested();

            try
            {
                var mapper = GetMapper(file);
                if (mapper == null) return ValueTask.CompletedTask;

                var tree = _cache != null
                    ? _cache.GetOrParse(file, mapper)
                    : mapper.ParseFile(file);

                // Use thread-local native matcher
                var matcher = _threadLocalNativeMatcher.Value!;
                var matches = matcher.Match(tree, pattern);

                foreach (var match in matches)
                {
                    results.Add(match);
                }
            }
            catch
            {
                // Ignore errors in native mode - continue scanning
            }

            return ValueTask.CompletedTask;
        });

        return results.OrderBy(r => r.Span.StartLine);
    }

    /// <summary>
    /// Scans files in parallel using native tree-sitter matching with string pattern.
    /// </summary>
    public Task<IEnumerable<NativeMatchResult>> ScanNativeAsync(
        IEnumerable<string> files,
        string patternText,
        int maxDegreeOfParallelism = 0,
        CancellationToken cancellationToken = default)
    {
        var parser = new NativePatternParser();
        var pattern = parser.Parse(patternText);
        return ScanNativeAsync(files, pattern, maxDegreeOfParallelism, cancellationToken);
    }

    /// <summary>
    /// Counts matches using native tree-sitter matching.
    /// More efficient than collecting full results.
    /// </summary>
    public async Task<int> CountNativeMatchesAsync(
        IEnumerable<string> files,
        NativePattern pattern,
        int maxDegreeOfParallelism = 0,
        CancellationToken cancellationToken = default)
    {
        var count = 0;

        var options = new ParallelOptions
        {
            MaxDegreeOfParallelism = maxDegreeOfParallelism > 0
                ? maxDegreeOfParallelism
                : Environment.ProcessorCount,
            CancellationToken = cancellationToken
        };

        await Parallel.ForEachAsync(files, options, (file, ct) =>
        {
            ct.ThrowIfCancellationRequested();

            try
            {
                var mapper = GetMapper(file);
                if (mapper == null) return ValueTask.CompletedTask;

                var tree = _cache != null
                    ? _cache.GetOrParse(file, mapper)
                    : mapper.ParseFile(file);

                // Use thread-local native matcher
                var matcher = _threadLocalNativeMatcher.Value!;
                var matchCount = matcher.CountMatches(tree, pattern);

                Interlocked.Add(ref count, matchCount);
            }
            catch
            {
                // Ignore errors in count mode
            }

            return ValueTask.CompletedTask;
        });

        return count;
    }

    #endregion

    private ILanguageMapper? GetMapper(string filePath)
    {
        // Lazy freeze on first lookup
        var mappers = _mappers ?? (_mappers = _mapperBuilder.ToFrozenDictionary(StringComparer.OrdinalIgnoreCase));

        var ext = Path.GetExtension(filePath)?.ToLowerInvariant() ?? "";
        return mappers.TryGetValue(ext, out var mapper) ? mapper : null;
    }

    private static bool MatchesLanguage(Rule rule, string language)
    {
        if (string.IsNullOrEmpty(rule.Language)) return true;
        return rule.Language.Equals(language, StringComparison.OrdinalIgnoreCase);
    }

    private static DiagnosticResult CreateDiagnostic(Rule rule, RuleMatch match, string file)
    {
        var nodeCaptures = new Dictionary<string, UastNode>();
        foreach (var (key, value) in match.Captures)
        {
            if (value is UastNode node)
            {
                nodeCaptures[key] = node;
            }
        }

        return new DiagnosticResult
        {
            RuleId = rule.Id,
            Message = rule.InterpolateMessage(match.Captures),
            FilePath = file,
            Span = match.MatchedNode.Span,
            Severity = ConvertSeverity(rule.Severity),
            Captures = nodeCaptures
        };
    }

    private static DiagnosticSeverity ConvertSeverity(Severity severity)
    {
        return severity switch
        {
            Rules.Severity.Error => DiagnosticSeverity.Error,
            Rules.Severity.Warning => DiagnosticSeverity.Warning,
            Rules.Severity.Info => DiagnosticSeverity.Information,
            Rules.Severity.Hint => DiagnosticSeverity.Hint,
            _ => DiagnosticSeverity.Warning
        };
    }

    private static string InterpolateMessage(string message, IReadOnlyDictionary<string, UastNode> captures)
    {
        var result = message;
        foreach (var (name, node) in captures)
        {
            var value = node.RawSource ?? node.ToString();
            result = result.Replace($"${name}", value);
            result = result.Replace($"∀{name}", value);
        }
        return result;
    }
}

/// <summary>
/// Represents a diagnostic result from pattern matching.
/// </summary>
public class DiagnosticResult
{
    /// <summary>
    /// The rule ID that triggered this diagnostic.
    /// </summary>
    public required string RuleId { get; init; }

    /// <summary>
    /// The diagnostic message.
    /// </summary>
    public required string Message { get; init; }

    /// <summary>
    /// The file path where the match was found.
    /// </summary>
    public required string FilePath { get; init; }

    /// <summary>
    /// The source location of the match.
    /// </summary>
    public required SourceSpan Span { get; init; }

    /// <summary>
    /// The severity of the diagnostic.
    /// </summary>
    public DiagnosticSeverity Severity { get; init; } = DiagnosticSeverity.Warning;

    /// <summary>
    /// Captured metavariable values.
    /// </summary>
    public IReadOnlyDictionary<string, UastNode>? Captures { get; init; }

    public override string ToString() =>
        $"{FilePath}:{Span.StartLine}:{Span.StartColumn}: [{Severity}] {RuleId}: {Message}";
}

/// <summary>
/// Diagnostic severity levels.
/// </summary>
public enum DiagnosticSeverity
{
    Hint,
    Information,
    Warning,
    Error
}
