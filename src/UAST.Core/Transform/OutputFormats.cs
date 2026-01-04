using System.Text.Json;
using System.Text.Json.Serialization;
using UAST.Core.Rules;
using UAST.Core.Schema;

namespace UAST.Core.Transform;

/// <summary>
/// Base class for output formatters.
/// </summary>
public abstract class OutputFormatter
{
    /// <summary>
    /// Formats diagnostic results for output.
    /// </summary>
    public abstract string Format(IEnumerable<DiagnosticResult> results, FormatOptions options);
}

/// <summary>
/// Options for formatting output.
/// </summary>
public record FormatOptions
{
    /// <summary>
    /// Whether to include captured metavariables.
    /// </summary>
    public bool IncludeCaptures { get; init; }

    /// <summary>
    /// Whether to include source snippets.
    /// </summary>
    public bool IncludeSource { get; init; } = true;

    /// <summary>
    /// Whether to include fix suggestions.
    /// </summary>
    public bool IncludeFixes { get; init; } = true;

    /// <summary>
    /// Base path for relative file paths.
    /// </summary>
    public string? BasePath { get; init; }
}

/// <summary>
/// Formats output as JSON.
/// </summary>
public class JsonOutputFormatter : OutputFormatter
{
    private static readonly JsonSerializerOptions s_options = new()
    {
        WriteIndented = true,
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };

    public override string Format(IEnumerable<DiagnosticResult> results, FormatOptions options)
    {
        var output = results.Select(r => new
        {
            ruleId = r.RuleId,
            severity = r.Severity.ToString().ToLowerInvariant(),
            message = r.Message,
            file = GetRelativePath(r.FilePath, options.BasePath),
            location = new
            {
                startLine = r.Span.StartLine,
                startColumn = r.Span.StartColumn,
                endLine = r.Span.EndLine,
                endColumn = r.Span.EndColumn
            },
            source = options.IncludeSource ? r.MatchedSource : null,
            fix = options.IncludeFixes ? r.Fix : null,
            url = r.Url,
            tags = r.Tags,
            captures = options.IncludeCaptures ? FormatCaptures(r.Captures) : null
        });

        return JsonSerializer.Serialize(output, s_options);
    }

    private static string GetRelativePath(string path, string? basePath)
    {
        if (string.IsNullOrEmpty(basePath))
            return path;

        try
        {
            return Path.GetRelativePath(basePath, path);
        }
        catch
        {
            return path;
        }
    }

    private static Dictionary<string, string>? FormatCaptures(IReadOnlyDictionary<string, object>? captures)
    {
        if (captures == null || captures.Count == 0)
            return null;

        return captures.ToDictionary(
            c => c.Key,
            c => c.Value switch
            {
                UastNode node => node.RawSource ?? "",
                IEnumerable<UastNode> nodes => string.Join(" ", nodes.Select(n => n.RawSource ?? "")),
                _ => c.Value?.ToString() ?? ""
            }
        );
    }
}

/// <summary>
/// Formats output as SARIF (Static Analysis Results Interchange Format).
/// SARIF is a standard JSON format for static analysis results used by many CI/CD systems.
/// </summary>
public class SarifOutputFormatter : OutputFormatter
{
    private static readonly JsonSerializerOptions s_options = new()
    {
        WriteIndented = true,
        DefaultIgnoreCondition = JsonIgnoreCondition.WhenWritingNull,
        PropertyNamingPolicy = JsonNamingPolicy.CamelCase
    };

    public override string Format(IEnumerable<DiagnosticResult> results, FormatOptions options)
    {
        var resultsList = results.ToList();
        var ruleIds = resultsList.Select(r => r.RuleId).Distinct().ToList();

        var sarif = new
        {
            version = "2.1.0",
            schema = "https://raw.githubusercontent.com/oasis-tcs/sarif-spec/master/Schemata/sarif-schema-2.1.0.json",
            runs = new[]
            {
                new
                {
                    tool = new
                    {
                        driver = new
                        {
                            name = "UAST-Grep",
                            version = "1.0.0",
                            informationUri = "https://github.com/yourorg/UAST-Grep",
                            rules = ruleIds.Select(id => new
                            {
                                id = id,
                                shortDescription = new { text = id },
                                defaultConfiguration = new
                                {
                                    level = GetDefaultLevel(resultsList.FirstOrDefault(r => r.RuleId == id)?.Severity ?? Severity.Warning)
                                }
                            }).ToArray()
                        }
                    },
                    results = resultsList.Select(r => FormatResult(r, options)).ToArray()
                }
            }
        };

        return JsonSerializer.Serialize(sarif, s_options);
    }

    private static object FormatResult(DiagnosticResult result, FormatOptions options)
    {
        var location = new
        {
            physicalLocation = new
            {
                artifactLocation = new
                {
                    uri = GetRelativeUri(result.FilePath, options.BasePath)
                },
                region = new
                {
                    startLine = result.Span.StartLine,
                    startColumn = result.Span.StartColumn + 1, // SARIF uses 1-indexed columns
                    endLine = result.Span.EndLine,
                    endColumn = result.Span.EndColumn + 1,
                    snippet = options.IncludeSource && result.MatchedSource != null
                        ? new { text = result.MatchedSource }
                        : null
                }
            }
        };

        var obj = new Dictionary<string, object?>
        {
            ["ruleId"] = result.RuleId,
            ["level"] = GetLevel(result.Severity),
            ["message"] = new { text = result.Message },
            ["locations"] = new[] { location }
        };

        if (options.IncludeFixes && !string.IsNullOrEmpty(result.Fix))
        {
            obj["fixes"] = new[]
            {
                new
                {
                    description = new { text = "Apply suggested fix" },
                    artifactChanges = new[]
                    {
                        new
                        {
                            artifactLocation = new { uri = GetRelativeUri(result.FilePath, options.BasePath) },
                            replacements = new[]
                            {
                                new
                                {
                                    deletedRegion = new
                                    {
                                        startLine = result.Span.StartLine,
                                        startColumn = result.Span.StartColumn + 1,
                                        endLine = result.Span.EndLine,
                                        endColumn = result.Span.EndColumn + 1
                                    },
                                    insertedContent = new { text = result.Fix }
                                }
                            }
                        }
                    }
                }
            };
        }

        return obj;
    }

    private static string GetRelativeUri(string path, string? basePath)
    {
        var relativePath = path;
        if (!string.IsNullOrEmpty(basePath))
        {
            try
            {
                relativePath = Path.GetRelativePath(basePath, path);
            }
            catch { }
        }
        return relativePath.Replace('\\', '/');
    }

    private static string GetLevel(Severity severity) => severity switch
    {
        Severity.Error => "error",
        Severity.Warning => "warning",
        Severity.Info => "note",
        Severity.Hint => "note",
        _ => "warning"
    };

    private static string GetDefaultLevel(Severity severity) => GetLevel(severity);
}

/// <summary>
/// Formats output as plain text.
/// </summary>
public class TextOutputFormatter : OutputFormatter
{
    public override string Format(IEnumerable<DiagnosticResult> results, FormatOptions options)
    {
        var sb = new System.Text.StringBuilder();
        var resultsList = results.ToList();

        var byFile = resultsList.GroupBy(r => r.FilePath);

        foreach (var fileGroup in byFile)
        {
            sb.AppendLine(GetRelativePath(fileGroup.Key, options.BasePath));

            foreach (var result in fileGroup)
            {
                var severityIndicator = result.Severity switch
                {
                    Severity.Error => "[ERROR]",
                    Severity.Warning => "[WARN]",
                    Severity.Info => "[INFO]",
                    Severity.Hint => "[HINT]",
                    _ => "[WARN]"
                };

                sb.AppendLine($"  {result.Span.StartLine}:{result.Span.StartColumn} {severityIndicator} {result.RuleId}");
                sb.AppendLine($"    {result.Message}");

                if (options.IncludeSource && !string.IsNullOrEmpty(result.MatchedSource))
                {
                    var source = result.MatchedSource.Length > 100
                        ? result.MatchedSource[..100] + "..."
                        : result.MatchedSource;
                    source = source.Replace("\r", "").Replace("\n", " ");
                    sb.AppendLine($"    > {source}");
                }

                if (options.IncludeFixes && !string.IsNullOrEmpty(result.Fix))
                {
                    sb.AppendLine($"    Fix: {result.Fix}");
                }
            }

            sb.AppendLine();
        }

        sb.AppendLine($"Found {resultsList.Count} issues in {byFile.Count()} files");

        return sb.ToString();
    }

    private static string GetRelativePath(string path, string? basePath)
    {
        if (string.IsNullOrEmpty(basePath))
            return path;

        try
        {
            return Path.GetRelativePath(basePath, path);
        }
        catch
        {
            return path;
        }
    }
}
