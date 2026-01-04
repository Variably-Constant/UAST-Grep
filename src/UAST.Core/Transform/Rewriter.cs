using System.Text;
using UAST.Core.Matching;
using UAST.Core.Rules;
using UAST.Core.Schema;

namespace UAST.Core.Transform;

/// <summary>
/// Applies rewrite/transformation rules to source code.
/// </summary>
public class Rewriter
{
    /// <summary>
    /// Applies a fix template to a matched node, substituting captured metavariables.
    /// </summary>
    /// <param name="matchedNode">The node that matched the pattern.</param>
    /// <param name="template">The fix template with ∀VAR placeholders.</param>
    /// <param name="captures">The captured metavariables.</param>
    /// <returns>The transformed text.</returns>
    public string Apply(UastNode matchedNode, string template, IReadOnlyDictionary<string, object> captures)
    {
        var result = new StringBuilder();
        var i = 0;

        while (i < template.Length)
        {
            if (template[i] == '∀')
            {
                // Extract metavariable name
                var name = new StringBuilder();
                i++;

                // Skip additional ∀ characters (for ∀∀∀ multi-match references)
                while (i < template.Length && template[i] == '∀')
                {
                    i++;
                }

                // Collect the variable name
                while (i < template.Length && (char.IsLetterOrDigit(template[i]) || template[i] == '_'))
                {
                    name.Append(template[i]);
                    i++;
                }

                var varName = name.ToString();

                // Handle anonymous placeholder
                if (varName == "_")
                {
                    // Anonymous - use the matched node's source
                    result.Append(matchedNode.RawSource ?? "");
                }
                else if (captures.TryGetValue(varName, out var captured))
                {
                    result.Append(RenderCaptured(captured));
                }
                else
                {
                    // Variable not found - keep original placeholder text
                    result.Append('∀').Append(varName);
                }
            }
            else
            {
                result.Append(template[i]);
                i++;
            }
        }

        return result.ToString();
    }

    /// <summary>
    /// Renders a captured value to string.
    /// </summary>
    private string RenderCaptured(object captured)
    {
        return captured switch
        {
            UastNode node => node.RawSource ?? "",
            IEnumerable<UastNode> nodes => string.Join(" ", nodes.Select(n => n.RawSource ?? "")),
            _ => captured?.ToString() ?? ""
        };
    }

    /// <summary>
    /// Computes text edits for a set of matches with their replacements.
    /// Edits are returned in reverse order (highest offset first) for safe application.
    /// </summary>
    public IEnumerable<TextEdit> ComputeEdits(
        string originalSource,
        IEnumerable<(RuleMatch Match, string Replacement)> replacements)
    {
        var edits = new List<TextEdit>();

        foreach (var (match, replacement) in replacements)
        {
            edits.Add(new TextEdit
            {
                StartOffset = match.Span.StartOffset,
                EndOffset = match.Span.EndOffset,
                NewText = replacement,
                StartLine = match.Span.StartLine,
                StartColumn = match.Span.StartColumn,
                EndLine = match.Span.EndLine,
                EndColumn = match.Span.EndColumn
            });
        }

        // Sort by start offset descending for safe application
        return edits.OrderByDescending(e => e.StartOffset).ToList();
    }

    /// <summary>
    /// Applies text edits to source code.
    /// Edits must be in reverse order (highest offset first).
    /// </summary>
    public string ApplyEdits(string source, IEnumerable<TextEdit> edits)
    {
        var result = source;

        foreach (var edit in edits.OrderByDescending(e => e.StartOffset))
        {
            if (edit.StartOffset >= 0 && edit.EndOffset <= result.Length && edit.StartOffset <= edit.EndOffset)
            {
                result = result[..edit.StartOffset] + edit.NewText + result[edit.EndOffset..];
            }
        }

        return result;
    }

    /// <summary>
    /// Applies all fixes from a rule to a source file.
    /// </summary>
    public RewriteResult ApplyRule(
        string source,
        Rule rule,
        IEnumerable<RuleMatch> matches,
        bool dryRun = false)
    {
        if (string.IsNullOrEmpty(rule.Fix))
        {
            return new RewriteResult
            {
                OriginalSource = source,
                TransformedSource = source,
                Edits = [],
                MatchCount = matches.Count(),
                EditCount = 0
            };
        }

        var replacements = matches.Select(m => (
            Match: m,
            Replacement: Apply(m.MatchedNode, rule.Fix, m.Captures)
        )).ToList();

        var edits = ComputeEdits(source, replacements).ToList();

        var transformedSource = dryRun ? source : ApplyEdits(source, edits);

        return new RewriteResult
        {
            OriginalSource = source,
            TransformedSource = transformedSource,
            Edits = edits,
            MatchCount = matches.Count(),
            EditCount = edits.Count
        };
    }
}

/// <summary>
/// Represents a text edit (replacement) in source code.
/// </summary>
public record TextEdit
{
    /// <summary>
    /// Start offset in the source (0-indexed byte offset).
    /// </summary>
    public int StartOffset { get; init; }

    /// <summary>
    /// End offset in the source (exclusive).
    /// </summary>
    public int EndOffset { get; init; }

    /// <summary>
    /// The new text to insert at this position.
    /// </summary>
    public required string NewText { get; init; }

    /// <summary>
    /// Start line (1-indexed).
    /// </summary>
    public int StartLine { get; init; }

    /// <summary>
    /// Start column (0-indexed).
    /// </summary>
    public int StartColumn { get; init; }

    /// <summary>
    /// End line (1-indexed).
    /// </summary>
    public int EndLine { get; init; }

    /// <summary>
    /// End column (0-indexed).
    /// </summary>
    public int EndColumn { get; init; }

    /// <summary>
    /// Gets the length of text being replaced.
    /// </summary>
    public int ReplacedLength => EndOffset - StartOffset;
}

/// <summary>
/// Result of applying rewrites to source code.
/// </summary>
public record RewriteResult
{
    /// <summary>
    /// The original source code.
    /// </summary>
    public required string OriginalSource { get; init; }

    /// <summary>
    /// The transformed source code.
    /// </summary>
    public required string TransformedSource { get; init; }

    /// <summary>
    /// The text edits that were applied.
    /// </summary>
    public required IReadOnlyList<TextEdit> Edits { get; init; }

    /// <summary>
    /// Number of matches found.
    /// </summary>
    public int MatchCount { get; init; }

    /// <summary>
    /// Number of edits applied.
    /// </summary>
    public int EditCount { get; init; }

    /// <summary>
    /// Whether the source was modified.
    /// </summary>
    public bool HasChanges => OriginalSource != TransformedSource;
}

/// <summary>
/// Represents a diagnostic result from scanning.
/// </summary>
public record DiagnosticResult
{
    /// <summary>
    /// The rule ID that matched.
    /// </summary>
    public required string RuleId { get; init; }

    /// <summary>
    /// Severity level.
    /// </summary>
    public Severity Severity { get; init; }

    /// <summary>
    /// The diagnostic message.
    /// </summary>
    public required string Message { get; init; }

    /// <summary>
    /// Path to the file containing the match.
    /// </summary>
    public required string FilePath { get; init; }

    /// <summary>
    /// Source span of the match.
    /// </summary>
    public required SourceSpan Span { get; init; }

    /// <summary>
    /// The matched source code.
    /// </summary>
    public string? MatchedSource { get; init; }

    /// <summary>
    /// The suggested fix (if available).
    /// </summary>
    public string? Fix { get; init; }

    /// <summary>
    /// URL to documentation.
    /// </summary>
    public string? Url { get; init; }

    /// <summary>
    /// Tags associated with the rule.
    /// </summary>
    public IReadOnlyList<string>? Tags { get; init; }

    /// <summary>
    /// Captured metavariables.
    /// </summary>
    public IReadOnlyDictionary<string, object>? Captures { get; init; }
}
