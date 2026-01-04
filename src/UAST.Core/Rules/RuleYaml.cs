namespace UAST.Core.Rules;

/// <summary>
/// Represents a YAML rule definition.
/// This is the top-level structure for a single rule in a YAML file.
/// </summary>
public class RuleYaml
{
    /// <summary>
    /// Unique identifier for the rule (e.g., "no-write-host").
    /// </summary>
    public string Id { get; set; } = "";

    /// <summary>
    /// Target language (e.g., "powershell", "csharp").
    /// </summary>
    public string Language { get; set; } = "";

    /// <summary>
    /// Severity level: error, warning, info, hint.
    /// </summary>
    public string Severity { get; set; } = "warning";

    /// <summary>
    /// Human-readable message describing the issue.
    /// May contain metavariable interpolations like ∀NAME.
    /// </summary>
    public string Message { get; set; } = "";

    /// <summary>
    /// Optional extended explanation or notes.
    /// </summary>
    public string? Note { get; set; }

    /// <summary>
    /// Optional URL to documentation about this rule.
    /// </summary>
    public string? Url { get; set; }

    /// <summary>
    /// The pattern rule definition.
    /// </summary>
    public RulePatternYaml Rule { get; set; } = new();

    /// <summary>
    /// Additional constraints on captured metavariables.
    /// Key is the metavariable name (without ∀).
    /// </summary>
    public Dictionary<string, ConstraintYaml>? Constraints { get; set; }

    /// <summary>
    /// Optional fix template for auto-fix.
    /// May contain metavariable interpolations.
    /// </summary>
    public string? Fix { get; set; }

    /// <summary>
    /// Optional glob patterns for files to include.
    /// </summary>
    public List<string>? Files { get; set; }

    /// <summary>
    /// Optional glob patterns for files to exclude.
    /// </summary>
    public List<string>? Ignores { get; set; }

    /// <summary>
    /// Optional tags for categorizing rules.
    /// </summary>
    public List<string>? Tags { get; set; }

    /// <summary>
    /// Whether this rule is enabled (default true).
    /// </summary>
    public bool Enabled { get; set; } = true;
}

/// <summary>
/// Represents a pattern within a rule.
/// Supports simple patterns, composites (any/all/not), and relations (inside/has).
/// </summary>
public class RulePatternYaml
{
    /// <summary>
    /// A simple pattern string (e.g., "Write-Host ∀∀∀ARGS").
    /// </summary>
    public string? Pattern { get; set; }

    /// <summary>
    /// Match any of these patterns (OR).
    /// </summary>
    public List<RulePatternYaml>? Any { get; set; }

    /// <summary>
    /// Match all of these patterns (AND).
    /// </summary>
    public List<RulePatternYaml>? All { get; set; }

    /// <summary>
    /// Negate this pattern (NOT).
    /// </summary>
    public RulePatternYaml? Not { get; set; }

    /// <summary>
    /// Require the match to be inside an ancestor matching this pattern.
    /// </summary>
    public RelationYaml? Inside { get; set; }

    /// <summary>
    /// Require the match to NOT be inside an ancestor matching this pattern.
    /// </summary>
    public RelationYaml? NotInside { get; set; }

    /// <summary>
    /// Require the match to have a descendant matching this pattern.
    /// </summary>
    public RelationYaml? Has { get; set; }

    /// <summary>
    /// Require the match to NOT have a descendant matching this pattern.
    /// </summary>
    public RelationYaml? NotHas { get; set; }

    /// <summary>
    /// Require a sibling after this to match (precedes).
    /// </summary>
    public RelationYaml? Precedes { get; set; }

    /// <summary>
    /// Require a sibling before this to match (follows).
    /// </summary>
    public RelationYaml? Follows { get; set; }

    /// <summary>
    /// Match by node kind directly (e.g., "CommandExpression").
    /// </summary>
    public string? Kind { get; set; }

    /// <summary>
    /// Match by regex on source text.
    /// </summary>
    public string? Regex { get; set; }
}

/// <summary>
/// Represents constraint options for a captured metavariable.
/// </summary>
public class ConstraintYaml
{
    /// <summary>
    /// Required node kind (e.g., "HashtableAst").
    /// </summary>
    public string? Kind { get; set; }

    /// <summary>
    /// Regex pattern the source text must match.
    /// </summary>
    public string? Regex { get; set; }

    /// <summary>
    /// A pattern the captured value must match.
    /// </summary>
    public RulePatternYaml? Pattern { get; set; }

    /// <summary>
    /// Regex pattern the source text must NOT match.
    /// </summary>
    public string? NotRegex { get; set; }

    /// <summary>
    /// Node kind that must NOT match.
    /// </summary>
    public string? NotKind { get; set; }
}

/// <summary>
/// Represents a relational constraint (inside, has, precedes, follows).
/// </summary>
public class RelationYaml
{
    /// <summary>
    /// The pattern to match in the relation.
    /// </summary>
    public string? Pattern { get; set; }

    /// <summary>
    /// Node kind to match.
    /// </summary>
    public string? Kind { get; set; }

    /// <summary>
    /// How far to search: "end" (default) or "neighbor".
    /// </summary>
    public string StopBy { get; set; } = "end";

    /// <summary>
    /// Whether the relation must be immediate (for precedes/follows).
    /// </summary>
    public bool Immediate { get; set; }
}

/// <summary>
/// Severity levels for rules.
/// </summary>
public enum Severity
{
    /// <summary>Hint-level (lowest priority).</summary>
    Hint,

    /// <summary>Information-level.</summary>
    Info,

    /// <summary>Warning-level (default).</summary>
    Warning,

    /// <summary>Error-level (highest priority).</summary>
    Error
}

/// <summary>
/// Configuration file schema for UAST-Grep.yaml.
/// </summary>
public class ConfigYaml
{
    /// <summary>
    /// Paths to rule files to load.
    /// </summary>
    public List<string>? Rules { get; set; }

    /// <summary>
    /// Glob patterns for files to include.
    /// </summary>
    public List<string>? Files { get; set; }

    /// <summary>
    /// Glob patterns for files to exclude.
    /// </summary>
    public List<string>? Ignores { get; set; }

    /// <summary>
    /// Rules to disable by ID.
    /// </summary>
    public List<string>? DisableRules { get; set; }

    /// <summary>
    /// Rule overrides by ID.
    /// </summary>
    public Dictionary<string, RuleOverrideYaml>? RuleOverrides { get; set; }
}

/// <summary>
/// Allows overriding rule properties in config.
/// </summary>
public class RuleOverrideYaml
{
    /// <summary>
    /// Override severity level.
    /// </summary>
    public string? Severity { get; set; }

    /// <summary>
    /// Override enabled state.
    /// </summary>
    public bool? Enabled { get; set; }

    /// <summary>
    /// Override fix template.
    /// </summary>
    public string? Fix { get; set; }
}
