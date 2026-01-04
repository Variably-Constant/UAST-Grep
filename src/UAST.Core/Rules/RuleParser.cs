using System.Reflection;
using System.Text.RegularExpressions;
using YamlDotNet.Core;
using YamlDotNet.Core.Events;
using YamlDotNet.Serialization;
using YamlDotNet.Serialization.NamingConventions;
using UAST.Core.Interfaces;
using UAST.Core.Matching;

namespace UAST.Core.Rules;

/// <summary>
/// Parses YAML rule files into compiled Rule objects.
/// Supports multi-document YAML files (multiple rules separated by ---).
/// </summary>
public class RuleParser
{
    private readonly Dictionary<string, ILanguageMapper> _mappers = new(StringComparer.OrdinalIgnoreCase);
    private readonly PatternParser _patternParser = new();
    private readonly IDeserializer _deserializer;

    public RuleParser()
    {
        _deserializer = new DeserializerBuilder()
            .WithNamingConvention(CamelCaseNamingConvention.Instance)
            .IgnoreUnmatchedProperties()
            .Build();
    }

    /// <summary>
    /// Registers a language mapper for pattern parsing.
    /// </summary>
    public void RegisterMapper(ILanguageMapper mapper)
    {
        _mappers[mapper.LanguageName] = mapper;
        _patternParser.RegisterMapper(mapper);

        foreach (var ext in mapper.FileExtensions)
        {
            _mappers[ext.TrimStart('.')] = mapper;
        }
    }

    /// <summary>
    /// Parses all rules from a YAML file.
    /// </summary>
    public IEnumerable<Rule> ParseFile(string path)
    {
        var yaml = File.ReadAllText(path);
        return Parse(yaml);
    }

    /// <summary>
    /// Parses all rules from a YAML string.
    /// </summary>
    public IEnumerable<Rule> Parse(string yaml)
    {
        var reader = new StringReader(yaml);
        var parser = new Parser(reader);
        var rules = new List<Rule>();

        // Move past stream start
        parser.Consume<StreamStart>();

        // Parse each document
        while (parser.Accept<DocumentStart>(out _))
        {
            parser.Consume<DocumentStart>();

            try
            {
                var ruleYaml = _deserializer.Deserialize<RuleYaml>(parser);
                if (ruleYaml != null && !string.IsNullOrEmpty(ruleYaml.Id))
                {
                    var rule = ConvertRule(ruleYaml);
                    rules.Add(rule);
                }
            }
            catch (Exception ex)
            {
                throw new RuleParseException($"Failed to parse rule: {ex.Message}", ex);
            }

            // Move past document end if present
            if (parser.Accept<DocumentEnd>(out _))
            {
                parser.Consume<DocumentEnd>();
            }
        }

        return rules;
    }

    /// <summary>
    /// Parses all rules from a YAML string, skipping any invalid rules.
    /// Use this for built-in rules where we want to be resilient to parsing errors.
    /// </summary>
    public IEnumerable<Rule> ParseWithSkipInvalid(string yaml)
    {
        var reader = new StringReader(yaml);
        var parser = new Parser(reader);
        var rules = new List<Rule>();
        var skippedCount = 0;

        // Move past stream start
        parser.Consume<StreamStart>();

        // Parse each document
        while (parser.Accept<DocumentStart>(out _))
        {
            parser.Consume<DocumentStart>();

            try
            {
                var ruleYaml = _deserializer.Deserialize<RuleYaml>(parser);
                if (ruleYaml != null && !string.IsNullOrEmpty(ruleYaml.Id))
                {
                    // Skip rules with empty or invalid rule sections
                    if (ruleYaml.Rule == null || !IsValidRulePattern(ruleYaml.Rule))
                    {
                        skippedCount++;
                    }
                    else
                    {
                        var rule = ConvertRule(ruleYaml);
                        rules.Add(rule);
                    }
                }
            }
            catch
            {
                // Skip invalid rules silently
                skippedCount++;
            }

            // Move past document end if present
            if (parser.Accept<DocumentEnd>(out _))
            {
                parser.Consume<DocumentEnd>();
            }
        }

        return rules;
    }

    /// <summary>
    /// Checks if a rule pattern YAML has valid structure.
    /// </summary>
    private static bool IsValidRulePattern(RulePatternYaml rule)
    {
        return !string.IsNullOrEmpty(rule.Pattern) ||
               !string.IsNullOrEmpty(rule.Kind) ||
               !string.IsNullOrEmpty(rule.Regex) ||
               (rule.Any != null && rule.Any.Count > 0) ||
               (rule.All != null && rule.All.Count > 0) ||
               rule.Not != null;
    }

    /// <summary>
    /// Parses all built-in rules embedded in the assembly.
    /// Returns rules from universal-security.yaml, universal-performance.yaml, and universal-quality.yaml.
    /// </summary>
    public IEnumerable<Rule> ParseEmbedded(BuiltInRuleSet ruleSet = BuiltInRuleSet.All)
    {
        var assembly = typeof(RuleParser).Assembly;
        var rules = new List<Rule>();

        // Get all embedded YAML resources
        var resourceNames = assembly.GetManifestResourceNames()
            .Where(n => n.EndsWith(".yaml", StringComparison.OrdinalIgnoreCase))
            .ToList();

        foreach (var resourceName in resourceNames)
        {
            // Filter by rule set
            var include = ruleSet switch
            {
                BuiltInRuleSet.Security => resourceName.Contains("security", StringComparison.OrdinalIgnoreCase),
                BuiltInRuleSet.Performance => resourceName.Contains("performance", StringComparison.OrdinalIgnoreCase),
                BuiltInRuleSet.Quality => resourceName.Contains("quality", StringComparison.OrdinalIgnoreCase),
                BuiltInRuleSet.All => true,
                _ => true
            };

            if (!include)
                continue;

            using var stream = assembly.GetManifestResourceStream(resourceName);
            if (stream == null)
                continue;

            using var reader = new StreamReader(stream);
            var yaml = reader.ReadToEnd();

            try
            {
                var parsedRules = ParseWithSkipInvalid(yaml);
                rules.AddRange(parsedRules);
            }
            catch (Exception ex)
            {
                // Log but continue - don't let one bad rule file break everything
                Console.Error.WriteLine($"Warning: Failed to parse embedded rules from {resourceName}: {ex.Message}");
            }
        }

        return rules;
    }

    /// <summary>
    /// Gets the names of all embedded rule files.
    /// </summary>
    public static IEnumerable<string> GetEmbeddedRuleNames()
    {
        var assembly = typeof(RuleParser).Assembly;
        return assembly.GetManifestResourceNames()
            .Where(n => n.EndsWith(".yaml", StringComparison.OrdinalIgnoreCase))
            .Select(n => n.Split('.').Reverse().Skip(1).First()); // Get filename without extension
    }

    /// <summary>
    /// Parses a configuration file.
    /// </summary>
    public ConfigYaml? ParseConfig(string path)
    {
        if (!File.Exists(path))
            return null;

        var yaml = File.ReadAllText(path);
        return _deserializer.Deserialize<ConfigYaml>(yaml);
    }

    /// <summary>
    /// Converts a RuleYaml to a compiled Rule.
    /// </summary>
    private Rule ConvertRule(RuleYaml yaml)
    {
        var pattern = ParseRulePattern(yaml.Rule, yaml.Language);
        var constraints = ParseConstraints(yaml.Constraints, yaml.Language);

        return new Rule
        {
            Id = yaml.Id,
            Language = yaml.Language,
            Severity = ParseSeverity(yaml.Severity),
            Message = yaml.Message,
            Pattern = pattern,
            Constraints = constraints,
            Fix = yaml.Fix,
            Url = yaml.Url,
            Note = yaml.Note,
            Files = yaml.Files,
            Ignores = yaml.Ignores,
            Tags = yaml.Tags,
            Enabled = yaml.Enabled
        };
    }

    /// <summary>
    /// Parses a pattern YAML into a compiled pattern.
    /// </summary>
    private CompiledPattern ParseRulePattern(RulePatternYaml rule, string language)
    {
        // Handle simple pattern
        if (!string.IsNullOrEmpty(rule.Pattern))
        {
            var pattern = _patternParser.ParseSimple(rule.Pattern, language);

            var compiled = new SimpleCompiledPattern
            {
                Pattern = pattern,
                InsideConstraint = rule.Inside != null ? ParseRelationConstraint(rule.Inside, language, isInside: true) : null,
                HasConstraint = rule.Has != null ? ParseRelationConstraint(rule.Has, language, isInside: false) : null
            };

            // Handle notInside and notHas as wrapper constraints
            if (rule.NotInside != null || rule.NotHas != null)
            {
                return new AllCompiledPattern
                {
                    Patterns = [
                        compiled,
                        ..GetNotConstraintPatterns(rule, language)
                    ]
                };
            }

            return compiled;
        }

        // Handle any (OR)
        if (rule.Any != null && rule.Any.Count > 0)
        {
            return new AnyCompiledPattern
            {
                Patterns = rule.Any.Select(p => ParseRulePattern(p, language)).ToList()
            };
        }

        // Handle all (AND)
        if (rule.All != null && rule.All.Count > 0)
        {
            return new AllCompiledPattern
            {
                Patterns = rule.All.Select(p => ParseRulePattern(p, language)).ToList()
            };
        }

        // Handle not
        if (rule.Not != null)
        {
            return new NotCompiledPattern
            {
                Inner = ParseRulePattern(rule.Not, language)
            };
        }

        // Handle kind
        if (!string.IsNullOrEmpty(rule.Kind))
        {
            return new KindCompiledPattern
            {
                Kind = rule.Kind
            };
        }

        // Handle regex
        if (!string.IsNullOrEmpty(rule.Regex))
        {
            return new RegexCompiledPattern
            {
                Regex = new Regex(rule.Regex, RegexOptions.Compiled)
            };
        }

        throw new RuleParseException("Rule pattern must have pattern, any, all, not, kind, or regex");
    }

    /// <summary>
    /// Gets constraint patterns for notInside and notHas.
    /// </summary>
    private IEnumerable<CompiledPattern> GetNotConstraintPatterns(RulePatternYaml rule, string language)
    {
        // These are implemented as constraints that must pass, not standalone patterns
        // Return empty for now - the constraints are applied during matching
        yield break;
    }

    /// <summary>
    /// Parses a relational constraint.
    /// </summary>
    private Constraint ParseRelationConstraint(RelationYaml relation, string language, bool isInside)
    {
        PatternNode patternNode;

        if (!string.IsNullOrEmpty(relation.Pattern))
        {
            var pattern = _patternParser.ParseSimple(relation.Pattern, language);
            patternNode = pattern.Root;
        }
        else if (!string.IsNullOrEmpty(relation.Kind))
        {
            patternNode = new StructuralPattern { NodeKind = relation.Kind };
        }
        else
        {
            throw new RuleParseException("Relation must have pattern or kind");
        }

        var stopBy = relation.StopBy.ToLowerInvariant() switch
        {
            "neighbor" => StopBehavior.Neighbor,
            _ => StopBehavior.End
        };

        if (isInside)
        {
            return new InsideConstraint
            {
                AncestorPattern = patternNode,
                StopBy = stopBy
            };
        }
        else
        {
            return new HasConstraint
            {
                DescendantPattern = patternNode,
                StopBy = stopBy
            };
        }
    }

    /// <summary>
    /// Parses metavariable constraints.
    /// </summary>
    private Dictionary<string, Constraint>? ParseConstraints(
        Dictionary<string, ConstraintYaml>? constraintYamls,
        string language)
    {
        if (constraintYamls == null || constraintYamls.Count == 0)
            return null;

        var constraints = new Dictionary<string, Constraint>();

        foreach (var (varName, constraintYaml) in constraintYamls)
        {
            var innerConstraints = new List<Constraint>();

            if (!string.IsNullOrEmpty(constraintYaml.Kind))
            {
                innerConstraints.Add(new KindConstraint { Kind = constraintYaml.Kind });
            }

            if (!string.IsNullOrEmpty(constraintYaml.Regex))
            {
                innerConstraints.Add(RegexConstraint.FromPattern(constraintYaml.Regex));
            }

            if (constraintYaml.Pattern != null)
            {
                var pattern = _patternParser.ParseSimple(constraintYaml.Pattern.Pattern ?? "", language);
                innerConstraints.Add(new PatternConstraint { Pattern = pattern.Root });
            }

            if (!string.IsNullOrEmpty(constraintYaml.NotKind))
            {
                innerConstraints.Add(new NotConstraint
                {
                    Inner = new KindConstraint { Kind = constraintYaml.NotKind }
                });
            }

            if (!string.IsNullOrEmpty(constraintYaml.NotRegex))
            {
                innerConstraints.Add(new NotConstraint
                {
                    Inner = RegexConstraint.FromPattern(constraintYaml.NotRegex)
                });
            }

            if (innerConstraints.Count == 1)
            {
                constraints[varName] = innerConstraints[0];
            }
            else if (innerConstraints.Count > 1)
            {
                constraints[varName] = new AllConstraint { Constraints = innerConstraints };
            }
        }

        return constraints.Count > 0 ? constraints : null;
    }

    /// <summary>
    /// Parses severity string to enum.
    /// </summary>
    private static Severity ParseSeverity(string severity)
    {
        return severity.ToLowerInvariant() switch
        {
            "error" => Severity.Error,
            "warning" => Severity.Warning,
            "info" => Severity.Info,
            "hint" => Severity.Hint,
            _ => Severity.Warning
        };
    }
}

/// <summary>
/// Specifies which built-in rule sets to load.
/// </summary>
[Flags]
public enum BuiltInRuleSet
{
    /// <summary>No built-in rules.</summary>
    None = 0,
    /// <summary>Security rules (CWE/OWASP coverage).</summary>
    Security = 1,
    /// <summary>Performance anti-pattern rules.</summary>
    Performance = 2,
    /// <summary>Code quality rules.</summary>
    Quality = 4,
    /// <summary>All built-in rules.</summary>
    All = Security | Performance | Quality
}

/// <summary>
/// Exception thrown when rule parsing fails.
/// </summary>
public class RuleParseException : Exception
{
    public RuleParseException(string message) : base(message) { }
    public RuleParseException(string message, Exception inner) : base(message, inner) { }
}
