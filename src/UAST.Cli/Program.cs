using System.CommandLine;
using System.CommandLine.NamingConventionBinder;
using UAST.Core.Interfaces;
using UAST.Parsers;
using UAST.Parsers.PowerShell;
using UAST.Parsers.Roslyn;
using UAST.Native;  // Rust backend for tree-sitter languages
using UAST.Core.Matching;
using UAST.Core.Schema;
using UAST.Core.Transform;
using UAST.Cli.Commands;  // CLI commands
using Spectre.Console;

// Disambiguate from Spectre.Console.Rule
using RuleDefinition = UAST.Core.Rules.Rule;
using RuleParser = UAST.Core.Rules.RuleParser;
using Severity = UAST.Core.Rules.Severity;
using ConfigYaml = UAST.Core.Rules.ConfigYaml;
using RuleMatch = UAST.Core.Rules.RuleMatch;
using BuiltInRuleSet = UAST.Core.Rules.BuiltInRuleSet;
// Disambiguate DiagnosticResult from Matching and Transform namespaces
using DiagnosticResult = UAST.Core.Transform.DiagnosticResult;

namespace UAST.Cli;

/// <summary>
/// UAST-Grep CLI - Structural code search using native parsers.
/// </summary>
class Program
{
    private static readonly Dictionary<string, ILanguageMapper> Mappers = new(StringComparer.OrdinalIgnoreCase);

    static async Task<int> Main(string[] args)
    {
        // Register mappers
        RegisterMappers();

        var rootCommand = new RootCommand("UAST-Grep - Structural code search using native parsers")
        {
            Name = "UAST-Grep"
        };

        // run command: search for pattern in files
        var runCommand = CreateRunCommand();
        rootCommand.AddCommand(runCommand);

        // scan command: run rules from YAML
        var scanCommand = CreateScanCommand();
        rootCommand.AddCommand(scanCommand);

        // parse command: parse a file and output UAST
        var parseCommand = CreateParseCommand();
        rootCommand.AddCommand(parseCommand);

        // languages command: list supported languages
        var langCommand = new Command("languages", "List supported languages");
        langCommand.SetHandler(() =>
        {
            var table = new Table();
            table.AddColumn("Language");
            table.AddColumn("Extensions");

            foreach (var mapper in Mappers.Values.DistinctBy(m => m.LanguageName))
            {
                table.AddRow(mapper.LanguageName, string.Join(", ", mapper.FileExtensions));
            }

            AnsiConsole.Write(table);
        });
        rootCommand.AddCommand(langCommand);

        // Native tree-sitter commands (use Rust FFI backend)
        var nativeCommand = NativeSearchCommand.Create(Mappers);
        rootCommand.AddCommand(nativeCommand);

        var tsQueryCommand = TsQueryCommand.Create(Mappers);
        rootCommand.AddCommand(tsQueryCommand);

        var directCommand = DirectCommand.Create(Mappers);
        rootCommand.AddCommand(directCommand);

        return await rootCommand.InvokeAsync(args);
    }

    private static void RegisterMappers()
    {
        // Initialize Rust native backend for languages that support it
        try
        {
            var grammarDir = AppContext.BaseDirectory;
            NativeLanguageRegistry.RegisterAllFromDirectory(grammarDir);
        }
        catch
        {
            // Rust backend initialization failed - continue with C# mappers only
        }

        // Register Rust-backed mappers first for supported languages (10-16x faster)
        if (MapperFactory.IsRustBackendAvailable)
        {
            RegisterMapper(RustBackedMapper.CreateRustMapper());
            RegisterMapper(RustBackedMapper.CreateCMapper());
            RegisterMapper(RustBackedMapper.CreateCppMapper());
        }

        // Use MapperFactory to get all available mappers (Rust mappers will be skipped
        // if already registered above, since we register by language name/extension)
        foreach (var mapper in MapperFactory.GetAllMappers())
        {
            try
            {
                // Skip if we already have a Rust-backed mapper for this language
                if (Mappers.ContainsKey(mapper.LanguageName.ToLowerInvariant()))
                    continue;

                RegisterMapper(mapper);
            }
            catch (InvalidOperationException)
            {
                // Some tree-sitter grammars may be incompatible - skip silently
            }
        }
    }

    private static void RegisterMapper(ILanguageMapper mapper)
    {
        // Register by language name
        Mappers[mapper.LanguageName.ToLowerInvariant()] = mapper;

        // Register by file extensions
        foreach (var ext in mapper.FileExtensions)
        {
            Mappers[ext.TrimStart('.').ToLowerInvariant()] = mapper;
        }
    }

    private static Command CreateRunCommand()
    {
        var runCommand = new Command("run", "Search for pattern in files");

        var patternOption = new Option<string>(["-p", "--pattern"], "Pattern to search for")
        {
            IsRequired = true
        };

        var languageOption = new Option<string>(["-l", "--lang"], "Language (powershell, csharp, etc.)")
        {
            IsRequired = true
        };

        var rewriteOption = new Option<string?>(["-r", "--rewrite"], "Replacement template");

        var jsonOption = new Option<bool>(["--json"], "Output results as JSON");

        var countOption = new Option<bool>(["-c", "--count"], "Only count matches");

        var nativeOption = new Option<bool>(["-n", "--native"], "Use native tree-sitter matching (bypasses UAST conversion for ~3x performance)");

        var filesArg = new Argument<string[]>("files", "Files or directories to search")
        {
            Arity = ArgumentArity.OneOrMore
        };

        runCommand.AddOption(patternOption);
        runCommand.AddOption(languageOption);
        runCommand.AddOption(rewriteOption);
        runCommand.AddOption(jsonOption);
        runCommand.AddOption(countOption);
        runCommand.AddOption(nativeOption);
        runCommand.AddArgument(filesArg);

        runCommand.Handler = CommandHandler.Create<string, string, string?, bool, bool, bool, string[]>(
            (pattern, lang, rewrite, json, count, native, files) => RunCommand(pattern, lang, rewrite, json, count, native, files));

        return runCommand;
    }

    private static int RunCommand(string pattern, string lang, string? rewrite, bool json, bool count, bool native, string[] files)
    {
        if (!Mappers.TryGetValue(lang, out var mapper))
        {
            AnsiConsole.MarkupLine($"[red]Unknown language: {lang}[/]");
            AnsiConsole.MarkupLine($"Supported: {string.Join(", ", Mappers.Values.Select(m => m.LanguageName).Distinct())}");
            return 1;
        }

        var expandedFiles = ExpandFiles(files, mapper.FileExtensions).ToList();

        if (expandedFiles.Count == 0)
        {
            AnsiConsole.MarkupLine("[yellow]No matching files found.[/]");
            return 0;
        }

        // Native mode - bypass UAST conversion for maximum performance
        if (native)
        {
            return RunNativeMode(pattern, mapper, json, count, expandedFiles);
        }

        var patternParser = new PatternParser();
        patternParser.RegisterMapper(mapper);

        Pattern parsedPattern;
        try
        {
            parsedPattern = patternParser.ParseSimple(pattern, lang);
        }
        catch (PatternParseException ex)
        {
            AnsiConsole.MarkupLine($"[red]Pattern parse error: {ex.Message}[/]");
            return 1;
        }

        var matcherEngine = new PatternMatcher();
        var allMatches = new System.Collections.Concurrent.ConcurrentBag<(string file, MatchResult match)>();

        // Process files in parallel for performance
        Parallel.ForEach(expandedFiles, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            try
            {
                // Each thread needs its own mapper instance for thread-safety
                var threadMapper = MapperFactory.GetMapper(lang);
                var tree = threadMapper.ParseFile(file);
                var matches = matcherEngine.Match(tree, parsedPattern);

                foreach (var match in matches)
                {
                    allMatches.Add((file, match));
                }
            }
            catch (Exception ex)
            {
                if (!json)
                {
                    AnsiConsole.MarkupLine($"[red]Error parsing {file}: {Markup.Escape(ex.Message)}[/]");
                }
            }
        });

        // Handle rewrite
        if (!string.IsNullOrEmpty(rewrite))
        {
            var rewriter = new Rewriter();
            var fileGroups = allMatches.GroupBy(m => m.file);

            foreach (var group in fileGroups)
            {
                var source = File.ReadAllText(group.Key);
                var ruleMatches = group.Select(m => new RuleMatch(
                    m.match.MatchedNode,
                    m.match.Captures.ToDictionary(c => c.Key, c => (object)c.Value),
                    m.match.Span
                )).ToList();

                var replacements = ruleMatches.Select(m => (
                    Match: m,
                    Replacement: rewriter.Apply(m.MatchedNode, rewrite, m.Captures)
                )).ToList();

                var edits = rewriter.ComputeEdits(source, replacements);
                var newSource = rewriter.ApplyEdits(source, edits);

                File.WriteAllText(group.Key, newSource);

                if (!json)
                {
                    AnsiConsole.MarkupLine($"[green]Rewrote {edits.Count()} occurrences in {Markup.Escape(group.Key)}[/]");
                }
            }

            return 0;
        }

        if (count)
        {
            if (json)
            {
                Console.WriteLine($"{{\"count\": {allMatches.Count}}}");
            }
            else
            {
                AnsiConsole.MarkupLine($"[green]{allMatches.Count}[/] matches found");
            }
        }
        else if (json)
        {
            OutputJsonResults(allMatches.ToList());
        }
        else
        {
            OutputConsoleResults(allMatches.ToList());
        }

        return allMatches.Count > 0 ? 0 : 1;
    }

    /// <summary>
    /// Runs in native tree-sitter mode, bypassing UAST conversion for maximum performance.
    /// Uses parallel processing by default.
    /// </summary>
    private static int RunNativeMode(string pattern, ILanguageMapper mapper, bool json, bool count, List<string> expandedFiles)
    {
        var nativeParser = new NativePatternParser();
        var nativeMatcher = new NativeTreeSitterMatcher();
        var lang = mapper.LanguageName;

        NativePattern parsedPattern;
        try
        {
            parsedPattern = nativeParser.Parse(pattern);
        }
        catch (Exception ex)
        {
            AnsiConsole.MarkupLine($"[red]Pattern parse error: {ex.Message}[/]");
            return 1;
        }

        var allMatches = new System.Collections.Concurrent.ConcurrentBag<(string file, NativeMatchResult match)>();

        // Process files in parallel for performance
        Parallel.ForEach(expandedFiles, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            try
            {
                // Each thread needs its own mapper instance for thread-safety
                var threadMapper = MapperFactory.GetMapper(lang);
                var tree = threadMapper.ParseFile(file);
                var matches = nativeMatcher.Match(tree, parsedPattern);

                foreach (var match in matches)
                {
                    allMatches.Add((file, match));
                }
            }
            catch (Exception ex)
            {
                if (!json)
                {
                    AnsiConsole.MarkupLine($"[red]Error parsing {file}: {Markup.Escape(ex.Message)}[/]");
                }
            }
        });

        if (count)
        {
            if (json)
            {
                Console.WriteLine($"{{\"count\": {allMatches.Count}}}");
            }
            else
            {
                AnsiConsole.MarkupLine($"[green]{allMatches.Count}[/] matches found (native mode)");
            }
        }
        else if (json)
        {
            OutputNativeJsonResults(allMatches.ToList());
        }
        else
        {
            OutputNativeConsoleResults(allMatches.ToList());
        }

        return allMatches.Count > 0 ? 0 : 1;
    }

    private static void OutputNativeConsoleResults(List<(string file, NativeMatchResult match)> matches)
    {
        if (matches.Count == 0)
        {
            AnsiConsole.MarkupLine("[yellow]No matches found.[/]");
            return;
        }

        var byFile = matches.GroupBy(m => m.file);

        foreach (var fileGroup in byFile)
        {
            AnsiConsole.MarkupLine($"[blue]{Markup.Escape(fileGroup.Key)}[/]");

            foreach (var (_, match) in fileGroup)
            {
                var span = match.Span;
                var source = match.Node.RawSource ?? "";
                var preview = source.Length > 100 ? source[..100] + "..." : source;
                preview = preview.Replace("\r", "").Replace("\n", " ");

                AnsiConsole.MarkupLine($"  [dim]{span.StartLine}:{span.StartColumn}[/] [green]{Markup.Escape(match.NativeType)}[/]");
                AnsiConsole.MarkupLine($"    {Markup.Escape(preview)}");

                if (match.Captures != null && match.Captures.Count > 0)
                {
                    foreach (var (name, node) in match.Captures)
                    {
                        var capturePreview = node.RawSource ?? "";
                        if (capturePreview.Length > 50)
                            capturePreview = capturePreview[..50] + "...";
                        AnsiConsole.MarkupLine($"    [yellow]${name}[/] = {Markup.Escape(capturePreview.Replace("\n", " "))}");
                    }
                }
            }

            AnsiConsole.WriteLine();
        }

        AnsiConsole.MarkupLine($"[green]{matches.Count}[/] matches in [blue]{byFile.Count()}[/] files [dim](native mode)[/]");
    }

    private static void OutputNativeJsonResults(List<(string file, NativeMatchResult match)> matches)
    {
        var results = matches.Select(m => new
        {
            file = m.file,
            nativeType = m.match.NativeType,
            span = new
            {
                startLine = m.match.Span.StartLine,
                startColumn = m.match.Span.StartColumn,
                endLine = m.match.Span.EndLine,
                endColumn = m.match.Span.EndColumn
            },
            source = m.match.Node.RawSource,
            captures = m.match.Captures?.ToDictionary(
                c => c.Key,
                c => c.Value.RawSource
            )
        });

        Console.WriteLine(System.Text.Json.JsonSerializer.Serialize(results, new System.Text.Json.JsonSerializerOptions
        {
            WriteIndented = true
        }));
    }

    private static void OutputConsoleResults(List<(string file, MatchResult match)> matches)
    {
        if (matches.Count == 0)
        {
            AnsiConsole.MarkupLine("[yellow]No matches found.[/]");
            return;
        }

        var byFile = matches.GroupBy(m => m.file);

        foreach (var fileGroup in byFile)
        {
            AnsiConsole.MarkupLine($"[blue]{Markup.Escape(fileGroup.Key)}[/]");

            foreach (var (_, match) in fileGroup)
            {
                var span = match.Span;
                var source = match.MatchedNode.RawSource ?? "";
                var preview = source.Length > 100 ? source[..100] + "..." : source;
                preview = preview.Replace("\r", "").Replace("\n", " ");

                AnsiConsole.MarkupLine($"  [dim]{span.StartLine}:{span.StartColumn}[/] [green]{Markup.Escape(match.MatchedNode.NodeKind)}[/]");
                AnsiConsole.MarkupLine($"    {Markup.Escape(preview)}");

                if (match.Captures.Count > 0)
                {
                    foreach (var (name, node) in match.Captures)
                    {
                        var capturePreview = node.RawSource ?? "";
                        if (capturePreview.Length > 50)
                            capturePreview = capturePreview[..50] + "...";
                        AnsiConsole.MarkupLine($"    [yellow]∀{name}[/] = {Markup.Escape(capturePreview.Replace("\n", " "))}");
                    }
                }
            }

            AnsiConsole.WriteLine();
        }

        AnsiConsole.MarkupLine($"[green]{matches.Count}[/] matches in [blue]{byFile.Count()}[/] files");
    }

    private static void OutputJsonResults(List<(string file, MatchResult match)> matches)
    {
        var results = matches.Select(m => new
        {
            file = m.file,
            nodeKind = m.match.MatchedNode.NodeKind,
            span = new
            {
                startLine = m.match.Span.StartLine,
                startColumn = m.match.Span.StartColumn,
                endLine = m.match.Span.EndLine,
                endColumn = m.match.Span.EndColumn
            },
            source = m.match.MatchedNode.RawSource,
            captures = m.match.Captures.ToDictionary(
                c => c.Key,
                c => c.Value.RawSource
            )
        });

        Console.WriteLine(System.Text.Json.JsonSerializer.Serialize(results, new System.Text.Json.JsonSerializerOptions
        {
            WriteIndented = true
        }));
    }

    private static Command CreateScanCommand()
    {
        var scanCommand = new Command("scan", "Scan files using YAML rules");

        var externalOption = new Option<string?>(["-e", "--external"], "Path to external/custom rules YAML file");
        var rulesetOption = new Option<string?>(["-r", "--ruleset"], () => "all", "Built-in ruleset: security, performance, quality, all, none");
        var configOption = new Option<string?>(["-c", "--config"], "Path to config file (UAST-Grep.yaml)");
        var formatOption = new Option<string>(["-f", "--format"], () => "text", "Output format: text, json, sarif");
        var fixOption = new Option<bool>("--fix", "Apply fixes automatically");
        var dryRunOption = new Option<bool>("--dry-run", "Show what fixes would be applied without making changes");
        var severityOption = new Option<string?>(["-s", "--severity"], "Minimum severity to report: hint, info, warning, error");

        var filesArg = new Argument<string[]>("files", "Files or directories to scan")
        {
            Arity = ArgumentArity.ZeroOrMore
        };

        scanCommand.AddOption(externalOption);
        scanCommand.AddOption(rulesetOption);
        scanCommand.AddOption(configOption);
        scanCommand.AddOption(formatOption);
        scanCommand.AddOption(fixOption);
        scanCommand.AddOption(dryRunOption);
        scanCommand.AddOption(severityOption);
        scanCommand.AddArgument(filesArg);

        scanCommand.Handler = CommandHandler.Create<string?, string?, string?, string, bool, bool, string?, string[]>(
            ScanCommand);

        return scanCommand;
    }

    private static int ScanCommand(string? external, string? ruleset, string? config, string format, bool fix, bool dryRun, string? severity, string[] files)
    {
        // Load config if specified or look for default
        var configPath = config ?? FindConfigFile();
        ConfigYaml? configYaml = null;

        var ruleParser = new RuleParser();
        foreach (var mapper in Mappers.Values.DistinctBy(m => m.LanguageName))
        {
            ruleParser.RegisterMapper(mapper);
        }

        if (configPath != null)
        {
            configYaml = ruleParser.ParseConfig(configPath);
        }

        // Parse all rules
        var allRules = new List<RuleDefinition>();

        // Determine external rule files to load
        var ruleFiles = new List<string>();

        if (!string.IsNullOrEmpty(external))
        {
            ruleFiles.Add(external);
        }

        if (configYaml?.Rules != null)
        {
            ruleFiles.AddRange(configYaml.Rules);
        }

        // Smart merging logic:
        // - If only -e provided: Run ONLY external rules (no built-in)
        // - If only -r provided: Run ONLY built-in rules matching that ruleset
        // - If both -e and -r provided: MERGE external + built-in rules
        // - If neither provided: Run ALL built-in rules (default)
        var hasExternalRules = ruleFiles.Count > 0;
        var rulesetProvided = ruleset != null;

        // Determine if we should load built-in rules
        // Load built-in rules if:
        // 1. Neither external nor ruleset provided (default to all)
        // 2. Ruleset explicitly provided (regardless of external)
        // Skip built-in rules if:
        // 1. Only external provided (no ruleset)
        var loadBuiltIn = !hasExternalRules || rulesetProvided;

        if (loadBuiltIn)
        {
            // Default to "all" if not specified
            var rulesetValue = ruleset ?? "all";
            var ruleSet = rulesetValue.ToLowerInvariant() switch
            {
                "none" => BuiltInRuleSet.None,
                "security" => BuiltInRuleSet.Security,
                "performance" => BuiltInRuleSet.Performance,
                "quality" => BuiltInRuleSet.Quality,
                "all" => BuiltInRuleSet.All,
                _ => BuiltInRuleSet.All // Default to all rules
            };

            if (ruleSet != BuiltInRuleSet.None)
            {
                try
                {
                    var builtInRules = ruleParser.ParseEmbedded(ruleSet);
                    allRules.AddRange(builtInRules);
                    AnsiConsole.MarkupLine($"[dim]Loaded {allRules.Count} built-in {rulesetValue} rules[/]");
                }
                catch (Exception ex)
                {
                    AnsiConsole.MarkupLine($"[yellow]Warning: Failed to load built-in rules: {Markup.Escape(ex.Message)}[/]");
                }
            }
        }

        // Parse external rules and merge
        foreach (var ruleFile in ruleFiles)
        {
            try
            {
                var parsedRules = ruleParser.ParseFile(ruleFile);
                allRules.AddRange(parsedRules);
                AnsiConsole.MarkupLine($"[dim]Loaded {parsedRules.Count()} rules from {Markup.Escape(ruleFile)}[/]");
            }
            catch (Exception ex)
            {
                AnsiConsole.MarkupLine($"[red]Error parsing rules from {ruleFile}: {Markup.Escape(ex.Message)}[/]");
                return 1;
            }
        }

        // Check if we have any rules at all
        if (allRules.Count == 0)
        {
            AnsiConsole.MarkupLine("[red]No rules loaded. Use -r/--ruleset to select built-in rules or -e/--external for custom rules[/]");
            return 1;
        }

        // Apply rule overrides from config
        if (configYaml?.RuleOverrides != null)
        {
            foreach (var rule in allRules)
            {
                if (configYaml.RuleOverrides.TryGetValue(rule.Id, out var over))
                {
                    // Note: Rules are immutable records, so we'd need to recreate
                    // For now, we'll filter disabled rules below
                }
            }
        }

        // Filter by disabled rules
        var disabledRules = new HashSet<string>(configYaml?.DisableRules ?? [], StringComparer.OrdinalIgnoreCase);
        allRules = allRules.Where(r => r.Enabled && !disabledRules.Contains(r.Id)).ToList();

        // Parse minimum severity
        var minSeverity = severity?.ToLowerInvariant() switch
        {
            "error" => Severity.Error,
            "warning" => Severity.Warning,
            "info" => Severity.Info,
            "hint" => Severity.Hint,
            _ => Severity.Hint
        };

        // Expand files to scan
        var scanFiles = files.Length > 0 ? files : ["."];
        var allFiles = new List<string>();

        foreach (var lang in allRules.Select(r => r.Language).Distinct())
        {
            if (Mappers.TryGetValue(lang, out var mapper))
            {
                allFiles.AddRange(ExpandFiles(scanFiles, mapper.FileExtensions));
            }
        }

        allFiles = allFiles.Distinct().ToList();

        if (allFiles.Count == 0)
        {
            AnsiConsole.MarkupLine("[yellow]No matching files found.[/]");
            return 0;
        }

        // Scan files in parallel for performance
        var results = new System.Collections.Concurrent.ConcurrentBag<DiagnosticResult>();
        var matcher = new PatternMatcher();
        var rewriter = new Rewriter();

        Parallel.ForEach(allFiles, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            var ext = Path.GetExtension(file).TrimStart('.').ToLowerInvariant();
            if (!Mappers.TryGetValue(ext, out var mapperInfo))
                return;

            // Each thread needs its own mapper instance for thread-safety
            var threadMapper = MapperFactory.GetMapper(mapperInfo.LanguageName);

            UastNode tree;
            try
            {
                tree = threadMapper.ParseFile(file);
            }
            catch (Exception ex)
            {
                AnsiConsole.MarkupLine($"[red]Error parsing {file}: {Markup.Escape(ex.Message)}[/]");
                return;
            }

            var fileRules = allRules.Where(r =>
                string.Equals(r.Language, threadMapper.LanguageName, StringComparison.OrdinalIgnoreCase));

            foreach (var rule in fileRules)
            {
                if (rule.Severity < minSeverity)
                    continue;

                var matches = rule.Pattern.Match(tree, matcher);

                foreach (var match in matches)
                {
                    string? fixedText = null;
                    if (!string.IsNullOrEmpty(rule.Fix))
                    {
                        fixedText = rewriter.Apply(match.MatchedNode, rule.Fix, match.Captures);
                    }

                    results.Add(new DiagnosticResult
                    {
                        RuleId = rule.Id,
                        Severity = rule.Severity,
                        Message = rule.InterpolateMessage(match.Captures),
                        FilePath = file,
                        Span = match.Span,
                        MatchedSource = match.MatchedNode.RawSource,
                        Fix = fixedText,
                        Url = rule.Url,
                        Tags = rule.Tags,
                        Captures = match.Captures
                    });
                }
            }
        });

        // Apply fixes if requested
        if (fix && !dryRun)
        {
            var fixableResults = results.Where(r => !string.IsNullOrEmpty(r.Fix)).ToList();
            var byFile = fixableResults.GroupBy(r => r.FilePath);

            foreach (var group in byFile)
            {
                var source = File.ReadAllText(group.Key);
                var edits = group.Select(r => new TextEdit
                {
                    StartOffset = r.Span.StartOffset,
                    EndOffset = r.Span.EndOffset,
                    NewText = r.Fix!,
                    StartLine = r.Span.StartLine,
                    StartColumn = r.Span.StartColumn,
                    EndLine = r.Span.EndLine,
                    EndColumn = r.Span.EndColumn
                }).OrderByDescending(e => e.StartOffset).ToList();

                var newSource = rewriter.ApplyEdits(source, edits);
                File.WriteAllText(group.Key, newSource);

                AnsiConsole.MarkupLine($"[green]Fixed {edits.Count} issues in {Markup.Escape(group.Key)}[/]");
            }
        }

        // Output results
        OutputFormatter formatter = format.ToLowerInvariant() switch
        {
            "json" => new JsonOutputFormatter(),
            "sarif" => new SarifOutputFormatter(),
            _ => new TextOutputFormatter()
        };

        var options = new FormatOptions
        {
            IncludeSource = true,
            IncludeFixes = true,
            IncludeCaptures = format == "json",
            BasePath = Directory.GetCurrentDirectory()
        };

        var output = formatter.Format(results, options);
        Console.WriteLine(output);

        // Return non-zero if there are errors
        return results.Any(r => r.Severity == Severity.Error) ? 1 : 0;
    }

    private static string? FindConfigFile()
    {
        var candidates = new[] { "UAST-Grep.yaml", "UAST-Grep.yml", ".UAST-Grep.yaml", ".UAST-Grep.yml" };
        var current = Directory.GetCurrentDirectory();

        while (!string.IsNullOrEmpty(current))
        {
            foreach (var candidate in candidates)
            {
                var path = Path.Combine(current, candidate);
                if (File.Exists(path))
                    return path;
            }

            var parent = Directory.GetParent(current)?.FullName;
            if (parent == current)
                break;
            current = parent;
        }

        return null;
    }

    private static Command CreateParseCommand()
    {
        var parseCommand = new Command("parse", "Parse a file and output UAST");

        var languageOption = new Option<string?>(["-l", "--lang"], "Language (auto-detect if not specified)");

        var formatOption = new Option<string>(["-f", "--format"], () => "json", "Output format (json, tree)");

        var fileArg = new Argument<string>("file", "File to parse");

        parseCommand.AddOption(languageOption);
        parseCommand.AddOption(formatOption);
        parseCommand.AddArgument(fileArg);

        parseCommand.Handler = CommandHandler.Create<string?, string, string>((lang, format, file) =>
        {
            if (!File.Exists(file))
            {
                AnsiConsole.MarkupLine($"[red]File not found: {file}[/]");
                return 1;
            }

            // Auto-detect language from extension or full filename
            var ext = Path.GetExtension(file).ToLowerInvariant();
            var fileName = Path.GetFileName(file);
            var detectedLang = lang ?? ext.TrimStart('.');

            // Try to find mapper - first by extension, then by full filename (for Dockerfile, Makefile, etc.)
            if (!Mappers.TryGetValue(detectedLang, out var mapper))
            {
                // Try full filename for files like Dockerfile, Makefile, CMakeLists.txt
                mapper = FindMapperByFileName(fileName);
            }

            if (mapper == null)
            {
                AnsiConsole.MarkupLine($"[red]Unknown language for extension: {ext}[/]");
                return 1;
            }

            try
            {
                var tree = mapper.ParseFile(file);

                if (format == "tree")
                {
                    PrintTree(tree, 0);
                }
                else
                {
                    var json = SerializeUast(tree);
                    Console.WriteLine(json);
                }

                return 0;
            }
            catch (Exception ex)
            {
                AnsiConsole.MarkupLine($"[red]Parse error: {Markup.Escape(ex.Message)}[/]");
                return 1;
            }
        });

        return parseCommand;
    }

    private static void PrintTree(UastNode node, int indent)
    {
        var prefix = new string(' ', indent * 2);
        var span = node.Span.IsValid ? $" @ {node.Span}" : "";
        AnsiConsole.MarkupLine($"{prefix}[green]{node.NodeKind}[/]{span}");

        // Print key properties
        switch (node)
        {
            case Core.Schema.Declarations.FunctionDeclaration func:
                AnsiConsole.MarkupLine($"{prefix}  [dim]Name:[/] {func.Name}");
                break;
            case Core.Schema.Expressions.CommandExpression cmd:
                AnsiConsole.MarkupLine($"{prefix}  [dim]Command:[/] {cmd.CommandName}");
                break;
            case Core.Schema.Expressions.VariableExpression var:
                AnsiConsole.MarkupLine($"{prefix}  [dim]Variable:[/] {var.Name}");
                break;
            case Core.Schema.Expressions.LiteralExpression lit:
                AnsiConsole.MarkupLine($"{prefix}  [dim]Value:[/] {lit.Value}");
                break;
        }

        foreach (var child in node.Children)
        {
            PrintTree(child, indent + 1);
        }
    }

    private static string SerializeUast(UastNode node)
    {
        return System.Text.Json.JsonSerializer.Serialize(ConvertToSerializable(node), new System.Text.Json.JsonSerializerOptions
        {
            WriteIndented = true,
            DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingNull,
            NumberHandling = System.Text.Json.Serialization.JsonNumberHandling.AllowNamedFloatingPointLiterals
        });
    }

    private static object ConvertToSerializable(UastNode node)
    {
        var dict = new Dictionary<string, object?>
        {
            ["nodeKind"] = node.NodeKind,
            ["language"] = node.Language,
            ["span"] = new
            {
                startLine = node.Span.StartLine,
                startColumn = node.Span.StartColumn,
                endLine = node.Span.EndLine,
                endColumn = node.Span.EndColumn
            }
        };

        // Add type-specific properties
        switch (node)
        {
            case Core.Schema.Declarations.FunctionDeclaration func:
                dict["name"] = func.Name;
                dict["parameters"] = func.Parameters.Select(p => p.Name).ToList();
                break;
            case Core.Schema.Expressions.CommandExpression cmd:
                dict["commandName"] = cmd.CommandName;
                break;
            case Core.Schema.Expressions.VariableExpression var:
                dict["variableName"] = var.Name;
                dict["scope"] = var.Scope;
                break;
            case Core.Schema.Expressions.LiteralExpression lit:
                dict["value"] = lit.Value;
                dict["kind"] = lit.Kind.ToString();
                break;
            case Core.Schema.Expressions.BinaryExpression bin:
                dict["operator"] = bin.Operator.ToString();
                break;
        }

        if (node.Children.Count > 0)
        {
            dict["children"] = node.Children.Select(ConvertToSerializable).ToList();
        }

        return dict;
    }

    /// <summary>
    /// Finds a mapper by matching the full filename against registered file extensions.
    /// This handles special cases like Dockerfile, Makefile, CMakeLists.txt which have
    /// no file extension or use the full filename as the identifier.
    /// </summary>
    private static ILanguageMapper? FindMapperByFileName(string fileName)
    {
        foreach (var mapper in Mappers.Values.DistinctBy(m => m.LanguageName))
        {
            foreach (var ext in mapper.FileExtensions)
            {
                // Check if the extension is actually a full filename (no leading dot, or contains dots in name)
                // e.g., "Dockerfile", "Makefile", "CMakeLists.txt"
                if (!ext.StartsWith('.') && ext.Equals(fileName, StringComparison.OrdinalIgnoreCase))
                {
                    return mapper;
                }
                // Check for patterns like "*.mk" matching against the file
                if (ext.StartsWith("*.") && fileName.EndsWith(ext[1..], StringComparison.OrdinalIgnoreCase))
                {
                    return mapper;
                }
                // Check if file matches extension pattern like "CMakeLists.txt"
                if (ext.Contains('.') && !ext.StartsWith('.') && fileName.Equals(ext, StringComparison.OrdinalIgnoreCase))
                {
                    return mapper;
                }
            }
        }
        return null;
    }

    private static IEnumerable<string> ExpandFiles(string[] paths, IReadOnlyList<string> extensions)
    {
        foreach (var path in paths)
        {
            if (File.Exists(path))
            {
                yield return path;
            }
            else if (Directory.Exists(path))
            {
                foreach (var ext in extensions)
                {
                    foreach (var file in Directory.EnumerateFiles(path, $"*{ext}", SearchOption.AllDirectories))
                    {
                        yield return file;
                    }
                }
            }
            else if (path.Contains('*'))
            {
                // Glob pattern
                var dir = Path.GetDirectoryName(path);
                if (string.IsNullOrEmpty(dir)) dir = ".";
                var pattern = Path.GetFileName(path);

                if (Directory.Exists(dir))
                {
                    foreach (var file in Directory.EnumerateFiles(dir, pattern, SearchOption.AllDirectories))
                    {
                        if (extensions.Any(e => file.EndsWith(e, StringComparison.OrdinalIgnoreCase)))
                        {
                            yield return file;
                        }
                    }
                }
            }
        }
    }
}
