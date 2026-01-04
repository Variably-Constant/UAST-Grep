using System.CommandLine;
using System.CommandLine.NamingConventionBinder;
using UAST.Core.Interfaces;
using UAST.Native;
using Spectre.Console;

namespace UAST.Cli.Commands;

/// <summary>
/// Command for direct tree-sitter scanning using the Rust FFI backend.
/// Bypasses UAST conversion for maximum performance.
/// Useful for counting specific node types or dumping AST structure.
/// </summary>
public static class DirectCommand
{
    /// <summary>
    /// Creates the direct command.
    /// </summary>
    public static Command Create(Dictionary<string, ILanguageMapper> mappers)
    {
        var command = new Command("direct", "Direct tree-sitter scan (bypasses UAST conversion, uses Rust FFI)")
        {
            Description = "Scan files directly with tree-sitter for maximum performance. " +
                          "Supports rust, c, cpp languages with built-in grammars."
        };

        var languageOption = new Option<string>(["-l", "--lang"], "Language (rust, c, cpp)")
        {
            IsRequired = true
        };

        var typeOption = new Option<string?>(["-t", "--type"], "Filter by node type (e.g., function_definition)");

        var namedOption = new Option<bool>(["-n", "--named"], "Only show named nodes (skip syntax tokens)");

        var statsOption = new Option<bool>(["-s", "--stats"], "Show node type statistics");

        var jsonOption = new Option<bool>(["--json"], "Output results as JSON");

        var depthOption = new Option<int>(["-d", "--depth"], () => int.MaxValue, "Maximum depth to traverse");

        var filesArg = new Argument<string[]>("files", "Files or directories to scan")
        {
            Arity = ArgumentArity.OneOrMore
        };

        command.AddOption(languageOption);
        command.AddOption(typeOption);
        command.AddOption(namedOption);
        command.AddOption(statsOption);
        command.AddOption(jsonOption);
        command.AddOption(depthOption);
        command.AddArgument(filesArg);

        command.Handler = CommandHandler.Create<string, string?, bool, bool, bool, int, string[]>(
            (lang, type, named, stats, json, depth, files) => Execute(mappers, lang, type, named, stats, json, depth, files));

        return command;
    }

    private static int Execute(
        Dictionary<string, ILanguageMapper> mappers,
        string lang,
        string? type,
        bool named,
        bool stats,
        bool json,
        int depth,
        string[] files)
    {
        // Check if the language is supported by the Rust backend
        if (!RustUastParser.IsAvailable(lang))
        {
            AnsiConsole.MarkupLine($"[red]Language '{lang}' is not supported by the Rust FFI backend.[/]");
            AnsiConsole.MarkupLine("[yellow]Supported languages for direct scan: rust, c, cpp[/]");
            return 1;
        }

        if (!mappers.TryGetValue(lang, out var mapper))
        {
            AnsiConsole.MarkupLine($"[red]Unknown language: {lang}[/]");
            return 1;
        }

        var expandedFiles = ExpandFiles(files, mapper.FileExtensions).ToList();

        if (expandedFiles.Count == 0)
        {
            AnsiConsole.MarkupLine("[yellow]No matching files found.[/]");
            return 0;
        }

        if (stats)
        {
            return ExecuteStats(lang, named, json, expandedFiles);
        }

        return ExecuteScan(lang, type, named, json, depth, expandedFiles);
    }

    private static int ExecuteStats(string lang, bool named, bool json, List<string> files)
    {
        var nodeTypeCounts = new System.Collections.Concurrent.ConcurrentDictionary<string, int>();
        int totalNodes = 0;
        int totalFiles = 0;

        Parallel.ForEach(files, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            try
            {
                using var parser = NativeParser.Create(lang);
                var source = File.ReadAllText(file);
                using var tree = parser.Parse(source);

                var nodes = named ? tree.WalkNamed() : tree.Walk();
                foreach (var node in nodes)
                {
                    nodeTypeCounts.AddOrUpdate(node.Kind, 1, (_, count) => count + 1);
                    Interlocked.Increment(ref totalNodes);
                }

                Interlocked.Increment(ref totalFiles);
            }
            catch (Exception ex)
            {
                if (!json)
                {
                    AnsiConsole.MarkupLine($"[red]Error parsing {file}: {Markup.Escape(ex.Message)}[/]");
                }
            }
        });

        if (json)
        {
            var result = new
            {
                totalFiles,
                totalNodes,
                nodeTypes = nodeTypeCounts.OrderByDescending(x => x.Value)
                    .ToDictionary(x => x.Key, x => x.Value)
            };
            Console.WriteLine(System.Text.Json.JsonSerializer.Serialize(result, new System.Text.Json.JsonSerializerOptions
            {
                WriteIndented = true
            }));
        }
        else
        {
            AnsiConsole.MarkupLine($"[blue]Scanned {totalFiles} files, {totalNodes} nodes[/]\n");

            var table = new Table();
            table.AddColumn("Node Type");
            table.AddColumn(new TableColumn("Count").RightAligned());
            table.AddColumn(new TableColumn("Percentage").RightAligned());

            foreach (var (nodeType, count) in nodeTypeCounts.OrderByDescending(x => x.Value).Take(50))
            {
                var percentage = (double)count / totalNodes * 100;
                table.AddRow(
                    $"[green]{Markup.Escape(nodeType)}[/]",
                    count.ToString("N0"),
                    $"{percentage:F1}%"
                );
            }

            AnsiConsole.Write(table);

            if (nodeTypeCounts.Count > 50)
            {
                AnsiConsole.MarkupLine($"\n[dim]... and {nodeTypeCounts.Count - 50} more node types[/]");
            }
        }

        return 0;
    }

    private static int ExecuteScan(string lang, string? type, bool named, bool json, int depth, List<string> files)
    {
        var allMatches = new System.Collections.Concurrent.ConcurrentBag<(string file, NativeNode node)>();

        Parallel.ForEach(files, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            try
            {
                using var parser = NativeParser.Create(lang);
                var source = File.ReadAllText(file);
                using var tree = parser.Parse(source);

                var nodes = named ? tree.WalkNamed() : tree.Walk();
                foreach (var node in nodes)
                {
                    // Filter by type if specified
                    if (type != null && !string.Equals(node.Kind, type, StringComparison.Ordinal))
                    {
                        continue;
                    }

                    allMatches.Add((file, node));
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

        if (json)
        {
            OutputJsonResults(allMatches.ToList());
        }
        else
        {
            OutputConsoleResults(allMatches.ToList(), type);
        }

        return allMatches.Count > 0 ? 0 : 1;
    }

    private static void OutputConsoleResults(List<(string file, NativeNode node)> matches, string? type)
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

            foreach (var (_, node) in fileGroup.Take(100)) // Limit per file
            {
                var text = node.Text;
                var preview = text.Length > 80 ? text[..77] + "..." : text;
                preview = preview.Replace("\r", "").Replace("\n", " ");

                AnsiConsole.MarkupLine($"  [dim]{node.StartRow + 1}:{node.StartColumn}[/] [green]{Markup.Escape(node.Kind)}[/]");
                if (!string.IsNullOrWhiteSpace(preview))
                {
                    AnsiConsole.MarkupLine($"    {Markup.Escape(preview)}");
                }
            }

            if (fileGroup.Count() > 100)
            {
                AnsiConsole.MarkupLine($"  [dim]... and {fileGroup.Count() - 100} more[/]");
            }

            AnsiConsole.WriteLine();
        }

        var typeInfo = type != null ? $" (type: {type})" : "";
        AnsiConsole.MarkupLine($"[green]{matches.Count}[/] matches in [blue]{byFile.Count()}[/] files [dim](direct scan{typeInfo})[/]");
    }

    private static void OutputJsonResults(List<(string file, NativeNode node)> matches)
    {
        var results = matches.Select(m => new
        {
            file = m.file,
            nodeKind = m.node.Kind,
            text = m.node.Text,
            span = new
            {
                startLine = (int)m.node.StartRow + 1,
                startColumn = (int)m.node.StartColumn,
                endLine = (int)m.node.EndRow + 1,
                endColumn = (int)m.node.EndColumn
            },
            isNamed = m.node.IsNamed,
            childCount = m.node.ChildCount
        });

        Console.WriteLine(System.Text.Json.JsonSerializer.Serialize(results, new System.Text.Json.JsonSerializerOptions
        {
            WriteIndented = true
        }));
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
