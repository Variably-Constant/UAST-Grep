using System.CommandLine;
using System.CommandLine.NamingConventionBinder;
using UAST.Core.Interfaces;
using UAST.Core.Schema;
using UAST.Parsers;
using Spectre.Console;

namespace UAST.Cli.Commands;

/// <summary>
/// Command for searching by native tree-sitter node type.
/// Bypasses UAST pattern matching for maximum performance when searching by node type.
/// </summary>
public static class NativeSearchCommand
{
    /// <summary>
    /// Creates the native search command.
    /// </summary>
    public static Command Create(Dictionary<string, ILanguageMapper> mappers)
    {
        var command = new Command("native", "Search by native tree-sitter node type (fast)");

        var typeOption = new Option<string>(["-t", "--type"], "Native node type to search for (e.g., function_definition)")
        {
            IsRequired = true
        };

        var languageOption = new Option<string>(["-l", "--lang"], "Language (powershell, csharp, rust, etc.)")
        {
            IsRequired = true
        };

        var jsonOption = new Option<bool>(["--json"], "Output results as JSON");

        var countOption = new Option<bool>(["-c", "--count"], "Only count matches");

        var filesArg = new Argument<string[]>("files", "Files or directories to search")
        {
            Arity = ArgumentArity.OneOrMore
        };

        command.AddOption(typeOption);
        command.AddOption(languageOption);
        command.AddOption(jsonOption);
        command.AddOption(countOption);
        command.AddArgument(filesArg);

        command.Handler = CommandHandler.Create<string, string, bool, bool, string[]>(
            (type, lang, json, count, files) => Execute(mappers, type, lang, json, count, files));

        return command;
    }

    private static int Execute(
        Dictionary<string, ILanguageMapper> mappers,
        string type,
        string lang,
        bool json,
        bool count,
        string[] files)
    {
        if (!mappers.TryGetValue(lang, out var mapper))
        {
            AnsiConsole.MarkupLine($"[red]Unknown language: {lang}[/]");
            AnsiConsole.MarkupLine($"Supported: {string.Join(", ", mappers.Values.Select(m => m.LanguageName).Distinct())}");
            return 1;
        }

        var expandedFiles = ExpandFiles(files, mapper.FileExtensions).ToList();

        if (expandedFiles.Count == 0)
        {
            AnsiConsole.MarkupLine("[yellow]No matching files found.[/]");
            return 0;
        }

        var allMatches = new System.Collections.Concurrent.ConcurrentBag<(string file, UastNode node)>();

        // Process files in parallel for performance
        Parallel.ForEach(expandedFiles, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            try
            {
                var threadMapper = MapperFactory.GetMapper(lang);
                var tree = threadMapper.ParseFile(file);

                foreach (var match in tree.DescendantsByNativeType(type))
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
                AnsiConsole.MarkupLine($"[green]{allMatches.Count}[/] matches found (native type: {type})");
            }
        }
        else if (json)
        {
            OutputJsonResults(allMatches.ToList(), type);
        }
        else
        {
            OutputConsoleResults(allMatches.ToList(), type);
        }

        return allMatches.Count > 0 ? 0 : 1;
    }

    private static void OutputConsoleResults(List<(string file, UastNode node)> matches, string type)
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

            foreach (var (_, node) in fileGroup)
            {
                var span = node.Span;
                var source = node.RawSource ?? "";
                var preview = source.Length > 100 ? source[..100] + "..." : source;
                preview = preview.Replace("\r", "").Replace("\n", " ");

                AnsiConsole.MarkupLine($"  [dim]{span.StartLine}:{span.StartColumn}[/] [green]{Markup.Escape(type)}[/]");
                AnsiConsole.MarkupLine($"    {Markup.Escape(preview)}");
            }

            AnsiConsole.WriteLine();
        }

        AnsiConsole.MarkupLine($"[green]{matches.Count}[/] matches in [blue]{byFile.Count()}[/] files [dim](native type search)[/]");
    }

    private static void OutputJsonResults(List<(string file, UastNode node)> matches, string type)
    {
        var results = matches.Select(m => new
        {
            file = m.file,
            nativeType = type,
            nodeKind = m.node.NodeKind,
            span = new
            {
                startLine = m.node.Span.StartLine,
                startColumn = m.node.Span.StartColumn,
                endLine = m.node.Span.EndLine,
                endColumn = m.node.Span.EndColumn
            },
            source = m.node.RawSource
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
