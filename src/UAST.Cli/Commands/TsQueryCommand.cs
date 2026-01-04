using System.CommandLine;
using System.CommandLine.NamingConventionBinder;
using UAST.Core.Interfaces;
using UAST.Native;
using Spectre.Console;

namespace UAST.Cli.Commands;

/// <summary>
/// Command for running tree-sitter S-expression queries directly.
/// Uses the native Rust FFI backend for maximum performance.
/// </summary>
public static class TsQueryCommand
{
    /// <summary>
    /// Creates the ts-query command.
    /// </summary>
    public static Command Create(Dictionary<string, ILanguageMapper> mappers)
    {
        var command = new Command("ts-query", "Run tree-sitter S-expression query (uses Rust FFI)");

        var queryOption = new Option<string>(["-q", "--query"], "Tree-sitter S-expression query (e.g., '(function_definition name: (identifier) @name)')")
        {
            IsRequired = true
        };

        var languageOption = new Option<string>(["-l", "--lang"], "Language (rust, c, cpp - languages with Rust backend support)")
        {
            IsRequired = true
        };

        var jsonOption = new Option<bool>(["--json"], "Output results as JSON");

        var countOption = new Option<bool>(["-c", "--count"], "Only count matches");

        var filesArg = new Argument<string[]>("files", "Files or directories to search")
        {
            Arity = ArgumentArity.OneOrMore
        };

        command.AddOption(queryOption);
        command.AddOption(languageOption);
        command.AddOption(jsonOption);
        command.AddOption(countOption);
        command.AddArgument(filesArg);

        command.Handler = CommandHandler.Create<string, string, bool, bool, string[]>(
            (query, lang, json, count, files) => Execute(mappers, query, lang, json, count, files));

        return command;
    }

    private static int Execute(
        Dictionary<string, ILanguageMapper> mappers,
        string query,
        string lang,
        bool json,
        bool count,
        string[] files)
    {
        // Check if the language is supported by the Rust backend
        if (!RustUastParser.IsAvailable(lang))
        {
            AnsiConsole.MarkupLine($"[red]Language '{lang}' is not supported by the Rust FFI backend.[/]");
            AnsiConsole.MarkupLine("[yellow]Supported languages for ts-query: rust, c, cpp[/]");
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

        var allMatches = new System.Collections.Concurrent.ConcurrentBag<(string file, QueryMatch match)>();

        // Process files in parallel for performance
        Parallel.ForEach(expandedFiles, new ParallelOptions { MaxDegreeOfParallelism = Environment.ProcessorCount }, file =>
        {
            try
            {
                using var parser = NativeParser.Create(lang);
                var source = File.ReadAllText(file);
                using var tree = parser.Parse(source);

                foreach (var match in tree.Query(query))
                {
                    allMatches.Add((file, match));
                }
            }
            catch (UastNativeException ex)
            {
                if (!json)
                {
                    AnsiConsole.MarkupLine($"[red]Query error in {file}: {Markup.Escape(ex.Message)}[/]");
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
                AnsiConsole.MarkupLine($"[green]{allMatches.Count}[/] matches found (ts-query)");
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

    private static void OutputConsoleResults(List<(string file, QueryMatch match)> matches)
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
                AnsiConsole.MarkupLine($"  [dim]Pattern {match.PatternIndex}[/]");

                foreach (var capture in match.Captures)
                {
                    var text = capture.Node.Text;
                    var preview = text.Length > 80 ? text[..77] + "..." : text;
                    preview = preview.Replace("\r", "").Replace("\n", " ");

                    AnsiConsole.MarkupLine($"    [yellow]@{capture.Name}[/] [green]{Markup.Escape(capture.Node.Kind)}[/] [{capture.Node.StartRow + 1}:{capture.Node.StartColumn}]");
                    AnsiConsole.MarkupLine($"      {Markup.Escape(preview)}");
                }
            }

            AnsiConsole.WriteLine();
        }

        AnsiConsole.MarkupLine($"[green]{matches.Count}[/] matches in [blue]{byFile.Count()}[/] files [dim](tree-sitter query)[/]");
    }

    private static void OutputJsonResults(List<(string file, QueryMatch match)> matches)
    {
        var results = matches.Select(m => new
        {
            file = m.file,
            patternIndex = m.match.PatternIndex,
            captures = m.match.Captures.Select(c => new
            {
                name = c.Name,
                nodeKind = c.Node.Kind,
                text = c.Node.Text,
                span = new
                {
                    startLine = (int)c.Node.StartRow + 1,
                    startColumn = (int)c.Node.StartColumn,
                    endLine = (int)c.Node.EndRow + 1,
                    endColumn = (int)c.Node.EndColumn
                }
            }).ToList()
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
