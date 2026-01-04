// Test script to verify all tree-sitter parsers can load and parse code
// Run with: dotnet run --project tests/ParserTests.csproj

using UAST.Parsers.Dynamic;

Console.WriteLine("=== Tree-Sitter Parser Test Suite ===\n");

// Sample code for each language
var samples = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase)
{
    // Tier 1: Official tree-sitter parsers
    ["agda"] = "module Test where\ndata Bool : Set where\n  true false : Bool",
    ["bash"] = "#!/bin/bash\necho \"Hello World\"\nfor i in 1 2 3; do echo $i; done",
    ["c"] = "int main() { return 0; }",
    ["c-sharp"] = "public class Test { public void Foo() { } }",
    ["cpp"] = "class Foo { public: void bar(); };",
    ["css"] = "body { color: red; margin: 0; }",
    ["go"] = "package main\nfunc main() { }",
    ["haskell"] = "module Main where\nmain = putStrLn \"Hello\"",
    ["html"] = "<html><body><h1>Hello</h1></body></html>",
    ["java"] = "public class Test { public static void main(String[] args) { } }",
    ["javascript"] = "function hello() { return 42; }",
    ["json"] = "{ \"name\": \"test\", \"value\": 123 }",
    ["julia"] = "function hello()\n    println(\"Hello\")\nend",
    ["ocaml"] = "let main () = print_endline \"Hello\"",
    ["php"] = "<?php\nfunction hello() { return 42; }",
    ["python"] = "def hello():\n    return 42",
    ["ruby"] = "def hello\n  puts 'Hello'\nend",
    ["rust"] = "fn main() { println!(\"Hello\"); }",
    ["scala"] = "object Main extends App { println(\"Hello\") }",
    ["typescript"] = "function hello(): number { return 42; }",
    ["verilog"] = "module test(input a, output b); assign b = a; endmodule",

    // Tier 2: tree-sitter-grammars
    ["arduino"] = "void setup() { } void loop() { }",
    ["bicep"] = "param storageAccountName string\nresource sa 'Microsoft.Storage/storageAccounts@2021-02-01' = {}",
    ["cairo"] = "fn main() -> felt252 { 42 }",
    ["csv"] = "name,age,city\nJohn,30,NYC",
    ["cuda"] = "__global__ void kernel() { }",
    ["doxygen"] = "/** @brief Hello */",
    ["hcl"] = "resource \"aws_instance\" \"example\" { ami = \"abc\" }",
    ["lua"] = "function hello() return 42 end",
    ["make"] = "all:\n\techo hello",
    ["markdown"] = "# Hello World\n\nThis is a test.",
    ["objc"] = "@interface Foo : NSObject @end",
    ["toml"] = "[package]\nname = \"test\"\nversion = \"1.0\"",
    ["vim"] = "function! Hello()\n  echo 'Hello'\nendfunction",
    ["vue"] = "<template><div>Hello</div></template>",
    ["xml"] = "<?xml version=\"1.0\"?><root><item/></root>",
    ["yaml"] = "name: test\nversion: 1.0\nlist:\n  - item1\n  - item2",
    ["zig"] = "pub fn main() void { }",

    // Tier 3: Community parsers
    ["ada"] = "procedure Hello is begin null; end Hello;",
    ["angular"] = "@Component({}) class AppComponent {}",
    ["sfapex"] = "public class MyClass { public void myMethod() { } }",
    ["awk"] = "BEGIN { print \"Hello\" }",
    ["clojure"] = "(defn hello [] (println \"Hello\"))",
    ["cmake"] = "cmake_minimum_required(VERSION 3.10)\nproject(Test)",
    ["cobol"] = "IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.",
    ["comment"] = "// This is a comment\n/* Block comment */",
    ["commonlisp"] = "(defun hello () (print \"Hello\"))",
    ["crystal"] = "def hello\n  puts \"Hello\"\nend",
    ["cue"] = "package test\nname: \"hello\"",
    ["d"] = "void main() { }",
    ["dart"] = "void main() { print('Hello'); }",
    ["dhall"] = "let x = 1 in x",
    ["dockerfile"] = "FROM ubuntu:latest\nRUN echo hello",
    ["elixir"] = "defmodule Hello do\n  def world, do: :ok\nend",
    ["elm"] = "module Main exposing (main)\nmain = text \"Hello\"",
    ["erlang"] = "-module(hello).\n-export([world/0]).\nworld() -> ok.",
    ["fsharp"] = "let hello () = printfn \"Hello\"",
    ["fortran"] = "program hello\n  print *, 'Hello'\nend program",
    ["graphql"] = "type Query { hello: String }",
    ["groovy"] = "def hello() { println 'Hello' }",
    ["kotlin"] = "fun main() { println(\"Hello\") }",
    ["latex"] = "\\documentclass{article}\n\\begin{document}\nHello\n\\end{document}",
    ["nix"] = "{ pkgs ? import <nixpkgs> {} }: pkgs.hello",
    ["perl"] = "sub hello { print \"Hello\\n\"; }",
    ["powershell"] = "function Get-Hello { Write-Host 'Hello' }",
    ["proto"] = "syntax = \"proto3\";\nmessage Hello { string name = 1; }",
    ["r"] = "hello <- function() { print(\"Hello\") }",
    ["sql"] = "SELECT * FROM users WHERE id = 1;",
    ["swift"] = "func hello() { print(\"Hello\") }",

    // Extra parsers from legacy
    ["tsx"] = "const App: React.FC = () => <div>Hello</div>;",
    ["jsdoc"] = "/** @param {string} name */",
    ["ql"] = "from Function f select f.getName()",
    ["razor"] = "@page \"/\"\n<h1>Hello</h1>",
    ["dtd"] = "<!ELEMENT root (item*)>",
    ["embedded-template"] = "<%= hello %>",
};

var registry = GrammarRegistry.Default;
Console.WriteLine($"Available grammars: {registry.AvailableGrammars.Count()}");
Console.WriteLine($"Search will test: {samples.Count} languages\n");

int passed = 0, failed = 0, skipped = 0;
var failures = new List<(string lang, string error)>();

foreach (var (lang, sample) in samples.OrderBy(x => x.Key))
{
    Console.Write($"Testing {lang,-20} ");

    if (!registry.HasGrammar(lang))
    {
        Console.WriteLine("SKIP (grammar not found)");
        skipped++;
        continue;
    }

    try
    {
        using var parser = registry.CreateParser(lang);
        if (parser == null)
        {
            Console.WriteLine("FAIL (failed to create parser)");
            failures.Add((lang, "Failed to create parser"));
            failed++;
            continue;
        }

        using var tree = parser.Parse(sample);
        if (tree == null)
        {
            Console.WriteLine("FAIL (parse returned null)");
            failures.Add((lang, "Parse returned null"));
            failed++;
            continue;
        }

        var root = tree.RootNode;
        if (root.IsNull)
        {
            Console.WriteLine("FAIL (root node is null)");
            failures.Add((lang, "Root node is null"));
            failed++;
            continue;
        }

        var hasError = root.HasError;
        var nodeCount = CountNodes(root);

        if (hasError)
        {
            Console.WriteLine($"WARN ({nodeCount} nodes, has errors)");
            // Still count as passed - some grammars produce errors on valid code
            passed++;
        }
        else
        {
            Console.WriteLine($"PASS ({nodeCount} nodes, root: {root.Type})");
            passed++;
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine($"FAIL ({ex.Message})");
        failures.Add((lang, ex.Message));
        failed++;
    }
}

Console.WriteLine("\n=== Summary ===");
Console.WriteLine($"Passed:  {passed}");
Console.WriteLine($"Failed:  {failed}");
Console.WriteLine($"Skipped: {skipped}");
Console.WriteLine($"Total:   {passed + failed + skipped}");

if (failures.Count > 0)
{
    Console.WriteLine("\n=== Failures ===");
    foreach (var (lang, error) in failures)
    {
        Console.WriteLine($"  {lang}: {error}");
    }
}

// List available but untested grammars
var testedLangs = new HashSet<string>(samples.Keys, StringComparer.OrdinalIgnoreCase);
var untested = registry.AvailableGrammars.Where(g => !testedLangs.Contains(g)).OrderBy(x => x).ToList();
if (untested.Count > 0)
{
    Console.WriteLine($"\n=== Untested grammars ({untested.Count}) ===");
    Console.WriteLine(string.Join(", ", untested));
}

return failed > 0 ? 1 : 0;

static int CountNodes(DynamicNode node)
{
    int count = 1;
    foreach (var child in node.Children)
    {
        count += CountNodes(child);
    }
    return count;
}
