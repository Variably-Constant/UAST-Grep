using UAST.Parsers;
using UAST.Core.Schema;

Console.WriteLine("=== UAST Mapper Test Suite ===\n");

var testCases = new Dictionary<string, string>
{
    // New mappers we added
    ["kotlin"] = "fun main() { println(\"Hello\") }",
    ["dart"] = "void main() { print('Hello'); }",
    ["lua"] = "function hello() print('Hello') end",
    ["yaml"] = "name: test\nversion: 1.0",
    ["sql"] = "SELECT * FROM users WHERE id = 1",
    ["xml"] = "<root><item>Hello</item></root>",
    ["dockerfile"] = "FROM alpine\nRUN echo hello",
    ["make"] = "all:\n\techo hello",
    ["markdown"] = "# Hello\n\nThis is text",
    ["vue"] = "<template><div>Hello</div></template>",
    ["proto"] = "message User { string name = 1; }",
    ["graphql"] = "type Query { user: User }",
    ["r"] = "hello <- function() { print('Hi') }",
    ["zig"] = "pub fn main() void {}",
    ["elixir"] = "def hello, do: :ok",
    ["cmake"] = "project(hello)\nadd_executable(hello main.c)",
    ["nix"] = "{ pkgs ? import <nixpkgs> {} }: pkgs.hello",
    ["fsharp"] = "let hello = printfn \"Hello\"",
    ["hcl"] = "resource \"aws_instance\" \"web\" { ami = \"ami-123\" }",
    ["perl"] = "sub hello { print \"Hello\"; }",
    ["erlang"] = "-module(hello).\nhello() -> ok.",
    ["groovy"] = "def hello() { println 'Hello' }",
    ["clojure"] = "(defn hello [] (println \"Hello\"))",
};

int passed = 0, failed = 0;

foreach (var (lang, code) in testCases)
{
    try
    {
        var mapper = MapperFactory.GetMapper(lang);
        var root = mapper.Parse(code);

        if (root != null)
        {
            var nodeCount = CountNodes(root);
            Console.WriteLine($"  {lang,-12} PASS ({nodeCount} UAST nodes, root: {root.NodeKind})");
            passed++;
        }
        else
        {
            Console.WriteLine($"  {lang,-12} FAIL (null result)");
            failed++;
        }
    }
    catch (Exception ex)
    {
        Console.WriteLine($"  {lang,-12} FAIL ({ex.GetType().Name}: {ex.Message.Split('\n')[0]})");
        failed++;
    }
}

Console.WriteLine($"\n=== Summary ===");
Console.WriteLine($"Passed: {passed}");
Console.WriteLine($"Failed: {failed}");
Console.WriteLine($"Total:  {testCases.Count}");

static int CountNodes(UastNode node)
{
    int count = 1;
    foreach (var child in node.Children)
        count += CountNodes(child);
    return count;
}
