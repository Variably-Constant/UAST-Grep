using System.Diagnostics;
using System.Text.Json;
using UAST.Core.Interfaces;
using UAST.Core.Schema;
using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Parsers.PowerShell;

/// <summary>
/// PowerShell mapper that uses a subprocess to parse PowerShell code.
/// This avoids bundling the PowerShell SDK with the CLI.
/// </summary>
public class SubprocessPowerShellMapper : ILanguageMapper
{
    private readonly string _parserScriptPath;
    private readonly List<ParseError> _errors = [];

    public string LanguageName => "PowerShell";
    public IReadOnlyList<string> FileExtensions => [".ps1", ".psm1", ".psd1"];

    public SubprocessPowerShellMapper()
    {
        // Look for parser script relative to the CLI executable
        var exeDir = AppContext.BaseDirectory;
        _parserScriptPath = Path.Combine(exeDir, "parsers", "powershell", "Parse-ToJson.ps1");

        if (!File.Exists(_parserScriptPath))
        {
            // Try relative to current directory for development
            _parserScriptPath = Path.Combine(Directory.GetCurrentDirectory(), "tools", "ps-parser", "Parse-ToJson.ps1");
        }
    }

    public SubprocessPowerShellMapper(string parserScriptPath)
    {
        _parserScriptPath = parserScriptPath;
    }

    public UastNode Parse(string source, string? filePath = null)
    {
        _errors.Clear();

        var jsonAst = InvokeParser(source, null);
        var result = ConvertFromJson(jsonAst, source);
        result.SetParentReferences();
        return result;
    }

    public UastNode ParseFile(string filePath)
    {
        _errors.Clear();

        var source = File.ReadAllText(filePath);
        var jsonAst = InvokeParser(null, filePath);
        var result = ConvertFromJson(jsonAst, source);
        result.SetParentReferences();
        return result;
    }

    public IReadOnlyList<ParseError> GetErrors() => _errors;

    private string InvokeParser(string? source, string? filePath)
    {
        // Find pwsh or powershell
        var pwsh = FindPowerShell();

        var startInfo = new ProcessStartInfo
        {
            FileName = pwsh,
            RedirectStandardInput = true,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            UseShellExecute = false,
            CreateNoWindow = true
        };

        if (filePath != null)
        {
            startInfo.ArgumentList.Add("-NoProfile");
            startInfo.ArgumentList.Add("-File");
            startInfo.ArgumentList.Add(_parserScriptPath);
            startInfo.ArgumentList.Add("-Path");
            startInfo.ArgumentList.Add(filePath);
        }
        else
        {
            startInfo.ArgumentList.Add("-NoProfile");
            startInfo.ArgumentList.Add("-File");
            startInfo.ArgumentList.Add(_parserScriptPath);
            startInfo.ArgumentList.Add("-Source");
        }

        using var process = Process.Start(startInfo)
            ?? throw new InvalidOperationException("Failed to start PowerShell process");

        if (source != null)
        {
            process.StandardInput.Write(source);
            process.StandardInput.Close();
        }

        var output = process.StandardOutput.ReadToEnd();
        var error = process.StandardError.ReadToEnd();
        process.WaitForExit();

        if (process.ExitCode != 0)
        {
            throw new InvalidOperationException($"PowerShell parser failed: {error}\n{output}");
        }

        return output;
    }

    private static string FindPowerShell()
    {
        // Try pwsh first (PowerShell 7+)
        if (TryFindExecutable("pwsh", out var pwsh))
            return pwsh;

        // Fall back to powershell (Windows PowerShell)
        if (TryFindExecutable("powershell", out var ps))
            return ps;

        // Direct paths for Windows
        if (OperatingSystem.IsWindows())
        {
            var pwshPath = @"C:\Program Files\PowerShell\7\pwsh.exe";
            if (File.Exists(pwshPath))
                return pwshPath;

            var psPath = @"C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe";
            if (File.Exists(psPath))
                return psPath;
        }

        throw new InvalidOperationException("PowerShell not found. Please install PowerShell 7 or ensure powershell/pwsh is in PATH.");
    }

    private static bool TryFindExecutable(string name, out string path)
    {
        path = "";
        try
        {
            var startInfo = new ProcessStartInfo
            {
                FileName = OperatingSystem.IsWindows() ? "where" : "which",
                Arguments = name,
                RedirectStandardOutput = true,
                UseShellExecute = false,
                CreateNoWindow = true
            };

            using var process = Process.Start(startInfo);
            if (process == null) return false;

            path = process.StandardOutput.ReadLine() ?? "";
            process.WaitForExit();
            return process.ExitCode == 0 && !string.IsNullOrEmpty(path);
        }
        catch
        {
            return false;
        }
    }

    private UastNode ConvertFromJson(string json, string source)
    {
        using var doc = JsonDocument.Parse(json);
        var root = doc.RootElement;

        if (root.TryGetProperty("Error", out var error))
        {
            throw new InvalidOperationException($"Parser error: {error.GetString()}");
        }

        if (root.TryGetProperty("Errors", out var errors))
        {
            foreach (var err in errors.EnumerateArray())
            {
                _errors.Add(new ParseError(
                    err.GetProperty("Message").GetString() ?? "",
                    ConvertExtent(err.GetProperty("Extent")),
                    err.TryGetProperty("ErrorId", out var id) ? id.GetString() : null
                ));
            }
        }

        var ast = root.GetProperty("Ast");
        return ConvertNode(ast, source);
    }

    private UastNode ConvertNode(JsonElement node, string source)
    {
        var type = node.GetProperty("Type").GetString() ?? "";
        var extent = ConvertExtent(node.GetProperty("Extent"));
        var rawSource = GetRawSource(node, source);

        return type switch
        {
            "ScriptBlockAst" => ConvertScriptBlock(node, source, extent, rawSource),
            "FunctionDefinitionAst" => ConvertFunctionDefinition(node, source, extent, rawSource),
            "IfStatementAst" => ConvertIf(node, source, extent, rawSource),
            "ForStatementAst" => ConvertFor(node, source, extent, rawSource),
            "ForEachStatementAst" => ConvertForEach(node, source, extent, rawSource),
            "WhileStatementAst" => ConvertWhile(node, source, extent, rawSource),
            "DoWhileStatementAst" => ConvertDoWhile(node, source, extent, rawSource, false),
            "DoUntilStatementAst" => ConvertDoWhile(node, source, extent, rawSource, true),
            "SwitchStatementAst" => ConvertSwitch(node, source, extent, rawSource),
            "TryStatementAst" => ConvertTry(node, source, extent, rawSource),
            "BreakStatementAst" => ConvertBreak(node, extent, rawSource),
            "ContinueStatementAst" => ConvertContinue(node, extent, rawSource),
            "ReturnStatementAst" => ConvertReturn(node, source, extent, rawSource),
            "ExitStatementAst" => ConvertExit(node, source, extent, rawSource),
            "ThrowStatementAst" => ConvertThrow(node, source, extent, rawSource),
            "PipelineAst" => ConvertPipeline(node, source, extent, rawSource),
            "CommandAst" => ConvertCommand(node, source, extent, rawSource),
            "CommandExpressionAst" => ConvertCommandExpression(node, source, extent, rawSource),
            "AssignmentStatementAst" => ConvertAssignment(node, source, extent, rawSource),
            "BinaryExpressionAst" => ConvertBinary(node, source, extent, rawSource),
            "UnaryExpressionAst" => ConvertUnary(node, source, extent, rawSource),
            "ConstantExpressionAst" or "StringConstantExpressionAst" => ConvertConstant(node, extent, rawSource),
            "ExpandableStringExpressionAst" => ConvertExpandableString(node, source, extent, rawSource),
            "VariableExpressionAst" => ConvertVariable(node, extent, rawSource),
            "ArrayExpressionAst" => ConvertArrayExpression(node, source, extent, rawSource),
            "ArrayLiteralAst" => ConvertArrayLiteral(node, source, extent, rawSource),
            "HashtableAst" => ConvertHashtable(node, source, extent, rawSource),
            "IndexExpressionAst" => ConvertIndex(node, source, extent, rawSource),
            "MemberExpressionAst" => ConvertMember(node, source, extent, rawSource),
            "InvokeMemberExpressionAst" => ConvertInvokeMember(node, source, extent, rawSource),
            "ScriptBlockExpressionAst" => ConvertScriptBlockExpr(node, source, extent, rawSource),
            "ParenExpressionAst" => ConvertParen(node, source, extent, rawSource),
            "SubExpressionAst" => ConvertSubExpr(node, source, extent, rawSource),
            "StatementBlockAst" => ConvertStatementBlock(node, source, extent, rawSource),
            "NamedBlockAst" => ConvertNamedBlock(node, source, extent, rawSource),
            _ => CreateUnknown(type, extent, rawSource)
        };
    }

    private BlockNode ConvertScriptBlock(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var statements = new List<StatementNode>();

        if (node.TryGetProperty("BeginBlock", out var begin) && begin.ValueKind != JsonValueKind.Null)
            statements.AddRange(ConvertNamedBlockStatements(begin, source));
        if (node.TryGetProperty("ProcessBlock", out var process) && process.ValueKind != JsonValueKind.Null)
            statements.AddRange(ConvertNamedBlockStatements(process, source));
        if (node.TryGetProperty("EndBlock", out var end) && end.ValueKind != JsonValueKind.Null)
            statements.AddRange(ConvertNamedBlockStatements(end, source));

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Statements = statements
        };
    }

    private IEnumerable<StatementNode> ConvertNamedBlockStatements(JsonElement block, string source)
    {
        if (block.TryGetProperty("Statements", out var stmts))
        {
            foreach (var stmt in stmts.EnumerateArray())
            {
                var node = ConvertNode(stmt, source);
                if (node is StatementNode stmtNode)
                    yield return stmtNode;
                else if (node is ExpressionNode expr)
                    yield return new ExpressionStatement
                    {
                        NodeKind = "ExpressionStatement",
                        Language = LanguageName,
                        Span = node.Span,
                        RawSource = node.RawSource,
                        Expression = expr
                    };
            }
        }
    }

    private FunctionDeclaration ConvertFunctionDefinition(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var name = node.GetProperty("Name").GetString() ?? "";
        var parameters = new List<ParameterNode>();

        if (node.TryGetProperty("Parameters", out var paramsElem))
        {
            foreach (var p in paramsElem.EnumerateArray())
            {
                parameters.Add(ConvertParameter(p, source));
            }
        }

        BlockNode? body = null;
        if (node.TryGetProperty("Body", out var bodyElem) && bodyElem.ValueKind != JsonValueKind.Null)
        {
            var bodyNode = ConvertNode(bodyElem, source);
            body = bodyNode as BlockNode;
        }

        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Name = name,
            Parameters = parameters,
            Body = body
        };
    }

    private ParameterNode ConvertParameter(JsonElement node, string source)
    {
        var name = node.GetProperty("Name").GetString() ?? "";
        var extent = ConvertExtent(node.GetProperty("Extent"));

        TypeReference? type = null;
        if (node.TryGetProperty("StaticType", out var typeElem) && typeElem.ValueKind != JsonValueKind.Null)
        {
            var typeName = typeElem.GetString();
            if (!string.IsNullOrEmpty(typeName) && typeName != "System.Object")
            {
                type = new NamedTypeReference
                {
                    NodeKind = "NamedTypeReference",
                    Language = LanguageName,
                    Span = SourceSpan.Empty,
                    Name = typeName.Split('.').Last()
                };
            }
        }

        ExpressionNode? defaultValue = null;
        if (node.TryGetProperty("DefaultValue", out var defaultElem) && defaultElem.ValueKind != JsonValueKind.Null)
        {
            defaultValue = ConvertNode(defaultElem, source) as ExpressionNode;
        }

        return new ParameterNode
        {
            NodeKind = "ParameterNode",
            Language = LanguageName,
            Span = extent,
            Name = name,
            Type = type,
            DefaultValue = defaultValue
        };
    }

    private IfStatement ConvertIf(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var clauses = node.GetProperty("Clauses").EnumerateArray().ToList();
        var firstClause = clauses[0];

        var condition = ConvertNode(firstClause.GetProperty("Condition"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid if condition");
        var thenBranch = ConvertNode(firstClause.GetProperty("Body"), source) as StatementNode
            ?? throw new InvalidOperationException("Invalid if body");

        StatementNode? elseBranch = null;
        if (clauses.Count > 1)
        {
            // Build elseif chain
            elseBranch = BuildElseIfChain(clauses.Skip(1).ToList(), node, source);
        }
        else if (node.TryGetProperty("ElseClause", out var elseClause) && elseClause.ValueKind != JsonValueKind.Null)
        {
            elseBranch = ConvertNode(elseClause, source) as StatementNode;
        }

        return new IfStatement
        {
            NodeKind = "IfStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Condition = condition,
            ThenBranch = thenBranch,
            ElseBranch = elseBranch
        };
    }

    private StatementNode BuildElseIfChain(List<JsonElement> remainingClauses, JsonElement originalNode, string source)
    {
        if (remainingClauses.Count == 0)
        {
            if (originalNode.TryGetProperty("ElseClause", out var elseClause) && elseClause.ValueKind != JsonValueKind.Null)
            {
                return ConvertNode(elseClause, source) as StatementNode ?? new EmptyStatement
                {
                    NodeKind = "EmptyStatement",
                    Language = LanguageName,
                    Span = SourceSpan.Empty
                };
            }
            return new EmptyStatement
            {
                NodeKind = "EmptyStatement",
                Language = LanguageName,
                Span = SourceSpan.Empty
            };
        }

        var clause = remainingClauses[0];
        var condition = ConvertNode(clause.GetProperty("Condition"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid if condition");
        var thenBranch = ConvertNode(clause.GetProperty("Body"), source) as StatementNode
            ?? throw new InvalidOperationException("Invalid if body");

        return new IfStatement
        {
            NodeKind = "IfStatement",
            Language = LanguageName,
            Span = ConvertExtent(clause.GetProperty("Condition").GetProperty("Extent")),
            Condition = condition,
            ThenBranch = thenBranch,
            ElseBranch = BuildElseIfChain(remainingClauses.Skip(1).ToList(), originalNode, source)
        };
    }

    private ForStatement ConvertFor(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        StatementNode? init = null;
        if (node.TryGetProperty("Initializer", out var initElem) && initElem.ValueKind != JsonValueKind.Null)
        {
            var initNode = ConvertNode(initElem, source);
            init = initNode as StatementNode ?? (initNode is ExpressionNode expr ? new ExpressionStatement
            {
                NodeKind = "ExpressionStatement",
                Language = LanguageName,
                Span = initNode.Span,
                Expression = expr
            } : null);
        }

        ExpressionNode? condition = null;
        if (node.TryGetProperty("Condition", out var condElem) && condElem.ValueKind != JsonValueKind.Null)
            condition = ConvertNode(condElem, source) as ExpressionNode;

        ExpressionNode? increment = null;
        if (node.TryGetProperty("Iterator", out var iterElem) && iterElem.ValueKind != JsonValueKind.Null)
            increment = ConvertNode(iterElem, source) as ExpressionNode;

        var body = ConvertNode(node.GetProperty("Body"), source) as StatementNode
            ?? throw new InvalidOperationException("Invalid for body");

        return new ForStatement
        {
            NodeKind = "ForStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Initializer = init,
            Condition = condition,
            Increment = increment,
            Body = body
        };
    }

    private ForEachStatement ConvertForEach(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var varName = node.GetProperty("Variable").GetString() ?? "";
        var iterable = ConvertNode(node.GetProperty("Condition"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid foreach iterable");
        var body = ConvertNode(node.GetProperty("Body"), source) as StatementNode
            ?? throw new InvalidOperationException("Invalid foreach body");

        return new ForEachStatement
        {
            NodeKind = "ForEachStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            VariableName = varName,
            Iterable = iterable,
            Body = body
        };
    }

    private WhileStatement ConvertWhile(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var condition = ConvertNode(node.GetProperty("Condition"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid while condition");
        var body = ConvertNode(node.GetProperty("Body"), source) as StatementNode
            ?? throw new InvalidOperationException("Invalid while body");

        return new WhileStatement
        {
            NodeKind = "WhileStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Condition = condition,
            Body = body
        };
    }

    private DoWhileStatement ConvertDoWhile(JsonElement node, string source, SourceSpan extent, string? rawSource, bool isUntil)
    {
        var body = ConvertNode(node.GetProperty("Body"), source) as StatementNode
            ?? throw new InvalidOperationException("Invalid do-while body");
        var condition = ConvertNode(node.GetProperty("Condition"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid do-while condition");

        return new DoWhileStatement
        {
            NodeKind = "DoWhileStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Body = body,
            Condition = condition,
            IsUntil = isUntil
        };
    }

    private SwitchStatement ConvertSwitch(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var subject = ConvertNode(node.GetProperty("Condition"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid switch subject");

        var cases = new List<SwitchCase>();
        if (node.TryGetProperty("Clauses", out var clauses))
        {
            foreach (var clause in clauses.EnumerateArray())
            {
                var patterns = new List<ExpressionNode>();
                if (clause.TryGetProperty("Pattern", out var pattern) && pattern.ValueKind != JsonValueKind.Null)
                {
                    if (ConvertNode(pattern, source) is ExpressionNode patternExpr)
                        patterns.Add(patternExpr);
                }

                var bodyNode = ConvertNode(clause.GetProperty("Body"), source);
                var bodyStatements = bodyNode is BlockNode block ? block.Statements.ToList()
                    : bodyNode is StatementNode stmt ? new List<StatementNode> { stmt }
                    : [];

                cases.Add(new SwitchCase
                {
                    NodeKind = "SwitchCase",
                    Language = LanguageName,
                    Span = ConvertExtent(clause.GetProperty("Body").GetProperty("Extent")),
                    Patterns = patterns,
                    Body = bodyStatements
                });
            }
        }

        var flags = new List<string>();
        if (node.TryGetProperty("Flags", out var flagsElem))
        {
            var flagStr = flagsElem.GetString() ?? "";
            if (flagStr.Contains("Regex")) flags.Add("Regex");
            if (flagStr.Contains("Wildcard")) flags.Add("Wildcard");
            if (flagStr.Contains("Exact")) flags.Add("Exact");
            if (flagStr.Contains("CaseSensitive")) flags.Add("CaseSensitive");
        }

        return new SwitchStatement
        {
            NodeKind = "SwitchStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Subject = subject,
            Cases = cases,
            Flags = flags
        };
    }

    private TryStatement ConvertTry(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var tryBlock = ConvertNode(node.GetProperty("Body"), source) as BlockNode
            ?? throw new InvalidOperationException("Invalid try body");

        var catchClauses = new List<CatchClause>();
        if (node.TryGetProperty("CatchClauses", out var catches))
        {
            foreach (var c in catches.EnumerateArray())
            {
                var catchBody = ConvertNode(c.GetProperty("Body"), source) as BlockNode
                    ?? throw new InvalidOperationException("Invalid catch body");

                var exTypes = new List<TypeReference>();
                if (c.TryGetProperty("CatchTypes", out var types))
                {
                    foreach (var t in types.EnumerateArray())
                    {
                        var typeName = t.GetString() ?? "";
                        exTypes.Add(new NamedTypeReference
                        {
                            NodeKind = "NamedTypeReference",
                            Language = LanguageName,
                            Span = SourceSpan.Empty,
                            Name = typeName.Split('.').Last()
                        });
                    }
                }

                catchClauses.Add(new CatchClause
                {
                    NodeKind = "CatchClause",
                    Language = LanguageName,
                    Span = ConvertExtent(c.GetProperty("Body").GetProperty("Extent")),
                    ExceptionTypes = exTypes,
                    Body = catchBody
                });
            }
        }

        BlockNode? finallyBlock = null;
        if (node.TryGetProperty("Finally", out var fin) && fin.ValueKind != JsonValueKind.Null)
        {
            finallyBlock = ConvertNode(fin, source) as BlockNode;
        }

        return new TryStatement
        {
            NodeKind = "TryStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            TryBlock = tryBlock,
            CatchClauses = catchClauses,
            FinallyBlock = finallyBlock
        };
    }

    private BreakStatement ConvertBreak(JsonElement node, SourceSpan extent, string? rawSource)
    {
        string? label = null;
        if (node.TryGetProperty("Label", out var labelElem) && labelElem.ValueKind != JsonValueKind.Null)
            label = labelElem.GetString();

        return new BreakStatement
        {
            NodeKind = "BreakStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Label = label
        };
    }

    private ContinueStatement ConvertContinue(JsonElement node, SourceSpan extent, string? rawSource)
    {
        string? label = null;
        if (node.TryGetProperty("Label", out var labelElem) && labelElem.ValueKind != JsonValueKind.Null)
            label = labelElem.GetString();

        return new ContinueStatement
        {
            NodeKind = "ContinueStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Label = label
        };
    }

    private ReturnStatement ConvertReturn(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        ExpressionNode? value = null;
        if (node.TryGetProperty("Pipeline", out var pipe) && pipe.ValueKind != JsonValueKind.Null)
            value = ConvertNode(pipe, source) as ExpressionNode;

        return new ReturnStatement
        {
            NodeKind = "ReturnStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Value = value
        };
    }

    private ExitStatement ConvertExit(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        ExpressionNode? code = null;
        if (node.TryGetProperty("Pipeline", out var pipe) && pipe.ValueKind != JsonValueKind.Null)
            code = ConvertNode(pipe, source) as ExpressionNode;

        return new ExitStatement
        {
            NodeKind = "ExitStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            ExitCode = code
        };
    }

    private ThrowStatement ConvertThrow(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        ExpressionNode? expr = null;
        if (node.TryGetProperty("Pipeline", out var pipe) && pipe.ValueKind != JsonValueKind.Null)
            expr = ConvertNode(pipe, source) as ExpressionNode;

        return new ThrowStatement
        {
            NodeKind = "ThrowStatement",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Expression = expr
        };
    }

    private ExpressionNode ConvertPipeline(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var elements = node.GetProperty("PipelineElements").EnumerateArray().ToList();

        if (elements.Count == 1)
        {
            return ConvertNode(elements[0], source) as ExpressionNode ?? CreateUnknownExpression("PipelineElement", extent, rawSource);
        }

        var commands = new List<ExpressionNode>();
        foreach (var elem in elements)
        {
            if (ConvertNode(elem, source) is ExpressionNode expr)
                commands.Add(expr);
        }

        var isBackground = false;
        if (node.TryGetProperty("Background", out var bg))
            isBackground = bg.GetBoolean();

        return new PipelineExpression
        {
            NodeKind = "PipelineExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Commands = commands,
            IsBackground = isBackground
        };
    }

    private CommandExpression ConvertCommand(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var elements = node.GetProperty("CommandElements").EnumerateArray().ToList();
        if (elements.Count == 0)
        {
            return new CommandExpression
            {
                NodeKind = "CommandExpression",
                Language = LanguageName,
                Span = extent,
                RawSource = rawSource,
                CommandName = ""
            };
        }

        var firstElem = elements[0];
        var commandName = GetTextFromElement(firstElem);

        var parameters = new List<CommandParameterNode>();
        var arguments = new List<ExpressionNode>();

        foreach (var elem in elements.Skip(1))
        {
            var elemType = elem.GetProperty("Type").GetString() ?? "";
            if (elemType == "CommandParameterAst")
            {
                var paramName = elem.GetProperty("ParameterName").GetString() ?? "";
                ExpressionNode? paramValue = null;
                if (elem.TryGetProperty("Argument", out var arg) && arg.ValueKind != JsonValueKind.Null)
                    paramValue = ConvertNode(arg, source) as ExpressionNode;

                parameters.Add(new CommandParameterNode
                {
                    NodeKind = "CommandParameterNode",
                    Language = LanguageName,
                    Span = ConvertExtent(elem.GetProperty("Extent")),
                    Name = paramName,
                    Value = paramValue
                });
            }
            else
            {
                if (ConvertNode(elem, source) is ExpressionNode expr)
                    arguments.Add(expr);
            }
        }

        return new CommandExpression
        {
            NodeKind = "CommandExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            CommandName = commandName,
            Parameters = parameters,
            Arguments = arguments
        };
    }

    private ExpressionNode ConvertCommandExpression(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        if (node.TryGetProperty("Expression", out var expr))
        {
            return ConvertNode(expr, source) as ExpressionNode ?? CreateUnknownExpression("Expression", extent, rawSource);
        }
        return CreateUnknownExpression("CommandExpression", extent, rawSource);
    }

    private AssignmentExpression ConvertAssignment(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var left = ConvertNode(node.GetProperty("Left"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid assignment target");
        var right = ConvertNode(node.GetProperty("Right"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid assignment value");

        var op = AssignmentOperator.Assign;
        if (node.TryGetProperty("Operator", out var opElem))
        {
            op = opElem.GetString() switch
            {
                "Equals" => AssignmentOperator.Assign,
                "PlusEquals" => AssignmentOperator.AddAssign,
                "MinusEquals" => AssignmentOperator.SubtractAssign,
                "MultiplyEquals" => AssignmentOperator.MultiplyAssign,
                "DivideEquals" => AssignmentOperator.DivideAssign,
                "RemainderEquals" => AssignmentOperator.ModuloAssign,
                "QuestionQuestionEquals" => AssignmentOperator.CoalesceAssign,
                _ => AssignmentOperator.Assign
            };
        }

        return new AssignmentExpression
        {
            NodeKind = "AssignmentExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Target = left,
            Operator = op,
            Value = right
        };
    }

    private BinaryExpression ConvertBinary(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var left = ConvertNode(node.GetProperty("Left"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid binary left");
        var right = ConvertNode(node.GetProperty("Right"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid binary right");

        var op = BinaryOperator.Equal;
        if (node.TryGetProperty("Operator", out var opElem))
        {
            op = ConvertBinaryOperator(opElem.GetString() ?? "");
        }

        return new BinaryExpression
        {
            NodeKind = "BinaryExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Left = left,
            Operator = op,
            Right = right
        };
    }

    private static BinaryOperator ConvertBinaryOperator(string op)
    {
        return op switch
        {
            "Plus" => BinaryOperator.Add,
            "Minus" => BinaryOperator.Subtract,
            "Multiply" => BinaryOperator.Multiply,
            "Divide" => BinaryOperator.Divide,
            "Rem" => BinaryOperator.Modulo,
            "Ieq" or "Ceq" => BinaryOperator.Equal,
            "Ine" or "Cne" => BinaryOperator.NotEqual,
            "Ilt" or "Clt" => BinaryOperator.LessThan,
            "Ile" or "Cle" => BinaryOperator.LessOrEqual,
            "Igt" or "Cgt" => BinaryOperator.GreaterThan,
            "Ige" or "Cge" => BinaryOperator.GreaterOrEqual,
            "And" => BinaryOperator.And,
            "Or" => BinaryOperator.Or,
            "Band" => BinaryOperator.BitwiseAnd,
            "Bor" => BinaryOperator.BitwiseOr,
            "Bxor" => BinaryOperator.BitwiseXor,
            "Ilike" or "Clike" => BinaryOperator.Like,
            "Inotlike" or "Cnotlike" => BinaryOperator.NotLike,
            "Imatch" or "Cmatch" => BinaryOperator.Match,
            "Inotmatch" or "Cnotmatch" => BinaryOperator.NotMatch,
            "DotDot" => BinaryOperator.Range,
            "QuestionQuestion" => BinaryOperator.Coalesce,
            _ => BinaryOperator.Equal
        };
    }

    private UnaryExpression ConvertUnary(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var child = ConvertNode(node.GetProperty("Child"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid unary operand");

        var op = UnaryOperator.Not;
        var isPrefix = true;
        if (node.TryGetProperty("TokenKind", out var tkElem))
        {
            var tk = tkElem.GetString() ?? "";
            (op, isPrefix) = tk switch
            {
                "Minus" => (UnaryOperator.Negate, true),
                "Not" or "Exclaim" => (UnaryOperator.Not, true),
                "Bnot" => (UnaryOperator.BitwiseNot, true),
                "PlusPlus" => (UnaryOperator.Increment, true),
                "MinusMinus" => (UnaryOperator.Decrement, true),
                "PostfixPlusPlus" => (UnaryOperator.Increment, false),
                "PostfixMinusMinus" => (UnaryOperator.Decrement, false),
                _ => (UnaryOperator.Not, true)
            };
        }

        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Operator = op,
            Operand = child,
            IsPrefix = isPrefix
        };
    }

    private LiteralExpression ConvertConstant(JsonElement node, SourceSpan extent, string? rawSource)
    {
        object? value = null;
        var kind = LiteralKind.String;

        if (node.TryGetProperty("Value", out var valElem))
        {
            value = valElem.ValueKind switch
            {
                JsonValueKind.String => valElem.GetString(),
                JsonValueKind.Number => valElem.TryGetInt64(out var l) ? l : valElem.GetDouble(),
                JsonValueKind.True => true,
                JsonValueKind.False => false,
                JsonValueKind.Null => null,
                _ => valElem.ToString()
            };

            kind = valElem.ValueKind switch
            {
                JsonValueKind.Number => value is double ? LiteralKind.Float : LiteralKind.Integer,
                JsonValueKind.True or JsonValueKind.False => LiteralKind.Boolean,
                JsonValueKind.Null => LiteralKind.Null,
                _ => LiteralKind.String
            };
        }

        return new LiteralExpression
        {
            NodeKind = "LiteralExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Value = value,
            Kind = kind,
            RawText = rawSource ?? ""
        };
    }

    private InterpolatedStringExpression ConvertExpandableString(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var parts = new List<InterpolatedStringPart>();

        if (node.TryGetProperty("NestedExpressions", out var nested))
        {
            foreach (var n in nested.EnumerateArray())
            {
                var expr = ConvertNode(n, source) as ExpressionNode;
                if (expr != null)
                {
                    parts.Add(new InterpolatedStringPart
                    {
                        NodeKind = "InterpolatedStringPart",
                        Language = LanguageName,
                        Span = expr.Span,
                        Expression = expr
                    });
                }
            }
        }

        return new InterpolatedStringExpression
        {
            NodeKind = "InterpolatedStringExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Parts = parts
        };
    }

    private VariableExpression ConvertVariable(JsonElement node, SourceSpan extent, string? rawSource)
    {
        var name = node.GetProperty("VariablePath").GetString() ?? "";
        string? scope = null;

        if (node.TryGetProperty("IsGlobal", out var g) && g.GetBoolean()) scope = "global";
        else if (node.TryGetProperty("IsLocal", out var l) && l.GetBoolean()) scope = "local";
        else if (node.TryGetProperty("IsScript", out var s) && s.GetBoolean()) scope = "script";
        else if (node.TryGetProperty("IsPrivate", out var p) && p.GetBoolean()) scope = "private";

        var splatted = false;
        if (node.TryGetProperty("Splatted", out var sp))
            splatted = sp.GetBoolean();

        return new VariableExpression
        {
            NodeKind = "VariableExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Name = name,
            Scope = scope,
            IsSplatted = splatted
        };
    }

    private ArraySubExpression ConvertArrayExpression(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var statements = new List<StatementNode>();
        if (node.TryGetProperty("SubExpression", out var sub))
        {
            var subNode = ConvertNode(sub, source);
            if (subNode is BlockNode block)
                statements = block.Statements.ToList();
        }

        return new ArraySubExpression
        {
            NodeKind = "ArraySubExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Statements = statements
        };
    }

    private ArrayExpression ConvertArrayLiteral(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var elements = new List<ExpressionNode>();
        if (node.TryGetProperty("Elements", out var elems))
        {
            foreach (var e in elems.EnumerateArray())
            {
                if (ConvertNode(e, source) is ExpressionNode expr)
                    elements.Add(expr);
            }
        }

        return new ArrayExpression
        {
            NodeKind = "ArrayExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Elements = elements
        };
    }

    private ObjectExpression ConvertHashtable(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var properties = new List<PropertyNode>();
        if (node.TryGetProperty("KeyValuePairs", out var pairs))
        {
            foreach (var pair in pairs.EnumerateArray())
            {
                var key = ConvertNode(pair.GetProperty("Key"), source) as ExpressionNode
                    ?? throw new InvalidOperationException("Invalid hashtable key");
                var value = ConvertNode(pair.GetProperty("Value"), source) as ExpressionNode
                    ?? throw new InvalidOperationException("Invalid hashtable value");

                properties.Add(new PropertyNode
                {
                    NodeKind = "PropertyNode",
                    Language = LanguageName,
                    Span = key.Span,
                    Key = key,
                    Value = value
                });
            }
        }

        return new ObjectExpression
        {
            NodeKind = "ObjectExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Properties = properties
        };
    }

    private IndexExpression ConvertIndex(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var target = ConvertNode(node.GetProperty("Target"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid index target");
        var index = ConvertNode(node.GetProperty("Index"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid index");

        var nullSafe = false;
        if (node.TryGetProperty("NullConditional", out var nc))
            nullSafe = nc.GetBoolean();

        return new IndexExpression
        {
            NodeKind = "IndexExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Object = target,
            Index = index,
            IsNullSafe = nullSafe
        };
    }

    private MemberExpression ConvertMember(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var obj = ConvertNode(node.GetProperty("Expression"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid member object");
        var member = GetTextFromElement(node.GetProperty("Member"));

        var isStatic = false;
        if (node.TryGetProperty("Static", out var st))
            isStatic = st.GetBoolean();

        var nullSafe = false;
        if (node.TryGetProperty("NullConditional", out var nc))
            nullSafe = nc.GetBoolean();

        return new MemberExpression
        {
            NodeKind = "MemberExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Object = obj,
            Member = member,
            IsStatic = isStatic,
            IsNullSafe = nullSafe
        };
    }

    private InvokeMemberExpression ConvertInvokeMember(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var obj = ConvertNode(node.GetProperty("Expression"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid invoke member object");
        var methodName = GetTextFromElement(node.GetProperty("Member"));

        var arguments = new List<ArgumentNode>();
        if (node.TryGetProperty("Arguments", out var args))
        {
            foreach (var arg in args.EnumerateArray())
            {
                if (ConvertNode(arg, source) is ExpressionNode expr)
                {
                    arguments.Add(new ArgumentNode
                    {
                        NodeKind = "ArgumentNode",
                        Language = LanguageName,
                        Span = expr.Span,
                        Value = expr
                    });
                }
            }
        }

        var isStatic = false;
        if (node.TryGetProperty("Static", out var st))
            isStatic = st.GetBoolean();

        var nullSafe = false;
        if (node.TryGetProperty("NullConditional", out var nc))
            nullSafe = nc.GetBoolean();

        return new InvokeMemberExpression
        {
            NodeKind = "InvokeMemberExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Object = obj,
            MethodName = methodName,
            Arguments = arguments,
            IsStatic = isStatic,
            IsNullSafe = nullSafe
        };
    }

    private ScriptBlockExpression ConvertScriptBlockExpr(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        BlockNode? begin = null, process = null, end = null;

        if (node.TryGetProperty("ScriptBlock", out var sb))
        {
            if (sb.TryGetProperty("BeginBlock", out var b) && b.ValueKind != JsonValueKind.Null)
                begin = ConvertNode(b, source) as BlockNode;
            if (sb.TryGetProperty("ProcessBlock", out var p) && p.ValueKind != JsonValueKind.Null)
                process = ConvertNode(p, source) as BlockNode;
            if (sb.TryGetProperty("EndBlock", out var e) && e.ValueKind != JsonValueKind.Null)
                end = ConvertNode(e, source) as BlockNode;
        }

        return new ScriptBlockExpression
        {
            NodeKind = "ScriptBlockExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Begin = begin,
            Process = process,
            End = end
        };
    }

    private ParenthesizedExpression ConvertParen(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var inner = ConvertNode(node.GetProperty("Pipeline"), source) as ExpressionNode
            ?? throw new InvalidOperationException("Invalid paren expression");

        return new ParenthesizedExpression
        {
            NodeKind = "ParenthesizedExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Expression = inner
        };
    }

    private SubExpression ConvertSubExpr(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var statements = new List<StatementNode>();
        if (node.TryGetProperty("SubExpression", out var sub))
        {
            var subNode = ConvertNode(sub, source);
            if (subNode is BlockNode block)
                statements = block.Statements.ToList();
        }

        return new SubExpression
        {
            NodeKind = "SubExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Statements = statements
        };
    }

    private BlockNode ConvertStatementBlock(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var statements = new List<StatementNode>();
        if (node.TryGetProperty("Statements", out var stmts))
        {
            foreach (var stmt in stmts.EnumerateArray())
            {
                var stmtNode = ConvertNode(stmt, source);
                if (stmtNode is StatementNode s)
                    statements.Add(s);
                else if (stmtNode is ExpressionNode expr)
                    statements.Add(new ExpressionStatement
                    {
                        NodeKind = "ExpressionStatement",
                        Language = LanguageName,
                        Span = stmtNode.Span,
                        Expression = expr
                    });
            }
        }

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Statements = statements
        };
    }

    private BlockNode ConvertNamedBlock(JsonElement node, string source, SourceSpan extent, string? rawSource)
    {
        var statements = new List<StatementNode>();
        if (node.TryGetProperty("Statements", out var stmts))
        {
            foreach (var stmt in stmts.EnumerateArray())
            {
                var stmtNode = ConvertNode(stmt, source);
                if (stmtNode is StatementNode s)
                    statements.Add(s);
                else if (stmtNode is ExpressionNode expr)
                    statements.Add(new ExpressionStatement
                    {
                        NodeKind = "ExpressionStatement",
                        Language = LanguageName,
                        Span = stmtNode.Span,
                        Expression = expr
                    });
            }
        }

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            Statements = statements
        };
    }

    private UnknownNode CreateUnknown(string type, SourceSpan extent, string? rawSource)
    {
        return new UnknownNode
        {
            NodeKind = "UnknownNode",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            NativeNodeType = type,
            Roles = []
        };
    }

    private UnknownExpression CreateUnknownExpression(string type, SourceSpan extent, string? rawSource)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = extent,
            RawSource = rawSource,
            NativeNodeType = type,
            Roles = []
        };
    }

    private SourceSpan ConvertExtent(JsonElement extent)
    {
        return new SourceSpan(
            extent.GetProperty("StartLine").GetInt32(),
            extent.GetProperty("StartColumn").GetInt32(),
            extent.GetProperty("EndLine").GetInt32(),
            extent.GetProperty("EndColumn").GetInt32(),
            extent.GetProperty("StartOffset").GetInt32(),
            extent.GetProperty("EndOffset").GetInt32()
        );
    }

    private static string? GetRawSource(JsonElement node, string source)
    {
        if (!node.TryGetProperty("Extent", out var extent))
            return null;

        if (extent.TryGetProperty("Text", out var text))
            return text.GetString();

        var start = extent.GetProperty("StartOffset").GetInt32();
        var end = extent.GetProperty("EndOffset").GetInt32();

        if (start >= 0 && end <= source.Length)
            return source[start..end];

        return null;
    }

    private static string GetTextFromElement(JsonElement elem)
    {
        var type = elem.GetProperty("Type").GetString() ?? "";

        if (type is "StringConstantExpressionAst" or "ConstantExpressionAst")
        {
            if (elem.TryGetProperty("Value", out var val))
                return val.ToString();
        }

        if (elem.TryGetProperty("Extent", out var extent) && extent.TryGetProperty("Text", out var text))
            return text.GetString() ?? "";

        return "";
    }
}
