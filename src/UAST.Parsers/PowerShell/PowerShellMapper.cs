using System.Management.Automation.Language;
using UAST.Core.Interfaces;
using UAST.Core.Schema;
using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Parsers.PowerShell;

/// <summary>
/// Maps PowerShell AST (via System.Management.Automation.Language.Parser) to UAST.
/// Uses the official PowerShell parser for native AST generation.
/// Embedded in CLI for single-package distribution.
/// </summary>
public class PowerShellMapper : ILanguageMapper
{
    private readonly List<UAST.Core.Interfaces.ParseError> _errors = [];

    public string LanguageName => "PowerShell";

    public IReadOnlyList<string> FileExtensions => [".ps1", ".psm1", ".psd1"];

    public UastNode Parse(string source, string? filePath = null)
    {
        _errors.Clear();

        var psErrors = new List<System.Management.Automation.Language.ParseError>();
        var ast = Parser.ParseInput(source, out var tokens, out var errors);

        if (errors != null && errors.Length > 0)
        {
            foreach (var error in errors)
            {
                _errors.Add(new UAST.Core.Interfaces.ParseError(
                    error.Message,
                    ConvertExtent(error.Extent),
                    error.ErrorId,
                    UAST.Core.Interfaces.ParseErrorSeverity.Error
                ));
            }
        }

        var result = ConvertScriptBlock(ast);
        result.SetParentReferences();
        return result;
    }

    public UastNode ParseFile(string filePath)
    {
        var source = File.ReadAllText(filePath);
        return Parse(source, filePath);
    }

    public IReadOnlyList<UAST.Core.Interfaces.ParseError> GetErrors() => _errors;

    #region Script Block Conversion

    private BlockNode ConvertScriptBlock(ScriptBlockAst ast)
    {
        var statements = new List<StatementNode>();

        // Handle param block
        if (ast.ParamBlock != null)
        {
            statements.Add(ConvertParamBlock(ast.ParamBlock));
        }

        // Handle named blocks
        if (ast.BeginBlock != null)
        {
            statements.Add(ConvertNamedBlock(ast.BeginBlock, "begin"));
        }
        if (ast.ProcessBlock != null)
        {
            statements.Add(ConvertNamedBlock(ast.ProcessBlock, "process"));
        }
        if (ast.EndBlock != null)
        {
            // EndBlock is the main script body when not explicitly named
            if (ast.EndBlock.Statements.Count > 0 || ast.EndBlock.Traps?.Count > 0)
            {
                if (ast.EndBlock.Unnamed)
                {
                    // Unnamed end block - add statements directly
                    foreach (var stmt in ast.EndBlock.Statements)
                    {
                        statements.Add(ConvertStatement(stmt));
                    }
                }
                else
                {
                    statements.Add(ConvertNamedBlock(ast.EndBlock, "end"));
                }
            }
        }
        if (ast.DynamicParamBlock != null)
        {
            statements.Add(ConvertNamedBlock(ast.DynamicParamBlock, "dynamicparam"));
        }
        if (ast.CleanBlock != null)
        {
            statements.Add(ConvertNamedBlock(ast.CleanBlock, "clean"));
        }

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertExtent(ast.Extent),
            RawSource = ast.Extent.Text,
            Statements = statements,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ScriptBlockAst"
            }
        };
    }

    private StatementNode ConvertParamBlock(ParamBlockAst paramBlock)
    {
        var parameters = paramBlock.Parameters.Select(ConvertParameter).ToList();

        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(paramBlock.Extent),
            RawSource = paramBlock.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(paramBlock.Extent),
                RawSource = paramBlock.Extent.Text,
                NativeNodeType = "ParamBlockAst",
                Roles = ["parameter-block"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ParamBlockAst",
                ["Parameters"] = parameters.Select(p => p.Name).ToList()
            }
        };
    }

    private BlockNode ConvertNamedBlock(NamedBlockAst namedBlock, string blockType)
    {
        var statements = namedBlock.Statements.Select(ConvertStatement).ToList();

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertExtent(namedBlock.Extent),
            RawSource = namedBlock.Extent.Text,
            Statements = statements,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "NamedBlockAst",
                ["BlockType"] = blockType
            }
        };
    }

    #endregion

    #region Statement Conversion

    private StatementNode ConvertStatement(StatementAst stmt)
    {
        return stmt switch
        {
            FunctionDefinitionAst func => ConvertFunctionDefinition(func),
            IfStatementAst if_ => ConvertIfStatement(if_),
            ForStatementAst for_ => ConvertForStatement(for_),
            ForEachStatementAst foreach_ => ConvertForEachStatement(foreach_),
            WhileStatementAst while_ => ConvertWhileStatement(while_),
            DoWhileStatementAst doWhile => ConvertDoWhileStatement(doWhile),
            DoUntilStatementAst doUntil => ConvertDoUntilStatement(doUntil),
            SwitchStatementAst switch_ => ConvertSwitchStatement(switch_),
            TryStatementAst try_ => ConvertTryStatement(try_),
            TrapStatementAst trap => ConvertTrapStatement(trap),
            ReturnStatementAst return_ => ConvertReturnStatement(return_),
            BreakStatementAst break_ => ConvertBreakStatement(break_),
            ContinueStatementAst continue_ => ConvertContinueStatement(continue_),
            ThrowStatementAst throw_ => ConvertThrowStatement(throw_),
            ExitStatementAst exit => ConvertExitStatement(exit),
            PipelineAst pipeline => ConvertPipeline(pipeline),
            AssignmentStatementAst assign => ConvertAssignmentStatement(assign),
            TypeDefinitionAst typeDef => ConvertTypeDefinition(typeDef),
            UsingStatementAst using_ => ConvertUsingStatement(using_),
            DataStatementAst data => ConvertDataStatement(data),
            BlockStatementAst block => ConvertBlockStatement(block),
            ConfigurationDefinitionAst config => ConvertConfigurationDefinition(config),
            DynamicKeywordStatementAst dynamic => ConvertDynamicKeywordStatement(dynamic),
            _ => ConvertUnknownStatement(stmt)
        };
    }

    private FunctionDeclaration ConvertFunctionDefinition(FunctionDefinitionAst func)
    {
        var parameters = new List<ParameterNode>();

        // Get parameters from function definition
        if (func.Parameters != null)
        {
            parameters.AddRange(func.Parameters.Select(ConvertParameter));
        }

        // Also check param block in body
        if (func.Body.ParamBlock != null)
        {
            parameters.AddRange(func.Body.ParamBlock.Parameters.Select(ConvertParameter));
        }

        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertExtent(func.Extent),
            RawSource = func.Extent.Text,
            Name = func.Name,
            Parameters = parameters,
            Body = ConvertScriptBlock(func.Body),
            IsAsync = false,
            IsGenerator = false,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "FunctionDefinitionAst",
                ["IsFilter"] = func.IsFilter,
                ["IsWorkflow"] = func.IsWorkflow
            }
        };
    }

    private IfStatement ConvertIfStatement(IfStatementAst if_)
    {
        // PowerShell if statements have multiple clauses
        var firstClause = if_.Clauses.FirstOrDefault();

        var condition = firstClause != null
            ? ConvertExpression(firstClause.Item1)
            : new LiteralExpression
            {
                NodeKind = "LiteralExpression",
                Language = LanguageName,
                Span = SourceSpan.Empty,
                Value = true,
                Kind = LiteralKind.Boolean,
                RawText = "true"
            };

        var thenBranch = firstClause != null
            ? ConvertStatementBlock(firstClause.Item2)
            : CreateEmptyBlock();

        // Handle elseif clauses and else
        StatementNode? elseBranch = null;
        if (if_.Clauses.Count > 1)
        {
            // Build nested if for remaining clauses
            elseBranch = BuildNestedIf(if_.Clauses.Skip(1).ToList(), if_.ElseClause);
        }
        else if (if_.ElseClause != null)
        {
            elseBranch = ConvertStatementBlock(if_.ElseClause);
        }

        return new IfStatement
        {
            NodeKind = "IfStatement",
            Language = LanguageName,
            Span = ConvertExtent(if_.Extent),
            RawSource = if_.Extent.Text,
            Condition = condition,
            ThenBranch = thenBranch,
            ElseBranch = elseBranch,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "IfStatementAst"
            }
        };
    }

    private StatementNode BuildNestedIf(List<Tuple<PipelineBaseAst, StatementBlockAst>> clauses, StatementBlockAst? elseClause)
    {
        if (clauses.Count == 0)
        {
            return elseClause != null ? ConvertStatementBlock(elseClause) : CreateEmptyBlock();
        }

        var firstClause = clauses[0];
        return new IfStatement
        {
            NodeKind = "IfStatement",
            Language = LanguageName,
            Span = ConvertExtent(firstClause.Item1.Extent),
            RawSource = firstClause.Item1.Extent.Text,
            Condition = ConvertExpression(firstClause.Item1),
            ThenBranch = ConvertStatementBlock(firstClause.Item2),
            ElseBranch = BuildNestedIf(clauses.Skip(1).ToList(), elseClause),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "IfStatementAst",
                ["IsElseIf"] = true
            }
        };
    }

    private ForStatement ConvertForStatement(ForStatementAst for_)
    {
        return new ForStatement
        {
            NodeKind = "ForStatement",
            Language = LanguageName,
            Span = ConvertExtent(for_.Extent),
            RawSource = for_.Extent.Text,
            Initializer = for_.Initializer != null ? ConvertPipeline(for_.Initializer) : null,
            Condition = for_.Condition != null ? ConvertExpression(for_.Condition) : null,
            Increment = for_.Iterator != null ? ConvertExpression(for_.Iterator) : null,
            Body = ConvertStatementBlock(for_.Body),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ForStatementAst"
            }
        };
    }

    private ForEachStatement ConvertForEachStatement(ForEachStatementAst foreach_)
    {
        return new ForEachStatement
        {
            NodeKind = "ForEachStatement",
            Language = LanguageName,
            Span = ConvertExtent(foreach_.Extent),
            RawSource = foreach_.Extent.Text,
            VariableName = foreach_.Variable.VariablePath.UserPath,
            Iterable = ConvertExpression(foreach_.Condition),
            Body = ConvertStatementBlock(foreach_.Body),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ForEachStatementAst",
                ["Flags"] = foreach_.Flags.ToString()
            }
        };
    }

    private WhileStatement ConvertWhileStatement(WhileStatementAst while_)
    {
        return new WhileStatement
        {
            NodeKind = "WhileStatement",
            Language = LanguageName,
            Span = ConvertExtent(while_.Extent),
            RawSource = while_.Extent.Text,
            Condition = ConvertExpression(while_.Condition),
            Body = ConvertStatementBlock(while_.Body),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "WhileStatementAst"
            }
        };
    }

    private DoWhileStatement ConvertDoWhileStatement(DoWhileStatementAst doWhile)
    {
        return new DoWhileStatement
        {
            NodeKind = "DoWhileStatement",
            Language = LanguageName,
            Span = ConvertExtent(doWhile.Extent),
            RawSource = doWhile.Extent.Text,
            Body = ConvertStatementBlock(doWhile.Body),
            Condition = ConvertExpression(doWhile.Condition),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "DoWhileStatementAst"
            }
        };
    }

    private DoWhileStatement ConvertDoUntilStatement(DoUntilStatementAst doUntil)
    {
        // DoUntil is DoWhile with negated condition
        return new DoWhileStatement
        {
            NodeKind = "DoWhileStatement",
            Language = LanguageName,
            Span = ConvertExtent(doUntil.Extent),
            RawSource = doUntil.Extent.Text,
            Body = ConvertStatementBlock(doUntil.Body),
            Condition = new UnaryExpression
            {
                NodeKind = "UnaryExpression",
                Language = LanguageName,
                Span = ConvertExtent(doUntil.Condition.Extent),
                Operator = UnaryOperator.Not,
                Operand = ConvertExpression(doUntil.Condition),
                IsPrefix = true
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "DoUntilStatementAst"
            }
        };
    }

    private SwitchStatement ConvertSwitchStatement(SwitchStatementAst switch_)
    {
        var cases = new List<SwitchCase>();

        foreach (var clause in switch_.Clauses)
        {
            cases.Add(new SwitchCase
            {
                NodeKind = "SwitchCase",
                Language = LanguageName,
                Span = ConvertExtent(clause.Item1.Extent),
                RawSource = clause.Item1.Extent.Text,
                Patterns = [ConvertExpression(clause.Item1)],
                Body = ConvertStatementBlock(clause.Item2).Statements.ToList()
            });
        }

        if (switch_.Default != null)
        {
            cases.Add(new SwitchCase
            {
                NodeKind = "SwitchCase",
                Language = LanguageName,
                Span = ConvertExtent(switch_.Default.Extent),
                RawSource = switch_.Default.Extent.Text,
                Patterns = [], // Empty patterns = default
                Body = ConvertStatementBlock(switch_.Default).Statements.ToList()
            });
        }

        return new SwitchStatement
        {
            NodeKind = "SwitchStatement",
            Language = LanguageName,
            Span = ConvertExtent(switch_.Extent),
            RawSource = switch_.Extent.Text,
            Subject = ConvertExpression(switch_.Condition),
            Cases = cases,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "SwitchStatementAst",
                ["Flags"] = switch_.Flags.ToString()
            }
        };
    }

    private TryStatement ConvertTryStatement(TryStatementAst try_)
    {
        return new TryStatement
        {
            NodeKind = "TryStatement",
            Language = LanguageName,
            Span = ConvertExtent(try_.Extent),
            RawSource = try_.Extent.Text,
            TryBlock = ConvertStatementBlock(try_.Body),
            CatchClauses = try_.CatchClauses.Select(ConvertCatchClause).ToList(),
            FinallyBlock = try_.Finally != null ? ConvertStatementBlock(try_.Finally) : null,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "TryStatementAst"
            }
        };
    }

    private CatchClause ConvertCatchClause(CatchClauseAst catch_)
    {
        return new CatchClause
        {
            NodeKind = "CatchClause",
            Language = LanguageName,
            Span = ConvertExtent(catch_.Extent),
            RawSource = catch_.Extent.Text,
            ExceptionTypes = catch_.CatchTypes.Select(t => (TypeReference)new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertExtent(t.Extent),
                Name = t.TypeName.FullName
            }).ToList(),
            Body = ConvertStatementBlock(catch_.Body),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "CatchClauseAst",
                ["IsCatchAll"] = catch_.IsCatchAll
            }
        };
    }

    private StatementNode ConvertTrapStatement(TrapStatementAst trap)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(trap.Extent),
            RawSource = trap.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(trap.Extent),
                RawSource = trap.Extent.Text,
                NativeNodeType = "TrapStatementAst",
                Roles = ["trap", "exception-handler"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "TrapStatementAst"
            }
        };
    }

    private ReturnStatement ConvertReturnStatement(ReturnStatementAst return_)
    {
        return new ReturnStatement
        {
            NodeKind = "ReturnStatement",
            Language = LanguageName,
            Span = ConvertExtent(return_.Extent),
            RawSource = return_.Extent.Text,
            Value = return_.Pipeline != null ? ConvertExpression(return_.Pipeline) : null,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ReturnStatementAst"
            }
        };
    }

    private BreakStatement ConvertBreakStatement(BreakStatementAst break_)
    {
        return new BreakStatement
        {
            NodeKind = "BreakStatement",
            Language = LanguageName,
            Span = ConvertExtent(break_.Extent),
            RawSource = break_.Extent.Text,
            Label = break_.Label?.ToString(),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "BreakStatementAst"
            }
        };
    }

    private ContinueStatement ConvertContinueStatement(ContinueStatementAst continue_)
    {
        return new ContinueStatement
        {
            NodeKind = "ContinueStatement",
            Language = LanguageName,
            Span = ConvertExtent(continue_.Extent),
            RawSource = continue_.Extent.Text,
            Label = continue_.Label?.ToString(),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ContinueStatementAst"
            }
        };
    }

    private ThrowStatement ConvertThrowStatement(ThrowStatementAst throw_)
    {
        return new ThrowStatement
        {
            NodeKind = "ThrowStatement",
            Language = LanguageName,
            Span = ConvertExtent(throw_.Extent),
            RawSource = throw_.Extent.Text,
            Expression = throw_.Pipeline != null ? ConvertExpression(throw_.Pipeline) : null,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ThrowStatementAst"
            }
        };
    }

    private StatementNode ConvertExitStatement(ExitStatementAst exit)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(exit.Extent),
            RawSource = exit.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(exit.Extent),
                RawSource = exit.Extent.Text,
                NativeNodeType = "ExitStatementAst",
                Roles = ["exit", "control-flow"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ExitStatementAst"
            }
        };
    }

    private ExpressionStatement ConvertPipeline(PipelineBaseAst pipeline)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(pipeline.Extent),
            RawSource = pipeline.Extent.Text,
            Expression = ConvertExpression(pipeline),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = pipeline.GetType().Name
            }
        };
    }

    private ExpressionStatement ConvertAssignmentStatement(AssignmentStatementAst assign)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(assign.Extent),
            RawSource = assign.Extent.Text,
            Expression = new AssignmentExpression
            {
                NodeKind = "AssignmentExpression",
                Language = LanguageName,
                Span = ConvertExtent(assign.Extent),
                RawSource = assign.Extent.Text,
                Target = ConvertExpression(assign.Left),
                Operator = ConvertAssignmentOperator(assign.Operator),
                Value = ConvertExpression(assign.Right),
                Extensions = new Dictionary<string, object>
                {
                    ["NativeNodeType"] = "AssignmentStatementAst"
                }
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "AssignmentStatementAst"
            }
        };
    }

    private TypeDeclaration ConvertTypeDefinition(TypeDefinitionAst typeDef)
    {
        var members = new List<DeclarationNode>();

        foreach (var member in typeDef.Members)
        {
            var converted = ConvertTypeMember(member);
            if (converted != null)
                members.Add(converted);
        }

        return new TypeDeclaration
        {
            NodeKind = "TypeDeclaration",
            Language = LanguageName,
            Span = ConvertExtent(typeDef.Extent),
            RawSource = typeDef.Extent.Text,
            Name = typeDef.Name,
            Kind = typeDef.IsClass ? TypeDeclarationKind.Class :
                   typeDef.IsEnum ? TypeDeclarationKind.Enum :
                   typeDef.IsInterface ? TypeDeclarationKind.Interface : TypeDeclarationKind.Class,
            Members = members,
            BaseTypes = typeDef.BaseTypes?.Select(t => (TypeReference)new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertExtent(t.Extent),
                Name = t.TypeName.FullName
            }).ToList() ?? [],
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "TypeDefinitionAst",
                ["TypeAttributes"] = typeDef.TypeAttributes.ToString()
            }
        };
    }

    private DeclarationNode? ConvertTypeMember(MemberAst member)
    {
        return member switch
        {
            PropertyMemberAst prop => new PropertyDeclaration
            {
                NodeKind = "PropertyDeclaration",
                Language = LanguageName,
                Span = ConvertExtent(prop.Extent),
                RawSource = prop.Extent.Text,
                Name = prop.Name,
                Type = prop.PropertyType != null
                    ? new NamedTypeReference
                    {
                        NodeKind = "NamedTypeReference",
                        Language = LanguageName,
                        Span = ConvertExtent(prop.PropertyType.Extent),
                        Name = prop.PropertyType.TypeName.FullName
                    }
                    : null,
                Initializer = prop.InitialValue != null ? ConvertExpression(prop.InitialValue) : null,
                Extensions = new Dictionary<string, object>
                {
                    ["NativeNodeType"] = "PropertyMemberAst",
                    ["Attributes"] = prop.Attributes.ToString()
                }
            },
            FunctionMemberAst func => new FunctionDeclaration
            {
                NodeKind = "FunctionDeclaration",
                Language = LanguageName,
                Span = ConvertExtent(func.Extent),
                RawSource = func.Extent.Text,
                Name = func.Name,
                Parameters = func.Parameters.Select(ConvertParameter).ToList(),
                Body = ConvertScriptBlock(func.Body),
                ReturnType = func.ReturnType != null
                    ? new NamedTypeReference
                    {
                        NodeKind = "NamedTypeReference",
                        Language = LanguageName,
                        Span = ConvertExtent(func.ReturnType.Extent),
                        Name = func.ReturnType.TypeName.FullName
                    }
                    : null,
                Extensions = new Dictionary<string, object>
                {
                    ["NativeNodeType"] = "FunctionMemberAst",
                    ["MethodAttributes"] = func.MethodAttributes.ToString(),
                    ["IsConstructor"] = func.IsConstructor
                }
            },
            _ => null
        };
    }

    private StatementNode ConvertUsingStatement(UsingStatementAst using_)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(using_.Extent),
            RawSource = using_.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(using_.Extent),
                RawSource = using_.Extent.Text,
                NativeNodeType = "UsingStatementAst",
                Roles = ["import", "using"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "UsingStatementAst",
                ["UsingStatementKind"] = using_.UsingStatementKind.ToString()
            }
        };
    }

    private StatementNode ConvertDataStatement(DataStatementAst data)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(data.Extent),
            RawSource = data.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(data.Extent),
                RawSource = data.Extent.Text,
                NativeNodeType = "DataStatementAst",
                Roles = ["data"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "DataStatementAst"
            }
        };
    }

    private BlockNode ConvertBlockStatement(BlockStatementAst block)
    {
        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertExtent(block.Extent),
            RawSource = block.Extent.Text,
            Statements = block.Body.Statements.Select(ConvertStatement).ToList(),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "BlockStatementAst",
                ["Kind"] = block.Kind.Text
            }
        };
    }

    private StatementNode ConvertConfigurationDefinition(ConfigurationDefinitionAst config)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(config.Extent),
            RawSource = config.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(config.Extent),
                RawSource = config.Extent.Text,
                NativeNodeType = "ConfigurationDefinitionAst",
                Roles = ["configuration", "dsc"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ConfigurationDefinitionAst"
            }
        };
    }

    private StatementNode ConvertDynamicKeywordStatement(DynamicKeywordStatementAst dynamic)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(dynamic.Extent),
            RawSource = dynamic.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(dynamic.Extent),
                RawSource = dynamic.Extent.Text,
                NativeNodeType = "DynamicKeywordStatementAst",
                Roles = ["dynamic-keyword"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "DynamicKeywordStatementAst"
            }
        };
    }

    private StatementNode ConvertUnknownStatement(StatementAst stmt)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertExtent(stmt.Extent),
            RawSource = stmt.Extent.Text,
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertExtent(stmt.Extent),
                RawSource = stmt.Extent.Text,
                NativeNodeType = stmt.GetType().Name,
                Roles = ["statement"]
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = stmt.GetType().Name
            }
        };
    }

    private BlockNode ConvertStatementBlock(StatementBlockAst block)
    {
        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertExtent(block.Extent),
            RawSource = block.Extent.Text,
            Statements = block.Statements.Select(ConvertStatement).ToList(),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "StatementBlockAst"
            }
        };
    }

    #endregion

    #region Expression Conversion

    private ExpressionNode ConvertExpression(Ast ast)
    {
        // Note: More specific subclasses must come before their parent classes
        return ast switch
        {
            PipelineAst pipeline => ConvertPipelineExpression(pipeline),
            CommandAst cmd => ConvertCommandExpression(cmd),
            CommandExpressionAst cmdExpr => ConvertExpression(cmdExpr.Expression),
            BinaryExpressionAst bin => ConvertBinaryExpression(bin),
            UnaryExpressionAst unary => ConvertUnaryExpression(unary),
            VariableExpressionAst var => ConvertVariableExpression(var),
            // StringConstantExpressionAst must come before ConstantExpressionAst (inheritance)
            StringConstantExpressionAst str => ConvertStringConstantExpression(str),
            ConstantExpressionAst constant => ConvertConstantExpression(constant),
            ExpandableStringExpressionAst expandable => ConvertExpandableStringExpression(expandable),
            // InvokeMemberExpressionAst must come before MemberExpressionAst (inheritance)
            InvokeMemberExpressionAst invoke => ConvertInvokeMemberExpression(invoke),
            MemberExpressionAst member => ConvertMemberExpression(member),
            IndexExpressionAst index => ConvertIndexExpression(index),
            ArrayExpressionAst array => ConvertArrayExpression(array),
            ArrayLiteralAst arrayLit => ConvertArrayLiteral(arrayLit),
            HashtableAst hash => ConvertHashtable(hash),
            ParenExpressionAst paren => ConvertParenExpression(paren),
            SubExpressionAst sub => ConvertSubExpression(sub),
            ScriptBlockExpressionAst scriptBlock => ConvertScriptBlockExpression(scriptBlock),
            TypeExpressionAst type => ConvertTypeExpression(type),
            ConvertExpressionAst convert => ConvertConvertExpression(convert),
            TernaryExpressionAst ternary => ConvertTernaryExpression(ternary),
            AttributedExpressionAst attributed => ConvertExpression(attributed.Child),
            UsingExpressionAst usingExpr => ConvertUsingExpression(usingExpr),
            _ => ConvertUnknownExpression(ast)
        };
    }

    private ExpressionNode ConvertPipelineExpression(PipelineAst pipeline)
    {
        if (pipeline.PipelineElements.Count == 1)
        {
            return ConvertPipelineElement(pipeline.PipelineElements[0]);
        }

        // Multiple pipeline elements - create a pipeline expression
        var commands = pipeline.PipelineElements.Select(ConvertPipelineElement).ToList();

        return new PipelineExpression
        {
            NodeKind = "PipelineExpression",
            Language = LanguageName,
            Span = ConvertExtent(pipeline.Extent),
            RawSource = pipeline.Extent.Text,
            Commands = commands,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "PipelineAst"
            }
        };
    }

    private ExpressionNode ConvertPipelineElement(CommandBaseAst element)
    {
        return element switch
        {
            CommandAst cmd => ConvertCommandExpression(cmd),
            CommandExpressionAst cmdExpr => ConvertExpression(cmdExpr.Expression),
            _ => ConvertUnknownExpression(element)
        };
    }

    private CommandExpression ConvertCommandExpression(CommandAst cmd)
    {
        var commandName = cmd.GetCommandName() ?? "";
        var parameters = new List<CommandParameterNode>();
        var arguments = new List<ExpressionNode>();

        foreach (var element in cmd.CommandElements.Skip(1)) // Skip the command name
        {
            if (element is CommandParameterAst param)
            {
                parameters.Add(new CommandParameterNode
                {
                    NodeKind = "CommandParameterNode",
                    Language = LanguageName,
                    Span = ConvertExtent(param.Extent),
                    RawSource = param.Extent.Text,
                    Name = param.ParameterName,
                    Value = param.Argument != null
                        ? ConvertExpression(param.Argument)
                        : null,
                    IsColonSeparated = param.Extent.Text.Contains(':')
                });
            }
            else if (element is ExpressionAst expr)
            {
                arguments.Add(ConvertExpression(expr));
            }
        }

        return new CommandExpression
        {
            NodeKind = "CommandExpression",
            Language = LanguageName,
            Span = ConvertExtent(cmd.Extent),
            RawSource = cmd.Extent.Text,
            CommandName = commandName,
            Parameters = parameters,
            Arguments = arguments,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "CommandAst"
            }
        };
    }

    private BinaryExpression ConvertBinaryExpression(BinaryExpressionAst bin)
    {
        return new BinaryExpression
        {
            NodeKind = "BinaryExpression",
            Language = LanguageName,
            Span = ConvertExtent(bin.Extent),
            RawSource = bin.Extent.Text,
            Left = ConvertExpression(bin.Left),
            Operator = ConvertBinaryOperator(bin.Operator),
            Right = ConvertExpression(bin.Right),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "BinaryExpressionAst",
                ["OperatorToken"] = bin.Operator.ToString()
            }
        };
    }

    private UnaryExpression ConvertUnaryExpression(UnaryExpressionAst unary)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertExtent(unary.Extent),
            RawSource = unary.Extent.Text,
            Operator = ConvertUnaryOperator(unary.TokenKind),
            Operand = ConvertExpression(unary.Child),
            IsPrefix = IsUnaryPrefix(unary.TokenKind),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "UnaryExpressionAst",
                ["TokenKind"] = unary.TokenKind.ToString()
            }
        };
    }

    private ExpressionNode ConvertVariableExpression(VariableExpressionAst var)
    {
        // Handle special PowerShell constants $true, $false, $null as LiteralExpression
        // for cross-language semantic equivalence
        var name = var.VariablePath.UserPath.ToLowerInvariant();
        if (name == "true")
        {
            return new LiteralExpression
            {
                NodeKind = "LiteralExpression",
                Language = LanguageName,
                Span = ConvertExtent(var.Extent),
                RawSource = var.Extent.Text,
                Value = true,
                Kind = LiteralKind.Boolean,
                RawText = var.Extent.Text,
                Extensions = new Dictionary<string, object>
                {
                    ["NativeNodeType"] = "VariableExpressionAst",
                    ["IsBuiltInConstant"] = true
                }
            };
        }
        else if (name == "false")
        {
            return new LiteralExpression
            {
                NodeKind = "LiteralExpression",
                Language = LanguageName,
                Span = ConvertExtent(var.Extent),
                RawSource = var.Extent.Text,
                Value = false,
                Kind = LiteralKind.Boolean,
                RawText = var.Extent.Text,
                Extensions = new Dictionary<string, object>
                {
                    ["NativeNodeType"] = "VariableExpressionAst",
                    ["IsBuiltInConstant"] = true
                }
            };
        }
        else if (name == "null")
        {
            return new LiteralExpression
            {
                NodeKind = "LiteralExpression",
                Language = LanguageName,
                Span = ConvertExtent(var.Extent),
                RawSource = var.Extent.Text,
                Value = null,
                Kind = LiteralKind.Null,
                RawText = var.Extent.Text,
                Extensions = new Dictionary<string, object>
                {
                    ["NativeNodeType"] = "VariableExpressionAst",
                    ["IsBuiltInConstant"] = true
                }
            };
        }

        return new VariableExpression
        {
            NodeKind = "VariableExpression",
            Language = LanguageName,
            Span = ConvertExtent(var.Extent),
            RawSource = var.Extent.Text,
            Name = var.VariablePath.UserPath,
            Scope = var.VariablePath.IsGlobal ? "global" :
                   var.VariablePath.IsLocal ? "local" :
                   var.VariablePath.IsScript ? "script" :
                   var.VariablePath.IsPrivate ? "private" : null,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "VariableExpressionAst",
                ["Splatted"] = var.Splatted
            }
        };
    }

    private LiteralExpression ConvertConstantExpression(ConstantExpressionAst constant)
    {
        return new LiteralExpression
        {
            NodeKind = "LiteralExpression",
            Language = LanguageName,
            Span = ConvertExtent(constant.Extent),
            RawSource = constant.Extent.Text,
            Value = constant.Value,
            Kind = InferLiteralKind(constant.Value),
            RawText = constant.Extent.Text,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ConstantExpressionAst"
            }
        };
    }

    private LiteralExpression ConvertStringConstantExpression(StringConstantExpressionAst str)
    {
        return new LiteralExpression
        {
            NodeKind = "LiteralExpression",
            Language = LanguageName,
            Span = ConvertExtent(str.Extent),
            RawSource = str.Extent.Text,
            Value = str.Value,
            Kind = LiteralKind.String,
            RawText = str.Extent.Text,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "StringConstantExpressionAst",
                ["StringConstantType"] = str.StringConstantType.ToString()
            }
        };
    }

    private InterpolatedStringExpression ConvertExpandableStringExpression(ExpandableStringExpressionAst expandable)
    {
        var parts = new List<InterpolatedStringPart>();

        foreach (var nested in expandable.NestedExpressions)
        {
            parts.Add(new InterpolatedStringPart
            {
                NodeKind = "InterpolatedStringPart",
                Language = LanguageName,
                Span = ConvertExtent(nested.Extent),
                Expression = ConvertExpression(nested)
            });
        }

        return new InterpolatedStringExpression
        {
            NodeKind = "InterpolatedStringExpression",
            Language = LanguageName,
            Span = ConvertExtent(expandable.Extent),
            RawSource = expandable.Extent.Text,
            Parts = parts,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ExpandableStringExpressionAst",
                ["StringConstantType"] = expandable.StringConstantType.ToString()
            }
        };
    }

    private MemberExpression ConvertMemberExpression(MemberExpressionAst member)
    {
        return new MemberExpression
        {
            NodeKind = "MemberExpression",
            Language = LanguageName,
            Span = ConvertExtent(member.Extent),
            RawSource = member.Extent.Text,
            Object = ConvertExpression(member.Expression),
            Member = member.Member.ToString(),
            IsStatic = member.Static,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "MemberExpressionAst",
                ["NullConditional"] = member.NullConditional
            }
        };
    }

    private InvokeMemberExpression ConvertInvokeMemberExpression(InvokeMemberExpressionAst invoke)
    {
        return new InvokeMemberExpression
        {
            NodeKind = "InvokeMemberExpression",
            Language = LanguageName,
            Span = ConvertExtent(invoke.Extent),
            RawSource = invoke.Extent.Text,
            Object = ConvertExpression(invoke.Expression),
            MethodName = invoke.Member.ToString(),
            Arguments = invoke.Arguments?.Select(a => new ArgumentNode
            {
                NodeKind = "ArgumentNode",
                Language = LanguageName,
                Span = ConvertExtent(a.Extent),
                RawSource = a.Extent.Text,
                Value = ConvertExpression(a)
            }).ToList() ?? [],
            IsStatic = invoke.Static,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "InvokeMemberExpressionAst",
                ["NullConditional"] = invoke.NullConditional
            }
        };
    }

    private IndexExpression ConvertIndexExpression(IndexExpressionAst index)
    {
        return new IndexExpression
        {
            NodeKind = "IndexExpression",
            Language = LanguageName,
            Span = ConvertExtent(index.Extent),
            RawSource = index.Extent.Text,
            Object = ConvertExpression(index.Target),
            Index = ConvertExpression(index.Index),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "IndexExpressionAst",
                ["NullConditional"] = index.NullConditional
            }
        };
    }

    private ArrayExpression ConvertArrayExpression(ArrayExpressionAst array)
    {
        var elements = new List<ExpressionNode>();

        if (array.SubExpression.Statements.Count == 1 &&
            array.SubExpression.Statements[0] is PipelineAst pipeline)
        {
            // Single pipeline - treat elements as array elements
            foreach (var element in pipeline.PipelineElements)
            {
                elements.Add(ConvertPipelineElement(element));
            }
        }
        else
        {
            foreach (var stmt in array.SubExpression.Statements)
            {
                if (stmt is PipelineAst p)
                {
                    elements.Add(ConvertExpression(p));
                }
            }
        }

        return new ArrayExpression
        {
            NodeKind = "ArrayExpression",
            Language = LanguageName,
            Span = ConvertExtent(array.Extent),
            RawSource = array.Extent.Text,
            Elements = elements,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ArrayExpressionAst"
            }
        };
    }

    private ArrayExpression ConvertArrayLiteral(ArrayLiteralAst arrayLit)
    {
        return new ArrayExpression
        {
            NodeKind = "ArrayExpression",
            Language = LanguageName,
            Span = ConvertExtent(arrayLit.Extent),
            RawSource = arrayLit.Extent.Text,
            Elements = arrayLit.Elements.Select(ConvertExpression).ToList(),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ArrayLiteralAst"
            }
        };
    }

    private ObjectExpression ConvertHashtable(HashtableAst hash)
    {
        var properties = new List<PropertyNode>();

        foreach (var kvp in hash.KeyValuePairs)
        {
            properties.Add(new PropertyNode
            {
                NodeKind = "PropertyNode",
                Language = LanguageName,
                Span = ConvertExtent(kvp.Item1.Extent),
                RawSource = kvp.Item1.Extent.Text,
                Key = ConvertExpression(kvp.Item1),
                Value = ConvertExpression(kvp.Item2)
            });
        }

        return new ObjectExpression
        {
            NodeKind = "ObjectExpression",
            Language = LanguageName,
            Span = ConvertExtent(hash.Extent),
            RawSource = hash.Extent.Text,
            Properties = properties,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "HashtableAst"
            }
        };
    }

    private ParenthesizedExpression ConvertParenExpression(ParenExpressionAst paren)
    {
        return new ParenthesizedExpression
        {
            NodeKind = "ParenthesizedExpression",
            Language = LanguageName,
            Span = ConvertExtent(paren.Extent),
            RawSource = paren.Extent.Text,
            Expression = ConvertExpression(paren.Pipeline),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ParenExpressionAst"
            }
        };
    }

    private ExpressionNode ConvertSubExpression(SubExpressionAst sub)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = ConvertExtent(sub.Extent),
            RawSource = sub.Extent.Text,
            NativeNodeType = "SubExpressionAst",
            Roles = ["subexpression"],
            ChildNodes = sub.SubExpression.Statements.Select(s => (UastNode)ConvertStatement(s)).ToList(),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "SubExpressionAst"
            }
        };
    }

    private LambdaExpression ConvertScriptBlockExpression(ScriptBlockExpressionAst scriptBlock)
    {
        return new LambdaExpression
        {
            NodeKind = "LambdaExpression",
            Language = LanguageName,
            Span = ConvertExtent(scriptBlock.Extent),
            RawSource = scriptBlock.Extent.Text,
            Parameters = scriptBlock.ScriptBlock.ParamBlock?.Parameters.Select(ConvertParameter).ToList() ?? [],
            Body = ConvertScriptBlock(scriptBlock.ScriptBlock),
            IsAsync = false,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ScriptBlockExpressionAst"
            }
        };
    }

    private TypeExpression ConvertTypeExpression(TypeExpressionAst type)
    {
        return new TypeExpression
        {
            NodeKind = "TypeExpression",
            Language = LanguageName,
            Span = ConvertExtent(type.Extent),
            RawSource = type.Extent.Text,
            Type = new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertExtent(type.Extent),
                Name = type.TypeName.FullName
            },
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "TypeExpressionAst"
            }
        };
    }

    private CastExpression ConvertConvertExpression(ConvertExpressionAst convert)
    {
        return new CastExpression
        {
            NodeKind = "CastExpression",
            Language = LanguageName,
            Span = ConvertExtent(convert.Extent),
            RawSource = convert.Extent.Text,
            Type = new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertExtent(convert.Type.Extent),
                Name = convert.Type.TypeName.FullName
            },
            Expression = ConvertExpression(convert.Child),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ConvertExpressionAst"
            }
        };
    }

    private ConditionalExpression ConvertTernaryExpression(TernaryExpressionAst ternary)
    {
        return new ConditionalExpression
        {
            NodeKind = "ConditionalExpression",
            Language = LanguageName,
            Span = ConvertExtent(ternary.Extent),
            RawSource = ternary.Extent.Text,
            Condition = ConvertExpression(ternary.Condition),
            ThenExpression = ConvertExpression(ternary.IfTrue),
            ElseExpression = ConvertExpression(ternary.IfFalse),
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "TernaryExpressionAst"
            }
        };
    }

    private ExpressionNode ConvertUsingExpression(UsingExpressionAst usingExpr)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = ConvertExtent(usingExpr.Extent),
            RawSource = usingExpr.Extent.Text,
            NativeNodeType = "UsingExpressionAst",
            Roles = ["using-scope"],
            ChildNodes = [ConvertExpression(usingExpr.SubExpression)],
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "UsingExpressionAst"
            }
        };
    }

    private UnknownExpression ConvertUnknownExpression(Ast ast)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = ConvertExtent(ast.Extent),
            RawSource = ast.Extent.Text,
            NativeNodeType = ast.GetType().Name,
            Roles = ["expression"],
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = ast.GetType().Name
            }
        };
    }

    #endregion

    #region Helper Methods

    private ParameterNode ConvertParameter(ParameterAst param)
    {
        return new ParameterNode
        {
            NodeKind = "ParameterNode",
            Language = LanguageName,
            Span = ConvertExtent(param.Extent),
            RawSource = param.Extent.Text,
            Name = param.Name.VariablePath.UserPath,
            Type = param.StaticType != typeof(object)
                ? new NamedTypeReference
                {
                    NodeKind = "NamedTypeReference",
                    Language = LanguageName,
                    Span = SourceSpan.Empty,
                    Name = param.StaticType.Name
                }
                : null,
            DefaultValue = param.DefaultValue != null ? ConvertExpression(param.DefaultValue) : null,
            IsOptional = param.DefaultValue != null,
            Extensions = new Dictionary<string, object>
            {
                ["NativeNodeType"] = "ParameterAst",
                ["Attributes"] = param.Attributes.Select(a => a.TypeName.FullName).ToList()
            }
        };
    }

    private SourceSpan ConvertExtent(IScriptExtent extent)
    {
        return new SourceSpan(
            extent.StartLineNumber,
            extent.StartColumnNumber - 1, // 1-based to 0-based
            extent.EndLineNumber,
            extent.EndColumnNumber - 1,
            extent.StartOffset,
            extent.EndOffset
        );
    }

    private BlockNode CreateEmptyBlock()
    {
        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = SourceSpan.Empty,
            Statements = []
        };
    }

    private AssignmentOperator ConvertAssignmentOperator(TokenKind kind)
    {
        return kind switch
        {
            TokenKind.Equals => AssignmentOperator.Assign,
            TokenKind.PlusEquals => AssignmentOperator.AddAssign,
            TokenKind.MinusEquals => AssignmentOperator.SubtractAssign,
            TokenKind.MultiplyEquals => AssignmentOperator.MultiplyAssign,
            TokenKind.DivideEquals => AssignmentOperator.DivideAssign,
            TokenKind.RemainderEquals => AssignmentOperator.ModuloAssign,
            TokenKind.QuestionQuestionEquals => AssignmentOperator.CoalesceAssign,
            _ => AssignmentOperator.Assign
        };
    }

    private BinaryOperator ConvertBinaryOperator(TokenKind kind)
    {
        return kind switch
        {
            TokenKind.Plus => BinaryOperator.Add,
            TokenKind.Minus => BinaryOperator.Subtract,
            TokenKind.Multiply => BinaryOperator.Multiply,
            TokenKind.Divide => BinaryOperator.Divide,
            TokenKind.Rem => BinaryOperator.Modulo,
            TokenKind.Ieq or TokenKind.Ceq => BinaryOperator.Equal,
            TokenKind.Ine or TokenKind.Cne => BinaryOperator.NotEqual,
            TokenKind.Ilt or TokenKind.Clt => BinaryOperator.LessThan,
            TokenKind.Ile or TokenKind.Cle => BinaryOperator.LessOrEqual,
            TokenKind.Igt or TokenKind.Cgt => BinaryOperator.GreaterThan,
            TokenKind.Ige or TokenKind.Cge => BinaryOperator.GreaterOrEqual,
            TokenKind.And => BinaryOperator.And,
            TokenKind.Or => BinaryOperator.Or,
            TokenKind.Xor => BinaryOperator.Xor,
            TokenKind.Band => BinaryOperator.BitwiseAnd,
            TokenKind.Bor => BinaryOperator.BitwiseOr,
            TokenKind.Bxor => BinaryOperator.BitwiseXor,
            TokenKind.Shl => BinaryOperator.LeftShift,
            TokenKind.Shr => BinaryOperator.RightShift,
            TokenKind.DotDot => BinaryOperator.Range,
            TokenKind.QuestionQuestion => BinaryOperator.Coalesce,
            TokenKind.Is => BinaryOperator.Is,
            TokenKind.IsNot => BinaryOperator.Is, // With negation
            TokenKind.As => BinaryOperator.As,
            TokenKind.Join => BinaryOperator.Join,
            TokenKind.Ilike or TokenKind.Clike => BinaryOperator.Like,
            TokenKind.Inotlike or TokenKind.Cnotlike => BinaryOperator.NotLike,
            TokenKind.Imatch or TokenKind.Cmatch => BinaryOperator.Match,
            TokenKind.Inotmatch or TokenKind.Cnotmatch => BinaryOperator.NotMatch,
            TokenKind.Ireplace or TokenKind.Creplace => BinaryOperator.Replace,
            TokenKind.Icontains or TokenKind.Ccontains => BinaryOperator.Contains,
            TokenKind.Inotcontains or TokenKind.Cnotcontains => BinaryOperator.NotContains,
            TokenKind.Iin or TokenKind.Cin => BinaryOperator.In,
            TokenKind.Inotin or TokenKind.Cnotin => BinaryOperator.NotIn,
            TokenKind.Isplit or TokenKind.Csplit => BinaryOperator.Split,
            _ => BinaryOperator.Equal
        };
    }

    private UnaryOperator ConvertUnaryOperator(TokenKind kind)
    {
        return kind switch
        {
            TokenKind.Not or TokenKind.Exclaim => UnaryOperator.Not,
            TokenKind.Minus => UnaryOperator.Negate,
            TokenKind.Plus => UnaryOperator.Negate, // Unary plus is essentially no-op
            TokenKind.PlusPlus => UnaryOperator.Increment,
            TokenKind.MinusMinus => UnaryOperator.Decrement,
            TokenKind.Bnot => UnaryOperator.BitwiseNot,
            TokenKind.Join => UnaryOperator.Not, // Join as unary
            TokenKind.Comma => UnaryOperator.Not, // Comma for array
            _ => UnaryOperator.Not
        };
    }

    private bool IsUnaryPrefix(TokenKind kind)
    {
        return kind switch
        {
            TokenKind.PlusPlus => true,
            TokenKind.MinusMinus => true,
            _ => true // Most PowerShell unary operators are prefix
        };
    }

    private LiteralKind InferLiteralKind(object? value)
    {
        return value switch
        {
            null => LiteralKind.Null,
            bool => LiteralKind.Boolean,
            string => LiteralKind.String,
            char => LiteralKind.Char,
            int or long or short or byte or sbyte or uint or ulong or ushort => LiteralKind.Integer,
            float or double or decimal => LiteralKind.Float,
            _ => LiteralKind.String
        };
    }

    #endregion
}
