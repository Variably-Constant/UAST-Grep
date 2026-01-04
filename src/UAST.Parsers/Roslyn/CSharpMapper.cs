using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;
using UAST.Core.Interfaces;
using UAST.Core.Schema;
using UAST.Core.Schema.Declarations;
using UAST.Core.Schema.Expressions;
using UAST.Core.Schema.Extensions;
using UAST.Core.Schema.Statements;
using UAST.Core.Schema.Types;

namespace UAST.Parsers.Roslyn;

/// <summary>
/// Maps C# AST (via Roslyn) to UAST.
/// Uses Microsoft.CodeAnalysis for parsing.
/// Embedded in CLI for single-package distribution.
/// </summary>
public class CSharpMapper : ILanguageMapper
{
    private readonly List<ParseError> _errors = [];
    private SyntaxTree? _currentTree;

    public string LanguageName => "CSharp";

    public IReadOnlyList<string> FileExtensions => [".cs"];

    public UastNode Parse(string source, string? filePath = null)
    {
        _errors.Clear();
        _currentTree = CSharpSyntaxTree.ParseText(source, path: filePath ?? "");
        var root = _currentTree.GetCompilationUnitRoot();

        foreach (var diagnostic in root.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error))
        {
            var lineSpan = diagnostic.Location.GetLineSpan();
            _errors.Add(new ParseError(
                diagnostic.GetMessage(),
                new SourceSpan(
                    lineSpan.StartLinePosition.Line + 1,
                    lineSpan.StartLinePosition.Character,
                    lineSpan.EndLinePosition.Line + 1,
                    lineSpan.EndLinePosition.Character,
                    diagnostic.Location.SourceSpan.Start,
                    diagnostic.Location.SourceSpan.End
                ),
                diagnostic.Id,
                ParseErrorSeverity.Error
            ));
        }

        var result = ConvertCompilationUnit(root);
        result.SetParentReferences();
        return result;
    }

    public UastNode ParseFile(string filePath)
    {
        var source = File.ReadAllText(filePath);
        return Parse(source, filePath);
    }

    public IReadOnlyList<ParseError> GetErrors() => _errors;

    #region Compilation Unit

    private BlockNode ConvertCompilationUnit(CompilationUnitSyntax unit)
    {
        var statements = new List<StatementNode>();

        // Using directives
        foreach (var usingDirective in unit.Usings)
        {
            statements.Add(ConvertUsingDirective(usingDirective));
        }

        // Extern alias directives
        foreach (var externAlias in unit.Externs)
        {
            statements.Add(ConvertExternAlias(externAlias));
        }

        // Attribute lists (assembly/module level)
        foreach (var attrList in unit.AttributeLists)
        {
            // Convert as expression statement wrapping attribute
            foreach (var attr in attrList.Attributes)
            {
                statements.Add(new ExpressionStatement
                {
                    NodeKind = "ExpressionStatement",
                    Language = LanguageName,
                    Span = ConvertSpan(attr.Span),
                    RawSource = attr.ToFullString(),
                    Expression = ConvertAttributeAsExpression(attr)
                });
            }
        }

        // Members (namespaces, classes, etc.)
        foreach (var member in unit.Members)
        {
            statements.Add(ConvertMember(member));
        }

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertSpan(unit.Span),
            RawSource = unit.ToFullString(),
            Statements = statements
        };
    }

    private StatementNode ConvertUsingDirective(UsingDirectiveSyntax usingDirective)
    {
        // Check if this is a using alias: using Alias = Type;
        if (usingDirective.Alias != null)
        {
            var aliasDirective = new CsUsingAliasDirective
            {
                NodeKind = "CsUsingAliasDirective",
                Language = LanguageName,
                Span = ConvertSpan(usingDirective.Span),
                RawSource = usingDirective.ToFullString(),
                Alias = usingDirective.Alias.Name.Identifier.Text,
                Type = usingDirective.NamespaceOrType != null
                    ? ConvertType(usingDirective.NamespaceOrType)
                    : new NamedTypeReference
                    {
                        NodeKind = "NamedTypeReference",
                        Language = LanguageName,
                        Span = ConvertSpan(usingDirective.Span),
                        Name = usingDirective.Name?.ToString() ?? ""
                    },
                IsGlobal = usingDirective.GlobalKeyword.IsKind(SyntaxKind.GlobalKeyword)
            };

            return new ExpressionStatement
            {
                NodeKind = "ExpressionStatement",
                Language = LanguageName,
                Span = ConvertSpan(usingDirective.Span),
                RawSource = usingDirective.ToFullString(),
                Expression = new UnknownExpression
                {
                    NodeKind = "UnknownExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(usingDirective.Span),
                    RawSource = usingDirective.ToFullString(),
                    NativeNodeType = "CsUsingAliasDirective",
                    Roles = ["import", "alias"],
                    Extensions = new Dictionary<string, object>
                    {
                        ["Alias"] = aliasDirective.Alias,
                        ["Type"] = aliasDirective.Type.ToString() ?? "",
                        ["IsGlobal"] = aliasDirective.IsGlobal
                    }
                }
            };
        }

        // Check if this is a global using: global using Namespace;
        if (usingDirective.GlobalKeyword.IsKind(SyntaxKind.GlobalKeyword))
        {
            return new ExpressionStatement
            {
                NodeKind = "ExpressionStatement",
                Language = LanguageName,
                Span = ConvertSpan(usingDirective.Span),
                RawSource = usingDirective.ToFullString(),
                Expression = new UnknownExpression
                {
                    NodeKind = "UnknownExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(usingDirective.Span),
                    RawSource = usingDirective.ToFullString(),
                    NativeNodeType = "CsGlobalUsingDirective",
                    Roles = ["import", "global"],
                    Extensions = new Dictionary<string, object>
                    {
                        ["Target"] = usingDirective.NamespaceOrType?.ToString() ?? usingDirective.Name?.ToString() ?? "",
                        ["IsStatic"] = usingDirective.StaticKeyword.IsKind(SyntaxKind.StaticKeyword)
                    }
                }
            };
        }

        // Standard using directive
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertSpan(usingDirective.Span),
            RawSource = usingDirective.ToFullString(),
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertSpan(usingDirective.Span),
                RawSource = usingDirective.ToFullString(),
                NativeNodeType = "UsingDirective",
                Roles = ["import"]
            }
        };
    }

    private ExpressionStatement ConvertExternAlias(ExternAliasDirectiveSyntax externAlias)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertSpan(externAlias.Span),
            RawSource = externAlias.ToFullString(),
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertSpan(externAlias.Span),
                RawSource = externAlias.ToFullString(),
                NativeNodeType = "ExternAliasDirective",
                Roles = ["import"]
            }
        };
    }

    #endregion

    #region Member Conversion

    private StatementNode ConvertMember(MemberDeclarationSyntax member)
    {
        return member switch
        {
            NamespaceDeclarationSyntax ns => ConvertNamespace(ns),
            FileScopedNamespaceDeclarationSyntax ns => ConvertFileScopedNamespace(ns),
            ClassDeclarationSyntax cls => ConvertClass(cls),
            StructDeclarationSyntax str => ConvertStruct(str),
            InterfaceDeclarationSyntax iface => ConvertInterface(iface),
            EnumDeclarationSyntax enm => ConvertEnum(enm),
            RecordDeclarationSyntax rec => ConvertRecord(rec),
            DelegateDeclarationSyntax del => ConvertDelegate(del),
            MethodDeclarationSyntax method => ConvertMethod(method),
            PropertyDeclarationSyntax prop => ConvertProperty(prop),
            FieldDeclarationSyntax field => ConvertField(field),
            EventDeclarationSyntax evt => ConvertEvent(evt),
            EventFieldDeclarationSyntax evtField => ConvertEventField(evtField),
            ConstructorDeclarationSyntax ctor => ConvertConstructor(ctor),
            DestructorDeclarationSyntax dtor => ConvertDestructor(dtor),
            IndexerDeclarationSyntax indexer => ConvertIndexer(indexer),
            OperatorDeclarationSyntax op => ConvertOperator(op),
            ConversionOperatorDeclarationSyntax conv => ConvertConversionOperator(conv),
            GlobalStatementSyntax global => ConvertStatement(global.Statement),
            _ => ConvertUnknownMember(member)
        };
    }

    private TypeDeclaration ConvertNamespace(NamespaceDeclarationSyntax ns)
    {
        return new TypeDeclaration
        {
            NodeKind = "TypeDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(ns.Span),
            RawSource = ns.ToFullString(),
            Name = ns.Name.ToString(),
            Kind = TypeDeclarationKind.Class, // Using Class as namespace placeholder
            Members = ns.Members.Select(m => (DeclarationNode)ConvertMember(m)).ToList(),
            Extensions = new Dictionary<string, object>
            {
                ["IsNamespace"] = true
            }
        };
    }

    private CsFileScopedNamespaceDeclaration ConvertFileScopedNamespace(FileScopedNamespaceDeclarationSyntax ns)
    {
        return new CsFileScopedNamespaceDeclaration
        {
            NodeKind = "CsFileScopedNamespaceDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(ns.Span),
            RawSource = ns.ToFullString(),
            Name = ns.Name.ToString(),
            NamespaceMembers = ns.Members.Select(m => (DeclarationNode)ConvertMember(m)).ToList()
        };
    }

    private TypeDeclaration ConvertClass(ClassDeclarationSyntax cls)
    {
        // Check if this is a file-scoped type (C# 11+)
        bool isFileScoped = cls.Modifiers.Any(m => m.IsKind(SyntaxKind.FileKeyword));

        // Check for primary constructor (C# 12+)
        bool hasPrimaryConstructor = cls.ParameterList != null;

        if (isFileScoped)
        {
            return new CsFileScopedTypeDeclaration
            {
                NodeKind = "CsFileScopedTypeDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(cls.Span),
                RawSource = cls.ToFullString(),
                Name = cls.Identifier.Text,
                Kind = TypeDeclarationKind.Class,
                Visibility = GetVisibility(cls.Modifiers),
                BaseTypes = cls.BaseList?.Types
                    .Select(t => ConvertType(t.Type))
                    .ToList() ?? [],
                TypeParameters = cls.TypeParameterList?.Parameters
                    .Select(ConvertTypeParameter)
                    .ToList() ?? [],
                Members = cls.Members
                    .Select(m => (DeclarationNode)ConvertMember(m))
                    .ToList(),
                Modifiers = GetModifiers(cls.Modifiers),
                Attributes = ConvertAttributeLists(cls.AttributeLists)
            };
        }

        if (hasPrimaryConstructor)
        {
            return new CsPrimaryConstructorDeclaration
            {
                NodeKind = "CsPrimaryConstructorDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(cls.Span),
                RawSource = cls.ToFullString(),
                Name = cls.Identifier.Text,
                Kind = TypeDeclarationKind.Class,
                Visibility = GetVisibility(cls.Modifiers),
                BaseTypes = cls.BaseList?.Types
                    .Select(t => ConvertType(t.Type))
                    .ToList() ?? [],
                TypeParameters = cls.TypeParameterList?.Parameters
                    .Select(ConvertTypeParameter)
                    .ToList() ?? [],
                Parameters = cls.ParameterList!.Parameters
                    .Select(ConvertParameter)
                    .ToList(),
                Members = cls.Members
                    .Select(m => (DeclarationNode)ConvertMember(m))
                    .ToList(),
                Modifiers = GetModifiers(cls.Modifiers),
                Attributes = ConvertAttributeLists(cls.AttributeLists)
            };
        }

        return new TypeDeclaration
        {
            NodeKind = "TypeDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(cls.Span),
            RawSource = cls.ToFullString(),
            Name = cls.Identifier.Text,
            Kind = TypeDeclarationKind.Class,
            Visibility = GetVisibility(cls.Modifiers),
            BaseTypes = cls.BaseList?.Types
                .Select(t => ConvertType(t.Type))
                .ToList() ?? [],
            TypeParameters = cls.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            Members = cls.Members
                .Select(m => (DeclarationNode)ConvertMember(m))
                .ToList(),
            Modifiers = GetModifiers(cls.Modifiers),
            Attributes = ConvertAttributeLists(cls.AttributeLists)
        };
    }

    private TypeDeclaration ConvertStruct(StructDeclarationSyntax str)
    {
        // Check if this is a file-scoped type (C# 11+)
        bool isFileScoped = str.Modifiers.Any(m => m.IsKind(SyntaxKind.FileKeyword));

        // Check for primary constructor (C# 12+)
        bool hasPrimaryConstructor = str.ParameterList != null;

        if (isFileScoped)
        {
            return new CsFileScopedTypeDeclaration
            {
                NodeKind = "CsFileScopedTypeDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(str.Span),
                RawSource = str.ToFullString(),
                Name = str.Identifier.Text,
                Kind = TypeDeclarationKind.Struct,
                Visibility = GetVisibility(str.Modifiers),
                BaseTypes = str.BaseList?.Types
                    .Select(t => ConvertType(t.Type))
                    .ToList() ?? [],
                TypeParameters = str.TypeParameterList?.Parameters
                    .Select(ConvertTypeParameter)
                    .ToList() ?? [],
                Members = str.Members
                    .Select(m => (DeclarationNode)ConvertMember(m))
                    .ToList(),
                Modifiers = GetModifiers(str.Modifiers),
                Attributes = ConvertAttributeLists(str.AttributeLists)
            };
        }

        if (hasPrimaryConstructor)
        {
            return new CsPrimaryConstructorDeclaration
            {
                NodeKind = "CsPrimaryConstructorDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(str.Span),
                RawSource = str.ToFullString(),
                Name = str.Identifier.Text,
                Kind = TypeDeclarationKind.Struct,
                Visibility = GetVisibility(str.Modifiers),
                BaseTypes = str.BaseList?.Types
                    .Select(t => ConvertType(t.Type))
                    .ToList() ?? [],
                TypeParameters = str.TypeParameterList?.Parameters
                    .Select(ConvertTypeParameter)
                    .ToList() ?? [],
                Parameters = str.ParameterList!.Parameters
                    .Select(ConvertParameter)
                    .ToList(),
                Members = str.Members
                    .Select(m => (DeclarationNode)ConvertMember(m))
                    .ToList(),
                Modifiers = GetModifiers(str.Modifiers),
                Attributes = ConvertAttributeLists(str.AttributeLists)
            };
        }

        return new TypeDeclaration
        {
            NodeKind = "TypeDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(str.Span),
            RawSource = str.ToFullString(),
            Name = str.Identifier.Text,
            Kind = TypeDeclarationKind.Struct,
            Visibility = GetVisibility(str.Modifiers),
            BaseTypes = str.BaseList?.Types
                .Select(t => ConvertType(t.Type))
                .ToList() ?? [],
            TypeParameters = str.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            Members = str.Members
                .Select(m => (DeclarationNode)ConvertMember(m))
                .ToList(),
            Modifiers = GetModifiers(str.Modifiers),
            Attributes = ConvertAttributeLists(str.AttributeLists)
        };
    }

    private TypeDeclaration ConvertInterface(InterfaceDeclarationSyntax iface)
    {
        return new TypeDeclaration
        {
            NodeKind = "TypeDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(iface.Span),
            RawSource = iface.ToFullString(),
            Name = iface.Identifier.Text,
            Kind = TypeDeclarationKind.Interface,
            Visibility = GetVisibility(iface.Modifiers),
            BaseTypes = iface.BaseList?.Types
                .Select(t => ConvertType(t.Type))
                .ToList() ?? [],
            TypeParameters = iface.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            Members = iface.Members
                .Select(m => (DeclarationNode)ConvertMember(m))
                .ToList(),
            Modifiers = GetModifiers(iface.Modifiers),
            Attributes = ConvertAttributeLists(iface.AttributeLists)
        };
    }

    private TypeDeclaration ConvertEnum(EnumDeclarationSyntax enm)
    {
        return new TypeDeclaration
        {
            NodeKind = "TypeDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(enm.Span),
            RawSource = enm.ToFullString(),
            Name = enm.Identifier.Text,
            Kind = TypeDeclarationKind.Enum,
            Visibility = GetVisibility(enm.Modifiers),
            BaseTypes = enm.BaseList?.Types
                .Select(t => ConvertType(t.Type))
                .ToList() ?? [],
            Members = enm.Members
                .Select(ConvertEnumMember)
                .Cast<DeclarationNode>()
                .ToList(),
            Modifiers = GetModifiers(enm.Modifiers),
            Attributes = ConvertAttributeLists(enm.AttributeLists)
        };
    }

    private EnumMemberDeclaration ConvertEnumMember(EnumMemberDeclarationSyntax member)
    {
        return new EnumMemberDeclaration
        {
            NodeKind = "EnumMemberDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(member.Span),
            RawSource = member.ToFullString(),
            Name = member.Identifier.Text,
            Value = member.EqualsValue != null ? ConvertExpression(member.EqualsValue.Value) : null,
            Attributes = ConvertAttributeLists(member.AttributeLists)
        };
    }

    private TypeDeclaration ConvertRecord(RecordDeclarationSyntax rec)
    {
        return new CsRecordDeclaration
        {
            NodeKind = "CsRecordDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(rec.Span),
            RawSource = rec.ToFullString(),
            Name = rec.Identifier.Text,
            Kind = TypeDeclarationKind.Record,
            IsRecordStruct = rec.ClassOrStructKeyword.IsKind(SyntaxKind.StructKeyword),
            Visibility = GetVisibility(rec.Modifiers),
            BaseTypes = rec.BaseList?.Types
                .Select(t => ConvertType(t.Type))
                .ToList() ?? [],
            TypeParameters = rec.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            PrimaryConstructorParameters = rec.ParameterList?.Parameters
                .Select(ConvertParameter)
                .ToList() ?? [],
            Members = rec.Members
                .Select(m => (DeclarationNode)ConvertMember(m))
                .ToList(),
            Modifiers = GetModifiers(rec.Modifiers),
            Attributes = ConvertAttributeLists(rec.AttributeLists)
        };
    }

    private FunctionDeclaration ConvertDelegate(DelegateDeclarationSyntax del)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(del.Span),
            RawSource = del.ToFullString(),
            Name = del.Identifier.Text,
            Parameters = del.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            ReturnType = ConvertType(del.ReturnType),
            TypeParameters = del.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            Visibility = GetVisibility(del.Modifiers),
            Modifiers = GetModifiers(del.Modifiers),
            Attributes = ConvertAttributeLists(del.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsDelegate"] = true
            }
        };
    }

    private FunctionDeclaration ConvertMethod(MethodDeclarationSyntax method)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(method.Span),
            RawSource = method.ToFullString(),
            Name = method.Identifier.Text,
            Visibility = GetVisibility(method.Modifiers),
            Modifiers = GetModifiers(method.Modifiers),
            Parameters = method.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            ReturnType = ConvertType(method.ReturnType),
            Body = method.Body != null
                ? ConvertBlock(method.Body)
                : method.ExpressionBody != null
                    ? ConvertExpressionBody(method.ExpressionBody)
                    : null,
            IsAsync = method.Modifiers.Any(SyntaxKind.AsyncKeyword),
            TypeParameters = method.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            Attributes = ConvertAttributeLists(method.AttributeLists)
        };
    }

    private FunctionDeclaration ConvertConstructor(ConstructorDeclarationSyntax ctor)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(ctor.Span),
            RawSource = ctor.ToFullString(),
            Name = ctor.Identifier.Text,
            Visibility = GetVisibility(ctor.Modifiers),
            Modifiers = GetModifiers(ctor.Modifiers),
            Parameters = ctor.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            Body = ctor.Body != null
                ? ConvertBlock(ctor.Body)
                : ctor.ExpressionBody != null
                    ? ConvertExpressionBody(ctor.ExpressionBody)
                    : null,
            Attributes = ConvertAttributeLists(ctor.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsConstructor"] = true,
                ["ThisOrBaseInitializer"] = ctor.Initializer?.ThisOrBaseKeyword.Text ?? ""
            }
        };
    }

    private FunctionDeclaration ConvertDestructor(DestructorDeclarationSyntax dtor)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(dtor.Span),
            RawSource = dtor.ToFullString(),
            Name = "~" + dtor.Identifier.Text,
            Body = dtor.Body != null
                ? ConvertBlock(dtor.Body)
                : dtor.ExpressionBody != null
                    ? ConvertExpressionBody(dtor.ExpressionBody)
                    : null,
            Attributes = ConvertAttributeLists(dtor.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsDestructor"] = true
            }
        };
    }

    private PropertyDeclaration ConvertProperty(PropertyDeclarationSyntax prop)
    {
        return new PropertyDeclaration
        {
            NodeKind = "PropertyDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(prop.Span),
            RawSource = prop.ToFullString(),
            Name = prop.Identifier.Text,
            Type = ConvertType(prop.Type),
            Visibility = GetVisibility(prop.Modifiers),
            Modifiers = GetModifiers(prop.Modifiers),
            Initializer = prop.Initializer != null ? ConvertExpression(prop.Initializer.Value) : null,
            Attributes = ConvertAttributeLists(prop.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["HasGetter"] = prop.AccessorList?.Accessors.Any(a => a.IsKind(SyntaxKind.GetAccessorDeclaration)) ?? prop.ExpressionBody != null,
                ["HasSetter"] = prop.AccessorList?.Accessors.Any(a => a.IsKind(SyntaxKind.SetAccessorDeclaration) || a.IsKind(SyntaxKind.InitAccessorDeclaration)) ?? false
            }
        };
    }

    private StatementNode ConvertField(FieldDeclarationSyntax field)
    {
        // Multiple declarators in a field become multiple variable declarations
        var declarations = new List<StatementNode>();
        foreach (var variable in field.Declaration.Variables)
        {
            declarations.Add(new VariableDeclaration
            {
                NodeKind = "VariableDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(variable.Span),
                RawSource = variable.ToFullString(),
                Name = variable.Identifier.Text,
                Type = ConvertType(field.Declaration.Type),
                Initializer = variable.Initializer != null ? ConvertExpression(variable.Initializer.Value) : null,
                Kind = VariableKind.Field,
                Visibility = GetVisibility(field.Modifiers),
                Modifiers = GetModifiers(field.Modifiers),
                Attributes = ConvertAttributeLists(field.AttributeLists)
            });
        }

        if (declarations.Count == 1)
            return declarations[0];

        // Return as block for multiple declarations
        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertSpan(field.Span),
            RawSource = field.ToFullString(),
            Statements = declarations
        };
    }

    private FunctionDeclaration ConvertEvent(EventDeclarationSyntax evt)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(evt.Span),
            RawSource = evt.ToFullString(),
            Name = evt.Identifier.Text,
            ReturnType = ConvertType(evt.Type),
            Visibility = GetVisibility(evt.Modifiers),
            Modifiers = GetModifiers(evt.Modifiers),
            Attributes = ConvertAttributeLists(evt.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsEvent"] = true
            }
        };
    }

    private StatementNode ConvertEventField(EventFieldDeclarationSyntax evtField)
    {
        var declarations = new List<StatementNode>();
        foreach (var variable in evtField.Declaration.Variables)
        {
            declarations.Add(new VariableDeclaration
            {
                NodeKind = "VariableDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(variable.Span),
                RawSource = variable.ToFullString(),
                Name = variable.Identifier.Text,
                Type = ConvertType(evtField.Declaration.Type),
                Kind = VariableKind.Field,
                Visibility = GetVisibility(evtField.Modifiers),
                Modifiers = GetModifiers(evtField.Modifiers),
                Attributes = ConvertAttributeLists(evtField.AttributeLists),
                Extensions = new Dictionary<string, object>
                {
                    ["IsEvent"] = true
                }
            });
        }

        if (declarations.Count == 1)
            return declarations[0];

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertSpan(evtField.Span),
            RawSource = evtField.ToFullString(),
            Statements = declarations
        };
    }

    private FunctionDeclaration ConvertIndexer(IndexerDeclarationSyntax indexer)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(indexer.Span),
            RawSource = indexer.ToFullString(),
            Name = "this",
            Parameters = indexer.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            ReturnType = ConvertType(indexer.Type),
            Visibility = GetVisibility(indexer.Modifiers),
            Modifiers = GetModifiers(indexer.Modifiers),
            Attributes = ConvertAttributeLists(indexer.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsIndexer"] = true
            }
        };
    }

    private FunctionDeclaration ConvertOperator(OperatorDeclarationSyntax op)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(op.Span),
            RawSource = op.ToFullString(),
            Name = "operator " + op.OperatorToken.Text,
            Parameters = op.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            ReturnType = ConvertType(op.ReturnType),
            Body = op.Body != null
                ? ConvertBlock(op.Body)
                : op.ExpressionBody != null
                    ? ConvertExpressionBody(op.ExpressionBody)
                    : null,
            Visibility = GetVisibility(op.Modifiers),
            Modifiers = GetModifiers(op.Modifiers),
            Attributes = ConvertAttributeLists(op.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsOperator"] = true
            }
        };
    }

    private FunctionDeclaration ConvertConversionOperator(ConversionOperatorDeclarationSyntax conv)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(conv.Span),
            RawSource = conv.ToFullString(),
            Name = (conv.ImplicitOrExplicitKeyword.IsKind(SyntaxKind.ImplicitKeyword) ? "implicit " : "explicit ") + "operator " + conv.Type.ToString(),
            Parameters = conv.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            ReturnType = ConvertType(conv.Type),
            Body = conv.Body != null
                ? ConvertBlock(conv.Body)
                : conv.ExpressionBody != null
                    ? ConvertExpressionBody(conv.ExpressionBody)
                    : null,
            Visibility = GetVisibility(conv.Modifiers),
            Modifiers = GetModifiers(conv.Modifiers),
            Attributes = ConvertAttributeLists(conv.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsConversionOperator"] = true,
                ["IsImplicit"] = conv.ImplicitOrExplicitKeyword.IsKind(SyntaxKind.ImplicitKeyword)
            }
        };
    }

    private StatementNode ConvertUnknownMember(MemberDeclarationSyntax member)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertSpan(member.Span),
            RawSource = member.ToFullString(),
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertSpan(member.Span),
                RawSource = member.ToFullString(),
                NativeNodeType = member.GetType().Name,
                Roles = ["declaration"]
            }
        };
    }

    #endregion

    #region Statement Conversion

    private BlockNode ConvertBlock(BlockSyntax block)
    {
        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertSpan(block.Span),
            RawSource = block.ToFullString(),
            Statements = block.Statements.Select(ConvertStatement).ToList()
        };
    }

    private BlockNode ConvertExpressionBody(ArrowExpressionClauseSyntax arrow)
    {
        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertSpan(arrow.Span),
            RawSource = arrow.ToFullString(),
            Statements =
            [
                new ReturnStatement
                {
                    NodeKind = "ReturnStatement",
                    Language = LanguageName,
                    Span = ConvertSpan(arrow.Expression.Span),
                    RawSource = arrow.Expression.ToFullString(),
                    Value = ConvertExpression(arrow.Expression)
                }
            ]
        };
    }

    private StatementNode ConvertStatement(StatementSyntax stmt)
    {
        return stmt switch
        {
            BlockSyntax block => ConvertBlock(block),
            LocalDeclarationStatementSyntax local => ConvertLocalDeclaration(local),
            ExpressionStatementSyntax expr => ConvertExpressionStatement(expr),
            IfStatementSyntax if_ => ConvertIf(if_),
            WhileStatementSyntax while_ => ConvertWhile(while_),
            DoStatementSyntax do_ => ConvertDoWhile(do_),
            ForStatementSyntax for_ => ConvertFor(for_),
            ForEachStatementSyntax foreach_ => ConvertForEach(foreach_),
            ForEachVariableStatementSyntax foreachVar => ConvertForEachVariable(foreachVar),
            SwitchStatementSyntax switch_ => ConvertSwitch(switch_),
            TryStatementSyntax try_ => ConvertTry(try_),
            ReturnStatementSyntax return_ => ConvertReturn(return_),
            BreakStatementSyntax break_ => ConvertBreak(break_),
            ContinueStatementSyntax continue_ => ConvertContinue(continue_),
            ThrowStatementSyntax throw_ => ConvertThrow(throw_),
            UsingStatementSyntax using_ => ConvertUsingStatement(using_),
            LockStatementSyntax lock_ => ConvertLock(lock_),
            YieldStatementSyntax yield => ConvertYield(yield),
            GotoStatementSyntax goto_ => ConvertGoto(goto_),
            LabeledStatementSyntax labeled => ConvertLabeled(labeled),
            CheckedStatementSyntax checked_ => ConvertChecked(checked_),
            UnsafeStatementSyntax unsafe_ => ConvertUnsafe(unsafe_),
            FixedStatementSyntax fixed_ => ConvertFixed(fixed_),
            LocalFunctionStatementSyntax localFunc => ConvertLocalFunction(localFunc),
            EmptyStatementSyntax => new EmptyStatement
            {
                NodeKind = "EmptyStatement",
                Language = LanguageName,
                Span = ConvertSpan(stmt.Span),
                RawSource = stmt.ToFullString()
            },
            _ => ConvertUnknownStatement(stmt)
        };
    }

    private StatementNode ConvertLocalDeclaration(LocalDeclarationStatementSyntax local)
    {
        var declarations = new List<StatementNode>();
        foreach (var variable in local.Declaration.Variables)
        {
            declarations.Add(new VariableDeclaration
            {
                NodeKind = "VariableDeclaration",
                Language = LanguageName,
                Span = ConvertSpan(variable.Span),
                RawSource = variable.ToFullString(),
                Name = variable.Identifier.Text,
                Type = ConvertType(local.Declaration.Type),
                Initializer = variable.Initializer != null ? ConvertExpression(variable.Initializer.Value) : null,
                Kind = local.IsConst ? VariableKind.Constant : VariableKind.Local,
                Modifiers = local.IsConst
                    ? [new ModifierNode { NodeKind = "ModifierNode", Language = LanguageName, Span = SourceSpan.Empty, Keyword = "const" }]
                    : local.UsingKeyword.IsKind(SyntaxKind.UsingKeyword)
                        ? [new ModifierNode { NodeKind = "ModifierNode", Language = LanguageName, Span = SourceSpan.Empty, Keyword = "using" }]
                        : []
            });
        }

        if (declarations.Count == 1)
            return declarations[0];

        return new BlockNode
        {
            NodeKind = "BlockNode",
            Language = LanguageName,
            Span = ConvertSpan(local.Span),
            RawSource = local.ToFullString(),
            Statements = declarations
        };
    }

    private ExpressionStatement ConvertExpressionStatement(ExpressionStatementSyntax expr)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertSpan(expr.Span),
            RawSource = expr.ToFullString(),
            Expression = ConvertExpression(expr.Expression)
        };
    }

    private IfStatement ConvertIf(IfStatementSyntax if_)
    {
        return new IfStatement
        {
            NodeKind = "IfStatement",
            Language = LanguageName,
            Span = ConvertSpan(if_.Span),
            RawSource = if_.ToFullString(),
            Condition = ConvertExpression(if_.Condition),
            ThenBranch = ConvertStatement(if_.Statement),
            ElseBranch = if_.Else != null ? ConvertStatement(if_.Else.Statement) : null
        };
    }

    private WhileStatement ConvertWhile(WhileStatementSyntax while_)
    {
        return new WhileStatement
        {
            NodeKind = "WhileStatement",
            Language = LanguageName,
            Span = ConvertSpan(while_.Span),
            RawSource = while_.ToFullString(),
            Condition = ConvertExpression(while_.Condition),
            Body = ConvertStatement(while_.Statement)
        };
    }

    private DoWhileStatement ConvertDoWhile(DoStatementSyntax do_)
    {
        return new DoWhileStatement
        {
            NodeKind = "DoWhileStatement",
            Language = LanguageName,
            Span = ConvertSpan(do_.Span),
            RawSource = do_.ToFullString(),
            Body = ConvertStatement(do_.Statement),
            Condition = ConvertExpression(do_.Condition)
        };
    }

    private ForStatement ConvertFor(ForStatementSyntax for_)
    {
        StatementNode? initializer = null;
        if (for_.Declaration != null)
        {
            var vars = for_.Declaration.Variables;
            if (vars.Count == 1)
            {
                initializer = new VariableDeclaration
                {
                    NodeKind = "VariableDeclaration",
                    Language = LanguageName,
                    Span = ConvertSpan(vars[0].Span),
                    Name = vars[0].Identifier.Text,
                    Type = ConvertType(for_.Declaration.Type),
                    Initializer = vars[0].Initializer != null ? ConvertExpression(vars[0].Initializer.Value) : null,
                    Kind = VariableKind.Local
                };
            }
            else
            {
                initializer = new BlockNode
                {
                    NodeKind = "BlockNode",
                    Language = LanguageName,
                    Span = ConvertSpan(for_.Declaration.Span),
                    Statements = vars.Select(v => (StatementNode)new VariableDeclaration
                    {
                        NodeKind = "VariableDeclaration",
                        Language = LanguageName,
                        Span = ConvertSpan(v.Span),
                        Name = v.Identifier.Text,
                        Type = ConvertType(for_.Declaration.Type),
                        Initializer = v.Initializer != null ? ConvertExpression(v.Initializer.Value) : null,
                        Kind = VariableKind.Local
                    }).ToList()
                };
            }
        }
        else if (for_.Initializers.Count > 0)
        {
            initializer = new ExpressionStatement
            {
                NodeKind = "ExpressionStatement",
                Language = LanguageName,
                Span = ConvertSpan(for_.Initializers[0].Span),
                Expression = ConvertExpression(for_.Initializers[0])
            };
        }

        return new ForStatement
        {
            NodeKind = "ForStatement",
            Language = LanguageName,
            Span = ConvertSpan(for_.Span),
            RawSource = for_.ToFullString(),
            Initializer = initializer,
            Condition = for_.Condition != null ? ConvertExpression(for_.Condition) : null,
            Increment = for_.Incrementors.Count > 0 ? ConvertExpression(for_.Incrementors[0]) : null,
            Body = ConvertStatement(for_.Statement)
        };
    }

    private ForEachStatement ConvertForEach(ForEachStatementSyntax foreach_)
    {
        return new ForEachStatement
        {
            NodeKind = "ForEachStatement",
            Language = LanguageName,
            Span = ConvertSpan(foreach_.Span),
            RawSource = foreach_.ToFullString(),
            VariableName = foreach_.Identifier.Text,
            VariableType = ConvertType(foreach_.Type),
            Iterable = ConvertExpression(foreach_.Expression),
            Body = ConvertStatement(foreach_.Statement)
        };
    }

    private ForEachStatement ConvertForEachVariable(ForEachVariableStatementSyntax foreachVar)
    {
        return new ForEachStatement
        {
            NodeKind = "ForEachStatement",
            Language = LanguageName,
            Span = ConvertSpan(foreachVar.Span),
            RawSource = foreachVar.ToFullString(),
            VariableName = foreachVar.Variable.ToString(),
            Iterable = ConvertExpression(foreachVar.Expression),
            Body = ConvertStatement(foreachVar.Statement)
        };
    }

    private SwitchStatement ConvertSwitch(SwitchStatementSyntax switch_)
    {
        return new SwitchStatement
        {
            NodeKind = "SwitchStatement",
            Language = LanguageName,
            Span = ConvertSpan(switch_.Span),
            RawSource = switch_.ToFullString(),
            Subject = ConvertExpression(switch_.Expression),
            Cases = switch_.Sections.Select(ConvertSwitchSection).ToList()
        };
    }

    private SwitchCase ConvertSwitchSection(SwitchSectionSyntax section)
    {
        var patterns = new List<ExpressionNode>();
        foreach (var label in section.Labels)
        {
            if (label is CaseSwitchLabelSyntax caseLabel)
            {
                patterns.Add(ConvertExpression(caseLabel.Value));
            }
            else if (label is CasePatternSwitchLabelSyntax patternLabel)
            {
                patterns.Add(ConvertPatternAsExpression(patternLabel.Pattern));
            }
            // DefaultSwitchLabelSyntax has no pattern (empty patterns = default)
        }

        return new SwitchCase
        {
            NodeKind = "SwitchCase",
            Language = LanguageName,
            Span = ConvertSpan(section.Span),
            RawSource = section.ToFullString(),
            Patterns = patterns,
            Body = section.Statements.Select(ConvertStatement).ToList()
        };
    }

    private TryStatement ConvertTry(TryStatementSyntax try_)
    {
        return new TryStatement
        {
            NodeKind = "TryStatement",
            Language = LanguageName,
            Span = ConvertSpan(try_.Span),
            RawSource = try_.ToFullString(),
            TryBlock = ConvertBlock(try_.Block),
            CatchClauses = try_.Catches.Select(ConvertCatch).ToList(),
            FinallyBlock = try_.Finally != null ? ConvertBlock(try_.Finally.Block) : null
        };
    }

    private CatchClause ConvertCatch(CatchClauseSyntax catch_)
    {
        return new CatchClause
        {
            NodeKind = "CatchClause",
            Language = LanguageName,
            Span = ConvertSpan(catch_.Span),
            RawSource = catch_.ToFullString(),
            ExceptionVariable = catch_.Declaration?.Identifier.Text,
            ExceptionTypes = catch_.Declaration != null
                ? [ConvertType(catch_.Declaration.Type)]
                : [],
            Body = ConvertBlock(catch_.Block)
        };
    }

    private ReturnStatement ConvertReturn(ReturnStatementSyntax return_)
    {
        return new ReturnStatement
        {
            NodeKind = "ReturnStatement",
            Language = LanguageName,
            Span = ConvertSpan(return_.Span),
            RawSource = return_.ToFullString(),
            Value = return_.Expression != null ? ConvertExpression(return_.Expression) : null
        };
    }

    private BreakStatement ConvertBreak(BreakStatementSyntax break_)
    {
        return new BreakStatement
        {
            NodeKind = "BreakStatement",
            Language = LanguageName,
            Span = ConvertSpan(break_.Span),
            RawSource = break_.ToFullString()
        };
    }

    private ContinueStatement ConvertContinue(ContinueStatementSyntax continue_)
    {
        return new ContinueStatement
        {
            NodeKind = "ContinueStatement",
            Language = LanguageName,
            Span = ConvertSpan(continue_.Span),
            RawSource = continue_.ToFullString()
        };
    }

    private ThrowStatement ConvertThrow(ThrowStatementSyntax throw_)
    {
        return new ThrowStatement
        {
            NodeKind = "ThrowStatement",
            Language = LanguageName,
            Span = ConvertSpan(throw_.Span),
            RawSource = throw_.ToFullString(),
            Expression = throw_.Expression != null ? ConvertExpression(throw_.Expression) : null
        };
    }

    private StatementNode ConvertUsingStatement(UsingStatementSyntax using_)
    {
        ExpressionNode resource;
        if (using_.Declaration != null)
        {
            var firstVar = using_.Declaration.Variables.First();
            resource = firstVar.Initializer != null
                ? ConvertExpression(firstVar.Initializer.Value)
                : new IdentifierExpression
                {
                    NodeKind = "IdentifierExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(firstVar.Span),
                    Name = firstVar.Identifier.Text
                };
        }
        else
        {
            resource = ConvertExpression(using_.Expression!);
        }

        return new UsingStatement
        {
            NodeKind = "UsingStatement",
            Language = LanguageName,
            Span = ConvertSpan(using_.Span),
            RawSource = using_.ToFullString(),
            Resource = resource,
            Body = ConvertStatement(using_.Statement)
        };
    }

    private StatementNode ConvertLock(LockStatementSyntax lock_)
    {
        return new LockStatement
        {
            NodeKind = "LockStatement",
            Language = LanguageName,
            Span = ConvertSpan(lock_.Span),
            RawSource = lock_.ToFullString(),
            LockObject = ConvertExpression(lock_.Expression),
            Body = ConvertStatement(lock_.Statement)
        };
    }

    private StatementNode ConvertYield(YieldStatementSyntax yield)
    {
        return new YieldStatement
        {
            NodeKind = "YieldStatement",
            Language = LanguageName,
            Span = ConvertSpan(yield.Span),
            RawSource = yield.ToFullString(),
            Value = yield.Expression != null ? ConvertExpression(yield.Expression) : null,
            IsBreak = yield.ReturnOrBreakKeyword.IsKind(SyntaxKind.BreakKeyword)
        };
    }

    private StatementNode ConvertGoto(GotoStatementSyntax goto_)
    {
        return new GotoStatement
        {
            NodeKind = "GotoStatement",
            Language = LanguageName,
            Span = ConvertSpan(goto_.Span),
            RawSource = goto_.ToFullString(),
            Label = goto_.Expression?.ToString() ?? goto_.CaseOrDefaultKeyword.Text
        };
    }

    private StatementNode ConvertLabeled(LabeledStatementSyntax labeled)
    {
        return new LabeledStatement
        {
            NodeKind = "LabeledStatement",
            Language = LanguageName,
            Span = ConvertSpan(labeled.Span),
            RawSource = labeled.ToFullString(),
            Label = labeled.Identifier.Text,
            Statement = ConvertStatement(labeled.Statement)
        };
    }

    private StatementNode ConvertChecked(CheckedStatementSyntax checked_)
    {
        return new CheckedStatement
        {
            NodeKind = "CheckedStatement",
            Language = LanguageName,
            Span = ConvertSpan(checked_.Span),
            RawSource = checked_.ToFullString(),
            IsChecked = checked_.Keyword.IsKind(SyntaxKind.CheckedKeyword),
            Body = ConvertBlock(checked_.Block)
        };
    }

    private StatementNode ConvertUnsafe(UnsafeStatementSyntax unsafe_)
    {
        return new UnsafeStatement
        {
            NodeKind = "UnsafeStatement",
            Language = LanguageName,
            Span = ConvertSpan(unsafe_.Span),
            RawSource = unsafe_.ToFullString(),
            Body = ConvertBlock(unsafe_.Block)
        };
    }

    private StatementNode ConvertFixed(FixedStatementSyntax fixed_)
    {
        return new FixedStatement
        {
            NodeKind = "FixedStatement",
            Language = LanguageName,
            Span = ConvertSpan(fixed_.Span),
            RawSource = fixed_.ToFullString(),
            Variables = fixed_.Declaration.Variables.Select(v =>
                v.Initializer != null
                    ? ConvertExpression(v.Initializer.Value)
                    : (ExpressionNode)new IdentifierExpression
                    {
                        NodeKind = "IdentifierExpression",
                        Language = LanguageName,
                        Span = ConvertSpan(v.Span),
                        Name = v.Identifier.Text
                    }
            ).ToList(),
            Body = ConvertStatement(fixed_.Statement)
        };
    }

    private FunctionDeclaration ConvertLocalFunction(LocalFunctionStatementSyntax localFunc)
    {
        return new FunctionDeclaration
        {
            NodeKind = "FunctionDeclaration",
            Language = LanguageName,
            Span = ConvertSpan(localFunc.Span),
            RawSource = localFunc.ToFullString(),
            Name = localFunc.Identifier.Text,
            Parameters = localFunc.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            ReturnType = ConvertType(localFunc.ReturnType),
            Body = localFunc.Body != null
                ? ConvertBlock(localFunc.Body)
                : localFunc.ExpressionBody != null
                    ? ConvertExpressionBody(localFunc.ExpressionBody)
                    : null,
            IsAsync = localFunc.Modifiers.Any(SyntaxKind.AsyncKeyword),
            TypeParameters = localFunc.TypeParameterList?.Parameters
                .Select(ConvertTypeParameter)
                .ToList() ?? [],
            Attributes = ConvertAttributeLists(localFunc.AttributeLists),
            Extensions = new Dictionary<string, object>
            {
                ["IsLocalFunction"] = true
            }
        };
    }

    private StatementNode ConvertUnknownStatement(StatementSyntax stmt)
    {
        return new ExpressionStatement
        {
            NodeKind = "ExpressionStatement",
            Language = LanguageName,
            Span = ConvertSpan(stmt.Span),
            RawSource = stmt.ToFullString(),
            Expression = new UnknownExpression
            {
                NodeKind = "UnknownExpression",
                Language = LanguageName,
                Span = ConvertSpan(stmt.Span),
                RawSource = stmt.ToFullString(),
                NativeNodeType = stmt.GetType().Name,
                Roles = ["statement"]
            }
        };
    }

    #endregion

    #region Expression Conversion

    private ExpressionNode ConvertExpression(ExpressionSyntax expr)
    {
        return expr switch
        {
            LiteralExpressionSyntax lit => ConvertLiteral(lit),
            IdentifierNameSyntax id => ConvertIdentifier(id),
            BinaryExpressionSyntax bin => ConvertBinary(bin),
            PrefixUnaryExpressionSyntax pre => ConvertPrefixUnary(pre),
            PostfixUnaryExpressionSyntax post => ConvertPostfixUnary(post),
            AssignmentExpressionSyntax assign => ConvertAssignment(assign),
            InvocationExpressionSyntax inv => ConvertInvocation(inv),
            MemberAccessExpressionSyntax ma => ConvertMemberAccess(ma),
            ElementAccessExpressionSyntax ea => ConvertElementAccess(ea),
            ObjectCreationExpressionSyntax oc => ConvertObjectCreation(oc),
            ImplicitObjectCreationExpressionSyntax ioc => ConvertImplicitObjectCreation(ioc),
            ConditionalExpressionSyntax cond => ConvertConditional(cond),
            ConditionalAccessExpressionSyntax condAccess => ConvertConditionalAccess(condAccess),
            ParenthesizedLambdaExpressionSyntax pLambda => ConvertLambda(pLambda),
            SimpleLambdaExpressionSyntax sLambda => ConvertSimpleLambda(sLambda),
            ArrayCreationExpressionSyntax arr => ConvertArrayCreation(arr),
            ImplicitArrayCreationExpressionSyntax iArr => ConvertImplicitArrayCreation(iArr),
            AwaitExpressionSyntax await_ => ConvertAwait(await_),
            ParenthesizedExpressionSyntax paren => ConvertParenthesized(paren),
            CastExpressionSyntax cast => ConvertCast(cast),
            TypeOfExpressionSyntax typeof_ => ConvertTypeOf(typeof_),
            SizeOfExpressionSyntax sizeof_ => ConvertSizeOf(sizeof_),
            DefaultExpressionSyntax default_ => ConvertDefault(default_),
            CheckedExpressionSyntax checked_ => ConvertCheckedExpr(checked_),
            ThrowExpressionSyntax throw_ => ConvertThrowExpr(throw_),
            AnonymousObjectCreationExpressionSyntax anon => ConvertAnonymousObject(anon),
            InitializerExpressionSyntax init => ConvertInitializer(init),
            TupleExpressionSyntax tuple => ConvertTuple(tuple),
            SwitchExpressionSyntax switchExpr => ConvertSwitchExpression(switchExpr),
            IsPatternExpressionSyntax isPattern => ConvertIsPattern(isPattern),
            InterpolatedStringExpressionSyntax interp => ConvertInterpolatedString(interp),
            RangeExpressionSyntax range => ConvertRange(range),
            RefExpressionSyntax ref_ => ConvertRef(ref_),
            QueryExpressionSyntax query => ConvertQuery(query),
            WithExpressionSyntax withExpr => ConvertWithExpression(withExpr),
            CollectionExpressionSyntax collExpr => ConvertCollectionExpression(collExpr),
            ThisExpressionSyntax => new IdentifierExpression
            {
                NodeKind = "IdentifierExpression",
                Language = LanguageName,
                Span = ConvertSpan(expr.Span),
                RawSource = expr.ToFullString(),
                Name = "this"
            },
            BaseExpressionSyntax => new IdentifierExpression
            {
                NodeKind = "IdentifierExpression",
                Language = LanguageName,
                Span = ConvertSpan(expr.Span),
                RawSource = expr.ToFullString(),
                Name = "base"
            },
            GenericNameSyntax gen => ConvertGenericName(gen),
            QualifiedNameSyntax qual => ConvertQualifiedName(qual),
            AliasQualifiedNameSyntax alias => ConvertAliasQualifiedName(alias),
            PredefinedTypeSyntax predef => ConvertPredefinedType(predef),
            NullableTypeSyntax nullable => ConvertNullableType(nullable),
            ArrayTypeSyntax arrayType => ConvertArrayType(arrayType),
            TypeSyntax type => ConvertTypeAsExpression(type),
            _ => ConvertUnknownExpression(expr)
        };
    }

    private ExpressionNode ConvertLiteral(LiteralExpressionSyntax lit)
    {
        // Check for raw string literals (C# 11+): """content""" or $"""content"""
        var tokenText = lit.Token.Text;
        if (lit.Kind() == SyntaxKind.StringLiteralExpression && tokenText.StartsWith("\"\"\""))
        {
            // Count the number of opening quotes
            int quoteCount = 0;
            for (int i = 0; i < tokenText.Length && tokenText[i] == '"'; i++)
                quoteCount++;

            return new CsRawStringLiteral
            {
                NodeKind = "CsRawStringLiteral",
                Language = LanguageName,
                Span = ConvertSpan(lit.Span),
                RawSource = lit.ToFullString(),
                Content = lit.Token.Value?.ToString() ?? "",
                QuoteCount = quoteCount,
                IsInterpolated = false
            };
        }

        // Check for UTF-8 string literals (C# 11+): "text"u8
        if (lit.Kind() == SyntaxKind.Utf8StringLiteralExpression)
        {
            return new LiteralExpression
            {
                NodeKind = "LiteralExpression",
                Language = LanguageName,
                Span = ConvertSpan(lit.Span),
                RawSource = lit.ToFullString(),
                Value = lit.Token.Value,
                Kind = LiteralKind.String,
                RawText = lit.Token.Text,
                Extensions = new Dictionary<string, object> { ["IsUtf8"] = true }
            };
        }

        var kind = lit.Kind() switch
        {
            SyntaxKind.NumericLiteralExpression => lit.Token.Value is int or long or byte or short
                ? LiteralKind.Integer
                : LiteralKind.Float,
            SyntaxKind.StringLiteralExpression => LiteralKind.String,
            SyntaxKind.CharacterLiteralExpression => LiteralKind.Char,
            SyntaxKind.TrueLiteralExpression or SyntaxKind.FalseLiteralExpression => LiteralKind.Boolean,
            SyntaxKind.NullLiteralExpression => LiteralKind.Null,
            SyntaxKind.DefaultLiteralExpression => LiteralKind.Null,
            _ => LiteralKind.String
        };

        return new LiteralExpression
        {
            NodeKind = "LiteralExpression",
            Language = LanguageName,
            Span = ConvertSpan(lit.Span),
            RawSource = lit.ToFullString(),
            Value = lit.Token.Value,
            Kind = kind,
            RawText = lit.Token.Text
        };
    }

    private IdentifierExpression ConvertIdentifier(IdentifierNameSyntax id)
    {
        return new IdentifierExpression
        {
            NodeKind = "IdentifierExpression",
            Language = LanguageName,
            Span = ConvertSpan(id.Span),
            RawSource = id.ToFullString(),
            Name = id.Identifier.Text
        };
    }

    private ExpressionNode ConvertBinary(BinaryExpressionSyntax bin)
    {
        // Handle 'is' type check as CsIsPatternExpression (e.g., "o is string")
        if (bin.Kind() == SyntaxKind.IsExpression)
        {
            return new CsIsPatternExpression
            {
                NodeKind = "CsIsPatternExpression",
                Language = LanguageName,
                Span = ConvertSpan(bin.Span),
                RawSource = bin.ToFullString(),
                Expression = ConvertExpression(bin.Left),
                Pattern = new CsTypePattern
                {
                    NodeKind = "CsTypePattern",
                    Language = LanguageName,
                    Span = ConvertSpan(bin.Right.Span),
                    RawSource = bin.Right.ToFullString(),
                    Type = ConvertType(bin.Right as TypeSyntax ?? SyntaxFactory.ParseTypeName(bin.Right.ToString()))
                }
            };
        }

        return new BinaryExpression
        {
            NodeKind = "BinaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(bin.Span),
            RawSource = bin.ToFullString(),
            Left = ConvertExpression(bin.Left),
            Operator = ConvertBinaryOperator(bin.Kind()),
            Right = ConvertExpression(bin.Right)
        };
    }

    private BinaryOperator ConvertBinaryOperator(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.AddExpression => BinaryOperator.Add,
            SyntaxKind.SubtractExpression => BinaryOperator.Subtract,
            SyntaxKind.MultiplyExpression => BinaryOperator.Multiply,
            SyntaxKind.DivideExpression => BinaryOperator.Divide,
            SyntaxKind.ModuloExpression => BinaryOperator.Modulo,
            SyntaxKind.EqualsExpression => BinaryOperator.Equal,
            SyntaxKind.NotEqualsExpression => BinaryOperator.NotEqual,
            SyntaxKind.LessThanExpression => BinaryOperator.LessThan,
            SyntaxKind.LessThanOrEqualExpression => BinaryOperator.LessOrEqual,
            SyntaxKind.GreaterThanExpression => BinaryOperator.GreaterThan,
            SyntaxKind.GreaterThanOrEqualExpression => BinaryOperator.GreaterOrEqual,
            SyntaxKind.LogicalAndExpression => BinaryOperator.And,
            SyntaxKind.LogicalOrExpression => BinaryOperator.Or,
            SyntaxKind.BitwiseAndExpression => BinaryOperator.BitwiseAnd,
            SyntaxKind.BitwiseOrExpression => BinaryOperator.BitwiseOr,
            SyntaxKind.ExclusiveOrExpression => BinaryOperator.BitwiseXor,
            SyntaxKind.LeftShiftExpression => BinaryOperator.LeftShift,
            SyntaxKind.RightShiftExpression => BinaryOperator.RightShift,
            SyntaxKind.UnsignedRightShiftExpression => BinaryOperator.RightShift,
            SyntaxKind.CoalesceExpression => BinaryOperator.Coalesce,
            SyntaxKind.AsExpression => BinaryOperator.As,
            SyntaxKind.IsExpression => BinaryOperator.Is,
            _ => BinaryOperator.Equal
        };
    }

    private UnaryExpression ConvertPrefixUnary(PrefixUnaryExpressionSyntax pre)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(pre.Span),
            RawSource = pre.ToFullString(),
            Operator = ConvertUnaryOperator(pre.Kind()),
            Operand = ConvertExpression(pre.Operand),
            IsPrefix = true
        };
    }

    private UnaryExpression ConvertPostfixUnary(PostfixUnaryExpressionSyntax post)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(post.Span),
            RawSource = post.ToFullString(),
            Operator = ConvertUnaryOperator(post.Kind()),
            Operand = ConvertExpression(post.Operand),
            IsPrefix = false
        };
    }

    private UnaryOperator ConvertUnaryOperator(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.UnaryMinusExpression => UnaryOperator.Negate,
            SyntaxKind.UnaryPlusExpression => UnaryOperator.Negate, // Unary plus is essentially a no-op
            SyntaxKind.LogicalNotExpression => UnaryOperator.Not,
            SyntaxKind.BitwiseNotExpression => UnaryOperator.BitwiseNot,
            SyntaxKind.PreIncrementExpression or SyntaxKind.PostIncrementExpression => UnaryOperator.Increment,
            SyntaxKind.PreDecrementExpression or SyntaxKind.PostDecrementExpression => UnaryOperator.Decrement,
            SyntaxKind.AddressOfExpression => UnaryOperator.Reference,
            SyntaxKind.PointerIndirectionExpression => UnaryOperator.Dereference,
            SyntaxKind.IndexExpression => UnaryOperator.Negate,
            _ => UnaryOperator.Not
        };
    }

    private AssignmentExpression ConvertAssignment(AssignmentExpressionSyntax assign)
    {
        return new AssignmentExpression
        {
            NodeKind = "AssignmentExpression",
            Language = LanguageName,
            Span = ConvertSpan(assign.Span),
            RawSource = assign.ToFullString(),
            Target = ConvertExpression(assign.Left),
            Operator = ConvertAssignmentOperator(assign.Kind()),
            Value = ConvertExpression(assign.Right)
        };
    }

    private AssignmentOperator ConvertAssignmentOperator(SyntaxKind kind)
    {
        return kind switch
        {
            SyntaxKind.SimpleAssignmentExpression => AssignmentOperator.Assign,
            SyntaxKind.AddAssignmentExpression => AssignmentOperator.AddAssign,
            SyntaxKind.SubtractAssignmentExpression => AssignmentOperator.SubtractAssign,
            SyntaxKind.MultiplyAssignmentExpression => AssignmentOperator.MultiplyAssign,
            SyntaxKind.DivideAssignmentExpression => AssignmentOperator.DivideAssign,
            SyntaxKind.ModuloAssignmentExpression => AssignmentOperator.ModuloAssign,
            SyntaxKind.AndAssignmentExpression => AssignmentOperator.AndAssign,
            SyntaxKind.OrAssignmentExpression => AssignmentOperator.OrAssign,
            SyntaxKind.ExclusiveOrAssignmentExpression => AssignmentOperator.XorAssign,
            SyntaxKind.CoalesceAssignmentExpression => AssignmentOperator.CoalesceAssign,
            _ => AssignmentOperator.Assign
        };
    }

    private ExpressionNode ConvertInvocation(InvocationExpressionSyntax inv)
    {
        // Check for nameof expression (C# 6+, extended in C# 11+)
        if (inv.Expression is IdentifierNameSyntax id && id.Identifier.Text == "nameof" && inv.ArgumentList.Arguments.Count == 1)
        {
            return new CsNameofExpression
            {
                NodeKind = "CsNameofExpression",
                Language = LanguageName,
                Span = ConvertSpan(inv.Span),
                RawSource = inv.ToFullString(),
                Argument = ConvertExpression(inv.ArgumentList.Arguments[0].Expression),
                // The resolved name will be the string value at compile time
                ResolvedName = inv.ArgumentList.Arguments[0].Expression.ToString()
            };
        }

        return new CallExpression
        {
            NodeKind = "CallExpression",
            Language = LanguageName,
            Span = ConvertSpan(inv.Span),
            RawSource = inv.ToFullString(),
            Callee = ConvertExpression(inv.Expression),
            Arguments = inv.ArgumentList.Arguments.Select(ConvertArgument).ToList()
        };
    }

    private ArgumentNode ConvertArgument(ArgumentSyntax arg)
    {
        return new ArgumentNode
        {
            NodeKind = "ArgumentNode",
            Language = LanguageName,
            Span = ConvertSpan(arg.Span),
            RawSource = arg.ToFullString(),
            Name = arg.NameColon?.Name.Identifier.Text,
            Value = ConvertExpression(arg.Expression),
            Extensions = arg.RefOrOutKeyword.IsKind(SyntaxKind.None)
                ? null
                : new Dictionary<string, object>
                {
                    ["RefKind"] = arg.RefOrOutKeyword.Text
                }
        };
    }

    private MemberExpression ConvertMemberAccess(MemberAccessExpressionSyntax ma)
    {
        return new MemberExpression
        {
            NodeKind = "MemberExpression",
            Language = LanguageName,
            Span = ConvertSpan(ma.Span),
            RawSource = ma.ToFullString(),
            Object = ConvertExpression(ma.Expression),
            Member = ma.Name.Identifier.Text,
            IsNullSafe = false
        };
    }

    private IndexExpression ConvertElementAccess(ElementAccessExpressionSyntax ea)
    {
        // Multiple indices become a tuple expression
        ExpressionNode index;
        if (ea.ArgumentList.Arguments.Count == 1)
        {
            index = ConvertExpression(ea.ArgumentList.Arguments[0].Expression);
        }
        else
        {
            index = new TupleExpression
            {
                NodeKind = "TupleExpression",
                Language = LanguageName,
                Span = ConvertSpan(ea.ArgumentList.Span),
                Elements = ea.ArgumentList.Arguments.Select(a => ConvertExpression(a.Expression)).ToList()
            };
        }

        return new IndexExpression
        {
            NodeKind = "IndexExpression",
            Language = LanguageName,
            Span = ConvertSpan(ea.Span),
            RawSource = ea.ToFullString(),
            Object = ConvertExpression(ea.Expression),
            Index = index
        };
    }

    private NewExpression ConvertObjectCreation(ObjectCreationExpressionSyntax oc)
    {
        return new NewExpression
        {
            NodeKind = "NewExpression",
            Language = LanguageName,
            Span = ConvertSpan(oc.Span),
            RawSource = oc.ToFullString(),
            Type = ConvertType(oc.Type),
            Arguments = oc.ArgumentList?.Arguments.Select(ConvertArgument).ToList() ?? [],
            Initializer = oc.Initializer != null
                ? new ObjectExpression
                {
                    NodeKind = "ObjectExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(oc.Initializer.Span),
                    Properties = oc.Initializer.Expressions.Select(ConvertInitializerExpression).ToList()
                }
                : null
        };
    }

    private NewExpression ConvertImplicitObjectCreation(ImplicitObjectCreationExpressionSyntax ioc)
    {
        return new NewExpression
        {
            NodeKind = "NewExpression",
            Language = LanguageName,
            Span = ConvertSpan(ioc.Span),
            RawSource = ioc.ToFullString(),
            Type = new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(ioc.Span),
                Name = "var" // Implicit type
            },
            Arguments = ioc.ArgumentList?.Arguments.Select(ConvertArgument).ToList() ?? [],
            Initializer = ioc.Initializer != null
                ? new ObjectExpression
                {
                    NodeKind = "ObjectExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(ioc.Initializer.Span),
                    Properties = ioc.Initializer.Expressions.Select(ConvertInitializerExpression).ToList()
                }
                : null
        };
    }

    private PropertyNode ConvertInitializerExpression(ExpressionSyntax expr)
    {
        if (expr is AssignmentExpressionSyntax assign)
        {
            return new PropertyNode
            {
                NodeKind = "PropertyNode",
                Language = LanguageName,
                Span = ConvertSpan(expr.Span),
                RawSource = expr.ToFullString(),
                Key = ConvertExpression(assign.Left),
                Value = ConvertExpression(assign.Right)
            };
        }

        return new PropertyNode
        {
            NodeKind = "PropertyNode",
            Language = LanguageName,
            Span = ConvertSpan(expr.Span),
            RawSource = expr.ToFullString(),
            Key = ConvertExpression(expr),
            Value = ConvertExpression(expr)
        };
    }

    private ConditionalExpression ConvertConditional(ConditionalExpressionSyntax cond)
    {
        return new ConditionalExpression
        {
            NodeKind = "ConditionalExpression",
            Language = LanguageName,
            Span = ConvertSpan(cond.Span),
            RawSource = cond.ToFullString(),
            Condition = ConvertExpression(cond.Condition),
            ThenExpression = ConvertExpression(cond.WhenTrue),
            ElseExpression = ConvertExpression(cond.WhenFalse)
        };
    }

    private ExpressionNode ConvertConditionalAccess(ConditionalAccessExpressionSyntax condAccess)
    {
        var inner = condAccess.WhenNotNull;
        if (inner is MemberBindingExpressionSyntax memberBinding)
        {
            return new MemberExpression
            {
                NodeKind = "MemberExpression",
                Language = LanguageName,
                Span = ConvertSpan(condAccess.Span),
                RawSource = condAccess.ToFullString(),
                Object = ConvertExpression(condAccess.Expression),
                Member = memberBinding.Name.Identifier.Text,
                IsNullSafe = true
            };
        }
        else if (inner is ElementBindingExpressionSyntax elementBinding)
        {
            return new IndexExpression
            {
                NodeKind = "IndexExpression",
                Language = LanguageName,
                Span = ConvertSpan(condAccess.Span),
                RawSource = condAccess.ToFullString(),
                Object = ConvertExpression(condAccess.Expression),
                Index = elementBinding.ArgumentList.Arguments.Count == 1
                    ? ConvertExpression(elementBinding.ArgumentList.Arguments[0].Expression)
                    : new TupleExpression
                    {
                        NodeKind = "TupleExpression",
                        Language = LanguageName,
                        Span = ConvertSpan(elementBinding.Span),
                        Elements = elementBinding.ArgumentList.Arguments.Select(a => ConvertExpression(a.Expression)).ToList()
                    },
                IsNullSafe = true
            };
        }
        else if (inner is InvocationExpressionSyntax invocation)
        {
            // Handle null-conditional method invocation: obj?.Method()
            // The invocation.Expression can be MemberBindingExpressionSyntax or MemberAccessExpressionSyntax
            string methodName;
            if (invocation.Expression is MemberBindingExpressionSyntax memberBindingInvoke)
            {
                methodName = memberBindingInvoke.Name.Identifier.Text;
            }
            else if (invocation.Expression is MemberAccessExpressionSyntax memberAccessInvoke)
            {
                methodName = memberAccessInvoke.Name.Identifier.Text;
            }
            else
            {
                // Fallback: extract name from the expression string
                methodName = invocation.Expression.ToString();
            }

            return new InvokeMemberExpression
            {
                NodeKind = "InvokeMemberExpression",
                Language = LanguageName,
                Span = ConvertSpan(condAccess.Span),
                RawSource = condAccess.ToFullString(),
                Object = ConvertExpression(condAccess.Expression),
                MethodName = methodName,
                Arguments = invocation.ArgumentList.Arguments.Select(ConvertArgument).ToList(),
                IsNullSafe = true
            };
        }

        return ConvertExpression(inner);
    }

    private LambdaExpression ConvertLambda(ParenthesizedLambdaExpressionSyntax lambda)
    {
        return new LambdaExpression
        {
            NodeKind = "LambdaExpression",
            Language = LanguageName,
            Span = ConvertSpan(lambda.Span),
            RawSource = lambda.ToFullString(),
            Parameters = lambda.ParameterList.Parameters.Select(ConvertParameter).ToList(),
            Body = lambda.Block != null
                ? ConvertBlock(lambda.Block)
                : ConvertExpression(lambda.ExpressionBody!),
            IsAsync = lambda.AsyncKeyword.IsKind(SyntaxKind.AsyncKeyword)
        };
    }

    private LambdaExpression ConvertSimpleLambda(SimpleLambdaExpressionSyntax lambda)
    {
        return new LambdaExpression
        {
            NodeKind = "LambdaExpression",
            Language = LanguageName,
            Span = ConvertSpan(lambda.Span),
            RawSource = lambda.ToFullString(),
            Parameters =
            [
                new ParameterNode
                {
                    NodeKind = "ParameterNode",
                    Language = LanguageName,
                    Span = ConvertSpan(lambda.Parameter.Span),
                    Name = lambda.Parameter.Identifier.Text
                }
            ],
            Body = lambda.Block != null
                ? ConvertBlock(lambda.Block)
                : ConvertExpression(lambda.ExpressionBody!),
            IsAsync = lambda.AsyncKeyword.IsKind(SyntaxKind.AsyncKeyword)
        };
    }

    private ArrayExpression ConvertArrayCreation(ArrayCreationExpressionSyntax arr)
    {
        return new ArrayExpression
        {
            NodeKind = "ArrayExpression",
            Language = LanguageName,
            Span = ConvertSpan(arr.Span),
            RawSource = arr.ToFullString(),
            Elements = arr.Initializer?.Expressions.Select(ConvertExpression).ToList() ?? [],
            Extensions = new Dictionary<string, object>
            {
                ["ElementType"] = arr.Type.ElementType.ToString()
            }
        };
    }

    private ArrayExpression ConvertImplicitArrayCreation(ImplicitArrayCreationExpressionSyntax iArr)
    {
        return new ArrayExpression
        {
            NodeKind = "ArrayExpression",
            Language = LanguageName,
            Span = ConvertSpan(iArr.Span),
            RawSource = iArr.ToFullString(),
            Elements = iArr.Initializer.Expressions.Select(ConvertExpression).ToList()
        };
    }

    private UnaryExpression ConvertAwait(AwaitExpressionSyntax await_)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(await_.Span),
            RawSource = await_.ToFullString(),
            Operator = UnaryOperator.Await,
            Operand = ConvertExpression(await_.Expression),
            IsPrefix = true
        };
    }

    private ParenthesizedExpression ConvertParenthesized(ParenthesizedExpressionSyntax paren)
    {
        return new ParenthesizedExpression
        {
            NodeKind = "ParenthesizedExpression",
            Language = LanguageName,
            Span = ConvertSpan(paren.Span),
            RawSource = paren.ToFullString(),
            Expression = ConvertExpression(paren.Expression)
        };
    }

    private CastExpression ConvertCast(CastExpressionSyntax cast)
    {
        return new CastExpression
        {
            NodeKind = "CastExpression",
            Language = LanguageName,
            Span = ConvertSpan(cast.Span),
            RawSource = cast.ToFullString(),
            Type = ConvertType(cast.Type),
            Expression = ConvertExpression(cast.Expression)
        };
    }

    private UnaryExpression ConvertTypeOf(TypeOfExpressionSyntax typeof_)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(typeof_.Span),
            RawSource = typeof_.ToFullString(),
            Operator = UnaryOperator.Typeof,
            Operand = new TypeExpression
            {
                NodeKind = "TypeExpression",
                Language = LanguageName,
                Span = ConvertSpan(typeof_.Type.Span),
                Type = ConvertType(typeof_.Type)
            },
            IsPrefix = true
        };
    }

    private UnaryExpression ConvertSizeOf(SizeOfExpressionSyntax sizeof_)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(sizeof_.Span),
            RawSource = sizeof_.ToFullString(),
            Operator = UnaryOperator.Sizeof,
            Operand = new TypeExpression
            {
                NodeKind = "TypeExpression",
                Language = LanguageName,
                Span = ConvertSpan(sizeof_.Type.Span),
                Type = ConvertType(sizeof_.Type)
            },
            IsPrefix = true
        };
    }

    private LiteralExpression ConvertDefault(DefaultExpressionSyntax default_)
    {
        return new LiteralExpression
        {
            NodeKind = "LiteralExpression",
            Language = LanguageName,
            Span = ConvertSpan(default_.Span),
            RawSource = default_.ToFullString(),
            Value = null,
            Kind = LiteralKind.Null,
            RawText = default_.ToFullString()
        };
    }

    private ExpressionNode ConvertCheckedExpr(CheckedExpressionSyntax checked_)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = ConvertSpan(checked_.Span),
            RawSource = checked_.ToFullString(),
            NativeNodeType = "CheckedExpression",
            Roles = ["expression"],
            ChildNodes = [ConvertExpression(checked_.Expression)]
        };
    }

    private ExpressionNode ConvertThrowExpr(ThrowExpressionSyntax throw_)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = ConvertSpan(throw_.Span),
            RawSource = throw_.ToFullString(),
            NativeNodeType = "ThrowExpression",
            Roles = ["expression", "exception"],
            ChildNodes = [ConvertExpression(throw_.Expression)]
        };
    }

    private ObjectExpression ConvertAnonymousObject(AnonymousObjectCreationExpressionSyntax anon)
    {
        return new ObjectExpression
        {
            NodeKind = "ObjectExpression",
            Language = LanguageName,
            Span = ConvertSpan(anon.Span),
            RawSource = anon.ToFullString(),
            Properties = anon.Initializers.Select(i => new PropertyNode
            {
                NodeKind = "PropertyNode",
                Language = LanguageName,
                Span = ConvertSpan(i.Span),
                Key = new IdentifierExpression
                {
                    NodeKind = "IdentifierExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(i.NameEquals?.Name.Span ?? i.Span),
                    Name = i.NameEquals?.Name.Identifier.Text ?? i.Expression.ToString()
                },
                Value = ConvertExpression(i.Expression)
            }).ToList()
        };
    }

    private ArrayExpression ConvertInitializer(InitializerExpressionSyntax init)
    {
        return new ArrayExpression
        {
            NodeKind = "ArrayExpression",
            Language = LanguageName,
            Span = ConvertSpan(init.Span),
            RawSource = init.ToFullString(),
            Elements = init.Expressions.Select(ConvertExpression).ToList()
        };
    }

    private TupleExpression ConvertTuple(TupleExpressionSyntax tuple)
    {
        return new TupleExpression
        {
            NodeKind = "TupleExpression",
            Language = LanguageName,
            Span = ConvertSpan(tuple.Span),
            RawSource = tuple.ToFullString(),
            Elements = tuple.Arguments.Select(a => ConvertExpression(a.Expression)).ToList()
        };
    }

    private ExpressionNode ConvertSwitchExpression(SwitchExpressionSyntax switchExpr)
    {
        return new CsSwitchExpression
        {
            NodeKind = "CsSwitchExpression",
            Language = LanguageName,
            Span = ConvertSpan(switchExpr.Span),
            RawSource = switchExpr.ToFullString(),
            GoverningExpression = ConvertExpression(switchExpr.GoverningExpression),
            Arms = switchExpr.Arms.Select(ConvertSwitchExpressionArm).ToList()
        };
    }

    private CsSwitchExpressionArm ConvertSwitchExpressionArm(SwitchExpressionArmSyntax arm)
    {
        return new CsSwitchExpressionArm
        {
            NodeKind = "CsSwitchExpressionArm",
            Language = LanguageName,
            Span = ConvertSpan(arm.Span),
            RawSource = arm.ToFullString(),
            Pattern = ConvertPattern(arm.Pattern),
            WhenClause = arm.WhenClause != null ? ConvertExpression(arm.WhenClause.Condition) : null,
            Expression = ConvertExpression(arm.Expression)
        };
    }

    private ExpressionNode ConvertIsPattern(IsPatternExpressionSyntax isPattern)
    {
        return new CsIsPatternExpression
        {
            NodeKind = "CsIsPatternExpression",
            Language = LanguageName,
            Span = ConvertSpan(isPattern.Span),
            RawSource = isPattern.ToFullString(),
            Expression = ConvertExpression(isPattern.Expression),
            Pattern = ConvertPattern(isPattern.Pattern)
        };
    }

    private InterpolatedStringExpression ConvertInterpolatedString(InterpolatedStringExpressionSyntax interp)
    {
        return new InterpolatedStringExpression
        {
            NodeKind = "InterpolatedStringExpression",
            Language = LanguageName,
            Span = ConvertSpan(interp.Span),
            RawSource = interp.ToFullString(),
            Parts = interp.Contents.Select(c => c switch
            {
                InterpolationSyntax interpolation => new InterpolatedStringPart
                {
                    NodeKind = "InterpolatedStringPart",
                    Language = LanguageName,
                    Span = ConvertSpan(interpolation.Span),
                    Expression = ConvertExpression(interpolation.Expression)
                },
                InterpolatedStringTextSyntax text => new InterpolatedStringPart
                {
                    NodeKind = "InterpolatedStringPart",
                    Language = LanguageName,
                    Span = ConvertSpan(text.Span),
                    Expression = new LiteralExpression
                    {
                        NodeKind = "LiteralExpression",
                        Language = LanguageName,
                        Span = ConvertSpan(text.Span),
                        Value = text.TextToken.Text,
                        Kind = LiteralKind.String,
                        RawText = text.TextToken.Text
                    }
                },
                _ => new InterpolatedStringPart
                {
                    NodeKind = "InterpolatedStringPart",
                    Language = LanguageName,
                    Span = ConvertSpan(c.Span),
                    Expression = new LiteralExpression
                    {
                        NodeKind = "LiteralExpression",
                        Language = LanguageName,
                        Span = ConvertSpan(c.Span),
                        Value = c.ToString(),
                        Kind = LiteralKind.String,
                        RawText = c.ToString()
                    }
                }
            }).ToList()
        };
    }

    private BinaryExpression ConvertRange(RangeExpressionSyntax range)
    {
        return new BinaryExpression
        {
            NodeKind = "BinaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(range.Span),
            RawSource = range.ToFullString(),
            Left = range.LeftOperand != null
                ? ConvertExpression(range.LeftOperand)
                : new LiteralExpression
                {
                    NodeKind = "LiteralExpression",
                    Language = LanguageName,
                    Span = SourceSpan.Empty,
                    Value = 0,
                    Kind = LiteralKind.Integer,
                    RawText = "0"
                },
            Operator = BinaryOperator.Range,
            Right = range.RightOperand != null
                ? ConvertExpression(range.RightOperand)
                : new LiteralExpression
                {
                    NodeKind = "LiteralExpression",
                    Language = LanguageName,
                    Span = SourceSpan.Empty,
                    Value = null,
                    Kind = LiteralKind.Null,
                    RawText = "^0"
                }
        };
    }

    private ExpressionNode ConvertRef(RefExpressionSyntax ref_)
    {
        return new UnaryExpression
        {
            NodeKind = "UnaryExpression",
            Language = LanguageName,
            Span = ConvertSpan(ref_.Span),
            RawSource = ref_.ToFullString(),
            Operator = UnaryOperator.Reference,
            Operand = ConvertExpression(ref_.Expression),
            IsPrefix = true
        };
    }

    private ExpressionNode ConvertQuery(QueryExpressionSyntax query)
    {
        return new LinqExpression
        {
            NodeKind = "LinqExpression",
            Language = LanguageName,
            Span = ConvertSpan(query.Span),
            RawSource = query.ToFullString(),
            Source = ConvertExpression(query.FromClause.Expression),
            Clauses = query.Body.Clauses.Select(c => new LinqClause
            {
                NodeKind = "LinqClause",
                Language = LanguageName,
                Span = ConvertSpan(c.Span),
                ClauseType = c.Kind().ToString(),
                Expression = c switch
                {
                    WhereClauseSyntax where => ConvertExpression(where.Condition),
                    OrderByClauseSyntax orderBy => ConvertExpression(orderBy.Orderings.First().Expression),
                    LetClauseSyntax let => ConvertExpression(let.Expression),
                    FromClauseSyntax from => ConvertExpression(from.Expression),
                    JoinClauseSyntax join => ConvertExpression(join.InExpression),
                    _ => new LiteralExpression
                    {
                        NodeKind = "LiteralExpression",
                        Language = LanguageName,
                        Span = ConvertSpan(c.Span),
                        Value = c.ToString(),
                        Kind = LiteralKind.String,
                        RawText = c.ToString()
                    }
                }
            }).ToList(),
            SelectOrGroup = query.Body.SelectOrGroup switch
            {
                SelectClauseSyntax select => ConvertExpression(select.Expression),
                GroupClauseSyntax group => new ObjectExpression
                {
                    NodeKind = "ObjectExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(group.Span),
                    Properties =
                    [
                        new PropertyNode
                        {
                            NodeKind = "PropertyNode",
                            Language = LanguageName,
                            Span = ConvertSpan(group.Span),
                            Key = new IdentifierExpression
                            {
                                NodeKind = "IdentifierExpression",
                                Language = LanguageName,
                                Span = SourceSpan.Empty,
                                Name = "group"
                            },
                            Value = ConvertExpression(group.GroupExpression)
                        },
                        new PropertyNode
                        {
                            NodeKind = "PropertyNode",
                            Language = LanguageName,
                            Span = ConvertSpan(group.Span),
                            Key = new IdentifierExpression
                            {
                                NodeKind = "IdentifierExpression",
                                Language = LanguageName,
                                Span = SourceSpan.Empty,
                                Name = "by"
                            },
                            Value = ConvertExpression(group.ByExpression)
                        }
                    ]
                },
                _ => null
            }
        };
    }

    private ExpressionNode ConvertWithExpression(WithExpressionSyntax withExpr)
    {
        return new CsWithExpression
        {
            NodeKind = "CsWithExpression",
            Language = LanguageName,
            Span = ConvertSpan(withExpr.Span),
            RawSource = withExpr.ToFullString(),
            Expression = ConvertExpression(withExpr.Expression),
            Initializers = withExpr.Initializer.Expressions
                .OfType<AssignmentExpressionSyntax>()
                .Select(a => new CsPropertyInitializer
                {
                    NodeKind = "CsPropertyInitializer",
                    Language = LanguageName,
                    Span = ConvertSpan(a.Span),
                    RawSource = a.ToFullString(),
                    Name = a.Left.ToString(),
                    Value = ConvertExpression(a.Right)
                })
                .ToList()
        };
    }

    private ExpressionNode ConvertCollectionExpression(CollectionExpressionSyntax collExpr)
    {
        return new CsCollectionExpression
        {
            NodeKind = "CsCollectionExpression",
            Language = LanguageName,
            Span = ConvertSpan(collExpr.Span),
            RawSource = collExpr.ToFullString(),
            Elements = collExpr.Elements.Select(ConvertCollectionElement).ToList()
        };
    }

    private CsCollectionElement ConvertCollectionElement(CollectionElementSyntax element)
    {
        return element switch
        {
            ExpressionElementSyntax expr => new CsExpressionElement
            {
                NodeKind = "CsExpressionElement",
                Language = LanguageName,
                Span = ConvertSpan(element.Span),
                RawSource = element.ToFullString(),
                Expression = ConvertExpression(expr.Expression)
            },
            SpreadElementSyntax spread => new CsSpreadElement
            {
                NodeKind = "CsSpreadElement",
                Language = LanguageName,
                Span = ConvertSpan(element.Span),
                RawSource = element.ToFullString(),
                Expression = ConvertExpression(spread.Expression)
            },
            _ => new CsExpressionElement
            {
                NodeKind = "CsExpressionElement",
                Language = LanguageName,
                Span = ConvertSpan(element.Span),
                RawSource = element.ToFullString(),
                Expression = new LiteralExpression
                {
                    NodeKind = "LiteralExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(element.Span),
                    Value = element.ToString(),
                    Kind = LiteralKind.String,
                    RawText = element.ToString()
                }
            }
        };
    }

    private ExpressionNode ConvertGenericName(GenericNameSyntax gen)
    {
        return new TypeExpression
        {
            NodeKind = "TypeExpression",
            Language = LanguageName,
            Span = ConvertSpan(gen.Span),
            RawSource = gen.ToFullString(),
            Type = new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(gen.Span),
                Name = gen.Identifier.Text,
                TypeArguments = gen.TypeArgumentList.Arguments.Select(ConvertType).ToList()
            }
        };
    }

    private ExpressionNode ConvertQualifiedName(QualifiedNameSyntax qual)
    {
        return new MemberExpression
        {
            NodeKind = "MemberExpression",
            Language = LanguageName,
            Span = ConvertSpan(qual.Span),
            RawSource = qual.ToFullString(),
            Object = ConvertExpression(qual.Left),
            Member = qual.Right.Identifier.Text
        };
    }

    private ExpressionNode ConvertAliasQualifiedName(AliasQualifiedNameSyntax alias)
    {
        return new MemberExpression
        {
            NodeKind = "MemberExpression",
            Language = LanguageName,
            Span = ConvertSpan(alias.Span),
            RawSource = alias.ToFullString(),
            Object = new IdentifierExpression
            {
                NodeKind = "IdentifierExpression",
                Language = LanguageName,
                Span = ConvertSpan(alias.Alias.Span),
                Name = alias.Alias.Identifier.Text
            },
            Member = alias.Name.Identifier.Text
        };
    }

    private ExpressionNode ConvertPredefinedType(PredefinedTypeSyntax predef)
    {
        return new TypeExpression
        {
            NodeKind = "TypeExpression",
            Language = LanguageName,
            Span = ConvertSpan(predef.Span),
            RawSource = predef.ToFullString(),
            Type = new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(predef.Span),
                Name = predef.Keyword.Text
            }
        };
    }

    private ExpressionNode ConvertNullableType(NullableTypeSyntax nullable)
    {
        return new TypeExpression
        {
            NodeKind = "TypeExpression",
            Language = LanguageName,
            Span = ConvertSpan(nullable.Span),
            RawSource = nullable.ToFullString(),
            Type = new NullableTypeReference
            {
                NodeKind = "NullableTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(nullable.Span),
                InnerType = ConvertType(nullable.ElementType)
            }
        };
    }

    private ExpressionNode ConvertArrayType(ArrayTypeSyntax arrayType)
    {
        return new TypeExpression
        {
            NodeKind = "TypeExpression",
            Language = LanguageName,
            Span = ConvertSpan(arrayType.Span),
            RawSource = arrayType.ToFullString(),
            Type = new ArrayTypeReference
            {
                NodeKind = "ArrayTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(arrayType.Span),
                ElementType = ConvertType(arrayType.ElementType),
                Dimensions = arrayType.RankSpecifiers.Sum(r => r.Rank)
            }
        };
    }

    private ExpressionNode ConvertTypeAsExpression(TypeSyntax type)
    {
        return new TypeExpression
        {
            NodeKind = "TypeExpression",
            Language = LanguageName,
            Span = ConvertSpan(type.Span),
            RawSource = type.ToFullString(),
            Type = ConvertType(type)
        };
    }

    private ExpressionNode ConvertUnknownExpression(ExpressionSyntax expr)
    {
        return new UnknownExpression
        {
            NodeKind = "UnknownExpression",
            Language = LanguageName,
            Span = ConvertSpan(expr.Span),
            RawSource = expr.ToFullString(),
            NativeNodeType = expr.GetType().Name,
            Roles = ["expression"]
        };
    }

    private CsPatternExpression ConvertPattern(PatternSyntax pattern)
    {
        return pattern switch
        {
            DeclarationPatternSyntax decl => new CsDeclarationPattern
            {
                NodeKind = "CsDeclarationPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Type = ConvertType(decl.Type),
                VariableName = decl.Designation?.ToString() ?? "_"
            },
            TypePatternSyntax typePat => new CsTypePattern
            {
                NodeKind = "CsTypePattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Type = ConvertType(typePat.Type)
            },
            ConstantPatternSyntax constant => new CsConstantPattern
            {
                NodeKind = "CsConstantPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Value = ConvertExpression(constant.Expression)
            },
            VarPatternSyntax var => new CsVarPattern
            {
                NodeKind = "CsVarPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Designation = var.Designation.ToString()
            },
            DiscardPatternSyntax => new CsDiscardPattern
            {
                NodeKind = "CsDiscardPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString()
            },
            RelationalPatternSyntax rel => new CsRelationalPattern
            {
                NodeKind = "CsRelationalPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Operator = rel.OperatorToken.Text,
                Value = ConvertExpression(rel.Expression)
            },
            BinaryPatternSyntax binary => new CsLogicalPattern
            {
                NodeKind = "CsLogicalPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Operator = binary.OperatorToken.Text,
                Left = ConvertPattern(binary.Left),
                Right = ConvertPattern(binary.Right)
            },
            UnaryPatternSyntax unary => new CsLogicalPattern
            {
                NodeKind = "CsLogicalPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Operator = "not",
                Right = ConvertPattern(unary.Pattern)
            },
            RecursivePatternSyntax recursive => ConvertRecursivePattern(recursive),
            ParenthesizedPatternSyntax paren => new CsParenthesizedPattern
            {
                NodeKind = "CsParenthesizedPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Pattern = ConvertPattern(paren.Pattern)
            },
            ListPatternSyntax list => new CsListPattern
            {
                NodeKind = "CsListPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Patterns = list.Patterns.Select(ConvertPattern).ToList(),
                Designation = list.Designation?.ToString()
            },
            SlicePatternSyntax slice => new CsSlicePattern
            {
                NodeKind = "CsSlicePattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Pattern = slice.Pattern != null ? ConvertPattern(slice.Pattern) : null
            },
            _ => new CsConstantPattern
            {
                NodeKind = "CsConstantPattern",
                Language = LanguageName,
                Span = ConvertSpan(pattern.Span),
                RawSource = pattern.ToFullString(),
                Value = new LiteralExpression
                {
                    NodeKind = "LiteralExpression",
                    Language = LanguageName,
                    Span = ConvertSpan(pattern.Span),
                    RawSource = pattern.ToFullString(),
                    Value = pattern.ToString(),
                    Kind = LiteralKind.String,
                    RawText = pattern.ToString()
                }
            }
        };
    }

    private CsPatternExpression ConvertRecursivePattern(RecursivePatternSyntax recursive)
    {
        // Check if it's a positional pattern (has positional subpatterns)
        if (recursive.PositionalPatternClause != null)
        {
            return new CsPositionalPattern
            {
                NodeKind = "CsPositionalPattern",
                Language = LanguageName,
                Span = ConvertSpan(recursive.Span),
                RawSource = recursive.ToFullString(),
                Type = recursive.Type != null ? ConvertType(recursive.Type) : null,
                Subpatterns = recursive.PositionalPatternClause.Subpatterns
                    .Select(s => ConvertPattern(s.Pattern!))
                    .ToList(),
                Designation = recursive.Designation?.ToString()
            };
        }

        // It's a property pattern
        return new CsPropertyPattern
        {
            NodeKind = "CsPropertyPattern",
            Language = LanguageName,
            Span = ConvertSpan(recursive.Span),
            RawSource = recursive.ToFullString(),
            Type = recursive.Type != null ? ConvertType(recursive.Type) : null,
            Subpatterns = recursive.PropertyPatternClause?.Subpatterns
                .Where(s => s.Pattern != null)
                .Select(s => new CsPropertySubpattern
                {
                    NodeKind = "CsPropertySubpattern",
                    Language = LanguageName,
                    Span = ConvertSpan(s.Span),
                    RawSource = s.ToFullString(),
                    Name = s.NameColon?.Name.Identifier.Text ?? s.ExpressionColon?.Expression.ToString() ?? "",
                    Pattern = ConvertPattern(s.Pattern!)
                })
                .ToList() ?? [],
            Designation = recursive.Designation?.ToString()
        };
    }

    private ExpressionNode ConvertPatternAsExpression(PatternSyntax pattern)
    {
        // For backward compatibility with switch case labels
        return ConvertPattern(pattern);
    }

    private ExpressionNode ConvertAttributeAsExpression(AttributeSyntax attr)
    {
        return new CallExpression
        {
            NodeKind = "CallExpression",
            Language = LanguageName,
            Span = ConvertSpan(attr.Span),
            RawSource = attr.ToFullString(),
            Callee = new IdentifierExpression
            {
                NodeKind = "IdentifierExpression",
                Language = LanguageName,
                Span = ConvertSpan(attr.Name.Span),
                Name = attr.Name.ToString()
            },
            Arguments = attr.ArgumentList?.Arguments.Select(a => new ArgumentNode
            {
                NodeKind = "ArgumentNode",
                Language = LanguageName,
                Span = ConvertSpan(a.Span),
                Name = a.NameEquals?.Name.Identifier.Text ?? a.NameColon?.Name.Identifier.Text,
                Value = ConvertExpression(a.Expression)
            }).ToList() ?? []
        };
    }

    #endregion

    #region Type Conversion

    private TypeReference ConvertType(TypeSyntax type)
    {
        return type switch
        {
            PredefinedTypeSyntax predef => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = predef.Keyword.Text
            },
            IdentifierNameSyntax id => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = id.Identifier.Text
            },
            GenericNameSyntax gen => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = gen.Identifier.Text,
                TypeArguments = gen.TypeArgumentList.Arguments.Select(ConvertType).ToList()
            },
            QualifiedNameSyntax qual => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = qual.Right.Identifier.Text,
                Namespace = qual.Left.ToString()
            },
            ArrayTypeSyntax arr => new ArrayTypeReference
            {
                NodeKind = "ArrayTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                ElementType = ConvertType(arr.ElementType),
                Dimensions = arr.RankSpecifiers.Sum(r => r.Rank)
            },
            NullableTypeSyntax nullable => new NullableTypeReference
            {
                NodeKind = "NullableTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                InnerType = ConvertType(nullable.ElementType)
            },
            TupleTypeSyntax tuple => new TupleTypeReference
            {
                NodeKind = "TupleTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                ElementTypes = tuple.Elements.Select(e => ConvertType(e.Type)).ToList()
            },
            PointerTypeSyntax pointer => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = pointer.ToString()
            },
            RefTypeSyntax refType => ConvertType(refType.Type),
            OmittedTypeArgumentSyntax => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = "_"
            },
            _ => new NamedTypeReference
            {
                NodeKind = "NamedTypeReference",
                Language = LanguageName,
                Span = ConvertSpan(type.Span),
                Name = type.ToString()
            }
        };
    }

    #endregion

    #region Helper Methods

    private ParameterNode ConvertParameter(ParameterSyntax param)
    {
        return new ParameterNode
        {
            NodeKind = "ParameterNode",
            Language = LanguageName,
            Span = ConvertSpan(param.Span),
            RawSource = param.ToFullString(),
            Name = param.Identifier.Text,
            Type = param.Type != null ? ConvertType(param.Type) : null,
            DefaultValue = param.Default != null ? ConvertExpression(param.Default.Value) : null,
            IsRest = param.Modifiers.Any(SyntaxKind.ParamsKeyword),
            Attributes = ConvertAttributeLists(param.AttributeLists),
            Extensions = param.Modifiers.Count > 0
                ? new Dictionary<string, object>
                {
                    ["Modifiers"] = param.Modifiers.Select(m => m.Text).ToList()
                }
                : null
        };
    }

    private TypeParameterNode ConvertTypeParameter(TypeParameterSyntax typeParam)
    {
        return new TypeParameterNode
        {
            NodeKind = "TypeParameterNode",
            Language = LanguageName,
            Span = ConvertSpan(typeParam.Span),
            RawSource = typeParam.ToFullString(),
            Name = typeParam.Identifier.Text,
            Variance = typeParam.VarianceKeyword.IsKind(SyntaxKind.InKeyword) ? "in" :
                      typeParam.VarianceKeyword.IsKind(SyntaxKind.OutKeyword) ? "out" : null
        };
    }

    private IReadOnlyList<AttributeNode> ConvertAttributeLists(SyntaxList<AttributeListSyntax> attrLists)
    {
        var attrs = new List<AttributeNode>();
        foreach (var attrList in attrLists)
        {
            foreach (var attr in attrList.Attributes)
            {
                attrs.Add(new AttributeNode
                {
                    NodeKind = "AttributeNode",
                    Language = LanguageName,
                    Span = ConvertSpan(attr.Span),
                    RawSource = attr.ToFullString(),
                    Name = attr.Name.ToString(),
                    Arguments = attr.ArgumentList?.Arguments.Select(a => new ArgumentNode
                    {
                        NodeKind = "ArgumentNode",
                        Language = LanguageName,
                        Span = ConvertSpan(a.Span),
                        Name = a.NameEquals?.Name.Identifier.Text ?? a.NameColon?.Name.Identifier.Text,
                        Value = ConvertExpression(a.Expression)
                    }).ToList() ?? []
                });
            }
        }
        return attrs;
    }

    private Visibility GetVisibility(SyntaxTokenList modifiers)
    {
        if (modifiers.Any(SyntaxKind.PublicKeyword)) return Visibility.Public;
        if (modifiers.Any(SyntaxKind.PrivateKeyword)) return Visibility.Private;
        if (modifiers.Any(SyntaxKind.ProtectedKeyword)) return Visibility.Protected;
        if (modifiers.Any(SyntaxKind.InternalKeyword)) return Visibility.Internal;
        return Visibility.Default;
    }

    private IReadOnlyList<ModifierNode> GetModifiers(SyntaxTokenList modifiers)
    {
        return modifiers
            .Where(m => !IsVisibilityModifier(m.Kind()))
            .Select(m => new ModifierNode
            {
                NodeKind = "ModifierNode",
                Language = LanguageName,
                Span = ConvertSpan(m.Span),
                Keyword = m.Text
            })
            .ToList();
    }

    private static bool IsVisibilityModifier(SyntaxKind kind)
    {
        return kind is SyntaxKind.PublicKeyword or SyntaxKind.PrivateKeyword
            or SyntaxKind.ProtectedKeyword or SyntaxKind.InternalKeyword;
    }

    private SourceSpan ConvertSpan(TextSpan span)
    {
        if (_currentTree == null)
            return SourceSpan.Empty;

        var start = _currentTree.GetLineSpan(span).StartLinePosition;
        var end = _currentTree.GetLineSpan(span).EndLinePosition;

        return new SourceSpan(
            start.Line + 1,      // 0-indexed to 1-indexed
            start.Character,
            end.Line + 1,
            end.Character,
            span.Start,
            span.End
        );
    }

    #endregion
}
