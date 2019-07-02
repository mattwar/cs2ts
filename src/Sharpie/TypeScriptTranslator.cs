using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using E = System.Linq.Expressions;

namespace Sharpie
{
    public class TranslationResult
    {
        public string Text { get; }
        public IReadOnlyList<Diagnostic> Diagnostics { get; }

        public TranslationResult(string text, IReadOnlyList<Diagnostic> diagnostics)
        {
            this.Text = text;
            this.Diagnostics = diagnostics;
        }
    }

    public class TypeScriptTranslator
    {
        public static TranslationResult Translate(CSharpCompilation compilation)
        {
            var writer = new StringWriter();
            var diagnostics = new List<Diagnostic>();
            var translator = new Translator(compilation, writer, diagnostics);

            // TODO: make this organized by symbol table to support unifying partial classes, etc.
            foreach (var tree in compilation.SyntaxTrees)
            {
                translator.Visit(tree.GetRoot());
            }

            return new TranslationResult(writer.ToString(), diagnostics);
        }

        private static TranslationResult Translate(CSharpCompilation compilation, SyntaxNode node)
        {
            var writer = new StringWriter();
            var diagnostics = new List<Diagnostic>();
            var translator = new Translator(compilation, writer, diagnostics);
            translator.Visit(node);
            return new TranslationResult(writer.ToString(), diagnostics);
        }

        public static TranslationResult TranslateCompilationUnit(string csharpText, IEnumerable<MetadataReference> references)
        {
            var tree = CSharpSyntaxTree.ParseText(csharpText);
            var compilation = CSharpCompilation.Create("source", new[] { tree }, references);
            var compUnit = tree.GetRoot();
            return Translate(compilation, compUnit);
        }

        public static TranslationResult TranslateExpression(string csharpExpression, IEnumerable<MetadataReference> references)
        {
            var csharpText = $"using System; public class Test {{ public void M() {{ var v = {csharpExpression}; }} }}";
            var tree = CSharpSyntaxTree.ParseText(csharpText);
            var compilation = CSharpCompilation.Create("source", new[] { tree }, references);
            var localDecl = (LocalDeclarationStatementSyntax)tree.GetRoot().DescendantNodes().First(n => n.IsKind(SyntaxKind.LocalDeclarationStatement));
            var expr = localDecl.Declaration.Variables[0].Initializer.Value;
            return Translate(compilation, expr);
        }

        public static TranslationResult TranslateStatement(string csharpStatement, IEnumerable<MetadataReference> references)
        {
            var csharpText = $"using System; public class Test {{ public void M() {{ {csharpStatement}; }} }}";
            var tree = CSharpSyntaxTree.ParseText(csharpText);
            var compilation = CSharpCompilation.Create("source", new[] { tree }, references);
            var methodDecl = (MethodDeclarationSyntax)tree.GetRoot().DescendantNodes().First(n => n.IsKind(SyntaxKind.MethodDeclaration));
            var statement = methodDecl.Body.Statements[0];
            return Translate(compilation, statement);
        }

        public static TranslationResult TranslateType(string csharpType, IEnumerable<MetadataReference> references)
        {
            var csharpText = $"using System; public class Test {{ public void M() {{ M<{csharpType}>(); }} }}";
            var tree = CSharpSyntaxTree.ParseText(csharpText);
            var compilation = CSharpCompilation.Create("source", new[] { tree }, references);
            var gname = (GenericNameSyntax)tree.GetRoot().DescendantNodes().First(n => n.IsKind(SyntaxKind.GenericName));
            var type = gname.TypeArgumentList.Arguments[0];
            return Translate(compilation, type);
        }

        #region Diagnostics
        public static readonly DiagnosticDescriptor SyntaxNotSupported = new DiagnosticDescriptor(
            "CS2TS001", "Syntax Not Supported", "The syntax '{0}' is not supported for translation to TypeScript", "TypeScript Translation", DiagnosticSeverity.Error, isEnabledByDefault: true);

        public static readonly DiagnosticDescriptor TypeNotSupported = new DiagnosticDescriptor(
            "CS2TS002", "Type Not Supported", "The type '{0}' is not supported for translation to TypeScript", "TypeScript Translation", DiagnosticSeverity.Error, isEnabledByDefault: true);

        public static readonly DiagnosticDescriptor MethodNotSupported = new DiagnosticDescriptor(
            "CS2TS003", "Method Not Supported", "The API method '{0}' is not supported for translation to TypeScript", "TypeScript Translation", DiagnosticSeverity.Error, isEnabledByDefault: true);

        public static Diagnostic GetSyntaxNotSupported(Location location, string syntax)
        {
            return Diagnostic.Create(SyntaxNotSupported, location, syntax);
        }

        public static Diagnostic GetTypeNotSupported(Location location, string typeName)
        {
            return Diagnostic.Create(TypeNotSupported, location, typeName);
        }

        public static Diagnostic GetMethodNotSupported(Location location, string methodName)
        {
            return Diagnostic.Create(MethodNotSupported, location, methodName);
        }
        #endregion

        private class Translator : CSharpSyntaxVisitor
        {
            private readonly CSharpCompilation _compilation;
            private readonly TypeScriptWriter _writer;
            private readonly Dictionary<SyntaxTree, SemanticModel> models = new Dictionary<SyntaxTree, SemanticModel>();
            private readonly List<Diagnostic> _diagnostics;

            public Translator(CSharpCompilation compilation, TextWriter writer, List<Diagnostic> diagnostics)
            {
                _compilation = compilation;
                _writer = new TypeScriptWriter(writer);
                _diagnostics = diagnostics;
            }

            private SemanticModel GetModel(SyntaxTree tree)
            {
                if (!models.TryGetValue(tree, out var model))
                {
                    model = _compilation.GetSemanticModel(tree, ignoreAccessibility: true);
                    models.Add(tree, model);
                }

                return model;
            }

            private ISymbol GetSymbol(ExpressionSyntax expr)
            {
                var model = GetModel(expr.SyntaxTree);
                var info = model.GetSymbolInfo(expr);
                return info.Symbol;
            }

            private ITypeSymbol GetType(ExpressionSyntax expr)
            {
                var model = GetModel(expr.SyntaxTree);
                var info = model.GetTypeInfo(expr);
                return info.Type;
            }

            private ITypeSymbol GetConvertedType(ExpressionSyntax expr)
            {
                var model = GetModel(expr.SyntaxTree);
                var info = model.GetTypeInfo(expr);
                return info.ConvertedType;
            }

            #region Writing
            private SyntaxTriviaList First(params SyntaxTriviaList?[] candidates)
            {
                foreach (var trivia in candidates)
                {
                    if (trivia != null && trivia.Value.Count > 0)
                    {
                        return trivia.Value;
                    }
                }

                return default(SyntaxTriviaList);
            }

            public void Write(SyntaxToken token)
            {
                Write(token.LeadingTrivia);
                Write(token.Text);
                Write(token.TrailingTrivia);
            }

            public void Write(string text)
            {
                if (string.IsNullOrWhiteSpace(text))
                {
                    _writer.WriteTrivia(text);
                }
                else
                {
                    _writer.WriteToken(text);
                }
            }

            public void WriteToken(SyntaxTriviaList leadingTrivia, string token, SyntaxTriviaList trailingTrivia)
            {
                Write(leadingTrivia);
                Write(token);
                Write(trailingTrivia);
            }

            public void Write(params object[] nodesAndTokens)
            {
                foreach (var nodeOrToken in nodesAndTokens)
                {
                    Write(nodeOrToken);
                }
            }

            public void Write(object nodeOrToken)
            {
                switch (nodeOrToken)
                {
                    case string s:
                        Write(s);
                        break;
                    case SyntaxNode n:
                        Visit(n);
                        break;
                    case SyntaxToken t:
                        Write(t);
                        break;
                    case SyntaxTriviaList tlist:
                        Write(tlist);
                        break;
                }
            }

            public void WriteLine()
            {
                _writer.WriteLine();
            }

            public void Write(SyntaxTriviaList list, bool squelch = false, bool lastLineOnly = false)
            {
                if (!IsSquelched(list))
                {
                    for (int i = lastLineOnly ? GetLastLineStart(list) : 0; i < list.Count; i++)
                    {
                        var trivia = list[i];

                        switch (trivia.Kind())
                        {
                            case SyntaxKind.WhitespaceTrivia:
                            case SyntaxKind.EndOfLineTrivia:
                            case SyntaxKind.SingleLineCommentTrivia:
                            case SyntaxKind.MultiLineCommentTrivia:
                                if (trivia.FullSpan.Length > 0)
                                {
                                    _writer.WriteTrivia(trivia.ToFullString());
                                }
                                break;

                            default:
                                // other trivia types not supported
                                break;
                        }
                    }

                    if (squelch)
                    {
                        Squelch(list);
                    }
                }
            }

            private static int GetLastLineStart(SyntaxTriviaList list)
            {
                var lastLineStart = 0;

                for (int i = 0; i < list.Count; i++)
                {
                    var trivia = list[i];
                    if (trivia.IsKind(SyntaxKind.EndOfLineTrivia))
                    {
                        lastLineStart = i + 1;
                    }
                }

                return lastLineStart;
            }

            /// <summary>
            /// Wraps the node with before & after tokens.
            /// The node's leading trivia occurs before the before-text and the trailing trivia after the after-text.
            /// </summary>
            private void Wrap(string before, SyntaxNode node, string after)
            {
                var lt = node.GetLeadingTrivia();
                var tt = node.GetTrailingTrivia();

                var ltSquelched = IsSquelched(lt);
                var ttSquelched = IsSquelched(tt);

                if (!ltSquelched)
                {
                    Write(lt);
                    Squelch(lt);
                }

                Write(before);

                if (!ttSquelched)
                {
                    Squelch(tt);
                }

                Visit(node);

                Write(after);

                if (!ltSquelched)
                {
                    Unsquelch(lt);
                }

                if (!ttSquelched)
                {
                    Unsquelch(tt);
                    Write(tt);
                }
            }

            /// <summary>
            /// The table of trivia lists that are squelched (starting positions)
            /// </summary>
            private readonly HashSet<int> _squelchedTrivia = new HashSet<int>();

            /// <summary>
            /// Prevent the specific trivia list from being written when the token containing it is written.
            /// </summary>
            public void Squelch(SyntaxTriviaList list)
            {
                if (list.Count > 0)
                {
                    _squelchedTrivia.Add(list.FullSpan.Start);
                }
            }

            /// <summary>
            /// Enable the specific trivia list to be written when the token containing it is written.
            /// </summary>
            public void Unsquelch(SyntaxTriviaList list)
            {
                _squelchedTrivia.Remove(list.FullSpan.Start);
            }

            public bool IsSquelched(SyntaxTriviaList list)
            {
                return _squelchedTrivia.Contains(list.FullSpan.Start);
            }

            public SyntaxNode SquelchLeading(SyntaxNode node)
            {
                Squelch(node.GetLeadingTrivia());
                return node;
            }

            public SyntaxToken SquelchLeading(SyntaxToken token)
            {
                Squelch(token.LeadingTrivia);
                return token;
            }

            public SyntaxNode SquelchTrailing(SyntaxNode node)
            {
                Squelch(node.GetTrailingTrivia());
                return node;
            }

            public SyntaxToken SquelchTrailing(SyntaxToken token)
            {
                Squelch(token.TrailingTrivia);
                return token;
            }

            public SyntaxNode Squelch(SyntaxNode node)
            {
                Squelch(node.GetLeadingTrivia());
                Squelch(node.GetTrailingTrivia());
                return node;
            }

            public SyntaxToken Squelch(SyntaxToken token)
            {
                Squelch(token.LeadingTrivia);
                Squelch(token.TrailingTrivia);
                return token;
            }
            #endregion

            public override void DefaultVisit(SyntaxNode node)
            {
                if (node != null)
                {
                    _diagnostics.Add(GetSyntaxNotSupported(node.GetLocation(), node.Kind().ToString()));

#if false
                    // default visit this sucker!
                    foreach (var nodeOrToken in node.ChildNodesAndTokens())
                    {
                        if (nodeOrToken.IsToken)
                        {
                            Write(nodeOrToken.AsToken());
                        }
                        else
                        {
                            Visit(nodeOrToken.AsNode());
                        }
                    }
#endif
                }
            }

            private void VisitList<T>(SyntaxList<T> list) where T : SyntaxNode
            {
                foreach (var node in list)
                {
                    Visit(node);
                }
            }

            private void VisitList<T>(SeparatedSyntaxList<T> list) where T : SyntaxNode
            {
                foreach (var nodeOrToken in list.GetWithSeparators())
                {
                    if (nodeOrToken.IsNode)
                    {
                        Visit(nodeOrToken.AsNode());
                    }
                    else
                    {
                        Write(nodeOrToken.AsToken());
                    }
                }
            }

#region Declarations
            public override void VisitCompilationUnit(CompilationUnitSyntax node)
            {
                VisitList(node.Members);
            }

            public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, node.Parent);
                Write(node.Keyword);
                Write(node.Identifier);
                Visit(node.TypeParameterList);
                Visit(node.BaseList);

                Write(node.OpenBraceToken);
                VisitList(node.Members);
                Write(node.CloseBraceToken);
            }

            public override void VisitStructDeclaration(StructDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, node.Parent);
                WriteToken(node.Keyword.LeadingTrivia, "class", node.Keyword.TrailingTrivia);
                Write(node.Identifier);
                Visit(node.TypeParameterList);
                Visit(node.BaseList);

                Write(node.OpenBraceToken);
                VisitList(node.Members);
                Write(node.CloseBraceToken);
            }

            public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, node.Parent);
                Write(node.Keyword);
                Write(node.Identifier);
                Visit(node.TypeParameterList);
                Visit(node.BaseList);

                Write(node.OpenBraceToken);
                VisitList(node.Members);
                Write(node.CloseBraceToken);
            }

            public override void VisitTypeParameterList(TypeParameterListSyntax node)
            {
                Write(node.LessThanToken);
                VisitList(node.Parameters);
                Write(node.GreaterThanToken);
            }

            public override void VisitTypeParameter(TypeParameterSyntax node)
            {
                // TODO: handle variance? attributes?
                Write(node.Identifier);
            }

            public override void VisitBaseList(BaseListSyntax node)
            {
                var interfaceKeyword = "implements";

                if (node.Parent is ClassDeclarationSyntax)
                {
                    var baseType = node.Types.FirstOrDefault(t => GetSymbol(t.Type) is INamedTypeSymbol nt && nt.TypeKind != TypeKind.Interface);
                    if (baseType != null)
                    {
                        Write(node.ColonToken.LeadingTrivia, squelch: true);
                        Write("extends");
                        Write(node.ColonToken.TrailingTrivia, squelch: true);

                        Visit(baseType.Type);
                    }
                }
                else if (node.Parent is InterfaceDeclarationSyntax)
                {
                    interfaceKeyword = "extends";
                }

                var interfaces = node.Types.Where(t => GetSymbol(t.Type) is INamedTypeSymbol nt && nt.TypeKind == TypeKind.Interface).ToList();
                if (interfaces.Count > 0)
                {
                    Write(node.ColonToken.LeadingTrivia, squelch: true);
                    Write(interfaceKeyword);
                    Write(node.ColonToken.TrailingTrivia, squelch: true);

                    for (int i = 0; i < interfaces.Count; i++)
                    {
                        var iface = interfaces[i];
                        if (i > 0)
                            Write(",");
                        Visit(iface.Type);
                    }
                }
            }

            private void WriteDeclarationModifiers(SyntaxTokenList modifiers, SyntaxNode container)
            {
                var hasAccessModifiers = modifiers.Any(m => IsAccessModifier(m));
                var hasModifiers = modifiers.Count > 0;

                bool isTypeMember = container is MemberDeclarationSyntax;
                bool isInterfaceMember = container is InterfaceDeclarationSyntax;

                if (!hasAccessModifiers && isTypeMember && !isInterfaceMember)
                {
                    Write("private");
                }

                foreach (var mod in modifiers)
                {
                    switch (mod.Kind())
                    {
                        case SyntaxKind.PublicKeyword:
                            // don't write public modifier (this is default)
                            break;

                        case SyntaxKind.InternalKeyword:
                            if (!isInterfaceMember)
                            {
                                // these is no internal access, use public instead
                                WriteToken(mod.LeadingTrivia, "public", mod.TrailingTrivia);
                            }
                            break;

                        case SyntaxKind.PrivateKeyword:
                        case SyntaxKind.ProtectedKeyword:
                            if (!isInterfaceMember)
                            {
                                Write(mod);
                            }
                            break;

                        case SyntaxKind.ReadOnlyKeyword:
                        case SyntaxKind.StaticKeyword:
                        case SyntaxKind.AbstractKeyword:
                            Write(mod);
                            break;

                        case SyntaxKind.VirtualKeyword:
                        case SyntaxKind.OverrideKeyword:
                            // these are implicit so dont write them
                            break;

                    }
                }
            }

            private static bool IsAccessModifier(SyntaxToken modifier)
            {
                switch (modifier.Kind())
                {
                    case SyntaxKind.PrivateKeyword:
                    case SyntaxKind.ProtectedKeyword:
                    case SyntaxKind.InternalKeyword:
                    case SyntaxKind.PublicKeyword:
                        return true;
                    default:
                        return false;
                }
            }

            public override void VisitConstructorDeclaration(ConstructorDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, node.Parent);

                WriteToken(node.Identifier.LeadingTrivia, "constructor", node.Identifier.TrailingTrivia);

                Visit(node.ParameterList);

                if (node.ExpressionBody != null)
                {
                    VisitExpressionBody(node.ExpressionBody, node.SemicolonToken, isVoid: true);
                }
                else
                {
                    Visit(node.Body);
                    Write(node.SemicolonToken);
                }
            }

            private void VisitExpressionBody(ArrowExpressionClauseSyntax body, SyntaxToken semicolonToken, bool isVoid)
            {
                Write(body.Expression.GetLeadingTrivia(), squelch: true);
                Write("{");
                if (!isVoid)
                {
                    Write("return");
                }
                Visit(body.Expression);
                Write(semicolonToken.Text);
                Write("}");
                Write(semicolonToken.TrailingTrivia);
            }

            public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                Write(First(node.Modifiers.GetLeadingTrivia(), node.ReturnType.GetLeadingTrivia(), node.Identifier.LeadingTrivia), squelch: true);

                WriteDeclarationModifiers(node.Modifiers, node.Parent);
                Write(node.ReturnType.GetLeadingTrivia());
                Write(node.Identifier);


                if (!IsVoidType(node.ReturnType))
                {
                    var tt = node.ParameterList.GetTrailingTrivia();
                    Squelch(tt);
                    Visit(node.ParameterList);

                    Write(":");
                    Squelch(node.ReturnType.GetTrailingTrivia());
                    Visit(node.ReturnType);

                    Unsquelch(tt);
                    Write(tt);
                }
                else
                {
                    Visit(node.ParameterList);
                }

                if (node.ExpressionBody != null)
                {
                    VisitExpressionBody(node.ExpressionBody, node.SemicolonToken, isVoid: IsVoidType(node.ReturnType));
                }
                else
                {
                    Visit(node.Body);
                    Write(node.SemicolonToken);
                }
            }

            public override void VisitParameterList(ParameterListSyntax node)
            {
                Write(node.OpenParenToken);
                VisitList(node.Parameters);
                Write(node.CloseParenToken);
            }

            public override void VisitParameter(ParameterSyntax node)
            {
                Write(First(node.Modifiers.GetLeadingTrivia(), node.Type?.GetLeadingTrivia(), node.Identifier.LeadingTrivia), squelch: true);

                WriteParameterModifiers(node.Modifiers);
                Write(node.Identifier);

                if (node.Type != null)
                {
                    Write(":");
                    Squelch(node.Type.GetTrailingTrivia());
                    Visit(node.Type);
                }
            }

            private void WriteParameterModifiers(SyntaxTokenList modifiers)
            {
                foreach (var mod in modifiers)
                {
                    switch (mod.Kind())
                    {
                        case SyntaxKind.ParamsKeyword:
                            Write(mod.LeadingTrivia);
                            Write("...");
                            break;

                        default:
                            _diagnostics.Add(GetSyntaxNotSupported(mod.GetLocation(), mod.Text));
                            break;
                    }
                }
            }

            public override void VisitFieldDeclaration(FieldDeclarationSyntax node)
            {
                Squelch(node.Declaration.Type.GetTrailingTrivia());

                // TODO: error when there is more than one variable.

                if (node.Declaration.Variables.Count > 0)
                {
                    var declarator = node.Declaration.Variables[0];
                    WriteDeclarationModifiers(node.Modifiers, node.Parent);
                    Write(declarator.Identifier.Text);
                    Write(":");
                    Visit(node.Declaration.Type);
                    Visit(declarator.Initializer);
                }

                Write(node.SemicolonToken);
            }

            public override void VisitPropertyDeclaration(PropertyDeclarationSyntax node)
            {
                var lt = node.GetLeadingTrivia();
                var isInterfaceProperty = node.Parent.IsKind(SyntaxKind.InterfaceDeclaration);

                if (node.ExpressionBody != null)
                {
                    Write(lt, squelch: true);
                    WriteDeclarationModifiers(node.Modifiers, node.Parent);
                    Write("get", node.Identifier.Text, "(", ")", ":");
                    Squelch(node.Type.GetTrailingTrivia());
                    Visit(node.Type);
                    VisitExpressionBody(node.ExpressionBody, node.SemicolonToken, isVoid: false);
                }
                else if (isInterfaceProperty)
                {
                    bool isReadOnly = !node.AccessorList.Accessors.Any(a => a.IsKind(SyntaxKind.SetAccessorDeclaration));

                    if (isReadOnly)
                    {
                        Write("readonly");
                    }

                    Squelch(node.Type.GetTrailingTrivia());
                    Write(node.Identifier.Text, ":", node.Type, node.Initializer, ";");
                }
                else if (IsAutoProperty(node.AccessorList) && !IsAbstract(node.Modifiers) && !isInterfaceProperty)
                {
                    bool isReadOnly = !node.AccessorList.Accessors.Any(a => a.IsKind(SyntaxKind.SetAccessorDeclaration));

                    if (IsVirtual(node.Modifiers) || IsOverride(node.Modifiers) || (isReadOnly && node.Initializer == null))
                    {
                        // need backing field for virtual/override auto-property property
                        var fieldName = "_" + CamelCase(node.Identifier.Text);

                        Write(lt, squelch: true);
                        Write("private", fieldName, ":");
                        Squelch(node.Type.GetTrailingTrivia());
                        Visit(node.Type);
                        Visit(node.Initializer);
                        Write(";");

                        for (int i = 0; i < node.AccessorList.Accessors.Count; i++)
                        {
                            var accessor = node.AccessorList.Accessors[i];
                            if (accessor.IsKind(SyntaxKind.GetAccessorDeclaration))
                            {
                                Unsquelch(lt);
                                Write(lt, squelch: true);
                                WriteDeclarationModifiers(node.Modifiers, node.Parent);
                                Write("get", node.Identifier.Text, "(", ")", ":");
                                Squelch(node.Type.GetTrailingTrivia());
                                Visit(node.Type);
                                Write("{", "return", fieldName, ";", "}");
                            }
                            else
                            {
                                Unsquelch(lt);
                                Write(lt, squelch: true);
                                WriteDeclarationModifiers(node.Modifiers, node.Parent);
                                Write("set", node.Identifier.Text, "(", "value", ":", node.Type, ")", "{", fieldName, "=", "value", ";", "}");
                            }
                        }
                    }
                    else 
                    {
                        // use fields for simple auto props
                        Write(lt, squelch: true);
                        WriteDeclarationModifiers(node.Modifiers, node.Parent);

                        if (isReadOnly)
                        {
                            Write("readonly");
                        }

                        Squelch(node.Type.GetTrailingTrivia());
                        Write(node.Identifier.Text, ":", node.Type, node.Initializer, ";");
                    }
                }
                else
                {
                    // translate accessors
                    for (int i = 0; i < node.AccessorList.Accessors.Count; i++)
                    {
                        var accessor = node.AccessorList.Accessors[i];

                        Unsquelch(lt);
                        Write(lt, squelch: true, lastLineOnly: i > 0);
                        WriteDeclarationModifiers(node.Modifiers, node.Parent);

                        bool isGetter = accessor.IsKind(SyntaxKind.GetAccessorDeclaration);
                        if (isGetter)
                        {
                            Squelch(node.Type.GetTrailingTrivia());
                            Write("get", node.Identifier.Text, "(", ")", ":", node.Type);
                        }
                        else
                        {
                            Squelch(node.Type.GetTrailingTrivia());
                            Write("set", node.Identifier.Text, "(", "value", ":", node.Type, ")");
                        }

                        if (accessor.ExpressionBody != null)
                        {
                            VisitExpressionBody(accessor.ExpressionBody, accessor.SemicolonToken, isVoid: !isGetter);
                        }
                        else
                        {
                            Visit(accessor.Body);
                            Write(accessor.SemicolonToken);
                        }
                    }
                }
            }

            public override void VisitIndexerDeclaration(IndexerDeclarationSyntax node)
            {
                var lt = node.GetLeadingTrivia();
                var isInterfaceProperty = node.Parent.IsKind(SyntaxKind.InterfaceDeclaration);

                if (node.ExpressionBody != null)
                {
                    Write(lt, squelch: true);
                    WriteDeclarationModifiers(node.Modifiers, node.Parent);
                    Write("get", "(");
                    VisitList(node.ParameterList.Parameters);
                    Write(")", ":");
                    Squelch(node.Type.GetTrailingTrivia());
                    Visit(node.Type);
                    VisitExpressionBody(node.ExpressionBody, node.SemicolonToken, isVoid: false);
                }
                else
                {
                    // translate accessors
                    for (int i = 0; i < node.AccessorList.Accessors.Count; i++)
                    {
                        var accessor = node.AccessorList.Accessors[i];

                        Unsquelch(lt);
                        Write(lt, squelch: true, lastLineOnly: i > 0);
                        WriteDeclarationModifiers(node.Modifiers, node.Parent);

                        bool isGetter = accessor.IsKind(SyntaxKind.GetAccessorDeclaration);
                        if (isGetter)
                        {
                            Squelch(node.Type.GetTrailingTrivia());
                            Write("get", "(");
                            VisitList(node.ParameterList.Parameters);
                            Write(")", ":", node.Type);
                        }
                        else
                        {
                            Squelch(node.Type.GetTrailingTrivia());
                            Write("set", "(");
                            VisitList(node.ParameterList.Parameters);
                            Write(",", "value", ":", node.Type, ")");
                        }

                        if (accessor.ExpressionBody != null)
                        {
                            VisitExpressionBody(accessor.ExpressionBody, accessor.SemicolonToken, isVoid: !isGetter);
                        }
                        else
                        {
                            Visit(accessor.Body);
                            Write(accessor.SemicolonToken);
                        }
                    }
                }
            }

            private static string CamelCase(string text)
            {
                if (text.Length > 0 && !char.IsLower(text[0]))
                {
                    return text.Substring(0, 1).ToLower() + text.Substring(1);
                }
                else
                {
                    return text;
                }
            }

            private static bool IsAbstract(SyntaxTokenList modifiers)
            {
                return modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword));
            }

            private static bool IsVirtual(SyntaxTokenList modifiers)
            {
                return modifiers.Any(m => m.IsKind(SyntaxKind.VirtualKeyword));
            }

            private static bool IsOverride(SyntaxTokenList modifiers)
            {
                return modifiers.Any(m => m.IsKind(SyntaxKind.OverrideKeyword));
            }

            private static bool IsAutoProperty(AccessorListSyntax accessors)
            {
                return accessors.Accessors.All(a => !HasBody(a));
            }

            private static bool HasBody(AccessorDeclarationSyntax acc)
            {
                return acc.Body != null || acc.ExpressionBody != null;
            }

            public override void VisitDelegateDeclaration(DelegateDeclarationSyntax node)
            {
                Write(node.DelegateKeyword.LeadingTrivia);
                Write("interface", node.Identifier.Text, node.TypeParameterList, "{", node.ParameterList);

                if (!IsVoidType(node.ReturnType))
                {
                    Write(":", Squelch(node.ReturnType));
                }

                Write(";", "}");
                Write(node.GetTrailingTrivia());
            }
#endregion

#region Statements
            public override void VisitBlock(BlockSyntax node)
            {
                Write(node.OpenBraceToken);

                foreach (var statement in node.Statements)
                {
                    Visit(statement);
                }

                Write(node.CloseBraceToken);
            }

            public override void VisitExpressionStatement(ExpressionStatementSyntax node)
            {
                Visit(node.Expression);
                Write(node.SemicolonToken);
            }

            public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
            {
                var leadingTrivia = First(node.Modifiers.GetLeadingTrivia(), node.Declaration.Type.GetLeadingTrivia(), node.Declaration.Variables[0].Identifier.LeadingTrivia);

                Write(leadingTrivia, squelch: true);
                Visit(node.Declaration);

                Write(node.SemicolonToken);
            }

            public override void VisitVariableDeclaration(VariableDeclarationSyntax node)
            {
                var leadingTrivia = First(node.Type.GetLeadingTrivia(), node.Variables[0].Identifier.LeadingTrivia);

                Write(leadingTrivia, squelch: true);
                Write("let");

                VisitList(node.Variables);
            }

            public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
            {
                if (node.Initializer != null)
                {
                    if (node.Initializer.Value.IsKind(SyntaxKind.NullLiteralExpression))
                    {
                        Squelch(node.Identifier.TrailingTrivia);
                        Write(node.Identifier);

                        if (node.Parent is VariableDeclarationSyntax vd)
                        {
                            Write(":");
                            Visit(vd.Type);
                        }

                        Visit(node.Initializer);
                    }
                    else
                    {
                        Write(node.Identifier);
                        Visit(node.Initializer);
                    }
                }
                else
                {
                    Squelch(node.Identifier.TrailingTrivia);
                    Write(node.Identifier);

                    if (node.Parent is VariableDeclarationSyntax vd)
                    {
                        var defValue = GetDefaultForType(vd.Type);

                        if (defValue == "null")
                        {
                            Write(":");
                            Visit(vd.Type);
                        }

                        Write("=");
                        Write(defValue);
                    }
                }
            }

            private string GetDefaultForType(TypeSyntax type)
            {
                if (GetSymbol(type) is INamedTypeSymbol symbol)
                {
                    switch (symbol.SpecialType)
                    {
                        case SpecialType.System_Boolean:
                            return "false";

                        case SpecialType.System_Decimal:
                        case SpecialType.System_Double:
                        case SpecialType.System_Single:
                        case SpecialType.System_Int16:
                        case SpecialType.System_Int32:
                        case SpecialType.System_Int64:
                        case SpecialType.System_UInt16:
                        case SpecialType.System_UInt32:
                        case SpecialType.System_UInt64:
                        case SpecialType.System_Byte:
                        case SpecialType.System_SByte:
                        case SpecialType.System_Char:
                            return "0";
                    }
                }

                return "null";
            }

            public override void VisitEqualsValueClause(EqualsValueClauseSyntax node)
            {
                Write(node.EqualsToken);
                Visit(node.Value);
            }

            public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
            {
                if (node.Left is ElementAccessExpressionSyntax eax && GetSymbol(eax) is IPropertySymbol ps)
                {
                    Write(node.GetLeadingTrivia(), squelch: true);

                    if (node.IsKind(SyntaxKind.SimpleAssignmentExpression))
                    {
                        if (eax.Expression != null)
                        {
                            Visit(eax.Expression);
                            Write(".");
                        }

                        Write("set", "(");
                        VisitList(eax.ArgumentList.Arguments);
                        Squelch(node.Right);
                        Write(",", node.Right, ")");
                        Unsquelch(node.Right.GetTrailingTrivia());
                        Write(node.Right.GetTrailingTrivia());
                    }
                    else
                    {
                        if (eax.Expression != null)
                        {
                            Visit(eax.Expression);
                            Write(".");
                        }

                        Write("set", "(");
                        VisitList(eax.ArgumentList.Arguments);
                        Write(",");

                        if (eax.Expression != null)
                        {
                            Visit(eax.Expression);
                            Write(".");
                        }

                        Write("get", "(");
                        VisitList(eax.ArgumentList.Arguments);
                        Write(")");
                        Write(eax.ArgumentList.GetTrailingTrivia());

                        Write(node.OperatorToken.LeadingTrivia, GetOperator(node), node.OperatorToken.TrailingTrivia);

                        Squelch(node.Right);
                        Write(node.Right);

                        Write(")");

                        Unsquelch(node.Right.GetTrailingTrivia());
                        Write(node.Right.GetTrailingTrivia());
                    }
                }
                else
                {
                    Visit(node.Left);
                    Write(node.OperatorToken);
                    Visit(node.Right);
                }
            }

            private static string GetOperator(AssignmentExpressionSyntax ax)
            {
                switch (ax.Kind())
                {
                    case SyntaxKind.AddAssignmentExpression:
                        return "+";
                    case SyntaxKind.DivideAssignmentExpression:
                        return "/";
                    case SyntaxKind.ExclusiveOrAssignmentExpression:
                        return "^";
                    case SyntaxKind.LeftShiftAssignmentExpression:
                        return "<<";
                    case SyntaxKind.ModuloAssignmentExpression:
                        return "%";
                    case SyntaxKind.MultiplyAssignmentExpression:
                        return "*";
                    case SyntaxKind.OrAssignmentExpression:
                        return "|";
                    case SyntaxKind.RightShiftAssignmentExpression:
                        return ">>";
                    case SyntaxKind.SubtractAssignmentExpression:
                        return "-";
                    default:
                        return "";
                }
            }

            public override void VisitIfStatement(IfStatementSyntax node)
            {
                Write(node.IfKeyword);
                Write(node.OpenParenToken);
                Visit(node.Condition);
                Write(node.CloseParenToken);
                Visit(node.Statement);
                Visit(node.Else);
            }

            public override void VisitElseClause(ElseClauseSyntax node)
            {
                Write(node.ElseKeyword);
                Visit(node.Statement);
            }

            public override void VisitSwitchStatement(SwitchStatementSyntax node)
            {
                Write(node.SwitchKeyword);
                Write(node.OpenParenToken);
                Visit(node.Expression);
                Write(node.CloseParenToken);
                Write(node.OpenBraceToken);
                VisitList(node.Sections);
                Write(node.CloseBraceToken);
            }

            public override void VisitSwitchSection(SwitchSectionSyntax node)
            {
                VisitList(node.Labels);
                VisitList(node.Statements);
            }

            public override void VisitCaseSwitchLabel(CaseSwitchLabelSyntax node)
            {
                Write(node.Keyword);
                Visit(node.Value);
                Write(node.ColonToken);
            }

            public override void VisitDefaultSwitchLabel(DefaultSwitchLabelSyntax node)
            {
                Write(node.Keyword);
                Write(node.ColonToken);
            }

            public override void VisitBreakStatement(BreakStatementSyntax node)
            {
                Write(node.BreakKeyword);
                Write(node.SemicolonToken);
            }

            public override void VisitContinueStatement(ContinueStatementSyntax node)
            {
                Write(node.ContinueKeyword);
                Write(node.SemicolonToken);
            }

            public override void VisitReturnStatement(ReturnStatementSyntax node)
            {
                Write(node.ReturnKeyword);
                Visit(node.Expression);
                Write(node.SemicolonToken);
            }

            public override void VisitWhileStatement(WhileStatementSyntax node)
            {
                Write(node.WhileKeyword);
                Write(node.OpenParenToken);
                Visit(node.Condition);
                Write(node.CloseParenToken);
                VisitBlock(node.Statement);
            }

            public override void VisitDoStatement(DoStatementSyntax node)
            {
                Write(node.DoKeyword);
                VisitBlock(node.Statement);
                Write(node.WhileKeyword);
                Write(node.OpenParenToken);
                Visit(node.Condition);
                Write(node.CloseParenToken);
                Write(node.SemicolonToken);
            }

            public override void VisitForStatement(ForStatementSyntax node)
            {
                Write(node.ForKeyword);
                Write(node.OpenParenToken);

                Visit(node.Declaration);  // either, or?
                VisitList(node.Initializers);
                Write(node.FirstSemicolonToken);

                Visit(node.Condition);
                Write(node.SecondSemicolonToken);

                VisitList(node.Incrementors);
                Write(node.CloseParenToken);

                VisitBlock(node.Statement);
            }

            public override void VisitForEachStatement(ForEachStatementSyntax node)
            {
                WriteToken(node.ForEachKeyword.LeadingTrivia, "for", node.ForEachKeyword.TrailingTrivia);
                Write(node.OpenParenToken);

                Write("let");

                if (!IsVarType(node.Type))
                {
                    Squelch(node.Identifier.TrailingTrivia);
                    Write(node.Identifier);
                    Write(":");
                    Visit(node.Type);
                }
                else
                {
                    Write(node.Identifier);
                }

                Write("of");
                Visit(node.Expression);

                Write(node.CloseParenToken);

                VisitBlock(node.Statement);
            }

            public override void VisitForEachVariableStatement(ForEachVariableStatementSyntax node)
            {
                base.VisitForEachVariableStatement(node);
            }

            private void VisitBlock(StatementSyntax statement)
            {
                if (statement is BlockSyntax)
                {
                    Visit(statement);
                }
                else
                {
                    Write("{");
                    Visit(statement);
                    Write("}");
                }
            }

            public override void VisitTryStatement(TryStatementSyntax node)
            {
                Write(node.TryKeyword);
                Visit(node.Block);


                if (node.Catches.Count > 0)
                {
                    Write("(", "e", ")", "{");

                    for (int i = 0; i < node.Catches.Count; i++)
                    {
                        var c = node.Catches[i];

                        if (c.Declaration != null)
                        {
                            //Write("if", "(", "e", "instanceof", c.Declaration.Type, ""
                        }
                    }

                    Write("}");
                }


                Visit(node.Finally);
            }

            public override void VisitCatchClause(CatchClauseSyntax node)
            {
                Write(node.CatchKeyword);

                if (node.Declaration != null)
                {
                    Visit(node.Declaration);
                }
                else
                {
                    Write("(", "_e", ")");
                }

                Visit(node.Filter);
                Visit(node.Block);
            }

            public override void VisitCatchDeclaration(CatchDeclarationSyntax node)
            {
                Write(node.OpenParenToken);
                Write(node.Identifier.Text);

                if (node.Type != null)
                {
                    Write(":");
                    Squelch(node.Type);
                    Visit(node.Type);
                }

                Write(node.CloseParenToken);
            }

            public override void VisitFinallyClause(FinallyClauseSyntax node)
            {
                Write(node.FinallyKeyword, node.Block);
            }

            public override void VisitThrowStatement(ThrowStatementSyntax node)
            {
                Write(node.ThrowKeyword, node.Expression, node.SemicolonToken);
            }
            #endregion

            #region Types
            private static bool IsVarType(ExpressionSyntax expr)
            {
                return expr is IdentifierNameSyntax id && id.Identifier.Text == "var";
            }

            private static bool IsVoidType(ExpressionSyntax expr)
            {
                return expr is PredefinedTypeSyntax pd && pd.Keyword.IsKind(SyntaxKind.VoidKeyword);
            }

            public override void VisitArrayType(ArrayTypeSyntax node)
            {
                Visit(node.ElementType);
                VisitList(node.RankSpecifiers);
            }

            public override void VisitNullableType(NullableTypeSyntax node)
            {
                Visit(node.ElementType);
                Write("|");
                Write("null");
            }

            public override void VisitPredefinedType(PredefinedTypeSyntax node)
            {
                switch (node.Keyword.Kind())
                {
                    case SyntaxKind.BoolKeyword:
                        WriteToken(node.Keyword.LeadingTrivia, "boolean", node.Keyword.TrailingTrivia);
                        break;

                    case SyntaxKind.DecimalKeyword:
                    case SyntaxKind.IntKeyword:
                    case SyntaxKind.UIntKeyword:
                    case SyntaxKind.LongKeyword:
                    case SyntaxKind.ULongKeyword:
                    case SyntaxKind.ShortKeyword:
                    case SyntaxKind.UShortKeyword:
                    case SyntaxKind.ByteKeyword:
                    case SyntaxKind.SByteKeyword:
                    case SyntaxKind.FloatKeyword:
                    case SyntaxKind.DoubleKeyword:
                    case SyntaxKind.CharKeyword:
                        WriteToken(node.Keyword.LeadingTrivia, "number", node.Keyword.TrailingTrivia);
                        break;

                    case SyntaxKind.ObjectKeyword:
                    case SyntaxKind.StringKeyword:
                    case SyntaxKind.VoidKeyword:
                        Write(node.Keyword);
                        break;

                    default:
                        _diagnostics.Add(GetTypeNotSupported(node.GetLocation(), node.ToString()));
                        Write(node.ToString());
                        break;
                }
            }

            private bool IsSpecialType(ITypeSymbol type)
            {
                switch (type)
                {
                    case INamedTypeSymbol nt:
                        switch (nt.SpecialType)
                        {
                            case SpecialType.System_Boolean:
                            case SpecialType.System_Decimal:
                            case SpecialType.System_Double:
                            case SpecialType.System_Single:
                            case SpecialType.System_Int16:
                            case SpecialType.System_Int32:
                            case SpecialType.System_Int64:
                            case SpecialType.System_UInt16:
                            case SpecialType.System_UInt32:
                            case SpecialType.System_UInt64:
                            case SpecialType.System_Byte:
                            case SpecialType.System_SByte:
                            case SpecialType.System_Object:
                            case SpecialType.System_String:
                            case SpecialType.System_Char:
                            case SpecialType.System_DateTime:
                                return true;
                        }

                        if (nt.IsGenericType)
                        {
                            switch (nt.ConstructedFrom.Name)
                            {
                                case "Func":
                                case "Action":
                                    return true;
                            }
                        }
                        break;
                }

                return false;
            }

            private void WriteType(ITypeSymbol type)
            {
                switch (type)
                {
                    case INamedTypeSymbol nt:
                        switch (nt.SpecialType)
                        {
                            case SpecialType.System_Boolean:
                                Write("boolean");
                                return;

                            case SpecialType.System_Decimal:
                            case SpecialType.System_Double:
                            case SpecialType.System_Single:
                            case SpecialType.System_Int16:
                            case SpecialType.System_Int32:
                            case SpecialType.System_Int64:
                            case SpecialType.System_UInt16:
                            case SpecialType.System_UInt32:
                            case SpecialType.System_UInt64:
                            case SpecialType.System_Byte:
                            case SpecialType.System_SByte:
                            case SpecialType.System_Char:
                                Write("number");
                                return;

                            case SpecialType.System_Object:
                                Write("object");
                                return;

                            case SpecialType.System_String:
                                Write("string");
                                return;

                            case SpecialType.System_DateTime:
                                Write("Date");
                                return;
                        }

                        if (nt.IsGenericType)
                        {
                            var name = nt.ConstructedFrom.Name;
                            switch (name)
                            {
                                case "Nullable":
                                    WriteType(nt.TypeArguments[0]);
                                    Write("|");
                                    Write("null");
                                    return;

                                case "Func":
                                    Write("(");
                                    for (int i = 0; i < nt.TypeArguments.Length - 1; i++)
                                    {
                                        var t = nt.TypeArguments[i];
                                        if (i > 0)
                                            Write(",");
                                        Write("arg" + i);
                                        Write(":");
                                        WriteType(t);
                                    }
                                    Write(")");
                                    Write("=>");
                                    WriteType(nt.TypeArguments[nt.TypeArguments.Length - 1]);
                                    return;

                                case "Action":
                                    Write("(");
                                    for (int i = 0; i < nt.TypeArguments.Length; i++)
                                    {
                                        var t = nt.TypeArguments[i];
                                        if (i > 0)
                                            Write(",");
                                        Write("arg" + i);
                                        Write(":");
                                        WriteType(t);
                                    }
                                    Write(")");
                                    Write("=>");
                                    Write("void");
                                    return;
                            }
                        }

                        // TODO: write full name (including containers/namespaces)
                        Write(nt.Name);
                        break;

                    case IArrayTypeSymbol at:
                        WriteType(at.ElementType);
                        Write("[]"); // TODO: multi-dimensional arrays?
                        break;

                    case ITypeParameterSymbol tp:
                        Write(tp.Name);
                        break;
                }


                /*
                if (ts.IsGenericType)
                {
                    switch (ts.ConstructedFrom.SpecialType)
                    {
                        case SpecialType.System_Collections_Generic_IReadOnlyCollection_T:
                        case SpecialType.System_Collections_Generic_ICollection_T:
                        case SpecialType.System_Collections_Generic_IReadOnlyList_T:
                        case SpecialType.System_Collections_Generic_IList_T:
                        case SpecialType.System_Collections_Generic_IEnumerable_T:
                            var elementType = ts.TypeArguments[0];
                            break;
                    }
                }
                */
            }

            private void VisitTypeList(SeparatedSyntaxList<TypeSyntax> list)
            {
                foreach (var nodeOrToken in list.GetWithSeparators())
                {
                    if (nodeOrToken.IsNode)
                    {
                        Visit((TypeSyntax)nodeOrToken.AsNode());
                    }
                    else
                    {
                        Write(nodeOrToken.AsToken());
                    }
                }
            }

#endregion

#region Literals
            public override void VisitLiteralExpression(LiteralExpressionSyntax node)
            {
                switch (node.Kind())
                {
                    case SyntaxKind.TrueLiteralExpression:
                    case SyntaxKind.FalseLiteralExpression:
                    case SyntaxKind.NullLiteralExpression:
                        Write(node.Token);
                        break;

                    case SyntaxKind.NumericLiteralExpression:
                        var text = node.Token.Text;

                        // remove any suffix
                        if (text.EndsWith("ul", StringComparison.OrdinalIgnoreCase))
                        {
                            text = text.Substring(0, text.Length - 2);
                        }
                        else if (text.EndsWith("l", StringComparison.OrdinalIgnoreCase)
                            || text.EndsWith("m", StringComparison.OrdinalIgnoreCase)
                            // hex literals can end with d or f which shouldn't be removed.
                            || (!text.StartsWith("0x", StringComparison.OrdinalIgnoreCase) 
                                && (text.EndsWith("f", StringComparison.OrdinalIgnoreCase)
                                    || text.EndsWith("d", StringComparison.OrdinalIgnoreCase))))
                        {
                            text = text.Substring(0, text.Length - 1);
                        }

                        // remove any underscores
                        if (text.Contains("_"))
                        {
                            text = text.Replace("_", "");
                        }

                        WriteToken(node.Token.LeadingTrivia, text, node.Token.TrailingTrivia);
                        break;

                    case SyntaxKind.StringLiteralExpression:
                        if (node.Token.Text.StartsWith("@"))
                        {
                            Write(GetStringLiteral(node.Token.ValueText));
                        }
                        else
                        {
                            Write(node.Token);
                        }
                        break;

                    case SyntaxKind.CharacterLiteralExpression:
                        Write(((ushort)((char)node.Token.Value)).ToString());
                        break;

                    default:
                        _diagnostics.Add(GetSyntaxNotSupported(node.GetLocation(), node.ToString()));
                        Write(node.ToString());
                        break;
                }
            }

            private static string GetStringLiteral(string text)
            {

                var encoded = EncodeAsStringLiteral(text);
                return $"\"{encoded}\"";
            }

            private static string EncodeAsStringLiteral(string text)
            {
                var builder = new StringBuilder();

                foreach (var c in text)
                {
                    switch (c)
                    {
                        case '\t':
                            builder.Append("\\t");
                            break;
                        case '\r':
                            builder.Append("\\r");
                            break;
                        case '\n':
                            builder.Append("\\n");
                            break;
                        case '\\':
                            builder.Append("\\\\");
                            break;
                        case '"':
                            builder.Append("\\\"");
                            break;
                        default:
                            builder.Append(c);
                            break;
                    }
                }

                return builder.ToString();
            }
#endregion

#region Expressions
            public override void VisitArrayRankSpecifier(ArrayRankSpecifierSyntax node)
            {
                Write(node.OpenBracketToken);
                VisitList(node.Sizes);
                Write(node.CloseBracketToken);
            }

            public override void VisitOmittedArraySizeExpression(OmittedArraySizeExpressionSyntax node)
            {
                // no nothing
            }

            private bool TryVisitType(ExpressionSyntax node)
            {
                if (GetSymbol(node) is ITypeSymbol ts)
                {
                    if (IsSpecialType(ts))
                    {
                        Write(node.GetLeadingTrivia());
                        WriteType(ts);
                        Write(node.GetTrailingTrivia());
                        return true;
                    }
                    else if (ts.Locations.Length > 0 && !ts.Locations[0].IsInSource)
                    {
                        _diagnostics.Add(GetTypeNotSupported(node.GetLocation(), ts.MetadataName));
                    }
                }

                return false;
            }

            public override void VisitIdentifierName(IdentifierNameSyntax node)
            {
                if (!TryVisitType(node))
                {
                    Write(node.Identifier);
                }
            }

            public override void VisitQualifiedName(QualifiedNameSyntax node)
            {
                if (!TryVisitType(node))
                {
                    Visit(node.Left);
                    Write(node.DotToken);
                    Visit(node.Right);
                }
            }

            public override void VisitGenericName(GenericNameSyntax node)
            {
                if (!TryVisitType(node))
                {
                    Write(node.Identifier);
                    Visit(node.TypeArgumentList);
                }
            }

            public override void VisitTypeArgumentList(TypeArgumentListSyntax node)
            {
                Write(node.LessThanToken);
                VisitTypeList(node.Arguments);
                Write(node.GreaterThanToken);
            }

            public override void VisitOmittedTypeArgument(OmittedTypeArgumentSyntax node)
            {
                // do nothing
            }

            public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
            {
                Visit(node.Expression);
                Write(node.OperatorToken);
                Visit(node.Name);
            }

            public override void VisitElementAccessExpression(ElementAccessExpressionSyntax node)
            {
                Visit(node.Expression);

                if (GetSymbol(node) is IPropertySymbol ps)
                {
                    // this is an indexer access
                    if (node.Expression != null)
                    {
                        Write(".");
                    }

                    if (node.Parent is AssignmentExpressionSyntax ax && ax.Left == node)
                    {
                        Write("set", "(");
                        VisitList(node.ArgumentList.Arguments);
                        Squelch(ax.Right);
                        Write(",", ax.Right, ")");
                        Unsquelch(ax.Right.GetTrailingTrivia());
                        Write(ax.Right.GetTrailingTrivia());
                    }
                    else
                    {
                        Write("get", "(");
                        VisitList(node.ArgumentList.Arguments);
                        Write(")");
                    }
                }
                else
                {
                    Visit(node.ArgumentList);
                }
            }

            public override void VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
            {
                Write(node.OpenParenToken);
                Visit(node.Expression);
                Write(node.CloseParenToken);
            }

            public override void VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                Visit(node.Left);
                Write(node.OperatorToken);
                Visit(node.Right);
            }

            public override void VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
            {
                Write(node.OperatorToken);
                Visit(node.Operand);
            }

            public override void VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
            {
                Visit(node.Operand);
                Write(node.OperatorToken);
            }

            public override void VisitConditionalExpression(ConditionalExpressionSyntax node)
            {
                Visit(node.Condition);
                Write(node.QuestionToken);
                Visit(node.WhenTrue);
                Write(node.ColonToken);
                Visit(node.WhenFalse);
            }

            public override void VisitInvocationExpression(InvocationExpressionSyntax node)
            {
                if (!TryVisitSpecialMethodInvocation(node))
                {
                    Visit(node.Expression);
                    Visit(node.ArgumentList);
                }
            }

            private bool TryVisitSpecialMethodInvocation(InvocationExpressionSyntax node)
            {
                // TODO: handle extension method invocation?

                if (GetSymbol(node) is IMethodSymbol ms
                    && ms.Locations.Length > 0 && !ms.Locations[0].IsInSource)
                {
                    var translator = GetInvocationTranslator(ms);
                    if (translator != null)
                    {
                        translator(node);
                        return true;
                    }

                    _diagnostics.Add(GetMethodNotSupported(node.GetLocation(), ms.ToDisplayString()));
                }

                return false;
            }

            public override void VisitArgumentList(ArgumentListSyntax node)
            {
                Write(node.OpenParenToken);
                VisitList(node.Arguments);
                Write(node.CloseParenToken);
            }

            public override void VisitArgument(ArgumentSyntax node)
            {
                // TODO: handle ref arguments
                Visit(node.Expression);
            }

            public override void VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node)
            {
                Visit(node.Parameter);
                Write(node.ArrowToken);
                Visit(node.Body);
            }

            public override void VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node)
            {
                Visit(node.ParameterList);
                Write(node.ArrowToken);
                Visit(node.Body);
            }

            public override void VisitAnonymousMethodExpression(AnonymousMethodExpressionSyntax node)
            {
                Write(First(node.AsyncKeyword.LeadingTrivia, node.DelegateKeyword.LeadingTrivia), squelch: true);
                Visit(node.ParameterList);
                Write("=>");
                Visit(node.Body);
            }

            public override void VisitArrayCreationExpression(ArrayCreationExpressionSyntax node)
            {
                WriteToken(node.NewKeyword.LeadingTrivia, "[", node.Initializer.OpenBraceToken.TrailingTrivia);
                VisitList(node.Initializer.Expressions);
                WriteToken(node.Initializer.CloseBraceToken.LeadingTrivia, "]", node.Initializer.CloseBraceToken.TrailingTrivia);
            }

            public override void VisitImplicitArrayCreationExpression(ImplicitArrayCreationExpressionSyntax node)
            {
                WriteToken(node.NewKeyword.LeadingTrivia, "[", node.Initializer.OpenBraceToken.TrailingTrivia);
                VisitList(node.Initializer.Expressions);
                WriteToken(node.Initializer.CloseBraceToken.LeadingTrivia, "]", node.Initializer.CloseBraceToken.TrailingTrivia);
            }

            public override void VisitObjectCreationExpression(ObjectCreationExpressionSyntax node)
            {
                // TODO: handle external type constructors

                Write(node.NewKeyword);
                Visit(node.Type);
                Visit(node.ArgumentList);
                Visit(node.Initializer);
            }

            public override void VisitThisExpression(ThisExpressionSyntax node)
            {
                Write(node.Token);
            }

            public override void VisitBaseExpression(BaseExpressionSyntax node)
            {
                base.VisitBaseExpression(node);
            }

            public override void VisitCastExpression(CastExpressionSyntax node)
            {
                if (GetSymbol(node) is IMethodSymbol ms)
                {
                    // TODO: handle user defined conversion...
                    _diagnostics.Add(GetMethodNotSupported(node.GetLocation(), ms.ToDisplayString()));
                }

                var fromType = GetType(node.Expression);
                var toType = GetType(node);
                Write(First(node.GetLeadingTrivia(), node.GetLeadingTrivia()), squelch: true);
                WriteConversion(fromType, toType, node.Expression, isExplicit: true);
            }

            private void WriteConversion(ITypeSymbol fromType, ITypeSymbol toType, SyntaxNode node, bool isExplicit = false)
            {
                var conv = _compilation.ClassifyConversion(fromType, toType);

                if (conv.IsNumeric)
                {
                    if ((fromType.SpecialType == SpecialType.System_Double || fromType.SpecialType == SpecialType.System_Single)
                        && (toType.SpecialType != SpecialType.System_Double && toType.SpecialType != SpecialType.System_Single))
                    {
                        // going from floating point to integer
                        Write("Math.floor");
                        Wrap("(", node, ")");
                    }
                    else
                    {
                        Visit(node);
                    }
                }
                else if ((conv.IsExplicit || isExplicit) && !(conv.IsIdentity || conv.IsBoxing || conv.IsNullable))
                {
                    Write("<");
                    WriteType(toType);
                    Write(">");
                    Visit(node);
                }
                else
                {
                    Visit(node);
                }
            }
#endregion

#region API Translations

            private Action<InvocationExpressionSyntax> GetInvocationTranslator(IMethodSymbol method)
            {
                InitTranslations();
                var its = _invocationTranslations[method.Name];
                var it = its.FirstOrDefault(m => m.Symbol.Equals(method));
                return it?.Translator;
            }

            private InvocationTranslation TranslateInvocation(E.Expression<Func<object>> lambda, Action<InvocationExpressionSyntax> translator)
            {
                var method = GetMethod(_compilation, lambda);
                return new InvocationTranslation(method, translator);
            }

            private static IMethodSymbol GetMethod(Compilation compilation, E.Expression<Func<object>> expr)
            {
                var body = expr.Body;

                if (body is E.UnaryExpression ue && ue.NodeType == E.ExpressionType.Convert)
                    body = ue.Operand;

                if (body is E.MethodCallExpression mc)
                {
                    return SymbolMapper.GetMethodSymbol(compilation, mc.Method);
                }

                return null;
            }

            private class InvocationTranslation
            {
                public IMethodSymbol Symbol { get; }

                public Action<InvocationExpressionSyntax> Translator;

                public InvocationTranslation(IMethodSymbol symbol, Action<InvocationExpressionSyntax> translator)
                {
                    this.Symbol = symbol;
                    this.Translator = translator;
                }
            }

            private void RenameInvocation(string name, InvocationExpressionSyntax invocation)
            {
                Visit(invocation.Expression);
                Write(".");
                Write(name);
                Visit(invocation.ArgumentList);
            }

            private void RenameStaticInvocation(string name, InvocationExpressionSyntax invocation)
            {
                Write(name);
                Visit(invocation.ArgumentList);
            }

            private void WriteFirstArgInvocation(string name, InvocationExpressionSyntax invocation)
            {
                Visit(invocation.ArgumentList.Arguments[0]);
                Write(".");
                Write(name);

                Write(invocation.ArgumentList.OpenParenToken);

                for (int i = 1; i < invocation.ArgumentList.Arguments.Count; i++)
                {
                    if (i > 1)
                        Write(",");
                    Visit(invocation.ArgumentList.Arguments[i]);
                }

                Write(invocation.ArgumentList.CloseParenToken);
            }

            private ILookup<string, InvocationTranslation> _invocationTranslations;

            private void InitTranslations()
            {
                if (_invocationTranslations != null)
                    return;

                var invocationTranslations = new InvocationTranslation[]
                {
                    TranslateInvocation(() => string.Concat((string)null, (string)null), inv => 
                        WriteFirstArgInvocation("concat", inv)),

                    TranslateInvocation(() => string.Concat((string)null, (string)null, (string)null), inv => 
                        WriteFirstArgInvocation("concat", inv)),

                    TranslateInvocation(() => string.Concat((string)null, (string)null, (string)null, (string)null), inv => 
                        WriteFirstArgInvocation("concat", inv)),

                    TranslateInvocation(() => string.Concat((string[])null), inv =>
                    {
                        Write("\"\".concat");
                        Write(inv.ArgumentList.OpenParenToken);
                        if (inv.ArgumentList.Arguments.Count == 1)
                        {
                            Write("...");
                        }
                        VisitList(inv.ArgumentList.Arguments);
                        Write(inv.ArgumentList.CloseParenToken);
                    })
                };

                _invocationTranslations = invocationTranslations.ToLookup(m => m.Symbol.Name);
            }

#endregion
        }
    }
}