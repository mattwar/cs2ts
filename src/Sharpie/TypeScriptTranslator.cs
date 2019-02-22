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
            var csharpText = $"using System; public class Test {{ public {csharpType} M() {{ }} }}";
            var tree = CSharpSyntaxTree.ParseText(csharpText);
            var compilation = CSharpCompilation.Create("source", new[] { tree }, references);
            var methodDecl = (MethodDeclarationSyntax)tree.GetRoot().DescendantNodes().First(n => n.IsKind(SyntaxKind.MethodDeclaration));
            var type = methodDecl.ReturnType;
            var writer = new StringWriter();
            var diagnostics = new List<Diagnostic>();
            var translator = new Translator(compilation, writer, diagnostics);
            translator.VisitType(type);
            return new TranslationResult(writer.ToString().Trim(), diagnostics);
        }

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


            #region Writing
            public void WriteToken(SyntaxToken token)
            {
                WriteTrivia(token.LeadingTrivia);
                WriteToken(token.Text);
                WriteTrivia(token.TrailingTrivia);
            }

            public void WriteToken(string token)
            {
                _writer.WriteToken(token);
            }

            public void WriteToken(SyntaxTriviaList leadingTrivia, string token, SyntaxTriviaList trailingTrivia)
            {
                WriteTrivia(leadingTrivia);
                WriteToken(token);
                WriteTrivia(trailingTrivia);
            }

            public void WriteTrivia(SyntaxTriviaList list, bool squelch = false)
            {
                if (!IsSquelched(list))
                {
                    foreach (var trivia in list)
                    {
                        WriteTrivia(trivia);
                    }

                    if (squelch)
                    {
                        Squelch(list);
                    }
                }
            }

            public void WriteTrivia(SyntaxTrivia trivia)
            {
                if (trivia.FullSpan.Length > 0)
                {
                    WriteTrivia(trivia.ToFullString());
                }
            }

            public void WriteTrivia(string trivia)
            {
                _writer.WriteTrivia(trivia);
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
            #endregion


            #region Diagnostics
            public static readonly DiagnosticDescriptor SyntaxNotSupported = new DiagnosticDescriptor(
                "CS2TS001", "Syntax Not Supported", "The C# syntax '{0}' is not supported for translation to TypeScript", "TypeScript Translation", DiagnosticSeverity.Error, isEnabledByDefault: true);

            private static Diagnostic GetSyntaxNotSupported(Location location, string syntax)
            {
                return Diagnostic.Create(SyntaxNotSupported, location, syntax);
            }
            #endregion


            public override void DefaultVisit(SyntaxNode node)
            {
                if (node != null)
                {
                    _diagnostics.Add(GetSyntaxNotSupported(node.GetLocation(), node.Kind().ToString()));
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
                        WriteToken(nodeOrToken.AsToken());
                    }
                }
            }

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

            private static bool IsVarType(ExpressionSyntax expr)
            {
                return expr is IdentifierNameSyntax id && id.Identifier.Text == "var";
            }

            private static bool IsVoidType(ExpressionSyntax expr)
            {
                return expr is PredefinedTypeSyntax pd && pd.Keyword.IsKind(SyntaxKind.VoidKeyword);
            }

            #region Declarations

            public override void VisitCompilationUnit(CompilationUnitSyntax node)
            {
                VisitList(node.Members);
            }

            public override void VisitClassDeclaration(ClassDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, isTypeMember: node.Parent is TypeDeclarationSyntax);
                WriteToken(node.Keyword);
                WriteToken(node.Identifier);
                Visit(node.TypeParameterList);
                Visit(node.BaseList);

                WriteToken(node.OpenBraceToken);
                VisitList(node.Members);
                WriteToken(node.CloseBraceToken);
            }

            public override void VisitStructDeclaration(StructDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, isTypeMember: node.Parent is TypeDeclarationSyntax);
                WriteToken(node.Keyword.LeadingTrivia, "class", node.Keyword.TrailingTrivia);
                WriteToken(node.Identifier);
                Visit(node.TypeParameterList);
                Visit(node.BaseList);

                WriteToken(node.OpenBraceToken);
                VisitList(node.Members);
                WriteToken(node.CloseBraceToken);
            }

            public override void VisitInterfaceDeclaration(InterfaceDeclarationSyntax node)
            {
                WriteDeclarationModifiers(node.Modifiers, isTypeMember: node.Parent is TypeDeclarationSyntax);
                WriteToken(node.Keyword);
                WriteToken(node.Identifier);
                Visit(node.TypeParameterList);
                Visit(node.BaseList);

                WriteToken(node.OpenBraceToken);
                VisitList(node.Members);
                WriteToken(node.CloseBraceToken);
            }

            public override void VisitTypeParameterList(TypeParameterListSyntax node)
            {
                WriteToken(node.LessThanToken);
                VisitList(node.Parameters);
                WriteToken(node.GreaterThanToken);
            }

            public override void VisitTypeParameter(TypeParameterSyntax node)
            {
                // TODO: handle variance? attributes?
                WriteToken(node.Identifier);
            }

            public override void VisitBaseList(BaseListSyntax node)
            {
                var interfaceKeyword = "implements";

                if (node.Parent is ClassDeclarationSyntax)
                {
                    var baseType = node.Types.FirstOrDefault(t => GetSymbol(t.Type) is INamedTypeSymbol nt && nt.TypeKind != TypeKind.Interface);
                    if (baseType != null)
                    {
                        WriteTrivia(node.ColonToken.LeadingTrivia, squelch: true);
                        WriteToken("extends");
                        WriteTrivia(node.ColonToken.TrailingTrivia, squelch: true);

                        VisitType(baseType.Type);
                    }
                }
                else if (node.Parent is InterfaceDeclarationSyntax)
                {
                    interfaceKeyword = "extends";
                }

                var interfaces = node.Types.Where(t => GetSymbol(t.Type) is INamedTypeSymbol nt && nt.TypeKind == TypeKind.Interface).ToList();
                if (interfaces.Count > 0)
                {
                    WriteTrivia(node.ColonToken.LeadingTrivia, squelch: true);
                    WriteToken(interfaceKeyword);
                    WriteTrivia(node.ColonToken.TrailingTrivia, squelch: true);

                    for (int i = 0; i < interfaces.Count; i++)
                    {
                        var iface = interfaces[i];
                        if (i > 0)
                            WriteToken(",");
                        VisitType(iface.Type);
                    }
                }
            }

            private void WriteDeclarationModifiers(SyntaxTokenList modifiers, bool isTypeMember)
            {
                var hasAccessModifiers = modifiers.Any(m => IsAccessModifier(m));
                var hasModifiers = modifiers.Count > 0;

                if (!hasAccessModifiers && isTypeMember)
                {
                    WriteToken("private");
                }

                foreach (var mod in modifiers)
                {
                    switch (mod.Kind())
                    {
                        case SyntaxKind.PublicKeyword:
                            // don't write public modifier (this is default)
                            // but write its leading trivia because it may include line start indentation
                            WriteTrivia(mod.LeadingTrivia);
                            break;

                        case SyntaxKind.PrivateKeyword:
                        case SyntaxKind.ProtectedKeyword:
                        case SyntaxKind.AbstractKeyword:
                        case SyntaxKind.VirtualKeyword:
                        case SyntaxKind.OverrideKeyword:
                        case SyntaxKind.ReadOnlyKeyword:
                            WriteToken(mod);
                            break;

                        case SyntaxKind.InternalKeyword:
                            WriteTrivia(mod.LeadingTrivia);
                            WriteToken("public");
                            WriteTrivia(mod.TrailingTrivia);
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
                WriteDeclarationModifiers(node.Modifiers, isTypeMember: node.Parent is TypeDeclarationSyntax);

                WriteToken(node.Identifier.LeadingTrivia, "constructor", node.Identifier.TrailingTrivia);

                Visit(node.ParameterList);

                if (node.ExpressionBody != null)
                {
                    VisitExpressionBody(node.ExpressionBody, node.SemicolonToken);
                }
                else
                {
                    Visit(node.Body);
                    WriteToken(node.SemicolonToken);
                }
            }

            private void VisitExpressionBody(ArrowExpressionClauseSyntax body, SyntaxToken semicolonToken)
            {
                WriteTrivia(body.Expression.GetLeadingTrivia(), squelch: true);
                WriteTrivia("{");
                Visit(body.Expression);
                WriteToken(semicolonToken);
                WriteTrivia("}");
            }

            public override void VisitMethodDeclaration(MethodDeclarationSyntax node)
            {
                WriteTrivia(First(node.Modifiers.GetLeadingTrivia(), node.ReturnType.GetLeadingTrivia(), node.Identifier.LeadingTrivia), squelch: true);

                WriteDeclarationModifiers(node.Modifiers, isTypeMember: node.Parent is TypeDeclarationSyntax);
                WriteTrivia(node.ReturnType.GetLeadingTrivia());
                WriteToken(node.Identifier);


                if (!IsVoidType(node.ReturnType))
                {
                    var tt = node.ParameterList.GetTrailingTrivia();
                    Squelch(tt);
                    Visit(node.ParameterList);

                    WriteToken(":");
                    Squelch(node.ReturnType.GetTrailingTrivia());
                    VisitType(node.ReturnType);

                    Unsquelch(tt);
                    WriteTrivia(tt);
                }
                else
                {
                    Visit(node.ParameterList);
                }

                if (node.ExpressionBody != null)
                {
                    VisitExpressionBody(node.ExpressionBody, node.SemicolonToken);
                }
                else
                {
                    Visit(node.Body);
                    WriteToken(node.SemicolonToken);
                }
            }

            public override void VisitParameterList(ParameterListSyntax node)
            {
                WriteToken(node.OpenParenToken);
                VisitList(node.Parameters);
                WriteToken(node.CloseParenToken);
            }

            public override void VisitParameter(ParameterSyntax node)
            {
                WriteTrivia(First(node.Modifiers.GetLeadingTrivia(), node.Type?.GetLeadingTrivia(), node.Identifier.LeadingTrivia), squelch: true);

                WriteParameterModifiers(node.Modifiers);
                WriteToken(node.Identifier);

                if (node.Type != null)
                {
                    WriteToken(":");
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
                            WriteTrivia(mod.LeadingTrivia);
                            WriteToken("...");
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
                    WriteDeclarationModifiers(node.Modifiers, isTypeMember: node.Parent is TypeDeclarationSyntax);
                    WriteToken(declarator.Identifier.Text);
                    WriteToken(":");
                    VisitType(node.Declaration.Type);
                    Visit(declarator.Initializer);
                }

                WriteToken(node.SemicolonToken);
            }
            #endregion

            #region Statements
            public override void VisitBlock(BlockSyntax node)
            {
                WriteToken(node.OpenBraceToken);

                foreach (var statement in node.Statements)
                {
                    Visit(statement);
                }

                WriteToken(node.CloseBraceToken);
            }

            public override void VisitExpressionStatement(ExpressionStatementSyntax node)
            {
                Visit(node.Expression);
                WriteToken(node.SemicolonToken);
            }

            public override void VisitLocalDeclarationStatement(LocalDeclarationStatementSyntax node)
            {
                var leadingTrivia = First(node.Modifiers.GetLeadingTrivia(), node.Declaration.Type.GetLeadingTrivia(), node.Declaration.Variables[0].Identifier.LeadingTrivia);

                WriteTrivia(leadingTrivia, squelch: true);
                Visit(node.Declaration);

                WriteToken(node.SemicolonToken);
            }

            public override void VisitVariableDeclaration(VariableDeclarationSyntax node)
            {
                var leadingTrivia = First(node.Type.GetLeadingTrivia(), node.Variables[0].Identifier.LeadingTrivia);

                WriteTrivia(leadingTrivia, squelch: true);
                WriteToken("let");

                VisitList(node.Variables);
            }

            public override void VisitVariableDeclarator(VariableDeclaratorSyntax node)
            {
                if (node.Initializer != null)
                {
                    WriteToken(node.Identifier);
                    Visit(node.Initializer);
                }
                else
                {
                    Squelch(node.Identifier.TrailingTrivia);
                    WriteToken(node.Identifier);

                    if (node.Parent is VariableDeclarationSyntax vd)
                    {
                        var defValue = GetDefaultForType(vd.Type);

                        if (defValue == "null")
                        {
                            WriteToken(":");
                            Visit(vd.Type);
                        }

                        WriteToken("=");
                        WriteToken(defValue);
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
                            return "0";
                    }
                }

                return "null";
            }

            public override void VisitEqualsValueClause(EqualsValueClauseSyntax node)
            {
                WriteToken(node.EqualsToken);
                Visit(node.Value);
            }

            public override void VisitAssignmentExpression(AssignmentExpressionSyntax node)
            {
                Visit(node.Left);
                WriteToken(node.OperatorToken);
                Visit(node.Right);
            }

            public override void VisitIfStatement(IfStatementSyntax node)
            {
                WriteToken(node.IfKeyword);
                WriteToken(node.OpenParenToken);
                Visit(node.Condition);
                WriteToken(node.CloseParenToken);
                Visit(node.Statement);
                Visit(node.Else);
            }

            public override void VisitElseClause(ElseClauseSyntax node)
            {
                WriteToken(node.ElseKeyword);
                Visit(node.Statement);
            }

            public override void VisitSwitchStatement(SwitchStatementSyntax node)
            {
                WriteToken(node.SwitchKeyword);
                WriteToken(node.OpenParenToken);
                Visit(node.Expression);
                WriteToken(node.CloseParenToken);
                WriteToken(node.OpenBraceToken);
                VisitList(node.Sections);
                WriteToken(node.CloseBraceToken);
            }

            public override void VisitSwitchSection(SwitchSectionSyntax node)
            {
                VisitList(node.Labels);
                VisitList(node.Statements);
            }

            public override void VisitCaseSwitchLabel(CaseSwitchLabelSyntax node)
            {
                WriteToken(node.Keyword);
                Visit(node.Value);
                WriteToken(node.ColonToken);
            }

            public override void VisitDefaultSwitchLabel(DefaultSwitchLabelSyntax node)
            {
                WriteToken(node.Keyword);
                WriteToken(node.ColonToken);
            }

            public override void VisitBreakStatement(BreakStatementSyntax node)
            {
                WriteToken(node.BreakKeyword);
                WriteToken(node.SemicolonToken);
            }

            public override void VisitContinueStatement(ContinueStatementSyntax node)
            {
                WriteToken(node.ContinueKeyword);
                WriteToken(node.SemicolonToken);
            }

            public override void VisitReturnStatement(ReturnStatementSyntax node)
            {
                WriteToken(node.ReturnKeyword);
                Visit(node.Expression);
                WriteToken(node.SemicolonToken);
            }

            public override void VisitWhileStatement(WhileStatementSyntax node)
            {
                WriteToken(node.WhileKeyword);
                WriteToken(node.OpenParenToken);
                Visit(node.Condition);
                WriteToken(node.CloseParenToken);
                VisitBlock(node.Statement);
            }

            public override void VisitDoStatement(DoStatementSyntax node)
            {
                WriteToken(node.DoKeyword);
                VisitBlock(node.Statement);
                WriteToken(node.WhileKeyword);
                WriteToken(node.OpenParenToken);
                Visit(node.Condition);
                WriteToken(node.CloseParenToken);
                WriteToken(node.SemicolonToken);
            }

            public override void VisitForStatement(ForStatementSyntax node)
            {
                WriteToken(node.ForKeyword);
                WriteToken(node.OpenParenToken);

                Visit(node.Declaration);  // either, or?
                VisitList(node.Initializers);
                WriteToken(node.FirstSemicolonToken);

                Visit(node.Condition);
                WriteToken(node.SecondSemicolonToken);

                VisitList(node.Incrementors);
                WriteToken(node.CloseParenToken);

                VisitBlock(node.Statement);
            }

            public override void VisitForEachStatement(ForEachStatementSyntax node)
            {
                WriteToken(node.ForEachKeyword.LeadingTrivia, "for", node.ForEachKeyword.TrailingTrivia);
                WriteToken(node.OpenParenToken);

                WriteToken("let");

                if (!IsVarType(node.Type))
                {
                    Squelch(node.Identifier.TrailingTrivia);
                    WriteToken(node.Identifier);
                    WriteToken(":");
                    VisitType(node.Type);
                }
                else
                {
                    WriteToken(node.Identifier);
                }

                WriteToken("of");
                Visit(node.Expression);

                WriteToken(node.CloseParenToken);

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
                    WriteToken("{");
                    Visit(statement);
                    WriteToken("}");
                }
            }
            #endregion

            #region Types
            public override void VisitArrayType(ArrayTypeSyntax node)
            {
                Visit(node.ElementType);
                VisitList(node.RankSpecifiers);
            }

            public override void VisitNullableType(NullableTypeSyntax node)
            {
                Visit(node.ElementType);
                WriteToken("|");
                WriteToken("null");
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
                        WriteToken(node.Keyword.LeadingTrivia, "number", node.Keyword.TrailingTrivia);
                        break;

                    case SyntaxKind.CharKeyword:
                        WriteToken(node.Keyword.LeadingTrivia, "string", node.Keyword.TrailingTrivia);
                        break;

                    case SyntaxKind.ObjectKeyword:
                    case SyntaxKind.StringKeyword:
                    case SyntaxKind.VoidKeyword:
                        WriteToken(node.Keyword);
                        break;

                    default:
                        _diagnostics.Add(GetSyntaxNotSupported(node.GetLocation(), node.ToString()));
                        WriteToken(node.ToString());
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
                                WriteToken("boolean");
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
                                WriteToken("number");
                                return;

                            case SpecialType.System_Object:
                                WriteToken("object");
                                return;

                            case SpecialType.System_String:
                            case SpecialType.System_Char:
                                WriteToken("string");
                                return;
                        }

                        if (nt.IsGenericType)
                        {
                            var name = nt.ConstructedFrom.Name;
                            switch (name)
                            {
                                case "Nullable":
                                    WriteType(nt.TypeArguments[0]);
                                    WriteToken("|");
                                    WriteToken("null");
                                    return;

                                case "Func":
                                    WriteToken("(");
                                    for (int i = 0; i < nt.TypeArguments.Length - 1; i++)
                                    {
                                        var t = nt.TypeArguments[i];
                                        if (i > 0)
                                            WriteToken(",");
                                        WriteToken("arg" + i);
                                        WriteToken(":");
                                        WriteType(t);
                                    }
                                    WriteToken(")");
                                    WriteToken("=>");
                                    WriteType(nt.TypeArguments[nt.TypeArguments.Length - 1]);
                                    return;

                                case "Action":
                                    WriteToken("(");
                                    for (int i = 0; i < nt.TypeArguments.Length; i++)
                                    {
                                        var t = nt.TypeArguments[i];
                                        if (i > 0)
                                            WriteToken(",");
                                        WriteToken("arg" + i);
                                        WriteToken(":");
                                        WriteType(t);
                                    }
                                    WriteToken(")");
                                    WriteToken("=>");
                                    WriteToken("void");
                                    return;
                            }
                        }

                        WriteToken(nt.Name);
                        break;

                    case IArrayTypeSymbol at:
                        WriteType(at.ElementType);
                        WriteToken("[]"); // TODO: multi-dimensional arrays?
                        break;

                    case ITypeParameterSymbol tp:
                        WriteToken(tp.Name);
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

            public void VisitType(ExpressionSyntax type)
            {
                var symbol = GetSymbol(type);
                if (symbol is ITypeSymbol ts && IsSpecialType(ts))
                {
                    WriteTrivia(type.GetLeadingTrivia());
                    WriteType(ts);
                    WriteTrivia(type.GetTrailingTrivia());
                }
                else
                {
                    Visit(type);
                }
            }

            private void VisitTypeList(SeparatedSyntaxList<TypeSyntax> list)
            {
                foreach (var nodeOrToken in list.GetWithSeparators())
                {
                    if (nodeOrToken.IsNode)
                    {
                        VisitType((TypeSyntax)nodeOrToken.AsNode());
                    }
                    else
                    {
                        WriteToken(nodeOrToken.AsToken());
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
                        WriteToken(node.Token);
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
                            || text.EndsWith("d", StringComparison.OrdinalIgnoreCase))
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
                            WriteToken(GetStringLiteral(node.Token.ValueText));
                        }
                        else
                        {
                            WriteToken(node.Token);
                        }
                        break;

                    case SyntaxKind.CharacterLiteralExpression:
                        WriteToken(GetStringLiteral(node.Token.ValueText));
                        break;

                    default:
                        _diagnostics.Add(GetSyntaxNotSupported(node.GetLocation(), node.ToString()));
                        WriteToken(node.ToString());
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
                WriteToken(node.OpenBracketToken);
                VisitList(node.Sizes);
                WriteToken(node.CloseBracketToken);
            }

            public override void VisitOmittedArraySizeExpression(OmittedArraySizeExpressionSyntax node)
            {
                // no nothing
            }

            public override void VisitIdentifierName(IdentifierNameSyntax node)
            {
                WriteToken(node.Identifier);
            }

            public override void VisitQualifiedName(QualifiedNameSyntax node)
            {
                Visit(node.Left);
                WriteToken(node.DotToken);
                Visit(node.Right);
            }

            public override void VisitGenericName(GenericNameSyntax node)
            {
                WriteToken(node.Identifier);
                Visit(node.TypeArgumentList);
            }

            public override void VisitTypeArgumentList(TypeArgumentListSyntax node)
            {
                WriteToken(node.LessThanToken);
                VisitTypeList(node.Arguments);
                WriteToken(node.GreaterThanToken);
            }

            public override void VisitOmittedTypeArgument(OmittedTypeArgumentSyntax node)
            {
                // do nothing
            }

            public override void VisitMemberAccessExpression(MemberAccessExpressionSyntax node)
            {
                Visit(node.Expression);
                WriteToken(node.OperatorToken);
                Visit(node.Name);
            }

            public override void VisitParenthesizedExpression(ParenthesizedExpressionSyntax node)
            {
                WriteToken(node.OpenParenToken);
                Visit(node.Expression);
                WriteToken(node.CloseParenToken);
            }

            public override void VisitBinaryExpression(BinaryExpressionSyntax node)
            {
                Visit(node.Left);
                WriteToken(node.OperatorToken);
                Visit(node.Right);
            }

            public override void VisitPrefixUnaryExpression(PrefixUnaryExpressionSyntax node)
            {
                WriteToken(node.OperatorToken);
                Visit(node.Operand);
            }

            public override void VisitPostfixUnaryExpression(PostfixUnaryExpressionSyntax node)
            {
                Visit(node.Operand);
                WriteToken(node.OperatorToken);
            }

            public override void VisitConditionalExpression(ConditionalExpressionSyntax node)
            {
                Visit(node.Condition);
                WriteToken(node.QuestionToken);
                Visit(node.WhenTrue);
                WriteToken(node.ColonToken);
                Visit(node.WhenFalse);
            }

            public override void VisitInvocationExpression(InvocationExpressionSyntax node)
            {
                Visit(node.Expression);
                Visit(node.ArgumentList);
            }

            public override void VisitArgumentList(ArgumentListSyntax node)
            {
                WriteToken(node.OpenParenToken);
                VisitList(node.Arguments);
                WriteToken(node.CloseParenToken);
            }

            public override void VisitArgument(ArgumentSyntax node)
            {
                // TODO: handle ref arguments
                Visit(node.Expression);
            }

            public override void VisitSimpleLambdaExpression(SimpleLambdaExpressionSyntax node)
            {
                Visit(node.Parameter);
                WriteToken(node.ArrowToken);
                Visit(node.Body);
            }

            public override void VisitParenthesizedLambdaExpression(ParenthesizedLambdaExpressionSyntax node)
            {
                Visit(node.ParameterList);
                WriteToken(node.ArrowToken);
                Visit(node.Body);
            }
            #endregion
        }
    }
}