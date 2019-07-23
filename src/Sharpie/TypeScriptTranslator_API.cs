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
using R = System.Reflection;

namespace Sharpie
{
    public partial class TypeScriptTranslator
    {
        private partial class Translator : CSharpSyntaxVisitor
        {
            private abstract class Translation
            {
                public abstract ISymbol Symbol { get; }
                public abstract void Translate(SyntaxNode syntax);
            }

            private class Translation<TSyntax> : Translation where TSyntax : SyntaxNode
            {
                public override ISymbol Symbol { get; }
                public Action<TSyntax> Translator;

                public Translation(ISymbol symbol, Action<TSyntax> translator)
                {
                    this.Symbol = symbol;
                    this.Translator = translator;
                }

                public override void Translate(SyntaxNode syntax)
                {
                    this.Translator((TSyntax)syntax);
                }
            }

            private Translation GetTranslator(ISymbol symbol)
            {
                InitTranslations();

                if (symbol is IMethodSymbol method)
                {
                    if (method.IsGenericMethod)
                    {
                        symbol = method.ConstructedFrom;
                    }
                }

                _translations.TryGetValue(symbol, out var translation);
                return translation;
            }

            private Translation TranslateInvocation(E.Expression<Func<object>> lambda, Action<InvocationExpressionSyntax> translator)
            {
                return new Translation<InvocationExpressionSyntax>(GetSymbol(_compilation, lambda), translator);
            }

            private Translation TranslateMemberAccess(E.Expression<Func<object>> lambda, Action<MemberAccessExpressionSyntax> translator)
            {
                return new Translation<MemberAccessExpressionSyntax>(GetSymbol(_compilation, lambda), translator);
            }

            private static ISymbol GetSymbol(Compilation compilation, E.Expression<Func<object>> expr)
            {
                var body = expr.Body;

                if (body is E.UnaryExpression ue && ue.NodeType == E.ExpressionType.Convert)
                    body = ue.Operand;

                if (body is E.MethodCallExpression mc)
                {
                    // get base definition if any generic arguments
                    var meth = mc.Method.GetBaseDefinition();
                    return SymbolMapper.GetMethodSymbol(compilation, meth);
                }
                else if (body is E.MemberExpression mx)
                {
                    if (mx.Member is R.PropertyInfo pi)
                    {
                        return SymbolMapper.GetPropertySymbol(compilation, pi);
                    }
                }

                return null;
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

            private Dictionary<ISymbol, Translation> _translations;

            private void InitTranslations()
            {
                if (_translations != null)
                    return;

                _translations = new Translation[]
                {
                    TranslateMemberAccess(() => "".Length,
                        ma => Write(ma.Expression, ma.OperatorToken, "length")),

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
                }.ToDictionary(t => t.Symbol);
            }
        }
    }
}