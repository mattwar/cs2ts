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
    public partial class TypeScriptTranslator
    {
        private partial class Translator : CSharpSyntaxVisitor
        {
            private Action<InvocationExpressionSyntax> GetInvocationTranslator(IMethodSymbol method)
            {
                InitTranslations();

                if (method.IsGenericMethod)
                {
                    method = method.ConstructedFrom;
                }

                var its = _invocationTranslations[method.Name];

                var it = its.FirstOrDefault(m => m.Symbol.Equals(method));
                return it?.Translator;
            }

            private InvocationTranslation TranslateInvocation(E.Expression<Func<object>> lambda, Action<InvocationExpressionSyntax> translator)
            {
                var method = GetExampleMethod(_compilation, lambda);
                return new InvocationTranslation(method, translator);
            }

            private static IMethodSymbol GetExampleMethod(Compilation compilation, E.Expression<Func<object>> expr)
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
        }
    }
}