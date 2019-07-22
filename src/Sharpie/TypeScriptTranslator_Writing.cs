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
            /// <summary>
            /// Gets the first non-empty <see cref="SyntaxTriviaList"/>
            /// </summary>
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

            /// <summary>
            /// Writes the token text and trivia as typescript.
            /// </summary>
            public void Write(SyntaxToken token)
            {
                Write(token.LeadingTrivia);
                Write(token.Text);
                Write(token.TrailingTrivia);
            }

            /// <summary>
            /// Write the text (token or trivia) as typescript.
            /// </summary>
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

            /// <summary>
            /// Write the trivia & token as typescript.
            /// </summary>
            public void WriteToken(SyntaxTriviaList leadingTrivia, string token, SyntaxTriviaList trailingTrivia)
            {
                Write(leadingTrivia);
                Write(token);
                Write(trailingTrivia);
            }

            /// <summary>
            /// Write one or more nodes, tokens, trivia or text as typescript.
            /// Nodes will be visited; tokens, trivia or text written directly.
            /// </summary>
            public void Write(params object[] nodesAndTokens)
            {
                foreach (var nodeOrToken in nodesAndTokens)
                {
                    Write(nodeOrToken);
                }
            }

            /// <summary>
            /// Write one node, token, trivia or text as typescript.
            /// Nodes will be visited; tokens, trivia or text written directly.
            /// </summary>
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

            /// <summary>
            /// Write an line ending.
            /// </summary>
            public void WriteLine()
            {
                _writer.WriteLine();
            }

            /// <summary>
            /// Write trivia
            /// </summary>
            /// <param name="list">The trivia to write.</param>
            /// <param name="squelch">If true, then any request to write this trivia again will be ignored.</param>
            /// <param name="lastLineOnly">If true, only the last line of the trivia is written.</param>
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

            /// <summary>
            /// Returns the starting text position of the beginning of the last line in the trivia.
            /// </summary>
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
            /// Gets the leading trivia from the node or token.
            /// </summary>
            private static SyntaxTriviaList GetLeadingTrivia(object nodeOrToken)
            {
                switch (nodeOrToken)
                {
                    case SyntaxNode n:
                        return n.GetLeadingTrivia();
                    case SyntaxToken t:
                        return t.LeadingTrivia;
                    default:
                        return SyntaxTriviaList.Empty;
                }
            }

            /// <summary>
            /// .Gets the trailing trivia from the node or token.
            /// </summary>
            private static SyntaxTriviaList GetTrailingTrivia(object nodeOrToken)
            {
                switch (nodeOrToken)
                {
                    case SyntaxNode n:
                        return n.GetTrailingTrivia();
                    case SyntaxToken t:
                        return t.TrailingTrivia;
                    default:
                        return SyntaxTriviaList.Empty;
                }
            }

            /// <summary>
            /// Wraps the node with before & after tokens.
            /// The node's leading trivia occurs before the before-text and the trailing trivia after the after-text.
            /// </summary>
            private void Wrap(string before, object nodeOrToken, string after)
            {
                var lt = GetLeadingTrivia(nodeOrToken);
                var tt = GetTrailingTrivia(nodeOrToken);

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

                Write(nodeOrToken);

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

            /// <summary>
            /// Returns true if the specific trivia list has been squelched.
            /// </summary>
            public bool IsSquelched(SyntaxTriviaList list)
            {
                return _squelchedTrivia.Contains(list.FullSpan.Start);
            }

            /// <summary>
            /// Squelch the leading and trialing trivia of the node.
            /// </summary>
            public SyntaxNode Squelch(SyntaxNode node)
            {
                Squelch(node.GetLeadingTrivia());
                Squelch(node.GetTrailingTrivia());
                return node;
            }

            /// <summary>
            /// Squelch the leading and trailing trivia of the token.
            /// </summary>
            /// <param name="token"></param>
            /// <returns></returns>
            public SyntaxToken Squelch(SyntaxToken token)
            {
                Squelch(token.LeadingTrivia);
                Squelch(token.TrailingTrivia);
                return token;
            }
        }
    }
}