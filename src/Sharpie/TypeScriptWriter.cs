using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Microsoft.CodeAnalysis;

namespace Sharpie
{
    public class TypeScriptWriter
    {
        private readonly TextWriter _writer;
        private string _last = " ";

        public TypeScriptWriter(TextWriter writer)
        {
            _writer = writer;
        }

        public void WriteToken(string token)
        {
            if (token.Length > 0)
            {
                if (RequiresInjectedSpace(_last, token))
                {
                    _writer.Write(" ");
                }

                _writer.Write(token);
                _last = token;
            }
        }

        private static bool RequiresInjectedSpace(string prev, string next)
        {
            if (prev.Length == 0 || EndsInWhitespace(prev))
                return false;

            if (prev == next)
            {
                // tokens with doubled versions
                switch (prev)
                {
                    case "+":
                    case "-":
                    case "&":
                    case "|":
                    case "=":
                    case "<":
                    case ">":
                        return true;
                }
            }

            var prevIsPunctuationOnly = IsPunctuationOnly(prev);
            var nextIsPunctuationOnly = IsPunctuationOnly(next);

            // both are keywords/identifiers/literals?
            if (!prevIsPunctuationOnly && !nextIsPunctuationOnly)
                return true;

            switch (prev)
            {
                case ":":
                case ";":
                case ",":
                case "=":
                case "==":
                case "!=":
                case "<=":
                case ">=":
                case "*":
                case "/":
                case "|":
                case "&":
                case "&&":
                case "||":
                case "+=":
                case "-=":
                case "*=":
                case "/=":
                case "%=":
                case "&=":
                case "|=":
                case "^=":
                case "=>":
                case "{":
                case "}":
                    return true;
            }

            switch (next)
            {
                case "=":
                case "==":
                case "!=":
                case "<=":
                case ">=":
                case "*":
                case "/":
                case "|":
                case "&":
                case "&&":
                case "||":
                case "+=":
                case "-=":
                case "*=":
                case "/=":
                case "%=":
                case "&=":
                case "|=":
                case "^=":
                case "=>":
                case "{":
                case "}":
                    return true;
            }

            return false;
        }

        private static bool IsIdentifierOrKeyword(string text)
        {
            return text.All(c => char.IsLetterOrDigit(c) || c == '_');
        }

        private static bool IsPunctuationOnly(string text)
        {
            return text.All(IsPunctuation);
        }

        private static bool IsPunctuation(char c)
        {
            return !char.IsLetterOrDigit(c) && c != '_';
        }

        private static bool EndsInWhitespace(string text)
        {
            return text.Length > 0 && char.IsWhiteSpace(text[text.Length - 1]);
        }

        private string newLine;

        public void WriteTrivia(string trivia)
        {
            if (trivia.Length > 0)
            {
                _writer.Write(trivia);
                _last = trivia;


                if (newLine == null)
                {
                    // discover new line based on first written line break
                    var i = trivia.IndexOf('\n');
                    if (i >= 0)
                    {
                        if (i > 0 && trivia[i - 1] == '\r')
                        {
                            newLine = "\r\n";
                        }
                        else
                        {
                            newLine = "\n";
                        }
                    }
                }
            }
        }

        public void WriteLine()
        {
            if (newLine == null)
            {
                _writer.WriteLine();
            }
            else
            {
                _writer.Write(newLine);
            }
        }
    }
}