using Microsoft.CodeAnalysis;

namespace Sharpie
{
    public static class SyntaxExtensions
    {
        public static SyntaxTriviaList GetLeadingTrivia(this SyntaxTokenList list)
        {
            if (list.Count == 0)
            {
                return default(SyntaxTriviaList);
            }
            else
            {
                return list[0].LeadingTrivia;
            }
        }

        public static SyntaxTriviaList GetTrailingTrivia(this SyntaxTokenList list)
        {
            if (list.Count == 0)
            {
                return default(SyntaxTriviaList);
            }
            else
            {
                return list[list.Count - 1].TrailingTrivia;
            }
        }
    }
}