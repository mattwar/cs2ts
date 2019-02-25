using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;
using static Sharpie.TypeScriptTranslator;

namespace SharpieTests
{
    [TestClass]
    public class ExpressionTests : TestBase
    {
        [TestMethod]
        public void TestIdentifierNames()
        {
            TestExpression("a", "a");
        }

        [TestMethod]
        public void TestGenericNames()
        {
            TestExpression("a<b>", "a<b>");
            TestExpression("a<b, c>", "a<b, c>");
        }

        [TestMethod]
        public void TestQualifiedNames()
        {
            TestExpression("a.b", "a.b");
            TestExpression("a.b.c", "a.b.c");
            TestExpression("a<b>.c<d>", "a<b>.c<d>");
        }

        [TestMethod]
        public void TestBooleanLiterals()
        {
            TestExpression("true", "true");
            TestExpression("false", "false");
        }

        [TestMethod]
        public void TestNullLiteral()
        {
            TestExpression("null", "null");
        }

        [TestMethod]
        public void TestNumericLiterals()
        {
            TestExpression("0", "0");
            TestExpression("1", "1");
            TestExpression("-1", "-1");
            TestExpression("100", "100");
            TestExpression("-100", "-100");
            TestExpression("1.0", "1.0");
            TestExpression("1.0e5", "1.0e5");
            TestExpression("1e5", "1e5");
            TestExpression("1e-5", "1e-5");

            // type suffix
            TestExpression("0l", "0");
            TestExpression("0ul", "0");
            TestExpression("0d", "0");
            TestExpression("0m", "0");
            TestExpression("0.0d", "0.0");
            TestExpression("0.0m", "0.0");
            TestExpression("0L", "0");
            TestExpression("0UL", "0");
            TestExpression("0.0D", "0.0");
            TestExpression("0.0M", "0.0");
        }

        [TestMethod]
        public void TestHexLiterals()
        {
            TestExpression("0xffff", "0xffff");
            TestExpression("0xfffful", "0xffff");
            TestExpression("0x0123456789ABCDEF", "0x0123456789ABCDEF");
            TestExpression("0x0UL", "0x0");
        }

        [TestMethod]
        public void TestBinaryLiterals()
        {
            TestExpression("0b01010101", "0b01010101");
            TestExpression("0b0101_0101", "0b01010101");
        }

        [TestMethod]
        public void TestStringLiterals()
        {
            TestExpression("\"abc\"", "\"abc\"");
            TestExpression("\"\\n\"", "\"\\n\"");
        }

        [TestMethod]
        public void TestVerbatimStringLiterals()
        {
            TestExpression("@\"abc\"", "\"abc\"");
        }

        [TestMethod]
        public void TestCharLiterals()
        {
            TestCharLiteral ("'a'", 'a');
            TestCharLiteral("'\\r'", '\r');
            TestCharLiteral("'\\n'", '\n');
            TestCharLiteral("'\\t'", '\t');
            TestCharLiteral("'\\\\'", '\\');
            TestCharLiteral("'\\\"'", '"');
        }

        private void TestCharLiteral(string csharpLiteral, char c)
        {
            TestExpression(csharpLiteral, ((ushort)c).ToString());
        }

        [TestMethod]
        public void TestBinaryExpressions()
        {
            TestExpression("a + b", "a + b");
            TestExpression("a - b", "a - b");
            TestExpression("a * b", "a * b");
            TestExpression("a / b", "a / b");
            TestExpression("a == b", "a == b");
            TestExpression("a != b", "a != b");
            TestExpression("a <= b", "a <= b");
            TestExpression("a >= b", "a >= b");
            TestExpression("a < b", "a < b");
            TestExpression("a > b", "a > b");
        }

        [TestMethod]
        public void TestUnaryExpressions()
        {
            TestExpression("+a", "+a");
            TestExpression("-a", "-a");
            TestExpression("!a", "!a");
            TestExpression("~a", "~a");
        }
        [TestMethod]
        public void TestMemberAccess()
        {
            TestExpression("a.b", "a.b");
        }

        [TestMethod]
        public void TestParenthesizedExpression()
        {
            TestExpression("(a)", "(a)");
            TestExpression("(((a)))", "(((a)))");
        }

        [TestMethod]
        public void TestInvocation()
        {
            TestExpression("m()", "m()");
            TestExpression("m(a)", "m(a)");
            TestExpression("m(a1, a2)", "m(a1, a2)");
            TestExpression("m<T>()", "m<T>()");
            TestExpression("m<int>()", "m<number>()");
        }

        [TestMethod]
        public void TestMethodNotSupported()
        {
            TestExpressionFails("System.Environment.Exit(0)", MethodNotSupported);
        }

        [TestMethod]
        public void TestAssignmentExpression()
        {
            TestExpression("x = y", "x = y");
            TestExpression("x += y", "x += y");
            TestExpression("x -= y", "x -= y");
            TestExpression("x *= y", "x *= y");
            TestExpression("x /= y", "x /= y");
            TestExpression("x %= y", "x %= y");
            TestExpression("x &= y", "x &= y");
            TestExpression("x |= y", "x |= y");
            TestExpression("x ^= y", "x ^= y");
        }

        [TestMethod]
        public void TestConditionalExpression()
        {
            TestExpression("a ? b : c", "a ? b : c");
        }

        [TestMethod]
        public void TestLambdaExpression()
        {
            TestExpression("x => y", "x => y");
            TestExpression("() => y", "() => y");
            TestExpression("(x) => y", "(x) => y");
            TestExpression("(a, b) => y", "(a, b) => y");
            TestExpression("(int x) => y", "(x: number) => y");
        }
    }
}
