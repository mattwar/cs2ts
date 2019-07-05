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
            TestExpression("a<b>()", "a<b>()");
            TestExpression("a<b, c>()", "a<b, c>()");
        }

        [TestMethod]
        public void TestQualifiedNames()
        {
            TestExpression("a.b", "a.b");
            TestExpression("a.b.c", "a.b.c");
            //TestExpression("a<b>.c<d>", "a<b>.c<d>");
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
            TestExpression("0f", "0");
            TestExpression("0m", "0");
            TestExpression("0.0d", "0.0");
            TestExpression("0.0f", "0.0");
            TestExpression("0.0m", "0.0");
            TestExpression("0L", "0");
            TestExpression("0UL", "0");
            TestExpression("0.0D", "0.0");
            TestExpression("0.0F", "0.0");
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
            TestExpression("x => x", "x => x");
            TestExpression("() => 0", "() => 0");
            TestExpression("(x) => x", "(x) => x");
            TestExpression("(a, b) => a + b", "(a, b) => a + b");
            TestExpression("(int x) => x", "(x: number) => x");

            // with statement body
            TestExpression("x => { return x; }", "x => { return x; }");
            TestExpression("() => { return 0; }", "() => { return 0; }");
            TestExpression("(x) => { return x; }", "(x) => { return x; }");
            TestExpression("(a, b) => { return a + b; }", "(a, b) => { return a + b; }");
            TestExpression("(int x) => { return x; }", "(x: number) => { return x; }");
        }

        [TestMethod]
        public void TestAnonymousMethodExpression()
        {
            TestExpression("delegate (int x) { return x; }", "(x: number) => { return x; }");
        }

        [TestMethod]
        public void TestArrayCreationExpression()
        {
            TestExpression("new int[] {1, 2, 3}", "[1, 2, 3]");
            TestExpression("new string[] {\"one\", \"two\", \"three\"}", "[\"one\", \"two\", \"three\"]");
            TestExpression("new object[] {1, \"two\", 3.0}", "[1, \"two\", 3.0]");

            // whitespace
            TestExpression("new int[] { 1, 2, 3 }", "[ 1, 2, 3 ]");

            // implicit typed
            TestExpression("new [] {1, 2, 3}", "[1, 2, 3]");
            TestExpression("new [] {\"one\", \"two\", \"three\"}", "[\"one\", \"two\", \"three\"]");
        }

        [TestMethod]
        public void TestObjectCreationExpression()
        {
            TestExpression("new C()", "new C()");
            TestExpression("new C(1, 2, 3)", "new C(1, 2, 3)");
        }

        [TestMethod]
        public void TestCastExpression()
        {
            // numeric
            TestExpression("(int)3.5", "Math.floor(3.5)");
            TestExpression("(int)3.5f", "Math.floor(3.5)");
            TestExpression("(float)3", "3");
            TestExpression("(double)3", "3");
            TestExpression("(int)3", "3");
            TestExpression("(long)3", "3");

            // non-primitive type conversion: should be okay for upcast/downcast/interface casting
            TestExpression("(C)b", "<C>b");
            TestCompilationUnit("class C { public void M() { C a = (C)b; } }", "class C { M() { let a = <C>b; } }");

            // identity
            TestCompilationUnit("class C { public void M() { C b = null; C a = (C)b; } }", "class C { M() { let b: C = null; let a = b; } }");
        }

        [TestMethod]
        public void TestDefault()
        {
            TestStatement("int x = default;", "let x = 0;");
            TestStatement("double x = default;", "let x = 0;");
            TestStatement("string x = default;", "let x: string = null;");
            TestStatement("bool x = default;", "let x = false;");
            TestStatement("T x = default;", "let x: T = null;");
            TestStatement("T? x = default;", "let x: T | null = null;");
            TestStatement("int? x = default;", "let x: number | null = null;");

            TestStatement("var x = default(int);", "let x = 0;");
            TestStatement("var x = default(double);", "let x = 0;");
            TestStatement("var x = default(string);", "let x = <string>null;");
            TestStatement("var x = default(bool);", "let x = false;");
            TestStatement("var x = default(T);", "let x = <T>null;");
            TestStatement("var x = default(T?);", "let x = <T | null>null;");
            TestStatement("var x = default(int?);", "let x = <number | null>null;");
        }
    }
}
