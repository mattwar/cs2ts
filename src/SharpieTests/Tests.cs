using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;

namespace SharpieTests
{
    [TestClass]
    public class Tests
    {
        #region Expressions
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
            TestExpression("'a'", "\"a\"");
            TestExpression("'\\r'", "\"\\r\"");
            TestExpression("'\\n'", "\"\\n\"");
            TestExpression("'\\t'", "\"\\t\"");
            TestExpression("'\\\\'", "\"\\\\\"");
            TestExpression("'\\\"'", "\"\\\"\"");
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
        public void TestBuiltInTypes()
        {
            TestType("int", "number");
            TestType("uint", "number");
            TestType("short", "number");
            TestType("ushort", "number");
            TestType("long", "number");
            TestType("ulong", "number");
            TestType("byte", "number");
            TestType("sbyte", "number");
            TestType("float", "number");
            TestType("double", "number");
            TestType("decimal", "number");
            TestType("bool", "boolean");
            TestType("object", "object");
            TestType("string", "string");
            TestType("char", "string");
        }

        [TestMethod]
        public void TestSystemPrimitiveTypes()
        {
            TestType("System.Int32", "number");
            TestType("System.UInt32", "number");
            TestType("System.Int16", "number");
            TestType("System.UInt16", "number");
            TestType("System.Int64", "number");
            TestType("System.UInt64", "number");
            TestType("System.Byte", "number");
            TestType("System.SByte", "number");
            TestType("System.Single", "number");
            TestType("System.Double", "number");
            TestType("System.Decimal", "number");
            TestType("System.Boolean", "boolean");
            TestType("System.Object", "object");
            TestType("System.String", "string");
            TestType("System.Char", "string");
        }

        [TestMethod]
        public void TestArrayTypes()
        {
            TestType("a[]", "a[]");
        }

        [TestMethod]
        public void TestNullableTypes()
        {
            TestType("a?", "a | null");
        }

        [TestMethod]
        public void TestFuncTypes()
        {
            TestType("Func<int>", "() => number");
            TestType("Func<A0, R>", "(arg0: A0) => R");
            TestType("Func<A0, A1, R>", "(arg0: A0, arg1: A1) => R");
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
        #endregion

        #region Statements
        [TestMethod]
        public void TestBlock()
        {
            TestStatement("{ }", "{ }");
            TestStatement("{\n}", "{\n}");
        }

        [TestMethod]
        public void TestExpressionStatement()
        {
            TestStatement("a;", "a;");
            TestStatement("m(a);", "m(a);");
        }

        [TestMethod]
        public void TestLocalDeclarationStatement()
        {
            TestStatement("int x = 10;", "let x = 10;");
            TestStatement("var x = 10;", "let x = 10;");
            TestStatement("int x;", "let x = 0;");
            TestStatement("T x;", "let x: T = null;");
            TestStatement("string x;", "let x: string = null;");
            TestStatement("var x = \"abc\";", "let x = \"abc\";");
        }

        [TestMethod]
        public void TestIfStatement()
        {
            TestStatement("if (a) s;", "if (a) s;");
            TestStatement("if (a) { s; }", "if (a) { s; }");
            TestStatement("if (a) { s1; } else { s2; }", "if (a) { s1; } else { s2; }");
            TestStatement("if (a) { s1; } else if (b) { s2; } else { s3; }", "if (a) { s1; } else if (b) { s2; } else { s3; }");
        }

        [TestMethod]
        public void TestSwitchStatement()
        {
            TestStatement("switch (e) { case 1: s1; break; }", "switch (e) { case 1: s1; break; }");
            TestStatement("switch (e) { case 1: s1; break; default: break; }", "switch (e) { case 1: s1; break; default: break; }");
        }

        [TestMethod]
        public void TestWhileStatement()
        {
            TestStatement("while (e) { s; }", "while (e) { s; }");
            TestStatement("while (e) s;", "while (e) {s;}");
        }

        [TestMethod]
        public void TestDoStatement()
        {
            TestStatement("do { s; } while (e);", "do { s; } while (e);");
        }

        [TestMethod]
        public void TestForStatement()
        {
            TestStatement("for (int x = 0; x < 10; x++) { }", "for (let x = 0; x < 10; x++) { }");
            TestStatement("for (int x = 0; x < 10; x++) s;", "for (let x = 0; x < 10; x++) {s;}");
        }

        [TestMethod]
        public void TestForEachStatement()
        {
            TestStatement("foreach (var x in y) { }", "for (let x of y) { }");
            TestStatement("foreach (int x in y) { }", "for (let x: number of y) { }");
        }
        #endregion

        private void TestExpression(string csharpExpression, string expectedTypeScriptExpression)
        {
            var actualTypeScriptExpression = TypeScriptTranslator.TranslateExpression(csharpExpression, s_references);
            Assert.AreEqual(expectedTypeScriptExpression, actualTypeScriptExpression);
        }

        private void TestStatement(string csharpStatement, string expectedTypeScriptStatement)
        {
            var actualTypeScriptStatement = TypeScriptTranslator.TranslateStatement(csharpStatement, s_references);
            Assert.AreEqual(expectedTypeScriptStatement, actualTypeScriptStatement);
        }

        private void TestType(string csharpType, string expectedTypeScriptType)
        {
            var actualTypeScriptType = TypeScriptTranslator.TranslateType(csharpType, s_references);
            Assert.AreEqual(expectedTypeScriptType, actualTypeScriptType);
        }

        private static IReadOnlyList<MetadataReference> s_references =
            new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
    }
}
