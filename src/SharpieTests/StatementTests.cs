using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;

namespace SharpieTests
{
    [TestClass]
    public class StatementTests : TestBase
    {
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
            TestStatement("A a = null;", "let a: A = null;");
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
            TestStatement("while (e) s;", "while (e) { s; }");
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
            TestStatement("for (int x = 0; x < 10; x++) s;", "for (let x = 0; x < 10; x++) { s; }");
        }

        [TestMethod]
        public void TestForEachStatement()
        {
            TestStatement("foreach (var x in y) { }", "for (let x of y) { }");
            TestStatement("foreach (int x in y) { }", "for (let x: number of y) { }");
        }
    }
}