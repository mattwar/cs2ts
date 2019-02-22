using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;

namespace SharpieTests
{
    public class TestBase
    {
        public void TestExpression(string csharpExpression, string expectedTypeScriptExpression)
        {
            var result = TypeScriptTranslator.TranslateExpression(csharpExpression, s_References);

            if (result.Diagnostics.Count > 0)
            {
                Assert.Fail("Translation Failure: " + result.Diagnostics[0].GetMessage());
            }

            Assert.AreEqual(expectedTypeScriptExpression, result.Text);
        }

        public void TestStatement(string csharpStatement, string expectedTypeScriptStatement)
        {
            var result = TypeScriptTranslator.TranslateStatement(csharpStatement, s_References);

            if (result.Diagnostics.Count > 0)
            {
                Assert.Fail("Translation Failure: " + result.Diagnostics[0].GetMessage());
            }

            Assert.AreEqual(expectedTypeScriptStatement, result.Text);
        }

        public void TestType(string csharpType, string expectedTypeScriptType)
        {
            var result = TypeScriptTranslator.TranslateType(csharpType, s_References);

            if (result.Diagnostics.Count > 0)
            {
                Assert.Fail("Translation Failure: " + result.Diagnostics[0].GetMessage());
            }

            Assert.AreEqual(expectedTypeScriptType, result.Text);
        }

        public void TestCompilationUnit(string csharpText, string expectedTypeScriptText)
        {
            var result = TypeScriptTranslator.TranslateCompilationUnit(csharpText, s_References);

            if (result.Diagnostics.Count > 0)
            {
                Assert.Fail("Translation Failure: " + result.Diagnostics[0].GetMessage());
            }

            Assert.AreEqual(expectedTypeScriptText, result.Text);
        }

        public static IReadOnlyList<MetadataReference> s_References =
            new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
    }
}