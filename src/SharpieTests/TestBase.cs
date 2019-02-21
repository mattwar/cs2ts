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
            var actualTypeScriptExpression = TypeScriptTranslator.TranslateExpression(csharpExpression, s_References);
            Assert.AreEqual(expectedTypeScriptExpression, actualTypeScriptExpression);
        }

        public void TestStatement(string csharpStatement, string expectedTypeScriptStatement)
        {
            var actualTypeScriptStatement = TypeScriptTranslator.TranslateStatement(csharpStatement, s_References);
            Assert.AreEqual(expectedTypeScriptStatement, actualTypeScriptStatement);
        }

        public void TestType(string csharpType, string expectedTypeScriptType)
        {
            var actualTypeScriptType = TypeScriptTranslator.TranslateType(csharpType, s_References);
            Assert.AreEqual(expectedTypeScriptType, actualTypeScriptType);
        }

        public static IReadOnlyList<MetadataReference> s_References =
            new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
    }
}