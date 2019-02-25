using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;

namespace SharpieTests
{
    public class TestBase
    {
        private void CheckSucceeds(TranslationResult result, string expectedText)
        {
            if (result.Diagnostics.Count > 0)
            {
                Assert.Fail("Translation Failure: " + result.Diagnostics[0].GetMessage());
            }

            Assert.AreEqual(expectedText, result.Text);
        }

        private void CheckFails(TranslationResult result, DiagnosticDescriptor[] expectedDiagnostics)
        {
            if (expectedDiagnostics.Length == 0 && result.Diagnostics.Count == 0)
            {
                Assert.Fail("Expected diagnostics");
            }
            else
            {
                foreach (var dx in expectedDiagnostics)
                {
                    if (!result.Diagnostics.Any(d => d.Id == dx.Id))
                    {
                        Assert.Fail($"Expected diagnostic: {dx.Description}");
                    }
                }
            }
        }

        public void TestExpression(string csharpExpression, string expectedTypeScriptExpression)
        {
            CheckSucceeds(TypeScriptTranslator.TranslateExpression(csharpExpression, s_References), expectedTypeScriptExpression);
        }

        public void TestExpressionFails(string csharpExpression, params DiagnosticDescriptor[] expectedDiagnostics)
        {
            CheckFails(TypeScriptTranslator.TranslateExpression(csharpExpression, s_References), expectedDiagnostics);
        }

        public void TestStatement(string csharpStatement, string expectedTypeScriptStatement)
        {
            CheckSucceeds(TypeScriptTranslator.TranslateStatement(csharpStatement, s_References), expectedTypeScriptStatement);
        }

        public void TestStatementFails(string csharpStatement, params DiagnosticDescriptor[] diagnostics)
        {
            CheckFails(TypeScriptTranslator.TranslateStatement(csharpStatement, s_References), diagnostics);
        }

        public void TestType(string csharpType, string expectedTypeScriptType)
        {
            CheckSucceeds(TypeScriptTranslator.TranslateType(csharpType, s_References), expectedTypeScriptType);
        }

        public void TestTypeFails(string csharpType, params DiagnosticDescriptor[] diagnostics)
        {
            CheckFails(TypeScriptTranslator.TranslateType(csharpType, s_References), diagnostics);
        }

        public void TestCompilationUnit(string csharpText, string expectedTypeScriptText)
        {
            CheckSucceeds(TypeScriptTranslator.TranslateCompilationUnit(csharpText, s_References), expectedTypeScriptText);
        }

        public void TestCompilationUnitFails(string csharpText, params DiagnosticDescriptor[] diagnostics)
        {
            CheckFails(TypeScriptTranslator.TranslateCompilationUnit(csharpText, s_References), diagnostics);
        }

        public static IReadOnlyList<MetadataReference> s_References =
            new[] { MetadataReference.CreateFromFile(typeof(object).Assembly.Location) };
    }
}