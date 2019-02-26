using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;
using static Sharpie.TypeScriptTranslator;

namespace SharpieTests
{
    [TestClass]
    public class ApiTranslationTests : TestBase
    {

        [TestMethod]
        public void TestStringConcat()
        {
            TestExpression("string.Concat(\"a\")", "String.concat(\"a\")");
            TestExpression("string.Concat(\"a\", \"b\")", "String.concat(\"a\", \"b\")");
            TestExpression("string.Concat(\"a\", \"b\", \"c\")", "String.concat(\"a\", \"b\", \"c\")");

            TestExpression("System.String.Concat(\"a\")", "String.concat(\"a\")");
            TestExpression("System.String.Concat(\"a\", \"b\")", "String.concat(\"a\", \"b\")");
            TestExpression("System.String.Concat(\"a\", \"b\", \"c\")", "String.concat(\"a\", \"b\", \"c\")");
        }
    }
}