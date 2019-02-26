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
            TestExpression("string.Concat(\"a\", \"b\")", "\"a\".concat(\"b\")");
            TestExpression("string.Concat(\"a\", \"b\", \"c\")", "\"a\".concat(\"b\", \"c\")");
            TestExpression("string.Concat(\"a\", \"b\", \"c\", \"d\")", "\"a\".concat(\"b\", \"c\", \"d\")");
            TestExpression("string.Concat(\"a\", \"b\", \"c\", \"d\", \"e\")", "\"\".concat(\"a\", \"b\", \"c\", \"d\", \"e\")");
        }
    }
}