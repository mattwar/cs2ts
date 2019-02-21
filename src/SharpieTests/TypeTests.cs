using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;

namespace SharpieTests
{
    [TestClass]
    public class TypeTests : TestBase
    {
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
    }
}
