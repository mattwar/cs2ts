using System;
using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Sharpie;

namespace SharpieTests
{
    [TestClass]
    public class DeclarationTests : TestBase
    {
        [TestMethod]
        public void TestClassAccessModifiers()
        {
            TestCompilationUnit(@"public class C { }", "class C { }");
            TestCompilationUnit(@"class C { }", "class C { }");
            TestCompilationUnit(@"internal class C { }", "public class C { }");
        }

        [TestMethod]
        public void TestStructAccessModifiers()
        {
            TestCompilationUnit(@"public struct S { }", "class S { }");
            TestCompilationUnit(@"struct S { }", "class S { }");
            TestCompilationUnit(@"internal struct S { }", "public class S { }");
        }

        [TestMethod]
        public void TestInterfaceAccessModifiers()
        {
            TestCompilationUnit(@"public interface I { }", "interface I { }");
            TestCompilationUnit(@"interface I { }", "interface I { }");
            TestCompilationUnit(@"internal interface I { }", "public interface I { }");
        }

        [TestMethod]
        public void TestNestedAccessModifiers()
        {
            TestCompilationUnit("class C { class N { } }", "class C { private class N { } }");
            TestCompilationUnit("class C { struct N { } }", "class C { private class N { } }");
            TestCompilationUnit("class C { interface N { } }", "class C { private interface N { } }");
        }

        [TestMethod]
        public void TestGenericClass()
        {
            TestCompilationUnit(@"class C<T> { }", "class C<T> { }");
            TestCompilationUnit(@"class C<T1, T2> { }", "class C<T1, T2> { }");
        }

        [TestMethod]
        public void TestGenericStruct()
        {
            TestCompilationUnit(@"struct S<T> { }", "class S<T> { }");
            TestCompilationUnit(@"struct S<T1, T2> { }", "class S<T1, T2> { }");
        }

        [TestMethod]
        public void TestGenericInterface()
        {
            TestCompilationUnit(@"interface I<T> { }", "interface I<T> { }");
            TestCompilationUnit(@"struct C<T1, T2> { }", "class C<T1, T2> { }");
        }

        [TestMethod]
        public void TestClassBaseList()
        {
            TestCompilationUnit(@"class B { } class D : B { }", "class B { } class D extends B { }");
            TestCompilationUnit(@"interface I { } class C : I { }", "interface I { } class C implements I { }");
            TestCompilationUnit(@"class B { } interface I { } class D : B, I { }", "class B { } interface I { } class D extends B implements I { }");
            TestCompilationUnit(@"class B { } interface I { } class D : I, B { }", "class B { } interface I { } class D extends B implements I { }");
            TestCompilationUnit(@"class B { } interface I1 { } interface I2 { } public class D : B, I1, I2 { }", "class B { } interface I1 { } interface I2 { } class D extends B implements I1, I2 { }");
        }

        [TestMethod]
        public void TestStructBaseList()
        {
            TestCompilationUnit(@"interface I { } class S : I { }", "interface I { } class S implements I { }");
            TestCompilationUnit(@"interface I1 { } interface I2 { } class S : I1, I2 { }", "interface I1 { } interface I2 { } class S implements I1, I2 { }");
        }

        [TestMethod]
        public void TestInterfaceBaseList()
        {
            TestCompilationUnit(@"interface B { } interface D : B { }", "interface B { } interface D extends B { }");
            TestCompilationUnit(@"interface B1 { } interface B2 { } interface D : B1, B2 { }", "interface B1 { } interface B2 { } interface D extends B1, B2 { }");
        }

        [TestMethod]
        public void TestConstructors()
        {
            TestCompilationUnit(@"class C { public C() { } }", "class C { constructor() { } }");
            TestCompilationUnit(@"class C { public C(int p) { } }", "class C { constructor(p: number) { } }");
            TestCompilationUnit(@"class C { public C(int p1, double p2) { } }", "class C { constructor(p1: number, p2: number) { } }");
            TestCompilationUnit(@"class C { public C() => E; }", "class C { constructor() { E; } }");
        }

        [TestMethod]
        public void TestMethods()
        {
            TestCompilationUnit(@"class C { public void M() { } }", "class C { M() { } }");
            TestCompilationUnit(@"class C { public void M(int p) { } }", "class C { M(p: number) { } }");
            TestCompilationUnit(@"class C { public void M(int p1, string p2) { } }", "class C { M(p1: number, p2: string) { } }");

            TestCompilationUnit(@"class C { public int M() { return 1; } }", "class C { M(): number { return 1; } }");
            TestCompilationUnit(@"class C { public int M(int p) { return 1; } }", "class C { M(p: number): number { return 1; } }");
            TestCompilationUnit(@"class C { public int M(int p1, string p2) { return 1; } }", "class C { M(p1: number, p2: string): number { return 1; } }");

            TestCompilationUnit(@"class C { public void M() => E; }", "class C { M() { E; } }");
            TestCompilationUnit(@"class C { public int M() => E; }", "class C { M(): number { E; } }");
        }

        [TestMethod]
        public void TestFields()
        {
            TestCompilationUnit(@"class C { public int X; }", "class C { X: number; }");
            TestCompilationUnit(@"class C { public int X = 1; }", "class C { X: number = 1; }");
        }
    }
}