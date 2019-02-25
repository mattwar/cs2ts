﻿using System;
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
        public void TestTypeAccessModifiers()
        {
            TestCompilationUnit(@"public class C { }", "class C { }");
            TestCompilationUnit(@"class C { }", "class C { }");
            TestCompilationUnit(@"internal class C { }", "public class C { }");

            TestCompilationUnit(@"public struct S { }", "class S { }");
            TestCompilationUnit(@"struct S { }", "class S { }");
            TestCompilationUnit(@"internal struct S { }", "public class S { }");

            TestCompilationUnit(@"public interface I { }", "interface I { }");
            TestCompilationUnit(@"interface I { }", "interface I { }");
            TestCompilationUnit(@"internal interface I { }", "public interface I { }");

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
            TestCompilationUnit(@"class C { public int M() => E; }", "class C { M(): number { return E; } }");

            TestCompilationUnit(@"abstract class C { public abstract int M(); }", "abstract class C { abstract M(): number; }");
        }

        [TestMethod]
        public void TestFields()
        {
            TestCompilationUnit(@"class C { public int X; }", "class C { X: number; }");
            TestCompilationUnit(@"class C { public int X = 1; }", "class C { X: number = 1; }");
        }

        [TestMethod]
        public void TestProperties()
        {
            // get only property
            TestCompilationUnit(@"class C { public int P { get { return x; } } }", "class C { get P(): number { return x; } }");

            // get-set property
            TestCompilationUnit(@"class C { public int P { get { return x; } set { x = value; } } }", "class C { get P(): number { return x; } set P(value: number) { x = value; } }");

            // set-get property
            TestCompilationUnit(@"class C { public int P { set { x = value; } get { return x; } } }", "class C { set P(value: number) { x = value; } get P(): number { return x; } }");

            // set-only property
            TestCompilationUnit(@"class C { public int P { set { x = value; } } }", "class C { set P(value: number) { x = value; } }");


            // virtual get only property
            TestCompilationUnit(@"class C { public virtual int P { get { return x; } } }", "class C { get P(): number { return x; } }");

            // virtual get-set property
            TestCompilationUnit(@"class C { public virtual int P { get { return x; } set { x = value; } } }", "class C { get P(): number { return x; } set P(value: number) { x = value; } }");

            // virtual set-get property
            TestCompilationUnit(@"class C { public virtual int P { set { x = value; } get { return x; } } }", "class C { set P(value: number) { x = value; } get P(): number { return x; } }");

            // virtual set-only property
            TestCompilationUnit(@"class C { public virtual int P { set { x = value; } } }", "class C { set P(value: number) { x = value; } }");


            // override get only property
            TestCompilationUnit(@"class C { public override int P { get { return x; } } }", "class C { get P(): number { return x; } }");

            // override get-set property
            TestCompilationUnit(@"class C { public override int P { get { return x; } set { x = value; } } }", "class C { get P(): number { return x; } set P(value: number) { x = value; } }");

            // override set-get property
            TestCompilationUnit(@"class C { public override int P { set { x = value; } get { return x; } } }", "class C { set P(value: number) { x = value; } get P(): number { return x; } }");

            // override set-only property
            TestCompilationUnit(@"class C { public override int P { set { x = value; } } }", "class C { set P(value: number) { x = value; } }");


            // abstract get-only property
            TestCompilationUnit(@"abstract class C { public abstract int P { get; } }", "abstract class C { abstract get P(): number; }");

            // abstract get-set property
            TestCompilationUnit(@"abstract class C { public abstract int P { get; set; } }", "abstract class C { abstract get P(): number; abstract set P(value: number); }");

            // abstract set-get property
            TestCompilationUnit(@"abstract class C { public abstract int P { set; get; } }", "abstract class C { abstract set P(value: number); abstract get P(): number; }");

            // abstract set-only property
            TestCompilationUnit(@"abstract class C { public abstract int P { set; } }", "abstract class C { abstract set P(value: number); }");
        }

        [TestMethod]
        public void TestAutoProperties()
        {
            // auto property
            TestCompilationUnit(@"class C { public int P { get; set; } }", @"class C { P: number; }");

            // readonly auto property
            TestCompilationUnit(@"class C { public int P { get; } }", @"class C { private _p: number; get P(): number { return _p; } }");

            // initialized auto property
            TestCompilationUnit(@"class C { public int P { get; set; } = 0; }", @"class C { P: number = 0; }");

            // initialized readonly auto property
            TestCompilationUnit(@"class C { public int P { get; } = 0; }", @"class C { readonly P: number = 0; }");


            // virtual auto property
            TestCompilationUnit(@"class C { public virtual int P { get; set; } }", @"class C { private _p: number; get P(): number { return _p; } set P(value: number) { _p = value; } }");

            // virtual readonly auto property
            TestCompilationUnit(@"class C { public virtual int P { get; } }", @"class C { private _p: number; get P(): number { return _p; } }");

            // virtual writeonly auto property
            TestCompilationUnit(@"class C { public virtual int P { set; } }", @"class C { private _p: number; set P(value: number) { _p = value; } }");

        }

        [TestMethod]
        public void TestExpressionBodiedProperties()
        {
            // expression bodied property
            TestCompilationUnit(@"class C { public int P => 3; }", "class C { get P(): number { return 3; } }");

            // expression bodied getter property
            TestCompilationUnit(@"class C { public int P { get => 3; } }", "class C { get P(): number { return 3; } }");

            // expression bodied setter property
            TestCompilationUnit(@"class C { public int P { set => F(value); } }", "class C { set P(value: number) { F(value); } }");

            // virtual expression bodied property
            TestCompilationUnit(@"class C { public virtual int P => 3; }", "class C { get P(): number { return 3; } }");

            // override expression bodied property
            TestCompilationUnit(@"class C { public override int P => 3; }", "class C { get P(): number { return 3; } }");
        }


        [TestMethod]
        public void TestInterfaceMethod()
        {
            // has no body... until C# 8?
            TestCompilationUnit(@"interface I { void M(); }", "interface I { M(); }");
        }

        [TestMethod]
        public void TestInterfaceProperties()
        {
            // looks like auto-property
            TestCompilationUnit("interface I { int P { get; set; } }", "interface I { get P(): number; set P(value: number); }");
            TestCompilationUnit("interface I { int P { get; } }", "interface I { get P(): number; }");
            TestCompilationUnit("interface I { int P { set; } }", "interface I { set P(value: number); }");
        }
    }
}