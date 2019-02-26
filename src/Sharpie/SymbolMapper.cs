using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Linq.Expressions;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace Sharpie
{
    public static class SymbolMapper
    {
        private static ConditionalWeakTable<Compilation, Dictionary<Type, ITypeSymbol>> typeMapMap
            = new ConditionalWeakTable<Compilation, Dictionary<Type, ITypeSymbol>>();

        public static ITypeSymbol GetTypeSymbol(Compilation compilation, Type type)
        {
            var typeMap = typeMapMap.GetOrCreateValue(compilation);

            if (!typeMap.TryGetValue(type, out var typeSymbol))
            {
                typeSymbol = CreateTypeSymbol();
#if DEBUG
                if (typeSymbol != null)
#endif
                typeMap.Add(type, typeSymbol);
            }

            return typeSymbol;

            ITypeSymbol CreateTypeSymbol()
            {
                if (type.IsArray)
                {
                    var elementSymbol = GetTypeSymbol(compilation, type.GetElementType());
                    return compilation.CreateArrayTypeSymbol(elementSymbol, type.GetArrayRank());
                }
                else if (type.IsConstructedGenericType)
                {
                    var typeDefSymbol = GetTypeSymbol(compilation, type.GetGenericTypeDefinition());
                    var typeArgs = GetTypeSymbols(compilation, type.GetGenericArguments());
                    return ((INamedTypeSymbol)typeDefSymbol).Construct(typeArgs);
                }
                else if (type.DeclaringType != null)
                {
                    var declaringTypeSymbol = GetTypeSymbol(compilation, type.DeclaringType);
                    var typeArgumentCount = type.IsGenericType ? type.GenericTypeArguments.Length : 0;

                    foreach (var nestedType in declaringTypeSymbol.GetTypeMembers(type.Name))
                    {
                        if ((nestedType.IsGenericType && type.IsGenericType && nestedType.TypeParameters.Length == typeArgumentCount)
                            || !nestedType.IsGenericType && !type.IsGenericType)
                        {
                            return nestedType;
                        }
                    }

                    return null;
                }
                else
                {
                    return compilation.GetTypeByMetadataName(type.FullName);
                }
            }
        }

        public static ITypeSymbol[] GetTypeSymbols(Compilation compilation, Type[] types)
        {
            return types.Select(t => GetTypeSymbol(compilation, t)).ToArray();
        }

        private static ConditionalWeakTable<Compilation, Dictionary<MethodInfo, IMethodSymbol>> methodMapMap
            = new ConditionalWeakTable<Compilation, Dictionary<MethodInfo, IMethodSymbol>>();

        public static IMethodSymbol GetMethodSymbol(Compilation compilation, MethodInfo method)
        {
            var methodMap = methodMapMap.GetOrCreateValue(compilation);
            if (!methodMap.TryGetValue(method, out var methodSymbol))
            {
                methodSymbol = CreateMethodSymbol();
#if DEBUG
                if (methodSymbol != null)
#endif
                methodMap.Add(method, methodSymbol);
            }

            return methodSymbol;

            IMethodSymbol CreateMethodSymbol()
            {
                var typeSymbol = GetTypeSymbol(compilation, method.DeclaringType);
                var parameterTypes = GetTypeSymbols(compilation, method.GetParameters().Select(p => p.ParameterType).ToArray());

                var namedMembers = typeSymbol.GetMembers(method.Name);

                foreach (var m in namedMembers)
                {
                    if (m is IMethodSymbol ms)
                    {
                        if (Matches(ms.Parameters, parameterTypes))
                            return ms;
                    }
                }

                return null;
            }
        }

        private static bool Matches(ImmutableArray<IParameterSymbol> parameters, ITypeSymbol[] matchTypes)
        {
            if (parameters.Length != matchTypes.Length)
                return false;

            for (int i = 0; i < parameters.Length; i++)
            {
                if (!parameters[i].Type.Equals(matchTypes[i]))
                    return false;
            }

            return true;
        }
    }
}