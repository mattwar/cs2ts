using System;
using Microsoft.AspNetCore.NodeServices;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace SharpieTests
{
    public static class TypeScriptValidator
    {
        private static INodeServices s_nodeServices;

        public static void Validate(string typescriptSource)
        {
            if (s_nodeServices == null)
            {
                var options = new NodeServicesOptions(NullServiceProvider.Instance);
                s_nodeServices = NodeServicesFactory.CreateNodeServices(options);
            }

            var task = s_nodeServices.InvokeAsync<string>("TypeScriptValidator.js", typescriptSource);
            var result = task.GetAwaiter().GetResult();

            if (result != "success")
            {
                Assert.Fail("\n" + typescriptSource + "\n\nErrors:\n" + result);
            }
        }

        private class NullServiceProvider : IServiceProvider
        {
            public object GetService(Type serviceType)
            {
                return null;
            }

            public static readonly IServiceProvider Instance = new NullServiceProvider();
        }
    }
}