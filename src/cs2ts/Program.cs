using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.Host.Mef;
using Sharpie;

namespace cs2ts
{
    class Program
    {
        static void Main(string[] args)
        {
            var workspace = new AdhocWorkspace(DesktopMefHostServices.DefaultServices);
            var projectInfo = CommandLineProject.CreateProjectInfo("cs2ts", LanguageNames.CSharp, args, Environment.CurrentDirectory, workspace);
            var project = workspace.AddProject(projectInfo);
            var compilation = (CSharpCompilation)project.GetCompilationAsync().GetAwaiter().GetResult();
            var result = TypeScriptTranslator.Translate(compilation);

            if (result.Diagnostics.Count > 0)
            {
                foreach (var dx in result.Diagnostics)
                {
                    Console.Out.WriteLine(dx);
                }
            }
            else
            {
                Console.Out.Write(result.Text);
            }
        }
    }
}
