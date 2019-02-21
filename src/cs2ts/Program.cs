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
            var diagnostics = new List<string>();
            var typeScriptText = TypeScriptTranslator.Translate(compilation, diagnostics);

            if (diagnostics.Count > 0)
            {
                foreach (var dx in diagnostics)
                {
                    Console.Out.WriteLine(dx);
                }
            }
            else
            {
                Console.Out.Write(typeScriptText);
            }
        }
    }
}
