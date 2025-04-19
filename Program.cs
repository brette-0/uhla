using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.Emit;

using AngouriMath;

using Tataru.Types;
using Tataru.Header;
using Tataru.Constants;

public static class Program {
    static bool DebugFile = false;
    static bool ListingFile = false;

    // line position at include : file as array of lines split by new line
    static InterruptableMultiline[] AssemblyFileTree = [];

    static string SourceEntryPointFilePath = "";
    static string TargetEntryPointFilePath = "";

    // scope[name] = value for all labels
    static Dictionary<string, Label?> LabelDataBase = [];
    static string ActiveScope = "root";

    public static int Main(String[] args) {
        Header.Target = Targets.None;

        LabelDataBase["root"] = new Label {
            Level   = EvaluationLevel.OK,
            Type    = typeof(Dictionary<string, Label?>).TypeHandle,
            Context = new(LabelDataBase)
        };


        EXIT_CODES resp = ResolveArguments(ref args);   // digest cli args
        if (resp != 0) return (int)resp;                // if fail, return with fail ctx

        AssemblyFileTree = [.. AssemblyFileTree, new InterruptableMultiline {Index = 0, Lines = File.ReadAllLines(SourceEntryPointFilePath) }];
        DecodeCodeBlock<int>(ref AssemblyFileTree[0].Lines, 0);
        return 0;
    }

    private static StatusResponse<T> DecodeCodeBlock<T>(ref String[] TargetContents, int TargetLine) {
        string[] Steps = [];        // expressions to decode split by ';'
        string[] CurrentStep = [];  // current expression split with regex

        while (TargetLine != TargetContents.Length) {
            // decode things

            Steps = TargetContents[TargetLine].Split(';');
            // CurrenStep split by regex

            // This part must resolve defines, labels, recurse on macro call and generate a Rosyln Evaluable C# String
            // It must also handle returning case too, will feed back in on itself
            foreach (string Element in CurrentStep) { 
                
            }
        }

        if (TargetLine == TargetContents.Length && AssemblyFileTree.Length == 1) {
            // we are at the end of assembly
            return new StatusResponse<T> {
                Status = EXIT_CODES.OK,
                Response = default
            };
        }

        return new();
    }

    private static EXIT_CODES ResolveArguments(ref String[] args) {
        if (args.Length == 0) return EXIT_CODES.NOTHING_TO_DO;
        for (int i = 0; i < args.Length; i++) {
            switch (args[i]) {
                case "-h":
                case "--help":
                    Console.Write(
"""
Tataru - Cursed 2a03 Assembler | Brette (2025)

Commands:
    -h | --help             : Help Command, you used it to see this page.
    -s | --source  "path"   : Specify Path to Entrypoint ASM Source Code
    -t | --target  "path"   : Specify Path for generated output ROMFile.
    =i | --include "paths"  : Specify array of filepaths for use in project.

""");
                    return EXIT_CODES.OK;

                case "-s":
                case "--source":
                    if (i == args.Length - 1) {
                        Console.WriteLine($"Command Line Instruction '{args[i]}' requires a path as parameter!");
                        return EXIT_CODES.NO_PARAMETER;
                    }
                    SourceEntryPointFilePath = args[++i];
                    break;

                case "-i":
                case "--include":
                    if (i == args.Length - 1) {
                        Console.WriteLine($"Command Line Instruction '{args[i]}' requires a path as parameter!");
                        return EXIT_CODES.NO_PARAMETER;
                    }
                    // throw warnings for missing or empty folders
                    // parse string into string[] split by comma within square brackets
                    break;

                case "-t":
                case "--target":
                    if (i == args.Length - 1) {
                        Console.WriteLine($"Command Line Instruction '{args[i]}' requires a path as parameter!");
                        return EXIT_CODES.NO_PARAMETER;
                    }
                    TargetEntryPointFilePath = args[++i];
                    break;

                case "-l":
                case "--listing":
                    ListingFile = true;
                    break;

                case "-d":
                case "--debug":
                    DebugFile = true;
                    break;

                default:
                    Console.WriteLine("Invalid Command Line Argument, please use '-h' or '--help' on available commands.");
                    return EXIT_CODES.INVALID_ARGUMENT;
            }
        }
        return EXIT_CODES.OK;
    }
}