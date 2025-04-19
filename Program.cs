using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.Emit;

using AngouriMath;

using Tataru.Types;
using Tataru.Header;
using Tataru.Constants;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Xml.Linq;

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

    static int ZPUsage, SYSRAMUsage;


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

    private static StatusResponse<int> ReserveMemory(MemoryTypes MemoryType, string Context) {

        bool BCD = false;
        bool Signed = false;
        int width = 0;


        if (Context.Contains('d')) BCD    = true;
        if (Context.Contains('i')) Signed = true;
        else if (!Context.Contains('u')) return new StatusResponse<int> { Status = EXIT_CODES.SYNTAX_ERROR };

        if (!int.TryParse(Regex.Replace(Context, @"[^\d]", ""), out width)) return new StatusResponse<int> { Status = EXIT_CODES.SYNTAX_ERROR };

        // PROGRAM LOGIC SUCH IF NO SYSRAM IS AVAILABLE, USE CARTRAM INSTEAD

        switch (MemoryType) {
            case MemoryTypes.Slow:
                if ((SYSRAMUsage + width) > 0x600) {
                    return new StatusResponse<int> {
                        Status = EXIT_CODES.NOTHING_TO_DO
                    };
                }

                SYSRAMUsage += width;
                return new StatusResponse<int> {
                    Status = EXIT_CODES.OK,
                    Response = 0x100 + SYSRAMUsage
                };

            case MemoryTypes.Fast:
                if ((ZPUsage + width) > 0x100) {
                    goto case MemoryTypes.Slow;
                } else goto case MemoryTypes.ZP;

            case MemoryTypes.ZP:
                if ((ZPUsage + width) > 0x100) {
                    return new StatusResponse<int> {
                        Status = EXIT_CODES.NOTHING_TO_DO
                    };
                }

                ZPUsage += (ushort)width;
                return new StatusResponse<int> {
                    Status = EXIT_CODES.OK,
                    Response = ZPUsage - width
                };

                //case MemoryTypes.MMC:
                //case MemoryTypes.PRG:
            }

        return new();
    }

    private static StatusResponse<T> DecodeCodeBlock<T>(ref String[] TargetContents, int TargetLine) {
        string[] Steps = [];        // expressions to decode split by ';'
        string[] CurrentStep = [];  // current expression split with regex

        while (TargetLine != TargetContents.Length) {
            // decode things

            Steps = TargetContents[TargetLine].Split(';');
            // CurrentStep split by regex

            // This part must resolve defines, labels, recurse on macro call and generate a Rosyln Evaluable C# String
            // It must also handle returning case too, will feed back in on itself

            for (int iter = 0; iter < CurrentStep.Length; iter++) {
                // decode request (directive, declaration, definition, keyword)

                MemoryTypes CurrentMemoryType = MemoryTypes.Fast;   // Default to fast memory every symbol accses in event of memory reservation

                if (Constants.Keywords.Contains(CurrentStep[iter])) {
                    switch (CurrentStep[iter]) {

                        case "proc":
                            // add new scope based off proc name
                            iter++;
                            LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter], new Label {
                                Context = new Union(new Dictionary<string, Label?>()),
                                Type = typeof(Dictionary<string, Label?>).TypeHandle,
                                Level = EvaluationLevel.OK
                            });
                            ActiveScope = CurrentStep[iter];
                            break;

                        case "slow":
                            CurrentMemoryType = MemoryTypes.Slow;
                            goto default;

                        case "zp":                                  // 'fast' keyword does nothing but assist legibility
                            CurrentMemoryType = MemoryTypes.ZP;
                            goto default;

                        default:
                            StatusResponse<int> Resp = ReserveMemory(CurrentMemoryType, CurrentStep[iter]);
                            switch (Resp.Status) {
                                case EXIT_CODES.OK:
                                    iter++;
                                    // Register Label with Offset
                                    LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter], 
                                        new Label { Context = new Union(Resp.Response), Level = EvaluationLevel.OK, Type = typeof(int).TypeHandle });

                                    break;

                                default:
                                    return (StatusResponse<T>)(object)Resp;
                            }
                            break;
                    }
                }
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