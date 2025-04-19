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
using HonkSharp.Fluency;
using System.Transactions;

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

        AssemblyFileTree = [.. AssemblyFileTree, new InterruptableMultiline {Index = 0, LinesBox = new Box<string[]>(File.ReadAllLines(SourceEntryPointFilePath)) }];
        DecodeCodeBlock<int>(ref AssemblyFileTree[0].LinesBox.Value, 0);
        return 0;
    }

    private static StatusResponse<Variable> ReserveMemory(MemoryTypes MemoryType, string Context) {

        bool __BCD__ = false;
        bool __Signed__ = false;
        bool __Endian__ = false;


        if (Context.Contains('d')) __BCD__ = true;
        if (Context.Contains('b')) __BCD__ = true;
        if (Context.Contains('i')) __Signed__ = true;
        else if (!Context.Contains('u')) return new StatusResponse<Variable> { Status = EXIT_CODES.SYNTAX_ERROR };

        if (!int.TryParse(Regex.Replace(Context, @"[^\d]", ""), out int __Width__)) return new StatusResponse<Variable> { Status = EXIT_CODES.SYNTAX_ERROR };

        // PROGRAM LOGIC SUCH IF NO SYSRAM IS AVAILABLE, USE CARTRAM INSTEAD

        switch (MemoryType) {
            case MemoryTypes.Slow:
                if ((SYSRAMUsage + __Width__) > 0x600) {
                    return new StatusResponse<Variable> {
                        Status = EXIT_CODES.NOTHING_TO_DO
                    };
                }

                SYSRAMUsage += __Width__;
                return new StatusResponse<Variable> {
                    Status = EXIT_CODES.OK,
                    Response = new Variable {Width = 0x200 + SYSRAMUsage - __Width__, Endian = __Endian__, BCD = __BCD__, Signed = __Signed__}
                };

            case MemoryTypes.Fast:
                if ((ZPUsage + __Width__) > 0x100) {
                    goto case MemoryTypes.Slow;
                } else goto case MemoryTypes.ZP;

            case MemoryTypes.ZP:
                if ((ZPUsage + __Width__) > 0x100) {
                    return new StatusResponse<Variable> {
                        Status = EXIT_CODES.NOTHING_TO_DO
                    };
                }

                ZPUsage += __Width__;
                return new StatusResponse<Variable> {
                    Status = EXIT_CODES.OK,
                    Response = new Variable { Width = ZPUsage - __Width__, Endian = __Endian__, BCD = __BCD__, Signed = __Signed__ }
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
            for (int Step = 0; Step < Steps.Length; Step++) {
                for (int iter = 0; iter < CurrentStep.Length; iter++) {
                    // decode request (directive, declaration, definition, keyword)

                    MemoryTypes CurrentMemoryType = MemoryTypes.Fast;   // Default to fast memory every symbol accses in event of memory reservation

                    if (Constants.Keywords.Contains(CurrentStep[iter])) {
                        switch (CurrentStep[iter]) {

                            case "int":                                 // assembler variable
                                iter++;
                                if (iter != CurrentStep.Length - 1) {
                                    int NewStringNameIndex = iter;

                                    if (++iter == '=') {
                                        iter++;
                                        if (int.TryParse(CurrentStep[iter], out int Content)) {
                                            LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter],
                                                new Label { Context = new Union(Content), Level = EvaluationLevel.OK, Type = typeof(int).TypeHandle });
                                        }
                                    } else if (iter == '(') {
                                        StatusResponse<T> AMSR = AddMacro<T>(typeof(int).TypeHandle, ref CurrentStep, ref iter, ref TargetLine);
                                        if (AMSR.Status != EXIT_CODES.OK) return AMSR;
                                    } else {
                                        return new StatusResponse<T> { Status = EXIT_CODES.SYNTAX_ERROR };
                                    }
                                } else {
                                    LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter],
                                        new Label { Context = new Union(null), Level = EvaluationLevel.WAIT, Type = typeof(int).TypeHandle });
                                }
                                break;

                            case "bool":                                 // assembler variable
                                iter++;
                                if (iter != CurrentStep.Length - 1) {
                                    int NewStringNameIndex = iter;

                                    if (++iter == '=') {
                                        iter++;
                                        if (bool.TryParse(CurrentStep[iter], out bool Content)) {
                                            LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter],
                                                new Label { Context = new Union(Content), Level = EvaluationLevel.OK, Type = typeof(bool).TypeHandle });
                                        }
                                    } else if (iter == '(') {
                                        StatusResponse<T> AMSR = AddMacro<T>(typeof(bool).TypeHandle, ref CurrentStep, ref iter, ref TargetLine);
                                        if (AMSR.Status != EXIT_CODES.OK) return AMSR;
                                    } else {
                                        return new StatusResponse<T> { Status = EXIT_CODES.SYNTAX_ERROR };
                                    }
                                } else {
                                    LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter],
                                        new Label { Context = new Union(null), Level = EvaluationLevel.WAIT, Type = typeof(bool).TypeHandle });
                                }
                                break;

                            // Need something for string and exp

                            case "del":
                                iter++;
                                if (LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.TryGetValue(CurrentStep[iter], out Label? Context)){
                                    if (Context!.Value.Type.Equals(typeof(Variable).TypeHandle)) {
                                        switch (Context.Value.Context.Get<Variable>().MemoryType) {
                                            case MemoryTypes.Slow:
                                                SYSRAMUsage -= Context.Value.Context.Get<Variable>().Width;
                                                break;

                                            case MemoryTypes.Fast:
                                                if (Context.Value.Context.Get<Variable>().Offset >= 0x100) {
                                                    goto case MemoryTypes.Slow;
                                                } else {
                                                    goto case MemoryTypes.Fast;
                                                }

                                            case MemoryTypes.ZP:
                                                ZPUsage -= Context.Value.Context.Get<Variable>().Width;
                                                break;
                                            }

                                            // case MemoryTypes.MMC
                                            // case MemoryTypes.PRG
                                    }
                                    LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Remove(CurrentStep[iter]);
                                    break;
                                } else {
                                    return new StatusResponse<T> {
                                        Status = EXIT_CODES.INVALID_ARGUMENT
                                    };
                                }

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
                                iter++;
                                CurrentMemoryType = MemoryTypes.Slow;
                                goto default;

                            case "zp":                                  // 'fast' keyword does nothing but assist legibility
                                iter++;
                                CurrentMemoryType = MemoryTypes.ZP;
                                goto default;

                            default:
                                StatusResponse<Variable> Resp = ReserveMemory(CurrentMemoryType, CurrentStep[iter]);
                                switch (Resp.Status) {
                                    case EXIT_CODES.OK:
                                        iter++;
                                        // Register Label with Offset
                                        LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter],
                                            new Label { Context = new Union(Resp.Response), Level = EvaluationLevel.OK, Type = typeof(Variable).TypeHandle });

                                        if (CurrentStep.Length - 1 != iter) goto default;
                                        continue;

                                    default:
                                        return (StatusResponse<T>)(object)Resp;
                                }
                            }
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

    private static StatusResponse<T> AddMacro<T>(object RTH, ref string[] CurrentStep, ref int iter, ref int TargetLine) {
        RuntimeTypeHandle[] MacroParameterBuffer = [];

        do {
            MacroParameterBuffer = [.. MacroParameterBuffer, 
                CurrentStep[iter] switch {
                    "int"       => typeof(int).TypeHandle,
                    "string"    => typeof(string).TypeHandle,
                    "exp"       => typeof(Exp).TypeHandle,
                    "register"  => typeof(Register).TypeHandle,
                    "flag"      => typeof(Flag).TypeHandle,
                    _           => typeof(EXIT_CODES).TypeHandle,
                }
            ];

            if (MacroParameterBuffer[^1].Equals(typeof(EXIT_CODES).TypeHandle)) {
                return new StatusResponse<T> { Status = EXIT_CODES.SYNTAX_ERROR };
            }

            ++iter; // TODO: Ensure parameter names are valid

            if (!new string[] { ",", ")" }.Contains(CurrentStep[++iter])) {
                return new StatusResponse<T> {
                    Status = EXIT_CODES.SYNTAX_ERROR
                };
            }
        } while (CurrentStep[iter] != ")");

        LabelDataBase[ActiveScope]!.Value.Context.Get<Dictionary<string, Label?>>()!.Add(CurrentStep[iter],
                new Label {
                    Context = new Union(new Macro {
                        LineNumber = TargetLine,
                        ReturnType = (RuntimeTypeHandle)RTH,
                        ParameterTypes = MacroParameterBuffer,
                        AssemblyTreeReference = AssemblyFileTree[^1]
                    }),
                    Level = EvaluationLevel.OK,
                    Type = typeof(Macro).TypeHandle
                });

        return new StatusResponse<T> {
            Status = EXIT_CODES.OK
        };
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