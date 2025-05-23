using Tataru.Types.AssembleTime;
using Tataru.Types.RunTime;
using Tataru.Memory;
using Tataru.Mapper;
using Tataru.Headers;
using Tataru.Engine.Types;
using System.Diagnostics;
using Antlr4.Runtime.Misc;

static partial class Program {
    private static int Main(string[] args) {
        (ExitConditions ReturnCode, string? Message) = CommandLineDecode(ref args);
        if (Message != null) Console.Write(Message);
        if (ReturnCode != ExitConditions.CONTINUE) return (int)ReturnCode;

        // null = linear codeflow, non-null = macro execution
        (ExitConditions ResponseStatus, object? _) = Assemble(null, null)!;
        return       (int)ResponseStatus;
    }

    private static (ExitConditions, object?) Assemble(string[]? TargetSource, int? TargetIndex) {
        bool LinearExecution = false;

        string[] LineElements = [];
        int      LineElementIndex = 0;
        object?  ReturnValue = null;                                        // void macros/linear codeflow should be null

        if (TargetSource == null) {
            LinearExecution = true;                                         // if linear execution, ensure on macro call we update
            TargetSource = CodeBlocks[^1];
            TargetIndex  = CodeBlockIndexes[^1];
        }

        for (; TargetIndex < TargetSource.Length; TargetIndex++) {
            if (TargetSource[(int)TargetIndex].StartsWith('#')) {
                // directives cannot have code following, so no ; at the end

                SplitCodeLine(ref LineElements);
                LineElementIndex = 1;                                       // start beyond the '#'


                switch (LineElements[LineElementIndex]) {
                    case "include":
                        TargetIndex++;
                        switch (LineElements[LineElementIndex]) {
                            case "\"":
                                LineElementIndex++;
                                string IncPath = LineElements[LineElementIndex];
                                if (LineElements[++LineElementIndex] != "\"") {
                                    Console.WriteLine("Local Include was not completed.");
                                    return (ExitConditions.ERROR, null);
                                }

                                try {
                                    CodeBlocks.Add(File.ReadAllLines(IncPath));
                                    CodeBlockIndexes.Add(0);
                                } catch (Exception e) {
                                    Console.WriteLine($"Could not access {IncPath} : {e}");
                                    return (ExitConditions.ERROR, null);
                                }

                                goto ExecuteNewCodeBlock;

                            case "<":
                                LineElementIndex++;
                                string ModuleName = LineElements[LineElementIndex];
                                if (LineElements[++LineElementIndex] != ">") {
                                    Console.WriteLine("Library Include was not completed.");
                                    return (ExitConditions.ERROR, null);
                                }

                                // attempt to find libpath_i/module_name.s (where libpath_i is iteration over subscribed paths to libpaths)

                                foreach (string libpath in LibPaths) {
                                    try {
                                        CodeBlocks.Add(File.ReadAllLines($"{libpath}/{LineElements[LineElementIndex]}.s"));
                                        CodeBlockIndexes.Add(0);
                                    } catch (Exception e) {
                                        if (libpath == LibPaths[^1]) {
                                            Console.WriteLine($"Could not Locate Library '{ModuleName}', is its parent subscribed to the libpath?");
                                            return (ExitConditions.ERROR, null);
                                        }
                                    }
                                }

                            // leak into ExecuteNewCodeBlock

                            ExecuteNewCodeBlock:
                                CodeBlockIndexes[^1] = 1 + (int)TargetIndex;    // on return, we should not re-enter to execute this line again
                                (ExitConditions ResponseStatus, object? ResponseContext) = Assemble(null, null);

                                // throw the error back, until we exit out
                                if (ResponseStatus == ExitConditions.ERROR) return (ResponseStatus, ResponseContext); 
                                break;

                            default:
                                Console.WriteLine($"Erroneous Symbol '{LineElements[LineElementIndex]}'");
                                return (ExitConditions.ERROR, null);
                        }
                        break;

                    case "define":
                        // symbolic define
                        break;

                    case "undefine":
                        // remove define
                        break;

                    case "assert":
                        // complete an assertation
                        break;

                    case "rom":
                        // pad until we reach desired rom point
                        break;

                    case "cpu":
                        // retarget cpu position
                        break;

                    default:
                        Console.WriteLine($"Unrecognied Directive #{TargetSource[(int)TargetIndex]}");
                        return (ExitConditions.ERROR, null);
                }
                TargetIndex++;
            }
            // line evaluation
        }

        return (ExitConditions.OK, ReturnValue);
    }

    private static void SplitCodeLine(ref string[] Code) {
        // CodeLine here refers to the code between semicolons, this should be a regex operation
        return;
    }

    private static (ExitConditions, string?) CommandLineDecode(ref string[] args) {
        foreach (string element in args) {
            switch (element) {
                case "-h":
                case "--help":
                    // display help message
                    return (ExitConditions.OK, default);

                case "-d":
                case "--debug":
                    Debug = true;
                    break;

                case "-l":
                case "--listing":
                    Listing = true;
                    break;

                case "-i":
                case "--input":
                    try {
                        if (CodeBlocks.Count > 0) return (ExitConditions.ERROR, "Cannot specify more than one debug file");
                        CodeBlocks = [File.ReadAllLines(element)];
                        break;
                    } catch (Exception e) {
                        return (ExitConditions.ERROR, $"Failed to access asssembly source file {element} : {e}");
                    }

                default:
                    return (ExitConditions.ERROR, $"Unknown request {element} during Argument Parsing");
            }
        }
        
        if (CodeBlocks.Count == 1) return (ExitConditions.CONTINUE, default);
                                   return (ExitConditions.ERROR,    "Unknown Error during Argument Parsing");
    }

    static List<string[]>   CodeBlocks = [];
    static List<int>        CodeBlockIndexes = [0];
    static List<byte>       Output     = [];
    static Memory SimulatedMemory = new(Mappers.NROM, new());
    static List<string>     LibPaths   = [];

    static bool Debug, Listing, Verbose = false;
}