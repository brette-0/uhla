using System.Collections.Specialized;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Data;

using Tataru;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;
using Microsoft.CodeAnalysis.Emit;
using static Program;

public static class Program {

    static readonly string OperatorRegex = @"(\d+|\w+|<=|>=|==|!=|\+\+|--|\+=|-=|\*=|/=|\$=|\??=|%=|&=|\|=|\^=|>>=|<<=|<<|>>|<=|>=|==|!=|\+|\-|\*|\/|&|\| |\^|\%|\$|!|#|\\|@|[(){}\[\]|<|>|\|]+)";
    static readonly HashSet<string> OperatorLUT = [
        "<", ">", "==", "!=", "<=", ">=", "?", ":", "??", "+", "-", "*", "/", "&", "|", "^",
        "%", "$", "!", ">>", "<<", ">|", "|<", "++", "--", "+=", "-=", "*=", "/=",
        "$=", "??=", "%=", "&=", "|=", "^=", ">>=", "<<=", "#", "\\", "@", "(", ")", "[", "]", "{", "}"
    ];



    static readonly HashSet<string> WeakOperatorLUT = [
        "(", ")", "[", "]", "{", "}"
    ];

    static readonly HashSet<string> ConstantKeywords = [
    "null"
    ];

    static bool DebugFile = false;
    static bool ListingFile = false;

    static Targets Target = Targets.None;

    // line position at include : file as array of lines split by new line
    static InterruptableMultiline[] AssemblyFileTree = [];

    static string SourceEntryPointFilePath = "";
    static string TargetEntryPointFilePath = "";

    // scope[name] = value for all labels
    static Dictionary<string, Dictionary<string, Label?>> LabelDataBase = [];
    static string ActiveScope = "root";



    static async Task<StatusResponse<string>> Eval<T>(string expression) {
        try {
            var result = await CSharpScript.EvaluateAsync<object>(
                expression,
                ScriptOptions.Default.WithImports("System")
            );

            return new StatusResponse<string> {
                Status = EXIT_CODES.OK,
                Response = result?.ToString() ?? "null" // Safely handle null
            };
        } catch (Exception) {
            return new StatusResponse<string> {
                Status = EXIT_CODES.SYNTAX_ERROR, 
                Response = default // or "error" if you prefer
            };
        }
    }

    public static int Main(String[] args) {

        LabelDataBase["root"] = [];                     // create root scope

        string TestText = "alpha * 10";
        StatusResponse<Symbol> Test = RequestEvaluate(ref TestText);
        //StatusResponse<int?> TestResult;
        //if (Test.Status == EXIT_CODES.OK) {
        //    TestResult = AccessLabelContent<int?>(Test);
        //}


        EXIT_CODES resp = ResolveArguments(ref args);   // digest cli args
        if (resp != 0) return (int)resp;                // if fail, return with fail ctx

        AssemblyFileTree = [.. AssemblyFileTree, new InterruptableMultiline {Index = 0, Lines = File.ReadAllLines(SourceEntryPointFilePath) }];

        //AssemblyFileTree.Add(0, new InterruptableMultiline { 0, File.ReadAllLines(SourceEntryPointFilePath) });  // Index 0 (top/entryhpoint) | Included at 0 (source include) | contents of file
        Assemble();
        return 0;
    }

    private static StatusResponse<T> AccessLabelContent<T>(StatusResponse<Symbol> Context) {
        if (Context.Status == EXIT_CODES.OK) {
            if (Context.Response.Context == "null") {
                return new StatusResponse<T> {
                    Status = EXIT_CODES.OK,
                };
            } else {
                switch (typeof(T) switch {
                    Type t when t == typeof(int) => 0x00,
                    Type t when t == typeof(int?) => 0x01,
                    Type t when t == typeof(string) => 0x02,
                    _ => -1
                }) {
                    case 0x00:
                        return new StatusResponse<T> { Status = EXIT_CODES.OK, Response = (T)(object)int.Parse(Context.Response.Context) };
                    case 0x01:
                        if (Context.Response.Context == "null") 
                        return new StatusResponse<T> { Status = EXIT_CODES.OK, Response = (T?)(object?)null };
                        return new StatusResponse<T> { Status = EXIT_CODES.OK, Response = (T)(object)int.Parse(Context.Response.Context) };
                    case 0x02:
                        return new StatusResponse<T> { Status = EXIT_CODES.OK, Response = (T)(object)Context.Response.Context };
                    default:
                        throw new Exception("May only define object as int or string");
                }
                throw new Exception("Determinism is dead, the universe is destroyed and there is no point in assembling this ROM");
            }
        } else {
            return new StatusResponse<T> {
                Status = EXIT_CODES.SYNTAX_ERROR,
            };
        }
    }

    private static EXIT_CODES Assemble() {
        for (int iter = AssemblyFileTree[^1].Index; iter < AssemblyFileTree[^1].Lines.Length; iter++) {
            // Request Decode Stage
            string CurrentString = AssemblyFileTree[^1].Lines[iter];

            // if the line cannot be completed then we assume it has information suceeding it on the following line, concaternate and increment if so

            if (CurrentString.Contains("#include")) {
                AssemblyFileTree[^1].Index = iter + 1; // register to start after include after include is finished

                string FilePath = "";

                

                // fetch path
                AssemblyFileTree[0].Index = 0; AssemblyFileTree[0].Lines = File.ReadAllLines(FilePath);
            }
        }
        return EXIT_CODES.OK;
    }

    private enum Types {
        e_int,
        e_str,
        e_arr,
        e_err
    }

    public enum EvaluationLevel {
        OK,     // ready for evaluation
        WAIT    // needs more information
    }

    public struct Symbol {
        public EvaluationLevel Level;
        public string Context;
    }

    public struct Label {
        public EvaluationLevel Level;
        public TypeCode TypeCode;
        public Union Context;
    }

    private static StatusResponse<Symbol> RequestEvaluate(ref string _Context) {
        // streamlined approach is the most logically optimal
        // parenthesis and brackets indicate hierarchy (sort of, brackets are semantically different)

        // split by operator regex

        static string ReplaceExponentiation(string expression) {
            return Regex.Replace(expression, @"(\d+)\s*\$\s*(\d+)", match =>
            {
                int baseNum = int.Parse(match.Groups[1].Value);
                int exponent = int.Parse(match.Groups[2].Value);
                return string.Join(" * ", new string[exponent].Select(_ => baseNum.ToString()));
            });
        }

        static string[] SplitExpression(string expression) {
            string[] result = Regex.Split(expression, OperatorRegex);

            result = Array.FindAll(result, s => !string.IsNullOrWhiteSpace(s));

            return result;
        }

        string[] Context = SplitExpression(_Context);

        EvaluationLevel Level = EvaluationLevel.OK;
        string Segment = "";

        bool OperatorClock = false;
        System.Globalization.NumberStyles ActiveDenotation = System.Globalization.NumberStyles.Number;   // default to decimal denotation alwayss

        static int FetchDenotedNumber(ref string Context, System.Globalization.NumberStyles NumberStyle) {
            return int.Parse(Context.Trim().ToLower(), NumberStyle);
        }

        string ExpressionAppend;

        for (int iter = 0; iter < Context.Length; iter++) {
            if (OperatorLUT.Contains(Context[iter].Trim())) {
                // if Operator is % or $ it could be binary or hex denotation
                if (OperatorClock || WeakOperatorLUT.Contains(Context[iter].Trim())) {
                    switch (Context[iter].Trim()) {
                        case "(":
                        case ")":
                            Segment += Context[iter].Trim();
                            continue;

                        default:
                            Segment += Context[iter].Trim();
                            break;
                    }
                } else {
                    switch (Context[iter].Trim()) {
                        case "$":   // hex denote
                            ActiveDenotation = System.Globalization.NumberStyles.HexNumber;
                            goto IntParse;

                        case "%":   // bin denote
                            ActiveDenotation = System.Globalization.NumberStyles.BinaryNumber;
                            goto IntParse;

                        IntParse:
                            try {
                                ExpressionAppend = FetchDenotedNumber(ref Context[++iter], ActiveDenotation).ToString();
                                ActiveDenotation = System.Globalization.NumberStyles.Number;            // 'reset' - potentially redundant
                            } catch { goto default; }
                            Segment += ExpressionAppend;
                            break;

                        default:    // SYNTAX ERROR | Two Consecutive Operators
                            return new StatusResponse<Symbol> {
                                Status = EXIT_CODES.SYNTAX_ERROR
                            };
                    }
                }
            } else {
                try {
                    int Temp = int.Parse(Context[iter].Trim());
                    ExpressionAppend = Temp.ToString();
                    Segment += ExpressionAppend;
                } catch {
                    if (LabelDataBase[ActiveScope].ContainsKey(Context[iter].Trim())) {
                        if (LabelDataBase[ActiveScope][Context[iter].Trim()] is null) {
                            // referenced but not defined
                            Segment += Context[iter];
                            Level = EvaluationLevel.WAIT;
                        } else if (LabelDataBase[ActiveScope][Context[iter].Trim()]!.Value.Level == EvaluationLevel.WAIT) {
                            // partially defined
                            Segment += Context[iter]; 
                        } else {
                            // defined
                            switch (LabelDataBase[ActiveScope][Context[iter].Trim()]!.Value.TypeCode) {
                                case TypeCode.Int32:
                                    Segment += LabelDataBase[ActiveScope][Context[iter].Trim()]!.Value.Context.Get<int>().ToString();
                                    break;

                                case TypeCode.String:
                                    Segment += LabelDataBase[ActiveScope][Context[iter].Trim()]!.Value.Context.Get<string>();
                                    break;
                            }
                        }
                    } else {
                        Segment += Context[iter];
                        if (ConstantKeywords.Contains(Context[iter])) {
                            switch (Context[iter]) {
                                case "null":
                                    break;

                                default:
                                    return new StatusResponse<Symbol> {
                                        Status = EXIT_CODES.SYNTAX_ERROR
                                    };
                            }
                        } else {
                            // new expression to generate
                            LabelDataBase[ActiveScope].Add(Context[iter].Trim(), null);
                            Level = EvaluationLevel.WAIT;                                   // expression cannot be resolved yet
                        }
                    }
                }
            }
            OperatorClock = !OperatorClock;
        }

        if (Level == EvaluationLevel.OK) {
            string? Temp = Eval<int?>(ReplaceExponentiation(Segment)).Result.Response;
            if (Temp is null) {
                return new StatusResponse<Symbol>(EXIT_CODES.SYNTAX_ERROR, new Symbol {
                    Level = EvaluationLevel.OK,
                    Context = ""
                });
            } else {
                return new StatusResponse<Symbol>(EXIT_CODES.OK, new Symbol {
                    Level = EvaluationLevel.OK,
                    Context = Temp!
                });
            }
        } else {
            return new StatusResponse<Symbol>(EXIT_CODES.OK, new Symbol {
                Level = EvaluationLevel.WAIT,
                Context = Segment
            });
        }
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
    -i | --input "path"     : Specify Path to Entrypoint ASM Source Code
    -o | --output "path"    : Specify Path for generated output ROMFile.

""");
                    return EXIT_CODES.OK;

                case "-i":
                case "--input":
                    if (i == args.Length - 1) {
                        Console.WriteLine($"Command Line Instruction '{args[i]}' requires a path as parameter!");
                        return EXIT_CODES.NO_PARAMETER;
                    }
                    SourceEntryPointFilePath = args[++i];
                    break;

                case "-o":
                case "--output":
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

    public enum EXIT_CODES {
        OK,
        INVALID_ARGUMENT,
        NO_PARAMETER,
        SYNTAX_ERROR,
        NOTHING_TO_DO
    }

    public struct StatusResponse<T> {
        public EXIT_CODES Status;
        public T? Response;

        public StatusResponse(EXIT_CODES status, T? response) => (Status, Response) = (status, response);
    }

    public class Union {
        private object? _value;

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public string? String() {
            return (string?)_value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public int? Int() {
            return (int?)_value;
        }

        public Union(object? value) {
            _value = value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public T? Get<T>() {
            return (T?)_value;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Set(object value) {
            _value = value;
        }
    }

    public enum Targets {
        None = -1,          // We permit untargetted code very briefly
        NES = 0,
        FDS = 1
    }

    struct InterruptableMultiline {
        public int Index;
        public string[] Lines;
    }

    public class Header {
        public byte[] raw;

        Header(int FDS_DiskAmt) {
            Target = Targets.FDS;
            raw = [0x46, 0x44, 0x53, 0x1a, (byte)(FDS_DiskAmt / ((FDS_DiskAmt > 65500) ? 65500 : 1))];
        }

        Header(int PRGROM, int CHRROM, bool NametableLayout, bool Battery, bool Trainer, bool AlternativeNametables, int Mapper, int ConsoleType, int SubMapper, int PRGRAM, int EEPROM, int CHRRAM, int CHRNVRAM, int CPU, int VsType, int VsPPU, int ExtenedConsoleType, int MiscROMs, int DefaultExpansionDevice) {
            Target = Targets.NES;
            if (PRGROM > 0x4000) {
                // since iNES2 spec allows for 2 bytes of PRGROM in multiples of 16KiB should it be that 0x4000 is specified its unlikely that it refers to ~268MiB so we do it this way
                PRGROM /= 0x4000;
            }

            if (CHRROM > 0x2000) {
                // likewise ~67MiB of Graphics ROM is always quite unbelievable
                CHRROM /= 0x2000;
            }

            // PRGRAM is stored as (0x40 << n)
            if (PRGRAM >= 0x40) {
                PRGRAM = (int)MathF.Log(PRGRAM) / 0x40;
            }

            // EEPROM is stored as (0x40 << n)
            if (EEPROM >= 0x40) {
                EEPROM = (int)MathF.Log(EEPROM) / 0x40;
            }

            // CHRRAM is stored as (0x40 << n)
            if (CHRRAM >= 0x40) {
                CHRRAM = (int)MathF.Log(CHRRAM) / 0x40;
            }

            // CHRNVRAM is stored as (0x40 << n)
            if (CHRNVRAM >= 0x40) {
                CHRNVRAM = (int)MathF.Log(CHRNVRAM) / 0x40;
            }

            raw = [
                0x4e, 0x45, 0x53, 0x1a, // NES<EOF>
                (byte)(PRGROM & 0xff),
                (byte)(CHRROM & 0xff),
                (byte)(
                    (NametableLayout       ? 1 : 0) |
                    (Battery               ? 2 : 0) |
                    (Trainer               ? 4 : 0) |
                    (AlternativeNametables ? 8 : 0) |
                    ((Mapper & 0b1111) <<  4)),
                (byte)(0x08 | CPU | (Mapper & 0xf0)),
                (byte)((Mapper >> 8) | (SubMapper << 4)),
                (byte)(
                    (PRGROM >> 8) |
                    (CHRROM & 0xf00) >> 8),
                (byte)(PRGRAM | (EEPROM << 4)),
                (byte)(CPU),
                (byte)(
                    ((ConsoleType == 1 ? 1: 0) * ((VsType << 4) |
                    VsPPU))
                    |
                    ((ConsoleType == 3 ? 1 : 0) * ExtenedConsoleType)),
                (byte)(MiscROMs),
                (byte)(DefaultExpansionDevice)
            ];
        }
    }


}