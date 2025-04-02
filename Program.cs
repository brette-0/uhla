using System.Collections.Specialized;
using System.Linq.Expressions;
using System.Runtime.CompilerServices;
using System.Text.RegularExpressions;
using System.Data;

using Tataru;
using Microsoft.CodeAnalysis.CSharp.Scripting;
using Microsoft.CodeAnalysis.Scripting;

public static class Program {
    static readonly string OperatorRegex = @"(\d+|\w+|<=|>=|==|!=|\+\+|--|\+=|-=|\*=|/=|\$=|\??=|%=|&=|\|=|\^=|>>=|<<=|<<|>>|<=|>=|==|!=|\+|\-|\*|\/|&|\| |\^|\%|\$|!|#|\\|@|[(){}\[\]|<|>|\|]+)";
    static readonly HashSet<string> OperatorLUT = [
        "<", ">", "==", "!=", "<=", ">=", "?", "??", "+", "-", "*", "/", "&", "|", "^",
        "%", "$", "!", ">>", "<<", ">|", "|<", "++", "--", "+=", "-=", "*=", "/=",
        "$=", "??=", "%=", "&=", "|=", "^=", ">>=", "<<=", "#", "\\", "@", "(", ")", "[", "]", "{", "}"
    ];

    static bool DebugFile = false;
    static bool ListingFile = false;

    static Targets Target = Targets.None;

    // line position at include : file as array of lines split by new line
    static InterruptableMultiline[] AssemblyFileTree = [];

    static string SourceEntryPointFilePath = "";
    static string TargetEntryPointFilePath = "";

    // scope[name] = value for all labels
    static Dictionary<string, Dictionary<string, int?>> LabelDataBase = [];
    static string ActiveScope = "root";


    static async Task<int> Eval(string expression) {
        try {
            return CSharpScript.EvaluateAsync<int>(
                expression,
                ScriptOptions.Default.WithImports("System")
            ).Result;
        } catch (Exception) {
            return 0;
        }
    }

    public static int Main(String[] args) {

        LabelDataBase["root"] = [];                     // create root scope

        string TestString = "2 $ 3";
        int foo = RequestEvaluate(ref TestString).Response;


        EXIT_CODES resp = ResolveArguments(ref args);   // digest cli args
        if (resp != 0) return (int)resp;                // if fail, return with fail ctx

        AssemblyFileTree = [.. AssemblyFileTree, new InterruptableMultiline {Index = 0, Lines = File.ReadAllLines(SourceEntryPointFilePath) }];

        //AssemblyFileTree.Add(0, new InterruptableMultiline { 0, File.ReadAllLines(SourceEntryPointFilePath) });  // Index 0 (top/entryhpoint) | Included at 0 (source include) | contents of file
        Assemble();
        return 0;
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

    private enum EvaluationLevel {
        OK,
        WAIT
    }

    struct ReconstructSegment {
        public EvaluationLevel Level;
        public string Segment;
        public int Index;
    }

    private static StatusResponse<int> RequestEvaluate(ref string _Context) {
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

        ReconstructSegment[] Reconstructs = [];
        Reconstructs =
        [ .. Reconstructs,
            new() {
                Segment = "",
                Level = EvaluationLevel.OK,
            },
        ];

        bool OperatorClock = false;
        System.Globalization.NumberStyles ActiveDenotation = System.Globalization.NumberStyles.Number;   // default to decimal denotation alwayss

        static int FetchDenotedNumber(ref string Context, System.Globalization.NumberStyles NumberStyle) {
            return int.Parse(Context.Trim().ToLower(), NumberStyle);
        }

        string ExpressionAppend;

        for (int iter = 0; iter < Context.Length; iter++) {
            if (OperatorLUT.Contains(Context[iter].Trim())) {
                // if Operator is % or $ it could be binary or hex denotation
                if (OperatorClock) {
                    switch (Context[iter].Trim()) {
                        case "(":
                            Reconstructs = [.. Reconstructs, new ReconstructSegment { Level = EvaluationLevel.OK, Segment = "" }];
                            continue;

                        case ")":
                            if (Reconstructs[^1].Level == EvaluationLevel.WAIT) {
                                Reconstructs[^2].Level = EvaluationLevel.WAIT;
                            }
                            Reconstructs[^2].Segment += $"({Reconstructs[^1].Segment})";
                            Reconstructs = [.. Reconstructs.Take(Reconstructs.Length - 2)];
                            break;

                        default:
                            Reconstructs[^1].Segment += Context[iter].Trim();
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
                            Reconstructs[^1].Segment += ExpressionAppend;
                            break;

                        default:    // SYNTAX ERROR | Two Consecutive Operators
                            return new StatusResponse<int> {
                                Status = EXIT_CODES.SYNTAX_ERROR
                            };
                    }
                }
            } else {
                try {
                    int Temp = int.Parse(Context[iter].Trim());
                    ExpressionAppend = Temp.ToString();
                    Reconstructs[^1].Segment += ExpressionAppend;
                } catch {
                    if (LabelDataBase[ActiveScope].ContainsKey(Context[iter].Trim())) {
                        if (LabelDataBase[ActiveScope][Context[iter].Trim()] is null) {
                            Reconstructs[^-1].Segment += Context[iter];
                        } else {
                            Reconstructs[^-1].Segment += LabelDataBase[ActiveScope][Context[iter].Trim()];
                        }
                    }
                }
            }
            OperatorClock = !OperatorClock;
        }


        return new StatusResponse<int>(EXIT_CODES.OK, Eval(ReplaceExponentiation(Reconstructs[0].Segment)).Result);
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

        public StatusResponse(EXIT_CODES status, T? response) => (this.Status, this.Response) = (status, response);
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