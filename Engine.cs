using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;

using Numinous.Langauges;

namespace Numinous {

    namespace Engine {
        internal ref struct NonLiteral {
            internal object value;
            internal object parent;
        }

        internal struct RunTimeVariable {
            internal int  size;   // in bytes
            internal bool signed; // false => unsigned
            internal bool endian; // false => little
        }

        internal enum AssembleTimeTypes {
            INT,        // assemble time integer
            STRING,     // assemble time string
            EXP,        // expression (not a string)
            VOID,       // void macro
            SCOPE,      // scope type
            RT,         // Runtime Variable
            REG,        // Register
            FLAG,       // CPU Status Flag
            PROC,       // Procedure
            INTER,      // Interrupt
            BANK        // Bank
        }

        internal enum AssemleTimeValueStatus {
            DECLARED,   // int foo;
            PARTIAL,    // int foo = defined_later;
            OK          // int foo = 2;
        }

        internal struct AssembleTimeValue {
            internal AssembleTimeTypes Type;
            internal AssemleTimeValueStatus Status;
            object Value;
        }


        internal enum ContextFetcherEnums {
            OK,
            MALFORMED,
            UNTERMINATED
        }

        /// <summary>
        /// This struct describes precisely the context of an error. Should describe what the bad region of a line is, specifying
        /// line number and source filename.
        /// </summary>
        internal struct ErrorContext {
            internal string SourceFilename;
            internal int LineNumber;
            internal int StepNumber;        // report with $"{ErrorLevel} : {ErrorType} at ({LineNumber}, {StepNumber})"
                                            // we will place wiggly lines under the bad region

            internal int IssueIndex;
            internal int IssueLength;

        }

        internal enum ErrorLevels {
            LOG, WARN, ERROR
        }

        internal enum ErrorTypes {
            None, SyntaxError, ParsingError, NothingToDo
        }

        internal enum DecodingPhase {
            TERMINAL, TOKEN
        }

        namespace Types {
            internal class Scope;
        }

        internal struct DatabaseItem {
            internal object value;
            internal object parent;
        }

        internal static class Engine {

            /// <summary>
            /// Used for reordering the Tokens into the mutation chronology.
            /// Parenthesis, indexing, attributes and scope routing are handling differently.
            /// Incrementor and Decrementor is also handled seperately
            /// </summary>
            internal static readonly List<string>[] OrderedOperators = [
                /* Unary            */ ["+", "-", "~", "!"],
                /* Switch           */ ["switch"],
                /* Multiplicative   */ ["*", "/", "%"],
                /* Additive         */ ["+", "-"],
                /* Shift            */ [">>", "<<"],
                /* Boolean And      */ ["&"],
                /* Boolean Xor      */ ["^"],
                /* Boolean Or       */ ["|"],
                /* Relational       */ [">", "<", ">=", "<=", "<=>"],
                /* Equality         */ ["==", "!="],
                /* Conditional And  */ ["&&"],
                /* Conditional Or   */ ["||"],
                /* Null coalesce    */ ["??"],
                /* Ternary          */ ["?", ":"],                                                                  // Violates VOV
                /* Assignment       */ ["=", "+=", "-=", "*=", "/=", "%=", "|=", "&=", "^=", "??=", ">>=", "<<="],  // Modifies in post
                /* Term             */ [","]
            ];

            internal static readonly string[] Reserved = [
                // directives
                "cpu", "rom", "define", "undefine", "include", "assert",
                
                // assemble time types
                "ref", "void", "int", "string", "exp", "bank", "proc", "reg", "flag", "scope", "namespace",
                
                // conditional assembly
                "if", "else", "loop", "break", "switch", "case", "return",
                
                // runtime type filters
                "ux", "ix", "lx", "bx", "ulx", "ilx", "ubx", "ibx", "num", 
                "x8", "x16", "x24", "x32", "x64", "l16", "l24", "l32", "l64", "b16", "b24", "b32", "b64",
            
                // runtime type memory adjectives
                "direct", "system", "program", "mapper", "field", "slow", "fast",

                // runtime types
                "u8", "i8", "u16", "i16", "u24", "i24", "u32", "i32", "u64", "i64",
                "ub16", "ib16", "ub24", "ib24", "ub32", "ib32", "ub64", "ib64",

                // Operators
                // Assignment
                "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", ">>=", "<<=",

                // Unary
                "==", "!=", "<=", ">=", ">", "<",

                // LLambda
                "=>",

                // Null Coalescence
                "??", "??=", "?.",

                // Math
                "+", "-", "*", "/", "%", "&", "|", "^", "~", "(", ")",

                // Indexing
                ",", "[", "]",

                // Numerical Systems
                "%", "$", "£", "0x", "0b", "0o",

                // Control
                ";", ":", "#", "\\", "\"", "{", "}", "?", ">", "<", "!", ".", ","
            ];

            internal static List<string> ReadValueReorderer(string[] Tokens) {
                List<string> Response = [];

                



                return Response;
            }

            /// <summary>
            /// Fetches context for the next step in decoding.
            /// Modified the Source File read Index for each accumulated context.
            /// Returns a list of string arrays for each split line of code.
            /// </summary>
            /// <param name="Source"></param>
            /// <param name="Index"></param>
            /// <returns></returns>
            internal static (List<string[]>?, ContextFetcherEnums Code) FetchContext(string[] Source, int Index, string Filename) {
                List<string[]> Tokens           = [];
                int      StartingIndex          = Index;            // Beginning Line Number for Error Reports
                int      StringIndex            = 0;                // How far into the raw strings we are
                int      VerifiedStringIndex    = 0;                // Sum of all verified (thus far) steps
                int      BufferTaleStringIndex  = 0;                // Last Open Encapsulation
                string   AccumulatedContext     = Source[Index];    // Accumolated Context for Error Reporting

                string[] TokenizedBuffer        = [.. Tokenize(Source[Index])];
                char[]   ContainerBuffer        = new char[TokenizedBuffer.Length];
                int[]    UnresolvedTermsBuffer  = new int[TokenizedBuffer.Length];
                int[]    nCapturedItemsBuffer   = new int[TokenizedBuffer.Length];
                bool[]   ResolvingTermsBuffer   = new bool[TokenizedBuffer.Length]; // begin collecting, post assignment begin resolving
                int      Hierachy;
                int      TokenizedCheckPoint    = 0;

                bool     HasSteps               = TokenizedBuffer.Contains(";");

                
                // Used to unify between string and char operator identifiers
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                void UnifiedModularHunk() {
                    if (!ResolvingTermsBuffer[1 + Hierachy]) {
                        UnresolvedTermsBuffer[1 + Hierachy] = nCapturedItemsBuffer[1 + Hierachy];
                        ResolvingTermsBuffer[1 + Hierachy] = true;
                    }
                }

                do {
                    Hierachy = -1;
                    for (int i = 0; i < TokenizedBuffer.Length; StringIndex += TokenizedBuffer[i].Length, i++) {
                        if      (TokenizedBuffer[i] == "+=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "-=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "*=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "/=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "%=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "|=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "^=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "&=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == ">>=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "<<=") UnifiedModularHunk();
                        else if (TokenizedBuffer[i] == "??=") UnifiedModularHunk();
                        else switch (TokenizedBuffer[i][0]) {
                            case '=':
                                UnifiedModularHunk();
                                break;

                            case ',':
                                if (Hierachy != -1 && ContainerBuffer[Hierachy] != '(' && ContainerBuffer[Hierachy] != '\"') {
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Comma, only parenthesis '()' and string parenthesis '\"\"' may contain commas")]}.",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                }
                                if (ResolvingTermsBuffer[1 + Hierachy]) UnresolvedTermsBuffer[1 + Hierachy]--; 
                                else                                    nCapturedItemsBuffer[ 1 + Hierachy]++;

                                if (UnresolvedTermsBuffer[1 + Hierachy] == -1) {
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Comma, the amount of terms to resolve is")]}: {nCapturedItemsBuffer[1 + Hierachy]}.",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                }
                                break;

                            case '(':
                                ++Hierachy;

                                nCapturedItemsBuffer[ 1 + Hierachy] = 0;        // New set of terms (begin 0)
                                ResolvingTermsBuffer[ 1 + Hierachy] = false;    // Mark as fetching
                                ContainerBuffer[Hierachy]           = '(';      // Log last used container
                                BufferTaleStringIndex = StringIndex;
                                continue;

                            case ')':
                                if (Hierachy == -1 || ContainerBuffer[Hierachy] != '(') {
                                    /*
                                     * May look like [1 + 2)    <-- invalid termination
                                     * Syntax Error : Unexpected Parenthesis (1, 2) :\n{line information}
                                     */
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Parenthesis in")]} {Filename}",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                } else {
                                    if (UnresolvedTermsBuffer[1 + Hierachy] != 0) {
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Terms left unaccouned for")]}: {nCapturedItemsBuffer[1 + Hierachy] - UnresolvedTermsBuffer[1 + Hierachy]}",
                                            StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    }
                                    Hierachy--;
                                    continue;
                                }

                            case '\"':
                                if (ContainerBuffer[Hierachy] == '\"') {
                                    ContainerBuffer[Hierachy] = '\x00';  // clear to indicate closed string
                                    Hierachy--;
                                    continue;
                                } else {
                                    Hierachy++;
                                    ContainerBuffer[Hierachy] = '\"';
                                    continue;
                                }

                            case '[':
                                Hierachy++;
                                ContainerBuffer[Hierachy] = '[';
                                BufferTaleStringIndex = StringIndex;
                                continue;

                            case ']':
                                if (Hierachy == -1 || ContainerBuffer[Hierachy] != '[') {
                                    /*
                                     * May look like {1 + 2]    <-- invalid termination
                                     * Syntax Error : Unexpected Bracket (1, 2) :\n{line information}
                                     */
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Bracket in")]} {Filename}",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                } else {
                                    Hierachy--;
                                    continue;
                                }

                            case '{':
                                Hierachy++;
                                ContainerBuffer[Hierachy] = '[';
                                BufferTaleStringIndex = StringIndex;
                                continue;

                            case '}':
                                if (Hierachy == -1 || ContainerBuffer[Hierachy] != '[') {
                                    /*
                                     * May look like (1 + 2}    <-- invalid termination
                                     * Syntax Error : Unexpected Brace (1, 2) :\n{line information}
                                     */
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Brace in")]} {Filename}",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                } else {
                                    Hierachy--;
                                    continue;
                                }

                            case ';':
                                if (Hierachy == -1) {
                                    if (UnresolvedTermsBuffer[0] != 0) {
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Terms left unaccounted for")]}: {nCapturedItemsBuffer[0] - UnresolvedTermsBuffer[0]}",
                                            StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    }
                                    ResolvingTermsBuffer[0] = false;
                                    nCapturedItemsBuffer[0] = 0;
                                    VerifiedStringIndex = StringIndex + 1;
                                    Tokens.Add(new string[i - TokenizedCheckPoint]);
                                    Array.Copy(TokenizedBuffer, TokenizedCheckPoint, Tokens[^1], 0, i - TokenizedCheckPoint);
                                    TokenizedCheckPoint = i + 1;
                                } else if (ContainerBuffer[Hierachy] == '\"') break;
                                  else {
                                    HasSteps |= true;
                                    /*
                                        * May look like (1 + 2;)    <-- invalid termination
                                        * Syntax Error : Unexpected Parenthesis (1, 2) :\n{line information}
                                        */

                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Line Termination in")]} {Filename}",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex, 1)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                }
                                continue;
                            }
                    }

                    if ((UnresolvedTermsBuffer[0] == 0) && Hierachy == -1) break;

                    // if no more context can be supplied, return unterminated and log error to user
                    if (++Index == Source.Length) {
                        Terminal.Error(
                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Could not Fetch Required Context from")]} {Filename}",
                            StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle($"{AccumulatedContext} ", StringIndex, 1)        
                        );
                        return (null, ContextFetcherEnums.UNTERMINATED);
                    }

                    ResolvingTermsBuffer[0] = false;
                    nCapturedItemsBuffer[0] = 0;
                    TokenizedBuffer = [.. TokenizedBuffer.TakeLast(TokenizedBuffer.Length - TokenizedCheckPoint)];
                    TokenizedCheckPoint = 0;
                    StringIndex         = VerifiedStringIndex;  // Reset for more accurate wiggling

                    AccumulatedContext += Source[Index];
                    TokenizedBuffer     = [.. TokenizedBuffer, .. Tokenize(Source[Index])];

                } while (true);


                return (Tokens, ContextFetcherEnums.OK);
            }

            internal static class Terminal {
                [Flags]
                internal enum AssemblyFlags {
                    Complete = 0x80,              // indicates that no context is required, as a task was completed here
                    Failed   = 0x40
                }
                
                internal static (string? InputPath, string? OutputPath, AssemblyFlags Flags) Parse(string[] args) {
                    string? InputPath = null, OutPutPath = null;
                    int StringIndex = 0;
                    string Flattened = string.Join(" ", args);
                    AssemblyFlags Flags = 0x00;

                    for (int i = 0; i < args.Length; i++, StringIndex += i == args.Length ? 0 : args[i].Length) {
                        switch (args[i]) {
                            case "-i":
                            case "--input":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "No Input Path Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    Flags |= AssemblyFlags.Failed;
                                } else if (InputPath != null) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Input Source File Path has already been specified")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    Flags |= AssemblyFlags.Failed;
                                } else {
                                    InputPath = args[++i];
                                }
                                break;

                            case "-o":
                            case "--output":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "No Output Path Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    Flags |= AssemblyFlags.Failed;
                                } else if (OutPutPath != null) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Output Binary File Path has already been specified")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    Flags |= AssemblyFlags.Failed;
                                }
                                OutPutPath = args[++i];
                                break;

                            case "-h":
                            case "--help":
                                Flags |= AssemblyFlags.Complete;
                                Log(ErrorTypes.None, DecodingPhase.TERMINAL,
$"""
Numinous 2a03 - GPL V2 Brette Allen 2026

-i | --input        | [path]    | {Language.Connectives[(Program.ActiveLanguage, "Entrypoint Source Assembly File")]}
-o | --output       | [path]    | {Language.Connectives[(Program.ActiveLanguage, "Output ROM/Disk Binary Output")]}
-h | --help         |           | {Language.Connectives[(Program.ActiveLanguage, "Display the help string (you did that)")]}
-l | --language     | [lang]    | {Language.Connectives[(Program.ActiveLanguage, "Choose a language to use")]}
-L | --Languages    |           | {Language.Connectives[(Program.ActiveLanguage, "Display all Languages")]}
       
""", null, null, null);
                                break;

                            case "-l":
                            case "--language":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "No Language Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    Flags |= AssemblyFlags.Failed;
                                    break;
                                }

                                Program.ActiveLanguage = args[++i] switch {
                                    "en_gb" => Languages.English_UK,
                                    "en_us" => Languages.English_US,
                                    "es"    => Languages.Spanish,
                                    "de"    => Languages.German,
                                    "ja"    => Languages.Japanese,
                                    "fr"    => Languages.French,
                                    "pt"    => Languages.Portuguese,
                                    "ru"    => Languages.Russian,
                                    "it"    => Languages.Italian,
                                    "ne"    => Languages.Dutch,
                                    "pl"    => Languages.Polish,
                                    "tr"    => Languages.Turkish,
                                    "vt"    => Languages.Vietnamese,
                                    "in"    => Languages.Indonesian,
                                    "cz"    => Languages.Czech,
                                    "ko"    => Languages.Korean,
                                    "uk"    => Languages.Ukrainian,
                                    "ar"    => Languages.Arabic,
                                    "sw"    => Languages.Swedish,
                                    "pe"    => Languages.Persian,
                                    "ch"    => Languages.Chinese,

                                    _       => Languages.Null
                                };

                                if (Program.ActiveLanguage == Languages.Null) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Invalid Language Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    Flags |= AssemblyFlags.Failed;
                                }
                                break;

                            case "-L":
                            case "--Languages":
                                Log(ErrorTypes.None, DecodingPhase.TERMINAL,@"
English (UK)      ""-l en_gb""
English (US)      ""-l en_us""
Español           ""-l es""
Deutsch           ""-l de""
日本語            ""-l ja""
Français          ""-l fr""
Português         ""-l pt""
Русский           ""-l ru""
Italiano          ""-l it""
Nederlands        ""-l ne""
Polski            ""-l pl""
Türkçe            ""-l tr""
Tiếng Việt        ""-l vt""
Bahasa Indonesia  ""-l in""
Čeština           ""-l cz""
한국어            ""-l ko""
Українська        ""-l uk""
العربية           ""-l ar""
Svenska           ""-l sw""
فارسی             ""-l pe""
中文              ""-l ch""
", null, null, null);
                                Flags |= AssemblyFlags.Complete;
                                break;

                            default:
                                Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Unrecognized Terminal Argument")]}.", null, null, ApplyWiggle(Flattened, 1 + StringIndex, args[i].Length));
                                Flags |= AssemblyFlags.Failed;
                                break;
                        }
                    }

                    return (InputPath, OutPutPath, Flags);
                }

                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    Languages UseLanguage = Program.ActiveLanguage;
                    if (Program.ActiveLanguage == Languages.Null) UseLanguage = Program.ActiveLanguage = Language.CaptureSystemLanguage();
                    if (Program.ActiveLanguage == Languages.Null) UseLanguage = Program.ActiveLanguage = Program.ActiveLanguage = Languages.English_UK;


                    Console.ForegroundColor = ErrorLevel switch {
                        ErrorLevels.LOG     => ConsoleColor.Cyan, 
                        ErrorLevels.WARN    => ConsoleColor.Yellow, 
                        ErrorLevels.ERROR   => ConsoleColor.Red, 
                        
                        _                   => ConsoleColor.White
                    };

                    string ErrorTypeString, ErrorTypeConnective, LocationString, DecodePhaseString;

                    if (ErrorType  == ErrorTypes.None) {
                        Console.WriteLine(Message);
                        goto Exit;
                    }

                    ErrorTypeString     = Language.ErrorTypeMessages[(UseLanguage, ErrorType)];
                    ErrorTypeConnective = Language.Connectives[(UseLanguage, "During")];
                    DecodePhaseString   = Language.DecodePhaseMessages[(UseLanguage, Phase)];
                    LocationString      = LineNumber == null ? "" : (StepNumber == null ? $"({LineNumber})" : $"({LineNumber}, {StepNumber})");
                    Context = Context == null ? "" : $": {Context}";

                    // Something Error During Something Phase :: Could not do a thing (1, 2) : ah, the issue is here.
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {LocationString}{Context}");
                    
                Exit:
                    Console.ResetColor();
                }

                internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }
            }

            internal static string ApplyWiggle(string input, int start, int length) {
                const char wiggle = '\u0330';
                var builder = new StringBuilder(input.Length * 2);

                for (int i = 0; i < input.Length; i++) {
                    builder.Append(input[i]);
                    if (i >= start && i < start + length)
                        builder.Append(wiggle);
                }

                return builder.ToString();
            }

            internal static void AddContext(string Filepath) {
                string[] Throwaway = File.ReadAllLines(Filepath);
                if (Throwaway.Length == 0) {
                    Terminal.Warn(ErrorTypes.NothingToDo, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Source file")]} {Filepath} {Language.Connectives[(Program.ActiveLanguage, "has no contents")]}", null, null, null);
                    return;
                }
                Program.SourceFileNameBuffer.Add(Filepath);
                Program.SourceFileContentBuffer.Add(Throwaway);
            }


            // Generated function : I don't know how regex works
            /// <summary>
            /// Tokenizes a line of code. SPACES ARE IMPORTANT FOR LINE INDEX MATH
            /// </summary>
            /// <param name="input"></param>
            /// <returns></returns>
            internal static List<string> Tokenize(string input) {
                // The characters to treat as separators, space included
                string separators = @"!""£$%\^&*()+\-=\[\]{};:'@#~\\|,<.>/?\s";

                // Manually form a valid regex character class from the separators
                string pattern = $"([^{separators}]+|[{separators}])";

                var matches = Regex.Matches(input, pattern);
                var tokens = new List<string>();

                foreach (Match match in matches) {
                    if (!string.IsNullOrEmpty(match.Value))
                        tokens.Add(match.Value);
                }

                return tokens;
            }
        }
    }
}