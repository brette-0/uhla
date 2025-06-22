using System.Collections.Generic;
using System.ComponentModel;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Xml.XPath;
using Numinous.Langauges;

namespace Numinous {
    namespace Engine {
        internal enum AssembleTimeTypes  : byte {
            WAIT,       // Waiting on first value (Evaluator Solving)
            INT,        // assemble time integer
            STRING,     // assemble time string
            DEFINE,     // define, capture then tokenize for CF
            VOID,       // void macro
            SCOPE,      // scope type
            RT,         // Runtime Variable
            REG,        // Register
            FLAG,       // CPU Status Flag
            PROC,       // Procedure
            INTER,      // Interrupt
            BANK,       // Bank

            CINT,       // Constant int
            CSTRING,    // Constant string
            CSCOPE,     // Constant Scope reference
            CRT,        // Constant runtime reference
            CREG,       // Constant register reference
            CFLAG,      // Constant flag reference
            CPROC,      // Constant procedure reference
            CINTER,     // Constant interrupt reference
            CBANK,      // Constant bank reference

            MACRO,      // void macro
            MINT,       // int macro
            MSTRING,    // string macro
            MEXP,       // expression macro
        }

        internal enum AssemleTimeValueStatus : byte {
            DECLARED,   // int foo;
            PARTIAL,    // int foo = defined_later;
            OK          // int foo = 2;
        }

        internal enum ContextFetcherEnums : byte {
            OK,
            MALFORMED,
            UNTERMINATED
        }

        internal struct RunTimeVariable {
            internal int size;      // in bytes
            internal bool signed;   // false => unsigned
            internal bool endian;   // false => little
        }

        internal enum ErrorLevels : byte {
            LOG, WARN, ERROR
        }

        internal enum ErrorTypes : byte {
            None, SyntaxError, ParsingError, NothingToDo
        }

        internal enum DecodingPhase : byte {
            TERMINAL, TOKEN, EVALUATION
        }

        internal enum Expectations : byte {
            VALUE,
            OPERATOR
        }

        internal static class Engine {
           internal struct DeltaTokens_t {
                internal string[] DeltaTokens;
                internal int Hierarchy;
                internal int Terms;
            }

            internal enum Unary : byte {
                INC,
                DEC,
                ABS,
                NEG,
                BIT,
                NOT
            };

            /// <summary>
            /// This will either push back an object reference, or a constant object literal.
            /// This will ignore all forms of containers - expects no type, just detects and works with it.
            /// If the Resolve is null, it means an error occured.
            /// Container may be either
            ///     (   - push back object ref tuple where possible, returning constant values where not
            ///     {   - push back cexp of tuple
            ///     [   - push back cint with creg
            /// </summary>
            /// <param name="Tokens"></param>
            /// <returns></returns>
            static internal (string? Representation, AssembleTimeTypes ResolveType, char Container) LinearEvaluate(List<string> Tokens) {
                int PendingOperations               = Tokens.Count(t => GetHierachy(t) != -1);
                bool ExpectValueToggle              = true;
                bool Mutated                        = false;    // does not affect object reference perpetuation.
                List<Unary> UnaryBuffer             = [];

                List<(object data, AssembleTimeTypes type)> DataBuffer             = [];
                List<AssembleTimeTypes> TypeBuffer  = [];

                Dictionary<string, (object data, AssembleTimeTypes type)> ActiveScope = Program.ActiveScope;

                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                void UnifiedConstAppendSystem(int Int) {
                    for (int u = UnaryBuffer.Count - 1; u >= 0; u--) {
                        switch (UnaryBuffer[u]) {
                            case Unary.ABS:
                                Int = Math.Abs(Int);
                                break;

                            case Unary.NEG:
                                Int = -Int;
                                break;

                            case Unary.BIT:
                                Int = ~Int;
                                break;

                            case Unary.NOT:
                                Int = 0 == Int ? 0 : 1;
                                break;
                        }

                        DataBuffer.Add((new Dictionary<string, (object data, AssembleTimeTypes type)> {
                            {"self", (Int, AssembleTimeTypes.CINT) }
                        }, AssembleTimeTypes.CINT));
                    }
                }
                
                for (int i = 1; i < Tokens.Count - 1; i++) {
                    if (Tokens[i][0] == ' ' || Tokens[i][0] == '\t') continue;
                    if (ExpectValueToggle) {
                        switch (Tokens[i]) {
                            case "++": if (Mutated) goto Error; UnaryBuffer.Add(Unary.INC); break;
                            case "--": if (Mutated) goto Error; UnaryBuffer.Add(Unary.DEC); break;
                            case "+":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.ABS); break;
                            case "-":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.NEG); break;
                            case "~":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.BIT); break;
                            case "!":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.NOT); break;

                            case "\\":

                                break;
                            case "\"":
                                ActiveScope = (Dictionary<string, (object data, AssembleTimeTypes type)>)ActiveScope["parent"].data;
                                Mutated = true;
                                break;


                            default:
                                if (GetHierachy(Tokens[i]) != -1) {
                                    // error, attempted to mutate a non unary operator (just as bad, but needs a different check)
                                    return default;
                                }

                                switch (Tokens[i][0]) {
                                    case '\"':
                                        i += 2;
                                        string StringCapture = $"\"{Tokens[i - 1]}";
                                        for (; i < Tokens.Count - 1 && Tokens[i - 1] != "\""; i++) {
                                            StringCapture += Tokens[i];
                                        }
                                        if (StringCapture[^1] != '\"') {
                                            // error string literal did not terminate
                                            return default;
                                        }

                                        object UnaryCapture = StringCapture;
                                        AssembleTimeTypes ResultType = AssembleTimeTypes.CSTRING;

                                        if (Mutated) {
                                            // error, cant mutate a literal
                                            return default;
                                        }

                                        for (int u = UnaryBuffer.Count - 1; u >= 0; u--) {
                                            switch (UnaryBuffer[u]) {
                                                case Unary.ABS:
                                                    if (ResultType == AssembleTimeTypes.CINT) {
                                                        UnaryCapture = Math.Abs((int)UnaryCapture);
                                                    } else {
                                                        UnaryCapture = ((string)UnaryCapture).ToUpper();
                                                    }
                                                    break;

                                                case Unary.NEG:
                                                    if (ResultType == AssembleTimeTypes.CINT) {
                                                        UnaryCapture = -(int)UnaryCapture;
                                                    } else {
                                                        UnaryCapture = ((string)UnaryCapture).ToLower();
                                                    }

                                                    break;

                                                case Unary.BIT:
                                                    if (ResultType == AssembleTimeTypes.CINT) {
                                                        UnaryCapture = ~(int)UnaryCapture;
                                                    } else {
                                                        UnaryCapture = ((string)UnaryCapture).Length;
                                                        ResultType = AssembleTimeTypes.CINT;
                                                    }

                                                    break;

                                                case Unary.NOT:
                                                    if (ResultType == AssembleTimeTypes.CINT) {
                                                        UnaryCapture = 0 == (int)UnaryCapture ? 0 : 1;
                                                    } else {
                                                        UnaryCapture = new string(' ', ((string)UnaryCapture).Length);
                                                    }

                                                    break;
                                            }
                                        }

                                        if (ResultType == AssembleTimeTypes.CINT) {
                                            DataBuffer.Add((new Dictionary<string, (object data, AssembleTimeTypes type)> {
                                                { "self", (UnaryCapture,            AssembleTimeTypes.CINT) },
                                            }, AssembleTimeTypes.CINT));
                                        } else {
                                            DataBuffer.Add((new Dictionary<string, (object data, AssembleTimeTypes type)> {
                                                { "self",   (UnaryCapture,          AssembleTimeTypes.CSTRING) },
                                                { "length", (StringCapture.Length,  AssembleTimeTypes.CINT) },
                                            }, AssembleTimeTypes.CSTRING));
                                        }

                                        // no need to bounds check this
                                        if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
                                            // error, post mut. Same issue as pre-mut on literal
                                            return default;
                                        }

                                        break;

                                    case '0':

                                        if (Mutated) {
                                            // error, cant mutate a literal
                                            return default;
                                        }

                                        switch (Tokens[i][1]) {
                                            case 'x':
                                                // hexadecimal int literal
                                                UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 16));
                                                continue;

                                            case 'b':
                                                // binary int literal
                                                UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 2));
                                                continue;

                                            case 'd':
                                                // octal int literal
                                                UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 8));
                                                continue;

                                            default:
                                                break;
                                        }
                                        goto case '9';

                                    case '1':
                                    case '2':
                                    case '3':
                                    case '4':
                                    case '5':
                                    case '6':
                                    case '7':
                                    case '8':
                                    case '9':

                                        if (Mutated) {
                                            // error, cant mutate a literal
                                            return default;
                                        }

                                        // decimal int literal
                                        UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i].ToLowerInvariant(), 8));
                                        break;
                                            
                                    case '$':

                                        if (Mutated) {
                                            // error, cant mutate a literal
                                            return default;
                                        }

                                        // hexadecimal int literal
                                        UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 16));
                                        break;

                                    case '%':

                                        if (Mutated) {
                                            // error, cant mutate a literal
                                            return default;
                                        }

                                        // binary int literal
                                        UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 2));
                                        break;

                                    case '£':

                                        if (Mutated) {
                                            // error, cant mutate a literal
                                            return default;
                                        }

                                        // octal int literal
                                        UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 8));
                                        break;

                                    default:
                                        if (ActiveScope.TryGetValue(Tokens[i], out (object data, AssembleTimeTypes type) value)){
                                            DataBuffer.Add(value);      // object reference
                                        } else {
                                            // error, invalid token
                                            return default;
                                        }
                                        break;
                                        // non literal
                                }
                                break;

                            Error: return default;
                        }
                    } else {
                        ActiveScope = Program.ActiveScope;
                        UnaryBuffer.Clear();
                    }
                }

                
                AssembleTimeTypes FinalType = AssembleTimeTypes.WAIT;
                string result = "";

                return Tokens[^1][0] switch {
                    ')'  => ($"{result}",       FinalType,                  Tokens[^1][0]),
                    ']'  => ($"[{result}]",     AssembleTimeTypes.CINT,     Tokens[^1][0]),
                    '}'  => ($"{result}",       AssembleTimeTypes.CSTRING,  Tokens[^1][0]),// feeds into a fstring, will result a string component
                    '\"' => ($"\"{result}\"",   AssembleTimeTypes.CSTRING,  Tokens[^1][0]),// returns cstring with static members
                    _    => default,
                };
            }

            static internal (bool, object) DeltaEvaluate(AssembleTimeTypes Type, List<DeltaTokens_t> Tokens, int MaxHierachy) {
                (bool, object) Response = default;

                while (MaxHierachy >= -1) {
                    int i; for (i = 0; i < Tokens.Count; i++) {
                        if (Tokens[i].Hierarchy == MaxHierachy) break;
                    }

                    if (i == Tokens.Count) {
                        MaxHierachy--;
                        continue;
                    }
                    // resolve what is inside : capture object, type and object_reference

                    if (MaxHierachy == -1) break;  // resolve, don't merge (you can't merge this with anything)


                    // to merge, append two to one
                    // return multiple terms as array of (type, value)
                    DeltaTokens_t MutToks = new() {
                        DeltaTokens = [.. Tokens[i - 1].DeltaTokens, .. Tokens[i].DeltaTokens, .. Tokens[i + 1].DeltaTokens],
                        Hierarchy = Tokens[i - 1].Hierarchy,
                        Terms = Tokens[i - 1].Terms,
                    };

                    // merge 3 tokens into 1
                    Tokens[i - 1] = MutToks;
                    Tokens.RemoveAt(i);
                    Tokens.RemoveAt(i);
                }

                return Response;
            }

            internal static bool IsNonLiteral(char First) =>
                    First switch {
                        '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9' or '$' or '%' or '&' or '+' or '-' or '!' or '^' or '*' or '[' or ']' or '{' or '}' or '\'' or '#' or '~' or ':' or ',' or '<' or '.' or '>' or '/' or '?' => false,
                        _ => true,
                    };

            internal static int GetHierachy(string op) => op switch {
                /* Property*/
                "." or "?."
                => 1,

                /* Multiplicative*/
                "*" or "/" or "%"
                => 2,

                /* Additive*/
                "+" or "-"
                => 3,

                /* Shift*/
                ">>" or "<<"
                => 4,

                /* Boolean And*/
                "&"
                => 5,

                /* Boolean Xor*/
                "^"
                => 6,

                /* Boolean Or*/
                "|"
                => 7,

                /* Relational*/
                ">" or "<" or ">=" or "<=" or "<=>"
                => 8,

                /* Equality*/
                "==" or "!="
                => 9,

                /* Conditional And*/
                "&&"
                => 10,

                /* Conditional Or*/
                "||"
                => 11,

                /* Null coalesce*/
                "??"
                => 12,

                /* Ternary*/
                "?" or ":"
                => 13,

                /* Assignment*/
                "=" or "+=" or "-=" or "*=" or "/=" or "%=" or "|=" or "&=" or "^=" or "??=" or ">>=" or "<<="
                => 14,

                /* Term*/
                ","
                => 15,

                /* Reserved*/
                "#" or "'"
                => 16,

                /* Not found*/
                _
                => -1
            };


            internal static readonly string[] Reserved = [
                // directives
                "cpu", "rom", "define", "undefine", "include", "assert",
                
                // assemble time types
                "ref", "void", "int", "string", "exp", "bank", "proc", "reg", "flag", "scope", "namespace",
                
                // conditional assembly
                "if", "else", "loop", "break", "return",
                
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

            // Best if inline, we want it to just use the result of tokenizing immediately.
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal static List<string> SolveDefines(List<string> tokens) {
                bool DidReplace;

                do {
                    DidReplace = false;
                    List<string> UpdatedTokens = [];

                    for (int i = 0; i < tokens.Count; i++) {
                        string token = tokens[i];
                        if (Program.ActiveScope.TryGetValue(token, out (object data, AssembleTimeTypes type) CapturedValue) && CapturedValue.type == AssembleTimeTypes.DEFINE) {
                            string Capture = (string)CapturedValue.data;
                            UpdatedTokens.AddRange(Tokenize(Capture));
                            DidReplace = true;
                        } else {
                            UpdatedTokens.Add(token);
                        }
                    }
                    
                    tokens = UpdatedTokens;
                } while (DidReplace);
                return tokens;
            }

            /// <summary>
            /// Fetches context for the next step in decoding.
            /// Modified the Source File read Index for each accumulated context.
            /// Returns a list of string arrays for each split line of code.
            /// </summary>
            /// <param name="Source"></param>
            /// <param name="Index"></param>
            /// <returns></returns>
            internal static (List<(List<DeltaTokens_t> DeltaTokens, int MaxHierachy)>?, ContextFetcherEnums Code) FetchContext(string[] Source, int Index, string Filename) {
                int      StartingIndex              = Index;            // Beginning Line Number for Error Reports
                int      StringIndex                = 0;                // How far into the raw strings we are
                int      VerifiedStringIndex        = 0;                // Sum of all verified (thus far) steps
                int      ContainerBufferTaleStringIndex  = 0;           // Last Open Encapsulation
                string   AccumulatedContext         = Source[Index];    // Accumolated Context for Error Reporting

                string[] TokenizedBuffer            = [.. SolveDefines(Tokenize(Source[Index]))];
                char[]   ContainerBuffer            = new char[TokenizedBuffer.Length];
                int[]    UnresolvedTermsBuffer      = new  int[TokenizedBuffer.Length + 1];
                int[]    nCapturedItemsBuffer       = new  int[TokenizedBuffer.Length + 1];
                bool[]   ResolvingTermsBuffer       = new bool[TokenizedBuffer.Length + 1]; // begin collecting, post assignment begin resolving
                int      Hierarchy, MaxHierachy;
                int      TokenizedCheckPoint        = 0;

                bool     HasSteps                   = TokenizedBuffer.Contains(";");
                int      LastNonEmptyTokenIndex     = 0;

                List<(List<DeltaTokens_t> Tokens, int MaxHierachy)> StepMatrixes    = [];
                (List<DeltaTokens_t> Tokens, int MaxHierachy) StepMatrix = ([], 0);
                int      HierarchyDeltaCheckpoint    = 0;

                
                // Used to unify between string and char operator identifiers
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                void UnifiedModularCompareAssignment() {
                    if (!ResolvingTermsBuffer[1 + Hierarchy]) {
                        UnresolvedTermsBuffer[1 + Hierarchy] = nCapturedItemsBuffer[1 + Hierarchy];
                        ResolvingTermsBuffer[1 + Hierarchy] = true;
                    }
                }

                // Clone method for final step
                [MethodImpl(MethodImplOptions.AggressiveInlining)]
                static (List<DeltaTokens_t>, int MaxHierarchy) CloneChunk((List<DeltaTokens_t>, int MaxHierarchy) source) {
                    var clone = new List<DeltaTokens_t>(source.Item1.Count);
                    foreach (var item in source.Item1)
                        clone.Add(new DeltaTokens_t {
                            DeltaTokens = (string[])item.DeltaTokens.Clone(),
                            Hierarchy = item.Hierarchy,
                            Terms = item.Terms
                        });
                    return (clone, source.MaxHierarchy);
                }

                do {
                    MaxHierachy = Hierarchy = -1;
                    for (int i = 0; i < TokenizedBuffer.Length; StringIndex += TokenizedBuffer[i].Length, i++) {
                        // capture tokens inside without checking for anything until code brace ends. Overrules everything.
                        // cannot evalute as we have not gauranteed intentional access if is conditional and the condition evaluates to negative.
                        if (Hierarchy == 0 && ContainerBuffer[Hierarchy] == '{' && TokenizedBuffer[i][0] != '}') continue;
                        if (TokenizedBuffer[i][0] == ' ' || TokenizedBuffer[i][0] == '\t') continue;
                        LastNonEmptyTokenIndex = i;

                        if (TokenizedBuffer[i][0] == '\"') {
                            if (Hierarchy == -1 || (ContainerBuffer[Hierarchy] != '\"' && ContainerBuffer[Hierarchy] != '$')) {
                                ContainerBuffer[++Hierarchy] = '\"';
                            } else {
                                ContainerBuffer[Hierarchy] = '\x00';  // clear to indicate closed string
                                Hierarchy--;
                            }
                        }
                        else if (Hierarchy != -1 && ContainerBuffer[Hierarchy] == '\"') continue;
                        else if (TokenizedBuffer[i] == "+=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "-=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "*=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "/=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "%=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "|=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "^=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "&=")  UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == ">>=") UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "<<=") UnifiedModularCompareAssignment();
                        else if (TokenizedBuffer[i] == "??=") UnifiedModularCompareAssignment();

                        else if (TokenizedBuffer[i] == "$\"") ContainerBuffer[++Hierarchy] = '$';
                        else switch (TokenizedBuffer[i][0]) {
                                case '=':
                                    UnifiedModularCompareAssignment();
                                    continue;

                                case ',':
                                    if (Hierarchy != -1 && ContainerBuffer[Hierarchy] == '[') {
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Brackets may only contain one term")]}.",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex + 1, 1)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    }
                                    if (ResolvingTermsBuffer[1 + Hierarchy]) UnresolvedTermsBuffer[1 + Hierarchy]--;
                                    else nCapturedItemsBuffer[1 + Hierarchy]++;

                                    if (UnresolvedTermsBuffer[1 + Hierarchy] == -1) {
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Comma, the amount of terms to resolve is")]} {1 + nCapturedItemsBuffer[1 + Hierarchy]}.",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex + 1, 1)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    }
                                    continue;

                                case '(':
                                    StepMatrix.Tokens.Add(new() { 
                                        DeltaTokens = new string[i - HierarchyDeltaCheckpoint], 
                                        Hierarchy = Hierarchy + 1, 
                                        Terms = nCapturedItemsBuffer[1 + Hierarchy] 
                                    });
                                    Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                    HierarchyDeltaCheckpoint = i;

                                    if (++Hierarchy > MaxHierachy) MaxHierachy = Hierarchy;

                                    nCapturedItemsBuffer[1 + Hierarchy] = 0;         // New set of terms (begin 0)
                                    ResolvingTermsBuffer[1 + Hierarchy] = false;     // Mark as fetching
                                    ContainerBuffer[Hierarchy] = '(';                // Log last used container
                                    ContainerBufferTaleStringIndex = StringIndex;
                                    continue;

                                case ')':
                                    if (Hierarchy == -1 || ContainerBuffer[Hierarchy] != '(') {
                                        /*
                                         * May look like [1 + 2)    <-- invalid termination
                                         * Syntax Error : Unexpected Parenthesis (1, 2) :\n{line information}
                                         */
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Parenthesis in")]} {Filename}",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, ContainerBufferTaleStringIndex + 1, StringIndex - ContainerBufferTaleStringIndex)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    } else {
                                        if (UnresolvedTermsBuffer[1 + Hierarchy] != 0) {
                                            Terminal.Error(
                                                ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Terms left unaccounted for")]}: {1 + nCapturedItemsBuffer[1 + Hierarchy] - UnresolvedTermsBuffer[1 + Hierarchy]}",
                                                StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex, 1)
                                            );
                                            return (null, ContextFetcherEnums.MALFORMED);
                                        }

                                        StepMatrix.Tokens.Add(new() {
                                            DeltaTokens = new string[i - HierarchyDeltaCheckpoint],
                                            Hierarchy = Hierarchy + 1,
                                            Terms = nCapturedItemsBuffer[1 + Hierarchy]
                                        });
                                        Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                        HierarchyDeltaCheckpoint = i;

                                        Hierarchy--;
                                        continue;
                                    }

                                case '[':
                                    StepMatrix.Tokens.Add(new() {
                                        DeltaTokens = new string[i - HierarchyDeltaCheckpoint],
                                        Hierarchy = Hierarchy + 1,
                                        Terms = nCapturedItemsBuffer[1 + Hierarchy]
                                    }); 
                                    Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                    HierarchyDeltaCheckpoint = i;
                                    // We need to ensure we do not skip the [ as it counts as the index operator

                                    if (++Hierarchy > MaxHierachy) MaxHierachy = Hierarchy;
                                    ContainerBuffer[Hierarchy] = '[';
                                    ContainerBufferTaleStringIndex = StringIndex;
                                    continue;

                                case ']':
                                    if (Hierarchy == -1 || ContainerBuffer[Hierarchy] != '[') {
                                        /*
                                         * May look like {1 + 2]    <-- invalid termination
                                         * Syntax Error : Unexpected Bracket (1, 2) :\n{line information}
                                         */
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Bracket in")]} {Filename}",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, ContainerBufferTaleStringIndex + 1, StringIndex - ContainerBufferTaleStringIndex)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    } else {

                                        StepMatrix.Tokens.Add(new() {
                                            DeltaTokens = new string[i - HierarchyDeltaCheckpoint],
                                            Hierarchy = Hierarchy + 1,
                                            Terms = nCapturedItemsBuffer[1 + Hierarchy]
                                        });
                                        Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                        HierarchyDeltaCheckpoint = i;

                                        Hierarchy--;
                                        continue;
                                    }

                                // braces are code block only, unless in format string
                                case '{':
                                    if (Hierarchy != -1 && ContainerBuffer[Hierarchy] != '$') {
                                        /*
                                         * May look like ({1 + 1}) or (+ {})
                                         */
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Brace in")]} {Filename}",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, ContainerBufferTaleStringIndex + 1, StringIndex - ContainerBufferTaleStringIndex)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    }

                                    StepMatrix.Tokens.Add(new() {
                                        DeltaTokens = new string[i - HierarchyDeltaCheckpoint],
                                        Hierarchy = Hierarchy + 1,
                                        Terms = nCapturedItemsBuffer[1 + Hierarchy]
                                    });
                                    Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                    HierarchyDeltaCheckpoint = i ;

                                    if (++Hierarchy > MaxHierachy) MaxHierachy = Hierarchy;
                                    ContainerBuffer[Hierarchy] = '{';
                                    ContainerBufferTaleStringIndex = StringIndex;
                                    continue;

                                case '}':
                                    if (Hierarchy == -1 || ContainerBuffer[Hierarchy] != '{') {
                                        /*
                                         * May look like (1 + 2}    <-- invalid termination
                                         * Syntax Error : Unexpected Brace (1, 2) :\n{line information}
                                         */
                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Brace in")]} {Filename}",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, ContainerBufferTaleStringIndex + 1, StringIndex - ContainerBufferTaleStringIndex)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    } else {
                                        StepMatrix.Tokens.Add(new() {
                                            DeltaTokens = new string[i - HierarchyDeltaCheckpoint],
                                            Hierarchy = Hierarchy + 1,
                                            Terms = nCapturedItemsBuffer[1 + Hierarchy]
                                        });
                                        Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                        HierarchyDeltaCheckpoint = i;

                                        Hierarchy--;
                                        continue;
                                    }

                                case ';':
                                    if (Hierarchy == -1) {
                                        if (UnresolvedTermsBuffer[0] != 0) {
                                            Terminal.Error(
                                                ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Terms left unaccounted for")]}: {1 + nCapturedItemsBuffer[0] - UnresolvedTermsBuffer[0]}",
                                                StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex, 1)
                                            );
                                            return (null, ContextFetcherEnums.MALFORMED);
                                        }

                                        ResolvingTermsBuffer[0] = false;
                                        nCapturedItemsBuffer[0] = 0;
                                        VerifiedStringIndex = StringIndex + 1;

                                        StepMatrix.Tokens.Add(new() {
                                            DeltaTokens = new string[i - HierarchyDeltaCheckpoint],
                                            Hierarchy = Hierarchy + 1,
                                            Terms = nCapturedItemsBuffer[1 + Hierarchy]
                                        });
                                        Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, i - HierarchyDeltaCheckpoint);
                                        HierarchyDeltaCheckpoint = i + 1;

                                        StepMatrix.MaxHierachy = MaxHierachy;
                                        StepMatrixes.Add(CloneChunk(StepMatrix));
                                        StepMatrix.Tokens.Clear();
                                        TokenizedCheckPoint = i + 1;
                                    } else {
                                        HasSteps |= true;
                                        /*
                                            * May look like (1 + 2;)    <-- invalid termination
                                            * Syntax Error : Unexpected Parenthesis (1, 2) :\n{line information}
                                            */

                                        Terminal.Error(
                                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Unexpected Line Termination in")]} {Filename}",
                                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex, 1)
                                        );
                                        return (null, ContextFetcherEnums.MALFORMED);
                                    }
                                    continue;
                            }
                    }

                    int  CheckForUnary = GetHierachy(TokenizedBuffer[LastNonEmptyTokenIndex]);
                    bool TokenindicatesContinuation = CheckForUnary != GetHierachy("++") && CheckForUnary != -1;


                    if ((UnresolvedTermsBuffer[0] == 0) && (Hierarchy == -1) && !TokenindicatesContinuation) break;

                    // if no more context can be supplied, return unterminated and log error to user
                    if (++Index == Source.Length) {
                        Terminal.Error(
                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"{Language.Connectives[(Program.ActiveLanguage, "Could not Fetch Required Context from")]} {Filename}",
                            StartingIndex, HasSteps ? StepMatrix.Tokens.Count : null, ApplyWiggle($"{AccumulatedContext} ", StringIndex, 1)        
                        );
                        return (null, ContextFetcherEnums.UNTERMINATED);
                    }

                    ResolvingTermsBuffer[0] = false;
                    nCapturedItemsBuffer[0] = 0;
                    TokenizedBuffer = [.. TokenizedBuffer.TakeLast(TokenizedBuffer.Length - TokenizedCheckPoint)];
                    TokenizedCheckPoint = HierarchyDeltaCheckpoint = 0;
                    StringIndex         = VerifiedStringIndex;  // Reset for more accurate wiggling

                    AccumulatedContext += Source[Index];
                    TokenizedBuffer     = [.. TokenizedBuffer, .. SolveDefines(Tokenize(Source[Index]))];

                } while (true);

                StepMatrix.Tokens.Add(new() {
                    DeltaTokens = new string[TokenizedBuffer.Length - HierarchyDeltaCheckpoint],
                    Hierarchy = Hierarchy + 1,
                    Terms = nCapturedItemsBuffer[1 + Hierarchy]
                });
                Array.Copy(TokenizedBuffer, HierarchyDeltaCheckpoint, StepMatrix.Tokens[^1].DeltaTokens, 0, TokenizedBuffer.Length - HierarchyDeltaCheckpoint);
                StepMatrix.MaxHierachy = MaxHierachy;
                StepMatrixes.Add(CloneChunk(StepMatrix));

                return (StepMatrixes, ContextFetcherEnums.OK);
            }

            internal static class Terminal {
                [Flags]
                internal enum AssemblyFlags : byte {
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

// in event of left in message, don't show on release
#if DEBUG
                internal static void Debug(string message) => Console.WriteLine(message);
#else
                internal static void debug() {}
#endif

#if DEBUG
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context,
                    int     lineNumber = 0, 
                    string  filePath = "", 
                    string  memberName = "")
#else
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) 
#endif
                {
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
#if DEBUG
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {LocationString}{Context}");
                    Console.WriteLine($"[{filePath}:{lineNumber}] {memberName}");
#else
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {LocationString}{Context}");
#endif

                Exit:
                    Console.ResetColor();
                }

#if DEBUG
                    
                internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context,
                    [CallerLineNumber] int lineNumber = 0,
                    [CallerFilePath] string filePath = "",
                    [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context, lineNumber, filePath, memberName);
                

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context,
                    [CallerLineNumber] int lineNumber = 0,
                    [CallerFilePath] string filePath = "",
                    [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context, lineNumber, filePath, memberName);


                internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context,
                    [CallerLineNumber] int lineNumber = 0,
                    [CallerFilePath] string filePath = "",
                    [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context, lineNumber, filePath, memberName);

#else
                internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }
#endif
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

            /// <summary>
            /// The include method.
            /// </summary>
            /// <param name="Filepath"></param>
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
                // Wide multi-character operators and atomic tokens
                string[] atomicTokens = new[] {
                    "$\"", "<=>", "==", "!=", "<=", ">=", "&&", "||", "++", "--",
                    "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<", ">>", "->", "??", "?."
                };

                // Escape them for regex and order by length
                string escapedTokens = string.Join("|", atomicTokens
                    .Select(Regex.Escape)
                    .OrderByDescending(s => s.Length));

                // Single-character separators including whitespace
                string separators = @"!""£$%\^&*()+\-=\[\]{};:'@#~\\|,<.>/?\s";

                // Final pattern:
                // 1. Match atomic tokens
                // 2. Match non-separator sequences
                // 3. Match single separators
                string pattern = $@"({escapedTokens})|([^{separators}]+)|([{separators}])";

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