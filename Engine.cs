using System.Runtime.CompilerServices;
using System.Runtime.Serialization;
using System.Text;
using System.Text.RegularExpressions;
using Numinous.Langauges;

namespace Numinous {
    namespace Engine {
        [Flags]
        internal enum WarningLevels : byte {
            IGNORE      = 0x00,
            DEFAULT     = 0x01,
            ERROR       = 0x02,
            VERBOSE     = 0x04,

            /* Internal     */
            NONE        = 0xff,
            NO_OVERRULE = 0x08,

            /* Composite    */
            STRICT      = VERBOSE | ERROR,
            CONTROLLED  = VERBOSE | ERROR | NO_OVERRULE,

        }

        internal enum Operators : byte {
            STRING,
            FSTRING,

            OPAREN,
            CPAREN,

            OBRACK,
            CBRACK,

            OBRACE,
            CBRACE,

            DESCOPE,

            PROPERTY,
            NULLPROPERTY,

            MULT,
            DIV,
            MOD,

            ADD,
            SUB,

            RIGHT,
            LEFT,

            BITMASK,

            BITFLIP,

            BITSET,

            GT,
            LT,
            GOET,
            LOET,
            SERIAL,

            EQUAL,
            INEQUAL,

            AND,
            
            OR,
            
            NULL,
            CHECK,
            ELSE,

            SET,
            INCREASE,
            DECREASE,
            MULTIPLY,
            DIVIDE,
            MODULATE,
            NULLSET,
            RIGHTSET,
            LEFTSET,
            
            ASSIGNMASK,
            ASSIGNSET,
            ASSIGNFLIP,

            TERM,

            NONE = 255
        }

        internal enum AssembleTimeTypes  : byte {
            PROPERTY,   // Property (Evaluator Solving)
            TYPE,       // typeof result
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
            EXP,        // Expression


            CONSTANTS,

            CINT,       // Constant int
            CSTRING,    // Constant string
            CSCOPE,     // Constant Scope reference
            CRT,        // Constant runtime reference
            CREG,       // Constant register reference
            CFLAG,      // Constant flag reference
            CPROC,      // Constant procedure reference
            CINTER,     // Constant interrupt reference
            CBANK,      // Constant bank reference
            CEXP,       // Constant Expression

            MACRO = 0x80,
                        // void macro
            MINT,       // int macro
            MSTRING,    // string macro
            MEXP,       // expression macro
        }

        internal enum AccessLevels : byte {
            PUBLIC = 0,
            PRIVATE = 1
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
            /*
             * Some notes:
             *      Tabs aren't equal width in each IDE, so we can't 'check' how many and generate the difference with spaces.
             *      Because of this we are going to have to store this infomration also.
             *      
             *      I propose we have a 'LastInformationIndex' which refers to where the last information what wasn't a space or tab was.
             *      (if useful)
             *      
             *      The goal of this method will be to convert the regex tokenized string responses and convert them into a system of tokens.
             *      the tokens are in object obfuscated form, but naturally should look something like Value Operator Value
             *      
             *      By storing information like
             *      (
             *          this + 
             *              (
             *                  that
             *              )
             *      )
             *      
             *      we can easily resolve the highest hierachies and inject the result in between the two outside it.
             *      by repeating this process until we have resolved the lowest hierachy we should be able to resolve any expression.
             *      
             *      Resolving isn't what the CF does, but orders it so it can be done.
             *      
             *      The CF will also need to encode whitespace in, which will seriously violate VOV.
             *      The Evaluator will need to check to exempt VOV from evaluation logic, but will need to be used in error report code.
             *      
             *      The rule has to be WVWO (repeating) where W is whitespace.
             *      
             *      Whitespace will have to be an CONSTEXPR that begins with a whitespace token.
             *      
             *      PER STEP
             *          TOKENS
             *              DELTA_TOKENS
             *                  STRING_OFFSET
             *                  DATA
             *                      ?: OPERTOR
             *                      ?: (ITEM, CEXP)
             *                  IS_OPERATOR
             *              HIERACHY
             *              TERMS
             *          MAX_HIERACHY
             *          SUCCESS
             *          
             *          
             *          TODO:
             *              CODE:
             *                  ERROR REPORT
             *                  CLEANSE
             *              
             *              TEST:
             *                  EXTREME CASE
             *                  EMPTY CASE
             *                      
             *                  
             *          
             */

            internal static (List<(List<(List<(int StringOffset, int StringLength, object data, bool IsOperator)> DeltaTokens, int Hierachy, int Terms)> Tokens, int MaxHierachy)>, bool Success) ContextFetcher(string[] SourceFileReference, ref int SourceLineReference) {
                List<string> RegexTokens = RegexTokenize(SourceFileReference[SourceLineReference]);

                List<(List<(List<(int StringOffset, int StringLength, object data, bool IsOperator)> DeltaTokens, int Hierachy, int Terms)> Tokens, int MaxHierachy)> Tokens = [];
                List<(List<(int StringOffset, int StringLength, object data, bool IsOperator)> DeltaTokens, int Hierachy, int Terms)> StepTokens = [];
                List<(int StringOffset, int StringLength, object data, bool IsOperator)> DeltaTokens = [];

                List<Operators> ContainerBuffer = [];
                List<int>       nTermBuffer     = [0];

                int MaxHierachy = 0;
                int LastNonWhiteSpaceIndex = -1;

                string WHITESPACE_CONSTEXP = "";
                string LITERAL_CSTRING =     "";

                do {
                    for (int i = 0, StringIndex = 0; i < RegexTokens.Count; StringIndex += RegexTokens[i].Length, i++) {
                        if (ContainerBuffer.Count != 0 && ContainerBuffer[^1] == Operators.FSTRING) {
                            CaptureCSTRING(ref i, ref StringIndex, c => c == '"' || c == '{');
                            if (RegexTokens[i][0] != '{') {
                                if (RegexTokens[i][0] == '"') {
                                    if (CloseContainer(StringIndex, Operators.STRING, Operators.FSTRING)) continue;
                                    else return default;
                                }
                            }
                        }


                        // capture whitespace
                        int CapturedStringIndex = StringIndex;
                        for (WHITESPACE_CONSTEXP = ""; RegexTokens[i][0] == ' ' || RegexTokens[i][0] == '\t'; i++, StringIndex++) WHITESPACE_CONSTEXP += RegexTokens[i];
                        if (CapturedStringIndex != StringIndex) {
                            DeltaTokens.Add((
                                CapturedStringIndex,
                                WHITESPACE_CONSTEXP.Length,
                                // everything is private here because it should never exist
                                new Dictionary<string, (object, AssembleTimeTypes, AccessLevels)> {
                                    {"self",    (WHITESPACE_CONSTEXP,           AssembleTimeTypes.CEXP, AccessLevels.PRIVATE) },
                                    {"length",  (WHITESPACE_CONSTEXP.Length,    AssembleTimeTypes.CINT, AccessLevels.PRIVATE) },
                                    {"type",    (AssembleTimeTypes.CEXP,        AssembleTimeTypes.TYPE, AccessLevels.PRIVATE)}
                                },
                                false
                            ));
                        }

                        // handle tokens
                        switch (RegexTokens[i]) {
                            case "+":   SimpleAddOperator(Operators.ADD,          StringIndex); break;
                            case "-":   SimpleAddOperator(Operators.SUB,          StringIndex); break;
                            case "*":   SimpleAddOperator(Operators.MULT,         StringIndex); break;
                            case "/":   SimpleAddOperator(Operators.DIV,          StringIndex); break;
                            case "%":   SimpleAddOperator(Operators.MOD,          StringIndex); break;
                            case ">>":  SimpleAddOperator(Operators.RIGHT,        StringIndex); break;
                            case "<<":  SimpleAddOperator(Operators.LEFT,         StringIndex); break;
                            case "&":   SimpleAddOperator(Operators.BITMASK,      StringIndex); break;
                            case "^":   SimpleAddOperator(Operators.BITFLIP,      StringIndex); break;
                            case "|":   SimpleAddOperator(Operators.BITSET,       StringIndex); break;
                            case "==":  SimpleAddOperator(Operators.EQUAL,        StringIndex); break;
                            case "!=":  SimpleAddOperator(Operators.INEQUAL,      StringIndex); break;
                            case ">=":  SimpleAddOperator(Operators.GOET,         StringIndex); break;
                            case "<=":  SimpleAddOperator(Operators.LOET,         StringIndex); break;
                            case ">":   SimpleAddOperator(Operators.GT,           StringIndex); break;
                            case "<":   SimpleAddOperator(Operators.LT,           StringIndex); break;
                            case "<=>": SimpleAddOperator(Operators.SERIAL,       StringIndex); break;
                            case "=":   SimpleAddOperator(Operators.SET,          StringIndex); break;
                            case "+=":  SimpleAddOperator(Operators.INCREASE,     StringIndex); break;
                            case "-=":  SimpleAddOperator(Operators.DECREASE,     StringIndex); break;
                            case "*=":  SimpleAddOperator(Operators.MULTIPLY,     StringIndex); break;
                            case "/=":  SimpleAddOperator(Operators.DIVIDE,       StringIndex); break;
                            case "%=":  SimpleAddOperator(Operators.MODULATE,     StringIndex); break;
                            case ">>=": SimpleAddOperator(Operators.RIGHTSET,     StringIndex); break;
                            case "<<=": SimpleAddOperator(Operators.LEFTSET,      StringIndex); break;
                            case "&=":  SimpleAddOperator(Operators.ASSIGNMASK,   StringIndex); break;
                            case "|=":  SimpleAddOperator(Operators.ASSIGNSET,    StringIndex); break;
                            case "^=":  SimpleAddOperator(Operators.ASSIGNFLIP,   StringIndex); break;
                            case "??=": SimpleAddOperator(Operators.NULLSET,      StringIndex); break;
                            case "??":  SimpleAddOperator(Operators.NULL,         StringIndex); break;
                            case ".":   SimpleAddOperator(Operators.PROPERTY,     StringIndex); break;
                            case "?.":  SimpleAddOperator(Operators.NULLPROPERTY, StringIndex); break;
                            case "?":   SimpleAddOperator(Operators.CHECK,        StringIndex); break;
                            case ":":   SimpleAddOperator(Operators.ELSE,         StringIndex); break;

                            // special case
                            case "\"":
                                i++;
                                if (CaptureCSTRING(ref i, ref StringIndex, c => c == '"')) break;
                                return default;

                            // Container Code
                            case "(":   OpenContainer(StringIndex, Operators.OPAREN);   break;
                            case "[":   OpenContainer(StringIndex, Operators.OBRACK);   break;
                            case "{":   OpenContainer(StringIndex, Operators.OBRACE);   break;
                            case "$\"": OpenContainer(StringIndex, Operators.FSTRING);  break;

                            case ")": if (SimpleCloseContainer(StringIndex, Operators.CPAREN)) break; else return default;
                            case "]": if (SimpleCloseContainer(StringIndex, Operators.CBRACK)) break; else return default;
                            case "}": if (SimpleCloseContainer(StringIndex, Operators.CBRACE)) break; else return default;

                            case ";":
                                if (ContainerBuffer.Count > 0) {
                                    // error : terminated mid-line
                                    return default;
                                }

                                if (i == RegexTokens.Count) {
                                    if (Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                                        // log/ error : do not end with a semicolon
                                        if (Program.WarningLevel.HasFlag(WarningLevels.ERROR)) return default;
                                    } else continue; // otherwise exit the loop entirely. We forgive our users for using semicolons despite having a great CF.
                                }
  

                                CopyDeltaTokens();
                                CopyStepTokens();
                                PrepareNextStep();

                                // modify regex tokens to remove used and stored
                                RegexTokens = [.. RegexTokens.TakeLast(RegexTokens.Count - i - 1)]; // trim last step from pattern
                                i = 0;                                                              // reset counter
                                break;

                            // Term Catching
                            case ",":
                                nTermBuffer[^1]++;
                                SimpleAddOperator(Operators.TERM, StringIndex);
                                break;

                            default:
                                DeltaTokens.Add((
                                    StringIndex,
                                    RegexTokens[i].Length,
                                    new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels)> {
                                        // private for now, but once its determined to be something then it will change accordingly
                                        // values also need a 'repr_length' to describe how long for errors in text the value is.
                                        { "self", (RegexTokens[i], AssembleTimeTypes.CEXP, AccessLevels.PRIVATE) },
                                    },
                                    false
                                ));
                                LastNonWhiteSpaceIndex = DeltaTokens.Count - 1;
                                break;

                            }
                    }

                    bool IsLastOperator = LastNonWhiteSpaceIndex != -1 && DeltaTokens.Count != 0 && DeltaTokens[LastNonWhiteSpaceIndex].IsOperator;

                    if (ContainerBuffer.Count == 0 && !IsLastOperator) break;

                    if (++SourceLineReference == SourceFileReference.Length) {
                        // error, cant take more context
                        return default;
                    }

                    // fetch more context, restart last StepToken. 

                    PrepareNextStep();
                    RegexTokens = [.. RegexTokens, .. RegexTokenize(SourceFileReference[SourceLineReference])];

                } while (true);

                CopyDeltaTokens();                                                                  // process final DeltaTokens (valid) to StepTokens end
                CopyStepTokens();                                                                   // add last captured StepToken to Tokens

                return (Tokens, true);

                void PrepareNextStep() {
                    MaxHierachy = 0;
                    StepTokens.Clear();
                    DeltaTokens.Clear();
                    nTermBuffer = [0];
                    ContainerBuffer = [];
                }

                bool CaptureCSTRING(ref int i, ref int si, Func<char, bool> HaltCapturePredicate) {
                    int csi = si;

                    for (; i < RegexTokens.Count && !HaltCapturePredicate(RegexTokens[i][0]); i++) {
                        LITERAL_CSTRING += RegexTokens[i];
                        si++;
                    }

                    if (i == RegexTokens.Count) {
                        // error unterminated string
                        return false;
                    }

                    if (csi != si) {
                        DeltaTokens.Add((
                            csi,
                            csi - si,
                            new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
                                {"self",    (LITERAL_CSTRING,           AssembleTimeTypes.CSTRING,  AccessLevels.PRIVATE) },
                                {"length",  (LITERAL_CSTRING.Length,    AssembleTimeTypes.CINT,     AccessLevels.PUBLIC) },
                            },
                            false
                        ));

                        LITERAL_CSTRING = "";   // wipe string for next capture
                    }


                    return true;
                }


                #region Context Fetcher Functions
                void AddOperator(Operators Operator, int si, int sl) {
                    DeltaTokens.Add((si, sl, Operator, true)); LastNonWhiteSpaceIndex = DeltaTokens.Count - 1; ;
                }

                void SimpleAddOperator(Operators Operator, int si) => AddOperator(Operator, si, 1);

                void ComplexOpenContainer(int si, int sl, Operators Operator) {
                    CopyDeltaTokens();
                    ContainerBuffer.Add(Operator);                                  // register container type
                    nTermBuffer.Add(0);

                    AddOperator(Operator, si, sl);
                    MaxHierachy = Math.Max(ContainerBuffer.Count, MaxHierachy);
                }

                void OpenContainer(int si, Operators Operator) => ComplexOpenContainer(si, 1, Operator);

                bool CloseContainer(int si, Operators CloseOperator, Operators OpenOperator) {
                    if (ContainerBuffer.Count == 0 || ContainerBuffer[^1] != OpenOperator) {
                        // error, bracket was not opened last before this
                        return false;
                    }

                    SimpleAddOperator(CloseOperator, si);
                    CopyDeltaTokens();
                    ContainerBuffer.RemoveAt(ContainerBuffer.Count - 1);
                    nTermBuffer.RemoveAt(nTermBuffer.Count - 1);

                    return true;
                }

                bool SimpleCloseContainer(int si, Operators Operator) => CloseContainer(si, Operator, Operator - 1);

                void CopyStepTokens() {
                    var StepTokenShallowCopy = StepTokens
                        .Select(t => (
                            t.DeltaTokens,                  // reference to a clone, should be fine
                            t.Hierachy,
                            t.Terms
                        )).ToList();

                    Tokens.Add((StepTokenShallowCopy, MaxHierachy));
                }

                void CopyDeltaTokens() {
                    // Clone Delta Tokens thus far
                    var DeltaTokenShallowCopy = DeltaTokens
                    .Select(t => (
                        t.StringOffset,
                        t.StringLength,
                        t.IsOperator
                            ? t.data
                            : Clone((Dictionary<string, (object, AssembleTimeTypes, AccessLevels)>)t.data),
                        t.IsOperator
                    )).ToList();

                    DeltaTokens = [];                                                               // wipe delta tokens for next operation
                    StepTokens.Add((DeltaTokenShallowCopy, ContainerBuffer.Count, nTermBuffer[^1]));// append copy to StepTokens
                }
                #endregion Context Fetcher Functions
            }


            // Generated Function | However I do find that this function is how I would code and meets criteria
            internal static Dictionary<TKey, TValue> Clone<TKey, TValue>(Dictionary<TKey, TValue> Source) where TKey : notnull {
                var clone = new Dictionary<TKey, TValue>(Source.Count);
                foreach (var kv in Source) {
                    var keyClone = Clone(kv.Key);
                    var valueClone = Clone(kv.Value);
                    clone[keyClone] = valueClone;
                }
                return clone;
            }

            internal static T Clone<T>(T ctx) => ctx switch {
                ICloneable c => (T)c.Clone(),
                string or ValueType => ctx,
#if DEBUG
                _ => throw new NotSupportedException($"Cannot clone type {ctx?.GetType()}")
#else
                _ => throw new NotSupportedException($"FATAL ERROR :: (REPORT THIS ON THE GITHUB) CANNOT CLONE TYPE {ctx?.GetType()}")
#endif
            };



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

            ///// <summary>
            ///// This will either push back an object reference, or a constant object literal.
            ///// This will ignore all forms of containers - expects no type, just detects and works with it.
            ///// If the Resolve is null, it means an error occured.
            ///// Container may be either
            /////     (   - push back object ref tuple where possible, returning constant values where not
            /////     {   - push back cexp of tuple
            /////     [   - push back cint with creg
            ///// </summary>
            ///// <param name="Tokens"></param>
            ///// <returns></returns>
            //static internal (string? Representation, AssembleTimeTypes ResolveType, char Container) LinearEvaluate(List<string> Tokens) {
            //    int PendingOperations               = Tokens.Count(t => GetHierachy(t) != -1);
            //    bool ExpectValueToggle              = true;
            //    bool Mutated                        = false;    // does not affect object reference perpetuation.
            //    List<Unary> UnaryBuffer             = [];

            //    List<(object? data, AssembleTimeTypes type)> DataBuffer             = [];
            //    List<AssembleTimeTypes> TypeBuffer  = [];

            //    Dictionary<string, (object data, AssembleTimeTypes type)> ActiveScope = Program.ActiveScope;
            //    List<(Operators Operator, int Level)> OperatorBuffer = [];

            //    [MethodImpl(MethodImplOptions.AggressiveInlining)]
            //    (object ObjectReturn, bool OK) ComputeOperation(Operators Op, object L, object R, AssembleTimeTypes LT, AssembleTimeTypes RT) {
            //        switch (Op) {
            //            case Operators.PROPERTY:
            //                switch (LT) {

            //                }
            //                break;
            //        }
            //        return default;
            //    }


            //    [MethodImpl(MethodImplOptions.AggressiveInlining)]
            //    void UnifiedConstAppendSystem(int Int) {
            //        for (int u = UnaryBuffer.Count - 1; u >= 0; u--) {
            //            switch (UnaryBuffer[u]) {
            //                case Unary.ABS:
            //                    Int = Math.Abs(Int);
            //                    break;

            //                case Unary.NEG:
            //                    Int = -Int;
            //                    break;

            //                case Unary.BIT:
            //                    Int = ~Int;
            //                    break;

            //                case Unary.NOT:
            //                    Int = 0 == Int ? 0 : 1;
            //                    break;
            //            }

            //            DataBuffer.Add((new Dictionary<string, (object data, AssembleTimeTypes type)> {
            //                {"self", (Int, AssembleTimeTypes.CINT) }
            //            }, AssembleTimeTypes.CINT));
            //        }
            //    }

            //    for (int i = 1; i < Tokens.Count - 1; i++) {
            //        if (Tokens[i][0] == ' ' || Tokens[i][0] == '\t') continue;
            //        if (ExpectValueToggle) {
            //            switch (Tokens[i]) {
            //                case "++": if (Mutated) goto Error; UnaryBuffer.Add(Unary.INC); break;
            //                case "--": if (Mutated) goto Error; UnaryBuffer.Add(Unary.DEC); break;
            //                case "+":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.ABS); break;
            //                case "-":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.NEG); break;
            //                case "~":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.BIT); break;
            //                case "!":  if (Mutated) goto Error; UnaryBuffer.Add(Unary.NOT); break;

            //                case "\\":
            //                    if (ActiveScope == Program.LabelDataBase["rs"]) {
            //                        // warn, your accessing the parent of root - which is root, relative scope systems may fail.
            //                    }
            //                    ActiveScope = (Dictionary<string, (object data, AssembleTimeTypes type)>)ActiveScope["parent"].data;
            //                    Mutated = true;
            //                    break;

            //                default:
            //                    // mark to want operator
            //                    ExpectValueToggle = false;

            //                    if (GetHierachy(Tokens[i]) != -1) {
            //                        // error, attempted to mutate a non unary operator (just as bad, but needs a different check)
            //                        return default;
            //                    }

            //                    switch (Tokens[i][0]) {
            //                        case '\"':
            //                            i += 2;
            //                            string StringCapture = $"\"{Tokens[i - 1]}";
            //                            for (; i < Tokens.Count - 1 && Tokens[i - 1] != "\""; i++) {
            //                                StringCapture += Tokens[i];
            //                            }
            //                            if (StringCapture[^1] != '\"') {
            //                                // error string literal did not terminate
            //                                return default;
            //                            }

            //                            object UnaryCapture = StringCapture;
            //                            AssembleTimeTypes ResultType = AssembleTimeTypes.CSTRING;

            //                            if (Mutated) {
            //                                // error, cant mutate a literal
            //                                return default;
            //                            }

            //                            for (int u = UnaryBuffer.Count - 1; u >= 0; u--) {
            //                                switch (UnaryBuffer[u]) {
            //                                    case Unary.ABS:
            //                                        if (ResultType == AssembleTimeTypes.CINT) {
            //                                            UnaryCapture = Math.Abs((int)UnaryCapture);
            //                                        } else {
            //                                            UnaryCapture = ((string)UnaryCapture).ToUpper();
            //                                        }
            //                                        break;

            //                                    case Unary.NEG:
            //                                        if (ResultType == AssembleTimeTypes.CINT) {
            //                                            UnaryCapture = -(int)UnaryCapture;
            //                                        } else {
            //                                            UnaryCapture = ((string)UnaryCapture).ToLower();
            //                                        }

            //                                        break;

            //                                    case Unary.BIT:
            //                                        if (ResultType == AssembleTimeTypes.CINT) {
            //                                            UnaryCapture = ~(int)UnaryCapture;
            //                                        } else {
            //                                            UnaryCapture = ((string)UnaryCapture).Length;
            //                                            ResultType = AssembleTimeTypes.CINT;
            //                                        }

            //                                        break;

            //                                    case Unary.NOT:
            //                                        if (ResultType == AssembleTimeTypes.CINT) {
            //                                            UnaryCapture = 0 == (int)UnaryCapture ? 0 : 1;
            //                                        } else {
            //                                            UnaryCapture = new string(' ', ((string)UnaryCapture).Length);
            //                                        }

            //                                        break;
            //                                }
            //                            }

            //                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                DataBuffer.Add((new Dictionary<string, (object data, AssembleTimeTypes type)> {
            //                                    { "self", (UnaryCapture,            AssembleTimeTypes.CINT) },
            //                                }, AssembleTimeTypes.CINT));
            //                            } else {
            //                                DataBuffer.Add((new Dictionary<string, (object data, AssembleTimeTypes type)> {
            //                                    { "self",   (UnaryCapture,          AssembleTimeTypes.CSTRING) },
            //                                    { "length", (StringCapture.Length,  AssembleTimeTypes.CINT) },
            //                                }, AssembleTimeTypes.CSTRING));
            //                            }

            //                            // no need to bounds check this
            //                            if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                // error, post mut. Same issue as pre-mut on literal
            //                                return default;
            //                            }

            //                            break;

            //                        case '0':

            //                            if (Mutated) {
            //                                // error, cant mutate a literal
            //                                return default;
            //                            }

            //                            switch (Tokens[i][1]) {
            //                                case 'x':
            //                                    // hexadecimal int literal
            //                                    UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 16));
            //                                    if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                        // error, post mut. Same issue as pre-mut on literal
            //                                        return default;
            //                                    }
            //                                    break;

            //                                case 'b':
            //                                    // binary int literal
            //                                    UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 2));
            //                                    if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                        // error, post mut. Same issue as pre-mut on literal
            //                                        return default;
            //                                    }
            //                                    break;

            //                                case 'd':
            //                                    // octal int literal
            //                                    if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                        // error, post mut. Same issue as pre-mut on literal
            //                                        return default;
            //                                    }
            //                                    UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 8));
            //                                    break;

            //                                default:
            //                                    break;
            //                            }
            //                            goto case '9';

            //                        case '1':
            //                        case '2':
            //                        case '3':
            //                        case '4':
            //                        case '5':
            //                        case '6':
            //                        case '7':
            //                        case '8':
            //                        case '9':

            //                            if (Mutated) {
            //                                // error, cant mutate a literal
            //                                return default;
            //                            }

            //                            // decimal int literal
            //                            UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i].ToLowerInvariant(), 8));
            //                            if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                // error, post mut. Same issue as pre-mut on literal
            //                                return default;
            //                            }
            //                            break;

            //                        case '$':

            //                            if (Mutated) {
            //                                // error, cant mutate a literal
            //                                return default;
            //                            }

            //                            // hexadecimal int literal
            //                            UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 16));
            //                            if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                // error, post mut. Same issue as pre-mut on literal
            //                                return default;
            //                            }
            //                            break;

            //                        case '%':

            //                            if (Mutated) {
            //                                // error, cant mutate a literal
            //                                return default;
            //                            }

            //                            // binary int literal
            //                            UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 2));
            //                            if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                // error, post mut. Same issue as pre-mut on literal
            //                                return default;
            //                            }
            //                            break;

            //                        case '£':

            //                            if (Mutated) {
            //                                // error, cant mutate a literal
            //                                return default;
            //                            }

            //                            // octal int literal
            //                            UnifiedConstAppendSystem(Convert.ToInt32(Tokens[i][2..].ToLowerInvariant(), 8));
            //                            if (Tokens[i + 1] == "++" || Tokens[i + 1] == "--") {
            //                                // error, post mut. Same issue as pre-mut on literal
            //                                return default;
            //                            }
            //                            break;

            //                        default:
            //                            // non literal
            //                            if (ActiveScope.TryGetValue(Tokens[i], out (object data, AssembleTimeTypes type) value)) {
            //                                UnaryCapture = value.data;
            //                                ResultType   = value.type;

            //                                // ensure unaries are used on right types, mutating to const type, as long as not null
            //                                if (UnaryBuffer.Count != 0) {
            //                                    if (UnaryCapture == null) {
            //                                        // error, cant access to modify value of null
            //                                        return default;
            //                                    }

            //                                    if      (ResultType == AssembleTimeTypes.INT)    ResultType = AssembleTimeTypes.CINT;
            //                                    else if (ResultType == AssembleTimeTypes.STRING) ResultType = AssembleTimeTypes.CSTRING;
            //                                    else {
            //                                        // return error, its inappropriate to use unaries on anything but int or string
            //                                        return default;
            //                                    }
            //                                } else if (UnaryCapture == null) {
            //                                    // when capturing null, it HAS to be an object reference. This could be assignment.
            //                                    // but if its not thats fine, but we still need to propogate the ref.
            //                                    // so far (MyNullObject) = 1 wont define as non-null, because we cant send back object ref
            //                                    DataBuffer.Add((null, ResultType));
            //                                    break;
            //                                }

            //                                for (int u = UnaryBuffer.Count - 1; u >= 0; u--) {
            //                                    switch (UnaryBuffer[u]) {
            //                                        case Unary.INC:
            //                                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                                UnaryCapture = 1 + (int)UnaryCapture!;
            //                                                ActiveScope[Tokens[i]] = (UnaryCapture, ResultType);
            //                                            } else {
            //                                                // error, cant do this to anything but an int
            //                                                return default;
            //                                            }
            //                                            break;

            //                                        case Unary.DEC:
            //                                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                                UnaryCapture = -1 + (int)UnaryCapture!;
            //                                                ActiveScope[Tokens[i]] = (UnaryCapture, ResultType);
            //                                            } else {
            //                                                // error, cant do this to anything but an int
            //                                                return default;
            //                                            }
            //                                            break;

            //                                        case Unary.ABS:
            //                                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                                UnaryCapture = Math.Abs((int)UnaryCapture!);
            //                                            } else if (ResultType == AssembleTimeTypes.CSTRING) {
            //                                                UnaryCapture = ((string)UnaryCapture!).ToUpper();
            //                                            }
            //                                            break;

            //                                        case Unary.NEG:
            //                                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                                UnaryCapture = -(int)UnaryCapture!;
            //                                            } else if (ResultType == AssembleTimeTypes.CSTRING) {
            //                                                UnaryCapture = ((string)UnaryCapture!).ToLower();
            //                                            }
            //                                            break;

            //                                        case Unary.BIT:
            //                                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                                UnaryCapture = ~(int)UnaryCapture!;
            //                                            } else if (ResultType == AssembleTimeTypes.CSTRING) {
            //                                                UnaryCapture = ((string)UnaryCapture!).Length;
            //                                            }
            //                                            break;

            //                                        case Unary.NOT:
            //                                            if (ResultType == AssembleTimeTypes.CINT) {
            //                                                UnaryCapture = 0 == (int)UnaryCapture! ? 0 : 1;
            //                                            } else if (ResultType == AssembleTimeTypes.CSTRING) {
            //                                                UnaryCapture = new string(' ', ((string)UnaryCapture!).Length);
            //                                            }
            //                                            break;
            //                                    }
            //                                }

            //                                // property capture

            //                                for(; i < Tokens.Count - 1; i++) {
            //                                    if (Tokens[i][0] == ' ' || Tokens[i][0] == '\t') continue;
            //                                    // we can optimise the underneath later?
            //                                    if (Tokens[i][0] == '.' || Tokens[i] == "?.") {
            //                                        Mutated = false;                            // free up for mutation, might be pointless

            //                                        if (Tokens[i].Length == 1) {
            //                                            if (UnaryCapture == null) {
            //                                                // sorry link, I can't sell credit. come back when your a little mmmmmmmmm richer
            //                                                // error : null cannot have members
            //                                                return default;
            //                                            }

            //                                            // set i to equal the property (could be at a?. b)
            //                                            for (; i < Tokens.Count - 1; i++) {
            //                                                if (Tokens[i][0] == ' ' || Tokens[i][0] == '\t') continue;
            //                                                if (GetHierachy(Tokens[i]) == -1) break;
            //                                            }
            //                                            if (((Dictionary<string, (object data, AssembleTimeTypes type)>)UnaryCapture).TryGetValue($"{Tokens[i]}", out (object data, AssembleTimeTypes type) PropertyAccess)) {
            //                                                UnaryCapture = PropertyAccess.data;
            //                                                ResultType = PropertyAccess.type;
            //                                            } else {
            //                                                // error, no property found
            //                                                return default;
            //                                            }
            //                                        } else {
            //                                            // ?.
            //                                            if (UnaryCapture == null) continue;         // null has no object, so its just const typed null from here
            //                                            // ah shit here we go again (fetch data)
                                                        
            //                                            // set i to equal the property (could be at a. b)
            //                                            for (; i < Tokens.Count - 1; i++) {
            //                                                if (Tokens[i][0] == ' ' || Tokens[i][0] == '\t') continue;
            //                                                if (GetHierachy(Tokens[i]) == -1) break;
            //                                            }
            //                                            if (((Dictionary<string, (object data, AssembleTimeTypes type)>)UnaryCapture).TryGetValue($"{Tokens[i]}", out (object data, AssembleTimeTypes type) PropertyAccess)){
            //                                                UnaryCapture = PropertyAccess.data;
            //                                                ResultType   = PropertyAccess.type;
            //                                            } else {
            //                                                // error, no property found
            //                                                return default;
            //                                            }
            //                                        }
            //                                    }

            //                                    if (Tokens[i] == "++" || Tokens[i] == "--") {
            //                                        if (Mutated) {
            //                                            // error, post mut after pre mut is illegal
            //                                            return default;
            //                                        }

            //                                        if (ResultType != AssembleTimeTypes.INT) {
            //                                            // error attempting to mut const goto Error? or is not int so illegal operation
            //                                            return default;
            //                                        }

            //                                        // mutate property
            //                                        if (Tokens[i][0] == '+') {
            //                                            ((Dictionary<string, (object data, AssembleTimeTypes type)>)UnaryCapture!)["self"] = (
            //                                                1 + (int)(((Dictionary<string, (object data, AssembleTimeTypes type)>)UnaryCapture)["self"].data),
            //                                                ((Dictionary<string, (object? data, AssembleTimeTypes type)>)UnaryCapture)["self"].type
            //                                            );
            //                                            UnaryCapture = 1 + (int)UnaryCapture;
            //                                        } else {
            //                                            ((Dictionary<string, (object data, AssembleTimeTypes type)>)UnaryCapture!)["self"] = (
            //                                                -1 + (int)(((Dictionary<string, (object data, AssembleTimeTypes type)>)UnaryCapture)["self"].data),
            //                                                ((Dictionary<string, (object? data, AssembleTimeTypes type)>)UnaryCapture)["self"].type
            //                                            );
            //                                            UnaryCapture = -1 + (int)UnaryCapture;
            //                                        }

            //                                        continue;
            //                                    }
            //                                }

            //                                DataBuffer.Add((UnaryCapture, ResultType));

            //                                // object post mod
            //                                break;
            //                            } else {
            //                                // error, invalid token
            //                                return default;
            //                            }
            //                    }
            //                    break;

            //                Error: return default;
            //            }
            //            // everything must reach here, this is to complete operations. 
            //            /*
            //             * if we have at least two operators, calculate the delta of their heirarchies. if its negative, compute the higher
            //             * adjust the buffers as needed. At the end OperatorBuffer should always have 1 operator, leaving 2 in data.
            //             * once we capture the last value, we'll always move to check final operator looking for a final ++ or --
            //             * 
            //             * if thats found then that's fine, but there won't be another operator if the user is of sound mind.
            //             * which should prompt a clean loop termination, leaving the final task to be resolved and compared against target type
            //             * 
            //             * error checking should be easy
            //             */

            //            if (OperatorBuffer.Count <= 1) continue;    // not enough captured to safely perform an operation

            //            // if O[n-1] < O[n], then we can perform O on T[n-1] and T[n]
            //            while (OperatorBuffer[^2].Level < OperatorBuffer[^1].Level) {
            //                byte LT = (byte)DataBuffer[^2].type;
            //                byte RT = (byte)DataBuffer[^1].type;

            //                // create reckless compatibility between const/nonconst types
            //                if (LT > (byte)AssembleTimeTypes.CONSTANTS) LT -= (byte)AssembleTimeTypes.CONSTANTS;
            //                if (RT > (byte)AssembleTimeTypes.CONSTANTS) RT -= (byte)AssembleTimeTypes.CONSTANTS;

            //                if (LT != RT) {
            //                    // error type noncompat
            //                    return default;
            //                }

                            
            //            }




            //        } else {
            //            ActiveScope = Program.ActiveScope;
            //            UnaryBuffer.Clear();

            //            if (Tokens[i] == "++") {
            //                if (Mutated) {
            //                    // error, cant mutate twice
            //                    return default;
            //                }

            //                if (AssembleTimeTypes.INT == DataBuffer[^1].type) {
            //                    DataBuffer[^1] = (1 + (int)DataBuffer[^1].data, AssembleTimeTypes.INT);
            //                    Mutated = true;
            //                }
            //            }


            //            if (Tokens[i] == "--") {
            //                if (Mutated) {
            //                    // error, cant mutate twice
            //                    return default;
            //                }

            //                if (AssembleTimeTypes.INT == DataBuffer[^1].type) {
            //                    DataBuffer[^1] = (-1 + (int)DataBuffer[^1].data, AssembleTimeTypes.INT);
            //                    Mutated = true;
            //                }
            //            }

            //            int Level = GetHierachy(Tokens[i]);
            //            Operators Operator = Tokens[i] switch {
            //                // Multiplicative
            //                "*"     => Operators.MULT,
            //                "/"     => Operators.DIV,
            //                "%"     => Operators.MOD,

            //                // Additive
            //                "+"     => Operators.ADD,
            //                "-"     => Operators.SUB,

            //                // Shift
            //                ">>"    => Operators.RIGHT,
            //                "<<"    => Operators.LEFT,

            //                // Boolean And
            //                "&"     => Operators.BITMASK,

            //                // Boolean Xor
            //                "^"     => Operators.BITFLIP,

            //                // Boolean Or
            //                "|"     => Operators.BITSET,

            //                // Relational
            //                ">"     => Operators.GT,
            //                "<"     => Operators.LT,
            //                ">="    => Operators.GOET,
            //                "<="    => Operators.LOET,
            //                "<=>"   => Operators.SERIAL,

            //                // Equality
            //                "=="    => Operators.EQUAL,
            //                "!="    => Operators.INEQUAL,

            //                // Conditional And
            //                "&&"    => Operators.AND,

            //                // Conditional Or
            //                "||"    => Operators.OR,

            //                // Null coalesce
            //                "??"    => Operators.NULL,

            //                // Ternary
            //                "?"     => Operators.CHECK,
            //                ":"     => Operators.ELSE,

            //                // Assignment
            //                "="     => Operators.SET,
            //                "+="    => Operators.INCREASE,
            //                "-="    => Operators.DECREASE,
            //                "*="    => Operators.MULTIPLY,
            //                "/="    => Operators.DIVIDE,
            //                "%="    => Operators.MODULATE,
            //                "|="    => Operators.ASSIGNSET,
            //                "&="    => Operators.ASSIGNMASK,
            //                "^="    => Operators.ASSIGNFLIP,
            //                "??="   => Operators.NULLSET,
            //                ">>="   => Operators.RIGHT,
            //                "<<="   => Operators.LEFT,

            //                // Term
            //                ","     => Operators.TERM,

            //                // Default fallback
            //                _ => throw new ArgumentOutOfRangeException(nameof(Tokens), $"Unsupported operator: {Tokens[i]}")

            //            };

            //            OperatorBuffer.Add((Operator, Level));
            //            Mutated = false;                        // unlock mutation for next term
            //            ExpectValueToggle = true;               // engage wait for next value
            //        }
            //    }

                
            //    AssembleTimeTypes FinalType = AssembleTimeTypes.PROPERTY;
            //    string result = "";

            //    return Tokens[^1][0] switch {
            //        ')'  => ($"{result}",       FinalType,                  Tokens[^1][0]),
            //        ']'  => ($"[{result}]",     AssembleTimeTypes.CINT,     Tokens[^1][0]),
            //        '}'  => ($"{result}",       AssembleTimeTypes.CSTRING,  Tokens[^1][0]),// feeds into a fstring, will result a string component
            //        '\"' => ($"\"{result}\"",   AssembleTimeTypes.CSTRING,  Tokens[^1][0]),// returns cstring with static members
            //        _    => default,
            //    };
            //}

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
                            UpdatedTokens.AddRange(RegexTokenize(Capture));
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

                string[] TokenizedBuffer            = [.. SolveDefines(RegexTokenize(Source[Index]))];
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

                    StepMatrix.Tokens.Clear();                  // corrects token length math
                    ResolvingTermsBuffer[0] = false;
                    nCapturedItemsBuffer[0] = 0;
                    TokenizedBuffer = [.. TokenizedBuffer.TakeLast(TokenizedBuffer.Length - TokenizedCheckPoint)];
                    TokenizedCheckPoint = HierarchyDeltaCheckpoint = 0;
                    StringIndex         = VerifiedStringIndex;  // Reset for more accurate wiggling

                    AccumulatedContext += Source[Index];
                    TokenizedBuffer     = [.. TokenizedBuffer, .. SolveDefines(RegexTokenize(Source[Index]))];

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
                internal enum Responses : byte {
                    Terminate_Error,
                    Terminate_Success,
                    Proceed,
                }
                
                internal static (string InputPath, string OutputPath, Responses Response) Parse(string[] args) {
                    string InputPath = "", OutputPath = "";
                    int StringIndex = 0;
                    string Flattened = string.Join(" ", args);

                    Responses Response = Responses.Proceed;
                    Program.WarningLevel = WarningLevels.NONE;

                    for (int i = 0; i < args.Length; i++) {
                        StringIndex += args[i].Length;

                        switch (args[i]) {
                            case "-i":
                            case "--input":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "No Input Path Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                } else if (InputPath.Length > 0) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Input Source File Path has already been specified")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                } else {
                                    InputPath = args[++i];
                                }
                                break;

                            case "-o":
                            case "--output":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "No Output Path Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                } else if (OutputPath.Length > 0) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Output Binary File Path has already been specified")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                }
                                OutputPath = args[++i];
                                break;

                            case "-w":
                            case "--warning":
                                if (i == args.Length - 1) {
                                    // error, no warnign description detected
                                    return default;
                                } else if (Program.WarningLevel != WarningLevels.NONE) {
                                    // error, already described warning level
                                    return default;
                                }
                                
                                Program.WarningLevel = args[++i] switch {
                                    "i" or "ignore"     or "I" or "IGNORE"      => WarningLevels.IGNORE,
                                    "d" or "default"    or "D" or "DEFAULT"     => WarningLevels.DEFAULT,
                                    "e" or "error"      or "E" or "ERROR"       => WarningLevels.ERROR,
                                    "v" or "verbose"    or "V" or "VERBOSE"     => WarningLevels.VERBOSE,
                                    "s" or "strict"     or "S" or "STRICT"      => WarningLevels.STRICT,
                                    "c" or "controlled" or "C" or "CONTROLLED"  => WarningLevels.CONTROLLED,

                                    _ => WarningLevels.NONE
                                };

                                if (Program.WarningLevel == WarningLevels.NONE) {
                                    // error : unrecognized warning level 
                                    return default;
                                }
                                break;

                            case "-h":
                            case "--help":
                                Response = Responses.Terminate_Success;

                                if (i == args.Length) {
                                    // generic help message
                                    Log(ErrorTypes.None, DecodingPhase.TERMINAL,
$"""
Numinous 2a03 - GPL V2 Brette Allen 2026

-i | --input        | [path]    | {Language.Connectives[(Program.ActiveLanguage, "Entrypoint Source Assembly File")]}
-o | --output       | [path]    | {Language.Connectives[(Program.ActiveLanguage, "Output ROM/Disk Binary Output")]}
-h | --help         |           | {Language.Connectives[(Program.ActiveLanguage, "Display the help string (you did that)")]}
-h | --help         | [arg]     | TODO: WRITE "GET INFO ON SPECIFIC ARGUMENT FUNCTION" HERE
-l | --language     | [lang]    | {Language.Connectives[(Program.ActiveLanguage, "Choose a language to use")]}
-w | --warning      | [level]   | TODO: Write "SET WARNING LEVEL" HERE
       
""", null, null, null);
                                } else {
                                    switch (args[++i]) {
                                        default: --i; break;

                                        case "l":
                                        case "lang":
                                        case "languages":
                                            // language specific help message.
                                            Log(ErrorTypes.None, DecodingPhase.TERMINAL, @"
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
                                            break;

                                        case "w":
                                        case "warn":
                                        case "warnings":
                                            // warnings specific help message
                                            Log(ErrorTypes.None, DecodingPhase.TERMINAL,
                                            $"""
Numinous Warning Types and how they work

ignore      : Will not display any warnings, but track the quantity for after completion.
default     : Will warn the user about potential issues with their code.
error       : Will convert all errors into warnings, enforcing the user to fix all issues.
verbose     : Will display much more warnings, reccomended and intended for those who wish to write perfect code.
strict      : Acts as 'verbose' but warnings become errors, not reccomended.
controlled  : Acts as 'strict' but prevents overruling.
       
""", null, null, null);
                                            break;

                                        case "i":
                                        case "input":
                                            Log(ErrorTypes.None, DecodingPhase.TERMINAL,
$"""
Numinous Input File

The input file argument (-i or --input) should be followed by a valid file path to a source assembly file. 
If the file is empty you will recieve an error, you may only pass one file here as the entry point file.
This decides what the root of the "include path" is, includes from here must be relative to this path.
       
""", null, null, null);
                                            break;

                                        case "o":
                                        case "output":
                                            Log(ErrorTypes.None, DecodingPhase.TERMINAL,
$"""
Numinous Output File

The output file argument (-o or --output) should be followed by a path pointing to a file to generate.
The file name must comply with the limits of your Operating System.
The directory the output file lives in must also already exist. 
If you wish to create an FDS Disk image, you must use the FDS Header variant as using the *.fds file extentsion
will not affect the kind of build produced. 

Numinous WILL overwrite a file existing with the same name at the output path if found.
       
""", null, null, null);
                                            break;
                                    }
                                }


                                break;

                            case "-l":
                            case "--language":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "No Language Provided")]}.", null, null, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
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
                                    return default;
                                }
                                break;

                            default:
                                Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(Program.ActiveLanguage, "Unrecognized Terminal Argument")]}.", null, null, ApplyWiggle(Flattened, 1 + StringIndex, args[i].Length));
                                return default;
                        }
                    }

                    if (Program.WarningLevel == WarningLevels.NONE) Program.WarningLevel = WarningLevels.DEFAULT;

                    return (InputPath, OutputPath, Response);
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
                    [CallerMemberName] string memberName = "") => WriteInfo(Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? ErrorLevels.ERROR : ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context, lineNumber, filePath, memberName);


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


            // Generated function : I don't know how regex works
            /// <summary>
            /// Tokenizes a line of code. SPACES ARE IMPORTANT FOR LINE INDEX MATH
            /// </summary>
            /// <param name="input"></param>
            /// <returns></returns>
            internal static List<string> RegexTokenize(string input) {
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