using System.Diagnostics;
using System.Globalization;
using System.Reflection.Metadata.Ecma335;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices.Swift;
using System.Text;
using System.Text.RegularExpressions;
using Antlr4.Runtime;
using Numinous.Language;

namespace Numinous {
    namespace Engine {
        namespace System {
            internal enum Registers { A, X, Y }
            
            [Flags]
            internal enum Flags {
                Carry       = 0x01,
                Zero        = 0x02,
            //  Interrupt   = 0x04,
            //  Decimal     = 0x08,
            //  Break       = 0x10,
            //  None (1)    = 0x20,
                Overflow    = 0x40,
                Negative    = 0x80
            }
        }


        [Flags]
        internal enum WarningLevels : byte {
            IGNORE = 0x00,
            DEFAULT = 0x01,
            ERROR = 0x02,
            VERBOSE = 0x04,

            /* Internal     */
            NONE = 0xff,
            NO_OVERRULE = 0x08,

            /* Composite    */
            STRICT = VERBOSE | ERROR,
            CONTROLLED = VERBOSE | ERROR | NO_OVERRULE,

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
            NOT,

            NONE = 255
        }

        internal enum AssembleTimeTypes : byte {
            INT,        // assemble time integer
            STRING,     // assemble time string
            
            SCOPE,      // scope type
            RT,         // Runtime Variable
            REG,        // Register
            FLAG,       // CPU Status Flag
            PROC,       // Procedure
            INTER,      // Interrupt
            BANK,       // Bank
            EXP,        // Expression


            CONSTANT = 0x040,

            CINT = CONSTANT,    // Constant int
            CSTRING,            // Constant string
            TYPE,               // typeof result

            COBJECT,            // Begin of Constant Objects

            CSCOPE = COBJECT,   // Constant Scope reference
            CRT,                // Constant runtime reference
            CREG,               // Constant register reference
            CFLAG,              // Constant flag reference
            CPROC,              // Constant procedure reference
            CINTER,             // Constant interrupt reference
            CBANK,              // Constant bank reference
            CEXP,               // Constant Expression

            IRWN,       // Indexing Register with N             foo[i + 2] situations
            ICRWN,      // Indexing Constant Register with N    foo[x + 2] situations

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

        internal enum AssembleTimeValueStatus : byte {
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


            internal static class Evaluate {

                /*
                 * Some notes:
                 *      This function needs to be able to resolve the information between the changes in hierarchy. The context of the capture is decided by the brackets not
                 *      containing the delta, but the ones receiving the result of the delta.
                 *      
                 *      The responsibilities of LinearEvaluate is:
                 *          Perform operations in the correct order
                 *          propagate object references as much as possible
                 *          generate constant static object literals
                 */

                internal static (List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>> Result, bool Success) StepLinearEvaluate(List<(int StringOffset, int StringLength, object data, bool IsOperator)> StepDeltaTokens) {
                    List<Operators> ValueMutators = [];
                    List<Operators> OperatorBuffer = [];
                    List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>> ValueTokenBuffer = [];

                    List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>> ResultTermTokens = [];

                    Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>? TargetScope = Program.ActiveScopeBuffer[^1];

                    bool ExpectOperator = false;

                    int i = 0; for (; i < StepDeltaTokens.Count; i++) {

                    }

                    return default;

                    ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool succses) ResolveCEXP() {
                        var LocalTargetScope = TargetScope;
                       for (; i < StepDeltaTokens.Count; i++, ExpectOperator ^= true) {
                            ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) = GetObjectFromAlias((string)StepDeltaTokens[i].data, LocalTargetScope, AccessLevels.PUBLIC);
                            if (ExpectOperator != StepDeltaTokens[i].IsOperator) {
                                // error, violated VOV
                                return default;
                            }

                            if (ExpectOperator) {
                                if ((Operators)(StepDeltaTokens[i].data) != Operators.PROPERTY) {
                                    ++i; // fetch member name
                                    if (ctx.data == null) {
                                        // error, null reference exception
                                        return default;
                                    }

                                    // else search object for member of alias

                                    // if const primitive, generate members
                                    switch (ctx.type) {
                                        case AssembleTimeTypes.CINT:
                                            // invoke GenerateCINT(ctx.data)
                                            break;

                                        case AssembleTimeTypes.CSTRING:
                                            // invoke GenerateCSTRING(ctx.data)
                                            break;

                                        default:    // other types
                                            break;
                                    }



                                    TargetScope = (Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>)ctx.data;

                                } else if ((Operators)StepDeltaTokens[i].data != Operators.NULLPROPERTY) {
                                    ++i; // fetch member name
                                    if (ctx.data == null) {
                                        continue;               // pass down ctx as null
                                    }

                                    // else search object for member of alias
                                } else {
                                    // end of value resolve, return value
                                    return (ctx, true);
                                }
                            }
                        }

                        return default;
                    }
                }

                /// <summary>
                /// Converts integer into Constant Integer Object (CINT)
                /// </summary>
                /// <param name="data"></param>
                /// <returns></returns>
                internal static (object data, AssembleTimeTypes type, AccessLevels access) GenerateCINT(int data) => (
                    new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> {
                        {"",    (data, default, default)},
                        {"lo",  (data & 0x00ff, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) },
                        {"hi",  (data >> 8 & 0xff, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
                    }, AssembleTimeTypes.CINT, AccessLevels.PUBLIC
                );

                /// <summary>
                /// Converts string into Constant String Object (CSTRING)
                /// </summary>
                /// <param name="data"></param>
                /// <returns></returns>
                internal static (object data, AssembleTimeTypes type, AccessLevels access) GenerateCSTRING(string data) => (
                    new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> {
                        {"",        (data, default, default)},
                        {"lower",   (data.ToLower(),    AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC) },
                        {"higher",  (data.ToUpper(),    AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC) },
                        {"length",  (data.Length,       AssembleTimeTypes.CINT, AccessLevels.PUBLIC) },
                    }, AssembleTimeTypes.CINT, AccessLevels.PUBLIC
                );



                /// <summary>
                /// Ensures the left side operator has precedence over the right side operator.
                /// </summary>
                /// <param name="Left"></param>
                /// <param name="Right"></param>
                /// <returns></returns>
                internal static bool HasPrecedence(Operators Left, Operators Right) => GetHierarchy(Left) < GetHierarchy(Right);

                /// <summary>
                /// Returns the ordinance of the operator, lowest means highest hierarchy.
                /// </summary>
                /// <param name="Operator"></param>
                /// <returns></returns>
                /// <exception cref="NotSupportedException"></exception>
                internal static int GetHierarchy(Operators Operator) => Operator switch {
                    Operators.MULT => 0,
                    Operators.DIV => 0,
                    Operators.MOD => 0,

                    Operators.ADD => 1,
                    Operators.SUB => 1,

                    Operators.LEFT => 2,
                    Operators.RIGHT => 2,

                    Operators.LT => 3,
                    Operators.GT => 3,
                    Operators.GOET => 3,
                    Operators.LOET => 3,
                    Operators.SERIAL => 3,

                    Operators.EQUAL => 4,
                    Operators.INEQUAL => 4,

                    Operators.BITMASK => 5,
                    Operators.BITFLIP => 5,
                    Operators.BITSET => 5,

                    Operators.AND => 6,
                    Operators.OR => 6,

                    Operators.NULL => 7,

                    Operators.CHECK => 8,
                    Operators.ELSE => 8,

                    Operators.SET => 9,
                    Operators.INCREASE => 9,
                    Operators.DECREASE => 9,
                    Operators.MULTIPLY => 9,
                    Operators.DIVIDE => 9,
                    Operators.MODULATE => 9,
                    Operators.ASSIGNMASK => 9,
                    Operators.ASSIGNSET => 9,
                    Operators.ASSIGNFLIP => 9,
                    Operators.LEFTSET => 9,
                    Operators.RIGHTSET => 9,
                    Operators.NULLSET => 9,
#if DEBUG
                    _ => throw new NotSupportedException($"Unusable Operator Type {Operator}")
#else
                    _ => throw new NotSupportedException($"FATAL ERROR :: (REPORT THIS ON THE GITHUB) INVALID OPERATOR TYPE {Operator}")
#endif
                };

                internal static Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> GetData(object data) => (Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)data;
            }



            /*
             * Some notes:
             *      Tabs aren't equal width in each IDE, so we can't 'check' how many and generate the difference with spaces.
             *      Because of this we are going to have to store this information also.
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
             *      we can easily resolve the highest hierarchies and inject the result in between the two outside it.
             *      by repeating this process until we have resolved the lowest hierarchy we should be able to resolve any expression.
             *      
             *      Resolving isn't what the CF does, but orders it so it can be done.
             *      
             *      The CF will also need to encode whitespace in, which will seriously violate VOV.
             *      The Evaluator will need to check to exempt VOV from evaluation logic, but will need to be used in error report code.
             *      
             *      The rule has to be WVWO (repeating) where W is whitespace.
             *      
             *      Whitespace will have to be an CEXP that begins with a whitespace token.
             *      
             *      PER STEP
             *          TOKENS
             *              DELTA_TOKENS [TERMS]
             *                  STEP_DELTA_TOKENS
             *                      STRING_OFFSET
             *                      DATA
             *                          ?: OPERTOR
             *                          ?: (ITEM, CEXP,    PRIVATE)
             *                          ?: (ITEM, CSTRING, PUBLIC)
             *                      IS_OPERATOR
             *              HIERACHY
             *          MAX_HIERACHY
             *          SUCCESS
             *          
             *          
             *          TODO:
             *              SOLVE DEFINES
             *              TEST MULTI TERM
             *              ADD ERROR REPORTS
             *              MULTI_LANG FOR ERRORS
             *              
             *              
             */

            internal static (List<(List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy)>, bool Success) ContextFetcher(string[] SourceFileReference, ref int SourceLineReference) {
                List<string> RegexTokens = ResolveDefines(RegexTokenize(SourceFileReference[SourceLineReference]));
                string CollectiveContext = string.Concat(RegexTokens);

                List <(List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy)> Tokens = [];
                List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> StepTokens = [];
                List<(int StringOffset, int StringLength, object data, bool IsOperator)> StepDeltaTokens    = [];
                List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens  = [];

                List<Operators> ContainerBuffer = [];

                int MaxHierarchy = 0;
                int LastNonWhiteSpaceIndex = -1;
                int LastOpenContainerOperatorStringIndex = -1;
                
                string LITERAL_CSTRING =     "";

                bool IsLastOperator = false;

                int i = 0, StringIndex = 0;
                do {
                    for (i = 0, StringIndex = 0; i < RegexTokens.Count; StringIndex += RegexTokens[i].Length, i++) {
                        if (RegexTokens[i][0] == ' ' || RegexTokens[i][0] == '\t') continue;                            // do not tokenize whitespace

                        if (ContainerBuffer.Count != 0 && ContainerBuffer[^1] == Operators.FSTRING) {
                            CaptureCSTRING(SourceLineReference, c => c == '"' || c == '{');
                            if (RegexTokens[i][0] != '{') {
                                if (RegexTokens[i][0] == '"') {
                                    if (CloseContainer(SourceLineReference, Operators.STRING, Operators.FSTRING)) continue;
                                    else return default;
                                }
                            }
                        }

                        // handle tokens
                        switch (RegexTokens[i]) {
                            case "+":   SimpleAddOperator(Operators.ADD         ); break;
                            case "-":   SimpleAddOperator(Operators.SUB         ); break;
                            case "*":   SimpleAddOperator(Operators.MULT        ); break;
                            case "/":   SimpleAddOperator(Operators.DIV         ); break;
                            case "%":   SimpleAddOperator(Operators.MOD         ); break;
                            case ">>":  SimpleAddOperator(Operators.RIGHT       ); break;
                            case "<<":  SimpleAddOperator(Operators.LEFT        ); break;
                            case "&":   SimpleAddOperator(Operators.BITMASK     ); break;
                            case "^":   SimpleAddOperator(Operators.BITFLIP     ); break;
                            case "|":   SimpleAddOperator(Operators.BITSET      ); break;
                            case "==":  SimpleAddOperator(Operators.EQUAL       ); break;
                            case "!=":  SimpleAddOperator(Operators.INEQUAL     ); break;
                            case ">=":  SimpleAddOperator(Operators.GOET        ); break;
                            case "<=":  SimpleAddOperator(Operators.LOET        ); break;
                            case ">":   SimpleAddOperator(Operators.GT          ); break;
                            case "<":   SimpleAddOperator(Operators.LT          ); break;
                            case "<=>": SimpleAddOperator(Operators.SERIAL      ); break;
                            case "=":   SimpleAddOperator(Operators.SET         ); break;
                            case "+=":  SimpleAddOperator(Operators.INCREASE    ); break;
                            case "-=":  SimpleAddOperator(Operators.DECREASE    ); break;
                            case "*=":  SimpleAddOperator(Operators.MULTIPLY    ); break;
                            case "/=":  SimpleAddOperator(Operators.DIVIDE      ); break;
                            case "%=":  SimpleAddOperator(Operators.MODULATE    ); break;
                            case ">>=": SimpleAddOperator(Operators.RIGHTSET    ); break;
                            case "<<=": SimpleAddOperator(Operators.LEFTSET     ); break;
                            case "&=":  SimpleAddOperator(Operators.ASSIGNMASK  ); break;
                            case "|=":  SimpleAddOperator(Operators.ASSIGNSET   ); break;
                            case "^=":  SimpleAddOperator(Operators.ASSIGNFLIP  ); break;
                            case "??=": SimpleAddOperator(Operators.NULLSET     ); break;
                            case "??":  SimpleAddOperator(Operators.NULL        ); break;
                            case ".":   SimpleAddOperator(Operators.PROPERTY    ); break;
                            case "?.":  SimpleAddOperator(Operators.NULLPROPERTY); break;
                            case "?":   SimpleAddOperator(Operators.CHECK       ); break;
                            case ":":   SimpleAddOperator(Operators.ELSE        ); break;
                            case "!":   SimpleAddOperator(Operators.NOT         ); break;

                            // special case
                            case "\"":
                                i++;
                                if (CaptureCSTRING(SourceLineReference, c => c == '"')) break;
                                return default;

                            // Container Code
                            case "(":   OpenContainer(Operators.OPAREN);   break;
                            case "[":   OpenContainer(Operators.OBRACK);   break;
                            case "{":   OpenContainer(Operators.OBRACE);   break;
                            case "$\"": OpenContainer(Operators.FSTRING);  break;

                            case ")": if (SimpleCloseContainer(SourceLineReference, Operators.CPAREN)) break; else return default;
                            case "]": if (SimpleCloseContainer(SourceLineReference, Operators.CBRACK)) break; else return default;
                            case "}": if (SimpleCloseContainer(SourceLineReference, Operators.CBRACE)) break; else return default;

                            case ";":
                                if (ContainerBuffer.Count > 0) {
                                    Terminal.Warn(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unexpected end of command.", SourceLineReference, Tokens.Count, ApplyWiggle(CollectiveContext, StringIndex + 1, 1));
                                    return default;
                                }

                                

                                if (i == RegexTokens.Count - 1) {
                                    if (Program.WarningLevel.HasFlag(WarningLevels.VERBOSE))
                                        Terminal.Warn(ErrorTypes.SyntaxError, DecodingPhase.TOKEN,
                                            "Lines should not end with a semi-colon", SourceLineReference, Tokens.Count, ApplyWiggle(CollectiveContext, StringIndex + 1, 1)
                                        );
                                    return Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? default : (Tokens, true);
                                }
  

                                CopyDeltaTokens();
                                CopyStepTokens();
                                PrepareNextStep();

                                // modify regex tokens to remove used and stored
                                RegexTokens = [.. RegexTokens.TakeLast(RegexTokens.Count - i - 1)]; // trim last step from pattern
                                i = 0;
                                CollectiveContext = CollectiveContext[(StringIndex + RegexTokens[i].Length)..];
                                StringIndex = 0;
                                break;

                            // Term Catching
                            case ",":
                                CopyStepDeltaTokens();
                                break;

                            default:
                                StepDeltaTokens.Add((
                                    StringIndex,
                                    RegexTokens[i].Length,
                                    new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels)> {
                                        // private for now, but once its determined to be something then it will change accordingly
                                        { "self", (RegexTokens[i], AssembleTimeTypes.CEXP, AccessLevels.PRIVATE) },
                                    },
                                    false
                                ));
                                LastNonWhiteSpaceIndex = StepDeltaTokens.Count - 1;
                                break;

                            }
                    }

                    // If IsLastOperator is enabled, we should only disable it until we have a non-whitespace line. 
                    IsLastOperator = LastNonWhiteSpaceIndex == -1 ? IsLastOperator : StepDeltaTokens.Count != 0 && StepDeltaTokens[LastNonWhiteSpaceIndex].IsOperator;

                    if (ContainerBuffer.Count == 0 && !IsLastOperator) break;

                    if (++SourceLineReference == SourceFileReference.Length) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Not enough context to satisfy request", SourceLineReference - 1, Tokens.Count, ApplyWiggle(CollectiveContext, CollectiveContext.Length - 1, 1));
                        return default;
                    }

                    // fetch more context, restart last StepToken. 

                    PrepareNextStep();
                    RegexTokens = [.. RegexTokens, .. RegexTokenize(SourceFileReference[SourceLineReference])];
                    CollectiveContext += SourceFileReference[SourceLineReference];
                } while (true);

                CopyDeltaTokens();                                                                  // process final StepDeltaTokens (valid) to StepTokens end
                CopyStepTokens();                                                                   // add last captured StepToken to Tokens

                return (Tokens, true);

                #region Context Fetcher Functions
                void PrepareNextStep() {
                    MaxHierarchy = 0;
                    StepTokens.Clear();
                    StepDeltaTokens.Clear();
                    ContainerBuffer = [];
                    LastNonWhiteSpaceIndex = -1;
                }

                bool CaptureCSTRING(int LineNumber, Func<char, bool> HaltCapturePredicate) {
                    int csi = StringIndex;

                    for (; i < RegexTokens.Count && !HaltCapturePredicate(RegexTokens[i][0]); i++) {
                        LITERAL_CSTRING += RegexTokens[i];
                        StringIndex += RegexTokens[i].Length;
                    }

                    if (i == RegexTokens.Count) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unterminated String", LineNumber, Tokens.Count, ApplyWiggle(CollectiveContext, csi + 1, StringIndex - csi));
                        return false;
                    }

                    if (csi != StringIndex) {
                        StepDeltaTokens.Add((
                            csi,
                            csi - StringIndex,
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

                void AddOperator(Operators Operator, int sl) {
                    StepDeltaTokens.Add((StringIndex, sl, Operator, true)); LastNonWhiteSpaceIndex = StepDeltaTokens.Count - 1; ;
                }

                void SimpleAddOperator(Operators Operator) => AddOperator(Operator, 1);

                void ComplexOpenContainer(int sl, Operators Operator) {
                    CopyDeltaTokens();
                    ContainerBuffer.Add(Operator);                                  // register container type

                    AddOperator(Operator, sl);
                    LastOpenContainerOperatorStringIndex = StringIndex;
                    MaxHierarchy = Math.Max(ContainerBuffer.Count, MaxHierarchy);
                }

                void OpenContainer(Operators Operator) => ComplexOpenContainer(1, Operator);

                bool CloseContainer(int SourceLineReference, Operators CloseOperator, Operators OpenOperator) {
                    SimpleAddOperator(CloseOperator);

                    if (ContainerBuffer.Count == 0) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"No Open Container before Close Container.",
  SourceLineReference, StepTokens.Count, ApplyWiggle(CollectiveContext, 0, LastNonWhiteSpaceIndex + 1));

                        return false;
                    }

                    if (ContainerBuffer[^1] != OpenOperator) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"Invalid Container Closer '{CollectiveContext[StringIndex]}' for Opening container '{CollectiveContext[LastOpenContainerOperatorStringIndex]}'.",
                          SourceLineReference, StepTokens.Count, ApplyWiggle(CollectiveContext, LastOpenContainerOperatorStringIndex + 1, StringIndex - LastOpenContainerOperatorStringIndex + 1));

                        return false;
                    }

                    CopyDeltaTokens();
                    ContainerBuffer.RemoveAt(ContainerBuffer.Count - 1);

                    return true;
                }

                bool SimpleCloseContainer(int SourceLineReference, Operators Operator) => CloseContainer(SourceLineReference, Operator, Operator - 1);

                void CopyStepTokens() {
                    var StepTokenShallowCopy = StepTokens
                        .Select(t => (
                            t.DeltaTokens,                  // reference to a clone, should be fine
                            t.Hierachy,
                            t.Representation
                        )).ToList();

                    Tokens.Add((StepTokenShallowCopy, MaxHierarchy));
                }

                void CopyDeltaTokens() {
                    CopyStepDeltaTokens();
                    var DeltaTokensShallowCopy = DeltaTokens.Select(t => t).ToList();
                    StepTokens.Add((DeltaTokensShallowCopy, ContainerBuffer.Count, i == RegexTokens.Count ? CollectiveContext : CollectiveContext[..(StringIndex + RegexTokens[i].Length)]));
                }

                void CopyStepDeltaTokens() {
                    if (LastNonWhiteSpaceIndex == -1) return;   // Do not copy whitespace

                    // Clone Delta Tokens thus far
                    var StepDeltaTokenShallowCopy = StepDeltaTokens
                    .Select(t => (
                        t.StringOffset,
                        t.StringLength,
                        t.IsOperator
                            ? t.data
                            : Clone((Dictionary<string, (object, AssembleTimeTypes, AccessLevels)>)t.data),
                        t.IsOperator
                    )).ToList();

                    StepDeltaTokens = [];                       // wipe delta tokens for next operation
                    DeltaTokens.Add(StepDeltaTokenShallowCopy);
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
            internal enum Unary : byte {
                INC,
                DEC,
                ABS,
                NEG,
                BIT,
                NOT
            };

            internal static bool IsNonLiteral(char First) =>
                    First switch {
                        '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9' or '$' or '%' or '&' or '+' or '-' or '!' or '^' or '*' or '[' or ']' or '{' or '}' or '\'' or '#' or '~' or ':' or ',' or '<' or '.' or '>' or '/' or '?' => false,
                        _ => true,
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

            internal static List<string> ResolveDefines(List<string> tokens) {
                bool DidReplace;

                do {
                    DidReplace = false;
                    List<string> UpdatedTokens = [];

                    for (int i = 0; i < tokens.Count; i++) {
                        string token = tokens[i];
                        ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) = Database.GetObjectFromAlias(token, AccessLevels.PUBLIC);
                        if (success && ctx.type == AssembleTimeTypes.CSTRING) {
                            var foo = (Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)ctx.data;
                            var bar = foo[""].data;
                            string Capture = (string)bar;
                            UpdatedTokens.AddRange(RegexTokenize(Capture));
                            DidReplace = true;
                        } else UpdatedTokens.Add(token);
                    }
                    
                    tokens = UpdatedTokens;
                } while (DidReplace);
                return tokens;
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
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "No Input Path Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                } else if (InputPath.Length > 0) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Input Source File Path has already been specified")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                } else {
                                    InputPath = args[++i];
                                }
                                break;

                            case "-o":
                            case "--output":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "No Output Path Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                } else if (OutputPath.Length > 0) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Output Binary File Path has already been specified")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                }
                                OutputPath = args[++i];
                                break;

                            case "-w":
                            case "--warning":
                                if (i == args.Length - 1) {
                                    // error, no warning description detected
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

-i | --input        | [path]    | {(Language.Language.Connectives[(Program.ActiveLanguage, "Entrypoint Source Assembly File")])}
-o | --output       | [path]    | {(Language.Language.Connectives[(Program.ActiveLanguage, "Output ROM/Disk Binary Output")])}
-h | --help         |           | {(Language.Language.Connectives[(Program.ActiveLanguage, "Display the help string (you did that)")])}
-h | --help         | [arg]     | TODO: WRITE "GET INFO ON SPECIFIC ARGUMENT FUNCTION" HERE
-l | --language     | [lang]    | {(Language.Language.Connectives[(Program.ActiveLanguage, "Choose a language to use")])}
-w | --warning      | [level]   | TODO: Write "SET WARNING LEVEL" HERE
       
""", -1, default, null);
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
", -1, default, null);
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
verbose     : Will display much more warnings, recommended and intended for those who wish to write perfect code.
strict      : Acts as 'verbose' but warnings become errors, not recommended.
controlled  : Acts as 'strict' but prevents overruling.
       
""", -1, default, null);
                                            break;

                                        case "i":
                                        case "input":
                                            Log(ErrorTypes.None, DecodingPhase.TERMINAL,
$"""
Numinous Input File

The input file argument (-i or --input) should be followed by a valid file path to a source assembly file. 
If the file is empty you will receive an error, you may only pass one file here as the entry point file.
This decides what the root of the "include path" is, includes from here must be relative to this path.
       
""", -1, default, null);
                                            break;

                                        case "o":
                                        case "output":
                                            Log(ErrorTypes.None, DecodingPhase.TERMINAL,
$"""
Numinous Output File

The output file argument (-o or --output) should be followed by a path pointing to a file to generate.
The file name must comply with the limits of your Operating System.
The directory the output file lives in must also already exist. 
If you wish to create an FDS Disk image, you must use the FDS Header variant as using the *.fds file extension
will not affect the kind of build produced. 

Numinous WILL overwrite a file existing with the same name at the output path if found.
       
""", -1, default, null);
                                            break;
                                    }
                                }


                                break;

                            case "-l":
                            case "--language":
                                if (i == args.Length - 1) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "No Language Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                }

                                Program.ActiveLanguage = args[++i] switch {
                                    "en_gb" => Language.Languages.English_UK,
                                    "en_us" => Language.Languages.English_US,
                                    "es"    => Language.Languages.Spanish,
                                    "de"    => Language.Languages.German,
                                    "ja"    => Language.Languages.Japanese,
                                    "fr"    => Language.Languages.French,
                                    "pt"    => Language.Languages.Portuguese,
                                    "ru"    => Language.Languages.Russian,
                                    "it"    => Language.Languages.Italian,
                                    "ne"    => Language.Languages.Dutch,
                                    "pl"    => Language.Languages.Polish,
                                    "tr"    => Language.Languages.Turkish,
                                    "vt"    => Language.Languages.Vietnamese,
                                    "in"    => Language.Languages.Indonesian,
                                    "cz"    => Language.Languages.Czech,
                                    "ko"    => Language.Languages.Korean,
                                    "uk"    => Language.Languages.Ukrainian,
                                    "ar"    => Language.Languages.Arabic,
                                    "sw"    => Language.Languages.Swedish,
                                    "pe"    => Language.Languages.Persian,
                                    "ch"    => Language.Languages.Chinese,

                                    _       => Language.Languages.Null
                                };

                                if (Program.ActiveLanguage == Language.Languages.Null) {
                                    Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Invalid Language Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length));
                                    return default;
                                }
                                break;

                            default:
                                Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Unrecognized Terminal Argument")])}.", -1, default, ApplyWiggle(Flattened, 1 + StringIndex, args[i].Length));
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
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int StepNumber, string? Context,
                    int     lineNumber = 0, 
                    string  filePath = "", 
                    string  memberName = "")
#else
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) 
#endif
                {
                    Language.Languages UseLanguage = Program.ActiveLanguage;
                    if (Program.ActiveLanguage == Language.Languages.Null) UseLanguage = Program.ActiveLanguage = Language.Language.CaptureSystemLanguage();
                    if (Program.ActiveLanguage == Language.Languages.Null) UseLanguage = Program.ActiveLanguage = Program.ActiveLanguage = Language.Languages.English_UK;


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

                    ErrorTypeString     = Language.Language.ErrorTypeMessages[(UseLanguage, ErrorType)];
                    ErrorTypeConnective = Language.Language.Connectives[(UseLanguage, "During")];
                    DecodePhaseString   = Language.Language.DecodePhaseMessages[(UseLanguage, Phase)];
                    LocationString      = LineNumber == -1 ? "" : (StepNumber == 0 ? $"({LineNumber})" : $"({LineNumber}, {StepNumber})");
                    Context = Context == null ? "" : $": {Context}";

                    // Something Error During Something Phase :: Could not do a thing (1, 2) : ah, the issue is here.
#if DEBUG
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {Program.SourceFileNameBuffer[^1]} {LocationString}{Context}");
                    Console.WriteLine($"[{filePath}:{lineNumber}] {memberName}");
#else
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {LocationString}{Context}");
#endif

                Exit:
                    Console.ResetColor();
                }

#if DEBUG
                    
                internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int StepNumber, string? Context,
                    [CallerLineNumber] int lineNumber = 0,
                    [CallerFilePath] string filePath = "",
                    [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context, lineNumber, filePath, memberName);
                

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int StepNumber, string? Context,
                    [CallerLineNumber] int lineNumber = 0,
                    [CallerFilePath] string filePath = "",
                    [CallerMemberName] string memberName = "") => WriteInfo(Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? ErrorLevels.ERROR : ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context, lineNumber, filePath, memberName);


                internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int StepNumber, string? Context,
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

            /// <summary>
            /// Searches for 'Alias' in TargetScope (Either ActiveScope or specified scope)
            /// </summary>
            /// <param name="Alias"></param>
            /// <param name="TargetScope"></param>
            /// <param name="UsedAccessLevel"></param>
            /// <returns></returns>
            internal static ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) GetObjectFromAlias(string Alias, Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> TargetScope, AccessLevels UsedAccessLevel) {
                ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool found, bool error) = (default, default, default);
                List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>> LocalObjectSearchBuffer;

                if (TargetScope == Program.ActiveScopeBuffer[^1]) {
                        LocalObjectSearchBuffer = [.. Program.ObjectSearchBuffer, Program.ActiveScopeBuffer[^1]];
                } else  LocalObjectSearchBuffer = [TargetScope]; 


                if (!LocalObjectSearchBuffer.Contains(TargetScope)) LocalObjectSearchBuffer.Add(TargetScope);

                foreach (var LocalObjectSearchContainer in LocalObjectSearchBuffer) {
                    if (LocalObjectSearchContainer.TryGetValue(Alias, out ctx)) {
                        if (UsedAccessLevel < ctx.access) {
                            // error, invalid permissions to access item
                            return default;
                        } else return (ctx, true);
                    }
                }

                return default;
            }

            /// <summary>
            /// Database methods
            /// </summary>
            internal static class Database {
                /// <summary>
                /// Get without specifying target scope.
                /// 
                /// Order may be changed depending on Program.ObjectSearchBuffer. The ActiveScope is ALWAYS searched first, afterwards its down to this.
                /// By default the ObjectSearchBuffer only includes the root scope.
                /// </summary>
                /// <param name="Alias"></param>
                /// <param name="UsedAccessLevel"></param>
                /// <returns></returns>
                internal static ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) GetObjectFromAlias(string Alias, AccessLevels UsedAccessLevel) {
                    List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>> LocalObjectSearchBuffer = [Program.ActiveScopeBuffer[^1], .. Program.ObjectSearchBuffer];
                    return __GetObjectFromAlias(Alias, LocalObjectSearchBuffer, UsedAccessLevel);
                }
                /// <summary>
                /// Only check the specified scope, may be used like rs\foo. Note that the scope used to specify will be the result of the other method being used first.
                /// After this its hierarchy based and therefore rs\foo\foo may not always work.
                /// </summary>
                /// <param name="Alias"></param>
                /// <param name="TargetScope"></param>
                /// <param name="UsedAccessLevel"></param>
                /// <returns></returns>
                /// 
                internal static ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) GetObjectFromAlias(string Alias, Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> TargetScope, AccessLevels UsedAccessLevel) => __GetObjectFromAlias(Alias, [TargetScope], UsedAccessLevel);
                
                /// <summary>
                /// Internal function iterating over the LocalObjectSearchPath to find the required context if possible.
                /// </summary>
                /// <param name="Alias"></param>
                /// <param name="LocalObjectSearchBuffer"></param>
                /// <param name="UsedAccessLevel"></param>
                /// <returns></returns>
                private  static ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) __GetObjectFromAlias(string Alias, List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>> LocalObjectSearchBuffer, AccessLevels UsedAccessLevel) {
                    ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool found, bool error) = (default, default, default);
                    foreach (var LocalObjectSearchContainer in LocalObjectSearchBuffer) {
                        if (LocalObjectSearchContainer.TryGetValue(Alias, out ctx)) {
                            if (UsedAccessLevel < ctx.access) {
                                // error, invalid permissions to access item
                                return default;
                            } else return (ctx, true);
                        }
                    }

                    return default;
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