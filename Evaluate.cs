using static Numinous.Engine.Engine;

// YOU NEED TO WRITE RULES FOR LOADING INSTRUCTIONS
// REMEMBER WE STILL NEED THE CF TO BE COOL

namespace Numinous {
    namespace Engine {
        internal static class Evaluate {

            internal enum OperationTypes : byte {
                FAIL,
                DIRECTIVE,          // eg.. #include
                OPERATION,           // eg.. lda foo
                EVALUATE,           // function, macros, RODATA writes
            }

            /// <summary>
            /// default : error
            /// found   : not immediate, overruled or enforced
            /// if has Immediate, Enforced or Overruled it counts as found
            /// </summary>
            [Flags]
            internal enum InstructionHeaderFlags : byte {
                Found           = 1 << 0,   // something

                Immediate       = 1 << 1,   // #
                Enforced_ABS    = 1 << 2,   // a:
                Enforced_ZP     = 1 << 3,   // z:
                Overruled       = 1 << 4,   // !

                Complete        = 1 << 5,   // for when there is no information after an instruction that supports implied addressing
            }

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

                bool ExpectOperator;

                int i = 0; for (; i < StepDeltaTokens.Count; i++) {

                }

                return default;

                ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool succses) ResolveCEXP() {
                    var LocalTargetScope = TargetScope;
                    ((object data, AssembleTimeTypes type, AccessLevels access) ctx, bool success) = (default, default);
                    for (; i < StepDeltaTokens.Count; i++) {
                        if (ExpectOperator != StepDeltaTokens[i].IsOperator) {
                            // error, violated VOV
                            return default;
                        }

                        (ctx, success) = GetObjectFromAlias((string)StepDeltaTokens[i].data, LocalTargetScope, AccessLevels.PUBLIC);
                        if (ExpectOperator) {
                            if ((Operators)(StepDeltaTokens[i].data) != Operators.PROPERTY) {
                                if (ctx.data == null) {
                                    // error, null reference exception
                                    return default;
                                }
                                ReTargetWithMember();     // else search object for member of alias

                            } else if ((Operators)StepDeltaTokens[i].data != Operators.NULLPROPERTY) {
                                if (ctx.data == null) {
                                    i++;                    // Skip next Operator, do not clear expect operator
                                    continue;               // pass down ctx as null
                                }
                                ReTargetWithMember();     // else search object for member of alias
                            } else {
                                // end of value resolve, return value
                                return (ctx, true);
                            }
                        }
                    }

                    return default;

                    void ReTargetWithMember() {
                        ExpectOperator = false;                     // mark from here not to expect an operator
                        switch (ctx.type) {
                            case AssembleTimeTypes.CINT:
                                TargetScope = (Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)GenerateCINT((int)ctx.data).data;
                                return;

                            case AssembleTimeTypes.CSTRING:
                                TargetScope = (Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)GenerateCSTRING((string)ctx.data).data;
                                return;

                            default:                                // other types
                                TargetScope = (Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)ctx.data;
                                return;
                        }
                    }
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
            *              REWORK FORMAT
            *                   
            *                   We now tokenize regex tokens into proper tokens progressively over the entire source code regex tokenized.
            *                   Based on detected operation, we'll decide when we need to withdraw.
            *                   
            *                   We need to mutate the Index we return for future regex token processing, as well as check the line for error reporting.
            *                   
            *                   Now we are dealing with giant data sets, we will need to write less write-heavy code. Accessing is cheap, but restructuring?
            *               
            *               
            *              FIX CollectiveContext issue
            *              ADD ERROR REPORTS
            *              MULTI_LANG FOR ERRORS
            *              
            *              
            */

            /// <summary>
            /// Tokenizes referred source code at referred line from referred substring index into tokens split by step, then by delta in hierarchy, then by term.
            /// </summary>
            /// <param name="SourceFileReference"></param>
            /// <param name="SourceLineReference"></param>
            /// <param name="SourceLineSubStringIndex"></param>
            /// <returns></returns>
            internal static (List<(List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, OperationTypes OperationType)>, int Finish, bool Success, bool Continue) ContextFetcher(ref List<string> BasicRegexTokens, ref int SourceStringIndex, ref int ErrorReportLineNumber, string SourceFilePath) {
                // use BasicRegexTokens => RegexTokens (ref, no cloning?) | Ensures we solve all new defines without mutating the original
                List<string> RegexTokens = ResolveDefines(BasicRegexTokens);
                string CollectiveContext = string.Concat(RegexTokens);

                List<(List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, OperationTypes OperationType)> Tokens = [];
                List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> StepTokens = [];
                List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens = [];
                List<(int StringOffset, int StringLength, object data, bool IsOperator)> DeltaTermTokens = [];
                List<Operators> ContainerBuffer = [];

                int Finish = -1;
                int MaxHierarchy = 0;
                int LastNonWhiteSpaceIndex = -1;
                int LastOpenContainerOperatorStringIndex = -1;

                string LITERAL_CSTRING = "";

                bool IsLastOperator = false;

                int i = 0, StringIndex = 0;
                OperationTypes OperationType = ExtractOperation();

                if (OperationType == default) return default;
                if (OperationType == OperationTypes.DIRECTIVE) return ([], default, true, false);

                if (OperationType == OperationTypes.FAIL) return default;        // error pass back
                for (i = 0, StringIndex = 0; i < RegexTokens.Count; step()) {
                    if (RegexTokens[i][0] == ' ' || RegexTokens[i][0] == '\t') continue;                            // do not tokenize whitespace

                    if (ContainerBuffer.Count != 0 && ContainerBuffer[^1] == Operators.FSTRING) {
                        CaptureCSTRING(c => c == '"' || c == '{', ref ErrorReportLineNumber);
                        if (RegexTokens[i][0] != '{') {
                            if (RegexTokens[i][0] == '"') {
                                if (CloseContainer(ref ErrorReportLineNumber, Operators.STRING, Operators.FSTRING)) continue;
                                else return default;
                            }
                        }
                    }

                    // handle tokens
                    switch (RegexTokens[i]) {
                        case "\n":
                            ErrorReportLineNumber++;
                            // consider return
                            break;


                        case "+": SimpleAddOperator(Operators.ADD); break;
                        case "-": SimpleAddOperator(Operators.SUB); break;
                        case "*": SimpleAddOperator(Operators.MULT); break;
                        case "/": SimpleAddOperator(Operators.DIV); break;
                        case "%": SimpleAddOperator(Operators.MOD); break;
                        case ">>": SimpleAddOperator(Operators.RIGHT); break;
                        case "<<": SimpleAddOperator(Operators.LEFT); break;
                        case "&": SimpleAddOperator(Operators.BITMASK); break;
                        case "^": SimpleAddOperator(Operators.BITFLIP); break;
                        case "|": SimpleAddOperator(Operators.BITSET); break;
                        case "==": SimpleAddOperator(Operators.EQUAL); break;
                        case "!=": SimpleAddOperator(Operators.INEQUAL); break;
                        case ">=": SimpleAddOperator(Operators.GOET); break;
                        case "<=": SimpleAddOperator(Operators.LOET); break;
                        case ">": SimpleAddOperator(Operators.GT); break;
                        case "<": SimpleAddOperator(Operators.LT); break;
                        case "<=>": SimpleAddOperator(Operators.SERIAL); break;
                        case "=": SimpleAddOperator(Operators.SET); break;
                        case "+=": SimpleAddOperator(Operators.INCREASE); break;
                        case "-=": SimpleAddOperator(Operators.DECREASE); break;
                        case "*=": SimpleAddOperator(Operators.MULTIPLY); break;
                        case "/=": SimpleAddOperator(Operators.DIVIDE); break;
                        case "%=": SimpleAddOperator(Operators.MODULATE); break;
                        case ">>=": SimpleAddOperator(Operators.RIGHTSET); break;
                        case "<<=": SimpleAddOperator(Operators.LEFTSET); break;
                        case "&=": SimpleAddOperator(Operators.ASSIGNMASK); break;
                        case "|=": SimpleAddOperator(Operators.ASSIGNSET); break;
                        case "^=": SimpleAddOperator(Operators.ASSIGNFLIP); break;
                        case "??=": SimpleAddOperator(Operators.NULLSET); break;
                        case "??": SimpleAddOperator(Operators.NULL); break;
                        case ".": SimpleAddOperator(Operators.PROPERTY); break;
                        case "?.": SimpleAddOperator(Operators.NULLPROPERTY); break;
                        case "?": SimpleAddOperator(Operators.CHECK); break;
                        case ":": SimpleAddOperator(Operators.ELSE); break;
                        case "!": SimpleAddOperator(Operators.NOT); break;

                        // special case
                        case "\"":
                            i++;
                            if (CaptureCSTRING(c => c == '"', ref ErrorReportLineNumber)) break;
                            return default;

                        // Container Code
                        case "(": OpenContainer(Operators.OPAREN); break;
                        case "[": OpenContainer(Operators.OBRACK); break;
                        case "{":
                            if (ContainerBuffer.Count > 0 && ContainerBuffer[0] == Operators.FSTRING) {
                                // Format String
                                OpenContainer(Operators.OBRACE);
                            } else if (ContainerBuffer.Count > 0) {
                                // error, codeblock in erroneous location
                                return default;
                            } else {
                                // Early return, this is a code block. Do not context fetch this
                                Finish = StringIndex;
                                CopyDeltaTermTokens();
                                CopyDeltaTokens();
                                CopyStepTokens();
                                return Success();
                            }
                            break;

                        case "$\"":
                            if (ContainerBuffer[^1] == Operators.FSTRING) {
                                // error layered fstring
                                return default;
                            }

                            OpenContainer(Operators.FSTRING);
                            break;

                        case ")": if (SimpleCloseContainer(ref ErrorReportLineNumber, Operators.CPAREN)) break; else return default;
                        case "]": if (SimpleCloseContainer(ref ErrorReportLineNumber, Operators.CBRACK)) break; else return default;
                        case "}": if (SimpleCloseContainer(ref ErrorReportLineNumber, Operators.CBRACE)) break; else return default;

                        case ";":
                            if (ContainerBuffer.Count > 0) {
                                Terminal.Warn(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unexpected end of command.", ErrorReportLineNumber, Tokens.Count, ApplyWiggle(CollectiveContext, StringIndex + 1, 1));
                                return default;
                            }

                            CopyDeltaTokens();
                            CopyStepTokens();

                            //if (i == RegexTokens.Count - 1) {
                            //    if (Program.WarningLevel.HasFlag(WarningLevels.VERBOSE))
                            //        Terminal.Warn(ErrorTypes.SyntaxError, DecodingPhase.TOKEN,
                            //            "Lines should not end with a semi-colon", ErrorReportLineNumber, Tokens.Count, ApplyWiggle(CollectiveContext, StringIndex + 1, 1)
                            //        );
                            //    return Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? default : Success();
                            //}

                            PrepareNextStep();

                            // modify regex tokens to remove used and stored
                            RegexTokens = [.. RegexTokens.TakeLast(RegexTokens.Count - i - 1)]; // trim last step from pattern
                            i = 0;
                            CollectiveContext = CollectiveContext[(StringIndex + RegexTokens[i].Length)..];
                            StringIndex = 0;
                            break;

                        // Term Catching
                        case ",":
                            CopyDeltaTermTokens();
                            break;

                        default:
                            DeltaTermTokens.Add((
                                StringIndex,
                                RegexTokens[i].Length,
                                new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels)> {
                                    // private for now, but once its determined to be something then it will change accordingly
                                    { "self", (RegexTokens[i], AssembleTimeTypes.CEXP, AccessLevels.PRIVATE) },
                                },
                                false
                            ));
                            LastNonWhiteSpaceIndex = DeltaTermTokens.Count - 1;
                            break;

                    }
                }

                if (IsThisSuccess()) {
                    // final steps

                    CopyDeltaTermTokens();
                    CopyDeltaTokens();                                                                  // process final DeltaTermTokens (valid) to StepTokens end
                    CopyStepTokens();                                                                   // add last captured StepToken to Tokens

                    return Success();
                }

                Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Not enough context to satisfy request", ErrorReportLineNumber - 1, Tokens.Count, ApplyWiggle(CollectiveContext, CollectiveContext.Length - 1, 1));
                return default;

                #region Context Fetcher Functions
                (List<(List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, OperationTypes OperationType)>, int finish, bool Success, bool Continue) Success() => (Tokens, Finish, true, false);
                void step() => StringIndex += RegexTokens[i++].Length;
                void steps(Func<bool> SeekPredicate) {
                    while (SeekPredicate()) {
                        step();
                    }
                }

                bool IsThisSuccess() {
                    switch (OperationType) {
                        default:
                        case OperationTypes.EVALUATE:
                            // as long as the container buffer is clear, we didn't end on an operator ... we should be good.
                            // we do permit ending with ", ;, ), ] and } however    --> although should we be hitting code blocks?

                            IsLastOperator = LastNonWhiteSpaceIndex == -1 ? IsLastOperator : DeltaTermTokens.Count != 0 && DeltaTermTokens[LastNonWhiteSpaceIndex].IsOperator;
                            return (ContainerBuffer.Count == 0 && !IsLastOperator);

                        case OperationTypes.OPERATION:
                            /*
                             * This may end up being more complex
                             * 
                             * instructions can as a rule result in 0, 1 or 2 terms - it should be impossible for more.
                             * 
                             * however stuff like rfs/rfc/jfs/jfc optimization get icky - perhaps by default it won't have that.
                             */


                            return true;
#if DEBUG
                            throw new Exception($"Error, unrecognized Operation Type : No means to evaluate CF level success with {OperationType}");
#else
                                throw new Exception($"FATAL ERROR :: (REPORT THIS ON THE GITHUB) UNRECOGNIZED OPERATION TYPE {OperationType}")
#endif
                    }

                    #region IsThisSuccess Functions
                    int GetOpCodeTermQuantityLimit() {
                        // if (idtable) NEW TABLE TODO: Implement idtable integration
                        return "" switch {


                            _ =>
#if DEBUG
                                 throw new Exception($"Error, unrecognized opcode :{opcode}")
#else
                                     throw new Exception($"FATAL ERROR :: (REPORT THIS ON THE GITHUB) UNRECOGNIZED OPCODE {opcode}")
#endif
                        };
                    }
                    #endregion
                }

                OperationTypes ExtractOperation() {
                    if (RegexTokens[i][0] == '#') {
                        if (DeltaTokens.Count > 0 || StepTokens.Count > 0 || LastNonWhiteSpaceIndex != 0) {
                            // error, assembler directives must own the entire line. (stylistic enforcement? I'm not sure)
                            return default;
                        }

                        step();

                        switch (RegexTokens[i]) {
                            case "include":
                                // demands <> :: We can take over from here

                                bool Binary = false;

                                for (; i < RegexTokens.Count; step()) {
                                    switch (RegexTokens[i]) {
                                        case " ":
                                        case "\t":
                                            continue;

                                        case "<":
                                            string library_request = CollectiveContext[(StringIndex + 1)..CollectiveContext.LastIndexOf('>')];
                                            (library_request, bool success) = CheckInclude(library_request);
                                            if (!success) {
                                                // error, no lib to include
                                                return default;
                                            }

                                            if (Binary) {
                                                // include as RODATA (call write task)
                                                return OperationTypes.DIRECTIVE;
                                            }

                                            // Add the source to the read target

                                            (object _, AssembleTimeTypes _, bool Success) = Assemble([]);   // arg-less no-return-type call to assemble
                                            if (!Success) return default;
                                            else return OperationTypes.DIRECTIVE;

                                        case "\"":
                                            library_request = CollectiveContext[(StringIndex + 1)..CollectiveContext.LastIndexOf('"')];
                                            if (File.Exists($"{Path.GetDirectoryName(SourceFilePath)}/{library_request}")) {
                                                if (Binary) {
                                                    // include as RODATA (call write task)
                                                    return OperationTypes.DIRECTIVE;
                                                }
                                                // add the source to the read target
                                                // recurse to Assemble()
                                            } else {
                                                // error
                                                return default;
                                            }
                                            break;

                                        case "bin":
                                            if (Binary) {
                                                // error set twice
                                                return default;
                                            }

                                            Binary = true;
                                            continue;

                                        default:
                                            // error, erroneous token
                                            return default;
                                    }
                                }


                                return default;
                            case "assert":
                            // requires a boolean, we'll depend on Eval

                            case "cart":
                                if (Program.Mode == Modes.None) {
                                    Program.Mode = Modes.Cartridge;
                                    return OperationTypes.DIRECTIVE;
                                } else {
                                    // error already set
                                    return default;
                                }


                            case "disk":
                                if (Program.Mode == Modes.None) {
                                    Program.Mode = Modes.Disk;
                                    return OperationTypes.DIRECTIVE;
                                } else {
                                    // error already set
                                    return default;
                                }

                            case "define":
                            // well take over here
                            // define exp ctx
                            // define exp(arg, arg) ctx-arg-arg
                            case "undefine":
                            // undefine ctx (we already replace this, so this will require some restructuring

                            case "rom":
                            // #rom <CINT>/<INT>
                            case "cpu":
                                // #cpu <CINT>/<INT>
                                return OperationTypes.DIRECTIVE;
                        }
                    }
                    return default;

                    #region         OperationExtract Local Functions

                    InstructionHeaderFlags FetchInstructionHeader() {
                        string[] CanUseA = ["asl", "lsr", "rol", "ror"];    // asl a, lsr a, rol a and ror a are all valid instructions with a as operand, no others.
                        /*
                         * ldr  (ld) + check for r
                         * str  (st) + check for r
                         * 
                         * tra  (t)  + check for r
                         * trx  (t)  + check for r
                         * try  (t)  + check for r
                         * tar  (ta) + check for r
                         * tyr  (ty) + check for r
                         * txr  (tx) + check for r
                         * tir  (t)  + check for i + check for r
                         *      check if i == r
                         * 
                         * bfc  (b)  + check for f
                         * bfs  (b)  + check for f
                         *      f must be flag type
                         *      
                         * inr  (in) + check for r
                         *      r must be:
                         *          indexing flag type
                         *          memory location
                         *          preprocessor INT
                         *          
                         * der (de)  + check for r
                         *      r must be:
                         *          indexing flag type
                         *          memory location
                         *          preprocessor INT
                         *          
                         */

                        switch (RegexTokens[i].ToLower()) {
                            case "cpa": // cmp
                            case "cpx":
                            case "cpy":
                            case "adc":
                            case "and":
                            case "cmp":
                            case "eor":
                            case "lda":
                            case "ldx":
                            case "ldy":
                            case "ora":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();
                                goto CheckMemoryAccessRulesWithImmediate;

                            case "sta":
                            case "stx":
                            case "sty":
                            case "dec":
                            case "inc":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();
                                goto CheckMemoryAccessRules;

                            case "bgt":
                            case "blt":
                            case "bpl":
                            case "bmi":
                            case "bcc":
                            case "bcs":
                            case "bnc":
                            case "bns":
                            case "bvc":
                            case "bvs":
                            case "bzc":
                            case "bzs":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();

                                // this check is if they are at $8000 and are branching a hard 128 bytes into PRGRAM 
                                if (RegexTokens[i][0] == '!') return InstructionHeaderFlags.Overruled;
                                step();
                                return InstructionHeaderFlags.Found;

                            case "bit":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();
                                goto CheckMemoryAccessRulesWithImmediate;

                            // if (id table && matching immediate do some magic) else its : u8 temp; sta temp; lda #reorg; php; lda temp; del temp
                            case "brk":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t' && RegexTokens[i][0] != '\n') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();
                                if (RegexTokens[i][0] == '#') return InstructionHeaderFlags.Immediate;
                                step();
                                return InstructionHeaderFlags.Found;


                            case "clc":
                            case "clv":
                            case "sec":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t' && RegexTokens[i][0] != '\n') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();
                                if (RegexTokens[i][0] == '!') return InstructionHeaderFlags.Overruled;
                                step();
                                return InstructionHeaderFlags.Found;


                            //case "txy": 
                            //case "tyx": 

                            case "tax":
                            case "tay":
                            case "tsx":
                            case "txa":
                            case "txs":
                            case "dex":
                            case "dey":
                            case "inx":
                            case "iny":
                            case "pha":
                            case "php":
                            case "pla":
                            case "plp":
                            case "sei":
                            case "rti":
                            case "rts":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t' && RegexTokens[i][0] != '\n') {
                                    // error malformed instruction
                                    return default;
                                }
                                step();  
                                return InstructionHeaderFlags.Found;

                            case "jmp":
                            case "jsr":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }
                                step();
                                if (RegexTokens[i][0] == '!') return InstructionHeaderFlags.Overruled;
                                step();
                                return InstructionHeaderFlags.Found;

                            case "asl":
                            case "lsr": 
                            case "rol": 
                            case "ror": // implied or memory
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t' && RegexTokens[i][0] != '\n') {
                                    // error malformed instruction
                                    return default;
                                }
                                step();
                                goto CheckMemoryAccessRules;


                            case "nop":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t' && RegexTokens[i][0] != '\n') {
                                    // error malformed instruction
                                    return default;
                                }
                                seek_nowhitespace();
                                goto CheckMemoryAccessRulesWithImmediate;

                            // Illegal instructions

                            case "aso":
                            case "slo":
                            case "rla":
                            case "rln":
                            case "lse":
                            case "sre":
                            case "rrd":
                            case "rra":
                            case "aax":
                            case "sax":
                            case "dcm":
                            case "dcp":
                            case "usb":
                            case "isc":
                            case "axa":
                            case "ahx":
                            case "sha":
                            case "sxa":
                            case "xas":
                            case "shx":
                            case "sya":
                            case "say":
                            case "shy":
                            case "shs":
                            case "tas":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }

                                seek_nowhitespace();
                                if (RegexTokens[i][0] == '!' && RegexTokens[i + 1][0] == '#') {
                                    step();
                                    return InstructionHeaderFlags.Immediate;
                                }

                                goto CheckMemoryAccessRules;

                            case "lax":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }

                                seek_nowhitespace();
                                if (RegexTokens[i][0] == '!' && RegexTokens[i + 1][0] == '#') return InstructionHeaderFlags.Immediate;
                                goto CheckMemoryAccessRules;


                            case "ana":
                            case "asb":
                            case "anc":
                            case "asr":
                            case "alr":
                            case "sbx":
                            case "xma":
                            case "arr":
                            case "axs":
                            case "axm":
                            case "ane":
                            case "xaa":
                                step();
                                goto CheckImmediate;

                            case "lar":
                            case "las":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }

                                seek_nowhitespace();
                                if (RegexTokens[i][0] == '!') {
                                    step();
                                    return InstructionHeaderFlags.Overruled;
                                }

                                return InstructionHeaderFlags.Found;

                            case "kil":
                            case "hlt":
                            case "jam":
                            case "stp":
                                step();
                                if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                    // error malformed instruction
                                    return default;
                                }

                                seek_nowhitespace();
                                if (RegexTokens[i][0] == '!') {
                                    step();
                                    return InstructionHeaderFlags.Overruled;
                                }
                                
                                return InstructionHeaderFlags.Found;


                                CheckImmediate:
                                if (RegexTokens[i][0] == '!') {
                                    seek_nowhitespace();

                                    if (RegexTokens[i][0] != '#') {
                                        // error, instruction is immediate only
                                        return default;
                                    }
                                    
                                    step();
                                    return InstructionHeaderFlags.Immediate | InstructionHeaderFlags.Overruled;
                                }
                                if (RegexTokens[i][0] != '#') {
                                    // error, instruction is immediate only
                                    return default;
                                }

                                step();
                                return InstructionHeaderFlags.Immediate;

                            CheckMemoryAccessRulesWithImmediate:
                                if (RegexTokens[i][0] == '#') {
                                    step();
                                    return InstructionHeaderFlags.Immediate;
                                }

                                goto CheckMemoryAccessRules;

                            CheckMemoryAccessRules:
                                return CheckMemoryAccessRules();
                        }

                        switch (RegexTokens[i][..2]) {

                            case "ld":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error : ldc, ldn, ldv and ldz are forbidden terms.
                                        // ldc   : lda #$00, rol            3 bytes, 4 cycles
                                        // ldn   : rol, rol, lda #$00       4 bytes, 6 cycles
                                        // ldv   : nothing
                                        // ldz   : lda #$00                 2 bytes, 2 cycles
                                        return default;
                                }

                                goto CheckMemoryAccessRulesWithImmediate;

                            case "cp":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error : not instructions
                                        return default;
                                }

                                goto CheckMemoryAccessRulesWithImmediate;


                            case "st":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error not an instruction
                                        // stz   : ldr #$00, str tar    (uses whichever reg is provably zero if reg awareness enabled, otherwise fails)
                                        return default;
                                }

                                goto CheckMemoryAccessRules;

                            case "ta":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                    case 'a':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        step();
                                        return InstructionHeaderFlags.Found;
                                }
                            case "tx":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                    case 'x':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        step();
                                        return InstructionHeaderFlags.Found;
                                }

                            case "ty":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                    case 'y':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        step();
                                        return InstructionHeaderFlags.Found;
                                }

                            case "in":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                        // memory, we be wanting to eval some
                                        step();
                                        goto CheckMemoryAccessRules;

                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        step();
                                        return InstructionHeaderFlags.Found;
                                }
                            case "de":
                                switch (RegexTokens[i][2]) {
                                    case 'c':
                                        // memory, we be wanting to eval some
                                        step();
                                        goto CheckMemoryAccessRules;

                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;


                                    default:
                                        step();
                                        return InstructionHeaderFlags.Found;
                                }

                            default:
                                // branch on flag value
                                if (RegexTokens[i][0] == 'b') {
                                    switch (RegexTokens[i][1]) {
                                        case 'a':
                                        case 'x':
                                        case 'y':
                                            return default;
                                    }

                                    switch (RegexTokens[i][2]) {
                                        case 's':
                                        case 'c':
                                            break;

                                        default: return default;
                                    }

                                    step();

                                    if (RegexTokens[i][0] != ' ' && RegexTokens[i][0] != '\t') {
                                        // error, no space after opcode
                                        return default;
                                    }

                                    if (RegexTokens[i][0] == '!') {
                                        step();
                                        return InstructionHeaderFlags.Overruled;
                                    }
                                    return InstructionHeaderFlags.Found;

                                } else if (RegexTokens[i][0] == 't') {
                                    if (RegexTokens[i][1] == RegexTokens[i][2]) {
                                        // error error nothing to do not an instruction
                                        return default;
                                    }

                                    Func<char, bool> isFlag = (char c) => c switch { 'c' or 'n' or 'v' or 'z' => default, _ => true };

                                    if (isFlag(RegexTokens[i][1]) || isFlag(RegexTokens[i][2])) {
                                        // error not an instruction
                                        return default;
                                    }

                                    return InstructionHeaderFlags.Found;
                                }

#if DEBUG
                                throw new Exception($"Could not identify as an instruction : {RegexTokens[i]}");
#else
                                    throw new Exception($"FATAL ERROR :: (REPORT THIS ON THE GITHUB) COULD NOT IDENTIFY AS AN INSTRUCTION {opcode}")
#endif

                            CheckMemoryAccessRulesWithImmediate:
                                step();
                                if (RegexTokens[i][0] == '#') {
                                    step();
                                    return InstructionHeaderFlags.Immediate;
                                }

                                goto CheckMemoryAccessRules;


                            CheckMemoryAccessRules:
                                return CheckMemoryAccessRules(); ; // rules
                        }

                        InstructionHeaderFlags CheckMemoryAccessRules() {
                            if (RegexTokens[i] == "a") {
                                step();
                                if (RegexTokens[i][0] == ':') {
                                    step();
                                    return InstructionHeaderFlags.Enforced_ABS;
                                } else if (CanUseA.Contains(RegexTokens[i - 3])) {
                                    step();
                                    return InstructionHeaderFlags.Found;
                                } else {
                                    // error, a is reserved
                                    return default;
                                }
                            } else if (RegexTokens[i] == "z") {
                                step();
                                if (RegexTokens[i][0] == ':') {
                                    step();
                                    return InstructionHeaderFlags.Enforced_ZP;
                                } else {
                                    // error, z is reserved
                                    return default;
                                }
                            } else if (RegexTokens[i][0] == '!') {
                                step();
                                if (RegexTokens[i][0] == ':') {
                                    step();
                                    return InstructionHeaderFlags.Enforced_ABS | InstructionHeaderFlags.Overruled;
                                } else {
                                    // error, a is reserved
                                    return default;
                                }
                            } else if (RegexTokens[i] == "z") {
                                step();
                                if (RegexTokens[i][0] == ':') {
                                    step();
                                    return InstructionHeaderFlags.Enforced_ZP | InstructionHeaderFlags.Overruled;
                                } else {
                                    // error, z is reserved
                                    return default;
                                }
                            }

                            return InstructionHeaderFlags.Found;
                        }
                    }

                    // keep seeking beyond whitespace
                    void seek_nowhitespace() => steps(() => (RegexTokens[i][0] == ' ' || RegexTokens[i][0] == '\t'));

                    #endregion      OperationExtract Local Functions
                }

                void PrepareNextStep() {
                    MaxHierarchy = 0;
                    StepTokens.Clear();
                    DeltaTermTokens.Clear();
                    ContainerBuffer = [];
                    LastNonWhiteSpaceIndex = -1;
                    ExtractOperation();
                }

                bool CaptureCSTRING(Func<char, bool> HaltCapturePredicate, ref int ErrorReportLineNumber) {
                    int csi = StringIndex;

                    for (; i < RegexTokens.Count && !HaltCapturePredicate(RegexTokens[i][0]); i++) {
                        LITERAL_CSTRING += RegexTokens[i];
                        StringIndex += RegexTokens[i].Length;
                    }

                    if (i == RegexTokens.Count) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unterminated String", ErrorReportLineNumber, Tokens.Count, ApplyWiggle(CollectiveContext, csi + 1, StringIndex - csi));
                        return false;
                    }

                    if (csi != StringIndex) {
                        DeltaTermTokens.Add((
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
                    DeltaTermTokens.Add((StringIndex, sl, Operator, true)); LastNonWhiteSpaceIndex = DeltaTermTokens.Count - 1; ;
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

                bool CloseContainer(ref int ErrorReportLineNumber, Operators CloseOperator, Operators OpenOperator) {
                    SimpleAddOperator(CloseOperator);

                    if (ContainerBuffer.Count == 0) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"No Open Container before Close Container.",
  ErrorReportLineNumber, StepTokens.Count, ApplyWiggle(CollectiveContext, 0, LastNonWhiteSpaceIndex + 1));

                        return false;
                    }

                    if (ContainerBuffer[^1] != OpenOperator) {
                        Terminal.Error(ErrorTypes.SyntaxError, DecodingPhase.TOKEN, $"Invalid Container Closer '{CollectiveContext[StringIndex]}' for Opening container '{CollectiveContext[LastOpenContainerOperatorStringIndex]}'.",
                          ErrorReportLineNumber, StepTokens.Count, ApplyWiggle(CollectiveContext, LastOpenContainerOperatorStringIndex + 1, StringIndex - LastOpenContainerOperatorStringIndex + 1));

                        return false;
                    }

                    CopyDeltaTokens();
                    ContainerBuffer.RemoveAt(ContainerBuffer.Count - 1);

                    return true;
                }

                bool SimpleCloseContainer(ref int ErrorReportLineNumber, Operators Operator) => CloseContainer(ref ErrorReportLineNumber, Operator, Operator - 1);

                void CopyStepTokens() {
                    var StepTokenShallowCopy = StepTokens
                        .Select(t => (
                            t.DeltaTokens,                  // reference to a clone, should be fine
                            t.Hierachy,
                            t.Representation
                        )).ToList();

                    Tokens.Add((StepTokenShallowCopy, MaxHierarchy, OperationType));
                }

                void CopyDeltaTokens() {
                    CopyDeltaTermTokens();
                    var DeltaTokensShallowCopy = DeltaTokens.Select(t => t).ToList();
                    StepTokens.Add((DeltaTokensShallowCopy, ContainerBuffer.Count, i == RegexTokens.Count ? CollectiveContext : CollectiveContext[..(StringIndex + RegexTokens[i].Length)]));
                }

                void CopyDeltaTermTokens() {
                    if (LastNonWhiteSpaceIndex == -1) return;   // Do not copy whitespace

                    // Clone Delta Tokens thus far
                    var StepDeltaTokenShallowCopy = DeltaTermTokens
                    .Select(t => (
                        t.StringOffset,
                        t.StringLength,
                        t.IsOperator
                            ? t.data
                            : Clone((Dictionary<string, (object, AssembleTimeTypes, AccessLevels)>)t.data),
                        t.IsOperator
                    )).ToList();

                    DeltaTermTokens = [];                       // wipe delta tokens for next operation
                    DeltaTokens.Add(StepDeltaTokenShallowCopy);
                }
                #endregion Context Fetcher Functions
            }
        }
    }
}
