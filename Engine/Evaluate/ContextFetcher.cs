using Microsoft.CodeAnalysis;
using static Numinous.Engine.Engine;

// immediate illegal overruling might not work

namespace Numinous.Engine {
    internal static partial class Evaluate {
        /// <summary>
        /// Tokenizes referred source code at referred line from referred substring index into tokens split by step, then by delta in hierarchy, then by term.
        /// </summary>
        /// <param name="SourceFileReference"></param>
        /// <param name="SourceLineReference"></param>
        /// <param name="SourceLineSubStringIndex"></param>
        /// <returns></returns>
        internal static (List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, (OperationTypes Type, object Context) Operation, int Finish, bool Success, bool Continue) ContextFetcher(ref List<string> BasicRegexTokens, ref int SourceStringIndex, ref int ErrorReportLineNumber, ref int ErrorReportStepNumber, string SourceFilePath) {
            // use BasicRegexTokens => RegexTokens (ref, no cloning?) | Ensures we solve all new defines without mutating the original
            List<string> RegexTokens = ResolveDefines(BasicRegexTokens);
            string CollectiveContext = string.Concat(RegexTokens);

            List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens = [];
            List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens = [];
            List<(int StringOffset, int StringLength, object data, bool IsOperator)> DeltaTermTokens = [];
            List<Operators> ContainerBuffer = [];

            int Finish = -1;
            int MaxHierarchy = 0;
            int LastNonWhiteSpaceIndex = -1;
            int LastOpenContainerOperatorStringIndex = -1;

            string LITERAL_CSTRING = "";

            bool IsLastOperator = false;

            Terminal.ErrorContext ErrorContext = new();

            int i = 0, StringIndex = 0;
            (OperationTypes oper, object ctx) OperationType = ExtractOperation(ref ErrorReportLineNumber, ref ErrorReportStepNumber);

            if (OperationType == default) return default;                                                       // error pass back
            if (OperationType.oper == OperationTypes.DIRECTIVE) return default;
            for (i = 0, StringIndex = 0; i < RegexTokens.Count; step(true)) {
                if (RegexTokens[i][0] == ' ' || RegexTokens[i][0] == '\t') continue;                            // do not tokenize whitespace

                if (ContainerBuffer.Count != 0 && ContainerBuffer[^1] == Operators.FSTRING) {
                    CaptureCSTRING(c => c == '"' || c == '{', ref ErrorReportLineNumber, ref ErrorReportStepNumber);
                    if (RegexTokens[i][0] != '{') {
                        if (RegexTokens[i][0] == '"') {
                            if (CloseContainer(ref ErrorReportLineNumber, ref ErrorReportStepNumber, Operators.STRING, Operators.FSTRING)) continue;
                            else return default;
                        }
                    }
                }

                // handle tokens
                switch (RegexTokens[i]) {
                    //case "\n":
                    //    // marks the end of this tasks ctx
                    //    //ErrorReportLineNumber++;
                    //    goto CheckForTermination;

                    case "//":
                        // marks the end of this tasks ctx
                        steps(() => RegexTokens[i][0] != '\n');
                        break;

                    case "/*":
                        // marks the end of this tasks ctx
                        while (RegexTokens[i] != "*/") {
                            steps(() => RegexTokens[i][0] != '\n' || RegexTokens[i] != "*/");
                            ErrorReportLineNumber++;
                        }
                        step();
                        break;

                    case ";":
                        ErrorReportStepNumber++;            // if the user ends with ; then their errors will have a step number for 0 here, nothing I want to do about this.
                        goto CheckForTermination;


                    case "\n":
                    CheckForTermination:
                        if (ContainerBuffer.Count > 0 && ErrorContext.ErrorLevel == default) {
                            // error, unexpected end of command
                            ErrorContext = new() {
                                ErrorLevel      = ErrorLevels.ERROR,
                                ErrorType       = ErrorTypes.SyntaxError,
                                DecodingPhase   = DecodingPhases.TOKEN,
                                Message         = "Unexpected end of command.",
                                LineNumber      = ErrorReportLineNumber,
                                StepNumber      = ErrorReportStepNumber,
                                Context         = () => ApplyWiggle(CollectiveContext, StringIndex + 1, 1)
                            };
                        }

                        if (ErrorContext.ErrorLevel == ErrorLevels.ERROR) {
                            Terminal.Error(ErrorContext);
                            // report the error with the current context
                            return default;
                        }



                        CopyDeltaTokens();

                        //if (i == RegexTokens.Count - 1) {
                        //    if (Program.WarningLevel.HasFlag(WarningLevels.VERBOSE))
                        //        Terminal.Warn(ErrorTypes.SyntaxError, DecodingPhases.TOKEN,
                        //            "Lines should not end with a semi-colon", ErrorReportLineNumber, Tokens.Count, ApplyWiggle(CollectiveContext, StringIndex + 1, 1)
                        //        );
                        //    return Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? default : Success();
                        //}

                        // modify regex tokens to remove used and stored
                        RegexTokens = [.. RegexTokens.TakeLast(RegexTokens.Count - i - 1)]; // trim last step from pattern
                        i = 0;
                        CollectiveContext = CollectiveContext[(StringIndex + RegexTokens[i].Length)..];
                        StringIndex = 0;
                        break;

                    case "+":   SimpleAddOperator(Operators.ADD);           break;
                    case "-":   SimpleAddOperator(Operators.SUB);           break;
                    case "*":   SimpleAddOperator(Operators.MULT);          break;
                    case "/":   SimpleAddOperator(Operators.DIV);           break;
                    case "%":   SimpleAddOperator(Operators.MOD);           break;
                    case ">>":  SimpleAddOperator(Operators.RIGHT);         break;
                    case "<<":  SimpleAddOperator(Operators.LEFT);          break;
                    case "&":   SimpleAddOperator(Operators.BITMASK);       break;
                    case "^":   SimpleAddOperator(Operators.BITFLIP);       break;
                    case "|":   SimpleAddOperator(Operators.BITSET);        break;
                    case "==":  SimpleAddOperator(Operators.EQUAL);         break;
                    case "!=":  SimpleAddOperator(Operators.INEQUAL);       break;
                    case ">=":  SimpleAddOperator(Operators.GOET);          break;
                    case "<=":  SimpleAddOperator(Operators.LOET);          break;
                    case ">":   SimpleAddOperator(Operators.GT);            break;
                    case "<":   SimpleAddOperator(Operators.LT);            break;
                    case "<=>": SimpleAddOperator(Operators.SERIAL);        break;
                    case "=":   SimpleAddOperator(Operators.SET);           break;
                    case "+=":  SimpleAddOperator(Operators.INCREASE);      break;
                    case "-=":  SimpleAddOperator(Operators.DECREASE);      break;
                    case "*=":  SimpleAddOperator(Operators.MULTIPLY);      break;
                    case "/=":  SimpleAddOperator(Operators.DIVIDE);        break;
                    case "%=":  SimpleAddOperator(Operators.MODULATE);      break;
                    case ">>=": SimpleAddOperator(Operators.RIGHTSET);      break;
                    case "<<=": SimpleAddOperator(Operators.LEFTSET);       break;
                    case "&=":  SimpleAddOperator(Operators.ASSIGNMASK);    break;
                    case "|=":  SimpleAddOperator(Operators.ASSIGNSET);     break;
                    case "^=":  SimpleAddOperator(Operators.ASSIGNFLIP);    break;
                    case "??=": SimpleAddOperator(Operators.NULLSET);       break;
                    case "??":  SimpleAddOperator(Operators.NULL);          break;
                    case ".":   SimpleAddOperator(Operators.PROPERTY);      break;
                    case "?.":  SimpleAddOperator(Operators.NULLPROPERTY);  break;
                    case "?":   SimpleAddOperator(Operators.CHECK);         break;
                    case ":":   SimpleAddOperator(Operators.ELSE);          break;
                    case "!":   SimpleAddOperator(Operators.NOT);           break;

                    // special case
                    case "\"":
                        i++;
                        if (CaptureCSTRING(c => c == '"', ref ErrorReportLineNumber, ref ErrorReportStepNumber)) break;
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

                    case ")": if (SimpleCloseContainer(ref ErrorReportLineNumber, ref ErrorReportStepNumber, Operators.CPAREN)) break; else return default;
                    case "]": if (SimpleCloseContainer(ref ErrorReportLineNumber, ref ErrorReportStepNumber, Operators.CBRACK)) break; else return default;
                    case "}": if (SimpleCloseContainer(ref ErrorReportLineNumber, ref ErrorReportStepNumber, Operators.CBRACE)) break; else return default;

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

            IsLastOperator = LastNonWhiteSpaceIndex == -1 ? IsLastOperator : DeltaTermTokens.Count != 0 && DeltaTermTokens[LastNonWhiteSpaceIndex].IsOperator;
            if (ContainerBuffer.Count == 0 && !IsLastOperator) {
                // final steps

                CopyDeltaTermTokens();
                CopyDeltaTokens();                                                                  // process final DeltaTermTokens (valid) to StepTokens end

                return Success();
            }

            // this is permissible as context is full here - no need to involve ErrorContext
            Terminal.Error(ErrorTypes.SyntaxError, DecodingPhases.TOKEN, "Not enough context to satisfy request", ErrorReportLineNumber - 1, Tokens.Count, ApplyWiggle(CollectiveContext, CollectiveContext.Length - 1, 1));
            return default;

            #region Context Fetcher Functions
            (List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, (OperationTypes Type, object Context) Operation, int Finish, bool Success, bool Continue) Success() => (Tokens, MaxHierarchy, OperationType, Finish, true, false);
            void step(bool CollectContext = false) {
                if (CollectContext) CollectiveContext += RegexTokens[i];
                StringIndex += RegexTokens[i++].Length;
            }

            void steps(Func<bool> SeekPredicate, bool skip = false) {
                if (skip) {
                    do {
                        step();
                    } while (SeekPredicate());
                    return;
                }

                while (SeekPredicate()) {
                    step();
                }
            }

            (OperationTypes oper, object ctx) ExtractOperation(ref int ErrorReportLineNumber, ref int ErrorReportStepNumber) {
                object ctx; bool success;
                if (RegexTokens[i][0] == '#') {
                    if (DeltaTokens.Count > 0 || ErrorReportStepNumber >= 0 || LastNonWhiteSpaceIndex != 0) {
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
                                        (library_request, success) = CheckInclude(library_request);
                                        if (!success) {
                                            // error, no lib to include
                                            return default;
                                        }

                                        if (Binary) {
                                            // include as RODATA (call write task)
                                            return (OperationTypes.DIRECTIVE, default(int));
                                        }

                                        // Add the source to the read target

                                        (object _, AssembleTimeTypes _, bool Success) = Assemble([]);   // arg-less no-return-type call to assemble
                                        if (!Success) return default;
                                        else return (OperationTypes.DIRECTIVE, default(int));

                                    case "\"":
                                        library_request = CollectiveContext[(StringIndex + 1)..CollectiveContext.LastIndexOf('"')];
                                        if (File.Exists($"{Path.GetDirectoryName(SourceFilePath)}/{library_request}")) {
                                            if (Binary) {
                                                // include as RODATA (call write task)
                                                return (OperationTypes.DIRECTIVE, default(int));
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
                                return (OperationTypes.DIRECTIVE, default(int));
                            } else {
                                // error already set
                                return default;
                            }


                        case "disk":
                            if (Program.Mode == Modes.None) {
                                Program.Mode = Modes.Disk;
                                return (OperationTypes.DIRECTIVE, default(int));
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
                            return (OperationTypes.DIRECTIVE, default(int));
                    }
                }

                // keywords
                switch (RegexTokens[i]) {
                    // functions ... typeof()
                    case "if":                  // (bool) code  OR  (bool) {code block}
                    case "else":                // else if (bool code) or (bool) {code block}
                    case "loop":                // loop {code block}
                    case "break":               // exit loop
                    case "return":              // return from macro
                    case "del":                 // delete RT or AT variable

                    // AssembleTime Variables   : could be macro return or declaration
                    case "bank":
                    case "proc":
                    case "interrupt":
                    case "int":
                    case "string":
                    case "register":
                    case "flag":
                    case "const":
                        // const int, const string ... etc  (macro return type OR constant declaration)
                        break;
                }

                (ctx, success) = ParseAsVariable();
                if (success) return (OperationTypes.KEYWORD, ctx);

                // any token could be the last, we might end on an implied instruction with no implied mode for example
                if (i + 1 != RegexTokens.Count && RegexTokens[i + 1][0] == ':') {
                    if (RegexTokens[i][0] == '-') {
                        step();
                        step();

                        // next branch back is here | when we need to "beq -" we now can
                        return (OperationTypes.ANON_REL_BRANCH, '-');
                    } else if (RegexTokens[i][0] == '+') {
                        step();
                        step();

                        // for terms waiting on the next forward branch, we can now solve for "beq +"
                        return (OperationTypes.ANON_REL_BRANCH, '+');
                    }
                }

                //(ctx, success) = ParseAsFilter();
                //if (success) return (OperationTypes.KEYWORD, ctx);


                string opcode = RegexTokens[i];
                OperandDecorators FIS_ctx = VerifyInstruction(ref ErrorReportLineNumber, ref ErrorReportStepNumber);
                if (FIS_ctx == default) return default;                 // error pass back
                if (FIS_ctx == OperandDecorators.Missing)          return (OperationTypes.EVALUATE, default(int));
                else return (OperationTypes.INSTRUCTION, (opcode, FIS_ctx));

                #region         OperationExtract Local Functions

                (RunTimeVariableType ctx, bool succses) ParseAsVariable() {
                    string type = RegexTokens[i].ToLower();
                    if (type.Length < 2) return default;
                    RunTimeVariableType ctx = default;

                    ctx.signed = type[0] == 'i';
                    if (!ctx.signed && type[0] != 'u') return default;
                    

                    int substring = 2;
                    if      (type[1] == 'b') ctx.endian = true;
                    else if (type[1] != 'l') substring = 1;

                    if (substring == type.Length) return default;
                    if (!uint.TryParse(type[substring..], out ctx.size)) return default;
                    

                    if ((ctx.size & 0b111) > 0) return default;
                    

                    ctx.size >>= 3;
                    if (ctx.size == 0u) return default;
                    return (ctx, true);
                }

                (RunTimeVariableFilterType ctx, bool succses) ParseAsFilter() {
                    string type = RegexTokens[i];
                    if (type.Length < 2) return default;
                    RunTimeVariableFilterType ctx = default;

                    if (type == "num") return (default, true);

                    uint size = 0;

                    if      (type[0] == 'i') ctx.signed = true;                         // ix, ilx, ibx
                    else if (type[0] == 'u') ctx.signed = false;                        // ux, ulx, ubx
                    else if (type[0] == 'l') ctx.endian = false;                        // lx, l16, l32, l64..
                    else if (type[0] == 'b') ctx.endian = true;                         // bx, b16, b32, b64
                    else if (type[0] == 'x') {
                        if (!uint.TryParse(type[1..], out size))    return default;
                        if ((size & 0b111) > 0)                     return default;     // impossible size
                        if (size == 0u)                             return default;     // impossible size
                    } else                                          return default;

                    if      (type[1] == 'l') {                                          // ilx, ulx
                        if (ctx.endian != null)                     return default;
                        else ctx.endian = false;
                    }
                    else if (type[1] == 'b') {                                          // ibx, ubx
                        if (ctx.endian != null)                     return default;
                        else ctx.endian = true;
                    } else if (type[1] == 'x') {                                        // ix, ux, bx, lx
                        if (!uint.TryParse(type[2..], out size))    return default;
                        if ((size & 0b111) > 0)                     return default;     // impossible size
                        if (size == 0u)                             return default;     // impossible size
                        ctx.size = size >> 3;                       return (ctx, true); // specified size (one type allowed, exclusive filter)
                    } else                                          return default;

                    if (type.Length < 3)                            return default;     // il, bl, ul and ub are not valid
                    if (type[2] == 'x')                             return (ctx, true); // null size

                    if (type.Length == 3)                           return default;
                    if (!uint.TryParse(type[2..], out size))        return default;
                    if ((size & 0b111) > 0)                         return default;     // impossible size
                    if (size == 0u)                                 return default;     // impossible size
                    ctx.size = size >> 3;                           return (ctx, true); // specified size (one type allowed, exclusive filter)
                }

                OperandDecorators VerifyInstruction(ref int ErrorReportLineNumber, ref int ErrorReportStepNumber) {
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
                        #region     Explicit Instructions Supporting Immediate
                        case "nop":
                        case "cpa": // cmp
                        case "cpx":
                        case "cpy":
                        case "adc":
                        case "and":
                        case "cmp":
                        case "eor": // #
                        case "lda": // !z:
                        case "ldx": // !a:
                        case "ldy": // a:
                        case "ora": // z:
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
                        case "bzc": // #foo
                        case "bzs": // foo
                        case "bit":
                            if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                            // error malformed instruction
                            return default;
                        #endregion  Explicit Instructions Supporting Immediate
                        #region     Explicit Instructions Not Supporting Immediate
                        case "sta":
                        case "stx": // !z:
                        case "sty": // !a:
                        case "dec": // a:
                        case "inc": // z:
                        case "jmp":
                        case "jeq":
                        case "jne":
                        case "jzs":
                        case "jzc":
                        case "jpl":
                        case "jmi":
                        case "jns":
                        case "jnc":
                        case "jcs":
                        case "jcc":
                        case "jgt":
                        case "jlt":
                        case "jvc":
                        case "jvs":
                        case "jsr":
                        case "ceq":
                        case "cne":
                        case "czs":
                        case "czc":
                        case "cpl":
                        case "cmi":
                        case "cns":
                        case "cnc":
                        case "ccs":
                        case "ccc":
                        case "cgt":
                        case "clt":
                        case "cvc": // jmp !foo
                        case "cvs": // jmp foo
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
                        case "shy": // !foo
                        case "shs": // !z:foo
                        case "tas": // !a:foo
                        case "lar": // z:foo
                        case "las": // a:foo
                            if (CheckFormat(false)) goto CheckMemoryAccessRulesNotPermittingImmediate;
                            // error malformed instruction
                            return default;
                        #endregion  Explicit Instructions Not Supporting Immediate
                        #region     Explicit Instructions Not Supporting Immediate, Supporting Implied A
                        case "asl":
                        case "lsr": // !z:
                        case "rol": // !a:
                        case "ror": // a:
                        case "irl": // z:
                        case "irr": //
                            if (CheckFormat(true)) {
                                if (CheckLineTerminated()) return OperandDecorators.Found;
                                goto CheckMemoryAccessRulesNotPermittingImmediate;
                            }
                            // error malformed instruction
                            return default;
                        #endregion  Explicit Instructions Not Supporting Immediate, Supporting Implied A
                        #region     Explicit Implied Instructions Supporting Overrule
                        case "kil":
                        case "hlt":
                        case "jam": // !
                        case "stp": // 
                        case "clc":
                        case "clv": // !
                        case "sec": //      Implied or Implied Overruled
                            if (CheckFormat(true)) {
                                if (CheckLineTerminated()) return OperandDecorators.Found;
                                else if (RegexTokens[i][0] == '!') {
                                    step();
                                    if (CheckLineTerminated()) return OperandDecorators.Overruled;
                                    seek_no_whitespace();
                                    if (CheckLineTerminated()) return OperandDecorators.Overruled;
                                    // malformed instruction
                                    return default;
                                }
                            }
                            // error malformed instruction
                            return default;
                        #endregion  Explicit Implied Instructions Supporting Overrule
                        #region     Explicit Immediate Instructions
                        case "neg":
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
                        case "ane": // !#foo
                        case "xaa": // #foo
                            if (CheckFormat(false)) goto CheckImmediate;
                            // error malformed instruction
                            return default;
                        #endregion  Explicit Immediate Instructions
                        #region     Explicit Implied Instructions Not Supporting Overrule
                        case "txy": 
                        case "tyx": 
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
                        case "ccf":
                        case "sex":
                        case "abs":
                        case "rnc":
                        case "rns":
                        case "rpl":
                        case "rmi":
                        case "rvc":
                        case "rvs":
                        case "rcc":
                        case "rlt":
                        case "rcs":
                        case "rgt":
                        case "req":
                        case "rzs":
                        case "rne":
                        case "rzc": //
                            if (CheckFormat(true)) return CheckLineTerminated() ? OperandDecorators.Found: default;
                            // error malformed instruction
                            return default;
                        #endregion  Explicit Implied Instructions Not Supporting Overrule
                        
                            // brk !
                        // brk #foo
                        case "brk": // brk  : Immediate OR Implied OR Implied (Overruled)
                            if (CheckFormat(true)) {
                                if (CheckLineTerminated()) return OperandDecorators.Found;
                                else if (RegexTokens[i][0] == '!') {
                                    step();
                                    if (CheckLineTerminated()) return OperandDecorators.Overruled;
                                    seek_no_whitespace();
                                    if (CheckLineTerminated()) return OperandDecorators.Overruled;
                                    Terminal.Log(ErrorTypes.SyntaxError, DecodingPhases.TOKEN, "Malformed Instruction", ErrorReportLineNumber, ErrorReportStepNumber, CollectiveContext);
                                    // malformed instruction
                                    return default;
                                }

                                seek_no_whitespace(false);
                                if (CheckLineTerminated()) return OperandDecorators.Found;
                                else if (RegexTokens[i][0] == '#') goto CheckImmediate;
                            }
                            // error malformed instruction
                            return default;

                                    // !#foo
                                    // #foo
                                    // !foo
                                    // !z:foo
                                    // !a:foo
                                    // z:foo
                        case "lax": // a:foo
                            // TODO: write some lax safety

                            if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                            // error malformed instruction
                            return default;

                        CheckImmediate:
                            if (RegexTokens[i][0] == '!') {
                                seek_no_whitespace();
                                if (CheckLineTerminated()) return default;

                                if (RegexTokens[i][0] != '#') {
                                    // error, instruction is immediate only
                                    return default;
                                }

                                step();
                                return OperandDecorators.Immediate | OperandDecorators.Overruled;
                            }

                            if (RegexTokens[i][0] != '#') {
                                // error, instruction is immediate only
                                return default;
                            }

                            step();
                            return OperandDecorators.Immediate;

                        CheckMemoryAccessRulesNotPermittingImmediate:
                            if (RegexTokens[i][0] == '#') {
                                // error, does not permit immediate
                                return default;
                            }

                            goto CheckMemoryAccessRules;

                        CheckMemoryAccessRulesWithImmediate:
                            if (RegexTokens[i][0] == '#') {
                                step();
                                return OperandDecorators.Immediate;
                            }

                            goto CheckMemoryAccessRules;

                        CheckMemoryAccessRules:
                            return CheckMemoryAccessRules();
                    }

                    #region     Implicit Instruction Checking
                    switch (RegexTokens[i][..2]) {

                        case "ld":
                        case "cp":
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

                            if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                            // error malformed instruction
                            return default;


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

                            if (CheckFormat(false)) goto CheckMemoryAccessRulesNotPermittingImmediate;
                            // error malformed instruction
                            return default;

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
                                    if (CheckFormat(true)) return OperandDecorators.Found;
                                    // error malformed instruction
                                    return default;
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
                                    if (CheckFormat(true)) return OperandDecorators.Found;
                                    // error malformed instruction
                                    return default;
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
                                    if (CheckFormat(true)) return OperandDecorators.Found;
                                    // error malformed instruction
                                    return default;
                            }

                        case "in":
                            switch (RegexTokens[i][2]) {
                                case 'n':
                                case 'v':
                                case 'z':
                                    // error not an instruction
                                    // tac   : cmp #$01 ?
                                    return default;

                                default:
                                    if (CheckFormat(true)) return OperandDecorators.Found;
                                    // error malformed instruction
                                    return default;
                            }
                        case "de":
                            switch (RegexTokens[i][2]) {
                                case 'n':
                                case 'v':
                                case 'z':
                                    // error not an instruction
                                    // tac   : cmp #$01 ?
                                    return default;


                                default:
                                    if (CheckFormat(true)) return OperandDecorators.Found;
                                    // error malformed instruction
                                    return default;
                            }

                        default:
                            switch (RegexTokens[i][0]) {
                                case 'b':
                                case 'r':
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

                                    if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                                    // error malformed instruction
                                    return default;

                                case 't':
                                    if (RegexTokens[i][1] == RegexTokens[i][2]) {
                                        // error error nothing to do not an instruction
                                        return default;
                                    }

                                    Func<char, bool> isFlag = (char c) => c switch { 'c' or 'n' or 'v' or 'z' => default, _ => true };

                                    if (isFlag(RegexTokens[i][1]) || isFlag(RegexTokens[i][2])) {
                                        // error not an instruction
                                        return default;
                                    }

                                    if (CheckFormat(true)) return OperandDecorators.Found;
                                    // error malformed instruction
                                    return default;


                                case 'c':
                                case 'j':
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

                                    if (CheckFormat(false)) goto CheckMemoryAccessRulesNotPermittingImmediate;
                                    // error malformed instruction
                                    return default;

                            }
                            return OperandDecorators.Missing; ;

                        CheckMemoryAccessRulesNotPermittingImmediate:
                            if (RegexTokens[i][0] == '#') {
                                // error, does not permit immediate
                                return default;
                            }

                            goto CheckMemoryAccessRules;

                        CheckMemoryAccessRulesWithImmediate:
                            if (RegexTokens[i][0] == '#') {
                                step();
                                return OperandDecorators.Immediate;
                            }

                            goto CheckMemoryAccessRules;


                        CheckMemoryAccessRules:
                            return CheckMemoryAccessRules(); ; // rules
                    }
                    #endregion  Implicit Instruction Checking

                    bool CheckFormat(bool SupportsImplied) {
                        step();
                        if (i == RegexTokens.Count)     return SupportsImplied;                         // implied may complete source (might be fail later)
                        if (CheckLineTerminated())      return SupportsImplied;                         // implied may complete line
                        if (RegexTokens[i][0] != ' ')   return false;                                   // if space does not follow opcode, fail
                        seek_no_whitespace();
                        if (i == RegexTokens.Count)     return SupportsImplied;                         // if only whitespace follows, safe if implied supported
                        if (CheckLineTerminated())      return SupportsImplied;                         // implied may complete line
                        return true;                                                                    // otherwise its safe to interpret
                    }

                    OperandDecorators CheckMemoryAccessRules() {
                        if (RegexTokens[i] == "a") {
                            if (CanUseA.Contains(RegexTokens[i - 2])) {
                                step();
                                return OperandDecorators.Found;
                            }

                            step();
                            if (CheckLineTerminated()) {                                                // we've already established we can't use the syntax a here
                                // error malformed
                                return default;
                            }
                            
                            if (RegexTokens[i][0] == ':') {
                                step();
                                return OperandDecorators.Enforced_ABS;
                            } else {
                                // error, a is reserved
                                return default;
                            }
                        } else if (RegexTokens[i] == "z") {
                            step();
                            if (CheckLineTerminated()) return default;                                  // standalone z refers to zero flag, not implicit and unsuitable here
                            if (RegexTokens[i][0] == ':') {
                                step();
                                return OperandDecorators.Enforced_ZP;
                            } else {
                                // error, z is reserved
                                return default;
                            }
                        } else if (RegexTokens[i][0] == '!') {
                            step();
                            if (CheckLineTerminated()) {                                                // must overrule something
                                // error malformed
                                return default;
                            }

                            if (RegexTokens[i] == "a") {
                                step();
                                if (RegexTokens[i][0] == ':') {
                                    step();
                                    return OperandDecorators.Enforced_ABS | OperandDecorators.Overruled;
                                } else {
                                    // error, a is reserved
                                    return default;
                                }
                            } else if (RegexTokens[i] == "z") {
                                step();
                                if (RegexTokens[i][0] == ':') {
                                    step();
                                    return OperandDecorators.Enforced_ZP | OperandDecorators.Overruled;
                                } else {
                                    // error, z is reserved
                                    return default;
                                }
                            } else return OperandDecorators.Overruled;
                        }

                        return OperandDecorators.Found;
                    }
                }
                bool CheckLineTerminated() => i == RegexTokens.Count || RegexTokens[i][0] == ';' || RegexTokens[i][0] == '\n' || RegexTokens[i] == "//" || RegexTokens[i] == "/*";

                // keep seeking beyond whitespace
                void seek_no_whitespace(bool skip = false) => steps(() => !CheckLineTerminated() && (RegexTokens[i][0] == ' ' || RegexTokens[i][0] == '\t'), skip);

                #endregion      OperationExtract Local Functions
            }

            //void PrepareNextStep(ref int ErrorReportLineNumber) {
            //    MaxHierarchy = 0;
            //    StepTokens.Clear();
            //    DeltaTermTokens.Clear();
            //    ContainerBuffer = [];
            //    LastNonWhiteSpaceIndex = -1;
            //    ExtractOperation(ref ErrorReportLineNumber);
            //}

            bool CaptureCSTRING(Func<char, bool> HaltCapturePredicate, ref int ErrorReportLineNumber, ref int ErrorReportStepNumber) {
                int csi = StringIndex;

                for (; i < RegexTokens.Count && !HaltCapturePredicate(RegexTokens[i][0]); i++) {
                    LITERAL_CSTRING += RegexTokens[i];
                    StringIndex += RegexTokens[i].Length;
                }

                if (i == RegexTokens.Count && ErrorContext.ErrorLevel == default) {
                    // Unterminated String
                    ErrorContext = new() {
                        ErrorLevel      = ErrorLevels.ERROR,
                        ErrorType       = ErrorTypes.SyntaxError,
                        DecodingPhase   = DecodingPhases.TOKEN,
                        Message         = "Unterminated String",
                        LineNumber      = ErrorReportLineNumber,
                        StepNumber      = ErrorReportStepNumber,
                        Context         = () => ApplyWiggle(CollectiveContext, csi + 1, StringIndex - csi)
                    };
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

            bool CloseContainer(ref int ErrorReportLineNumber, ref int ErrorReportStepNumber, Operators CloseOperator, Operators OpenOperator) {
                SimpleAddOperator(CloseOperator);

                if (ContainerBuffer.Count == 0 && ErrorContext.ErrorLevel == default) {
                    ErrorContext = new() {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = "No Open Container before Close Container.",
                        LineNumber = ErrorReportLineNumber,
                        StepNumber = ErrorReportStepNumber,
                        Context = () => ApplyWiggle(CollectiveContext, 0, LastNonWhiteSpaceIndex + 1)
                    };

                    return false;
                }

                if (ContainerBuffer[^1] != OpenOperator && ErrorContext.ErrorLevel == default) {
                    ErrorContext = new() {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = $"Invalid Container Closer '{CollectiveContext[StringIndex]}' for Opening container '{CollectiveContext[LastOpenContainerOperatorStringIndex]}'.",
                        LineNumber = ErrorReportLineNumber,
                        StepNumber = ErrorReportStepNumber,
                        Context = () => ApplyWiggle(CollectiveContext, LastOpenContainerOperatorStringIndex + 1, StringIndex - LastOpenContainerOperatorStringIndex + 1)
                    };

                    return false;
                }

                CopyDeltaTokens();
                ContainerBuffer.RemoveAt(ContainerBuffer.Count - 1);

                return true;
            }

            bool SimpleCloseContainer(ref int ErrorReportLineNumber, ref int ErrorReportStepNumber, Operators Operator) => CloseContainer(ref ErrorReportLineNumber, ref ErrorReportStepNumber, Operator, Operator - 1);

            //void CopyStepTokens() {
            //    var StepTokenShallowCopy = StepTokens
            //        .Select(t => (
            //            t.DeltaTokens,                  // reference to a clone, should be fine
            //            t.Hierachy,
            //            t.Representation
            //        )).ToList();

            //    Tokens.Add((StepTokenShallowCopy, MaxHierarchy, OperationType));
            //}

            void CopyDeltaTokens() {
                CopyDeltaTermTokens();
                var DeltaTokensShallowCopy = DeltaTokens.Select(t => t).ToList();
                Tokens.Add((DeltaTokensShallowCopy, ContainerBuffer.Count, i == RegexTokens.Count ? CollectiveContext : CollectiveContext[..(StringIndex + RegexTokens[i].Length)]));
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
