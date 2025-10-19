/*
    TODO: RegexTokenizedBuffer to store (ctx, index, length)
          On error report, refer to RegexParsed[0].ctx, index, length etc...
*/

namespace uhla.Core {
    internal static partial class Core {

        internal enum Modes {
            STANDARD,
            HEADER,
            OPERAND,
            LABEL,                  // looks something like 'foo:' ... uses AngoriMath
        }

        internal enum Statuses {
            FAIL,
            
            OK,                     // generic success
            INIT_ANGORI,            // use angori math
            CLOSE_BLOCK             // close code block :: found terminating '}'
        }
        
        /// <summary>
        /// The Lexer takes in the entire source file as byref array of string tokens from a simple regex system.
        /// It's sole purpose is to check on instruction terminations, which according to it are comprised of:
        ///     - semicolons outside of strings
        ///     - ending a line with a value where all containers are closed
        ///
        /// Because some systems may break the rules above, sometimes certain processes will NOT invoke the standard lexer.
        /// </summary>
        /// <param name="BasicRegexTokens">The source information that has been regex parsed and split into string tokens.</param>
        /// <param name="SourceTokenIndex">The index of the token we being lexing first.</param>
        /// <param name="ErrorReportLineNumber">The line the token we are lexing first is on.</param>
        /// <param name="ErrorReportStepNumber">The count of instructions that have passed on this line + 1</param>
        /// <param name="SourceFilePath">The file path we obtained the source content from, for debugging information.</param>
        /// <returns></returns>
        internal static (List<HierarchyTokens_t> Tokens, int MaxHierarchy, Statuses Status) Lexer(Memory<string> BasicRegexTokens, ref int SourceTokenIndex, ref int ErrorReportLineNumber, ref int ErrorReportStepNumber, string SourceFilePath, Modes mode = Modes.STANDARD) {
            // use BasicRegexTokens => RegexTokens (ref, no cloning?) | Ensures we solve all new defines without mutating the original
            //List<string> RegexTokens = ResolveDefines(BasicRegexTokens);
            var Representation = string.Empty;

            List<HierarchyTokens_t> Tokens          = [];
            List<Operators>       ContainerBuffer = [];
            List<(string token, int StringIndex, int StringLength)> DefineResolveBuffer = [];

            int LocalSourceTokenIndex = SourceTokenIndex, LocalErrorReportLineNumber = ErrorReportLineNumber, LocalErrorReportStepNumber = ErrorReportStepNumber;
            int MaxHierarchy = 0, LastNonWhiteSpaceIndex = -1, LastOpenContainerOperatorStringIndex = -1;

            var literalCstring = string.Empty;
            var IsLastOperator = false;

            Terminal.ErrorContext ErrorContext = new();

            int i = 0, StringIndex = 0; (string ctx, int StringIndex, int StringLength) ActiveToken;

            Step();

            for (; DefineResolveBuffer.Count > 0 && i < BasicRegexTokens.Length; Step()) {
                if (ActiveToken.ctx[0] == ' ' || ActiveToken.ctx[0] == '\t') continue;                          // do not tokenize whitespace TODO: check redundant
                
                // capture interpolated string
                if (ContainerBuffer.Count != 0 && ContainerBuffer[^1] == Operators.FSTRING) {
                    CaptureCSTRING(c => c is '"' or '{');
                    if (ActiveToken.ctx[0] is '"') {
                        if (CloseContainer(Operators.STRING, Operators.FSTRING)) continue;
                        return default;
                    }
                }

                // handle tokens
                switch (ActiveToken.ctx) {
                    case "//":
                        // marks the end of this task's ctx
                        Steps(() => ActiveToken.ctx[0] != '\n');
                        break;

                    case "/*":
                        // marks the end of this task's ctx
                        while (ActiveToken.ctx != "*/") {
                            Steps(() => ActiveToken.ctx[0] != '\n' || ActiveToken.ctx != "*/");
                            ErrorReportLineNumber++;
                        }
                        Step();
                        break;

                    case ";":
                        ErrorReportStepNumber++;            // if the user ends with ; then their errors will have a step number for 0 here, nothing I want to do about this.
                        goto CheckForTermination;


                    case "\n":
                    CheckForTermination:
                        if (ContainerBuffer.Count > 0 && ErrorContext.ErrorLevel == default) {
                            // error, unexpected end of command
                            ErrorContext = new Terminal.ErrorContext {
                                ErrorLevel      = ErrorLevels.ERROR,
                                ErrorType       = ErrorTypes.SyntaxError,
                                DecodingPhase   = DecodingPhases.TOKEN,
                                Message         = "Unexpected end of command.",
                                LineNumber      = ErrorReportLineNumber,
                                StepNumber      = ErrorReportStepNumber,
                                Context         = () => Terminal.ApplyWiggle(Representation, ActiveToken.StringIndex + 1, 1)
                            };
                        }

                        if (ErrorContext.ErrorLevel == ErrorLevels.ERROR) {
                            Terminal.Error(ErrorContext);
                            // report the error with the current context
                            return default;
                        }

                        Tokens.Add(new HierarchyTokens_t([], 0, string.Empty));
                    
                        i                 = 0;
                        Representation = Representation[(ActiveToken.StringIndex + ActiveToken.StringLength)..];
                        ActiveToken.StringIndex       = 0;
                        break;

                    case "~":   SimpleAddOperator(Operators.BITNOT);        break;
                    case "++":  SimpleAddOperator(Operators.INC);           break;
                    case "--":  SimpleAddOperator(Operators.DEC);           break;
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
                    case "!":   SimpleAddOperator(Operators.NOT);           break;

                    // special case
                    case "\"":
                        i++;
                        if (CaptureCSTRING(c => c is '"')) break;
                        return default;

                    // Container Code
                    case "(": OpenContainer(Operators.OPAREN); break;
                    case "[": OpenContainer(Operators.OBRACK); break;
                    case "?": OpenContainer(Operators.CHECK);  break;
                    case "{":
                        switch (ContainerBuffer.Count, LexerMode: mode) {
                            case (> 0, _) when ContainerBuffer[^1] == Operators.FSTRING:
                                // Format String
                                OpenContainer(Operators.OBRACE);
                                break;
                            
                            case (0, Modes.HEADER):
                                // header lexing is complete
                                SourceTokenIndex = LocalSourceTokenIndex; ErrorReportLineNumber = LocalErrorReportLineNumber; ErrorReportStepNumber = LocalErrorReportStepNumber;
                                return Success();

                            default:
                                // error, codeblock in erroneous location
                                return default;
                        }
                        break;

                    case "$\"":
                        if (ContainerBuffer[^1] == Operators.FSTRING) {
                            // error layered fstring
                            return default;
                        }

                        OpenContainer(Operators.FSTRING);
                        break;

                    case ")": if (SimpleCloseContainer(Operators.CPAREN)) break; return default;
                    case "]": if (SimpleCloseContainer(Operators.CBRACK)) break; return default;
                    case "}":
                        if (ContainerBuffer.Count == 0) {
                            SourceTokenIndex = LocalSourceTokenIndex; ErrorReportLineNumber = LocalErrorReportLineNumber; ErrorReportStepNumber = LocalErrorReportStepNumber;
                            return Success(Statuses.CLOSE_BLOCK);
                        }
                        if (SimpleCloseContainer(Operators.CBRACE))       break; return default;
                        
                    
                    case ":":
                        if (ContainerBuffer.Count > 0) {
                            if (CloseContainer(Operators.ELSE, Operators.CHECK)) break; return default;
                        }

                        Step(); // We don't include the colon, it's a lexer flag if anything. If it was typed will still appear in the error report.
                        SourceTokenIndex = LocalSourceTokenIndex; ErrorReportLineNumber = LocalErrorReportLineNumber; ErrorReportStepNumber = LocalErrorReportStepNumber;
                        return Success(Statuses.INIT_ANGORI);

                    // Term Catching
                    case ",":
                        Tokens[^1].DeltaTokens.Add([]);
                        break;

                    default:
                        Tokens[^1].DeltaTokens[^1].Add(new EvalToken(
                            ActiveToken.StringIndex,
                            ActiveToken.StringLength,
                            new ObjectToken(new Dictionary<string, ObjectToken> {
                                { "#self", new ObjectToken(ActiveToken.ctx, AssembleTimeTypes.EXP) }
                            }, AssembleTimeTypes.EXP),
                            false
                        ));
                        LastNonWhiteSpaceIndex = Tokens[^1].DeltaTokens[^1].Count - 1;
                        break;

                }
            }

            IsLastOperator = LastNonWhiteSpaceIndex == -1 ? IsLastOperator : Tokens[^1].DeltaTokens[^1].Count != 0 && Tokens[^1].DeltaTokens[^1][LastNonWhiteSpaceIndex].IsOperator;
            if (ContainerBuffer.Count == 0 && !IsLastOperator) {
                // final steps
                SourceTokenIndex = LocalSourceTokenIndex; ErrorReportLineNumber = LocalErrorReportLineNumber; ErrorReportStepNumber = LocalErrorReportStepNumber;
                return Success();
            }

            // this is permissible as context is full here - no need to involve ErrorContext
            Terminal.Error(ErrorTypes.SyntaxError, DecodingPhases.TOKEN, "Not enough context to satisfy request", ErrorReportLineNumber - 1, Tokens.Count, Terminal.ApplyWiggle(Representation, Representation.Length - 1, 1), SourceFilePath);
            return default;

            #region Lexer Functions

            /*
             * Evalutes a token without context, returns an incomplete ErrorContext
             */
            Terminal.ErrorContext ComputeToken() {
                return default;
            }

            void CheckProcessFunctionalDefines() {
                // very object in DB
                var FEXPQueery = Database.GetObjectFromAlias(ActiveToken.ctx, Program.ActiveScopeBuffer[^1]);
                if (FEXPQueery is null || FEXPQueery.type != AssembleTimeTypes.FEXP) return;

                // fetch object from DB and capture arguments to invoke it
                var FEXPObj = (Dictionary<string, ObjectToken>)FEXPQueery.data;
                var ArgsCapture = CaptureFEXP((int)FEXPObj["args"].data);

                // if capturing was unsuccessful, exit.
                if (!ArgsCapture.success) return;

                // execute define
                var FEXP_Lambda = (Func<List<(string token, int StringIndex, int StringLength)>, List<(string token, int StringIndex, int StringLength)>>)FEXPObj[""].data;
                
                // store processed result in Lexer buffer
                DefineResolveBuffer.InsertRange(0, FEXP_Lambda(ArgsCapture.args));
            }
            
            (List<(string token, int StringIndex, int StringLength)> args, bool success) CaptureFEXP(int nArgs) {
                // capture tokens 

                var args = new List<(string ctx, int StringIndex, int StringLength)>();

                if (nArgs == 0) {
                    seek_no_whitespace();
                    if (ActiveToken.ctx[0] == ')') return ([], true);
                    // error : did not expect arguments
                    return default;
                }
                
                while (!CheckLineTerminated() && ActiveToken.ctx[0] != ')') {
                    var token = string.Empty;

                    for (; ActiveToken.ctx[0] != ',' || ActiveToken.ctx[0] == ')'; Step()) {
                        token += ActiveToken;
                    }

                    args.Add((token, ActiveToken.StringIndex, token.Length));
                    nArgs--;
                }

                if (nArgs > 0) {
                    // error, unsatisfied arguments
                    return default;
                }

                if (nArgs < 0) {
                    // error, too many arguments
                    return default;
                }

                if (CheckLineTerminated()) {
                    // malformed FEXP call
                    return default;
                }

                return (args, true);
            }
            
            (List<HierarchyTokens_t> Tokens, int MaxHierachy, Statuses Status) Success(Statuses status = Statuses.OK) => (Tokens, MaxHierarchy, status);
            
            void Step(bool regexParse = true) {
                ActiveToken = default;

                if (DefineResolveBuffer.Count == 0) {                                                   // si to be mutated ONLY by typed tokens
                    Representation += BasicRegexTokens.Span[i];
                    StringIndex += BasicRegexTokens.Span[i].Length;
                }

                if (regexParse) {
                    bool                                                    success;
                    List<(string token, int StringIndex, int StringLength)> ctx;
                    if (DefineResolveBuffer.Count == 0) {
                        (ctx, success)      = PartialResolveDefine(BasicRegexTokens.Span[i++]);
                        DefineResolveBuffer = ctx;
                    }
                    else {
                        (ctx, success) = PartialResolveDefine(DefineResolveBuffer[0].token);
                        DefineResolveBuffer.RemoveAt(0);
                        DefineResolveBuffer.InsertRange(0, ctx);
                    }

                    if (success) Step();
                    ActiveToken = DefineResolveBuffer[0];
                    DefineResolveBuffer.RemoveAt(0);
                }
                else ActiveToken = (BasicRegexTokens.Span[i++], StringIndex, BasicRegexTokens.Span[i - 1].Length);
            }
            
            bool CheckLineTerminated() => (i == BasicRegexTokens.Length && DefineResolveBuffer.Count == 0) || ActiveToken.ctx[0] == ';' || ActiveToken.ctx[0] == '\n' || ActiveToken.ctx == "//" || ActiveToken.ctx == "/*";

            // keep seeking beyond whitespace
            void seek_no_whitespace(bool skip = false, bool regexParse = true) => Steps(() => !CheckLineTerminated() && (ActiveToken.ctx[0] == ' ' || ActiveToken.ctx[0] == '\t'), skip, regexParse);

            void Steps(Func<bool> seekPredicate, bool skip = false, bool regexParse = true) {
                if (skip) {
                    do {
                        Step(regexParse);
                    } while (seekPredicate());
                    return;
                }

                while (seekPredicate()) {
                    Step(regexParse);
                }
            }

            bool CaptureCSTRING(Func<char, bool> HaltCapturePredicate) {
                int csi = ActiveToken.StringIndex;

                // TODO: Empty out Define Resolved Buffer first, then any captured tokens DO NOT UNDERGO DEFINE EVALUATION

                for (; i < BasicRegexTokens.Length && !HaltCapturePredicate(ActiveToken.ctx[0]); Step(false)) {
                    literalCstring += ActiveToken;
                }

                if (i == BasicRegexTokens.Length && ErrorContext.ErrorLevel == default) {
                    // Unterminated String
                    ErrorContext = new Terminal.ErrorContext {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = "Unterminated String",
                        LineNumber = LocalErrorReportLineNumber,
                        StepNumber = LocalErrorReportStepNumber,
                        Context = () => Terminal.ApplyWiggle(Representation, csi + 1, ActiveToken.StringIndex - csi)
                    };
                    return false;
                }

                if (csi != ActiveToken.StringIndex) {
                    Tokens[^1].DeltaTokens[^1].Add(new EvalToken(
                    csi,
                    csi - ActiveToken.StringIndex,
                    new ObjectToken(
                        new Dictionary<string, ObjectToken>() {
                           {"self",    new ObjectToken(literalCstring,        AssembleTimeTypes.STRING) },
                           {"length",  new ObjectToken(literalCstring.Length, AssembleTimeTypes.INT) },
                        },AssembleTimeTypes.STRING),
                        false
                    ));

                    literalCstring = "";   // wipe string for next capture
                }
                return true;
            }

            // Tokens[^1] contains tokens per term for last delta. DeltaTokens[^1] is the last term's tokens.
            void AddOperator(Operators Operator, int sl) {
                Tokens[^1].DeltaTokens[^1].Add(new EvalToken(ActiveToken.StringIndex, sl, new ObjectToken(Operator, AssembleTimeTypes.OPERATOR), true));
                LastNonWhiteSpaceIndex = Tokens[^1].DeltaTokens[^1].Count - 1;
            } 
            
            void SimpleAddOperator(Operators Operator) => AddOperator(Operator, 1);

            void ComplexOpenContainer(int sl, Operators Operator) {
                Tokens.Add(new HierarchyTokens_t([], 0, string.Empty));
                ContainerBuffer.Add(Operator);                                  // register container type

                AddOperator(Operator, sl);
                LastOpenContainerOperatorStringIndex = ActiveToken.StringIndex;
                MaxHierarchy = Math.Max(ContainerBuffer.Count, MaxHierarchy);
            }

            void OpenContainer(Operators Operator) => ComplexOpenContainer(1, Operator);

            bool CloseContainer(Operators CloseOperator, Operators OpenOperator) {
                SimpleAddOperator(CloseOperator);

                if (ContainerBuffer.Count == 0 && ErrorContext.ErrorLevel == default) {
                    ErrorContext = new Terminal.ErrorContext {
                        ErrorLevel    = ErrorLevels.ERROR,
                        ErrorType     = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message       = "No Open Container before Close Container.",
                        LineNumber    = LocalErrorReportLineNumber,
                        StepNumber    = LocalErrorReportStepNumber,
                        Context       = () => Terminal.ApplyWiggle(Representation, 0, LastNonWhiteSpaceIndex + 1)
                    };

                    return false;
                }

                if (ContainerBuffer[^1] != OpenOperator && ErrorContext.ErrorLevel == default) {
                    ErrorContext = new Terminal.ErrorContext {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = $"Invalid Container Closer '{Representation[ActiveToken.StringIndex]}' for Opening container '{Representation[LastOpenContainerOperatorStringIndex]}'.",
                        LineNumber = LocalErrorReportLineNumber,
                        StepNumber = LocalErrorReportStepNumber,
                        Context = () => Terminal.ApplyWiggle(Representation, LastOpenContainerOperatorStringIndex + 1, ActiveToken.StringIndex - LastOpenContainerOperatorStringIndex + 1)
                    };

                    return false;
                }

                Tokens.Add(new HierarchyTokens_t([], 0, string.Empty));
                ContainerBuffer.RemoveAt(ContainerBuffer.Count - 1);

                return true;
            }

            bool SimpleCloseContainer(Operators Operator) => CloseContainer(Operator, Operator - 1);
          
            (List<(string token, int StringIndex, int StringLength)> ctx, bool success) PartialResolveDefine(string Token) {
                List<(string token, int StringIndex, int StringLength)>    Resolved = [(Token, ActiveToken.StringIndex, Token.Length)];
                ObjectToken? ctx;
                bool                                                       success;
                bool                                                       HadSuccess = false;

                do {
                    ctx = Database.GetObjectFromAlias(Resolved[0].token);

                    if (ctx is not null && ctx.type == AssembleTimeTypes.EXP) {
                        HadSuccess = true;
                        Resolved.RemoveAt(0);
                        
                        Resolved.InsertRange(0, RegexTokenize((string)((Dictionary<string, (object data, AssembleTimeTypes type)>)ctx.data)[""].data)
                                                        .Select(token => (token, ActiveToken.StringIndex, token.Length))
                                                        .ToList());
                    }

                } while (ctx is not null && ctx.type == AssembleTimeTypes.EXP);

                return (Resolved, HadSuccess);
            }
            
            #endregion Lexer Functions
        }
    }
}
