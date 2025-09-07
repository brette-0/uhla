using System.Security.AccessControl;
using System.Text.RegularExpressions;
using Antlr4.Runtime;
/*  TODO: change lexer define processing to return if define was succesful, determines if tokenising yields a new si/sl
         its imperative that a line such as '1 + lang' does not become reported as '1 + English_UK'
         furthermore FEXP (functional defines) must report on their parameters not on the results -> garbelling possible
          
         it seems that any define may contain a semicolon and that is the extent that ctx will be reported?
         So '#define foo(bar) bar;' may truncate something like 'foo(2) + 3' into a RODATA write and that's not a syntax error.

    TODO: RegexTokenizedBuffer to store (ctx, index, length)
          On error report, refer to RegexParsed[0].ctx, index, length etc...
*/

using static Numinous.Memory;

namespace Numinous.Engine {
    internal static partial class Engine {
        
        /// <summary>
        /// The Lexer takes in the entire source file as byref array of string tokens from a simple regex system.
        /// It's sole purpose is to check on instruction terminations, which according to it are comprised of:
        ///     - semicolons outside of strings
        ///     - ending a line with a value where all containers are closed
        ///
        /// Because some systems may break the rules above, sometimes certain processes will NOT invoke the standard lexer.
        ///
        /// TODO: Treat ? and : as containers.
        ///
        ///     lda foo ? bar
        ///             : ash
        /// 
        /// </summary>
        /// <param name="BasicRegexTokens">The source information that has been regex parsed and split into string tokens.</param>
        /// <param name="SourceTokenIndex">The index of the token we being lexing first.</param>
        /// <param name="ErrorReportLineNumber">The line the token we are lexing first is on.</param>
        /// <param name="ErrorReportStepNumber">The count of instructions that have passed on this line + 1</param>
        /// <param name="SourceFilePath">The file path we obtained the source content from, for debugging information.</param>
        /// <returns></returns>
        internal static (List<(List<List<LexerToken>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, int Finish, bool Success, bool Continue) Lexer(Memory<string> BasicRegexTokens, ref int SourceTokenIndex, ref int ErrorReportLineNumber, ref int ErrorReportStepNumber, string SourceFilePath) {
            // use BasicRegexTokens => RegexTokens (ref, no cloning?) | Ensures we solve all new defines without mutating the original
            //List<string> RegexTokens = ResolveDefines(BasicRegexTokens);
            var CollectiveContext = "";

            List<(List<List<LexerToken>> DeltaTokens, int Hierachy, string Representation)> Tokens = [];
            List<List<LexerToken>> DeltaTokens = [];
            List<LexerToken> TermTokens = [];
            List<Operators> ContainerBuffer = [];

            List<(string token, int StringIndex, int StringLength)> DefineResolveBuffer = [];

            int LocalSourceTokenIndex = SourceTokenIndex, LocalErrorReportLineNumber = ErrorReportLineNumber, LocalErrorReportStepNumber = ErrorReportStepNumber;

            var Finish = -1;
            var MaxHierarchy = 0;
            var LastNonWhiteSpaceIndex = -1;
            var LastOpenContainerOperatorStringIndex = -1;

            var literalCstring = "";

            var IsLastOperator = false;

            Terminal.ErrorContext ErrorContext = new();

            int i = 0, StringIndex = 0, StringLength = 0; (string ctx, int StringIndex, int StringLength) ActiveToken;

            Step();

            for (; DefineResolveBuffer.Count > 0 && i < BasicRegexTokens.Length; Step()) {

                if (ActiveToken.ctx[0] == ' ' || ActiveToken.ctx[0] == '\t') continue;                          // do not tokenize whitespace

                if (ContainerBuffer.Count != 0 && ContainerBuffer[^1] == Operators.FSTRING) {
                    CaptureCSTRING(c => c == '"' || c == '{');
                    if (ActiveToken.ctx[0] != '{') {
                        if (ActiveToken.ctx[0] == '"') {
                            if (CloseContainer(Operators.STRING, Operators.FSTRING)) continue;
                            else return default;
                        }
                    }
                }

                // handle tokens
                switch (ActiveToken.ctx) {
                    case "//":
                        // marks the end of this tasks ctx
                        Steps(() => ActiveToken.ctx[0] != '\n');
                        break;

                    case "/*":
                        // marks the end of this tasks ctx
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
                            ErrorContext = new() {
                                ErrorLevel      = ErrorLevels.ERROR,
                                ErrorType       = ErrorTypes.SyntaxError,
                                DecodingPhase   = DecodingPhases.TOKEN,
                                Message         = "Unexpected end of command.",
                                LineNumber      = ErrorReportLineNumber,
                                StepNumber      = ErrorReportStepNumber,
                                Context         = () => ApplyWiggle(CollectiveContext, ActiveToken.StringIndex + 1, 1)
                            };
                        }

                        if (ErrorContext.ErrorLevel == ErrorLevels.ERROR) {
                            Terminal.Error(ErrorContext);
                            // report the error with the current context
                            return default;
                        }

                        CopyDeltaTokens();
                    
                        i                 = 0;
                        CollectiveContext = CollectiveContext[(ActiveToken.StringIndex + ActiveToken.StringLength)..];
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
                    case "?":   SimpleAddOperator(Operators.CHECK);         break;
                    case ":":   SimpleAddOperator(Operators.ELSE);          break;
                    case "!":   SimpleAddOperator(Operators.NOT);           break;

                    // special case
                    case "\"":
                        i++;
                        if (CaptureCSTRING(c => c == '"')) break;
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
                            Finish = ActiveToken.StringIndex;
                            CopyDeltaTermTokens();
                            CopyDeltaTokens();
                            SourceTokenIndex = LocalSourceTokenIndex; ErrorReportLineNumber = LocalErrorReportLineNumber; ErrorReportStepNumber = LocalErrorReportStepNumber;
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

                    case ")": if (SimpleCloseContainer(Operators.CPAREN)) break; else return default;
                    case "]": if (SimpleCloseContainer(Operators.CBRACK)) break; else return default;
                    case "}": if (SimpleCloseContainer(Operators.CBRACE)) break; else return default;

                    // Term Catching
                    case ",":
                        CopyDeltaTermTokens();
                        break;

                    default:
                        TermTokens.Add(new(
                            new ObjectToken( new Dictionary<string, ObjectToken> {
                                {string.Empty, new (ActiveToken.ctx, AssembleTimeTypes.EXP, AccessLevels.PUBLIC)}
                            } ,AssembleTimeTypes.EXP, AccessLevels.PUBLIC),              
                            ActiveToken.StringIndex,
                            ActiveToken.StringLength,
                            false
                        ));
                        LastNonWhiteSpaceIndex = TermTokens.Count - 1;
                        break;

                }
            }

            IsLastOperator = LastNonWhiteSpaceIndex == -1 ? IsLastOperator : TermTokens.Count != 0 && TermTokens[LastNonWhiteSpaceIndex].IsOperator;
            if (ContainerBuffer.Count == 0 && !IsLastOperator) {
                // final steps

                CopyDeltaTermTokens();
                CopyDeltaTokens();                                                                  // process final TermTokens (valid) to StepTokens end

                SourceTokenIndex = LocalSourceTokenIndex; ErrorReportLineNumber = LocalErrorReportLineNumber; ErrorReportStepNumber = LocalErrorReportStepNumber;
                return Success();
            }

            // this is permissible as context is full here - no need to involve ErrorContext
            Terminal.Error(ErrorTypes.SyntaxError, DecodingPhases.TOKEN, "Not enough context to satisfy request", ErrorReportLineNumber - 1, Tokens.Count, ApplyWiggle(CollectiveContext, CollectiveContext.Length - 1, 1), SourceFilePath);
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
                var FEXP_Queery = Database.GetObjectFromAlias(ActiveToken.ctx, Program.ActiveScopeBuffer[^1], AccessLevels.PUBLIC);
                if (FEXP_Queery is null || FEXP_Queery.type != AssembleTimeTypes.FEXP) return;

                // fetch object from DB and capture arguments to invoke it
                var FEXP_Obj = (Dictionary<string, ObjectToken>)FEXP_Queery.data;
                var args_capture = CaptureFEXP((int)FEXP_Obj["args"].data);

                // if capturing was unsuccessful, exit.
                if (!args_capture.success) return;

                // execute define
                var FEXP_Lambda = (Func<List<(string token, int StringIndex, int StringLength)>, List<(string token, int StringIndex, int StringLength)>>)FEXP_Obj[""].data;
                
                // store processed result in Lexer buffer
                DefineResolveBuffer.InsertRange(0, FEXP_Lambda(args_capture.args));
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
            
            (List<(List<List<LexerToken>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, int Finish, bool Success, bool Continue) Success() => (Tokens, MaxHierarchy, Finish, true, false);
            
            void Step(bool regexParse = true) {
                ActiveToken = default;
                List<(string token, int StringIndex, int StringLength)> ctx;
                bool success;

                if (DefineResolveBuffer.Count == 0) {                                                   // si to be mutated ONLY by typed tokens
                    CollectiveContext += BasicRegexTokens.Span[i];
                    StringIndex += BasicRegexTokens.Span[i].Length;      
                    StringLength = BasicRegexTokens.Span[i].Length;
                }

                if (regexParse) {
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
                    ErrorContext = new() {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = "Unterminated String",
                        LineNumber = LocalErrorReportLineNumber,
                        StepNumber = LocalErrorReportStepNumber,
                        Context = () => ApplyWiggle(CollectiveContext, csi + 1, ActiveToken.StringIndex - csi)
                    };
                    return false;
                }

                if (csi != ActiveToken.StringIndex) {
                    TermTokens.Add(new(
                  new Dictionary<string, ObjectToken>() {
                               {"self",    new(literalCstring,           AssembleTimeTypes.CSTRING,  AccessLevels.PRIVATE) },
                               {"length",  new(literalCstring.Length,    AssembleTimeTypes.CINT,     AccessLevels.PUBLIC) },
                        },
                        csi,
                        csi - ActiveToken.StringIndex,
                        false
                    ));

                    literalCstring = "";   // wipe string for next capture
                }
                return true;
            }

            void AddOperator(Operators Operator, int sl) {
                TermTokens.Add(new(Operator, ActiveToken.StringIndex, sl, true)); LastNonWhiteSpaceIndex = TermTokens.Count - 1; ;
            }

            void SimpleAddOperator(Operators Operator) => AddOperator(Operator, 1);

            void ComplexOpenContainer(int sl, Operators Operator) {
                CopyDeltaTokens();
                ContainerBuffer.Add(Operator);                                  // register container type

                AddOperator(Operator, sl);
                LastOpenContainerOperatorStringIndex = ActiveToken.StringIndex;
                MaxHierarchy = Math.Max(ContainerBuffer.Count, MaxHierarchy);
            }

            void OpenContainer(Operators Operator) => ComplexOpenContainer(1, Operator);

            bool CloseContainer(Operators CloseOperator, Operators OpenOperator) {
                SimpleAddOperator(CloseOperator);

                if (ContainerBuffer.Count == 0 && ErrorContext.ErrorLevel == default) {
                    ErrorContext = new() {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = "No Open Container before Close Container.",
                        LineNumber = LocalErrorReportLineNumber,
                        StepNumber = LocalErrorReportStepNumber,
                        Context = () => ApplyWiggle(CollectiveContext, 0, LastNonWhiteSpaceIndex + 1)
                    };

                    return false;
                }

                if (ContainerBuffer[^1] != OpenOperator && ErrorContext.ErrorLevel == default) {
                    ErrorContext = new() {
                        ErrorLevel = ErrorLevels.ERROR,
                        ErrorType = ErrorTypes.SyntaxError,
                        DecodingPhase = DecodingPhases.TOKEN,
                        Message = $"Invalid Container Closer '{CollectiveContext[ActiveToken.StringIndex]}' for Opening container '{CollectiveContext[LastOpenContainerOperatorStringIndex]}'.",
                        LineNumber = LocalErrorReportLineNumber,
                        StepNumber = LocalErrorReportStepNumber,
                        Context = () => ApplyWiggle(CollectiveContext, LastOpenContainerOperatorStringIndex + 1, ActiveToken.StringIndex - LastOpenContainerOperatorStringIndex + 1)
                    };

                    return false;
                }

                CopyDeltaTokens();
                ContainerBuffer.RemoveAt(ContainerBuffer.Count - 1);

                return true;
            }

            bool SimpleCloseContainer(Operators Operator) => CloseContainer(Operator, Operator - 1);

            void CopyDeltaTokens() {
                CopyDeltaTermTokens();
                var DeltaTokensShallowCopy = DeltaTokens.Select(t => t).ToList();
                Tokens.Add((DeltaTokensShallowCopy, ContainerBuffer.Count, CollectiveContext));
            }

            void CopyDeltaTermTokens() {
                if (LastNonWhiteSpaceIndex == -1) return;   // Do not copy whitespace

                // Clone Delta Tokens thus far
                var StepDeltaTokenShallowCopy = TermTokens
                .Select(t => new LexerToken(
                    t.isOperator
                        ? t.data
                        : Clone((Dictionary<string, (object, AssembleTimeTypes, AccessLevels)>)t.data),
                    t.StringIndex,
                    t.StringLength,
                    t.IsOperator
                )).ToList();

                TermTokens = [];                       // wipe delta tokens for next operation
                DeltaTokens.Add(StepDeltaTokenShallowCopy);
            }
          
            (List<(string token, int StringIndex, int StringLength)> ctx, bool success) PartialResolveDefine(string Token) {
                List<(string token, int StringIndex, int StringLength)>    Resolved = [(Token, ActiveToken.StringIndex, Token.Length)];
                ObjectToken? ctx;
                bool                                                       success;
                bool                                                       HadSuccess = false;

                do {
                    ctx = Database.GetObjectFromAlias(Resolved[0].token, AccessLevels.PUBLIC);

                    if (ctx is not null && ctx.type == AssembleTimeTypes.CEXP) {
                        HadSuccess = true;
                        Resolved.RemoveAt(0);
                        
                        Resolved.InsertRange(0, RegexTokenize((string)((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)ctx.data)[""].data)
                                                .Select(token => (token, ActiveToken.StringIndex, token.Length))
                                                .ToList());
                    }

                } while (ctx is not null && ctx.type == AssembleTimeTypes.CEXP);

                return (Resolved, HadSuccess);
            }
            
            #endregion Lexer Functions
        }
    }
}
