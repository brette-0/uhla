using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Tataru {

    namespace Engine {
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

        static class Engine {
            /// <summary>
            /// Fetches context for the next step in decoding.
            /// Modified the Source File read Index for each accumulated context.
            /// Returns a list of string arrays for each split line of code.
            /// </summary>
            /// <param name="Source"></param>
            /// <param name="Index"></param>
            /// <returns></returns>
            internal static (List<string[]>?, ContextFetcherEnums Code) FetchContext(string[] Source, int Index) {
                List<string[]> Tokens           = [];
                int      StartingIndex          = Index;            // Beginning Line Number for Error Reports
                int      StringIndex            = 0;                // How far into the raw strings we are
                int      VerifiedStringIndex    = 0;                // Sum of all verified (thus far) steps
                int      BufferTaleStringIndex  = 0;                // Last Open Encapsulation
                string   AccumulatedContext     = Source[Index];    // Accumolated Context for Error Reporting

                string[] TokenizedBuffer        = [.. Tokenize(Source[Index])];
                
                char[]   ContainerBuffer        = new char[TokenizedBuffer.Length];
                int      BufferIndex;
                int      TokenizedCheckPoint    = 0;

                bool     HasSteps               = TokenizedBuffer.Contains(";");

                do {
                    BufferIndex = -1;
                    for (int i = 0; i < TokenizedBuffer.Length; StringIndex += TokenizedBuffer[i].Length, i++) {
                        switch (TokenizedBuffer[i][0]) {
                            case '(':
                                BufferIndex++;
                                ContainerBuffer[BufferIndex] = '(';
                                BufferTaleStringIndex = StringIndex;
                                continue;

                            case ')':
                                if (BufferIndex == -1 || ContainerBuffer[BufferIndex] != '(') {
                                    /*
                                     * May look like [1 + 2)    <-- invalid termination
                                     * Syntax Error : Unexpected Parenthesis (1, 2) :\n{line information}
                                     */
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unexpected Parenthesis",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                } else {
                                    BufferIndex--;
                                    continue;
                                }

                            case '\"':
                                if (ContainerBuffer[BufferIndex] == '\"') {
                                    ContainerBuffer[BufferIndex] = '\x00';  // clear to indicate closed string
                                    BufferIndex--;
                                    continue;
                                } else {
                                    BufferIndex++;
                                    ContainerBuffer[BufferIndex] = '\"';
                                    continue;
                                }

                            case '[':
                                BufferIndex++;
                                ContainerBuffer[BufferIndex] = '[';
                                BufferTaleStringIndex = StringIndex;
                                continue;

                            case ']':
                                if (BufferIndex == -1 || ContainerBuffer[BufferIndex] != '[') {
                                    /*
                                     * May look like {1 + 2]    <-- invalid termination
                                     * Syntax Error : Unexpected Bracket (1, 2) :\n{line information}
                                     */
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unexpected Bracket",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                } else {
                                    BufferIndex--;
                                    continue;
                                }

                            case '{':
                                BufferIndex++;
                                ContainerBuffer[BufferIndex] = '[';
                                BufferTaleStringIndex = StringIndex;
                                continue;

                            case '}':
                                if (BufferIndex == -1 || ContainerBuffer[BufferIndex] != '[') {
                                    /*
                                     * May look like (1 + 2}    <-- invalid termination
                                     * Syntax Error : Unexpected Brace (1, 2) :\n{line information}
                                     */
                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unexpected Brace",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, BufferTaleStringIndex + 1, StringIndex - BufferTaleStringIndex)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                } else {
                                    BufferIndex--;
                                    continue;
                                }

                            case ';':
                                if (BufferIndex == -1) {
                                    VerifiedStringIndex = StringIndex + 1;
                                    Tokens.Add(new string[i - TokenizedCheckPoint]);
                                    Array.Copy(TokenizedBuffer, TokenizedCheckPoint, Tokens[^1], 0, i - TokenizedCheckPoint);
                                    TokenizedCheckPoint = i + 1;
                                } else if (ContainerBuffer[BufferIndex] == '\"') break;
                                  else {
                                    /*
                                        * May look like (1 + 2;)    <-- invalid termination
                                        * Syntax Error : Unexpected Parenthesis (1, 2) :\n{line information}
                                        */

                                    Terminal.Error(
                                        ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Unexpected Line Termination",
                                        StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle(AccumulatedContext, StringIndex, 1)
                                    );
                                    return (null, ContextFetcherEnums.MALFORMED);
                                }
                                continue;
                            }
                    }

                    if (BufferIndex == -1) break;

                    // if no more context can be supplied, return unterminated and log error to user
                    if (++Index == Source.Length) {
                        Terminal.Error(
                            ErrorTypes.SyntaxError, DecodingPhase.TOKEN, "Could not Fetch Required Context",
                            StartingIndex, HasSteps ? Tokens.Count : null, ApplyWiggle($"{AccumulatedContext} ", StringIndex, 1)        
                        );
                        return (null, ContextFetcherEnums.UNTERMINATED);
                    }

                    TokenizedBuffer = [.. TokenizedBuffer.TakeLast(TokenizedBuffer.Length - TokenizedCheckPoint)];
                    TokenizedCheckPoint = 0;
                    StringIndex = VerifiedStringIndex;  // Reset for more accurate wiggling

                    AccumulatedContext += Source[Index];
                    TokenizedBuffer = [.. TokenizedBuffer, .. Tokenize(Source[Index])];

                    HasSteps |= TokenizedBuffer.Contains(";");

                } while (true);


                return (Tokens, ContextFetcherEnums.OK);
            }

            internal enum ErrorLevels {
                LOG, WARN, ERROR
            }

            internal enum ErrorTypes {
                SyntaxError
            }

            internal enum DecodingPhase {
                TOKEN
            }

            internal static class Terminal {
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int? StepNumber, string Context) {
                    Console.ForegroundColor = ErrorLevel switch {ErrorLevels.LOG => ConsoleColor.Cyan, ErrorLevels.WARN => ConsoleColor.Yellow, ErrorLevels.ERROR => ConsoleColor.Red, _ => ConsoleColor.White};
                    Console.WriteLine( ErrorType switch {
                                            ErrorTypes.SyntaxError => "Syntax Error",
                                            _                      => "Unknown Error"
                                   } + " During " + Phase switch {
                                            DecodingPhase.TOKEN => "Token Decoding",
                                            _                   => "Unknown"
                                   } + $" Phase : {Message} " + (StepNumber == null ? $"({LineNumber})" : $"({LineNumber}, {StepNumber})")+ $" : {Context}");
                    Console.ResetColor();
                }

                internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int? StepNumber, string Context) {
                    WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int? StepNumber, string Context) {
                    WriteInfo(ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int LineNumber, int? StepNumber, string Context) {
                    WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }
            }

            internal static partial class Error {
                public static void WriteError(string message) {
                    Console.WriteLine(ApplyWiggle(message, 0, message.Length)); // example usage
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
            public static List<string> Tokenize(string input) {
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