using System.Runtime.InteropServices;
using UHLA.Engine;
using UHLA.InterfaceProtocol;

namespace Architectures {
    internal class NMOS_6502 : IArchitecture {
        public virtual bool    IsMnemonic(string mnemonic) {
            throw new NotImplementedException();
        }
        public virtual object? GatherAdditionalMnemonicContext() {
            throw new NotImplementedException();
        }
        public virtual int? TryCompleteInstruction(string mnemonic, ref List<EvalToken> args) {
            throw new NotImplementedException();
        }
        public virtual void Initalize() {
            Program.LabelDataBase["a"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"",         new ObjectToken(UHLA.Engine.System.Registers.A, AssembleTimeTypes.REG, AccessLevels.PRIVATE)},
                {"indexing", new ObjectToken(0,                                  AssembleTimeTypes.INT, AccessLevels.PUBLIC) }
            }, AssembleTimeTypes.REG, AccessLevels.PUBLIC);

            Program.LabelDataBase["x"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"",         new ObjectToken(UHLA.Engine.System.Registers.X, AssembleTimeTypes.INT, AccessLevels.PRIVATE)},
                {"indexing", new ObjectToken(0,                                  AssembleTimeTypes.INT, AccessLevels.PUBLIC) }
            }, AssembleTimeTypes.REG, AccessLevels.PUBLIC);

            Program.LabelDataBase["y"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"",         new ObjectToken(UHLA.Engine.System.Registers.Y, AssembleTimeTypes.INT, AccessLevels.PRIVATE)},
                {"indexing", new ObjectToken(0,                                  AssembleTimeTypes.INT, AccessLevels.PUBLIC) }
            }, AssembleTimeTypes.REG, AccessLevels.PUBLIC);
        }

        public virtual CheckDirectiveStatus CheckDirective(ref List<EvalToken> args,
                                                           ref List<(string token, int StringIndex, int StringLength)> pDefineResolveBuffer,
                                                           ref int pStringIndex, ref int pTokenIndex,
                                                           ref (string ctx, int StringIndex, int StringLength) pActiveToken,
                                                           ref string pRepresentation) {
            var       BasicRegexTokens          = new Memory<string>(Program.SourceFileContentBuffer[^1].ToArray());
            Span<int> SourceFileIndexBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileIndexBuffer);
            Span<int> SourceFileLineBufferSpan  = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
            Span<int> SourceFileStepBufferSpan  = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);

            var Representation = pRepresentation;
            var StringIndex    = pStringIndex;
            var TokenIndex     = pTokenIndex;
            var ActiveToken    = pActiveToken;

            var success             = CheckDirectiveStatus.None;
            var DefineResolveBuffer = new List<(string token, int StringIndex, int StringLength)>(pStringIndex);

            throw new NotImplementedException();
            return success;

            void Step(bool regexParse = true) {
                ActiveToken = default;
                if (DefineResolveBuffer.Count == 0) { // si to be mutated ONLY by typed tokens
                    Representation += BasicRegexTokens.Span[TokenIndex];
                    StringIndex    += BasicRegexTokens.Span[TokenIndex].Length;
                }

                if (regexParse) {
                    List<(string token, int StringIndex, int StringLength)> ctx;
                    bool                                                    success;
                    if (DefineResolveBuffer.Count == 0) {
                        (ctx, success)      = PartialResolveDefine(BasicRegexTokens.Span[TokenIndex++]);
                        DefineResolveBuffer = ctx;
                    } else {
                        (ctx, success) = PartialResolveDefine(DefineResolveBuffer[0].token);
                        DefineResolveBuffer.RemoveAt(0);
                        DefineResolveBuffer.InsertRange(0, ctx);
                    }

                    if (success) Step();
                    ActiveToken = DefineResolveBuffer[0];
                    DefineResolveBuffer.RemoveAt(0);
                } else
                    ActiveToken = (BasicRegexTokens.Span[TokenIndex++], StringIndex,
                        BasicRegexTokens.Span[TokenIndex - 1].Length);
            }

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

            (List<(string token, int StringIndex, int StringLength)> ctx, bool success)
                PartialResolveDefine(string Token) {


                List<(string token, int StringIndex, int StringLength)> Resolved =
                    [(Token, ActiveToken.StringIndex, Token.Length)];
                ObjectToken? ctx;
                bool         success;
                bool         HadSuccess = false;

                do {
                    ctx = Engine.Database.GetObjectFromAlias(Resolved[0].token, AccessLevels.PUBLIC);

                    if (ctx is not null && ctx.type == AssembleTimeTypes.EXP) {
                        HadSuccess = true;
                        Resolved.RemoveAt(0);

                        Resolved.InsertRange(0,
                            Engine.RegexTokenize(
                                       (string)((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels
                                           access)>)ctx.data)[""].data)
                                  .Select(token => (token, ActiveToken.StringIndex, token.Length))
                                  .ToList());
                    }

                } while (ctx is not null && ctx.type == AssembleTimeTypes.EXP);

                return (Resolved, HadSuccess);
            }

            bool CheckLineTerminated() => (TokenIndex == BasicRegexTokens.Length && DefineResolveBuffer.Count == 0) ||
                                          ActiveToken.ctx[0] == ';' || ActiveToken.ctx[0] == '\n' ||
                                          ActiveToken.ctx    == "//" || ActiveToken.ctx == "/*";

            // keep seeking beyond whitespace
            void seek_no_whitespace(bool skip = false, bool regexParse = true) => Steps(
                () => !CheckLineTerminated() && (ActiveToken.ctx[0] == ' ' || ActiveToken.ctx[0] == '\t'), skip,
                regexParse);
        }

        protected static List<bool> PragmaIllegalBuffer;
        protected static List<bool> PragmaCPUAwareBuffer;
        protected static List<bool> PragmaGPRAwareBuffer;

    }
}

