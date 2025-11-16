using System.Runtime.InteropServices;
using uhla.Core;

namespace uhla.Architectures {
    internal class NMOS_6502 : IArchitecture {
        
        // TODO: The memory instructions for 6502 either don't care ever or ask for a 'map file' for memory
        
        public int MemoryReserve(ref RunTimeVariableType ctx) {
            throw new NotImplementedException();
        }
        public bool MemoryFree(ref    RunTimeVariableType ctx) {
            throw new NotImplementedException();
        }

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
            Program.LabelDataBase["a"] = new AssembleTimeObject(new Dictionary<string, AssembleTimeObject>() {
                {"#self",    new AssembleTimeObject('a', AssembleTimeTypes.REG, true, true)},
                {"indexing", new AssembleTimeObject(0,   AssembleTimeTypes.INT, true, true) }
            }, AssembleTimeTypes.REG, true, true);

            Program.LabelDataBase["x"] = new AssembleTimeObject(new Dictionary<string, AssembleTimeObject>() {
                {"#self",    new AssembleTimeObject('x', AssembleTimeTypes.INT, true, true)},
                {"indexing", new AssembleTimeObject(0,   AssembleTimeTypes.INT, true, true) }
            }, AssembleTimeTypes.REG, true, true);

            Program.LabelDataBase["y"] = new AssembleTimeObject(new Dictionary<string, AssembleTimeObject>() {
                {"#self",    new AssembleTimeObject('y', AssembleTimeTypes.INT, true, true)},
                {"indexing", new AssembleTimeObject(0,   AssembleTimeTypes.INT, true, true) }
            }, AssembleTimeTypes.REG, true, true);

            if (!Program.Linker!.Rules.ContainsKey("direct")) {
                // Error : must have direct page rule
            }
            
            if (!Program.Linker!.Rules.ContainsKey("system")) {
                // Error : must have system page rule
            }
            
            if (!Program.Linker!.Rules.ContainsKey("fast")) {
                // Error : must have fast page rule
            }
            
            if (!Program.Linker!.Rules.ContainsKey("slow")) {
                // Error : must have slow page rule
            }
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
            
            // add a 'set ram offset' and 'set rom offset' pragmas
            return success;

            void Step(bool regexParse = true) {
                ActiveToken = default;
                if (DefineResolveBuffer.Count == 0) { // is to be mutated ONLY by typed tokens
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
                AssembleTimeObject? ctx;
                bool         success;
                bool         HadSuccess = false;

                do {
                    ctx = Database.GetObjectFromAlias(Resolved[0].token);

                    if (ctx is not null && ctx.type == AssembleTimeTypes.EXP) {
                        HadSuccess = true;
                        Resolved.RemoveAt(0);

                        Resolved.InsertRange(0,
                            Core.Core.RegexTokenize(
                                       (string)((Dictionary<string, (object data, AssembleTimeTypes type)>)ctx.data)[""].data)
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

