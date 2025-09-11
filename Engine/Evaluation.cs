using System.Runtime.InteropServices;

namespace Numinous.Engine {
    internal struct HierarchyTokens {
        internal HierarchyTokens(List<List<EvalToken>> pDeltaTokens, int pHierachy, string pRepresentation) {
            DeltaTokens    = pDeltaTokens;
            Hierarchy      = pHierachy;
            Representation = pRepresentation;
        }
    
    
        internal List<List<EvalToken>> DeltaTokens;
        internal int                   Hierarchy;
        internal string                Representation;
    }

    internal static partial class Engine {
        internal static ObjectToken? Evaluate(LexerModes LexerMode = LexerModes.STANDARD) {
            var SourceFileIndexBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileIndexBuffer);
            var SourceFileLineBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
            var SourceFileStepBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileStepBuffer);


            var (Tokens, MaxHierarchy, Status) = Lexer(
                Program.SourceFileContentBuffer[^1].ToArray(), 
                ref SourceFileIndexBufferSpan[^1],
                ref SourceFileLineBufferSpan[^1], 
                ref SourceFileStepBufferSpan[^1], 
                Program.SourceFileNameBuffer[^1], 
                LexerMode
            );

            switch (Status, LexerMode) {
                case (LexerStatuses.INIT_ANGORI, _):
                    /*
                     *  This can look like
                     *      foo:
                     *
                     *      1 + foo:
                     *      1 + (foo, bar)
                     * 
                     *  Will provide only one definition to as many declared found in one hierarchy.
                     *  Must have at least undefined token. Will Generate as CINT local to the active scope.
                     */
                    break;
                
                
                case (LexerStatuses.OK, _):
                    /*
                     *  Invokes Generic Evaluate, Yielding List<EvalToken>
                     *
                     *  If returns with a lack of definition, subscribe to definition manager
                     *  otherwise pass back to Assemble.
                     */
                    break;
                
                case (LexerStatuses.FAIL, _): return null;
            }

            return null;
        }
    }
}

