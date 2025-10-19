using System.Runtime.InteropServices;

namespace uhla.Core {
    internal static partial class Core {
        internal static EvalToken? Evaluate(Core.Modes mode = Core.Modes.STANDARD, List<HierarchyTokens_t>? Tokens = null, int MaxHierarchy = -1, Core.Statuses Status = Core.Statuses.OK) {
            if (Tokens is null) {
                var SourceFileIndexBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileIndexBuffer);
                var SourceFileLineBufferSpan  = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
                var SourceFileStepBufferSpan  = CollectionsMarshal.AsSpan(Program.SourceFileStepBuffer);


                (Tokens, MaxHierarchy, Status) = Core.Lexer(
                    Program.SourceFileContentBuffer[^1].ToArray(), 
                    ref SourceFileIndexBufferSpan[^1],
                    ref SourceFileLineBufferSpan[^1], 
                    ref SourceFileStepBufferSpan[^1], 
                    Program.SourceFileNameBuffer[^1], 
                    mode
                );
            }

            switch (Status, LexerMode: mode) {
                case (Core.Statuses.INIT_ANGORI, _):
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
                
                
                case (Core.Statuses.OK, _):
                    /*
                     *  Invokes Generic Evaluate, Yielding List<EvalToken>
                     *
                     *  If returns with a lack of definition, subscribe to definition manager
                     *  otherwise pass back to Assemble.
                     */

                    while (MaxHierarchy > 0) {
                        var Targets = Tokens.Where(t => t.Hierarchy == MaxHierarchy);
                        foreach (var Target in Targets) {
                            var (ctx, status) = LinearEvaluate(Target); switch (status) {
                                case EvaluationStatus.ERROR:            return null; // error pass back
                                case EvaluationStatus.OK:               break;
                                case EvaluationStatus.SYMBOL_UNDEFINED: return null;    // return definition issue

                                default:
                                    throw new ArgumentOutOfRangeException();
                            }

                            // inject ctx where appropriate
                            var Offset = Tokens.IndexOf(Target);

                            Tokens[Offset - 1].DeltaTokens[^1].Add((EvalToken)ctx.ctx);
                            Tokens[Offset - 1].DeltaTokens.AddRange(Tokens[Offset + 1].DeltaTokens);
                            
                            Tokens[Offset - 1] = new HierarchyTokens_t() {
                                DeltaTokens    = Tokens[Offset - 1].DeltaTokens,
                                Hierarchy      = Tokens[Offset - 1].Hierarchy,
                                Representation = Tokens[Offset - 1].Representation + Tokens[Offset].Representation + Tokens[Offset + 1].Representation
                            };

                            Tokens.RemoveAt(Offset);
                            Tokens.RemoveAt(Offset);    // remove at n, and n + 1
                        }

                        MaxHierarchy--;
                    }
                    
                    break;
                
                case (Core.Statuses.FAIL, _): return null;
            }

            return null;
            
            static (LE_Relationship ctx, EvaluationStatus status) LinearEvaluate(HierarchyTokens_t HierarchyTokens) {
                return default;
            }
        }

        /// <summary>
        /// LE must replace invocations with constant copy-of object result in-place, replace non-constant object
        /// references with temporary const byval clone. Use of undeclared objects are declared as const int and tested
        /// for compatibility for selected paths.
        /// </summary>
        /// <param name="Tokens"></param>
        /// <returns></returns>
        private static (LE_Relationship ctx, EvaluationStatus status) LinearEvaluate(HierarchyTokens_t Tokens) {
            return default;
        }

        private static EvalToken? DeltaEvaluate(List<HierarchyTokens_t>? Tokens, int MaxHierarchy, Core.Statuses Status) {
            while (MaxHierarchy > 0) {
                var Targets = Tokens.Where(t => t.Hierarchy == MaxHierarchy);
                foreach (var Target in Targets) {
                    var (ctx, status) = LinearEvaluate(Target); switch (status) {
                        case EvaluationStatus.ERROR:            return null; // error pass back
                        case EvaluationStatus.OK:               break;
                        case EvaluationStatus.SYMBOL_UNDEFINED: 
                            // accept 'reshape' return
                            return null;    // return definition issue

                        default:
                            throw new ArgumentOutOfRangeException();
                    }

                    // inject ctx where appropriate
                    var Offset = Tokens.IndexOf(Target);

                    Tokens[Offset - 1].DeltaTokens[^1].Add((EvalToken)ctx.ctx);
                    Tokens[Offset - 1].DeltaTokens.AddRange(Tokens[Offset + 1].DeltaTokens);
                            
                    Tokens[Offset - 1] = new HierarchyTokens_t() {
                        DeltaTokens = Tokens[Offset - 1].DeltaTokens,
                        Hierarchy   = Tokens[Offset - 1].Hierarchy,
                        Representation = Tokens[Offset - 1].Representation + Tokens[Offset].Representation + Tokens[Offset + 1].Representation
                    };

                    Tokens.RemoveAt(Offset);
                    Tokens.RemoveAt(Offset);    // remove at n, and n + 1
                }

                MaxHierarchy--;
            }

            return null;
        }
    }
    
    
    /*
     *
     * The Evaluation pipeline requires that:
     *  - tokens can invoke Delta Evaluate
     *  - DE requests Database.ProvideDefinition, and Database.Declare
     *  - LE reshapes to prevent evaluation latency based over-invocation
     *
     * 
     */
}

