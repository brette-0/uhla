using System.Runtime.InteropServices;

namespace uhla.Core {
    internal static partial class Core {
        internal static EvalToken? Evaluate(Modes mode = Modes.STANDARD, List<HierarchyTokens_t>? Tokens = null, int MaxHierarchy = -1, Statuses Status = Statuses.OK) {
            if (Tokens is null) {
                var SourceFileIndexBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileIndexBuffer);
                var SourceFileLineBufferSpan  = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
                var SourceFileStepBufferSpan  = CollectionsMarshal.AsSpan(Program.SourceFileStepBuffer);


                (Tokens, MaxHierarchy, Status) = Lexer(
                    Program.SourceFileContentBuffer[^1].ToArray(), 
                    ref SourceFileIndexBufferSpan[^1],
                    ref SourceFileLineBufferSpan[^1], 
                    ref SourceFileStepBufferSpan[^1], 
                    Program.SourceFileNameBuffer[^1], 
                    mode
                );
            }

            switch (Status, LexerMode: mode) {
                case (Statuses.INIT_ANGORI, _):
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
                
                
                case (Statuses.OK, _):
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
                
                case (Statuses.FAIL, _): return null;
            }

            return null;
            
            
            // TODO: consider how LE can be integrated into DE 
            static (LE_Relationship ctx, EvaluationStatus status) LinearEvaluate(HierarchyTokens_t HierarchyTokens) {
                foreach (var TermTokens in HierarchyTokens.DeltaTokens) {
                    var resp = TermEvaluate(TermTokens);
                }
                return default;
            }

            // total definition will return one member per term, whereas in the event of lazy definition
            // it will be a simplified formula
            static List<EvalToken> TermEvaluate(List<EvalToken> Tokens) {
                var             WantOperator          = false;
                List<Operators> PendingUnaryOperators = [];
                var             TokenEnumerator       = Tokens.GetEnumerator();
                List<EvalToken> ObjectStack           = [];
                List<Operators> OperatorStack         = [];
                
                
                List<EvalToken> Response = [];
                
                
                
                var i = 0; while (i < Tokens.Count) {
                    if (WantOperator) {

                    }

                    // collect object unary operators
                    while (TokenEnumerator.Current.IsOperator()) {
                        switch ((Operators)TokenEnumerator.Current.ObjectData) {
                            case Operators.INC:     // pre inc
                            case Operators.DEC:     // pre dec
                            case Operators.ADD:     // absolute (int)   | uppercase (string)
                            case Operators.SUB:     // negate   (int)   | lowercase (string)
                            case Operators.NOT:     // is zero  (int)   | clear     (string)
                            case Operators.BITNOT:  // bitwise negate   | length of (string)
                                PendingUnaryOperators.Add((Operators)TokenEnumerator.Current.ObjectData);
                                if (!TokenEnumerator.MoveNext()) {
                                    // error, collecting unary operators prior to object resulted in term completion
                                    // syntax error/nothing to do
                                    return [];
                                }
                                continue;
                            
                            default:
                                // error, this is not a unary operator
                                return [];
                        }
                    }

                    var member = Database.GetObjectFromAlias((string)TokenEnumerator.Current.ObjectData);

                    while (TokenEnumerator.MoveNext()) {
                        if (!TokenEnumerator.Current.IsOperator()) {
                            // Value following value is prohibited
                            return [];
                        }

                        // if operator is not member fetching, break iterative logic
                        if ((Operators)TokenEnumerator.Current.ObjectData is not (Operators.PROPERTY or Operators.NULLPROPERTY)) break;
                        
                        // access member
                        if (!TokenEnumerator.MoveNext()) {
                            // error, literally no member/property to view
                            return [];
                        }
                             
                        // null propagation used here, lazily defined constant comprised members cannot have
                        // evaluated properties at this point
                        member = member?.GetMember((string)TokenEnumerator.Current.ObjectData);
                    }

                    if (member is null) {
                        if (PendingUnaryOperators.Contains(Operators.INC) ||
                            PendingUnaryOperators.Contains(Operators.DEC)) {
                            // error, pre mut requesting mut on constant component
                            return [];
                        }
                        
                        // store pre muts
                    } else {
                        // compute pre muts
                        PendingUnaryOperators.Reverse();

                        if (member.type is not (AssembleTimeTypes.INT or AssembleTimeTypes.STRING)) {
                            // error, primitives only compatible with non property accessing members
                            return [];
                        }

                        var memnew = new ObjectToken(new Dictionary<string, ObjectToken>() {
                            {"#self", new ObjectToken(
                                member.type switch {
                                    AssembleTimeTypes.INT    => 0,
                                    AssembleTimeTypes.STRING => string.Empty
                                }, default
                             )}
                        }, member.type);

                        // process unaries 
                        while (PendingUnaryOperators.Count > 0) {
                            if (!ProcessUnaryOperator(PendingUnaryOperators[^1], ref memnew)) return [];
                            PendingUnaryOperators.RemoveAt(PendingUnaryOperators.Count - 1);
                        }

                        // member is managed, so maybe this should work, check if not
                        member = new ObjectToken(memnew);   // clone memnew to update member
                    }
                    
                    // perform non unary operations
                    
                    // post mut/dec tracking
                    if ((Operators)TokenEnumerator.Current.ObjectData is Operators.INC or Operators.DEC) {
                        if (member is null) {
                            // error, constants cannot mutate
                            return [];
                        }
                        if (!ProcessUnaryOperator((Operators)TokenEnumerator.Current.ObjectData, ref member)) return [];
                    } else if (member is null && ((Operators)TokenEnumerator.Current.ObjectData) == Operators.CHECK) {
                        // branching logic requires definition <due to mid-line mutation based on conditional scripting>
                        return [];
                    }
                    
                    WantOperator = !WantOperator;
                }
                return Response;

                bool ProcessUnaryOperator(Operators op, ref ObjectToken memnew) {
                    switch (op, memnew.type) {
                        case (Operators.ADD,    AssembleTimeTypes.INT):    memnew.GetMember("#self")!.data = +(int)memnew.GetMember("#self")!.data; break;
                        case (Operators.SUB,    AssembleTimeTypes.INT):    memnew.GetMember("#self")!.data = -(int)memnew.GetMember("#self")!.data; break;
                        case (Operators.NOT,    AssembleTimeTypes.INT):    memnew.GetMember("#self")!.data = 0 == (int)memnew.GetMember("#self")!.data ? 1 : 0; break;
                        case (Operators.BITNOT, AssembleTimeTypes.INT):    memnew.GetMember("#self")!.data = ~(int)memnew.GetMember("#self")!.data; break;
                        case (Operators.INC,    AssembleTimeTypes.INT):    
                            if (PendingUnaryOperators.Count == 1) memnew.GetMember("#self")!.data = 1 + (int)memnew.GetMember("#self")!.data;
                            else {
                                // error, only permits increment once and at the end
                                return false;
                            }
                            break;
                        case (Operators.DEC,    AssembleTimeTypes.INT):
                            if (PendingUnaryOperators.Count == 1) memnew.GetMember("#self")!.data = (int)memnew.GetMember("#self")!.data - 1;
                            else {
                                // error, only permits decrement once and at the end
                                return false;
                            }
                            break;
                        case (Operators.ADD,    AssembleTimeTypes.STRING): memnew.GetMember("#self")!.data = ((string)memnew.GetMember("#self")!.data).ToUpper(); break;
                        case (Operators.SUB,    AssembleTimeTypes.STRING): memnew.GetMember("#self")!.data = ((string)memnew.GetMember("#self")!.data).ToLower(); break;
                        case (Operators.NOT,    AssembleTimeTypes.STRING): memnew.GetMember("#self")!.data =  new string(' ', ((string)memnew.GetMember("#self")!.data).Length); break;
                        case (Operators.BITNOT, AssembleTimeTypes.STRING): memnew.GetMember("#self")!.data = ((string)memnew.GetMember("#self")!.data).Length;
                                                                           memnew.type = AssembleTimeTypes.STRING; break;
                        case (_,    AssembleTimeTypes.STRING): 
                            // error, inc/dec not possible on string
                            return false;
                    }

                    return true;
                }
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

        private static EvalToken? DeltaEvaluate(List<HierarchyTokens_t>? Tokens, int MaxHierarchy, Statuses Status) {
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

