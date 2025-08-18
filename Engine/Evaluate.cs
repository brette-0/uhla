using static Numinous.Engine.Engine;

// YOU NEED TO WRITE RULES FOR LOADING INSTRUCTIONS
// REMEMBER WE STILL NEED THE CF TO BE COOL

namespace Numinous {
    namespace Engine {
        internal static partial class Engine {

            [Flags]
            internal enum 
                Directives : byte {
                PUSH_ILLEGAL,
                POP_ILLEGAL,

                PUSH_CPU,
                POP_CPU,

                PUSH_GPR,
                POP_GPR,

                PUSH_MEM,
                POP_MEM,
                
                INCLUDE,
                LOCAL_INCLUDE,
                INCLUDEBIN,
                LOCAL_INCLUDEBIN,
                
                ASSERT,
                
                CART,
                DISK,
                
                DEFINE,
                FUNCDEFINE,
                UNDEFINE,
                
                ROM,
                CPU,
                
                ERROR = byte.MaxValue
            }

            internal enum OperationTypes : byte {
                FAIL,
                DIRECTIVE,          // eg.. #include
                INSTRUCTION,        // eg.. lda foo
                EVALUATE,           // function, macros, RODATA writes
                KEYWORD,            // int foo = bar, return
                RUNTIME,            // u8 foo       : is a keyword BUT returns different ctx

                ANON_REL_BRANCH,    // +: and -:
            }

            /// <summary>
            /// default : error
            /// found   : not immediate, overruled or enforced
            /// if has Immediate, Enforced or Overruled it counts as found
            /// </summary>
            [Flags]
            internal enum OperandDecorators : byte {
                Found           = 1 << 0,   // something

                Immediate       = 1 << 1,   // #
                Enforced_ABS    = 1 << 2,   // a:
                Enforced_ZP     = 1 << 3,   // z:
                Overruled       = 1 << 4,   // !

                Complete        = 1 << 5,   // for when there is no information after an instruction that supports implied addressing
                Missing         = 1 << 6,   // does not count as a fail, just means CF for RODATA later
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

            
        }
    }
}
