using System.Runtime.InteropServices.ComTypes;
using Antlr4.Runtime.Atn;
using static Numinous.Engine.Engine;

// YOU NEED TO WRITE RULES FOR LOADING INSTRUCTIONS
// REMEMBER WE STILL NEED THE CF TO BE COOL

namespace Numinous {
    namespace Engine {
        internal static partial class Engine {


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
            
            /// <summary>
            /// 
            /// 
            /// </summary>
            /// <param name="LinearTokens">Tokens that live between deltas in hierarchy when lexing.</param>
            /// <returns></returns>

            internal static (List<(int StringOffset, int StringLength, (object data, AssembleTimeTypes type, AccessLevels level) data, bool IsOperator)> result, bool Success, bool Unevaluable) LinearEvaluate(List<(int StringOffset, int StringLength, object data, bool IsOperator)> LinearTokens) {
                List<Operators>                                                          ValueMutators    = [];
                List<Operators>                                                          OperatorBuffer   = [];
                
                List<(int StringOffset, int StringLength, object data, bool IsOperator)> ValueTokenBuffer = [];

                List<(int StringOffset, int StringLength, (object data, AssembleTimeTypes type, AccessLevels level) data, bool IsOperator)> ResultTermTokens = [];

                Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>? TargetScope = Program.ActiveScopeBuffer[^1];
                
                var  LinearTokenIndex = 0;

                var ResolveOperationIndex = ^2;

                // process the start here as there is no need to perform a check until we have 2 operators in the buffer.
                bool Terminate; var (Success, Unevaluable) = ProcessValue();
                if (!Success) return Unevaluable ? ([], false, true) : default;    // error pass back 

                List<bool> SkipBuffer = [];
                
                // each access is (Operator, Value)
                LinearTokenIndex++; while (LinearTokenIndex < LinearTokens.Count) {
                    var        CheckCount       = 0;
                    while (SkipBuffer.Count > 0 && !SkipBuffer[^1]) {
                        LinearTokenIndex += 2;  // ensure there is information at space lti -1, but we can accept no info at lti
                        if (!LinearTokens[LinearTokenIndex].IsOperator) {
                            // error malformed
                            return default;
                        }

                        if      ((Operators)LinearTokens[LinearTokenIndex].data == Operators.CHECK) CheckCount++;
                        else if ((Operators)LinearTokens[LinearTokenIndex].data == Operators.ELSE && --CheckCount == 0)
                            SkipBuffer.RemoveAt(SkipBuffer.Count - 1);
                    }
                    
                    (Success, Terminate)                = ProcessOperator(); LinearTokenIndex++;
                    if (!Success) {
                        if (!Terminate) return default; // error pass back 
                        
                        ResolveOperationIndex = ^1;     // clean out all tokens
                        while (OperatorBuffer.Count > 0) ResolveOperation();
                        ResolveOperationIndex = ^2;     // reset checker
                        continue;
                    }

                    if (!LinearTokens[LinearTokenIndex].IsOperator) {
                        // error operator malformed
                    }

                    if ((Operators)LinearTokens[LinearTokenIndex].data == Operators.CHECK) {
                        // evaluate the buffer

                        var CheckValue = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> data, AssembleTimeTypes type, AccessLevels access))ValueTokenBuffer[^1].data;
                        bool result;
                        
                        switch (CheckValue.type) {
                            case AssembleTimeTypes.INT: 
                            case AssembleTimeTypes.CINT:    result = (int)CheckValue.data[""].data == 0; break;
                            
                            case AssembleTimeTypes.STRING:
                            case AssembleTimeTypes.CSTRING: result = ((string)CheckValue.data[""].data).Length == 0; break;
                            
                            default:
                                // error: ternary cannot use type
                                return default;
                        }

                        SkipBuffer.Add(!result);
                    } else if ((Operators)LinearTokens[LinearTokenIndex].data == Operators.ELSE) {
                        // evaluate the buffer
                        ResolveOperationIndex = ^1;     // clean out all tokens
                        while (OperatorBuffer.Count > 0) ResolveOperation();
                        ResolveOperationIndex = ^2;     // reset checker
                        
                        SkipBuffer[^1]        = true;
                    } else if ((Operators)LinearTokens[LinearTokenIndex].data is Operators.TERM) {
                        // evaluate the buffer
                        ResolveOperationIndex = ^1;     // clean out all tokens
                        while (OperatorBuffer.Count > 0) ResolveOperation();
                        ResolveOperationIndex = ^2;     // reset checker
                        
                        ResultTermTokens.Add(((int StringOffset, int StringLength, (object data, AssembleTimeTypes type, AccessLevels level) data, bool IsOperator))ValueTokenBuffer[0]);
                    }
                    
                    (Success, Unevaluable)               = ProcessValue();   LinearTokenIndex++;
                    if (!Success) return Unevaluable ? ([], false, true) : default; // error pass back 
                    
                    
                    // if the top element has lower priority than second from top, resolve second from top.
                    while (OperatorBuffer.Count > 1 && GetHierarchy(OperatorBuffer[^1]) > GetHierarchy(OperatorBuffer[^2])) {
                        // solve ^2 until the above is false.
                        ResolveOperation();
                    }
                }
                
                // process last task (2 values, 1 operator) | invoke ProcessOperation for final elements
                ResolveOperationIndex = ^1;
                ResolveOperation();

                return (ResultTermTokens, true, false);
                

                // foo oper bar | first/second value order DOES NOT MATTER
                bool ResolveOperation() {
                    // the new value at ValueTokenBuffer[ResolveOperationIndex] is the new output variable.
                    
                    // The modifier should store this as an object entry, data would contain its 'self' value but its type is contained on its object level
                    var Modifier = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)ValueTokenBuffer[ResolveOperationIndex].data);
                    ValueTokenBuffer.RemoveAt(ResolveOperationIndex.GetOffset(ValueTokenBuffer.Count));
                    var Output = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)ValueTokenBuffer[ResolveOperationIndex].data);

                    if (OperatorBuffer[ResolveOperationIndex] is Operators.SET or Operators.INCREASE or Operators.DECREASE
                                                         or Operators.MULTIPLY or Operators.DIVIDE
                                                         or Operators.MODULATE or Operators.ASSIGNMASK
                                                         or Operators.ASSIGNFLIP or Operators.ASSIGNSET
                                                         or Operators.RIGHTSET or Operators.LEFTSET
                                                         or Operators.NULLSET) {
                        // ensure type compatibility for setting (polarity matters)
                    }
                    
                    if (OperatorBuffer[ResolveOperationIndex] is Operators.PROPERTY or Operators.NULLPROPERTY) {
                        // member getting is always a possible operation, given the member exists.
                    }

                    // for each type, switch case for operation. | won't ever assign. nor get member
                    // math with anything with a location uses the offset of that member. Bank, Proc, Inter, RT
                    switch (Modifier[""].type, Output[""].type) {
                        case (AssembleTimeTypes.INT, AssembleTimeTypes.INT):
                        case (AssembleTimeTypes.CINT, AssembleTimeTypes.INT):
                        case (AssembleTimeTypes.INT, AssembleTimeTypes.CINT):
                        case (AssembleTimeTypes.CINT, AssembleTimeTypes.CINT):
                            switch (OperatorBuffer[ResolveOperationIndex]) {
                                case Operators.ADD:
                                    Output[""] = (
                                       data:    (int)(Output[""].data) + (int)(Modifier[""].data),
                                       type:    AssembleTimeTypes.CINT,
                                       access: Output[""].access
                                    ); 
                                    break;
                                
                                // polarity matters
                                case Operators.SUB:
                                    Output[""] = (
                                        data:    (int)(Output[""].data) - (int)(Modifier[""].data),
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.MULT:
                                    Output[""] = (
                                        data:    (int)(Output[""].data) * (int)(Modifier[""].data),
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                // polarity matters here on
                                case Operators.DIV:
                                    Output[""] = (
                                        data:    (int)(Output[""].data) / (int)(Modifier[""].data),
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.MOD:
                                    Output[""] = (
                                        data:    (int)(Output[""].data) % (int)(Modifier[""].data),
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.RIGHT:
                                    Output[""] = (
                                        data:    (int)Output[""].data >>> (int)Modifier[""].data,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.LEFT:
                                    Output[""] = (
                                        data:    (int)Output[""].data << (int)Modifier[""].data,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.EQUAL:
                                    Output[""] = (
                                        data:    Output[""].data == Modifier[""].data ? 1 : 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.INEQUAL:
                                    Output[""] = (
                                        data:    Output[""].data != Modifier[""].data ? 1 : 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.NULL:
                                    Output[""] = (
                                        data:    Output[""].data ?? Modifier[""].data,  // TODO: Look into why resharper thinks ?? will never trigger.
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.GT:
                                    Output[""] = (
                                        data:    (int)Output[""].data > (int)Modifier[""].data ? 1 : 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.LT:
                                    Output[""] = (
                                        data:    (int)Output[""].data < (int)Modifier[""].data ? 1 : 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.GOET:
                                    Output[""] = (
                                        data:    (int)Output[""].data >= (int)Modifier[""].data ? 1 : 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.LOET:
                                    Output[""] = (
                                        data:    (int)Output[""].data <= (int)Modifier[""].data ? 1 : 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.SERIAL:
                                    Output[""] = (
                                        data:    Output[""].data == Modifier[""].data ? 0 : (int)Output[""].data < (int)Modifier[""].data ? -1 : 1,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.BITMASK:
                                    Output[""] = (
                                        data:    (int)Output[""].data & (int)Modifier[""].data,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.BITSET:
                                    Output[""] = (
                                        data:    (int)Output[""].data | (int)Modifier[""].data,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.BITFLIP:
                                    Output[""] = (
                                        data:    (int)Output[""].data ^ (int)Modifier[""].data,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.OR:
                                    Output[""] = (
                                        data:    (int)Output[""].data > 0 || (int)Modifier[""].data > 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                case Operators.AND:
                                    Output[""] = (
                                        data:    (int)Output[""].data > 0 && (int)Modifier[""].data > 0,
                                        type:    AssembleTimeTypes.CINT,
                                        access: Output[""].access
                                    ); 
                                    break;
                                
                                
                                default:
                                    // error operator isn't supported between types int and int
                                    return false;
                                
                            }
                            // yields CINT
                            break;
                        
                        case (AssembleTimeTypes.STRING, AssembleTimeTypes.STRING):
                        case (AssembleTimeTypes.CSTRING, AssembleTimeTypes.STRING):
                        case (AssembleTimeTypes.STRING, AssembleTimeTypes.CSTRING):
                        case (AssembleTimeTypes.CSTRING, AssembleTimeTypes.CSTRING):
                            if (OperatorBuffer[ResolveOperationIndex] == Operators.ADD)
                                Output[""] = (
                                    data: (string)Output[""].data + (string)Modifier[""].data,
                                    type: AssembleTimeTypes.CINT,
                                    access: Output[""].access
                                );
                            else
                                // error operator isn't supported between types string and string
                                return false;

                            // yields CSTRING
                            break;
                        
//                        case (AssembleTimeTypes.CHARMAP, AssembleTimeTypes.CHARMAP):                  // TODO: Later version
//                            // per element, process remap as X[n] oper Y[n] for each value of n.
//                            break;
                        
                        case (AssembleTimeTypes.CHARMAP, AssembleTimeTypes.STRING):
                        case (AssembleTimeTypes.STRING, AssembleTimeTypes.CHARMAP):
                        case (AssembleTimeTypes.CHARMAP, AssembleTimeTypes.CSTRING):
                        case (AssembleTimeTypes.CSTRING, AssembleTimeTypes.CHARMAP):
                            // yields formatted char array
                            break;
                            
                        case (AssembleTimeTypes.ICRWN, AssembleTimeTypes.INT):
                        case (AssembleTimeTypes.ICRWN, AssembleTimeTypes.CINT):
                        case (AssembleTimeTypes.IRWN, AssembleTimeTypes.INT):
                        case (AssembleTimeTypes.IRWN, AssembleTimeTypes.CINT):
                        case (AssembleTimeTypes.CREG, AssembleTimeTypes.INT):
                        case (AssembleTimeTypes.CREG, AssembleTimeTypes.CINT):
                        case (AssembleTimeTypes.REG, AssembleTimeTypes.INT):
                        case (AssembleTimeTypes.REG, AssembleTimeTypes.CINT):
                            Output[""] = (
                                data: 0,
                                type: AssembleTimeTypes.CINT,
                                access: Output[""].access
                            );

                            switch (OperatorBuffer[ResolveOperationIndex]) {
                                case Operators.ADD:
                                    Output["coefficient"] = (
                                        data: (int)Modifier[""].data,
                                        type: AssembleTimeTypes.CINT,
                                        access: AccessLevels.PRIVATE
                                    );
                                    
                                    break;
                                
                                case Operators.SUB:
                                    Output["coefficient"] = (
                                        data: -(int)Modifier[""].data,
                                        type: AssembleTimeTypes.CREG,
                                        access: AccessLevels.PRIVATE
                                    );
                                    
                                    break;

                                default:
                                    // error : registers may be only added or subtracted to by a constant
                                    return false;
                            }
                            
                            Output["register"] = (
                                data: (System.Registers)Output[""].data,
                                type: AssembleTimeTypes.CREG,
                                access: AccessLevels.PRIVATE
                            );
                            
                            // yields constant indexing register with constant (IRWC) X, Y or R
                            break;
                        
                        case (AssembleTimeTypes.BANK,   AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.BANK):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.CBANK):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.PROC):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.CPROC):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.INTER):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.CINTER):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.MACRO):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.RT):
                        case (AssembleTimeTypes.CBANK,  AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.PROC,   AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.CPROC,  AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.INTER,  AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.CINTER, AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.MACRO,  AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.RT,     AssembleTimeTypes.CRT):
                        case (AssembleTimeTypes.CRT,    AssembleTimeTypes.CRT):
                            Output = new() {
                                {"", ((int)Modifier["offset"].data + (int)Output["offset"].data, AssembleTimeTypes.CINT, AccessLevels.PUBLIC)}
                            };
                            
                            break;
                        
                        default:
                            // error : cannot use types against other type
                            return false;
                    }
                    
                    
                    // We perform the operation as described in the parameters, removing data afterward. 
                    return false;
                }

                (bool success, bool unevaluable) ProcessValue() {
                    ValueMutators = [];
                    var Modified = false;
                    while (LinearTokens[LinearTokenIndex].IsOperator) {
                        if ((Operators)LinearTokens[LinearTokenIndex].data is not (Operators.INC or Operators.DEC or Operators.ADD or Operators.SUB or Operators.BITNOT or Operators.NOT)) {
                            // error, invalid value modifier
                            return default;
                        } else if (Modified && (Operators)LinearTokens[LinearTokenIndex].data is Operators.INC or Operators.DEC) {
                            // error, double unary assignment
                            return default;
                        }

                        ValueMutators.Add((Operators)LinearTokens[LinearTokenIndex].data);
                    }
                    
                    // template latest buffer entry with token form
                    if (GetOperatorPrecedence((string)LinearTokens[LinearTokenIndex].data) > -1) {
                        // error, expected a value
                        return default;
                    }
                    
                    // push token into processor
                    ValueTokenBuffer.Add(LinearTokens[LinearTokenIndex]);
                    
                    var resp = GetObjectFromAlias((string)ValueTokenBuffer[LinearTokenIndex].data, Program.ActiveScopeBuffer[^1], AccessLevels.PUBLIC);
                    if (!resp.success) {
                        return (false, true);   // resulting value to be marked as Unevaluable
                    }

                    while (ValueMutators.Count > 0) if (!ApplyPreMutation()) return default; // error pass back, process value mutators from back to front

                    // New gen tokens must contain object data.
                    ValueTokenBuffer[LinearTokenIndex] = (
                        ValueTokenBuffer[LinearTokenIndex].StringOffset,
                        ValueTokenBuffer[LinearTokenIndex].StringLength,
                        resp.ctx,                                   // inject object reference
                        ValueTokenBuffer[LinearTokenIndex].IsOperator
                    );
                    
                    // scan for post mut, looks like ++, --, INDEX, or CALL
                    LinearTokenIndex++;
                    if (LinearTokens[LinearTokenIndex].IsOperator) {
                        var LastValue = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>) ValueTokenBuffer[^1].data);
                        if (LastValue[""].type is not (AssembleTimeTypes.INT or AssembleTimeTypes.CINT)) {
                            switch ((Operators)LinearTokens[LinearTokenIndex].data) {
                                case Operators.INC:
                                case  Operators.DEC:
                                    
                                    
                                default:    // operator is intended for next operation
                                    break;
                            } 
                        }
                    } else {
                        var CurrentValue = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>) LinearTokens[LinearTokenIndex].data);
                        var LastValue = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>) ValueTokenBuffer[^1].data);
                        
                        if (CurrentValue[""].type is AssembleTimeTypes.INDEX) {
                            /*
                             *  int/cint : cpu space
                             *                  indexed with int, reg, ir
                             *                  lda 2[2], lda 2[x], lda 2[x + 2]
                             * 
                             *  rt       : endian based index (like CS index type)
                             *                  lda foo[2]
                             *                  lda foo[x]      :: do not support for big endian!
                             *                  lda foo[2 + x]  :: do not support for big endian!
                             *  string   : char
                             *      "phrase"[2]
                             *
                             *  we do support the following:
                             *  2[2][2] => 6
                             *  2[2][x] => 4[x]
                             *  2[x][2] => 4[x]
                             *  2[x + 2][2] = 4[x]
                             *  2[x][x] => error, no double x memory address mode
                             *  2[x][y] => error, cannot index by both x and y
                             * 
                             *  array type : element    NOT IMPLEMENTED YET
                             */

                            switch (LastValue[""].type) {
                                case  AssembleTimeTypes.RT:
                                case AssembleTimeTypes.CRT:
                                    RunTimeVariableType tRT       =  (RunTimeVariableType)LastValue[""].data;
                                    int                 offset;
                                    switch (CurrentValue[""].type) {
                                        case AssembleTimeTypes.INT:
                                        case AssembleTimeTypes.CINT:
                                            offset = (int)LastValue["offset"].data + (tRT.endian ? (int)CurrentValue[""].data : (int)(tRT.size - (int)CurrentValue[""].data));
                                            break;
                                        
                                        case AssembleTimeTypes.REG:
                                        case AssembleTimeTypes.CREG:
                                            // new irwn/icrwn combination?
                                        
                                        case AssembleTimeTypes.IRWN:
                                        case AssembleTimeTypes.ICRWN:
                                            // apply immediate to offset based on endian, apply register. New ir objet
                                            break;
                                        
                                        default:
                                            // error cannot index with type {type}
                                            return default;
                                    } 
                                    
                                    // create cint literal in place
                                    break;
                                    
                                case AssembleTimeTypes.STRING:
                                case AssembleTimeTypes.CSTRING:
                                    // int type of the char at index specified
                                    break;
                                
                                default:
                                    // error type does not support index
                                    return default;
                            }
                        } else if (CurrentValue[""].type is AssembleTimeTypes.CALL) {
                            // load macro into source buffer
                            // recurse into invocation
                            // create new constant object in place of LastValue
                        }
                    }
                    
                    
                    return (true, false);

                    bool ApplyPreMutation() {
                        var LastValue = ((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels level)>) ValueTokenBuffer[^1].data);
                        switch (LastValue[""].type, ValueMutators[^1]) {

                            case (AssembleTimeTypes.INT, Operators.ADD):
                            case (AssembleTimeTypes.CINT, Operators.ADD): // abs
                                LastValue[""] = new (){
                                    data  = Math.Abs((int)LastValue[""].data),
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                            
                                break;
                            
                            // inc/dec does NOT support constant object references for obvious reasons
                            case (AssembleTimeTypes.INT, Operators.INC): // inc
                                LastValue[""] = new (){
                                    data  = Math.Abs((int)LastValue[""].data + 1),
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                            
                                break;
                            
                            case (AssembleTimeTypes.INT, Operators.DEC): // dec
                                LastValue[""] = new (){
                                    data  = Math.Abs((int)LastValue[""].data - 1),
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                            
                                break;
                            
                            case (AssembleTimeTypes.INT, Operators.SUB):
                            case (AssembleTimeTypes.CINT, Operators.SUB):   // neg
                                LastValue[""] = new (){
                                    data  = -(int)LastValue[""].data,
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;

                            case (AssembleTimeTypes.INT, Operators.NOT):
                            case (AssembleTimeTypes.CINT, Operators.NOT):   // == 0
                                LastValue[""] = new (){
                                    data  = (0 == (int)LastValue[""].data) ? 1 : 0,
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;

                            case (AssembleTimeTypes.INT, Operators.BITNOT):
                            case (AssembleTimeTypes.CINT, Operators.BITNOT):// ^= (uint)-1
                                LastValue[""] = new (){
                                    data  = ~(int)LastValue[""].data,
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;

                            case (AssembleTimeTypes.STRING, Operators.ADD):
                            case (AssembleTimeTypes.CSTRING, Operators.ADD): // upper case
                                LastValue[""] = new (){
                                    data  = ((string)LastValue[""].data).ToUpper(),
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;
                            
                            case (AssembleTimeTypes.STRING, Operators.SUB):
                            case (AssembleTimeTypes.CSTRING, Operators.SUB): // lower case
                                LastValue[""] = new (){
                                    data  = ((string)LastValue[""].data).ToLower(),
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;

                                
                            case (AssembleTimeTypes.STRING, Operators.NOT):
                            case (AssembleTimeTypes.CSTRING, Operators.NOT): // turn into spaces
                                LastValue[""] = new (){
                                    data  = new string(' ', ((string)LastValue[""].data).Length),
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;

                            case (AssembleTimeTypes.STRING, Operators.BITNOT):
                            case (AssembleTimeTypes.CSTRING, Operators.BITNOT):  // length of
                                LastValue[""] = new (){
                                    data  = ((string)LastValue[""].data).Length,
                                    type  = LastValue[""].type,
                                    level = LastValue[""].level
                                };
                                break;
                            
                            default:
                                // error : cannot apply unary to this
                                return false;
                        }

                        return true;
                    }
                }
                
                (bool Success, bool Terminate) ProcessOperator() {
                    if (!LinearTokens[LinearTokenIndex].IsOperator) {
                        // error not an operator
                        return default;
                    }

                    if ((Operators)LinearTokens[LinearTokenIndex].data is Operators.INC or Operators.DEC or Operators.NOT or Operators.BITNOT) {
                        // error this operator cannot be used that way
                        return default;
                    } 
                    
                    OperatorBuffer.Add((Operators)LinearTokens[LinearTokenIndex].data);
                    return (true, false);
                }


                // Only for Operations between values, value modifiers like ++, --, +, -, ! and ~ are ackwnoledged on the value level.
                int GetOperatorPrecedence(string Operator) => Operator switch {
                    "*" or "/" or "%"  => 0,
                    "+" or "-" => 1,
                    "<<" or ">>" => 2,
                    "<" or "<=" or ">" or ">=" or "<=>" => 3,
                    "==" or "!=" => 4,
                    "&" => 5,
                    "^" => 6,
                    "|" => 7,
                    "&&" => 8,
                    "||" => 9,
                    "??" => 10,
                    "?" or ":" => 11,
                    "=" or "+=" or "-=" or "*=" or "/=" or "%=" or ">>=" or "<<=" or "&=" or "^=" or "|=" or "??=" => 12,
                    
                    _ => -1
                };
            }
            
            /// <summary>
            /// 
            /// </summary>
            /// <param name="Tokens"></param>
            /// <param name="MaxHierachy"></param>
            /// <returns></returns>
            internal static (List<(int StringOffset, int StringLength, (object data, AssembleTimeTypes type, AccessLevels level) data)> result, bool Success, bool Unevaluable) DeltaEvaluate(List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy) {
                while (MaxHierachy > 0) {
                    foreach (var deltas in Tokens) {
                        if (deltas.Hierachy == MaxHierachy) {
                            // linear evaluate delta tokens
                            // convert two contigous spaces into one hunk.
                            break;
                        }
                    }
                }
                return default;
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
