namespace Tataru {
    namespace Types {
        namespace AssembleTime {
            internal enum AssembleTimeTypes {
                VOID,
                INT,
                STRING,
                EXP,
                SCOPE,
                MACRO,
                PROCEDURE,
                BANK,
                REGISTER,
                FLAG,               // c z v n (Carry, Zero, Overflow, Negative)
                RUNTIME
            }

            internal struct AssembleTimeValue {
                internal AssembleTimeTypes Type;
                internal object Value;
            }

            internal class AssembleTimeBoxClass {
                internal object? Value;
            }

            internal class Scope     : AssembleTimeBoxClass {
                
            }

            internal class Macro     : AssembleTimeBoxClass {

            }

            internal class Procedure : AssembleTimeBoxClass {

            }

            internal class Bank      : AssembleTimeBoxClass {

            }

            internal class Runtime   : AssembleTimeBoxClass {

            }
        }

        namespace RunTime {
            internal enum RunTimeVariableRule {
                DIRECT,     // CPU [0x0000:0x0100]
                            // We do not promote storing to stack accessible memory
                SYSTEM,     // CPU [0x0200:0x0800]

                PROGRAM,    // Based on Mapper
                MAPPER,     // Based on Mapper

                FPGA,       // Based on Mapper

                SLOW,       // SYSTEM => MAPPER => FPGA => PROGRAM => DIRECT
                FAST,       // DIRECT => SYSTEM => MAPPER => FPGA => PROGRAM

                UNDEFINED = -1
            }

            internal enum Instruction {
                EXPLICIT, IMPLICIT, UNRECOGNIZED
            }

            internal struct RunTimeValue {
                internal byte Width;
                internal bool Signed;
                internal bool Endian;
                internal bool Pointer;

                internal ushort Offset;             // Offset in CPU Space
                internal ulong  ROMOffset;          // Offset in PRGROM/PRGRAM space (Promotes Banking)
                internal RunTimeVariableRule Rule;  // Generated rule
            }
            
            internal static class RunTime {
                static internal RunTimeValue Decode(ref string VariableTypeKeyword) {
                    RunTimeValue ReturnRTV = new();
                    if (VariableTypeKeyword.Contains('b'))        ReturnRTV.Endian  = true;
                    if (VariableTypeKeyword.StartsWith('i'))      ReturnRTV.Signed  = true;
                    if (VariableTypeKeyword.EndsWith('*'))        ReturnRTV.Pointer = true;

                         if (VariableTypeKeyword.Contains('8'))   ReturnRTV.Width   = 1;
                    else if (VariableTypeKeyword.Contains("16"))  ReturnRTV.Width   = 2;
                    else if (VariableTypeKeyword.Contains("24"))  ReturnRTV.Width   = 3;
                    else if (VariableTypeKeyword.Contains("32"))  ReturnRTV.Width   = 4;
                    else if (VariableTypeKeyword.Contains("64"))  ReturnRTV.Width   = 8;


                    // Access Memory Logging outside
                    ReturnRTV.Rule = RunTimeVariableRule.UNDEFINED;
                    return ReturnRTV;
                }
            }
        }
    }
}
