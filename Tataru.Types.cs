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
                PROCEDURE
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

            internal struct RunTimeVariable {
                internal byte Width;
                internal bool Signed;
                internal bool Endian;
                internal bool Decimal;
                internal bool Pointer;

                internal ushort Offset;             // Offset in CPU Space
                internal ulong  ROMOffset;          // Offset in PRGRAM space (promotes bankable RAM)
                internal RunTimeVariableRule Rule;  // Generated rule
            }
            
            internal static class RunTime {
                static internal RunTimeVariable Decode(ref string VariableTypeKeyword) {
                    RunTimeVariable ReturnRTV = new();
                    if (VariableTypeKeyword.Contains('d'))        ReturnRTV.Decimal = true;
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
