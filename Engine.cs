using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.RegularExpressions;
using Numinous.Language;
using Tomlyn;

namespace Numinous {
    internal enum ScopeTypes {
        Root      = 0,
        Namespace = 1,
        Macro     = 2,
        Bank      = 3,
        Procedure = 4,
        Interrupt = 5
    }
    
    internal enum Modes {
        None,
        Cartridge,
        Disk
    }

    namespace Engine {

        internal class ObjectToken {
            internal ObjectToken(object pData, AssembleTimeTypes pType, AccessLevels pLevel) {
                data         = pData;
                type         = pType;
                level        = pLevel;
            }

            ObjectToken? GetMember(string name) {
                var obj = (Dictionary<string, ObjectToken>)data;
                return obj.GetValueOrDefault(name);
            }
            
            internal  object           data;  // contains members
            internal AssembleTimeTypes type;  // type of object
            internal AccessLevels      level; // access level
        }

        internal class EvaluatedLexerToken : ObjectToken {
            internal EvaluatedLexerToken(
                int               pStringIndex,
                int               pStringLength,
                object            pData,
                AssembleTimeTypes pType,
                AccessLevels      pLevel,
                bool              pIsOperator
            ) : base(pData, pType, pLevel) {
                StringIndex  = pStringIndex;
                StringLength = pStringLength;
                IsOperator   = pIsOperator;
            }

            internal int               StringIndex, StringLength; // debugging indexes
            internal bool              IsOperator;                // if its an operator or a value
        }
        
        
        namespace System {
            internal enum Registers { A, X, Y }
            
            [Flags]
            internal enum Flags : byte {
                Carry = 0x01,
                Zero  = 0x02,
                //  Interrupt   = 0x04,
                //  Decimal     = 0x08,
                //  Break       = 0x10,
                //  None (1)    = 0x20,
                Overflow = 0x40,
                Negative = 0x80
            }

            [Flags]
            internal enum AddressModeFlags : ushort {
                Implied     = 1 << 0,
                Immediate   = 1 << 1,
                ZeroPage    = 1 << 2,
                ZeroPageX   = 1 << 3,
                ZeroPageY   = 1 << 4,
                Absolute    = 1 << 5,
                AbsoluteX   = 1 << 6,
                AbsoluteY   = 1 << 7,
                Indirect    = 1 << 8,
                IndirectX   = 1 << 9,
                IndirectY   = 1 << 10,
                Accumulator = 1 << 11,
                A           = 1 << 12,
                X           = 1 << 13,
                Y           = 1 << 14,
                Relative    = 1 << 15,
            }

            internal static class System {
                readonly static internal Dictionary<string, AddressModeFlags> InstructionAddressModes = new() {
                    { "adc", adc }, { "and", and }, { "cmp", cmp }, { "eor", eor },
                    { "lda", lda }, { "ora", ora }, { "sta", sta },

                    { "bcc", bcc }, { "bcs", bcs }, { "bnc", bnc }, { "bns", bns },
                    { "bvc", bvc }, { "bvs", bvs }, { "bzc", bzc }, { "bzs", bzs },

                    { "bit", bit }, { "brk", brk },

                    { "clc", clc }, { "clv", clv }, { "sec", sec },
                    { "tax", tax }, { "tay", tay }, { "tsx", tsx },
                    { "txa", txa }, { "txs", txs }, { "txy", txy },

                    { "cpx", cpx }, { "cpy", cpy },

                    { "dec", dec }, { "inc", inc },

                    { "dex", dex }, { "dey", dey }, { "inx", inx }, { "iny", iny },
                    { "pha", pha }, { "php", php }, { "pla", pla }, { "plp", plp },
                    { "sei", sei }, { "rti", rti }, { "rts", rts },

                    { "jmp", jmp }, { "jsr", jsr },

                    { "ldx", ldx }, { "ldy", ldy },

                    { "asl", asl }, { "lsr", lsr }, { "rol", rol }, { "ror", ror },

                    { "nop", nop },

                    { "stx", stx }, { "sty", sty }
                };

                readonly internal static AddressModeFlags[] MemoryAddressModeInstructionTypes = [
                    // adc and cmp eor lda ora sta
                    AddressModeFlags.Immediate |
                    AddressModeFlags.ZeroPageX | 
                    AddressModeFlags.ZeroPageY |
                    AddressModeFlags.Absolute  | 
                    AddressModeFlags.AbsoluteX |
                    AddressModeFlags.AbsoluteY | 
                    AddressModeFlags.IndirectX | 
                    AddressModeFlags.IndirectY  ,

                    // bzc bzs bns bnc bvs bvc bcs bcc
                    AddressModeFlags.Relative   ,

                    // bit
                    AddressModeFlags.Immediate | 
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.Absolute   ,

                    // brk
                    AddressModeFlags.Implied    | 
                    AddressModeFlags.Immediate  ,

                    // clc clv sec tax tay tsx txa txs txy
                    AddressModeFlags.Implied    ,

                    // cpx cpy
                    AddressModeFlags.Immediate | 
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.Absolute   ,

                    // dec inc
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.ZeroPageX |
                    AddressModeFlags.Absolute  |
                    AddressModeFlags.AbsoluteX  ,

                    // dex dey inx iny pha php pla plp rti rts sei 
                    AddressModeFlags.Implied    ,

                    // jmp
                    AddressModeFlags.Absolute   | 
                    AddressModeFlags.Indirect   ,

                    // jsr 
                    AddressModeFlags.Absolute   ,

                    // ldx
                    AddressModeFlags.Immediate |
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.ZeroPageY |
                    AddressModeFlags.Absolute  |
                    AddressModeFlags.AbsoluteY  ,

                    // ldy
                    AddressModeFlags.Immediate |
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.ZeroPageX |
                    AddressModeFlags.Absolute  |
                    AddressModeFlags.AbsoluteX  ,

                    // asl lsr rol ror
                    AddressModeFlags.Implied   | 
                    AddressModeFlags.A         |
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.ZeroPageX |
                    AddressModeFlags.Absolute  |
                    AddressModeFlags.AbsoluteX  ,

                    // nop
                    AddressModeFlags.Implied   | 
                    AddressModeFlags.Immediate | 
                    AddressModeFlags.ZeroPage  | 
                    AddressModeFlags.ZeroPageX | 
                    AddressModeFlags.Absolute  | 
                    AddressModeFlags.AbsoluteX  ,

                    // stx
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.ZeroPageY |
                    AddressModeFlags.Absolute   ,

                    // sty
                    AddressModeFlags.ZeroPage  |
                    AddressModeFlags.ZeroPageX |
                    AddressModeFlags.Absolute   ,
                ];

                readonly internal static AddressModeFlags adc = MemoryAddressModeInstructionTypes[0],
                                                          and = MemoryAddressModeInstructionTypes[0],
                                                          cmp = MemoryAddressModeInstructionTypes[0],
                                                          eor = MemoryAddressModeInstructionTypes[0],
                                                          lda = MemoryAddressModeInstructionTypes[0],
                                                          ora = MemoryAddressModeInstructionTypes[0],
                                                          sta = MemoryAddressModeInstructionTypes[0];

                readonly internal static  AddressModeFlags bcc = MemoryAddressModeInstructionTypes[1],
                                                           bcs = MemoryAddressModeInstructionTypes[1],
                                                           bnc = MemoryAddressModeInstructionTypes[1],
                                                           bns = MemoryAddressModeInstructionTypes[1],
                                                           bvc = MemoryAddressModeInstructionTypes[1],
                                                           bvs = MemoryAddressModeInstructionTypes[1],
                                                           bzc = MemoryAddressModeInstructionTypes[1],
                                                           bzs = MemoryAddressModeInstructionTypes[1];

                readonly internal static AddressModeFlags bit = MemoryAddressModeInstructionTypes[2];
                readonly internal static AddressModeFlags brk = MemoryAddressModeInstructionTypes[3];

                readonly internal static AddressModeFlags clc = MemoryAddressModeInstructionTypes[4],
                                                          clv = MemoryAddressModeInstructionTypes[4],
                                                          sec = MemoryAddressModeInstructionTypes[4],
                                                          tax = MemoryAddressModeInstructionTypes[4],
                                                          tay = MemoryAddressModeInstructionTypes[4],
                                                          tsx = MemoryAddressModeInstructionTypes[4],
                                                          txa = MemoryAddressModeInstructionTypes[4],
                                                          txs = MemoryAddressModeInstructionTypes[4],
                                                          txy = MemoryAddressModeInstructionTypes[4];

                readonly internal static AddressModeFlags cpx = MemoryAddressModeInstructionTypes[5],
                                                          cpy = MemoryAddressModeInstructionTypes[5];

                readonly internal static AddressModeFlags dec = MemoryAddressModeInstructionTypes[6],
                                                          inc = MemoryAddressModeInstructionTypes[6];

                readonly internal static AddressModeFlags dex = MemoryAddressModeInstructionTypes[7],
                                                          dey = MemoryAddressModeInstructionTypes[7],
                                                          inx = MemoryAddressModeInstructionTypes[7],
                                                          iny = MemoryAddressModeInstructionTypes[7],
                                                          pha = MemoryAddressModeInstructionTypes[7],
                                                          php = MemoryAddressModeInstructionTypes[7],
                                                          pla = MemoryAddressModeInstructionTypes[7],
                                                          plp = MemoryAddressModeInstructionTypes[7],
                                                          sei = MemoryAddressModeInstructionTypes[7],
                                                          rti = MemoryAddressModeInstructionTypes[7],
                                                          rts = MemoryAddressModeInstructionTypes[7];

                readonly internal static AddressModeFlags jmp = MemoryAddressModeInstructionTypes[8];
                readonly internal static AddressModeFlags jsr = MemoryAddressModeInstructionTypes[9];

                readonly internal static AddressModeFlags ldx = MemoryAddressModeInstructionTypes[10];
                readonly internal static AddressModeFlags ldy = MemoryAddressModeInstructionTypes[11];

                readonly internal static AddressModeFlags asl = MemoryAddressModeInstructionTypes[12],
                                                          lsr = MemoryAddressModeInstructionTypes[12],
                                                          rol = MemoryAddressModeInstructionTypes[12],
                                                          ror = MemoryAddressModeInstructionTypes[12];

                readonly internal static AddressModeFlags nop = MemoryAddressModeInstructionTypes[13];

                readonly internal static AddressModeFlags stx = MemoryAddressModeInstructionTypes[14];
                readonly internal static AddressModeFlags sty = MemoryAddressModeInstructionTypes[15];

            }
        }


        [Flags]
        internal enum WarningLevels : byte {
            IGNORE  = 0x00,
            DEFAULT = 0x01,
            ERROR   = 0x02,
            VERBOSE = 0x04,

            /* Internal     */
            NONE        = 0xff,
            NO_OVERRULE = 0x08,

            /* Composite    */
            STRICT     = VERBOSE | ERROR,
            CONTROLLED = VERBOSE | ERROR | NO_OVERRULE,

        }

        internal enum Operators : byte {
            INC,
            DEC,
            BITNOT,
            
            STRING,
            FSTRING,

            OPAREN,
            CPAREN,

            OBRACK,
            CBRACK,

            OBRACE,
            CBRACE,

            DESCOPE,

            PROPERTY,
            NULLPROPERTY,

            MULT,
            DIV,
            MOD,

            ADD,
            SUB,

            RIGHT,
            LEFT,

            BITMASK,

            BITFLIP,

            BITSET,

            GT,
            LT,
            GOET,
            LOET,
            SERIAL,

            EQUAL,
            INEQUAL,

            AND,

            OR,

            NULL,
            CHECK,
            ELSE,

            SET,
            INCREASE,
            DECREASE,
            MULTIPLY,
            DIVIDE,
            MODULATE,
            NULLSET,
            RIGHTSET,
            LEFTSET,

            ASSIGNMASK,
            ASSIGNSET,
            ASSIGNFLIP,

            TERM,
            NOT,

            NONE = 255
        }

        internal enum AssembleTimeTypes : byte {
            INT,    // assemble time integer
            STRING, // assemble time string
            
            SCOPE, // scope type
            RT,    // Runtime Variable
            REG,   // Register
            FLAG,  // CPU Status Flag
            PROC,  // Procedure
            INTER, // Interrupt
            BANK,  // Bank
            EXP,   // Expression

            OBJECT,  // The Boxed 'AnyType' such as long as its not constant      | Cast Exclusive Type
            
            CONSTANT = 0x040,

            CINT = CONSTANT, // Constant int
            CSTRING,         // Constant string
            
            CSCOPE,  // Constant Scope reference
            CRT,     // Constant runtime reference
            CREG,    // Constant register reference
            CFLAG,   // Constant flag reference
            CPROC,   // Constant procedure reference
            CINTER,  // Constant interrupt reference
            CBANK,   // Constant bank reference
            CEXP,    // Constant Expression
            COBJECT, // The Boxed 'AnyType' clearing object reference from object, or a constant object      | Cast Exclusive Type
            
            FEXP,    // Functional Expression

            IRWN,  // Indexing Register with N             foo[i + 2] situations
            ICRWN, // Indexing Constant Register with N    foo[x + 2] situations

            FUNCTION, // Macro Function
            OPER,     // Operation

            MACRO = 0x80,
            
            TYPE,    // typeof result
            CHARMAP, // for ASCII redirection
            
            INDEX,
            CALL,
            
            TUPLE,   // elems = List<Object>    types = List<AssembleTimeTypes>
            
            TOKENS,  // for unresolved but declared expressions
        }

        internal enum AccessLevels : byte {
            PUBLIC  = 0,
            PRIVATE = 1
        }

        internal enum AssembleTimeValueStatus : byte {
            DECLARED, // int foo;
            PARTIAL,  // int foo = defined_later;
            OK        // int foo = 2;
        }

        internal enum ContextFetcherEnums : byte {
            OK,
            MALFORMED,
            UNTERMINATED
        }

        internal struct RunTimeVariableFilterType {
            internal uint? size;
            internal bool? signed;
            internal bool? endian;
        }

        internal struct RunTimeVariableType {
            internal uint size;   // in bytes
            internal bool signed; // false => unsigned
            internal bool endian; // false => little
        }

        internal enum ErrorLevels : byte {
            NONE, LOG, WARN, ERROR
        }

        internal enum ErrorTypes : byte {
            None, SyntaxError, ParsingError, NothingToDo
        }

        internal enum DecodingPhases : byte {
            TERMINAL, TOKEN, EVALUATION
        }

        internal enum Expectations : byte {
            VALUE,
            OPERATOR
        }
        
        internal enum OperationTypes : byte {
            FAIL,
            DIRECTIVE,   // eg.. #include
            INSTRUCTION, // eg.. lda foo
            EVALUATE,    // function, macros, RODATA writes
            KEYWORD,     // int foo = bar, return
            RUNTIME,     // u8 foo       : is a keyword BUT returns different ctx

            ANON_REL_BRANCH,    // +: and -:
        }


        internal static partial class Engine {
            internal static bool Permits(RunTimeVariableFilterType filter, RunTimeVariableType variable) =>
                       filter.size   == null || filter.size   == variable.size   &&
                       filter.endian == null || filter.endian == variable.endian &&
                       filter.signed == null || filter.signed == variable.signed;
            
            
            
            // Generated Function
            // TODO: Rewrite this code looks terrible - GPT has been underperfoming
            internal static object GenerateFunctionalDefine(string Context, List<string> ParameterMapping) {
                foreach (var param in ParameterMapping)
                    Context = Regex.Replace(Context, $@"\b{Regex.Escape(param)}\b", $"{{{param}}}");

                
                return new Func<
                    List<(string token, int StringIndex, int StringLength)>, 
                    int, 
                    int, 
                    List<(string token, int StringIndex, int StringLength)>
                >(
                  (args, fallbackIndex, fallbackLength) => {
                      // Map param name → tuple
                      var map = ParameterMapping
                               .Zip(args, (k, v) => (k, v))
                               .ToDictionary(p => p.k, p => p.v);

                      // Interpolate into tuple list (no flattening to string)
                      var tokens = Interpolate(Context, map);

                      // Apply macro operators on tuple list
                      return ProcessDefineOperators(tokens, map, fallbackIndex, fallbackLength);
                  }
                 );
                
                static List<(string token, int StringIndex, int StringLength)> Interpolate(string format, Dictionary<string, (string token, int StringIndex, int StringLength)> map)  {
                    var result = new List<(string, int, int)>();
                    var regex  = new Regex(@"\{([A-Za-z_][A-Za-z0-9_]*)\}");

                    int lastPos = 0;
                    foreach (Match match in regex.Matches(format)) {
                        // Add literal text before placeholder (treated as its own token with fallback index)
                        if (match.Index > lastPos) {
                            string literal = format.Substring(lastPos, match.Index - lastPos);
                            result.Add((literal, -1, literal.Length)); // literal with no source index
                        }

                        // Add parameter replacement
                        var key = match.Groups[1].Value;
                        if (map.TryGetValue(key, out var tuple))
                            result.Add(tuple);
                        else
                            result.Add(($"{{{key}}}", -1, key.Length + 2)); // missing param

                        lastPos = match.Index + match.Length;
                    }

                    // Add any trailing literal text
                    if (lastPos < format.Length) {
                        string literal = format.Substring(lastPos);
                        result.Add((literal, -1, literal.Length));
                    }

                    return result;
                }

                static List<(string token, int StringIndex, int StringLength)> ProcessDefineOperators(
                    List<(string token, int StringIndex, int StringLength)>               tokens,
                    Dictionary<string, (string token, int StringIndex, int StringLength)> map,
                    int                                                                   fallbackIndex,
                    int                                                                   fallbackLength) {
                    // Token-pasting (##)
                    for (var i = 0; i < tokens.Count - 2; i++) {
                        if (tokens[i + 1].token != "##") continue;
                        var left  = tokens[i];
                        var right = tokens[i + 2];

                        var idx = left.StringIndex >= 0 ? left.StringIndex :
                            right.StringIndex         >= 0 ? right.StringIndex : fallbackIndex;
                        var len                     = left.StringLength + right.StringLength;
                        if (idx == fallbackIndex) len = fallbackLength;

                        var merged = (left.token + right.token, idx, len);

                        // Replace [left, ##, right] with merged
                        tokens.RemoveAt(i); // remove left
                        tokens.RemoveAt(i); // remove ##
                        tokens[i] = merged; // replace right with merged
                        i--;                // re-check in case of multiple pastes
                    }

                    // Stringification (#)
                    for (int i = 0; i < tokens.Count - 1; i++) {
                        if (tokens[i].token == "#") {
                            var param  = tokens[i + 1];
                            var quoted = ($"\"{param.token}\"", param.StringIndex, param.StringLength);
                            tokens.RemoveAt(i); // remove #
                            tokens[i] = quoted; // replace param with quoted version
                        }
                    }

                    return tokens;
                }

            }
            
            /// <summary>
            /// GENERATED CODE : Attempts to normalize and validate a path as safe across Windows, macOS, and Linux.
            /// Returns true if the path is valid and portable; false otherwise.
            /// </summary>
            internal static bool TryNormalizeSafePath(string input, out string normalized)
            {
                normalized = string.Empty;
                if (string.IsNullOrWhiteSpace(input)) return false;

                try
                {
                    string path = Path.GetFullPath(input.Trim());

                    if (path.Length >= 240) return false;

                    // Reject invalid path characters
                    if (path.IndexOfAny(Path.GetInvalidPathChars()) != -1) return false;

                    // Split into segments to check each component
                    string[] parts = path.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
                    if (string.IsNullOrWhiteSpace(parts[0])) return false;
                    
                    foreach (var part in parts)
                    {
                        if (string.IsNullOrWhiteSpace(part)) continue;

                        // Trim + reject bad chars
                        string name = part.Trim();
                        if (name.Length                                     == 0) return false;
                        if (name.IndexOfAny(Path.GetInvalidFileNameChars()) != -1) return false;

                        // Disallow reserved Windows device names
                        string upper = name.ToUpperInvariant();
                        if (Regex.IsMatch(upper, @"^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$"))
                            return false;

                        // No trailing dot or space
                        if (name.EndsWith(" ") || name.EndsWith(".")) return false;
                    }

                    normalized = path;
                    return true;
                }
                catch
                {
                    return false;
                }
            }
            
            /// <summary>
            /// GENERATED CODE : C# function to interpret a C-style escape sequence, removing IsHexDigit helper and relying on TryParse
            /// Processes a single C-style escape sequence starting at index 0 in the input string.
            /// Returns the interpreted result as a string and a success flag indicating whether the escape produced a different result.
            /// </summary>
            internal static (string ctx, bool success) InterpretEscape(string input) {
                if (string.IsNullOrEmpty(input) || input[0] != '\\')
                    return (input, false);

                var i = 1;
                if (i >= input.Length)
                    return (input, false);

                var c = input[i];
                switch (c) {
                    case 'n':  return ("\n", true);
                    case 'r':  return ("\r", true);
                    case 't':  return ("\t", true);
                    case 'b':  return ("\b", true);
                    case 'f':  return ("\f", true);
                    case 'a':  return ("\a", true);
                    case 'v':  return ("\v", true);
                    case '\\': return ("\\", true);
                    case '\'': return ("'", true);
                    case '\"': return ("\"", true);
                    case '?':  return ("?", true);

                    case 'x': {
                        var start = i + 1;
                        var j     = start;

                        while (j < input.Length) {
                            char ch = input[j];
                            bool isHexChar = (ch >= '0' && ch <= '9') ||
                                             (ch >= 'a' && ch <= 'f') ||
                                             (ch >= 'A' && ch <= 'F');

                            if (!isHexChar)
                                break;
                            j++;
                        }

                        if (j == start) return (input, false);

                        var hex = input.Substring(start, j - start);
                        if (int.TryParse(hex, global::System.Globalization.NumberStyles.HexNumber, null, out var value))
                            return (((char)value).ToString(), true);

                        return (input, false);
                    }

                    case >= '0' and <= '7': {
                        var value = ParseOctal(input.AsSpan(i));
                        return (((char)value).ToString(), true);
                    }

                    default:
                        return (input, false);
                }

                static int ParseOctal(ReadOnlySpan<char> span)
                {
                    int value = 0, count = 0;
                    while (count < 3 && count < span.Length)
                    {
                        var digit = span[count];
                        if (digit is < '0' or > '7') break;
                        value = value * 8 + (digit - '0');
                        count++;
                    }
                    return value;
                }
            }
            
            internal static (object Return, AssembleTimeTypes Type, bool Success) Assemble(Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> args) {

                Span<List<string>>  SourceFileContentBufferSpan = CollectionsMarshal.AsSpan(Program.SourceFileContentBuffer);
                Span<string>        SourceFileNameBufferSpan    = CollectionsMarshal.AsSpan(Program.SourceFileNameBuffer);
                Span<int>           SourceTokenIndexBufferSpan   = CollectionsMarshal.AsSpan(Program.SourceTokenIndexBuffer);
                Span<int>           SourceFileLineBufferSpan    = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
                Span<int>           SourceFileStepBufferSpan    = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
                
                var                                                     CollectiveContext = "";
                (string ctx, int StringIndex, int StringLength)         ActiveToken;
                List<(string token, int StringIndex, int StringLength)> DefineResolveBuffer = [];

                var BasicRegexTokens = new global::System.Memory<string>(Program.SourceFileContentBuffer[^1].ToArray());

                var StringIndex = 0;
                var TokenIndex  = 0;
                
                (List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, int Finish, bool Success, bool Continue) lexer_resp; 
                (List<(int StringOffset, int StringLength, object data, AssembleTimeTypes type, AccessLevels level, bool IsOperator)> result, bool Success, bool Unevaluable)                                              evaluate_resp;

                while (true) {
                    Step();
                    if (ActiveToken.ctx[0] == '#') {
                        // parse as directive
                        Step();

                        switch (ActiveToken.ctx) {
                            // directives
                            
                            case "pragma":
                                Step();
                                switch (ActiveToken.ctx) {
                                    case "push":
                                        Step();
                                        switch (ActiveToken.ctx) {
                                            case "illegal":
                                                // lexer parse
                                                // evaluate parse
                                                // Add to buffer
                                            case "cpu":
                                                // lexer parse
                                                // evaluate parse
                                                // Add to buffer
                                            case "gpr":
                                                // lexer parse
                                                // evaluate parse
                                                // Add to buffer
                                            case "memory":    
                                                // lexer parse
                                                // evaluate parse
                                                // Add to buffer
                                                break;
                                            
                                            default:
                                                // error malformed pragma
                                                return default;
                                        }
                                        continue;
                                    
                                    case "pop":
                                        Step();
                                        switch (ActiveToken.ctx) {
                                            case "illegal": {
                                                if (Program.PragmaIllegalBuffer.Count == 1) {
                                                    // unable to pop, stack clear. Stack ends at 1 == Default Settings
                                                    return default;
                                                }
                                                
                                                Program.PragmaIllegalBuffer .RemoveAt(Program.PragmaIllegalBuffer .Count - 1); 
                                                break;
                                            }
                                            case "cpu":     
                                                if (Program.PragmaIllegalBuffer.Count == 1) {
                                                    // unable to pop, stack clear. Stack ends at 1 == Default Settings
                                                    return default;
                                                }
                                                
                                                Program.PragmaCPUAwareBuffer.RemoveAt(Program.PragmaCPUAwareBuffer.Count - 1); 
                                                break;
                                            
                                            case "gpr":     
                                                if (Program.PragmaIllegalBuffer.Count == 1) {
                                                    // unable to pop, stack clear. Stack ends at 1 == Default Settings
                                                    return default;
                                                }
                                                
                                                Program.PragmaGPRAwareBuffer.RemoveAt(Program.PragmaGPRAwareBuffer.Count - 1); 
                                                break;
                                            
                                            case "memory":                                                  
                                                if (Program.PragmaIllegalBuffer.Count == 1) {
                                                    // unable to pop, stack clear. Stack ends at 1 == Default Settings
                                                    return default;
                                                }
                                                
                                                Program.PragmaRAMAwareBuffer.RemoveAt(Program.PragmaRAMAwareBuffer.Count - 1); 
                                                break;
                                            default:
                                                // error malformed pragma
                                                return default;
                                        }
                                        continue;
                                    
                                    default:
                                        // error, pragma follows malfrormed syntax.
                                        return default;
                                }

                                break;
                            
                            case "assert":
                                // lexer the rightside
                                // evaluate the rightside
                                // if clear : error
                                break;
                            
                            case "include":
                                var    Binary = false;
                                var    fp     = string.Empty;

                                Step();

                                if (ActiveToken.ctx[0] is not (' ' or '\t')) {
                                    // error, malformed include
                                }
                                
                                seek_no_whitespace();
                                if (ActiveToken.ctx == "bin") {
                                    if (Binary) {
                                        // error, binary already set
                                        return default;
                                    }

                                    Binary = true;
                                    seek_no_whitespace();
                                }

                                ProcessPath();
                                
                                if (fp == string.Empty) {
                                    // error, no lib
                                    return default;
                                }

                                if (Binary) {
                                    // include as anonymous table
                                } else {
                                    (fp, var success) = CheckInclude(fp);
                                    if (!success) {
                                        // lib does not exist
                                        return default;
                                    }
                                    
                                    // recurse now
                                    AddSourceContext(fp);
                                    Assemble([]);
                                }
                                continue;

                                void ProcessPath() {
                                    if (ActiveToken.ctx[0] == '<') {
                                        Step();
                                        fp = LibGetPathFromContext();
                                        // manual parsing
                                    } else {
                                        // else lexer -> evaluate
                                    }
                                
                                    // include from path, (IncludePath)

                                    return;
                                    
                                    string LibGetPathFromContext() {
                                        var fp                    = string.Empty;
                                        var InitialCharacterCount = 0u;

                                        for (; !CheckLineTerminated(); Step()) {
                                            fp += ActiveToken;
                                            switch (ActiveToken.ctx[0]) {
                                                case '<':
                                                    InitialCharacterCount++;
                                                    continue;
                            
                                                case '>':
                                                    if (--InitialCharacterCount == 0) break;
                                                    continue;
                            
                                                default: continue;
                                            }
                        
                                            break;
                                        }

                                        if (!TryNormalizeSafePath($"{fp}.s", out fp)) {
                                            // error, malformed path
                                            return string.Empty;
                                        }

                                        if (ActiveToken.ctx[0] != '>') {
                                            // error, malformed lib
                                            return string.Empty;
                                        } else {
                                            return InitialCharacterCount == 0 ? fp : string.Empty;   
                                        }
                                    }
                                }
                            
                            case "undef":
                                seek_no_whitespace();
                                var resp = Database.GetObjectFromAlias(ActiveToken.ctx, Program.ActiveScopeBuffer[^1],
                                                              AccessLevels.PUBLIC);
                                if (resp is null) {
                                    // error, could not find
                                    return default;
                                } else if (resp.type is AssembleTimeTypes.EXP 
                                                         or AssembleTimeTypes.FEXP
                                                         or AssembleTimeTypes.CEXP) {
                                    Program.ActiveScopeBuffer[^1].Remove(ActiveToken.ctx);
                                } else {
                                    // target is not define type, cannot undefine.
                                    return default;
                                }
                                continue;
                            
                            case "define":
                                // special manual parsing
                                continue;
                            
                            case "rom": // evaluate => set rom
                            case "cpu": // evaluate => set cpu
                            case "mapper":
                                if (Header.Mapper != Mappers.UNSPECIFIED) {
                                    // error, cant set mapper twice
                                    return default;
                                }
                                Step();
                                var MapperNumber = ActiveToken.ctx switch {
                                    "nrom" => 0,
                                    _ => -1
                                };

                                if (MapperNumber == -1) {
                                    // evaluate the information
                                }
                                
                                // set up mapper
                                continue;
                            
                            // all below only take numbers, because nomenclature sucks.
                            case "prgrom":
                            case "chrrom":
                            case "mirror":
                            case "battery":
                            case "trainer":
                            case "altnt":
                            case "console":
                            case "prgram":
                            case "chrram":
                            case "eeprom":
                            case "chrnvram":
                            case "vstype":
                            case "vsppu":
                            case "ectype":
                            case "miscroms":
                            case "dexpd":    
                                break;
                        }

                        continue;
                    }

                    var MemoryMode = Program.ActiveScopeTypeBuffer[^1] switch {
                        ScopeTypes.Macro => Memory.MemoryModes.FAST,
                        ScopeTypes.Root or ScopeTypes.Bank => Memory.MemoryModes.SYSTEM,
                        _ => Memory.MemoryModes.SLOW
                    };

                    switch (ActiveToken.ctx) {
                        case "if":
                            // lexer grab header
                            // -> if true enter code body
                            // other wise skim lexer through to code brace end
                        case "else":
                            // if tracked prior condition was failed
                            //  if followed by 'if' simply goto case if
                            //  otherwise enter
                            // else
                            //  skim lexer through to code brace end
                        case "loop":
                            // store the regex index into a buffer
                            // on closed code brace, continue
                        case "break":
                            // skim lexer though to code brace end, pop loop
                        case "return":
                            // may NOT exist inside a loop. may return a value.
                        case "bank":
                            // bank declare, top level only
                        case "proc":
                            // proc declare, second level only.
                        case "interrupt":
                            // interrupt handler declare, second level only
                        case "nmi":
                            // interupt => nmi (if nmi isn't parameterised already), second level only
                        case "irq":
                            // interupt => irq (if irq isn't parameterised already), second level only
                        case "reset":
                            // interupt => reset (if reset isn't parameterised already), second level only
                        case "table" :
                            // typless table declare, just raw const rodata here.
                        
                        case "const":
                            // const int, const string etc..
                        
                        case "void":
                            // void foo(args)
                        case "int":
                            // int foo = 5 OR int foo(args)
                        case "string":    
                            // string foo = "bar" OR string foo(args)
                            break;
                        
                        case "register": 
                        case "flag":
                            
                        // rt variable contextualisation
                        case "direct":
                        case "system":
                        case "mapper":
                        case "program":
                        case "fast":
                        case "slow":
                            break;
                        
                        case "del":
                            seek_no_whitespace();
                            var resp = Database.GetObjectFromAlias(ActiveToken.ctx,  Program.ActiveScopeBuffer[^1], AccessLevels.PUBLIC);
                            if (resp is null) {
                                // error : label does not exist
                                return default;
                            }

                            if (!(resp.type is AssembleTimeTypes.FUNCTION
                                                or AssembleTimeTypes.ICRWN
                                                or AssembleTimeTypes.IRWN
                                                or AssembleTimeTypes.OPER)) {
                                // error : not a deletable type
                            }

                            if (!resp.type.HasFlag(AssembleTimeTypes.CONSTANT)) {
                                // error : cannot delete constant objects :: TODO: drop them a link to the wiki explaining the backwards definition mutation problem
                                return default;
                            }

                            Program.ActiveScopeBuffer[^1].Remove(ActiveToken.ctx);
                            continue;
                        
                        
                        case "extern":
                            // declare but dont define
                            break;
                    }
                    
                    // check if memory mode implicit variable reserve
                    
                    // check if function call
                    
                    // evaluate for RODATA
                    // if specified lvalue, then an assigning operator DO NOT WRITE DATA
                }
                
                // TODO: Function to capture requested task
                
                void Step(bool regexParse = true) {
                    ActiveToken = default;
                    if (DefineResolveBuffer.Count == 0) {                                                   // si to be mutated ONLY by typed tokens
                        CollectiveContext += BasicRegexTokens.Span[TokenIndex];
                        StringIndex       += BasicRegexTokens.Span[TokenIndex].Length;
                    }

                    if (regexParse) {
                        List<(string token, int StringIndex, int StringLength)> ctx;
                        bool                                                    success;
                        if (DefineResolveBuffer.Count == 0) {
                            (ctx, success)      = PartialResolveDefine(BasicRegexTokens.Span[TokenIndex++]);
                            DefineResolveBuffer = ctx;
                        }
                        else {
                            (ctx, success) = PartialResolveDefine(DefineResolveBuffer[0].token);
                            DefineResolveBuffer.RemoveAt(0);
                            DefineResolveBuffer.InsertRange(0, ctx);
                        }

                        if (success) Step();
                        ActiveToken = DefineResolveBuffer[0];
                        DefineResolveBuffer.RemoveAt(0);
                    }
                    else ActiveToken = (BasicRegexTokens.Span[TokenIndex++], StringIndex, BasicRegexTokens.Span[TokenIndex - 1].Length);
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
                
                (List<(string token, int StringIndex, int StringLength)> ctx, bool success) PartialResolveDefine(string Token) {
                

                    List<(string token, int StringIndex, int StringLength)>    Resolved = [(Token, ActiveToken.StringIndex, Token.Length)];
                    ObjectToken? ctx;
                    bool                                                       success;
                    bool                                                       HadSuccess = false;

                    do {
                        ctx = Database.GetObjectFromAlias(Resolved[0].token, AccessLevels.PUBLIC);

                        if (ctx is not null && ctx.type == AssembleTimeTypes.CEXP) {
                            HadSuccess = true;
                            Resolved.RemoveAt(0);
                        
                            Resolved.InsertRange(0, RegexTokenize((string)((Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>)ctx.data)[""].data)
                                                   .Select(token => (token, ActiveToken.StringIndex, token.Length))
                                                   .ToList());
                        }

                    } while (ctx is not null && ctx.type == AssembleTimeTypes.CEXP);

                    return (Resolved, HadSuccess);
                }
                
                /*(OperationTypes oper, object ctx) ExtractOperation() {
                    object ctx; bool success;
                    if (ActiveToken.ctx[0] == '#') {
                        Step(); if (CheckDirectiveMalformed()) return default;
                    
                        var Directive = ActiveToken;
                        switch (Directive.ctx) {
                            case "pragma":
                                Step(); if (CheckDirectiveMalformed()) return default;
                                if (ActiveToken.ctx[0] != ' ') {
                                    // error, malformed pragma
                                    return default;
                                }

                                Step(); if (CheckDirectiveMalformed()) return default;
                                var Modifier = ActiveToken.ctx switch { "push" => 0, "pop" => 1, _ => 0xff};
                                if (Modifier == 0xff) {
                                    // error, erroneous action for pragma
                                    return default;
                                }
                            
                                Step(); if (CheckDirectiveMalformed()) return default;
                                if (ActiveToken.ctx[0] != ' ') {
                                    // error, malformed pragma
                                    return default;
                                }
                            
                                Step(); if (CheckDirectiveMalformed()) return default;
                                var Pragma = (Directives)(Modifier | (byte)(ActiveToken.ctx switch {
                                    "memory_aware" => Directives.PUSH_MEM,
                                    "gpr_aware"    => Directives.PUSH_GPR,
                                    "cpu_aware"    => Directives.PUSH_CPU,
                                    "illegal"      => Directives.PUSH_ILLEGAL,
                                
                                    _               => Directives.ERROR,          
                                }));

                                if (Pragma == Directives.ERROR) {
                                    // error, bad pragma
                                    return default;
                                }

                                Step();
                                if (ActiveToken.ctx[0] != ' ') {
                                    // error malformed pragma
                                    return default;
                                }

                                return (OperationTypes.DIRECTIVE, Pragma);

                            case "include":
                                string fp;
                                // demands <> :: We can take over from here
                                if (CheckDirectiveMalformed()) return default;
                                Step();

                                if (ActiveToken.ctx[0] != ' ') {
                                    // error: directive malformed
                                    return default;
                                }

                                Step();
                                if (ActiveToken.ctx[0] == '<') {        // local func: get from lib
                                    if (CheckLineTerminated()) {
                                        // error: malformed
                                        return default;
                                    }

                                    Step();
                                    fp = LibGetPathFromContext();
                                    if (fp == string.Empty) {
                                        // error malformed path
                                        return default;
                                    }
                                
                                    success = TryNormalizeSafePath($"{fp}.s", out fp);
                                    if (!success) {
                                        // error: malformed path    :: Must be mac/linux/windows compatible
                                        return default;
                                    }
                                
                                    (fp, success) = CheckInclude(fp);
                                    if (!success) {
                                        // error: library not found
                                        return default;
                                    }

                                    // recurse now.
                                    AddSourceContext(fp);
                                    Assemble([]);
                                
                                    return (OperationTypes.DIRECTIVE, Directives.INCLUDE);
                                } else if (ActiveToken.ctx[0] == '\"') { // local func: get from src
                                
                                    // recurse once path has been evaluated.
                                    return (OperationTypes.DIRECTIVE, Directives.LOCAL_INCLUDE);
                                } else if (ActiveToken.ctx == "bin") {
                                    if (CheckLineTerminated()) {
                                        // error, malformed directive
                                        return default;
                                    }

                                    Step();
                                    if (CheckLineTerminated() || ActiveToken.ctx[0] != ' ') {
                                        // error, malformed directive
                                        return default;
                                    }

                                    Step();
                                    if (ActiveToken.ctx[0] == '<') { // local func: get from lib as bin
                                        if (CheckLineTerminated()) {
                                            // error: malformed
                                            return default;
                                        }

                                        Step();
                                        fp = LibGetPathFromContext();
                                        if (fp == string.Empty) {
                                            // error malformed path
                                            return default;
                                        }
                                    
                                        // stdlib graphics do not use a filetype
                                        success = TryNormalizeSafePath(fp, out fp);
                                        if (!success) {
                                            // error: malformed path    :: Must be mac/linux/windows compatible
                                            return default;
                                        }
                                
                                        (fp, success) = CheckInclude(fp);
                                        if (!success) {
                                            // error: library not found
                                            return default;
                                        }
    
                                        // write contents to ROM immediately
                                    
                                        return (OperationTypes.DIRECTIVE, Directives.INCLUDEBIN);
                                    } else if (ActiveToken.ctx[0] == '\"') { // local func: get from src as bin
                                        // write contents to ROM once string resolved (fstring support)
                                    
                                        return (OperationTypes.DIRECTIVE, Directives.LOCAL_INCLUDEBIN);
                                    } else {
                                        // error: malformed include path
                                        return default;
                                    }
                                } else {
                                    // error: malformed include path
                                    return default;
                                }
                            
                                return default;
                            case "assert":
                            // requires a boolean, we'll depend on Eval

                            case "cart":
                                if (Program.Mode == Modes.None) {
                                    Program.Mode = Modes.Cartridge;
                                    return (OperationTypes.DIRECTIVE, Directives.CART);
                                } else {
                                    // error already set
                                    return default;
                                }


                            case "disk":
                                if (Program.Mode == Modes.None) {
                                    Program.Mode = Modes.Disk;
                                    return (OperationTypes.DIRECTIVE, Directives.DISK);
                                } else {
                                    // error already set
                                    return default;
                                }

                            case "define":
                                if (CheckDirectiveMalformed()) return default;
                                Step();
                                if (CheckDirectiveMalformed() || ActiveToken.ctx[0] != ' ') return default;
                                Step();
                                var define_id  = ActiveToken;
                                var define_ctx = string.Empty;
                            
                                // check if reserved, operator, multiple tokens or an existing identity : fail if so
                                if (CheckLineTerminated()) {
                                    // ctx of define will be literally nothingness. This is ok.
                                    // add to db

                                    return (OperationTypes.DIRECTIVE, Directives.DEFINE);
                                }

                                seek_no_whitespace();
                                if (CheckLineTerminated()) {
                                    // ctx of define will be literally nothingness. This is ok.
                                    // add to db

                                    return (OperationTypes.DIRECTIVE, Directives.DEFINE);
                                }
                            
                            
                                // if it's a foo(x, y) y, x situation we will mark as it as a FUNCDEFINE
                                // that just means gather operands to use for an fstring, instead of a string for CEXP result 
                                // recurse lexer???
                                if (ActiveToken.ctx[0] == '(') {
                                    // gather and match param names to ids
                                    List<string> ParameterMapping = [];

                                    do {
                                        seek_no_whitespace(regexParse: false);
                                        if (CheckLineTerminated()) {
                                            // error, malformed define
                                            return default;
                                        }

                                        if (ParameterMapping.Contains(ActiveToken.ctx)) {
                                            // error, duplicate declared parameter
                                            return default;
                                        }

                                        // if ActiveToken is operator, keyword, identity ... fail
                                        if (Reserved.Contains(ActiveToken.ctx)) {
                                            // error, token is reserved
                                            return default;
                                        }
                                   
                                        (_, success) = GetObjectFromAlias(ActiveToken.ctx, Program.ActiveScopeBuffer[^1], AccessLevels.PUBLIC);
                                        if (success) {
                                            // parameter already has a definition : cannot be used
                                            return default;
                                        }

                                        ParameterMapping.Add(ActiveToken.ctx);
                                        seek_no_whitespace(regexParse: false);
                                        if (ActiveToken.ctx[0] == ',') continue;
                                        if (ActiveToken.ctx[0] == ')') break;
                                    
                                        // error malformed: no comma or close bracket
                                        return default;
                                    } while(true);
                                
                                    seek_no_whitespace();
                                    for (; !CheckLineTerminated(); Step(false)) {
                                        if (ActiveToken == define_id) {
                                            // error, define is explicitly recursive
                                            return default;
                                        }
                                        define_ctx  += ActiveToken;
                                    }
                                
                                    if (ActiveToken.ctx != "//" || ActiveToken.ctx != "/*") define_ctx  += ActiveToken;
                                
                                    // create fexp header with param count
                                    Program.ActiveScopeBuffer[^1][define_id.ctx] = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
                                        {"args", (ParameterMapping.Count, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)},
                                        {"", (GenerateFunctionalDefine(define_ctx, ParameterMapping), default, default)}
                                    }, AssembleTimeTypes.FEXP, AccessLevels.PUBLIC);

                                    return (OperationTypes.DIRECTIVE, Directives.FUNCDEFINE);
                                }

                            
                                /*while (DefineResolveBuffer.Count > 0) {
                                    Step();
                                    define_ctx += ActiveToken[0];
                                }#1#

                                // gather until ctx exhausted, that's our define. Add as DEFINE and return as DEFINE
                                while (DefineResolveBuffer.Count > 0 || CheckLineTerminated()) {
                                    Step(false);
                                    define_ctx += ActiveToken.ctx[0];
                                }
                            
                                // this will allow comments are defines, not being included in them!
                                if (ActiveToken.ctx != "//" || ActiveToken.ctx != "/*") define_ctx += ActiveToken;

                                Program.ActiveScopeBuffer[^1][define_id.ctx] = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
                                    {"", (define_ctx, default, default)}
                                }, AssembleTimeTypes.CEXP, AccessLevels.PUBLIC);
                            
                            
                                return default;
                        
                            case "undefine":
                                if (CheckDirectiveMalformed()) return default;
                                Step();

                                if (ActiveToken.ctx[0] != ' ') {
                                    // error malformed
                                    return default;
                                }

                                if (CheckDirectiveMalformed()) return default;
                                Step();

                                (_, success) = GetObjectFromAlias(ActiveToken.ctx, Program.ActiveScopeBuffer[^1], AccessLevels.PUBLIC);
                                if (success) {
                                    Program.LabelDataBase.Remove(ActiveToken.ctx);
                                    return (OperationTypes.DIRECTIVE, Directives.UNDEFINE);
                                } else {
                                    // error - no define to undefine
                                    return default;
                                }

                            case "rom":
                                // #rom <CINT>/<INT>
                                return (OperationTypes.DIRECTIVE, Directives.ROM);
                        
                            case "cpu":
                                // #cpu <CINT>/<INT>
                                return (OperationTypes.DIRECTIVE,  Directives.CPU);
                        }
                    }

                    // keywords
                
                    // some do NOT require evaluation
                    // memory reservation simply wants to capture a label, check it and check line formatting.
                    var MemoryMode = Program.ActiveScopeTypeBuffer[^1] switch {
                        ScopeTypes.Macro => Memory.MemoryModes.FAST,
                        ScopeTypes.Root  => Memory.MemoryModes.SYSTEM,
                        ScopeTypes.Bank  => Memory.MemoryModes.SYSTEM,
                    
                        _                => Memory.MemoryModes.SLOW
                    };
                    string      Alias;
                
                    switch (ActiveToken.ctx) {
                        // functions ... typeof()
                        case "if":     // (bool) code  OR  (bool) {code block}
                        case "else":   // else if (bool code) or (bool) {code block}
                        case "loop":   // loop {code block}
                        case "break":  // exit loop
                        case "return": // return from macro
                        case "del":    // delete RT or AT variable

                        // AssembleTime Variables   : could be macro return or declaration
                        case "bank":
                        case "proc":
                        case "interrupt":
                        case "int":
                        case "string":
                        case "register":
                        case "flag":
                        case "const":
                            // const int, const string ... etc  (macro return type OR constant declaration)
                            break;
                    
                        case "system":
                        case "direct":
                        case "program":
                        case "mapper":
                        case "fast":
                        case "slow":    
                            MemoryMode = ActiveToken.ctx switch {
                                "direct"  => Memory.MemoryModes.DIRECT, 
                                "system"  => Memory.MemoryModes.SYSTEM, 
                                "mapper"  => Memory.MemoryModes.MAPPER, 
                                "program" => Memory.MemoryModes.PROGRAM, 
                                "fast"    => Memory.MemoryModes.FAST, 
                                "slow"    => Memory.MemoryModes.SLOW, 
                                _         => throw new NotSupportedException()
                            };
                            seek_no_whitespace();
                            (ctx, success) = ParseAsVariable(); // ctx is boxed anonymous RunTimeVariable type
                            if (!success) {
                                // error : invalid variable type
                                return default;
                            }
                            seek_no_whitespace();
                            Alias = ActiveToken.ctx;
                            seek_no_whitespace();
                            if (CheckLineTerminated()) {
                                if (!Memory.TryReserve(MemoryMode, (int)((RunTimeVariableType)ctx).size)) return default;  // error pass back
                                return (OperationTypes.RUNTIME, (MemoryMode, ctx, Alias));
                            } else if (ActiveToken.ctx[0] != '=') {
                                // error malformed instruction
                                return default;
                            } else {
                                Step();
                                seek_no_whitespace();
                                // evaluate rightside
                                break;
                            }
                    }

                    (ctx, success) = ParseAsVariable();
                    if (success) return (OperationTypes.KEYWORD, ctx);


                    if (ActiveToken.ctx == "-:") {
                        Step();

                        // next branch back is here | when we need to "beq -" we now can
                        return (OperationTypes.ANON_REL_BRANCH, '-');
                    } else if (ActiveToken.ctx == "+:") {
                        Step();

                        // for terms waiting on the next forward branch, we can now solve for "beq +"
                        return (OperationTypes.ANON_REL_BRANCH, '+');
                    }

                    //(ctx, success) = ParseAsFilter();
                    //if (success) return (OperationTypes.KEYWORD, ctx);


                    var opcode = ActiveToken;

                    // Gather Instruction Information
                    var FIS_ctx = VerifyInstruction();

                    if (FIS_ctx == default) return default;                 // error pass back
                    if (FIS_ctx == OperandDecorators.Missing)          return (OperationTypes.EVALUATE, 0);
                    else return (OperationTypes.INSTRUCTION, (opcode, FIS_ctx));

                    #region         OperationExtract Local Functions

                    string LibGetPathFromContext() {
                        var fp                    = string.Empty;
                        var InitialCharacterCount = 0u;

                        for (; !CheckLineTerminated(); Step()) {
                            fp += ActiveToken;
                            switch (ActiveToken.ctx[0]) {
                                case '<':
                                    InitialCharacterCount++;
                                    continue;
                            
                                case '>':
                                    if (--InitialCharacterCount == 0) break;
                                    continue;
                            
                                default: continue;
                            }
                        
                            break;
                        }

                        if (ActiveToken.ctx[0] != '>') {
                            return string.Empty;
                        } else {
                            return InitialCharacterCount == 0 ? fp : string.Empty;   
                        }
                    }
                
                    bool CheckDirectiveMalformed() {
                        if (CheckLineTerminated()) {
                            // error malformed
                            return true;
                        }

                        return false;
                    }
                
                    (RunTimeVariableType ctx, bool succses) ParseAsVariable() {
                        var type = ActiveToken.ctx.ToLower();
                        if (type.Length < 2) return default;
                        RunTimeVariableType ctx = default;

                        ctx.signed = type[0]       == 'i';
                        if (!ctx.signed && type[0] != 'u') return default;
                    

                        var substring = 2;
                        if      (type[1] == 'b') ctx.endian = true;
                        else if (type[1] != 'l') substring  = 1;

                        if (substring == type.Length) return default;
                        if (!uint.TryParse(type[substring..], out ctx.size)) return default;
                    

                        if ((ctx.size & 0b111) > 0) return default;
                    

                        ctx.size >>= 3;
                        if (ctx.size == 0u) return default;
                        return (ctx, true);
                    }

                    (RunTimeVariableFilterType ctx, bool succses) ParseAsFilter() {
                        var type = ActiveToken;
                        if (type.StringLength < 2) return default;
                        RunTimeVariableFilterType ctx = default;

                        if (type.ctx == "num") return (default, true);

                        uint size = 0;

                        if      (type.ctx[0] == 'i') ctx.signed = true;  // ix, ilx, ibx
                        else if (type.ctx[0] == 'u') ctx.signed = false; // ux, ulx, ubx
                        else if (type.ctx[0] == 'l') ctx.endian = false; // lx, l16, l32, l64..
                        else if (type.ctx[0] == 'b') ctx.endian = true;  // bx, b16, b32, b64
                        else if (type.ctx[0] == 'x') {
                            if (!uint.TryParse(type.ctx[1..], out size))return default;
                            if ((size & 0b111) > 0)                     return default;           // impossible size
                            if (size           == 0u)                             return default; // impossible size
                        } else                                          return default;

                        if      (type.ctx[1] == 'l') {                                      // ilx, ulx
                            if (ctx.endian != null)                     return default;
                            else ctx.endian = false;
                        }
                        else if (type.ctx[1] == 'b') {                                      // ibx, ubx
                            if (ctx.endian != null)                     return default;
                            else ctx.endian = true;
                        } else if (type.ctx[1] == 'x') {                                    // ix, ux, bx, lx
                            if (!uint.TryParse(type.ctx[2..], out size))return default;
                            if ((size & 0b111) > 0)                     return default;     // impossible size
                            if (size           == 0u)                             return default;     // impossible size
                            ctx.size = size >> 3;                       return (ctx, true); // specified size (one type allowed, exclusive filter)
                        } else                                          return default;

                        if (type.StringLength < 3)                        return default;     // il, bl, ul and ub are not valid
                        if (type.ctx[2] == 'x')                         return (ctx, true); // null size

                        if (type.StringLength == 3)                       return default;
                        if (!uint.TryParse(type.ctx[2..], out size))    return default;
                        if ((size & 0b111) > 0)                         return default;     // impossible size
                        if (size           == 0u)                                 return default;     // impossible size
                        ctx.size = size >> 3;                           return (ctx, true); // specified size (one type allowed, exclusive filter)
                    }

                
                    OperandDecorators VerifyInstruction() {
                        string[] CanUseA = ["asl", "lsr", "rol", "ror"];    // asl a, lsr a, rol a and ror a are all valid instructions with a as operand, no others.
                        /*
                         * ldr  (ld) + check for r
                         * str  (st) + check for r
                         *
                         * tra  (t)  + check for r
                         * trx  (t)  + check for r
                         * try  (t)  + check for r
                         * tar  (ta) + check for r
                         * tyr  (ty) + check for r
                         * txr  (tx) + check for r
                         * tir  (t)  + check for i + check for r
                         *      check if i == r
                         *
                         * bfc  (b)  + check for f
                         * bfs  (b)  + check for f
                         *      f must be flag type
                         *
                         * inr  (in) + check for r
                         *      r must be:
                         *          indexing flag type
                         *          memory location
                         *          preprocessor INT
                         *
                         * der (de)  + check for r
                         *      r must be:
                         *          indexing flag type
                         *          memory location
                         *          preprocessor INT
                         *
                         #1#

                        var opcode = ActiveToken.ctx.ToLower();
                        switch (opcode) {
                            #region     Explicit Instructions Supporting Immediate
                            case "cpa": // cmp
                            case "cpx":
                            case "cpy":
                            case "adc":
                            case "and":
                            case "cmp":
                            case "eor": // #
                            case "lda": // !z:
                            case "ldx": // !a:
                            case "ldy": // a:
                            case "ora": // z:
                            case "bgt":
                            case "blt":
                            case "bpl":
                            case "bmi":
                            case "bcc":
                            case "bcs":
                            case "bnc":
                            case "bns":
                            case "bvc":
                            case "bvs":
                            case "bzc": // #foo
                            case "bzs": // foo
                            case "bit":
                                if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                                // error malformed instruction
                                return default;
                            #endregion  Explicit Instructions Supporting Immediate
                            #region     Explicit Instructions Not Supporting Immediate
                            case "sta":
                            case "stx": // !z:
                            case "sty": // !a:
                            case "dec": // a:
                            case "inc": // z:
                            case "jmp":
                            case "jeq":
                            case "jne":
                            case "jzs":
                            case "jzc":
                            case "jpl":
                            case "jmi":
                            case "jns":
                            case "jnc":
                            case "jcs":
                            case "jcc":
                            case "jgt":
                            case "jlt":
                            case "jvc":
                            case "jvs":
                            case "jsr":
                            case "ceq":
                            case "cne":
                            case "czs":
                            case "czc":
                            case "cpl":
                            case "cmi":
                            case "cns":
                            case "cnc":
                            case "ccs":
                            case "ccc":
                            case "cgt":
                            case "clt":
                            case "cvc": // jmp !foo
                            case "cvs": // jmp foo
                            case "aso":
                            case "slo":
                            case "rla":
                            case "rln":
                            case "lse":
                            case "sre":
                            case "rrd":
                            case "rra":
                            case "aax":
                            case "sax":
                            case "dcm":
                            case "dcp":
                            case "usb":
                            case "isc":
                            case "axa":
                            case "ahx":
                            case "sha":
                            case "sxa":
                            case "xas":
                            case "shx":
                            case "sya":
                            case "say":
                            case "shy": // !foo
                            case "shs": // !z:foo
                            case "tas": // !a:foo
                            case "lar": // z:foo
                            case "las": // a:foo
                                if (CheckFormat(false)) goto CheckMemoryAccessRulesNotPermittingImmediate;
                                // error malformed instruction
                                return default;
                            #endregion  Explicit Instructions Not Supporting Immediate
                            #region     Explicit Instructions Not Supporting Immediate, Supporting Implied A
                            case "asl":
                            case "lsr": // !z:
                            case "rol": // !a:
                            case "ror": // a:
                            case "irl": // z:
                            case "irr": //
                                if (CheckFormat(true)) {
                                    if (CheckLineTerminated()) return OperandDecorators.Found;
                                    goto CheckMemoryAccessRulesNotPermittingImmediate;
                                }
                                // error malformed instruction
                                return default;
                            #endregion  Explicit Instructions Not Supporting Immediate, Supporting Implied A
                            #region     Explicit Implied Instructions Supporting Overrule
                            case "kil":
                            case "hlt":
                            case "jam": // !
                            case "stp": // 
                            case "clc":
                            case "clv": // !
                            case "sec": //      Implied or Implied Overruled\
                                if (CheckFormat(true)) {
                                    if (CheckLineTerminated()) {
                                        if (ActiveToken.ctx[0] == '!') return OperandDecorators.Overruled;
                                        else return OperandDecorators.Found;
                                    } else if (ActiveToken.ctx[0] == '!') {
                                        Step();
                                        if (CheckLineTerminated()) return OperandDecorators.Overruled;
                                        seek_no_whitespace();
                                        if (CheckLineTerminated()) return OperandDecorators.Overruled;
                                        // malformed instruction
                                        return default;
                                    }
                                }
                                // error malformed instruction
                                return default;
                            #endregion  Explicit Implied Instructions Supporting Overrule
                            #region     Explicit Immediate Instructions
                            case "neg":
                                if (CheckFormat(false)) goto CheckImmediate;
                                // error malformed instruction
                                return default;

                            case "ana":
                            case "asb":
                            case "anc":
                            case "asr":
                            case "alr":
                            case "sbx":
                            case "xma":
                            case "arr":
                            case "axs":
                            case "axm":
                            case "ane": // !#foo
                            case "xaa": // #foo
                                if (CheckFormat(false)) {
                                    if (ActiveToken.ctx[0] == '!') {
                                        if (CheckLineTerminated()) return default;
                                        Step();
                                        if (ActiveToken.ctx[0] == '#') return OperandDecorators.Overruled | OperandDecorators.Immediate;
                                        else return OperandDecorators.Immediate;
                                    }
                                    goto CheckImmediate;
                                }
                                // error malformed instruction
                                return default;
                            #endregion  Explicit Immediate Instructions
                            #region     Explicit Implied Instructions Not Supporting Overrule
                            case "txy": 
                            case "tyx": 
                            case "tax":
                            case "tay":
                            case "tsx":
                            case "txa":
                            case "txs":
                            case "dex":
                            case "dey":
                            case "inx":
                            case "iny":
                            case "pha":
                            case "php":
                            case "pla":
                            case "plp":
                            case "sei":
                            case "rti":
                            case "rts":
                            case "ccf":
                            case "sex":
                            case "abs":
                            case "rnc":
                            case "rns":
                            case "rpl":
                            case "rmi":
                            case "rvc":
                            case "rvs":
                            case "rcc":
                            case "rlt":
                            case "rcs":
                            case "rgt":
                            case "req":
                            case "rzs":
                            case "rne":
                            case "rzc": //
                                if (CheckFormat(true)) return CheckLineTerminated() ? OperandDecorators.Found: default;
                                // error malformed instruction
                                return default;
                            #endregion  Explicit Implied Instructions Not Supporting Overrule


                            case "nop":
                                if (CheckFormat(true)) goto CheckMemoryAccessRulesWithImmediate;
                                // error malformed instruction
                                return default;
                            // brk !
                            // brk #foo
                            case "brk": // brk  : Immediate OR Implied OR Implied (Overruled)
                                if (CheckFormat(true)) {
                                    if (ActiveToken.ctx[0]      == '!') return OperandDecorators.Overruled;
                                    else if (ActiveToken.ctx[0] == '#') goto CheckImmediate;
                                    else if (CheckLineTerminated()) return OperandDecorators.Found;
                                    return default;
                                }
                                // error malformed instruction
                                return default;

                            // !#foo
                            // #foo
                            // !foo
                            // !z:foo
                            // !a:foo
                            // z:foo
                            case "lax": // a:foo
                                // TODO: write some lax safety

                                if (CheckFormat(false)) {
                                    if (ActiveToken.ctx[0] == '!') {
                                        Step();
                                        if (ActiveToken.ctx[0] == '#') return OperandDecorators.Overruled | OperandDecorators.Immediate;
                                        else return default;
                                    }
                                    goto CheckMemoryAccessRulesWithImmediate;
                                }
                                // error malformed instruction
                                return default;

                                CheckImmediate:
                                if (ActiveToken.ctx[0] == '!') {
                                    seek_no_whitespace();
                                    if (CheckLineTerminated()) return default;

                                    if (ActiveToken.ctx[0] != '#') {
                                        // error, instruction is immediate only
                                        return default;
                                    }

                                    Step();
                                    return OperandDecorators.Immediate | OperandDecorators.Overruled;
                                }

                                if (ActiveToken.ctx[0] != '#') {
                                    // error, instruction is immediate only
                                    return default;
                                }

                                Step();
                                return OperandDecorators.Immediate;

                                CheckMemoryAccessRulesNotPermittingImmediate:
                                if (ActiveToken.ctx[0] == '#') {
                                    // error, does not permit immediate
                                    return default;
                                }

                                goto CheckMemoryAccessRules;

                                CheckMemoryAccessRulesWithImmediate:
                                if (ActiveToken.ctx[0] == '#') {
                                    Step();
                                    return OperandDecorators.Immediate;
                                }

                                goto CheckMemoryAccessRules;

                                CheckMemoryAccessRules:
                                return CheckMemoryAccessRules();
                        }

                        #region     Implicit Instruction Checking
                        switch (opcode[..2]) {

                            case "ld":
                            case "cp":
                                switch (opcode[2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error : ldc, ldn, ldv and ldz are forbidden terms.
                                        // ldc   : lda #$00, rol            3 bytes, 4 cycles
                                        // ldn   : rol, rol, lda #$00       4 bytes, 6 cycles
                                        // ldv   : nothing
                                        // ldz   : lda #$00                 2 bytes, 2 cycles
                                        return default;
                                }

                                if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                                // error malformed instruction
                                return default;


                            case "st":
                                switch (opcode[2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error not an instruction
                                        // stz   : ldr #$00, str tar    (uses whichever reg is provably zero if reg awareness enabled, otherwise fails)
                                        return default;
                                }

                                if (CheckFormat(false)) goto CheckMemoryAccessRulesNotPermittingImmediate;
                                // error malformed instruction
                                return default;

                            case "ta":
                                switch (opcode[2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                    case 'a':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        if (CheckFormat(true)) return OperandDecorators.Found;
                                        // error malformed instruction
                                        return default;
                                }
                            case "tx":
                                switch (opcode[2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                    case 'x':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        if (CheckFormat(true)) return OperandDecorators.Found;
                                        // error malformed instruction
                                        return default;
                                }

                            case "ty":
                                switch (opcode[2]) {
                                    case 'c':
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                    case 'y':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        if (CheckFormat(true)) return OperandDecorators.Found;
                                        // error malformed instruction
                                        return default;
                                }

                            case "in":
                            case "de":
                                switch (opcode[2]) {
                                    case 'n':
                                    case 'v':
                                    case 'z':
                                        // error not an instruction
                                        // tac   : cmp #$01 ?
                                        return default;

                                    default:
                                        if (CheckFormat(true)) return OperandDecorators.Found;
                                        // error malformed instruction
                                        return default;
                                }

                            default:
                                switch (opcode[0]) {
                                    case 'b':
                                    case 'r':
                                        switch (opcode[1]) {
                                            case 'a':
                                            case 'x':
                                            case 'y':
                                                return default;
                                        }

                                        switch (opcode[2]) {
                                            case 's':
                                            case 'c':
                                                break;

                                            default: return default;
                                        }

                                        if (CheckFormat(false)) goto CheckMemoryAccessRulesWithImmediate;
                                        // error malformed instruction
                                        return default;

                                    case 't':
                                        if (opcode[1] == opcode[2]) {
                                            // error nothing to do not an instruction
                                            return default;
                                        }

                                        var isFlag = (char c) => c switch { 'c' or 'n' or 'v' or 'z' => true, _ => default };

                                        if (isFlag(opcode[1]) || isFlag(opcode[2])) {
                                            // error not an instruction
                                            return default;
                                        }

                                        if (CheckFormat(true)) return OperandDecorators.Found;
                                        // error malformed instruction
                                        return default;


                                    case 'c':
                                    case 'j':
                                        switch (opcode[1]) {
                                            case 'a':
                                            case 'x':
                                            case 'y':
                                                return default;
                                        }

                                        switch (opcode[2]) {
                                            case 's':
                                            case 'c':
                                                break;

                                            default: return default;
                                        }

                                        if (CheckFormat(false)) goto CheckMemoryAccessRulesNotPermittingImmediate;
                                        // error malformed instruction
                                        return default;

                                }
                                return OperandDecorators.Missing; ;

                                CheckMemoryAccessRulesNotPermittingImmediate:
                                if (ActiveToken.ctx[0] == '#') {
                                    // error, does not permit immediate
                                    return default;
                                }

                                goto CheckMemoryAccessRules;

                                CheckMemoryAccessRulesWithImmediate:
                                if (ActiveToken.ctx[0] == '#') {
                                    Step();
                                    return OperandDecorators.Immediate;
                                }

                                goto CheckMemoryAccessRules;


                                CheckMemoryAccessRules:
                                return CheckMemoryAccessRules(); ; // rules
                        }
                        #endregion  Implicit Instruction Checking

                        bool CheckFormat(bool SupportsImplied) {
                            if (CheckLineTerminated())      return SupportsImplied;
                            Step();
                            if (TokenIndex == BasicRegexTokens.Length && DefineResolveBuffer.Count == 0)       
                                return SupportsImplied;                                                     // implied may complete source (might be fail later)
                            if (CheckLineTerminated())      return SupportsImplied;                         // implied may complete line
                            if (ActiveToken.ctx[0] != ' ')      return false;                                   // if space does not follow opcode, fail
                            seek_no_whitespace();
                            return true;                                                                    // otherwise its safe to interpret
                        }

                        OperandDecorators CheckMemoryAccessRules() {
                            if (ActiveToken.ctx == "a") {
                                if (CanUseA.Contains(opcode)) {
                                    Step();
                                    return OperandDecorators.Found;
                                }

                                Step();
                                if (CheckLineTerminated()) {                                                // we've already established we can't use the syntax a here
                                    // error malformed
                                    return default;
                                }
                            
                                if (ActiveToken.ctx[0] == ':') {
                                    Step();
                                    return OperandDecorators.Enforced_ABS;
                                } else {
                                    // error, a is reserved
                                    return default;
                                }
                            } else if (ActiveToken.ctx == "z") {
                                Step();
                                if (CheckLineTerminated()) return default;                                  // standalone z refers to zero flag, not implicit and unsuitable here
                                if (ActiveToken.ctx[0] == ':') {
                                    Step();
                                    return OperandDecorators.Enforced_ZP;
                                } else {
                                    // error, z is reserved
                                    return default;
                                }
                            } else if (ActiveToken.ctx[0] == '!') {
                                Step();
                                if (CheckLineTerminated()) {                                                // must overrule something
                                    // error malformed
                                    return default;
                                }

                                if (ActiveToken.ctx == "a") {
                                    Step();
                                    if (ActiveToken.ctx[0] == ':') {
                                        Step();
                                        return OperandDecorators.Enforced_ABS | OperandDecorators.Overruled;
                                    } else {
                                        // error, a is reserved
                                        return default;
                                    }
                                } else if (ActiveToken.ctx == "z") {
                                    Step();
                                    if (ActiveToken.ctx[0] == ':') {
                                        Step();
                                        return OperandDecorators.Enforced_ZP | OperandDecorators.Overruled;
                                    } else {
                                        // error, z is reserved
                                        return default;
                                    }
                                } else return OperandDecorators.Overruled;
                            }

                            return OperandDecorators.Found;
                        }
                    }


                    #endregion      OperationExtract Local Functions
                }*/
            
                bool CheckLineTerminated() => (TokenIndex == BasicRegexTokens.Length && DefineResolveBuffer.Count == 0) || ActiveToken.ctx[0] == ';' || ActiveToken.ctx[0] == '\n' || ActiveToken.ctx == "//" || ActiveToken.ctx == "/*";

                // keep seeking beyond whitespace
                void seek_no_whitespace(bool skip = false, bool regexParse = true) => Steps(() => !CheckLineTerminated() && (ActiveToken.ctx[0] == ' ' || ActiveToken.ctx[0] == '\t'), skip, regexParse);

            }


            internal static (string filepath, bool success) CheckInclude(string target) {
                foreach (var search in Program.SourceFileSearchPaths) {
                    #if DEBUG
                    var fullPath = Path.GetFullPath(Path.Combine(Environment.CurrentDirectory, search, target));
                    #else
                        var fullPath = Path.Combine(AppContext.BaseDirectory, search, target);
                    #endif
                    
                    if (File.Exists(fullPath)) return (fullPath, true);
                    
                }
                return default;
            }

            /// <summary>
            /// Add Tokenized Source to Buffer
            /// </summary>
            /// <param name="FilePath"></param>
            internal static void AddSourceContext(string FilePath) {
                Program.SourceFileNameBuffer.Add(FilePath);
                Program.SourceFileContentBuffer.Add(RegexTokenize(File.ReadAllText(FilePath)));
                Program.SourceTokenIndexBuffer.Add(0);
                Program.SourceFileIndexBuffer.Add(0);
            }


            // Generated Function | However I do find that this function is how I would code and meets criteria
            internal static Dictionary<TKey, TValue> Clone<TKey, TValue>(Dictionary<TKey, TValue> Source) where TKey : notnull {
                var clone = new Dictionary<TKey, TValue>(Source.Count);
                foreach (var kv in Source) {
                    var keyClone   = Clone(kv.Key);
                    var valueClone = Clone(kv.Value);
                    clone[keyClone] = valueClone;
                }
                return clone;
            }

            internal static T Clone<T>(T ctx) => ctx switch {
                ICloneable c        => (T)c.Clone(),
                string or ValueType => ctx,
                #if DEBUG
                _ => throw new NotSupportedException($"Cannot clone type {ctx?.GetType()}")
                #else
                _ => throw new NotSupportedException($"FATAL ERROR :: (REPORT THIS ON THE GITHUB) CANNOT CLONE TYPE {ctx?.GetType()}")
                #endif
            };
            internal enum Unary : byte {
                INC,
                DEC,
                ABS,
                NEG,
                BIT,
                NOT
            };

            internal static bool IsNonLiteral(char First) =>
                First switch {
                    '0' or '1' or '2' or '3' or '4' or '5' or '6' or '7' or '8' or '9' or '$' or '%' or '&' or '+' or '-' or '!' or '^' or '*' or '[' or ']' or '{' or '}' or '\'' or '#' or '~' or ':' or ',' or '<' or '.' or '>' or '/' or '?' => false,
                    _ => true,
                };


            internal static readonly string[] Reserved = [
                // directives
                "cpu", "rom", "define", "undefine", "include", "assert",
                
                // assemble time types
                "ref", "void", "int", "string", "exp", "bank", "proc", "reg", "flag", "scope", "namespace",
                
                // conditional assembly
                "if", "else", "loop", "break", "return",
                
                // runtime type filters
                "ux", "ix", "lx", "bx", "ulx", "ilx", "ubx", "ibx", "num", 
                "x8", "x16", "x24", "x32", "x64", "l16", "l24", "l32", "l64", "b16", "b24", "b32", "b64",
            
                // runtime type memory adjectives
                "direct", "system", "program", "mapper", "field", "slow", "fast",

                // runtime types
                "u8", "i8", "u16", "i16", "u24", "i24", "u32", "i32", "u64", "i64",
                "ub16", "ib16", "ub24", "ib24", "ub32", "ib32", "ub64", "ib64",

                // Operators
                // Assignment
                "=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", ">>=", "<<=",

                // Unary
                "==", "!=", "<=", ">=", ">", "<",

                // LLambda
                "=>",

                // Null Coalescence
                "??", "??=", "?.",

                // Math
                "+", "-", "*", "/", "%", "&", "|", "^", "~", "(", ")",

                // Indexing
                ",", "[", "]",

                // Numerical Systems
                "%", "$", "£", "0x", "0b", "0o",

                // Control
                ";", ":", "#", "\\", "\"", "{", "}", "?", ">", "<", "!", ".", ","
            ];

            internal static class Terminal {
                internal enum Responses : byte {
                    Terminate_Error,
                    Terminate_Success,
                    Proceed,
                }
                
                
                internal static (string InputPath, string OutputPath, Responses Response) Parse(string[] args) {
                    string InputPath   = "", OutputPath = "";
                    var    StringIndex = 0;
                    var    Flattened   = string.Join(" ", args);

                    var Response = Responses.Proceed;
                    Program.WarningLevel = WarningLevels.NONE;

                    var LoadedConfig = false;
                    var CWDSet       = false;

                    for (var i = 0; i < args.Length; i++) {
                        StringIndex += args[i].Length;

                        switch (args[i]) {
                            case "-i":
                            case "--input":
                                if (i == args.Length - 1) {
                                    if (!LoadedConfig) LoadConfig();
                                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "No Input Path Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                                    return default;
                                }  
                                
                                if (InputPath.Length > 0) {
                                    if (!LoadedConfig) LoadConfig();
                                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Input Source File Path has already been specified")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                                    return default;
                                }
                                
                                InputPath = args[++i];
                                break;

                            case "-o":
                            case "--output":
                                if (i == args.Length - 1) {
                                    if (!LoadedConfig) LoadConfig();
                                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "No Output Path Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                                    return default;
                                }  
                                
                                if (OutputPath.Length > 0) {
                                    if (!LoadedConfig) LoadConfig();
                                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Output Binary File Path has already been specified")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                                    return default;
                                }
                                OutputPath = args[++i];
                                break;

                            case "-d":
                            case "--directory":
                                if (i == args.Length - 1) {
                                    if (!LoadedConfig) LoadConfig();
                                    // error: no cwd provided
                                    return default;
                                }

                                if (CWDSet) {
                                    if (!LoadedConfig) LoadConfig();
                                    // error: cwd already set
                                    return default;
                                }
                                
                                Environment.CurrentDirectory = args[++i];
                                CWDSet                       = true;
                                break;
                            
                            case "-c":
                            case "--config":
                                if (i == args.Length - 1) {
                                    // error: no path for config
                                    return default;
                                }
                                
                                LoadedConfig = LoadConfig(args[++i]);
                                if (!LoadedConfig) {
                                    // error passback
                                    return default;
                                }
                                break;
                            
                            case "-w":
                            case "--warning":
                                if (i == args.Length - 1) {
                                    if (!LoadedConfig) LoadConfig();
                                    // error, no warning description detected
                                    return default;
                                } else if (Program.WarningLevel != WarningLevels.NONE) {
                                    if (!LoadedConfig) LoadConfig();
                                    // error, already described warning level
                                    return default;
                                }
                                
                                Program.WarningLevel = args[++i] switch {
                                    "i" or "ignore"     or "I" or "IGNORE"     => WarningLevels.IGNORE,
                                    "d" or "default"    or "D" or "DEFAULT"    => WarningLevels.DEFAULT,
                                    "e" or "error"      or "E" or "ERROR"      => WarningLevels.ERROR,
                                    "v" or "verbose"    or "V" or "VERBOSE"    => WarningLevels.VERBOSE,
                                    "s" or "strict"     or "S" or "STRICT"     => WarningLevels.STRICT,
                                    "c" or "controlled" or "C" or "CONTROLLED" => WarningLevels.CONTROLLED,

                                    _ => WarningLevels.NONE
                                };

                                if (Program.WarningLevel == WarningLevels.NONE) {
                                    if (!LoadedConfig) LoadConfig();
                                    // error : unrecognized warning level 
                                    return default;
                                }
                                break;

                            case "-h":
                            case "--help":
                                if (!LoadedConfig) LoadConfig();
                                Response = Responses.Terminate_Success;

                                if (i == args.Length - 1) {
                                    // generic help message
                                    Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                        $"""
                                         Numinous 2a03 - GPL V2 Brette Allen 2026

                                         -i | --input        | [path]    | {(Language.Language.Connectives[(Program.ActiveLanguage, "Entrypoint Source Assembly File")])}
                                         -o | --output       | [path]    | {(Language.Language.Connectives[(Program.ActiveLanguage, "Output ROM/Disk Binary Output")])}
                                         -h | --help         |           | {(Language.Language.Connectives[(Program.ActiveLanguage, "Display the help string (you did that)")])}
                                         -h | --help         | [arg]     | TODO: WRITE "GET INFO ON SPECIFIC ARGUMENT FUNCTION" HERE
                                         -l | --language     | [lang]    | {(Language.Language.Connectives[(Program.ActiveLanguage, "Choose a language to use")])}
                                         -w | --warning      | [level]   | TODO: Write "SET WARNING LEVEL" HERE
                                         -d | --directory    | [path]    | TODO: Write "SET CWD" HERE
                                         -c | --config       | [path]    | TODO: Write "CONFIG FETCH" HERE
                                                
                                         """,                                                     -1, default, null, null);
                                } else {
                                    // TODO: Add support for all new arguments
                                    switch (args[++i]) {
                                        default: 
                                            // error: cannot help with this : unrecognized context
                                            return default;

                                        case "l":
                                        case "lang":
                                        case "languages":
                                            // language specific help message.
                                            Log(ErrorTypes.None, DecodingPhases.TERMINAL, @"
English (UK)      ""-l en_gb""
English (US)      ""-l en_us""
Español           ""-l es""
Deutsch           ""-l de""
日本語            ""-l ja""
Français          ""-l fr""
Português         ""-l pt""
Русский           ""-l ru""
Italiano          ""-l it""
Nederlands        ""-l ne""
Polski            ""-l pl""
Türkçe            ""-l tr""
Tiếng Việt        ""-l vt""
Bahasa Indonesia  ""-l in""
Čeština           ""-l cz""
한국어            ""-l ko""
Українська        ""-l uk""
العربية           ""-l ar""
Svenska           ""-l sw""
فارسی             ""-l pe""
中文              ""-l ch""
", -1, default, null, null);
                                            break;

                                        case "w":
                                        case "warn":
                                        case "warnings":
                                            // warnings specific help message
                                            Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                                $"""
                                                 Numinous Warning Types and how they work

                                                 ignore      : Will not display any warnings, but track the quantity for after completion.
                                                 default     : Will warn the user about potential issues with their code.
                                                 error       : Will convert all errors into warnings, enforcing the user to fix all issues.
                                                 verbose     : Will display much more warnings, recommended and intended for those who wish to write perfect code.
                                                 strict      : Acts as 'verbose' but warnings become errors, not recommended.
                                                 controlled  : Acts as 'strict' but prevents overruling.
                                                        
                                                 """,                                                             -1, default, null, null);
                                            break;

                                        case "i":
                                        case "input":
                                            Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                                $"""
                                                 Numinous Input File

                                                 The input file argument (-i or --input) should be followed by a valid file path to a source assembly file. 
                                                 If the file is empty you will receive an error, you may only pass one file here as the entry point file.
                                                 This decides what the root of the "include path" is, includes from here must be relative to this path.
                                                        
                                                 """,                                                             -1, default, null, null);
                                            break;

                                        case "o":
                                        case "output":
                                            Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                                $"""
                                                 Numinous Output File

                                                 The output file argument (-o or --output) should be followed by a path pointing to a file to generate.
                                                 The file name must comply with the limits of your Operating System.
                                                 The directory the output file lives in must also already exist. 
                                                 If you wish to create an FDS Disk image, you must use the FDS Header variant as using the *.fds file extension
                                                 will not affect the kind of build produced. 

                                                 Numinous WILL overwrite a file existing with the same name at the output path if found.
                                                        
                                                 """,                                                             -1, default, null, null);
                                            break;
                                    }
                                }


                                break;

                            case "-l":
                            case "--language":
                                if (i == args.Length - 1) {
                                    if (!LoadedConfig) LoadConfig();
                                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "No Language Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                                    return default;
                                }

                                Program.ActiveLanguage = args[++i] switch {
                                    "en_gb" => Language.Languages.English_UK,
                                    "en_us" => Language.Languages.English_US,
                                    "es"    => Language.Languages.Spanish,
                                    "de"    => Language.Languages.German,
                                    "ja"    => Language.Languages.Japanese,
                                    "fr"    => Language.Languages.French,
                                    "pt"    => Language.Languages.Portuguese,
                                    "ru"    => Language.Languages.Russian,
                                    "it"    => Language.Languages.Italian,
                                    "ne"    => Language.Languages.Dutch,
                                    "pl"    => Language.Languages.Polish,
                                    "tr"    => Language.Languages.Turkish,
                                    "vt"    => Language.Languages.Vietnamese,
                                    "in"    => Language.Languages.Indonesian,
                                    "cz"    => Language.Languages.Czech,
                                    "ko"    => Language.Languages.Korean,
                                    "uk"    => Language.Languages.Ukrainian,
                                    "ar"    => Language.Languages.Arabic,
                                    "sw"    => Language.Languages.Swedish,
                                    "pe"    => Language.Languages.Persian,
                                    "ch"    => Language.Languages.Chinese,

                                    _       => Language.Languages.Null
                                };

                                if (Program.ActiveLanguage == Language.Languages.Null) {
                                    if (!LoadedConfig) LoadConfig();
                                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Invalid Language Provided")])}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                                    return default;
                                }
                                break;

                            default:
                                if (!LoadedConfig) LoadConfig();
                                Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{(Language.Language.Connectives[(Program.ActiveLanguage, "Unrecognized Terminal Argument")])}.", -1, default, ApplyWiggle(Flattened, 1 + StringIndex, args[i].Length), null);
                                return default;
                        }
                    }

                    if (!LoadedConfig) LoadedConfig = LoadConfig();
                    return LoadedConfig ? (InputPath, OutputPath, Response) : default;

                    static bool LoadConfig(string? path = null) {
                        #if DEBUG
                        path ??= Environment.CurrentDirectory;
                        #else
                        path ??= AppContext.BaseDirectory;
                        #endif
                        if (!File.Exists($"{path}/Numinous.toml")) {
                            File.WriteAllText($"{path}/Numinous.toml", """
                                                                       [Defaults]
                                                                       DefaultLanguage             = "System"
                                                                       DefaultWarningLevel         = "Default"

                                                                       [Paths]
                                                                       LibraryIncludePaths         = ["./lib"]
                                                                       """);
                        }

                        var Config = Toml.ToModel<NuminousConfigTomlTemplate>(
                                                                              File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "Numinous.toml")),
                                                                              null,
                                                                              new TomlModelOptions { ConvertPropertyName = name => name }
                                                                             );

                        #region Warning level from Config TOML
                        if (Program.WarningLevel == WarningLevels.NONE) Program.WarningLevel = Config.Defaults.DefaultWarningLevel switch {
                            "Ignore"     => WarningLevels.IGNORE,
                            "Default"    => WarningLevels.DEFAULT,
                            "Error"      => WarningLevels.ERROR,
                            "Verbose"    => WarningLevels.VERBOSE,
                            "Strict"     => WarningLevels.STRICT,
                            "Controlled" => WarningLevels.CONTROLLED, 

                            _               => WarningLevels.NONE // mark to fix toml
                        };
                       
                        if (Program.WarningLevel == WarningLevels.NONE) {
                            Warn(ErrorTypes.SyntaxError, DecodingPhases.TERMINAL, $"""
                                      The config file (at {AppContext.BaseDirectory}/Numinous.toml) is malformed! 
                                      Ensure that it contains the key 'DefaultWarningLevel' under 'Defaults' table. The data may be any of the following:

                                      Ignore                  : By default will ignore all warnings, great for sloppy vibe coding with minimal output.
                                      Default                 : Provides few errors and doesn't halt your workflow
                                      Error                   : Treats warning as errors, not recommended but does enforce clean code.
                                      Verbose                 : Shows more warnings, even those which are harmless.
                                      Strict                  : Shows more warnings as errors, not recommended but does enforce clean code.
                                      Controlled              : Functions like Strict but prevents use of overrides. 

                                      Project Numinous will NOT continue until you fix this or manually specify your Warning Level!
                                      """, default, default, default, null);
                            return false;
                        }
                        #endregion Warning level from Config TOML

                        #region Default Langauge from Config TOML
                        if (Program.ActiveLanguage == Language.Languages.Null) Program.ActiveLanguage = Config.Defaults.DefaultLanguage switch {
                            "English UK" => Language.Languages.English_UK,
                            "English US" => Language.Languages.English_US,
                            "Spanish"    => Language.Languages.Spanish,
                            "German"     => Language.Languages.German,
                            "Japanese"   => Language.Languages.Japanese,
                            "French"     => Language.Languages.French,
                            "Portuguese" => Language.Languages.Portuguese,
                            "Russian"    => Language.Languages.Russian,
                            "Italian"    => Language.Languages.Italian,
                            "Dutch"      => Language.Languages.Dutch,
                            "Polish"     => Language.Languages.Polish,
                            "Turkish"    => Language.Languages.Turkish,
                            "Vietnamese" => Language.Languages.Vietnamese,
                            "Indonesian" => Language.Languages.Indonesian,
                            "Czech"      => Language.Languages.Czech,
                            "Korean"     => Language.Languages.Korean,
                            "Ukrainian"  => Language.Languages.Ukrainian,
                            "Arabic"     => Language.Languages.Arabic,
                            "Swedish"    => Language.Languages.Swedish,
                            "Persian"    => Language.Languages.Persian,
                            "Chinese"    => Language.Languages.Chinese,

                            "System" => Language.Language.CaptureSystemLanguage(),
                            _        => Language.Languages.Null
                        };

                        if (Program.ActiveLanguage == Language.Languages.Null) {
                            Warn(ErrorTypes.SyntaxError, DecodingPhases.TERMINAL, $"""
                                      The config file (at {AppContext.BaseDirectory}/Numinous.toml) is malformed! 
                                      Ensure that it contains the key 'DefaultLanguage' under 'Defaults' table. The data may be any of the following:

                                      English UK
                                      English US
                                      Spanish
                                      German
                                      Japanese
                                      French
                                      Portuguese
                                      Russian
                                      Italian
                                      Dutch
                                      Polish
                                      Turkish
                                      Vietnamese
                                      Indonesian
                                      Czech
                                      Korean
                                      Ukrainian
                                      Arabic
                                      Swedish
                                      Persian
                                      Chinese

                                      Project Numinous will NOT continue until you fix this or manually specify your language!
                                      """, default, default, default, null);
                            return false;
                        }
                        #endregion Default Langauge from Config TOML

                        Program.SourceFileSearchPaths = [.. Config.Paths.LibraryIncludePaths];

                        if (Program.SourceFileSearchPaths.Count == 0) {
                            // warn, no libraries at all (this is unusual, they should at least have the standard library)
                            return false;
                        }

                        return true;
                    }
                }

                internal class NuminousConfigTomlTemplate {
                    public class DefaultsBlock {
                        public string DefaultWarningLevel { get; set; } = "DefaultWarningLevel";
                        public string DefaultLanguage     { get; set; } = "DefaultLanguage";
                    }

                    public class PathsBlock {
                        public string[] LibraryIncludePaths { get; set; } = [];
                    }

                    public PathsBlock    Paths    { get; set; } = new();
                    public DefaultsBlock Defaults { get; set; } = new();
                }

                internal struct ErrorContext {
                    internal ErrorLevels    ErrorLevel;
                    internal ErrorTypes     ErrorType;
                    internal DecodingPhases DecodingPhase;
                    internal string         Message;
                    internal int            LineNumber, StepNumber;
                    internal Func<string?>  Context;
                    internal string?        ContextFileName;
                }

                // in event of left in message, don't show on release
                #if DEBUG
                internal static void Debug(string message) => Console.WriteLine(message);
                #else
                internal static void debug() {}
                #endif

                #if DEBUG
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? ContextFileName, string? Context,
                                               int     lineNumber = 0, 
                                               string  filePath = "", 
                                               string  memberName = "") {
                    #else
                internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    #endif
                
                    Console.ForegroundColor = ErrorLevel switch {
                        ErrorLevels.LOG   => ConsoleColor.Cyan, 
                        ErrorLevels.WARN  => ConsoleColor.Yellow, 
                        ErrorLevels.ERROR => ConsoleColor.Red, 
                        
                        _                   => ConsoleColor.White
                    };

                    string ErrorTypeString, ErrorTypeConnective, LocationString, DecodePhaseString;

                    if (ErrorType  == ErrorTypes.None) {
                        Console.WriteLine(Message);
                        goto Exit;
                    }

                    ErrorTypeString     = Language.Language.ErrorTypeMessages[(Program.ActiveLanguage, ErrorType)];
                    ErrorTypeConnective = Language.Language.Connectives[(Program.ActiveLanguage, "During")];
                    DecodePhaseString   = Language.Language.DecodePhaseMessages[(Program.ActiveLanguage, Phase)];
                    LocationString      = LineNumber == -1 ? "" : (StepNumber == 0 ? $"({LineNumber})" : $"({LineNumber}, {StepNumber})");
                    Context = Context == null ? "" : $": {Context}";
                    ContextFileName ??= "";

                    // Something Error During Something Phase :: Could not do a thing (1, 2) : ah, the issue is here.
                    #if DEBUG
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {ContextFileName} {LocationString}{Context}");
                    Console.WriteLine($"[{filePath}:{lineNumber}] {memberName}");
                    #else
                    Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {ContextFileName} {LocationString}{Context}");
                    #endif

                    Exit:
                    Console.ResetColor();
                }

                #if DEBUG
                internal static void Log(ErrorContext              ctx,
                                         [CallerLineNumber] int    lineNumber = 0,
                                         [CallerFilePath]   string filePath   = "",
                                         [CallerMemberName] string memberName = "") {
                    if (ctx.ErrorLevel != ErrorLevels.LOG)
                        throw new InvalidOperationException($"Log() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

                    WriteInfo(ErrorLevels.LOG, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context(),
                              ctx.ContextFileName, lineNumber, filePath, memberName);
                }

                internal static void Warn(ErrorContext              ctx,
                                          [CallerLineNumber] int    lineNumber = 0,
                                          [CallerFilePath]   string filePath   = "",
                                          [CallerMemberName] string memberName = "") {
                    var expectedLevel = Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? ErrorLevels.ERROR : ErrorLevels.WARN;
                    if (ctx.ErrorLevel != expectedLevel)
                        throw new InvalidOperationException($"Warn() called with mismatched ErrorLevel: {ctx.ErrorLevel}, expected: {expectedLevel}");

                    WriteInfo(expectedLevel, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context(),
                              ctx.ContextFileName, lineNumber, filePath, memberName);
                }

                internal static void Error(ErrorContext              ctx,
                                           [CallerLineNumber] int    lineNumber = 0,
                                           [CallerFilePath]   string filePath   = "",
                                           [CallerMemberName] string memberName = "") {
                    if (ctx.ErrorLevel != ErrorLevels.ERROR)
                        throw new InvalidOperationException($"Error() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

                    WriteInfo(ErrorLevels.ERROR, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context(),
                              ctx.ContextFileName, lineNumber, filePath, memberName);
                }


                internal static void   Log(ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? Context, string? ContextFileName,
                                           [CallerLineNumber] int lineNumber = 0,
                                           [CallerFilePath] string filePath = "",
                                           [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context, ContextFileName, lineNumber, filePath, memberName);
                

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? Context, string? ContextFileName,
                                           [CallerLineNumber] int lineNumber = 0,
                                           [CallerFilePath] string filePath = "",
                                           [CallerMemberName] string memberName = "") => WriteInfo(Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? ErrorLevels.ERROR : ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context, ContextFileName, lineNumber, filePath, memberName);


                internal static void Error(ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? Context, string? ContextFileName,
                                           [CallerLineNumber] int lineNumber = 0,
                                           [CallerFilePath] string filePath = "",
                                           [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context, ContextFileName, lineNumber, filePath, memberName);

                #else
                internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
                    WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context);
                }

                internal static void Log(ErrorContext ctx)
                {
                    if (ctx.ErrorLevel != ErrorLevels.LOG)
                        throw new InvalidOperationException($"Log() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

                    WriteInfo(ErrorLevels.LOG, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context());
                }

                internal static void Warn(ErrorContext ctx)
                {
                    var expectedLevel = ErrorLevels.WARN; // No error override in release
                    if (ctx.ErrorLevel != expectedLevel)
                        throw new InvalidOperationException($"Warn() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

                    WriteInfo(expectedLevel, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context());
                }

                internal static void Error(ErrorContext ctx)
                {
                    if (ctx.ErrorLevel != ErrorLevels.ERROR)
                        throw new InvalidOperationException($"Error() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

                    WriteInfo(ErrorLevels.ERROR, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context());
                }
                #endif
            }

            /// <summary>
            /// Database methods
            /// </summary>
            internal static class Database {
                /// <summary>
                /// Get without specifying target scope.
                /// 
                /// Order may be changed depending on Program.ObjectSearchBuffer. The ActiveScope is ALWAYS searched first, afterwards its down to this.
                /// By default the ObjectSearchBuffer only includes the root scope.
                /// </summary>
                /// <param name="Alias"></param>
                /// <param name="UsedAccessLevel"></param>
                /// <returns></returns>
                internal static ObjectToken? GetObjectFromAlias(string Alias, AccessLevels UsedAccessLevel) {
                    List<Dictionary<string, ObjectToken>> LocalObjectSearchBuffer = [Program.ActiveScopeBuffer[^1], .. Program.ObjectSearchBuffer];
                    return __GetObjectFromAlias(Alias, LocalObjectSearchBuffer, UsedAccessLevel);
                }
                /// <summary>
                /// Only check the specified scope, may be used like rs\foo. Note that the scope used to specify will be the result of the other method being used first.
                /// After this its hierarchy based and therefore rs\foo\foo may not always work.
                /// </summary>
                /// <param name="Alias"></param>
                /// <param name="TargetScope"></param>
                /// <param name="UsedAccessLevel"></param>
                /// <returns></returns>
                /// 
                internal static ObjectToken? GetObjectFromAlias(string Alias, Dictionary<string, ObjectToken> TargetScope, AccessLevels UsedAccessLevel) => __GetObjectFromAlias(Alias, [TargetScope], UsedAccessLevel);
                
                /// <summary>
                /// Internal function iterating over the LocalObjectSearchPath to find the required context if possible.
                /// </summary>
                /// <param name="Alias"></param>
                /// <param name="LocalObjectSearchBuffer"></param>
                /// <param name="UsedAccessLevel"></param>
                /// <returns></returns>
                private  static ObjectToken? __GetObjectFromAlias(string Alias, List<Dictionary<string, ObjectToken>> LocalObjectSearchBuffer, AccessLevels UsedAccessLevel) {
                    ObjectToken ctx;
                    var (found, error) = (false, false);
                    foreach (var LocalObjectSearchContainer in LocalObjectSearchBuffer) {
                        if (LocalObjectSearchContainer.TryGetValue(Alias, out ctx)) {
                            if (UsedAccessLevel < ctx.level) {
                                // error, invalid permissions to access item
                                return null;
                            }   return null;
                        }
                    }

                    return default;
                }
            }

            internal static string ApplyWiggle(string input, int start, int length) {
                const char wiggle  = '\u0330';
                var        builder = new StringBuilder(input.Length * 2);

                for (int i = 0; i < input.Length; i++) {
                    builder.Append(input[i]);
                    if (i >= start && i < start + length)
                        builder.Append(wiggle);
                }

                return builder.ToString();
            }


            /// <summary>
            /// NOT PART OF THE DEFINE SYSTEM => THIS IS THE REGEX PATTERN MATCHER TO CREATE LEXER TOKENS
            /// </summary>
            /// <param name="input"></param>
            /// <returns></returns>
            internal static List<string> RegexTokenize(string input) {
                // Wide multi-character operators and atomic tokens, now including comment tokens
                string[] atomicTokens = [
                    "//", "/*", "*/",
                    "$\"", "<=>", "==", "!=", "<=", ">=", "&&", "||", "++", "--",
                    "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<", ">>", "->", "??", "?.", "+:", "-:"
                ];

                // Escape them for regex and order by length
                string escapedTokens = string.Join("|", atomicTokens
                                                       .Select(Regex.Escape)
                                                       .OrderByDescending(s => s.Length));

                // Single-character separators including whitespace
                string separators = @"!""£$%\^&*()+\-=\[\]{};:'@#~\\|,<.>/?\s";

                // Final pattern:
                // 1. Match atomic tokens
                // 2. Match non-separator sequences
                // 3. Match single separators
                string pattern = $@"({escapedTokens})|([^{separators}]+)|([{separators}])";

                var matches = Regex.Matches(input, pattern);
                var tokens  = new List<string>();

                foreach (Match match in matches) {
                    if (!string.IsNullOrEmpty(match.Value))
                        tokens.Add(match.Value);
                }

                return tokens;
            }
        }
    }
}