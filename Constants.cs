using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Tataru {
    namespace Constants {
        static internal class Constants {
            static internal readonly string OperatorRegex = @"(\d+|\w+|<=|>=|==|!=|\+\+|--|\+=|-=|\*=|/=|\$=|\??=|%=|&=|\|=|\^=|>>=|<<=|<<|>>|<=|>=|==|!=|\+|\-|\*|\/|&|\| |\^|\%|\$|!|#|\\|@|[(){}\[\]|<|>|\|]+)";
            static internal readonly HashSet<string> UnevaluableOperators = [
                "<", ">", "==", "!=", "<=", ">=", "?", ":", "??", "+", "-", "*", "/", "&", "|", "^",
                "%", "$", "!", ">>", "<<", ">|", "|<", "++", "--", "+=", "-=", "*=", "/=",
                "$=", "??=", "%=", "&=", "|=", "^=", ">>=", "<<=", "#", "\\", "@", "(", ")", "[", "]", "{", "}", ";",
                ".", ","
            ];

            static internal readonly HashSet<string> Operators = [
                "(", ")", "[", "]", "{", "}", "\""
            ];

            static internal readonly HashSet<string> ConstantKeywords = [
                "null"
            ];

            static internal readonly string[] Keywords = {
                "proc",                                                                 // define a procedure with its own encapuslated lexical scope
                
                "nmi", "irq", "reset",                                                  // interrupt procedures may be used like proc or set to an int
                
                "int", "string", "bool", "exp",                                         // macro return types or compile time variables
                "void",                                                                 // macro return type
                "return",                                                               // used to exit a macro with a value or optionally with null

                "slow", "fast", "zp",                                                   // Speed Rules

                  "u8",   "u16",   "u24",   "u32",   "u64",                             //     Little Endian Unsigned Types
                  "i8",   "i16",   "i24",   "i32",   "i64",                             //     Little Endian   Signed Types
                 "bu8",  "bu16",  "bu24",  "bu32",  "bu64",                             //        Big Endian Unsigned Types
                 "bi8",  "bi16",  "bi24",  "bi32",  "bi64",                             //        Big Endian   Signed Types
                 "du8",  "du16",  "du24",  "du32",  "du64",                             // BCD Little Endian Unsigned Types
                 "di8",  "di16",  "di24",  "di32",  "di64",                             // BCD Little Endian   Signed Types
                "dbu8", "dbu16", "dbu24", "dbu32", "dbu64",                             // BCD    Big Endian Unsigned Types
                "dbi8", "dbi16", "dbi24", "dbi32", "dbi64",                             // BCD    Big Endian   Signed Types

                "if", "else",                                                           // Conditional Assembly
                "loop",                                                                 // Generating Compile Time Iteration
            };
        }
    }
}
