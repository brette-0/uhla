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

            static internal readonly string[] CodeBodyKeywords = {
                "int", "bool", "token", "string", "void", "irq", "nmi", "reset", "proc", "loop"
            };
        }
    }
}
