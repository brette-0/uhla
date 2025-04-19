using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Tataru {
    namespace Constants {
        static internal class Constants {
            static readonly string OperatorRegex = @"(\d+|\w+|<=|>=|==|!=|\+\+|--|\+=|-=|\*=|/=|\$=|\??=|%=|&=|\|=|\^=|>>=|<<=|<<|>>|<=|>=|==|!=|\+|\-|\*|\/|&|\| |\^|\%|\$|!|#|\\|@|[(){}\[\]|<|>|\|]+)";
            static readonly HashSet<string> UnevaluableOperators = [
                "<", ">", "==", "!=", "<=", ">=", "?", ":", "??", "+", "-", "*", "/", "&", "|", "^",
                "%", "$", "!", ">>", "<<", ">|", "|<", "++", "--", "+=", "-=", "*=", "/=",
                "$=", "??=", "%=", "&=", "|=", "^=", ">>=", "<<=", "#", "\\", "@", "(", ")", "[", "]", "{", "}", ";",
                ".", ","
            ];

            static readonly HashSet<string> Operators = [
                "(", ")", "[", "]", "{", "}", "\""
            ];

            static readonly HashSet<string> ConstantKeywords = [
                "null"
            ];
        }
    }
}
