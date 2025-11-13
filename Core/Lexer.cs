/*
    TODO: RegexTokenizedBuffer to store (ctx, index, length)
          On error report, refer to RegexParsed[0].ctx, index, length etc...
*/

namespace uhla.Core {
    internal static partial class Core {
        internal static (List<string>? line, Terminal.ErrorContext? err) Lex(ref List<string>.Enumerator src) {
            List<char>   containerBuff = [];
            List<string> line = [];
            while (src.MoveNext()) {
                while (containerBuff.Count > 0 && containerBuff[^1] is '\"') {
                    if (src.Current[0] is '\"') {
                        containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                        if (!src.MoveNext()) {
                            if (containerBuff.Count is 0) {
                                // safe exit
                                return (line, null);
                            } else {
                                // container left open (no more context)
                                return (null, new Terminal.ErrorContext()); // TODO: parameterise 
                            }
                        }
                        break;
                    } else if (!src.MoveNext()) {
                        // container left open (no more context) :: unterminated string
                        return (null, new Terminal.ErrorContext()); // TODO: parameterise 
                    }
                }
                switch (src.Current[0]) {
                    case '(' or '[': containerBuff.Add(src.Current[0]); break;
                    case '{':
                        if (containerBuff.Count is 0) {                                         // ending with all closed
                            return (line, null);
                        } else if (containerBuff.Count > 1 && containerBuff[^1] is '\"') {      // string interpolation
                            containerBuff[^1] = src.Current[0];
                            continue;
                        } else {                                                                // ending with unclosed
                            // container left open (terminated)
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }
                    case ')':
                        if (containerBuff[^1] is '(') {
                            containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                            continue;
                        } else {
                            // error, unmatching containers
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }
                    
                    case ']':
                        if (containerBuff[^1] is ']') {
                            containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                            continue;
                        } else {
                            // error, unmatching containers
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }
                    
                    case '}':
                        if (containerBuff.Count is 0) {
                            // exit with presumed block closure
                            return (line, null);
                        } else if (containerBuff[^1] is '{') {
                            // close string interpolation
                            containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                            continue;
                        } else {
                            // error, unmatching containers
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }

                    case '\"':
                        containerBuff.Add('\"');
                        continue;
                }
            }
            return default;
        }
    }
}
