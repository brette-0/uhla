using EList;

namespace uhla.Core {
    internal static partial class Core {
        internal static (EList<EList<Token>>? line, Terminal.ErrorContext? err) Lex(ref EList<string> src) {
            List<char>   containerBuff = [];
            EList<EList<Token>>  line = [[]];
            var lastToken = string.Empty;
            
            while (src.MoveNext()) {
                line[^1].Add(new Token(src.Current, src.Index));
                if (src.Current[0] is '\n') {
                    // check if we have an open container or if our last token was not an operator
                    if (containerBuff.Count is 0 && GetOperator(lastToken) is not null) {
                        return (line, null);
                    }   // else continue as normal, we have context to finish.
                }
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
                
                lastToken = src.Current;
                switch (src.Current) {
                    case ";":
                        if (containerBuff.Count is 0) {                                         // ending with all closed
                            line[^1].RemoveAt(^1);
                            return (line, null);
                        } else {                                                                // ending with unclosed
                            // container left open (terminated)
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }

                    case "(" or "[": containerBuff.Add(src.Current[0]); break;
                    case "{":
                        if (containerBuff.Count is 0) {                                         // ending with all closed
                            line[^1].RemoveAt(^1);
                            return (line, null);
                        } else if (containerBuff.Count > 1 && containerBuff[^1] is '$') {      // string interpolation
                            containerBuff[^1] = src.Current[0];
                            continue;
                        } else {                                                                // ending with unclosed
                            // container left open (terminated)
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }
                    case ")":
                        if (containerBuff[^1] is '(') {
                            containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                            continue;
                        } else {
                            // error, unmatching containers
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }
                    
                    case "]":
                        if (containerBuff[^1] is ']') {
                            containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                            continue;
                        } else {
                            // error, unmatching containers
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }
                    
                    case "}":
                        if (containerBuff.Count is 0) {
                            // exit with presumed block closure
                            line[^1].RemoveAt(^1);
                            return (line, null);
                        } else if (containerBuff[^1] is '{') {
                            // close string interpolation
                            containerBuff = [.. containerBuff.Take(containerBuff.Count - 1)];
                            continue;
                        } else {
                            // error, unmatching containers
                            return (null, new Terminal.ErrorContext()); // TODO: parameterise
                        }

                    case "\"":
                        containerBuff.Add('\"');
                        continue;
                    case "$\"":
                        containerBuff.Add('$');
                        continue;
                    
                    case ",":
                        line.Add([]);
                        continue;
                }
            }

            if (containerBuff.Count is not 0) {
                // error, unterminated container
                return (null, new Terminal.ErrorContext());
            }
            
            if (GetOperator(lastToken) is not null) {
                // error, no context ended on token
                return (null, new Terminal.ErrorContext());
            }
            
            return (line, null);
        }
    }
}