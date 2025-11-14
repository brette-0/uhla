using System.Text.RegularExpressions;
using uhla.Architectures;
using EList;

namespace uhla.Core {
    internal static partial class Core {
        internal static void Initialize(string InputPath, out int success) {
            #if DEBUG
            if (Program.EArchitecture is Architectures.None) {
                throw new Exception("Ran Init Twice, or did not init architecture on first run");
            }
            #endif

            var InputFile = File.ReadAllText(InputPath);
            if (InputFile.Length == 0) {
                Terminal.Error(ErrorTypes.NothingToDo, DecodingPhases.TOKEN, $"{Language.Language.Connectives[(Program.ActiveLanguage, "Source file")]} {InputPath} {Language.Language.Connectives[(Program.ActiveLanguage, "has no contents")]}", -1, 0, null, null);
                success = (int)ErrorTypes.NothingToDo;
            }
            Program.SourceFileNameBuffer   .Add(InputPath!);
            Program.SourceFileContentBuffer.Add(RegexTokenize(InputFile));
            Program.SourceFileIndexBuffer  .Add(0); // begin from char 0
            Program.SourceFileLineBuffer   .Add(0); // debug line, naturally 0
            Program.SourceFileStepBuffer   .Add(0); // debug step, naturally 0

            Program.LabelDataBase["type"] =  new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"",        new ObjectToken(ScopeTypes.Root, AssembleTimeTypes.EXP, true, true)},
            }, AssembleTimeTypes.EXP, true, true);

            // rs "Root Scope" has itself as key, value and parent - sitting in the root pointing to itself.
            // this is the only way via asm to directly refer to rs. Useful for when you use a 'as' level keyword but desires rs resolve.
            Program.LabelDataBase["rs"] =  new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"#self", new ObjectToken(Program.LabelDataBase, AssembleTimeTypes.SCOPE, true, true)},
            }, AssembleTimeTypes.SCOPE, true, true);

            // make language a compiler variable
            Program.LabelDataBase["lang"]   = new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"", new ObjectToken($"\"{Program.ActiveLanguage}\"", AssembleTimeTypes.INT, true, true)},
            }, AssembleTimeTypes.STRING, true, true);
            
            Program.LabelDataBase["ToString"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
                    {"args", new ObjectToken(1, AssembleTimeTypes.INT, true, true)},
                    {"#self", new ObjectToken(GenerateFunctionalDefine("# args", ["args"]), AssembleTimeTypes.FUNCTION, true, true)}
            }, AssembleTimeTypes.FEXP, true, true);
            
            // Functions are just lambdas, 0 refers to arg 0, and so on. They are of type Function returns type of type 'type'
            // The 'self' containing the lambda's type is the return type
            Program.LabelDataBase["typeof"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"",        new ObjectToken((ObjectToken ctx) => new ObjectToken(ctx.type, AssembleTimeTypes.TYPE, true, true), AssembleTimeTypes.TYPE, true, true)},
                {"ctx",     new ObjectToken(0, AssembleTimeTypes.OBJECT, true, true) },
                
                // arg num 0 => ctx
                {"0",       new ObjectToken("ctx", default, true, true)},
                
                {"args",    new ObjectToken(1, AssembleTimeTypes.INT, true, true)}
            }, AssembleTimeTypes.FUNCTION, true, true);

            Program.LabelDataBase["exists"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
                {"",        new ObjectToken((string ctx) => Database.GetObjectFromAlias(ctx) is null, AssembleTimeTypes.INT, true, true)},
                {"ctx",     new ObjectToken(0, AssembleTimeTypes.OBJECT, true, true) },
                
                // arg num 0 => ctx
                {"0",       new ObjectToken("ctx", default, false, true)},
                
                {"args",    new ObjectToken(1, AssembleTimeTypes.INT, true, true)}
            }, AssembleTimeTypes.FUNCTION, true, true);

            Program.ActiveScopeBuffer.Add(Program.LabelDataBase); // add rs to 'as', default rs
            Program.ObjectSearchBuffer.Add(Program.LabelDataBase); // by default, contains nothing more than this. For each search AS[^1] is added

            Program.Architecture = Program.EArchitecture switch {
                Architectures.NMOS_6502  => new NMOS_6502(),
                Architectures.NMOS_6507  => throw new NotImplementedException(),
                Architectures.RICOH_2A03 => new Ricoh_2a03(),
                
                
                Architectures.None => throw new NotImplementedException(),
                _                       => throw new NotImplementedException()
            };
            success = 0;
        }
        
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
                    if (int.TryParse(hex, System.Globalization.NumberStyles.HexNumber, null, out var value))
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

        internal static (ObjectToken obj, Terminal.ErrorContext? err)? Assemble(ref EList<string> src, List<EvalToken> args) {
            var         blockNumber = 0;
            EList<int>  jumpPoints  = [];
            bool?       success     = null;
            EList<bool> needsJump   = [];
            
            loop: if (!ContinueUntilToken(ref src)) {
                // error in continuance
                return null;
            }
         
            // control flow keywords TODO: needs more work
            switch (src.Current) {
                case "if":
                    if (!ContinueUntilToken(ref src)) {
                        // error in continuance
                        return null;
                    }
                    
                    var (line, err) = Lex(ref src);
                    if (err is not null) {
                        // error pass back
                        return null;
                    }

                    (var result, err) = (new ObjectToken(0, default, false, false), null);

                    if (err is not null) {
                        // error pass back
                        return null;
                    }

                    if (result.data is int condition) {
                        success = condition is 1;
                    } else {
                        // error condition is not int
                        return null;
                    }
                    
                    if (src.Current[0] is '{') {
                        needsJump.Add(false);
                        if ((bool)success) {
                            goto loop;
                            // take path
                        } else {
                            if (!ContinueUntilCloseBrace(ref src)) {
                                // continuation error
                                return null;
                            }

                            if (src.Current is "else") goto case "else";
                            break;
                        }
                    } else {
                        // erroneous content following else keyword
                        return null;
                    }
                    
                case "else":
                    if (success is null) {
                        // no condition to fail, keyword is erroneous
                        return null;
                    } else if (!(bool)success) {
                        if (!ContinueUntilToken(ref src)) {
                            // error in continuance
                            return null;
                        }
                        if (src.Current is "if") goto case "if";
                        if (src.Current[0] is '{') {
                            needsJump.Add(false);
                            // open else block
                        } else {
                            // erroneous content following else keyword
                            return null;
                        }
                        
                        // this is else body
                    } else {
                        if (!ContinueUntilCloseBrace(ref src)) {
                            // continuation error
                            return null;
                        }

                        if (src.Current is "else") goto case "else";
                    }

                    break;
                
                case "loop":
                    jumpPoints.Add(src.Index);
                    if (!ContinueUntilToken(ref src)) {
                        // error in continuance
                        return null;
                    }

                    if (src.Current[0] is not '{') {
                        // error, loop contains no body
                    }
                    // peak to bracket
                    break;
                
                case "break":
                    if (jumpPoints.Count is 0) {
                        // error, nothing to break out of
                        return null;
                    }
                    
                    // peak until closed brace
                    if (!ContinueUntilCloseBrace(ref src)) {
                        // continuation error
                        return null;
                    }
                    
                    jumpPoints.RemoveAt(^1);
                    break;
            }

            // if lexing terminated with this, we need to close a block AFTER finishing line.
            if (src.Current[0] is '}') {
                var jumpNow = needsJump.Pop(^1);
                if (jumpNow) {
                    src.JumpTo(jumpPoints.Pop(^1));
                } else {
                    if (!ContinueUntilToken(ref src)) {
                        // continuation error
                        return null;
                    }

                    if (src.Current is "else") {
                        if (!ContinueUntilCloseBrace(ref src)) {
                            // continuation error
                            return null;
                        }
                    }
                }
                // if we are closing a loop, we MUST jump. if we are closing condition we don't need to
            }
            
            while (src.MoveNext()) {
                if (src.Current[0] >= '0' && src.Current[0] <= '9') {
                    // error, first cannot begin with number
                    // TODO: Implement different branching method : Numerical
                    return null;
                }

                if (src.Current[0] is '#') {
                    // we have a core directive
                    if (src.MoveNext()) {
                        switch (src.Current) {
                            case "assert":
                            case "define":
                            case "undefine":
                            case "include":
                                break;
                        }
                    } else {
                        // error, directive body is missing
                    }
                } else if (src.Current[0] is '.') {
                    // we have an architecture directive / rule use
                }   // TODO: else check rt member declaration (no rule)
            }
            
            
            return null;

            bool ContinueUntilCloseBrace(ref EList<string> src) {
                while (src.Current[0] is not '{') {
                    if (!ContinueUntilToken(ref src)) {
                        // continuation error
                        return false;
                    }
                }
                var braceCount = 1;
                while (braceCount is not 0) {
                    if (!ContinueUntilToken(ref src)) {
                        // continuation error, or code block was not closed
                        return false;
                    }
                            
                    if      (src.Current[0] is '{') braceCount++;
                    else if (src.Current[0] is '}') braceCount--;
                }
            }

            bool ContinueUntilToken(ref EList<string> src) {
                while (src.Current[0] is not (' ' or '\t' or '\n')) {
                    if (!src.MoveNext()) {
                        // no else body or sequential condition
                        return false;
                    }

                    if (src.Current is not "//") continue;
                    while (src.Current[0] is not '\n') {
                        if (!src.MoveNext()) {
                            // no else body or sequential condition
                            return false;

                            // increment line debugger count
                        }

                        if (src.Current is not "/*") continue;
                        while (src.Current is not "*/") {
                            if (!src.MoveNext()) {
                                // error comment left unterminated
                                return false;
                            }
                        }
                    }
                }

                return true;
            }
        }
        
/*        internal static (object Return, AssembleTimeTypes Type, bool Success) Assemble(List<EvalToken> args) {
            Span<int>           SourceFileIndexBufferSpan   = CollectionsMarshal.AsSpan(Program.SourceFileIndexBuffer);
            Span<int>           SourceFileLineBufferSpan    = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
            Span<int>           SourceFileStepBufferSpan    = CollectionsMarshal.AsSpan(Program.SourceFileLineBuffer);
            
            var                                                     Representation = "";
            (string ctx, int StringIndex, int StringLength)         ActiveToken;
            List<(string token, int StringIndex, int StringLength)> DefineResolveBuffer = [];

            var BasicRegexTokens = new Memory<string>(Program.SourceFileContentBuffer[^1].ToArray());

            var StringIndex = 0;
            var TokenIndex  = SourceFileIndexBufferSpan[^1];
            
            (List<(List<List<(int StringOffset, int StringLength, object data, bool IsOperator)>> DeltaTokens, int Hierachy, string Representation)> Tokens, int MaxHierachy, int Finish, bool Success, bool Continue) lexer_resp; 
            (List<(int StringOffset, int StringLength, object data, AssembleTimeTypes type, bool IsOperator)> result, bool Success, bool Unevaluable)                                              evaluate_resp;

            /*while (true) {
                Step();
                switch (ActiveToken.ctx[0]) {
                    case '#': {
                        // parse as directive
                        Step();
                        
                        switch (ActiveToken.ctx) {
                            // core directives
                        
                            case "include":
                            case "define": 
                            case "undefine": 
                            case "assert":
                                throw new NotImplementedException();
                        
                            default:
                                // error : is not a core provided directive
                                return default;
                        }

                    }

                    case '.':
                        switch (Program.Architecture.CheckDirective(ref args, ref DefineResolveBuffer, ref StringIndex, ref TokenIndex, ref ActiveToken, ref Representation)) {
                            case CheckDirectiveStatus.Error:   
                            case CheckDirectiveStatus.None:    return default;    // error, not architecture provided :: pass back
                            case CheckDirectiveStatus.Success: continue;

                            default:
                                throw new ArgumentOutOfRangeException();
                        }
                }

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
                    case "table" :
                        // typeless table declare, just raw const rodata here.
                    case "const":
                        // const int, const string etc..
                    case "void":
                        // void foo(args)
                    case "macro":
                        // macro foo {code; return}
                    case "int":
                        // int foo = 5 OR int foo(args)
                    case "string":    
                        // string foo = "bar" OR string foo(args)
                        break;
                    
                    case "register": 
                    case "flag":
                    
                    case "del":
                        seek_no_whitespace();
                        var resp = Database.GetObjectFromAlias(ActiveToken.ctx,  Program.ActiveScopeBuffer[^1]);
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

                        if (!resp.constant) {
                            // error : cannot delete constant objects :: TODO: drop them a link to the wiki explaining the backwards definition mutation problem
                            return default;
                        }

                        Program.ActiveScopeBuffer[^1].Remove(ActiveToken.ctx);
                        continue;
                }
                
                // check if memory mode implicit variable reserve
                // check if function call
                // evaluate if performing macro
            }
            
            // TODO: Function to capture requested task

            EvalToken? ProcessRValue() {
                return null;
            }#1#
            
            /*void Step(bool regexParse = true) {
                ActiveToken = default;
                if (DefineResolveBuffer.Count == 0) {                                                   // si to be mutated ONLY by typed tokens
                    Representation += BasicRegexTokens.Span[TokenIndex];
                    StringIndex    += BasicRegexTokens.Span[TokenIndex].Length;
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
            }#1#
            
            /*(List<(string token, int StringIndex, int StringLength)> ctx, bool success) PartialResolveDefine(string Token) {
            

                List<(string token, int StringIndex, int StringLength)>    Resolved = [(Token, ActiveToken.StringIndex, Token.Length)];
                ObjectToken? ctx;
                bool                                                       success;
                bool                                                       HadSuccess = false;

                do {
                    ctx = Database.GetObjectFromAlias(Resolved[0].token);

                    if (ctx is not null && ctx.type == AssembleTimeTypes.EXP) {
                        HadSuccess = true;
                        Resolved.RemoveAt(0);
                    
                        Resolved.InsertRange(0, RegexTokenize((string)((Dictionary<string, (object data, AssembleTimeTypes type)>)ctx.data)[""].data)
                                               .Select(token => (token, ActiveToken.StringIndex, token.Length))
                                               .ToList());
                    }

                } while (ctx is not null && ctx.type == AssembleTimeTypes.EXP);

                return (Resolved, HadSuccess);
            }#1#
            
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
                            }#2#

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
                     #2#

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
            }#1#
        
            bool CheckLineTerminated() => (TokenIndex == BasicRegexTokens.Length && DefineResolveBuffer.Count == 0) || ActiveToken.ctx[0] == ';' || ActiveToken.ctx[0] == '\n' || ActiveToken.ctx == "//" || ActiveToken.ctx == "/*";

            // keep seeking beyond whitespace
            // void seek_no_whitespace(bool skip = false, bool regexParse = true) => Steps(() => !CheckLineTerminated() && (ActiveToken.ctx[0] == ' ' || ActiveToken.ctx[0] == '\t'), skip, regexParse);

        }*/


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
            Program.SourceFileIndexBuffer.Add(0);
            Program.SourceFileIndexBuffer.Add(0);
        }
        
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