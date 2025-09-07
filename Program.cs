using System.Runtime.InteropServices;
using Numinous;
using Numinous.Engine;
using Numinous.Language;

using static Numinous.Engine.Engine;

internal static class Program {
    internal static int Main(string[] args) {
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        var (InputPath, OutputPath, Response) = Terminal.Parse(args);
        if (Response == Terminal.Responses.Terminate_Error)   return (int)ErrorTypes.ParsingError;   // Exit if parsing returns error
        if (Response == Terminal.Responses.Terminate_Success) return 0;

        //if (ActiveLanguage == Languages.Null) ActiveLanguage = Language.CaptureSystemLanguage();
        //if (ActiveLanguage == Languages.Null) {
        //    ActiveLanguage = Languages.English_UK;
        //    // TODO: Consider SystemError as more suitable?
        //    Terminal.Log(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, "Could not detect language, choosing English (UK).",
        //            -1, default, null);
        //}

        if (InputPath == string.Empty) {
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Connectives[(ActiveLanguage, "Input path must be provided")]}.",
                -1, default, null, null);
            return (int)ErrorTypes.ParsingError;
        }

        if (OutputPath == string.Empty) {
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Connectives[(ActiveLanguage, "Output path must be provided")]}.",
                    -1, default, null, null);
            return (int)ErrorTypes.ParsingError;
        }

        var InputFile = File.ReadAllText(InputPath);
        if (InputFile.Length == 0) {
            Terminal.Error(ErrorTypes.NothingToDo, DecodingPhases.TOKEN, $"{Language.Connectives[(ActiveLanguage, "Source file")]} {InputPath} {Language.Connectives[(ActiveLanguage, "has no contents")]}", -1, 0, null, null);
            return (int)ErrorTypes.NothingToDo;
        }
        SourceFileNameBuffer.Add(InputPath!);
        SourceFileContentBuffer.Add(RegexTokenize(InputFile));
        SourceFileIndexBuffer.Add(0);       // begin from "main.s" (CONTENTS) : (0)
        SourceTokenIndexBuffer.Add(0);      // begin from char 0
        SourceFileLineBuffer.Add(0);        // debug line, naturally 0
        SourceFileStepBuffer.Add(0);        // debug step, naturally 0


        // rs "Root Scope" has itself as key, value and parent - sitting in the root pointing to itself.
        // this is the only way via asm to directly refer to rs. Useful for when you use a 'as' level keyword but desires rs resolve.
        LabelDataBase["rs"] =  new(new Dictionary<string, ObjectToken>() {
            {"",        new (LabelDataBase, AssembleTimeTypes.CSCOPE, AccessLevels.PRIVATE)},
        }, AssembleTimeTypes.CSCOPE, AccessLevels.PUBLIC);

        // make language a compiler variable
        LabelDataBase["lang"]   = new(new Dictionary<string, ObjectToken>() {
            {"",        new($"\"{ActiveLanguage}\"", AssembleTimeTypes.CINT, AccessLevels.PRIVATE)},
        }, AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC);

        LabelDataBase["a"] = new(new Dictionary<string, ObjectToken>() {
            {"",         new(Numinous.Engine.System.Registers.A, AssembleTimeTypes.CREG, AccessLevels.PRIVATE)},
            {"indexing", new(0, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
        }, AssembleTimeTypes.CREG, AccessLevels.PUBLIC);

        LabelDataBase["x"] = new(new Dictionary<string, ObjectToken>() {
            {"",         new(Numinous.Engine.System.Registers.X, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)},
            {"indexing", new(0, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
        }, AssembleTimeTypes.CREG, AccessLevels.PUBLIC);

        LabelDataBase["y"] = new(new Dictionary<string, ObjectToken>() {
            {"",         new(Numinous.Engine.System.Registers.Y, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)},
            {"indexing", new(0, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
        }, AssembleTimeTypes.CREG, AccessLevels.PUBLIC);

        LabelDataBase["ToString"] = new(
            new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
                {"args", (1, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)},
                {"", (GenerateFunctionalDefine("# args", ["args"]), default, default)}
            }, AssembleTimeTypes.FEXP , AccessLevels.PUBLIC);
        
        // Functions are just lambdas, 0 refers to arg 0, and so on. They are of type Function returns type of type 'type'
        // The 'self' containing the lambda's type is the return type
        LabelDataBase["typeof"] = new(new Dictionary<string, ObjectToken>() {
            {"",        new((ObjectToken ctx) => new ObjectToken(ctx.type, AssembleTimeTypes.TYPE, AccessLevels.PUBLIC), AssembleTimeTypes.TYPE, AccessLevels.PRIVATE)},
            {"ctx",     new(0, AssembleTimeTypes.COBJECT, AccessLevels.PRIVATE) },
            
            // arg num 0 => ctx
            {"0",       new("ctx", default, default)},
            
            {"args",    new(1, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)}
        }, AssembleTimeTypes.FUNCTION, AccessLevels.PUBLIC);

        LabelDataBase["exists"] = new(new Dictionary<string, ObjectToken>() {
            {"",        new ObjectToken((string ctx) => Database.GetObjectFromAlias(ctx, AccessLevels.PUBLIC) is null, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)},
            {"ctx",     new ObjectToken(0, AssembleTimeTypes.COBJECT, AccessLevels.PRIVATE) },
            
            // arg num 0 => ctx
            {"0",       new ObjectToken("ctx", default, default)},
            
            {"args",    new ObjectToken(1, AssembleTimeTypes.CINT, AccessLevels.PRIVATE)}
        }, AssembleTimeTypes.FUNCTION, AccessLevels.PUBLIC);

        ActiveScopeBuffer.Add(LabelDataBase);   // add rs to 'as', default rs
        ObjectSearchBuffer = [LabelDataBase];   // by default, contains nothing more than this. For each search AS[^1] is added

        Assemble([]);
        
        return 0;
    }

    internal static List<List<string>>  SourceFileContentBuffer = [];
    internal static List<int>           SourceFileIndexBuffer = [];
    internal static List<int>           SourceTokenIndexBuffer = [];
    internal static List<string>        SourceFileNameBuffer  = [];
    internal static List<int>           SourceFileLineBuffer  = [];  // Used for ERROR REPORT ONLY
    internal static List<int>           SourceFileStepBuffer  = [];  // Used for ERROR REPORT ONLY

    internal static Dictionary<string, ObjectToken>         LabelDataBase         = [];
    internal static List<Dictionary<string, ObjectToken>>   ActiveScopeBuffer     = [];
    internal static List<ScopeTypes>                        ActiveScopeTypeBuffer =[];
    internal static List<Dictionary<string, ObjectToken>>   ObjectSearchBuffer    = [];

    internal static List<string> SourceFileSearchPaths = [];

    internal static List<bool>          PragmaIllegalBuffer     = [];
    internal static List<bool>          PragmaCPUAwareBuffer    = [];
    internal static List<bool>          PragmaGPRAwareBuffer    = [];
    internal static List<bool>          PragmaRAMAwareBuffer    = [];

    internal static Func<int, bool>     ReadPermitted;
    internal static Func<int, bool>     WritePermitted;
    internal static Func<int, bool>     CallPermitted;

    internal static Languages ActiveLanguage;
    internal static WarningLevels WarningLevel;
    
    internal static Numinous.Modes Mode; 
}