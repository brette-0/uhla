﻿using Architectures;
using UHLA;
using UHLA.Engine;
using UHLA.Language;

using static UHLA.Engine.Engine;

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

        if (!File.Exists(InputPath)) {
            // error, path does not exist
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"Input path {Path.GetFullPath(Path.Combine(Environment.CurrentDirectory, InputPath))} does not exist.",
                -1, default, null, null);
            return (int)ErrorTypes.ParsingError;
        }
        
        var InputFile = File.ReadAllText(InputPath);
        if (InputFile.Length == 0) {
            Terminal.Error(ErrorTypes.NothingToDo, DecodingPhases.TOKEN, $"{Language.Connectives[(ActiveLanguage, "Source file")]} {InputPath} {Language.Connectives[(ActiveLanguage, "has no contents")]}", -1, 0, null, null);
            return (int)ErrorTypes.NothingToDo;
        }
        SourceFileNameBuffer   .Add(InputPath!);
        SourceFileContentBuffer.Add(RegexTokenize(InputFile));
        SourceFileIndexBuffer  .Add(0);         // begin from char 0
        SourceFileLineBuffer   .Add(0);         // debug line, naturally 0
        SourceFileStepBuffer   .Add(0);         // debug step, naturally 0

        LabelDataBase["type"] =  new ObjectToken(new Dictionary<string, ObjectToken>() {
            {"",        new ObjectToken(ScopeTypes.Root, AssembleTimeTypes.EXP)},
        }, AssembleTimeTypes.EXP);

        // rs "Root Scope" has itself as key, value and parent - sitting in the root pointing to itself.
        // this is the only way via asm to directly refer to rs. Useful for when you use a 'as' level keyword but desires rs resolve.
        LabelDataBase["rs"] =  new ObjectToken(new Dictionary<string, ObjectToken>() {
            {"",        new ObjectToken(LabelDataBase, AssembleTimeTypes.SCOPE)},
        }, AssembleTimeTypes.SCOPE);

        // make language a compiler variable
        LabelDataBase["lang"]   = new ObjectToken(new Dictionary<string, ObjectToken>() {
            {"",        new ObjectToken($"\"{ActiveLanguage}\"", AssembleTimeTypes.INT)},
        }, AssembleTimeTypes.STRING);
        
        LabelDataBase["ToString"] = new ObjectToken(
            new Dictionary<string, (object data, AssembleTimeTypes type)>() {
                {"args", (1, AssembleTimeTypes.INT)},
                {"", (GenerateFunctionalDefine("# args", ["args"]), default)}
            }, AssembleTimeTypes.FEXP);
        
        // Functions are just lambdas, 0 refers to arg 0, and so on. They are of type Function returns type of type 'type'
        // The 'self' containing the lambda's type is the return type
        LabelDataBase["typeof"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
            {"",        new ObjectToken((ObjectToken ctx) => new ObjectToken(ctx.type, AssembleTimeTypes.TYPE), AssembleTimeTypes.TYPE)},
            {"ctx",     new ObjectToken(0, AssembleTimeTypes.OBJECT) },
            
            // arg num 0 => ctx
            {"0",       new ObjectToken("ctx", default, default)},
            
            {"args",    new ObjectToken(1, AssembleTimeTypes.INT)}
        }, AssembleTimeTypes.FUNCTION);

        LabelDataBase["exists"] = new ObjectToken(new Dictionary<string, ObjectToken>() {
            {"",        new ObjectToken((string ctx) => Database.GetObjectFromAlias(ctx) is null, AssembleTimeTypes.INT)},
            {"ctx",     new ObjectToken(0, AssembleTimeTypes.OBJECT) },
            
            // arg num 0 => ctx
            {"0",       new ObjectToken("ctx", default, default)},
            
            {"args",    new ObjectToken(1, AssembleTimeTypes.INT)}
        }, AssembleTimeTypes.FUNCTION);

        ActiveScopeBuffer.Add(LabelDataBase);   // add rs to 'as', default rs
        ObjectSearchBuffer = [LabelDataBase];   // by default, contains nothing more than this. For each search AS[^1] is added

        Architecture = EArchitecture switch {
            UHLA.Architectures.NMOS_6502  => new NMOS_6502(),
            UHLA.Architectures.NMOS_6507  => throw new NotImplementedException(),
            UHLA.Architectures.RICOH_2A03 => new Ricoh_2a03(),
            
            
            UHLA.Architectures.None => throw new NotImplementedException(),
            _                       => throw new NotImplementedException()
        };

        Architecture.Initalize();
        Assemble([]);
        
        return 0;
    }

    internal static List<List<string>>  SourceFileContentBuffer = [];
    internal static List<int>           SourceFileIndexBuffer = [];
    internal static List<string>        SourceFileNameBuffer  = [];
    internal static List<int>           SourceFileLineBuffer  = [];  // Used for ERROR REPORT ONLY
    internal static List<int>           SourceFileStepBuffer  = [];  // Used for ERROR REPORT ONLY

    internal static Dictionary<string, ObjectToken>         LabelDataBase         = [];
    internal static List<Dictionary<string, ObjectToken>>   ActiveScopeBuffer     = [];
    internal static List<Dictionary<string, ObjectToken>>   ObjectSearchBuffer    = [];

    internal static List<string> SourceFileSearchPaths = [];

    internal static Languages     ActiveLanguage;
    internal static WarningLevels WarningLevel;

    internal static UHLA.Architectures EArchitecture;
    internal static UHLA.InterfaceProtocol.IArchitecture      Architecture;
}