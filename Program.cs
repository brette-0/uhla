using System.Runtime.InteropServices;

using Numinous.Engine;
using Numinous.Language;

using static Numinous.Engine.Engine;

internal static class Program {
    internal static int Main(string[] args) {
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        (string? InputPath, string? OutputPath, Terminal.Responses Response) = Terminal.Parse(args);
        if (Response == Terminal.Responses.Terminate_Error) return (int)ErrorTypes.ParsingError;   // Exit if parsing returns error
        if (Response == Terminal.Responses.Terminate_Success) return 0;

        if (ActiveLanguage == Languages.Null) ActiveLanguage = Language.CaptureSystemLanguage();
        if (ActiveLanguage == Languages.Null) {
            ActiveLanguage = Languages.English_UK;
            // TODO: Consider SystemError as more suitable?
            Terminal.Log(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, "Could not detect language, choosing English (UK).",
                    -1, default, null);
        }

        if (InputPath == null) {
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(ActiveLanguage, "Input path must be provided")]}.",
                -1, default, null);
            return (int)ErrorTypes.ParsingError;
        }

        if (OutputPath == null) {
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, $"{Language.Connectives[(ActiveLanguage, "Output path must be provided")]}.",
                    -1, default, null);
            return (int)ErrorTypes.ParsingError;
        }

        string[] InputFile = File.ReadAllLines(InputPath!);
        if (InputFile.Length == 0) {
            Terminal.Error(ErrorTypes.NothingToDo, DecodingPhase.TOKEN, $"{Language.Connectives[(ActiveLanguage, "Source file")]} {InputPath} {Language.Connectives[(ActiveLanguage, "has no contents")]}", -1, 0, null);
            return (int)ErrorTypes.NothingToDo;
        }
        SourceFileNameBuffer.Add(InputPath!);
        SourceFileContentBuffer.Add(InputFile);
        SourceFileIndexBuffer.Add(0);           // begin from "main.s" (CONTENTS) : (0)

        // rs "Root Scope" has itself as key, value and parent - sitting in the root pointing to itself.
        // this is the only way via asm to directly refer to rs. Useful for when you use a 'as' level keyword but desires rs resolve.
        LabelDataBase["rs"] = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
            {"",        (LabelDataBase, default, AccessLevels.PRIVATE)},
            {"parent",  (LabelDataBase, AssembleTimeTypes.CSCOPE, AccessLevels.PRIVATE)}
        }, AssembleTimeTypes.CSCOPE, AccessLevels.PUBLIC);

        // make language a compiler variable
        LabelDataBase["lang"]   = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
            {"",        (ActiveLanguage.ToString(), AssembleTimeTypes.CSTRING, AccessLevels.PRIVATE)},
            {"parent",  (LabelDataBase, AssembleTimeTypes.CSCOPE, AccessLevels.PRIVATE)}
        }, AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC);

        LabelDataBase["a"] = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
            {"",        (Numinous.Engine.System.Registers.A, AssembleTimeTypes.CREG, AccessLevels.PRIVATE)},
            {"indexing", (0, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
        }, AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC);

        LabelDataBase["x"] = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
            {"",        (Numinous.Engine.System.Registers.X, default, AccessLevels.PRIVATE)},
            {"indexing", (0, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
        }, AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC);

        LabelDataBase["y"] = (new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
            {"",        (Numinous.Engine.System.Registers.Y, default, AccessLevels.PRIVATE)},
            {"indexing", (0, AssembleTimeTypes.CINT, AccessLevels.PUBLIC) }
        }, AssembleTimeTypes.CSTRING, AccessLevels.PUBLIC);

        Span<int> SourceFileIndexBufferSpan = CollectionsMarshal.AsSpan(SourceFileIndexBuffer);

        ActiveScopeBuffer.Add(LabelDataBase);   // add rs to as, default rs
        ObjectSearchBuffer = [LabelDataBase];   // by default, contains nothing more than this. For each search AS[^1] is added
        var Demo = ContextFetcher(SourceFileContentBuffer[^1], ref SourceFileIndexBufferSpan[^1]);

        return 0;
    }

    internal static List<string[]> SourceFileContentBuffer = [];
    internal static List<int> SourceFileIndexBuffer = [];
    internal static List<string> SourceFileNameBuffer = [];

    internal static Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)> LabelDataBase = [];
    internal static List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>> ActiveScopeBuffer = [];
    internal static List<Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>> ObjectSearchBuffer = [];
    
    internal static Languages ActiveLanguage;
    internal static WarningLevels WarningLevel;
}