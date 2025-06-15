using Numinous.Engine;
using static Numinous.Engine.Engine;

using Numinous.Langauges;

internal static class Program {
    internal static int Main(string[] args) {
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        (string? InputPath, string? OutputPath, Terminal.AssemblyFlags Flags) = Terminal.Parse(args);
        if (Flags.HasFlag(Terminal.AssemblyFlags.Failed))   return (int)ErrorTypes.ParsingError;   // Exit if parsing returns error
        if (Flags.HasFlag(Terminal.AssemblyFlags.Complete)) return 0;

        if (ActiveLanguage == Languages.Null) ActiveLanguage = Language.CaptureSystemLanguage();
        if (ActiveLanguage == Languages.Null) {
            ActiveLanguage =  Languages.English_UK;
            // TODO: Consider SystemError as more suitable?
            Terminal.Log(ErrorTypes.ParsingError, DecodingPhase.TERMINAL, "Could not detect language, choosing English (UK).",
                    null, null, null);
        }

        if (InputPath == null) {
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL,  $"{Language.Connectives[(ActiveLanguage, "Input path must be provided")]}.",
                null, null, null);
            return (int)ErrorTypes.ParsingError;
        }

        if (OutputPath == null) {
            Terminal.Error(ErrorTypes.ParsingError, DecodingPhase.TERMINAL,  $"{Language.Connectives[(ActiveLanguage, "Output path must be provided")]}.",
                    null, null, null);
            return (int)ErrorTypes.ParsingError;
        }

        string[] InputFile = File.ReadAllLines(InputPath!);
        if (InputFile.Length == 0) {
            Terminal.Error(ErrorTypes.NothingToDo,  DecodingPhase.TOKEN,     $"{Language.Connectives[(ActiveLanguage, "Source file")]} {InputPath} {Language.Connectives[(ActiveLanguage, "has no contents")]}", null, null, null);
            return (int)ErrorTypes.NothingToDo;
        }
        SourceFileNameBuffer.Add(InputPath!);
        SourceFileContentBuffer.Add(InputFile);

        // rs "Root Scope" has itself as key, value and parent - sitting in the root pointing to itself.
        LabelDataBase["rs"] = new DatabaseItem {value = LabelDataBase, parent = LabelDataBase};
        ActiveScope = (Dictionary<string, DatabaseItem>)LabelDataBase["rs"].value;
        
        (List<string[]>? Result, ContextFetcherEnums Code) = FetchContext(SourceFileContentBuffer[^1], 0, SourceFileNameBuffer[^1]);
        return 0;
    }

    internal static List<string[]> SourceFileContentBuffer = [];
    internal static List<string>   SourceFileNameBuffer    = [];

    internal static Dictionary<string, DatabaseItem> LabelDataBase = [];
    internal static Dictionary<string, DatabaseItem> ActiveScope = [];      // Generated default should never be used.
    internal static Languages ActiveLanguage;
}