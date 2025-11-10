using uhla.Architectures;
using uhla.Core;
using uhla.Core.Language;

namespace uhla;

internal static class Program {
    internal static int Main(string[] args) {
        Console.OutputEncoding = System.Text.Encoding.UTF8;

        var (InputPath, OutputPath, Response) = Terminal.Parse(args);
        
        switch (Response) {
            case Terminal.Responses.Terminate_Error:
                return (int)ErrorTypes.ParsingError;   // Exit if parsing returns error

            case Terminal.Responses.Terminate_Success:
                return 0;

            case Terminal.Responses.Proceed:
                break;

            default:
                throw new ArgumentOutOfRangeException();
        }

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

        Core.Core.Initialize(InputPath, out var error);
        if (error is not 0) return error;
        
        Architecture.Initalize();
        Core.Core.Assemble([]);
        
        return 0;
    }

    internal static readonly List<List<string>> SourceFileContentBuffer = [];
    internal static readonly List<int>          SourceFileIndexBuffer   = [];
    internal static readonly List<string>       SourceFileNameBuffer    = [];
    internal static readonly List<int>          SourceFileLineBuffer    = []; // Used for ERROR REPORT ONLY
    internal static readonly List<int>          SourceFileStepBuffer    = []; // Used for ERROR REPORT ONLY

    internal static readonly Dictionary<string, ObjectToken?>       LabelDataBase      = [];
    internal static readonly List<Dictionary<string, ObjectToken?>> ActiveScopeBuffer  = [];
    internal static readonly List<Dictionary<string, ObjectToken?>> ObjectSearchBuffer = [];

    internal static readonly List<string>                           SourceFileSearchPaths = [];

    internal static Languages     ActiveLanguage;
    internal static WarningLevels WarningLevel;

    internal static Core.Architectures     EArchitecture;
    internal static IArchitecture          Architecture;
    internal static Linker?                Linker;
}