using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using Tomlyn;
using uhla.Core.Language;

namespace uhla.Core;

internal static class Terminal {
    internal enum Responses : byte {
        Terminate_Error,
        Terminate_Success,
        Proceed,
    }
                
                
    internal static (string InputPath, string OutputPath, Responses Response) Parse(string[] args) {
        var (InputPath, OutputPath) = (string.Empty, string.Empty);
        var StringIndex = 0;
        var Flattened   = string.Join(" ", args);

        var Response = Responses.Proceed;
        Program.WarningLevel = WarningLevels.NONE;

        var LoadedConfig = false;
        var CWDSet       = false;

        for (var i = 0; i < args.Length; i++) {
            StringIndex += args[i].Length;

            switch (args[i]) {
                case "-ll":
                case "--link":
                    if (i == args.Length - 1) {
                        // error, no linker script provided
                        return default;
                    }

                    if (Program.Linker is not null) {
                        // error, linker is already created
                        return default;
                    }

                    Program.Linker = new Linker(args[++i]);
                    if (Program.Linker.Script is null) {
                        // error creating
                        return default;
                    }
                    break;
                
                case "-a":
                case "--architecture":
                    if (i == args.Length - 1) {
                        // error, no target provided
                        return default;
                    }

                    if (Program.EArchitecture != Architectures.None) {
                        // error, architecture has been set already
                        return default;
                    }

                    Program.EArchitecture = args[++i] switch {
                        "nes" or "fds" or "famicom" or "2a03" => Architectures.RICOH_2A03,
                        "6502" => Architectures.NMOS_6502,
                        "6507" => Architectures.NMOS_6507,
                        
                        _ => Architectures.None
                    };

                    if (Program.EArchitecture == Architectures.None) {
                        // error, specified architecture is not supported.
                        return default;
                    }
                    
                    break;
                
                case "-i":
                case "--input":
                    if (i == args.Length - 1) {
                        if (!LoadedConfig) LoadConfig();
                        Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "No Input Path Provided")]}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                        return default;
                    }  
                    
                    if (InputPath.Length > 0) {
                        if (!LoadedConfig) LoadConfig();
                        Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "Input Source File Path has already been specified")]}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                        return default;
                    }
                    
                    InputPath = args[++i];
                    break;

                case "-o":
                case "--output":
                    if (i == args.Length - 1) {
                        if (!LoadedConfig) LoadConfig();
                        Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "No Output Path Provided")]}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                        return default;
                    }  
                    
                    if (OutputPath.Length > 0) {
                        if (!LoadedConfig) LoadConfig();
                        Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "Output Binary File Path has already been specified")]}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                        return default;
                    }
                    OutputPath = args[++i];
                    break;

                case "-d":
                case "--directory":
                    if (i == args.Length - 1) {
                        if (!LoadedConfig) LoadConfig();
                        // error: no cwd provided
                        return default;
                    }

                    if (CWDSet) {
                        if (!LoadedConfig) LoadConfig();
                        // error: cwd already set
                        return default;
                    }
                    
                    Environment.CurrentDirectory = args[++i];
                    CWDSet                       = true;
                    break;
                
                case "-c":
                case "--config":
                    if (i == args.Length - 1) {
                        // error: no path for config
                        return default;
                    }
                    
                    LoadedConfig = LoadConfig(args[++i]);
                    if (!LoadedConfig) {
                        // error passback
                        return default;
                    }
                    break;
                
                case "-w":
                case "--warning":
                    if (i == args.Length - 1) {
                        if (!LoadedConfig) LoadConfig();
                        // error, no warning description detected
                        return default;
                    } else if (Program.WarningLevel != WarningLevels.NONE) {
                        if (!LoadedConfig) LoadConfig();
                        // error, already described warning level
                        return default;
                    }
                    
                    Program.WarningLevel = args[++i] switch {
                        "i" or "ignore"     or "I" or "IGNORE"     => WarningLevels.IGNORE,
                        "d" or "default"    or "D" or "DEFAULT"    => WarningLevels.DEFAULT,
                        "e" or "error"      or "E" or "ERROR"      => WarningLevels.ERROR,
                        "v" or "verbose"    or "V" or "VERBOSE"    => WarningLevels.VERBOSE,
                        "s" or "strict"     or "S" or "STRICT"     => WarningLevels.STRICT,
                        "c" or "controlled" or "C" or "CONTROLLED" => WarningLevels.CONTROLLED,

                        _ => WarningLevels.NONE
                    };

                    if (Program.WarningLevel == WarningLevels.NONE) {
                        if (!LoadedConfig) LoadConfig();
                        // error : unrecognized warning level 
                        return default;
                    }
                    break;

                case "-h":
                case "--help":
                    if (!LoadedConfig) LoadConfig();
                    Response = Responses.Terminate_Success;

                    if (i == args.Length - 1) {
                        // generic help message
                        Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                            $"""
                             UHLA 2a03 - GPL V2 Brette Allen 2026

                             -a  | --architecture | [arg]     | TODO: WRITE 'SET Architecture' INFO HERE.
                             -ll | --link         | [path]    | TODO: WRITE 'SET LINK PATH' INFO HERE
                             -i  | --input        | [path]    | {Language.Language.Connectives[(Program.ActiveLanguage, "Entrypoint Source Assembly File")]}
                             -o  | --output       | [path]    | {Language.Language.Connectives[(Program.ActiveLanguage, "Output ROM/Disk Binary Output")]}
                             -h  | --help         |           | {Language.Language.Connectives[(Program.ActiveLanguage, "Display the help string (you did that)")]}
                             -h  | --help         | [arg]     | TODO: WRITE "GET INFO ON SPECIFIC ARGUMENT FUNCTION" HERE
                             -l  | --language     | [lang]    | {Language.Language.Connectives[(Program.ActiveLanguage, "Choose a language to use")]}
                             -w  | --warning      | [level]   | TODO: Write "SET WARNING LEVEL" HERE
                             -d  | --directory    | [path]    | TODO: Write "SET CWD" HERE
                             -c  | --config       | [path]    | TODO: Write "CONFIG FETCH" HERE
                                    
                             """,            -1, 0, null, null);
                    } else {
                        // TODO: Add support for all new arguments
                        switch (args[++i]) {
                            default: 
                                // error: cannot help with this : unrecognized context
                                return default;

                            case "l":
                            case "lang":
                            case "languages":
                                // language specific help message.
                                Log(ErrorTypes.None, DecodingPhases.TERMINAL, @"
English (UK)      ""-l en_gb""
English (US)      ""-l en_us""
Español           ""-l es""
Deutsch           ""-l de""
日本語            ""-l ja""
Français          ""-l fr""
Português         ""-l pt""
Русский           ""-l ru""
Italiano          ""-l it""
Nederlands        ""-l ne""
Polski            ""-l pl""
Türkçe            ""-l tr""
Tiếng Việt        ""-l vt""
Bahasa Indonesia  ""-l in""
Čeština           ""-l cz""
한국어            ""-l ko""
Українська        ""-l uk""
العربية           ""-l ar""
Svenska           ""-l sw""
فارسی             ""-l pe""
中文              ""-l ch""
", -1, default, null, null);
                                break;

                            case "w":
                            case "warn":
                            case "warnings":
                                // warnings specific help message
                                Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                    $"""
                                     UHLA Warning Types and how they work

                                     ignore      : Will not display any warnings, but track the quantity for after completion.
                                     default     : Will warn the user about potential issues with their code.
                                     error       : Will convert all errors into warnings, enforcing the user to fix all issues.
                                     verbose     : Will display much more warnings, recommended and intended for those who wish to write perfect code.
                                     strict      : Acts as 'verbose' but warnings become errors, not recommended.
                                     controlled  : Acts as 'strict' but prevents overruling.
                                            
                                     """,                                                             -1, default, null, null);
                                break;

                            case "i":
                            case "input":
                                Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                    $"""
                                     UHLA Input File

                                     The input file argument (-i or --input) should be followed by a valid file path to a source assembly file. 
                                     If the file is empty you will receive an error, you may only pass one file here as the entry point file.
                                     This decides what the root of the "include path" is, includes from here must be relative to this path.
                                            
                                     """,                                                             -1, default, null, null);
                                break;

                            case "o":
                            case "output":
                                Log(ErrorTypes.None, DecodingPhases.TERMINAL,
                                    $"""
                                     UHLA Output File

                                     The output file argument (-o or --output) should be followed by a path pointing to a file to generate.
                                     The file name must comply with the limits of your Operating System.
                                     The directory the output file lives in must also already exist. 
                                     
                                     UHLA will overwrite a file existing with the same name at the output path if found.
                                     """,                                                             -1, default, null, null);
                                break;
                        }
                    }


                    break;

                case "-l":
                case "--language":
                    if (i == args.Length - 1) {
                        if (!LoadedConfig) LoadConfig();
                        Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "No Language Provided")]}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                        return default;
                    }

                    Program.ActiveLanguage = args[++i] switch {
                        "en_gb" => Languages.English_UK,
                        "en_us" => Languages.English_US,
                        "es"    => Languages.Spanish,
                        "de"    => Languages.German,
                        "ja"    => Languages.Japanese,
                        "fr"    => Languages.French,
                        "pt"    => Languages.Portuguese,
                        "ru"    => Languages.Russian,
                        "it"    => Languages.Italian,
                        "ne"    => Languages.Dutch,
                        "pl"    => Languages.Polish,
                        "tr"    => Languages.Turkish,
                        "vt"    => Languages.Vietnamese,
                        "in"    => Languages.Indonesian,
                        "cz"    => Languages.Czech,
                        "ko"    => Languages.Korean,
                        "uk"    => Languages.Ukrainian,
                        "ar"    => Languages.Arabic,
                        "sw"    => Languages.Swedish,
                        "pe"    => Languages.Persian,
                        "ch"    => Languages.Chinese,

                        _       => Languages.Null
                    };

                    if (Program.ActiveLanguage == Languages.Null) {
                        if (!LoadedConfig) LoadConfig();
                        Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "Invalid Language Provided")]}.", -1, default, ApplyWiggle(Flattened, StringIndex, args[i].Length), null);
                        return default;
                    }
                    break;

                default:
                    if (!LoadedConfig) LoadConfig();
                    Error(ErrorTypes.ParsingError, DecodingPhases.TERMINAL, $"{Language.Language.Connectives[(Program.ActiveLanguage, "Unrecognized Terminal Argument")]}.", -1, default, ApplyWiggle(Flattened, 1 + StringIndex, args[i].Length), null);
                    return default;
            }
        }

        if (!LoadedConfig) LoadedConfig = LoadConfig();
        return LoadedConfig ? (InputPath, OutputPath, Response) : default;

        static bool LoadConfig(string? path = null) {
            #if DEBUG
            path ??= Environment.CurrentDirectory;
            #else
            path ??= AppContext.BaseDirectory;
            #endif
            if (!File.Exists($"{path}/UHLA.toml")) {
                File.WriteAllText($"{path}/UHLA.toml", """
                                                           [Defaults]
                                                           DefaultLanguage             = "System"
                                                           DefaultWarningLevel         = "Default"

                                                           [Paths]
                                                           LibraryIncludePaths         = ["./lib"]
                                                           """);
            }

            var Config = Toml.ToModel<uhlaConfigTomlTemplate>(
                                                                  File.ReadAllText(Path.Combine(AppContext.BaseDirectory, "UHLA.toml")),
                                                                  null,
                                                                  new TomlModelOptions { ConvertPropertyName = name => name }
                                                                 );

            #region Warning level from Config TOML
            if (Program.WarningLevel == WarningLevels.NONE) Program.WarningLevel = Config.Defaults.DefaultWarningLevel switch {
                "Ignore"     => WarningLevels.IGNORE,
                "Default"    => WarningLevels.DEFAULT,
                "Error"      => WarningLevels.ERROR,
                "Verbose"    => WarningLevels.VERBOSE,
                "Strict"     => WarningLevels.STRICT,
                "Controlled" => WarningLevels.CONTROLLED, 

                _               => WarningLevels.NONE // mark to fix toml
            };
           
            if (Program.WarningLevel == WarningLevels.NONE) {
                Warn(ErrorTypes.SyntaxError, DecodingPhases.TERMINAL, $"""
                          The config file (at {AppContext.BaseDirectory}/UHLA.toml) is malformed! 
                          Ensure that it contains the key 'DefaultWarningLevel' under 'Defaults' table. The data may be any of the following:

                          Ignore                  : By default will ignore all warnings, great for sloppy vibe coding with minimal output.
                          Default                 : Provides few errors and doesn't halt your workflow
                          Error                   : Treats warning as errors, not recommended but does enforce clean code.
                          Verbose                 : Shows more warnings, even those which are harmless.
                          Strict                  : Shows more warnings as errors, not recommended but does enforce clean code.
                          Controlled              : Functions like Strict but prevents use of overrides. 

                          Project UHLA will NOT continue until you fix this or manually specify your Warning Level!
                          """, default, default, default, null);
                return false;
            }
            #endregion Warning level from Config TOML

            #region Default Langauge from Config TOML
            if (Program.ActiveLanguage == Languages.Null) Program.ActiveLanguage = Config.Defaults.DefaultLanguage switch {
                "English UK" => Languages.English_UK,
                "English US" => Languages.English_US,
                "Spanish"    => Languages.Spanish,
                "German"     => Languages.German,
                "Japanese"   => Languages.Japanese,
                "French"     => Languages.French,
                "Portuguese" => Languages.Portuguese,
                "Russian"    => Languages.Russian,
                "Italian"    => Languages.Italian,
                "Dutch"      => Languages.Dutch,
                "Polish"     => Languages.Polish,
                "Turkish"    => Languages.Turkish,
                "Vietnamese" => Languages.Vietnamese,
                "Indonesian" => Languages.Indonesian,
                "Czech"      => Languages.Czech,
                "Korean"     => Languages.Korean,
                "Ukrainian"  => Languages.Ukrainian,
                "Arabic"     => Languages.Arabic,
                "Swedish"    => Languages.Swedish,
                "Persian"    => Languages.Persian,
                "Chinese"    => Languages.Chinese,

                "System" => Language.Language.CaptureSystemLanguage(),
                _        => Languages.Null
            };

            if (Program.ActiveLanguage == Languages.Null) {
                Warn(ErrorTypes.SyntaxError, DecodingPhases.TERMINAL, $"""
                          The config file (at {AppContext.BaseDirectory}/UHLA.toml) is malformed! 
                          Ensure that it contains the key 'DefaultLanguage' under 'Defaults' table. The data may be any of the following:

                          English UK
                          English US
                          Spanish
                          German
                          Japanese
                          French
                          Portuguese
                          Russian
                          Italian
                          Dutch
                          Polish
                          Turkish
                          Vietnamese
                          Indonesian
                          Czech
                          Korean
                          Ukrainian
                          Arabic
                          Swedish
                          Persian
                          Chinese

                          Project UHLA will NOT continue until you fix this or manually specify your language!
                          """, default, default, default, null);
                return false;
            }
            #endregion Default Langauge from Config TOML

            Program.SourceFileSearchPaths = [.. Config.Paths.LibraryIncludePaths];

            if (Program.SourceFileSearchPaths.Count == 0) {
                // warn, no libraries at all (this is unusual, they should at least have the standard library)
                return false;
            }

            return true;
        }
    }

    internal class uhlaConfigTomlTemplate {
        public class DefaultsBlock {
            public string DefaultWarningLevel { get; set; } = "DefaultWarningLevel";
            public string DefaultLanguage     { get; set; } = "DefaultLanguage";
        }

        public class PathsBlock {
            public string[] LibraryIncludePaths { get; set; } = [];
        }

        public PathsBlock    Paths    { get; set; } = new();
        public DefaultsBlock Defaults { get; set; } = new();
    }

    internal record struct ErrorContext {
        internal ErrorLevels    ErrorLevel;
        internal ErrorTypes     ErrorType;
        internal DecodingPhases DecodingPhase;
        internal string         Message;
        internal int            LineNumber, StepNumber;
        internal Func<string?>  Context;
        internal string?        ContextFileName;
    }

    // in event of left in message, don't show on release
    #if DEBUG
    internal static void Debug(string message) => Console.WriteLine(message);
    #else
    internal static void debug() {}
    #endif

    #if DEBUG
    internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? ContextFileName, string? Context,
                                   int     lineNumber = 0, 
                                   string  filePath = "", 
                                   string  memberName = "") {
        #else
    internal static void WriteInfo(ErrorLevels ErrorLevel, ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
        #endif
    
        Console.ForegroundColor = ErrorLevel switch {
            ErrorLevels.LOG   => ConsoleColor.Cyan, 
            ErrorLevels.WARN  => ConsoleColor.Yellow, 
            ErrorLevels.ERROR => ConsoleColor.Red, 
            
            _                   => ConsoleColor.White
        };

        string ErrorTypeString, ErrorTypeConnective, LocationString, DecodePhaseString;

        if (ErrorType  == ErrorTypes.None) {
            Console.WriteLine(Message);
            goto Exit;
        }

        ErrorTypeString     = Language.Language.ErrorTypeMessages[(Program.ActiveLanguage, ErrorType)];
        ErrorTypeConnective = Language.Language.Connectives[(Program.ActiveLanguage, "During")];
        DecodePhaseString   = Language.Language.DecodePhaseMessages[(Program.ActiveLanguage, Phase)];
        LocationString      = LineNumber == -1 ? "" : StepNumber == 0 ? $"({LineNumber})" : $"({LineNumber}, {StepNumber})";
        Context = Context == null ? "" : $": {Context}";
        ContextFileName ??= "";

        // Something Error During Something Phase :: Could not do a thing (1, 2) : ah, the issue is here.
        #if DEBUG
        Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {ContextFileName} {LocationString}{Context}");
        Console.WriteLine($"[{filePath}:{lineNumber}] {memberName}");
        #else
        Console.WriteLine($"{ErrorTypeString} {ErrorTypeConnective} {DecodePhaseString} :: {Message} {ContextFileName} {LocationString}{Context}");
        #endif

        Exit:
        Console.ResetColor();
    }

    #if DEBUG
    internal static void Log(ErrorContext              ctx,
                             [CallerLineNumber] int    lineNumber = 0,
                             [CallerFilePath]   string filePath   = "",
                             [CallerMemberName] string memberName = "") {
        if (ctx.ErrorLevel != ErrorLevels.LOG)
            throw new InvalidOperationException($"Log() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

        WriteInfo(ErrorLevels.LOG, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context(),
                  ctx.ContextFileName, lineNumber, filePath, memberName);
    }

    internal static void Warn(ErrorContext              ctx,
                              [CallerLineNumber] int    lineNumber = 0,
                              [CallerFilePath]   string filePath   = "",
                              [CallerMemberName] string memberName = "") {
        var expectedLevel = Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? ErrorLevels.ERROR : ErrorLevels.WARN;
        if (ctx.ErrorLevel != expectedLevel)
            throw new InvalidOperationException($"Warn() called with mismatched ErrorLevel: {ctx.ErrorLevel}, expected: {expectedLevel}");

        WriteInfo(expectedLevel, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context(),
                  ctx.ContextFileName, lineNumber, filePath, memberName);
    }

    internal static void Error(ErrorContext              ctx,
                               [CallerLineNumber] int    lineNumber = 0,
                               [CallerFilePath]   string filePath   = "",
                               [CallerMemberName] string memberName = "") {
        if (ctx.ErrorLevel != ErrorLevels.ERROR)
            throw new InvalidOperationException($"Error() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

        WriteInfo(ErrorLevels.ERROR, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context(),
                  ctx.ContextFileName, lineNumber, filePath, memberName);
    }


    internal static void   Log(ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? Context, string? ContextFileName,
                               [CallerLineNumber] int lineNumber = 0,
                               [CallerFilePath] string filePath = "",
                               [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context, ContextFileName, lineNumber, filePath, memberName);
    

    internal static void  Warn(ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? Context, string? ContextFileName,
                               [CallerLineNumber] int lineNumber = 0,
                               [CallerFilePath] string filePath = "",
                               [CallerMemberName] string memberName = "") => WriteInfo(Program.WarningLevel.HasFlag(WarningLevels.ERROR) ? ErrorLevels.ERROR : ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context, ContextFileName, lineNumber, filePath, memberName);


    internal static void Error(ErrorTypes ErrorType, DecodingPhases Phase, string Message, int LineNumber, int StepNumber, string? Context, string? ContextFileName,
                               [CallerLineNumber] int lineNumber = 0,
                               [CallerFilePath] string filePath = "",
                               [CallerMemberName] string memberName = "") => WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context, ContextFileName, lineNumber, filePath, memberName);

    #else
    internal static void   Log(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
        WriteInfo(ErrorLevels.LOG,   ErrorType, Phase, Message, LineNumber, StepNumber, Context);
    }

    internal static void  Warn(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
        WriteInfo(ErrorLevels.WARN,  ErrorType, Phase, Message, LineNumber, StepNumber, Context);
    }

    internal static void Error(ErrorTypes ErrorType, DecodingPhase Phase, string Message, int? LineNumber, int? StepNumber, string? Context) {
        WriteInfo(ErrorLevels.ERROR, ErrorType, Phase, Message, LineNumber, StepNumber, Context);
    }

    internal static void Log(ErrorContext ctx)
    {
        if (ctx.ErrorLevel != ErrorLevels.LOG)
            throw new InvalidOperationException($"Log() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

        WriteInfo(ErrorLevels.LOG, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context());
    }

    internal static void Warn(ErrorContext ctx)
    {
        var expectedLevel = ErrorLevels.WARN; // No error override in release
        if (ctx.ErrorLevel != expectedLevel)
            throw new InvalidOperationException($"Warn() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

        WriteInfo(expectedLevel, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context());
    }

    internal static void Error(ErrorContext ctx)
    {
        if (ctx.ErrorLevel != ErrorLevels.ERROR)
            throw new InvalidOperationException($"Error() called with mismatched ErrorLevel: {ctx.ErrorLevel}");

        WriteInfo(ErrorLevels.ERROR, ctx.ErrorType, ctx.DecodingPhase, ctx.Message, ctx.LineNumber, ctx.StepNumber, ctx.Context());
    }
    #endif
    
    internal static string ApplyWiggle(string input, int start, int length) {
        const char wiggle  = '\u0330';
        var        builder = new StringBuilder(input.Length * 2);

        for (int i = 0; i < input.Length; i++) {
            builder.Append(input[i]);
            if (i >= start && i < start + length)
                builder.Append(wiggle);
        }

        return builder.ToString();
    }
    
    /// <summary>
    /// GENERATED CODE : Attempts to normalize and validate a path as safe across Windows, macOS, and Linux.
    /// Returns true if the path is valid and portable; false otherwise.
    /// </summary>
    internal static bool TryNormalizeSafePath(string input, out string normalized)
    {
        normalized = string.Empty;
        if (string.IsNullOrWhiteSpace(input)) return false;

        try
        {
            string path = Path.GetFullPath(input.Trim());

            if (path.Length >= 240) return false;

            // Reject invalid path characters
            if (path.IndexOfAny(Path.GetInvalidPathChars()) != -1) return false;

            // Split into segments to check each component
            string[] parts = path.Split(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar);
            if (string.IsNullOrWhiteSpace(parts[0])) return false;
                
            foreach (var part in parts)
            {
                if (string.IsNullOrWhiteSpace(part)) continue;

                // Trim + reject bad chars
                string name = part.Trim();
                if (name.Length                                     == 0) return false;
                if (name.IndexOfAny(Path.GetInvalidFileNameChars()) != -1) return false;

                // Disallow reserved Windows device names
                string upper = name.ToUpperInvariant();
                if (Regex.IsMatch(upper, @"^(CON|PRN|AUX|NUL|COM[1-9]|LPT[1-9])$"))
                    return false;

                // No trailing dot or space
                if (name.EndsWith(" ") || name.EndsWith(".")) return false;
            }

            normalized = path;
            return true;
        }
        catch
        {
            return false;
        }
    }
}