using System.Runtime.CompilerServices;

public static class Tataru {
    static bool DebugFile = false;
    static bool ListingFile = false;

    static string SourceEntryPointFilePath = "";
    static string TargetEntryPointFilePath = "";

    public static int Main(String[] args) {
        EXIT_CODES resp = ResolveArguments(ref args);   // digest cli args
        if (resp != 0) return (int)resp;                // if fail, return with fail ctx

        return 0;
    }

    private static EXIT_CODES ResolveArguments(ref String[] args) {
        for (int i = 0; i < args.Length; i++) {
            switch (args[i]) {
                case "-h":
                case "--help":
                    Console.Write(
"""
Tataru - Cursed 2a03 Assembler | Brette (2025)

Commands:
    -h | --help             : Help Command, you used it to see this page.
    -i | --input "path"     : Specify Path to Entrypoint ASM Source Code
    -o | --output "path"    : Specify Path for generated output ROMFile.

""");
                    return EXIT_CODES.OK;

                case "-i":
                case "--input":
                    if (i == args.Length - 1) {
                        Console.WriteLine($"Command Line Instruction '{args[i]}' requires a path as parameter!");
                        return EXIT_CODES.NO_PARAMETER;
                    }
                    SourceEntryPointFilePath = args[++i];
                    break;

                case "-o":
                case "--output":
                    if (i == args.Length - 1) {
                        Console.WriteLine($"Command Line Instruction '{args[i]}' requires a path as parameter!");
                        return EXIT_CODES.NO_PARAMETER;
                    }
                    TargetEntryPointFilePath = args[++i];
                    break;

                case "-l":
                case "--listing":
                    ListingFile = true;
                    break;

                case "-d":
                case "--debug":
                    DebugFile = true;
                    break;

                default:
                    Console.WriteLine("Invalid Command Line Argument, please use '-h' or '--help' on available commands.");
                    return EXIT_CODES.INVALID_ARGUMENT;
            }
        }
        return 0;
    }

    public enum EXIT_CODES {
        OK,
        INVALID_ARGUMENT,
        NO_PARAMETER = 2
    }
}