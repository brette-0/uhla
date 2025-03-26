using System.Runtime.CompilerServices;

public static class Tataru {
    static bool DebugFile = false;
    static bool ListingFile = false;

    static Targets Target = Targets.NES;

    static string SourceEntryPointFilePath = "";
    static string TargetEntryPointFilePath = "";

    // scope[name] = value for all labels
    static Dictionary<string, Dictionary<string, int?>> LabelDataBas = new();

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

    public enum Targets {
        NES = 0,
        FDS = 1
    }

    public class Header {
        public byte[] raw;

        Header(int FDS_DiskAmt) {
            Target = Targets.FDS;
            raw = [0x46, 0x44, 0x53, 0x1a, (byte)(FDS_DiskAmt / ((FDS_DiskAmt > 65500) ? 65500 : 1))];
        }

        Header(int PRGROM, int CHRROM, bool NametableLayout, bool Battery, bool Trainer, bool AlternativeNametables, int Mapper, int ConsoleType, int SubMapper, int PRGRAM, int EEPROM, int CHRRAM, int CPU, int VsType, int VsPPU, int ExtenedConsoleType, int MiscROMs, int DefaultExpansionDevice) {
            Target = Targets.NES;
            if (PRGROM > 0x4000) {
                // since iNES2 spec allows for 2 bytes of PRGROM in multiples of 16KiB should it be that 0x4000 is specified its unlikely that it refers to ~268MiB so we do it this way
                PRGROM /= 0x4000;
            }

            if (CHRROM > 0x2000) {
                // likewise ~67MiB of Graphics ROM is always quite unbelievable
                CHRROM /= 0x2000;
            }

            raw = [
                0x4e, 0x45, 0x53, 0x1a, // NES<EOF>
                (byte)(PRGROM & 0xff),
                (byte)(CHRROM & 0xff),
                (byte)(
                    (NametableLayout       ? 1 : 0) |
                    (Battery               ? 2 : 0) |
                    (Trainer               ? 1 : 0) |
                    (AlternativeNametables ? 1 : 0) |
                    ((Mapper & 0b1111) <<  4)),
                (byte)(0x08 | CPU | (Mapper & 0xf0)),
                (byte)((Mapper >> 8) | (SubMapper << 4)),
                (byte)(
                    (PRGROM >> 8) |
                    (CHRROM & 0xf00) >> 8),
                (byte)(PRGRAM | (EEPROM << 4)),
                (byte)(CPU),
                (byte)(
                    ((ConsoleType == 1 ? 1: 0) * ((VsType << 4) |
                    VsPPU))
                    |
                    ((ConsoleType == 3 ? 1 : 0) * ExtenedConsoleType)),
                (byte)(MiscROMs),
                (byte)(DefaultExpansionDevice)
            ];
        }
    }
}