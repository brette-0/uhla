namespace Numinous;

using Bitfield;

internal static class Memory {
    internal static void Initialize() {
        if (Program.Mode == Modes.Disk) {
            // FDS Mode

            return;
        }

        MemoryMapping["ZP"]     = (new Bitfield(0x100), 0);     LastReserveIndex["ZP"]      = 0;
        MemoryMapping["SYSTEM"] = (new Bitfield(0x100), 0x100); LastReserveIndex["SYSTEM"]  = 0;

        // NES Mode
        // By Header
    }

    internal enum MemoryModes {
        DIRECT,
        SYSTEM,
        
        FAST,
        SLOW,
        
        MAPPER,
        PROGRAM
    }

    internal static bool TryReserve(MemoryModes MemoryMode, int bytes) {
        int    offset = -1;
        string TargetLocation;
            
        switch (MemoryMode) {
            case MemoryModes.DIRECT:
                if (!GetReserveIndex("ZP")) return false;
                Reserve("ZP");
                break;
            
            case MemoryModes.SYSTEM:
                if (!GetReserveIndex("SYSTEM")) return false;
                Reserve("SYSTEM");
                break;
            
            case MemoryModes.FAST:
                if (GetReserveIndex("ZP")) {
                    Reserve("ZP");
                    break;
                }
                
                if (GetReserveIndex("SYSTEM")) {
                    Reserve("ZP");
                    break;
                }

                TargetLocation = Header.MemoryLocationSequence(bytes);
                if (TargetLocation == string.Empty) return false;

                Reserve(TargetLocation);
                break;
            
            case MemoryModes.SLOW:
                if (GetReserveIndex("SYSTEM")) {
                    Reserve("ZP");
                    break;
                }
                
                TargetLocation = Header.MemoryLocationSequence(bytes);
                if (TargetLocation != string.Empty) {
                    Reserve(TargetLocation);
                    break;
                }
                
                if (GetReserveIndex("ZP")) {
                    // warn/error (verbose) : could not reserve variable as slow.
                    Reserve("ZP");
                    break;
                }
                
                break;
            
            case MemoryModes.MAPPER:
                if (!Header.HasMapperMemory) {
                    // error : Current Mapper {Mapper} does not support mapper memory.
                    return false;
                }
                
                if (!GetReserveIndex("SYSTEM")) return false;
                Reserve("SYSTEM");
                break;
            
            case MemoryModes.PROGRAM:
                if (Header.sProgramRAM == 0) {
                    // error : user has not requested program RAM
                    return false;
                }
                
                if (!GetReserveIndex("SYSTEM")) return false;
                Reserve("SYSTEM");
                break;
            
            default:
                throw new ArgumentOutOfRangeException(nameof(MemoryMode), MemoryMode, null);
        }
        
        return false;

        bool GetReserveIndex(string region) {
            if (LastReserveIndex[region] + bytes > MemoryMapping[region].field.Length) {
                var Reserved = 0;
                var i        = 0;

                for (; i < MemoryMapping[region].field.Length; i++) {
                    if (MemoryMapping[region].field[i]) {
                        Reserved++;
                    } else {
                        Reserved = 0;
                    }
                }

                if (Reserved == bytes) {
                    // warn fragmentation, succeed
                    offset = i;
                } else {
                    // could not allocate
                    return false;
                }
                
            } else offset = LastReserveIndex[region];
            return true;
        }
        void Reserve(string region) { for (var i = 0; i < bytes; i++) MemoryMapping[region].field[offset + i] = true; }
    }

    internal static Dictionary<string, int> LastReserveIndex = [];
    internal static Dictionary<string, (Bitfield field, int offset)> MemoryMapping = [];
}