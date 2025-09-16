using UHLA.Engine;
using static UHLA.Engine.Engine;

namespace UHLA;

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

    internal static MemoryModes? GetDefaultMemoryMode() {
        var ScopeTypeMember = Database.GetObjectFromAlias("type", AccessLevels.PUBLIC);
        if (ScopeTypeMember is null) {
            // Major error, we don't even know this is possible. This was written, however, to suppress a warning.
            return null;
        }

        switch ((ScopeTypes?)ScopeTypeMember.GetMember(string.Empty, AccessLevels.PRIVATE)?.data) {
            case ScopeTypes.Bank:
            case ScopeTypes.Root:
                return MemoryModes.SYSTEM;

            case ScopeTypes.Namespace:
            case ScopeTypes.Procedure:
                return MemoryModes.SLOW;

            case ScopeTypes.Macro:
                return MemoryModes.FAST;

            case ScopeTypes.Interrupt:
                // error : may not reserve memory within an interrupt!
                return null;

            case null:
                // FATAL : could not get local scopes 'type' member's content member.
                throw new ArgumentOutOfRangeException();

            default:
                // FATAL : out of range value obtained getting local scopes 'type' member's content member.
                throw new ArgumentOutOfRangeException();
        }
    }

    internal static bool TryReserve(MemoryModes MemoryMode, int bytes, int offset = -1) {
        var TargetLocation = string.Empty;
            
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

                TargetLocation = Header.MemoryLocationSequence(bytes, offset);
                if (TargetLocation == string.Empty) return false;

                Reserve(TargetLocation);
                break;
            
            case MemoryModes.SLOW:
                if (GetReserveIndex("SYSTEM")) {
                    Reserve("ZP");
                    break;
                }
                
                TargetLocation = Header.MemoryLocationSequence(bytes, offset);
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
            var Reserved = 0;
            var i        = 0;
            
            if (offset > -1) {
                for (; i < bytes; i++) {
                    if (MemoryMapping[region].field[i]) {
                        Reserved++;
                    } else {
                        Reserved = 0;
                    }
                }

                if (Reserved != bytes) {
                    // could not reserve at specific location
                    return false;
                }

                if (offset > 0 && !MemoryMapping[region].field[i - 1]) {
                    // warn fragmentation
                }
            } else if (LastReserveIndex[region] + bytes > MemoryMapping[region].field.Length) {
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