using System.Numerics;
using Numinous.Engine;

namespace Numinous;

public enum Mappers {
    NROM
}

public static class Header {
    // invoked by the header() function in Numinous
    
    public static bool NESHeaderFunction(List<string> args) {
        
            
        // get CPU space : read and write rules (CPU Space)

        if (!VerifyMapperIndependentAttributes()) return false;
        
        switch (Mapper) {
            case Mappers.NROM:
                if (AlternativeNametables) {
                    // warn/error that alt NT functionality not supported with NROM
                    return false;
                }
                
                if (sProgramRAM > 1) {
                    // Mapper doesn't support bankable Program Memory
                    return false;
                }
                
                if (sProgramROM > 2) {
                    // Mapper doesn't support bankable Program ROM
                    return false;
                }

                if (sCharacterROM > 1) {
                    // Mapper doesn't support bankable Character ROM
                    return false;
                }
                
                Program.ReadPermitted = i => {
                    if (i < 0x100) return true;
                    if (i < 0x200) {
                        // error : use of stack
                        return false;
                    }
                    if (i < 0x800) return true;
                    if (i < 0x2000 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error use of mirror if pedantic
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    if ((i & 0xf007) is 0x2000 or 0x2001 or 0x2003 or 0x2005 or 0x2006) {
                        // error, register is write-only
                        return false;
                    }

                    if (i < 0x4000 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error use of mirror if pedantic
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    if (i < 0x4018 && i != 0x4015) {
                        // error, register is write-only
                        return false;
                    }

                    if (i < 0x6000) {
                        // Open Bus!
                        return false;
                    }

                    if (i < 0x8000 && sProgramROM < 0x2000) {
                        // Open Bus!
                        return false;
                    }

                    if (i < 0xc000 && sProgramROM < 2 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error case acessing a mirror
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    return false;
                };
                
                Program.WritePermitted = i => {
                    if (i < 0x800) return true;
                    if (i < 0x2000 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error use of mirror if pedantic
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    if ((i & 0xf007) == 0x2002) {
                        // error, register is read-only
                        return false;
                    }

                    if (i < 0x4000 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error use of mirror if pedantic
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    if ((i & 0xf017) == 0x4015) {
                        // error, register is read-only
                        return false;
                    }

                    if (i < 0x6000) {
                        // Open Bus!
                        return false;
                    }

                    if (i < 0x8000 && sProgramROM < 0x2000) {
                        // Open Bus!
                        return false;
                    }

                    // without an MMC, writing to ROM is bad because of potential bus conflicts.
                    return false;
                };
                
                Program.CallPermitted = i => {
                    if (i < 0x800) return true;
                    if (i < 0x2000 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error use of mirror if pedantic
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    if (i > 0x2000 && i < 0x4018) {
                        // This area should not be called
                        return false;
                    }

                    if (i < 0x6000) {
                        // Open Bus!
                        return false;
                    }

                    if (i < 0x8000 && sProgramROM < 0x2000) {
                        // Open Bus!
                        return false;
                    }

                    if (i < 0xc000 && sProgramROM < 2 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                        // warn/error case
                        return Program.WarningLevel.HasFlag(WarningLevels.STRICT);
                    }

                    return false;
                };

                return true;
            
            default:
                // warn mapper not supported
                
                Program.ReadPermitted = _ => true;
                return true;
            
        }

        bool VerifyMapperIndependentAttributes() {
            if (Trainer && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                // warn/error : you probably don't want a trainer
                return false;
            }

            if (nMiscRoms > 0 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                // warn/error : you probably don't want this
                return false;
            }
            
            if (DefaultExpansionDevice == 0 && Program.WarningLevel.HasFlag(WarningLevels.VERBOSE)) {
                // warn/error : you might want to set it to 1 for Standard Controllers as default
                return false;
            }
            
            // Program ROM is tracked in 16KiB
            if ((sProgramROM - 1u & sProgramROM) != 0u) {
                // error : ROM size is not a power of 2.
                return false;
            } sProgramROM /= 0x4000;
        
            // Character ROM is tracked in 8KiB
            if ((sCharacterROM - 1u & sCharacterROM) != 0u) {
                // error : ROM size is not a power of 2.
                return false;
            } sCharacterROM /= 0x2000;

            if ((sProgramRAM - 1u & sProgramRAM) != 0u) {
                // error : ROM size is not a power of 2.
                return false;
            } sCharacterRAM = (uint)BitOperations.TrailingZeroCount(sProgramRAM);
        
            if ((sCharacterRAM - 1u & sCharacterRAM) != 0u) {
                // error : ROM size is not a power of 2.
                return false;
            } sCharacterRAM = (uint)BitOperations.TrailingZeroCount(sProgramRAM);
        
            if ((sEEPROM - 1u & sEEPROM) != 0u) {
                // error : ROM size is not a power of 2.
                return false;
            } sCharacterRAM = (uint)BitOperations.TrailingZeroCount(sEEPROM);
        
            if ((sCharacterNVRAM - 1u & sCharacterNVRAM) != 0u) {
                // error : ROM size is not a power of 2.
                return false;
            } sCharacterNVRAM = (uint)BitOperations.TrailingZeroCount(sCharacterNVRAM);

            if (sProgramRAM + sCharacterRAM == 0 && Battery) {
                // warn battery enabled but no memory for it to preserve
                return false;
            }

            return true;
        }
    }
    
    static internal Mappers Mapper                = Mappers.NROM;
    static internal uint    sProgramROM           = 1u;
    static internal uint    sCharacterROM         = 2u;
    static internal uint    NameTableArrangement  = 0u;
    static internal bool    Battery               = false;
    static internal bool    Trainer               = false;
    static internal bool    AlternativeNametables = false;
    static internal uint    ConsoleType           = 0u;
    static internal uint    sProgramRAM           = 0u;
    static internal uint    sEEPROM               = 0u;
    static internal uint    sCharacterRAM         = 0u;
    static internal uint    sCharacterNVRAM       = 0u;
    static internal uint    VSType                = 0;
    static internal uint    VSPPUType             = 0;
    static internal uint    ExtendedConsoleType   = 0;
    static internal uint    nMiscRoms             = 0u;
    static internal uint    DefaultExpansionDevice = 0u;

    static internal Func<int, int, string> MemoryLocationSequence;
    static internal bool    BankableProgramRAM {get; private set; } // refers to the 0x6000 space, if ROM went in here we'd still use this name.   
                                                                    // to know if TRULY RAM is bankable then we ensure that sProgarmRAM > 1.
                                                                    // purely a mapper property : does not reflect mapped contents
                                                                    
                                                                    
    static internal bool    HasMapperMemory {get; private set; }    // refers to memory preceding 0x6000. Will normally be false.
}