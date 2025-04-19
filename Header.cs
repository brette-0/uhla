using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Tataru {
    namespace Header {
        public enum Targets {
            None = -1,          // We permit untargetted code very briefly
            NES = 0,
            FDS = 1
        }

        public class Header {
            public byte[] raw;

            Header(int FDS_DiskAmt) {
                Target = Targets.FDS;
                raw = [0x46, 0x44, 0x53, 0x1a, (byte)(FDS_DiskAmt / ((FDS_DiskAmt > 65500) ? 65500 : 1))];
            }

            Header(int PRGROM, int CHRROM, bool NametableLayout, bool Battery, bool Trainer, bool AlternativeNametables, int Mapper, int ConsoleType, int SubMapper, int PRGRAM, int EEPROM, int CHRRAM, int CHRNVRAM, int CPU, int VsType, int VsPPU, int ExtenedConsoleType, int MiscROMs, int DefaultExpansionDevice) {
                Target = Targets.NES;
                if (PRGROM > 0x4000) {
                    // since iNES2 spec allows for 2 bytes of PRGROM in multiples of 16KiB should it be that 0x4000 is specified its unlikely that it refers to ~268MiB so we do it this way
                    PRGROM /= 0x4000;
                }

                if (CHRROM > 0x2000) {
                    // likewise ~67MiB of Graphics ROM is always quite unbelievable
                    CHRROM /= 0x2000;
                }

                // PRGRAM is stored as (0x40 << n)
                if (PRGRAM >= 0x40) {
                    PRGRAM = (int)MathF.Log(PRGRAM) / 0x40;
                }

                // EEPROM is stored as (0x40 << n)
                if (EEPROM >= 0x40) {
                    EEPROM = (int)MathF.Log(EEPROM) / 0x40;
                }

                // CHRRAM is stored as (0x40 << n)
                if (CHRRAM >= 0x40) {
                    CHRRAM = (int)MathF.Log(CHRRAM) / 0x40;
                }

                // CHRNVRAM is stored as (0x40 << n)
                if (CHRNVRAM >= 0x40) {
                    CHRNVRAM = (int)MathF.Log(CHRNVRAM) / 0x40;
                }

                raw = [
                    0x4e, 0x45, 0x53, 0x1a, // NES<EOF>
                (byte)(PRGROM & 0xff),
                (byte)(CHRROM & 0xff),
                (byte)(
                    (NametableLayout       ? 1 : 0) |
                    (Battery               ? 2 : 0) |
                    (Trainer               ? 4 : 0) |
                    (AlternativeNametables ? 8 : 0) |
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
}
