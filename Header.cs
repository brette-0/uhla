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

        static internal class Header {
            static internal byte[] Raw = [];
            static internal Targets Target;

            static internal int PRGROM, CHRROM, Mapper, ConsoleType, SubMapper, PRGRAM, EEPROM, CHRRAM, CHRNVRAM, CPU, VsType, VsPPU, ExtenedConsoleType, MiscROMs, DefaultExpansionDevice;
            static internal bool NametableLayout, Battery, Trainer, AlternativeNametables;

            static internal void FDS(int FDS_DiskAmt) {
                Target = Targets.FDS;
                Raw = [0x46, 0x44, 0x53, 0x1a, (byte)(FDS_DiskAmt / ((FDS_DiskAmt > 65500) ? 65500 : 1))];
            }

            static internal void NES(int __PRGROM__, int __CHRROM__, bool __NametableLayout__, bool __Battery__, bool __Trainer__, bool __AlternativeNametables__, int __Mapper__, int __ConsoleType__, int __SubMapper__, int __PRGRAM__, int __EEPROM__, int __CHRRAM__, int __CHRNVRAM__, int __CPU__, int __VsType__, int __VsPPU__, int __ExtenedConsoleType__, int __MiscROMs__, int __DefaultExpansionDevice__) {
                Target = Targets.NES;
                if (__PRGROM__ > 0x4000) {
                    // since iNES2 spec allows for 2 bytes of __PRGROM__ in multiples of 16KiB should it be that 0x4000 is specified its unlikely that it refers to ~268MiB so we do it this way
                    __PRGROM__ /= 0x4000;
                }

                if (__CHRROM__ > 0x2000) {
                    // likewise ~67MiB of Graphics ROM is always quite unbelievable
                    __CHRROM__ /= 0x2000;
                }

                // __PRGRAM__ is stored as (0x40 << n)
                if (__PRGRAM__ >= 0x40) {
                    __PRGRAM__ = (int)MathF.Log(__PRGRAM__) / 0x40;
                }

                // __EEPROM__ is stored as (0x40 << n)
                if (__EEPROM__ >= 0x40) {
                    __EEPROM__ = (int)MathF.Log(__EEPROM__) / 0x40;
                }

                // __CHRRAM__ is stored as (0x40 << n)
                if (__CHRRAM__ >= 0x40) {
                    __CHRRAM__ = (int)MathF.Log(__CHRRAM__) / 0x40;
                }

                // __CHRNVRAM__ is stored as (0x40 << n)
                if (__CHRNVRAM__ >= 0x40) {
                    __CHRNVRAM__ = (int)MathF.Log(__CHRNVRAM__) / 0x40;
                }

                Raw = [
                    0x4e, 0x45, 0x53, 0x1a, // NES<EOF>
                (byte)(__PRGROM__ & 0xff),
                (byte)(__CHRROM__ & 0xff),
                (byte)(
                    (__NametableLayout__       ? 1 : 0) |
                    (__Battery__               ? 2 : 0) |
                    (__Trainer__               ? 4 : 0) |
                    (__AlternativeNametables__ ? 8 : 0) |
                    ((__Mapper__ & 0b1111) <<  4)),
                (byte)(0x08 | __CPU__ | (__Mapper__ & 0xf0)),
                (byte)((__Mapper__ >> 8) | (__SubMapper__ << 4)),
                (byte)(
                    (__PRGROM__ >> 8) |
                    (__CHRROM__ & 0xf00) >> 8),
                (byte)(__PRGRAM__ | (__EEPROM__ << 4)),
                (byte)(__CPU__),
                (byte)(
                    ((__ConsoleType__ == 1 ? 1: 0) * ((__VsType__ << 4) |
                    __VsPPU__))
                    |
                    ((__ConsoleType__ == 3 ? 1 : 0) * __ExtenedConsoleType__)),
                (byte)(__MiscROMs__),
                (byte)(__DefaultExpansionDevice__)
                ];

                PRGROM = __PRGROM__;
                CHRROM = __CHRROM__;
                PRGRAM = __PRGRAM__;
                CHRRAM = __CHRRAM__;
                NametableLayout = __NametableLayout__;
                Battery = __Battery__;
                Trainer = __Trainer__;
                AlternativeNametables = __AlternativeNametables__;
                Mapper = __Mapper__;
                CPU = __CPU__;
                ConsoleType = __ConsoleType__;
                VsType = __VsType__;
                VsPPU = __VsPPU__;
                ExtenedConsoleType = __ExtenedConsoleType__;
                MiscROMs = __MiscROMs__;
                DefaultExpansionDevice = __DefaultExpansionDevice__;
            }
        }
    }
}
