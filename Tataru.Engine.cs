namespace Tataru {
    namespace Engine {
        namespace Types {
            internal struct Macro {         // basically an incredibly strong string, but its callable. We need to preserve this information.
                string[] Code;
                RuntimeTypeHandle Type;
            }

            internal struct Label {
                RuntimeTypeHandle Type;
                object Value;
            }

            internal enum ExitConditions { 
                OK, ERROR, CONTINUE
            }

            [Flags]
            internal enum MemoryAddressMode {
                Implicit  = 0x001,
                Direct    = 0x002,
                DirectX   = 0x004,
                DirectY   = 0x008,
                Absolute  = 0x010,
                AbsoluteX = 0x020,
                AbsoluteY = 0x030,
                Indirect  = 0x080,
                IndirectX = 0x100,
                IndirectY = 0x200
            }
        }
    }
}
