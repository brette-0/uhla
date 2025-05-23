using Tataru.Mapper;
using Tataru.Headers;

namespace Tataru {
    namespace Memory {
        internal class Memory {
            internal Memory(Mappers Mapper, Header _Header) {
                byte[] DirectRAM  = new byte[0x100];
                byte[] SystemRAM  = new byte[0x600];

                byte[] ProgramRAM;
                byte[] MapperRAM;
                byte[] FPGARAM;

                ulong  PRGRAMADDR;

                switch (Mapper) {
                    case Mappers.NROM:
                        break;


                    default:
                        Console.WriteLine($"Unsupported Mapper Type {(int)Mapper} : Features will be reduced.");
                        break;
                }
            }
        }
    }
}