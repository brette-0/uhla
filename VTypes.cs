using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Tataru {
    internal class VTypes {
        internal class num {
            [Flags]
            public enum TypeFlags {
                Endian    = 0x01,
                Decimal   = 0x02,
                Signed    = 0x04,
                Positive  = 0x08,
            }

            TypeFlags TypeMeta;
            public int Width;
            public ushort Offset;

            protected internal num(TypeFlags _TypeFlags, int _Width) {
                Width = _Width; TypeMeta = _TypeFlags;
            }

                // u8 holds Width 1 and TypeMeta 0
                // i8 holds Width 1 and TypeMeta 4
                // p7 holds Width 1 and TypeMeta 8
                // n7 holds Width 1 and TypeMeta c
                // bdn63 holds Width 8 and TypeMeta f
        }


        internal class u8    : num { internal    u8(ushort _Offset) : base(0, 1) { base.Offset = _Offset; } }
        internal class u16   : num { internal   u16(ushort _Offset) : base(0, 2) { base.Offset = _Offset; } }
        internal class u24   : num { internal   u24(ushort _Offset) : base(0, 3) { base.Offset = _Offset; } }
        internal class u32   : num { internal   u32(ushort _Offset) : base(0, 4) { base.Offset = _Offset; } }
        internal class u64   : num { internal   u64(ushort _Offset) : base(0, 8) { base.Offset = _Offset; } }
        internal class i8    : num { internal    i8(ushort _Offset) : base(num.TypeFlags.Signed, 1) { base.Offset = _Offset; } }
        internal class i16   : num { internal   i16(ushort _Offset) : base(num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class i24   : num { internal   i24(ushort _Offset) : base(num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class i32   : num { internal   i32(ushort _Offset) : base(num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class i64   : num { internal   i64(ushort _Offset) : base(num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class p7    : num { internal    p7(ushort _Offset) : base(num.TypeFlags.Positive, 1) { base.Offset = _Offset; } }
        internal class p15   : num { internal   p15(ushort _Offset) : base(num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class p23   : num { internal   p23(ushort _Offset) : base(num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class p31   : num { internal   p31(ushort _Offset) : base(num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class p63   : num { internal   p63(ushort _Offset) : base(num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class n7    : num { internal    n7(ushort _Offset) : base(num.TypeFlags.Signed | num.TypeFlags.Positive, 1) { base.Offset = _Offset; } }
        internal class n15   : num { internal   n15(ushort _Offset) : base(num.TypeFlags.Signed | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class n23   : num { internal   n23(ushort _Offset) : base(num.TypeFlags.Signed | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class n31   : num { internal   n31(ushort _Offset) : base(num.TypeFlags.Signed | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class n63   : num { internal   n63(ushort _Offset) : base(num.TypeFlags.Signed | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class du8   : num { internal   du8(ushort _Offset) : base(num.TypeFlags.Decimal, 1) { base.Offset = _Offset; } }
        internal class du16  : num { internal  du16(ushort _Offset) : base(num.TypeFlags.Decimal, 2) { base.Offset = _Offset; } }
        internal class du24  : num { internal  du24(ushort _Offset) : base(num.TypeFlags.Decimal, 3) { base.Offset = _Offset; } }
        internal class du32  : num { internal  du32(ushort _Offset) : base(num.TypeFlags.Decimal, 4) { base.Offset = _Offset; } }
        internal class du64  : num { internal  du64(ushort _Offset) : base(num.TypeFlags.Decimal, 8) { base.Offset = _Offset; } }
        internal class di8   : num { internal   di8(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed, 1) { base.Offset = _Offset; } }
        internal class di16  : num { internal  di16(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class di24  : num { internal  di24(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class di32  : num { internal  di32(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class di64  : num { internal  di64(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class dp7   : num { internal   dp7(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Positive, 1) { base.Offset = _Offset; } }
        internal class dp15  : num { internal  dp15(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class dp23  : num { internal  dp23(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class dp31  : num { internal  dp31(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class dp63  : num { internal  dp63(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class dn7   : num { internal   dn7(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 1) { base.Offset = _Offset; } }
        internal class dn15  : num { internal  dn15(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class dn23  : num { internal  dn23(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class dn31  : num { internal  dn31(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class dn63  : num { internal  dn63(ushort _Offset) : base(num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class bu16  : num { internal  bu16(ushort _Offset) : base(num.TypeFlags.Endian, 2) { base.Offset = _Offset; } }
        internal class bu24  : num { internal  bu24(ushort _Offset) : base(num.TypeFlags.Endian, 3) { base.Offset = _Offset; } }
        internal class bu32  : num { internal  bu32(ushort _Offset) : base(num.TypeFlags.Endian, 4) { base.Offset = _Offset; } }
        internal class bu64  : num { internal  bu64(ushort _Offset) : base(num.TypeFlags.Endian, 8) { base.Offset = _Offset; } }
        internal class bi16  : num { internal  bi16(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class bi24  : num { internal  bi24(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class bi32  : num { internal  bi32(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class bi64  : num { internal  bi64(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class bp15  : num { internal  bp15(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class bp23  : num { internal  bp23(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class bp31  : num { internal  bp31(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class bp63  : num { internal  bp63(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class bn15  : num { internal  bn15(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class bn23  : num { internal  bn23(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class bn31  : num { internal  bn31(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class bn63  : num { internal  bn63(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Signed | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class bdu16 : num { internal bdu16(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal, 2) { base.Offset = _Offset; } }
        internal class bdu24 : num { internal bdu24(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal, 3) { base.Offset = _Offset; } }
        internal class bdu32 : num { internal bdu32(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal, 4) { base.Offset = _Offset; } }
        internal class bdu64 : num { internal bdu64(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal, 8) { base.Offset = _Offset; } }
        internal class bdi16 : num { internal bdi16(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class bdi24 : num { internal bdi24(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class bdi32 : num { internal bdi32(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class bdi64 : num { internal bdi64(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class bdp15 : num { internal bdp15(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class bdp23 : num { internal bdp23(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class bdp31 : num { internal bdp31(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class bdp63 : num { internal bdp63(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
        internal class bdn15 : num { internal bdn15(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 2) { base.Offset = _Offset; } }
        internal class bdn23 : num { internal bdn23(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 3) { base.Offset = _Offset; } }
        internal class bdn31 : num { internal bdn31(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 4) { base.Offset = _Offset; } }
        internal class bdn63 : num { internal bdn63(ushort _Offset) : base(num.TypeFlags.Endian | num.TypeFlags.Decimal | num.TypeFlags.Signed | num.TypeFlags.Positive, 8) { base.Offset = _Offset; } }
    }
}