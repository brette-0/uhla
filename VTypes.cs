#pragma warning disable IDE1006
namespace Tataru {
    internal class VTypes {
        internal class Num {
            [Flags]
            public enum TypeFlags {
                Endian    = 0x01,
                Decimal   = 0x02,
                Signed    = 0x04
            }

            TypeFlags TypeMeta;
            public int Width;
            public ushort Offset;

            protected internal Num(TypeFlags _TypeFlags, int _Width) {
                Width = _Width; TypeMeta = _TypeFlags;
            }
        }

        internal class MPET<T> {
            internal struct DeterminismStruct {
                public ulong IndexField, ValueField;
            }
            DeterminismStruct Determinism;
            internal T Value;

            protected internal MPET(T value, ulong indexField, ulong valueField) {
                Value = value;
                Determinism = new DeterminismStruct { IndexField = indexField, ValueField = valueField };
            }
        }


        internal class u8    : Num { internal    u8(ushort _Offset) : base(0, 1) { base.Offset = _Offset; } }
        internal class u16   : Num { internal   u16(ushort _Offset) : base(0, 2) { base.Offset = _Offset; } }
        internal class u24   : Num { internal   u24(ushort _Offset) : base(0, 3) { base.Offset = _Offset; } }
        internal class u32   : Num { internal   u32(ushort _Offset) : base(0, 4) { base.Offset = _Offset; } }
        internal class u64   : Num { internal   u64(ushort _Offset) : base(0, 8) { base.Offset = _Offset; } }
        internal class i8    : Num { internal    i8(ushort _Offset) : base(Num.TypeFlags.Signed, 1) { base.Offset = _Offset; } }
        internal class i16   : Num { internal   i16(ushort _Offset) : base(Num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class i24   : Num { internal   i24(ushort _Offset) : base(Num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class i32   : Num { internal   i32(ushort _Offset) : base(Num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class i64   : Num { internal   i64(ushort _Offset) : base(Num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class du8   : Num { internal   du8(ushort _Offset) : base(Num.TypeFlags.Decimal, 1) { base.Offset = _Offset; } }
        internal class du16  : Num { internal  du16(ushort _Offset) : base(Num.TypeFlags.Decimal, 2) { base.Offset = _Offset; } }
        internal class du24  : Num { internal  du24(ushort _Offset) : base(Num.TypeFlags.Decimal, 3) { base.Offset = _Offset; } }
        internal class du32  : Num { internal  du32(ushort _Offset) : base(Num.TypeFlags.Decimal, 4) { base.Offset = _Offset; } }
        internal class du64  : Num { internal  du64(ushort _Offset) : base(Num.TypeFlags.Decimal, 8) { base.Offset = _Offset; } }
        internal class di8   : Num { internal   di8(ushort _Offset) : base(Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 1) { base.Offset = _Offset; } }
        internal class di16  : Num { internal  di16(ushort _Offset) : base(Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class di24  : Num { internal  di24(ushort _Offset) : base(Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class di32  : Num { internal  di32(ushort _Offset) : base(Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class di64  : Num { internal  di64(ushort _Offset) : base(Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class bu16  : Num { internal  bu16(ushort _Offset) : base(Num.TypeFlags.Endian, 2) { base.Offset = _Offset; } }
        internal class bu24  : Num { internal  bu24(ushort _Offset) : base(Num.TypeFlags.Endian, 3) { base.Offset = _Offset; } }
        internal class bu32  : Num { internal  bu32(ushort _Offset) : base(Num.TypeFlags.Endian, 4) { base.Offset = _Offset; } }
        internal class bu64  : Num { internal  bu64(ushort _Offset) : base(Num.TypeFlags.Endian, 8) { base.Offset = _Offset; } }
        internal class bi16  : Num { internal  bi16(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class bi24  : Num { internal  bi24(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class bi32  : Num { internal  bi32(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class bi64  : Num { internal  bi64(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
        internal class bdu16 : Num { internal bdu16(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal, 2) { base.Offset = _Offset; } }
        internal class bdu24 : Num { internal bdu24(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal, 3) { base.Offset = _Offset; } }
        internal class bdu32 : Num { internal bdu32(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal, 4) { base.Offset = _Offset; } }
        internal class bdu64 : Num { internal bdu64(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal, 8) { base.Offset = _Offset; } }
        internal class bdi16 : Num { internal bdi16(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 2) { base.Offset = _Offset; } }
        internal class bdi24 : Num { internal bdi24(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 3) { base.Offset = _Offset; } }
        internal class bdi32 : Num { internal bdi32(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 4) { base.Offset = _Offset; } }
        internal class bdi64 : Num { internal bdi64(ushort _Offset) : base(Num.TypeFlags.Endian | Num.TypeFlags.Decimal | Num.TypeFlags.Signed, 8) { base.Offset = _Offset; } }
    
        internal class p7    : MPET<i8>    { internal    p7(i8    Variable, ushort _Offset) : base(Variable, 0x0000000000000080, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class p15   : MPET<i16>   { internal   p15(i16   Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class p23   : MPET<i24>   { internal   p23(i24   Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class p31   : MPET<i32>   { internal   p31(i32   Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class p63   : MPET<i64>   { internal   p63(i64   Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class n7    : MPET<i8>    { internal    n7(i8    Variable, ushort _Offset) : base(Variable, 0x0000000000000080, 0x0000000000000080) { base.Value.Offset = _Offset; } }
        internal class n15   : MPET<i16>   { internal   n15(i16   Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000008000) { base.Value.Offset = _Offset; } }
        internal class n23   : MPET<i24>   { internal   n23(i24   Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000800000) { base.Value.Offset = _Offset; } }
        internal class n31   : MPET<i32>   { internal   n31(i32   Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000080000000) { base.Value.Offset = _Offset; } }
        internal class n63   : MPET<i64>   { internal   n63(i64   Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x8000000000000000) { base.Value.Offset = _Offset; } }
        internal class dp7   : MPET<di8>   { internal   dp7(di8   Variable, ushort _Offset) : base(Variable, 0x0000000000000080, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class dp15  : MPET<di16>  { internal  dp15(di16  Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class dp23  : MPET<di24>  { internal  dp23(di24  Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class dp31  : MPET<di32>  { internal  dp31(di32  Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class dp63  : MPET<di64>  { internal  dp63(di64  Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class dn7   : MPET<di8>   { internal   dn7(di8   Variable, ushort _Offset) : base(Variable, 0x0000000000000080, 0x0000000000000080) { base.Value.Offset = _Offset; } }
        internal class dn15  : MPET<di16>  { internal  dn15(di16  Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000008000) { base.Value.Offset = _Offset; } }
        internal class dn23  : MPET<di24>  { internal  dn23(di24  Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000800000) { base.Value.Offset = _Offset; } }
        internal class dn31  : MPET<di32>  { internal  dn31(di32  Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000080000000) { base.Value.Offset = _Offset; } }
        internal class dn63  : MPET<di64>  { internal  dn63(di64  Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x8000000000000000) { base.Value.Offset = _Offset; } }
        internal class bp15  : MPET<bi16>  { internal  bp15(bi16  Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bp23  : MPET<bi24>  { internal  bp23(bi24  Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bp31  : MPET<bi32>  { internal  bp31(bi32  Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bp63  : MPET<bi64>  { internal  bp63(bi64  Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bn15  : MPET<bi16>  { internal  bn15(bi16  Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000008000) { base.Value.Offset = _Offset; } }
        internal class bn23  : MPET<bi24>  { internal  bn23(bi24  Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000800000) { base.Value.Offset = _Offset; } }
        internal class bn31  : MPET<bi32>  { internal  bn31(bi32  Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000080000000) { base.Value.Offset = _Offset; } }
        internal class bn63  : MPET<bi64>  { internal  bn63(bi64  Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x8000000000000000) { base.Value.Offset = _Offset; } }
        internal class bdp15 : MPET<bdi16> { internal bdp15(bdi16 Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bdp23 : MPET<bdi24> { internal bdp23(bdi24 Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bdp31 : MPET<bdi32> { internal bdp31(bdi32 Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bdp63 : MPET<bdi64> { internal bdp63(bdi64 Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x0000000000000000) { base.Value.Offset = _Offset; } }
        internal class bdn15 : MPET<bdi16> { internal bdn15(bdi16 Variable, ushort _Offset) : base(Variable, 0x0000000000008000, 0x0000000000008000) { base.Value.Offset = _Offset; } }
        internal class bdn23 : MPET<bdi24> { internal bdn23(bdi24 Variable, ushort _Offset) : base(Variable, 0x0000000000800000, 0x0000000000800000) { base.Value.Offset = _Offset; } }
        internal class bdn31 : MPET<bdi32> { internal bdn31(bdi32 Variable, ushort _Offset) : base(Variable, 0x0000000080000000, 0x0000000080000000) { base.Value.Offset = _Offset; } }
        internal class bdn63 : MPET<bdi64> { internal bdn63(bdi64 Variable, ushort _Offset) : base(Variable, 0x8000000000000000, 0x8000000000000000) { base.Value.Offset = _Offset; } }
    }
}