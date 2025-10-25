namespace uhla.Core;

internal class EvalToken {
    internal EvalToken(
        int         pStringIndex,
        int         pStringLength,
        ObjectToken pData
    ) {
        Data         = pData;
        StringIndex  = pStringIndex;
        StringLength = pStringLength;
    }
            
    internal EvalToken(
        EvalToken pET
    ) {
        Data         = pET.Data;
        StringIndex  = pET.StringIndex;
        StringLength = pET.StringLength;
    }
            
            
    internal object            ObjectData => Data.data;
    internal AssembleTimeTypes ObjectType => Data.type;

    internal ObjectToken Data;
    internal int         StringIndex, StringLength; // debugging indexes
}