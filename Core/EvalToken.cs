namespace uhla.Core;

internal class EvalToken {
    internal EvalToken(
        int         pStringIndex,
        int         pStringLength,
        AssembleTimeObject pData
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

    internal bool IsOperator() => ObjectType is AssembleTimeTypes.OPERATOR;
            
            
    internal object            ObjectData => Data.data;
    internal AssembleTimeTypes ObjectType => Data.type;

    internal AssembleTimeObject Data;
    internal int         StringIndex, StringLength; // debugging indexes
}