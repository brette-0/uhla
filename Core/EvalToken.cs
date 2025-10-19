namespace uhla.Core;

internal class EvalToken {
    internal EvalToken(
        int         pStringIndex,
        int         pStringLength,
        ObjectToken pData,
        bool        pIsOperator
    ) {
        Data         = pData;
        StringIndex  = pStringIndex;
        StringLength = pStringLength;
        IsOperator   = pIsOperator;
    }
            
    internal EvalToken(
        EvalToken pET
    ) {
        Data         = pET.Data;
        StringIndex  = pET.StringIndex;
        StringLength = pET.StringLength;
        IsOperator   = pET.IsOperator;
    }
            
            
    internal object            ObjectData => Data.data;
    internal AssembleTimeTypes ObjectType => Data.type;

    internal ObjectToken Data;
    internal int         StringIndex, StringLength; // debugging indexes
    internal bool        IsOperator;                // if its an operator or a value
}