namespace uhla.Core;

internal record struct HierarchyTokens_t {
    internal HierarchyTokens_t(List<List<EvalToken>> pDeltaTokens, int pHierachy, string pRepresentation) {
        DeltaTokens    = pDeltaTokens;
        Hierarchy      = pHierachy;
        Representation = pRepresentation;
    }

    /// <summary>
    /// Converts all members within a hierarchy into a tuple object.
    ///
    /// TODO: Check ref/weak/copy philosophy
    /// TODO: Ensure UNDEFINED porting
    /// 
    /// </summary>
    /// <param name="index"></param>
    internal EvalToken ToTuple() {
        Dictionary<string, EvalToken> Mapping = [];
        for (var i = 0; i < Mapping.Count; i++) {
            #if DEBUG
            if (DeltaTokens[i].Count > 0) {
                // error, cannot Tuplicate this!
                throw new Exception("Cannot Tuplicate this!");
            }
            #endif
            Mapping[$"{i}"] = DeltaTokens[i][0];
        }

        return new EvalToken(
            Mapping["0"].StringIndex,
            Mapping[$"{Mapping.Count - 1}"].StringIndex + Mapping[$"{Mapping.Count - 1}"].StringLength,
            new ObjectToken(
                Mapping,
                AssembleTimeTypes.TUPLE
            ),
            false
        );
    }
    
    internal List<List<EvalToken>> DeltaTokens;
    internal int                   Hierarchy;
    internal string                Representation;
}

/// <summary>
/// Handed back from LE, might be a request for more context, term collection or an EvalToken 
/// </summary>
internal record struct LE_Relationship {
    internal object            ctx;
    internal AssembleTimeTypes type;
}

internal enum EvaluationStatus : byte {
    ERROR,
    OK,
    SYMBOL_UNDEFINED,
}

internal enum Unary : byte {
    INC,
    DEC,
    ABS,
    NEG,
    BIT,
    NOT
};

[Flags]
internal enum WarningLevels : byte {
    IGNORE  = 0x00,
    DEFAULT = 0x01,
    ERROR   = 0x02,
    VERBOSE = 0x04,

    /* Internal     */
    NONE        = 0xff,
    NO_OVERRULE = 0x08,

    /* Composite    */
    STRICT     = VERBOSE | ERROR,
    CONTROLLED = VERBOSE | ERROR | NO_OVERRULE,

}

internal enum Operators : byte {
    INC,
    DEC,
    BITNOT,
    
    STRING,
    FSTRING,

    OPAREN,
    CPAREN,

    OBRACK,
    CBRACK,

    OBRACE,
    CBRACE,

    DESCOPE,

    PROPERTY,
    NULLPROPERTY,

    MULT,
    DIV,
    MOD,

    ADD,
    SUB,

    RIGHT,
    LEFT,

    BITMASK,

    BITFLIP,

    BITSET,

    GT,
    LT,
    GOET,
    LOET,
    SERIAL,

    EQUAL,
    INEQUAL,

    AND,

    OR,

    NULL,
    CHECK,
    ELSE,

    SET,
    INCREASE,
    DECREASE,
    MULTIPLY,
    DIVIDE,
    MODULATE,
    NULLSET,
    RIGHTSET,
    LEFTSET,

    ASSIGNMASK,
    ASSIGNSET,
    ASSIGNFLIP,

    TERM,
    NOT,

    NONE = 255
}

[Flags]
internal enum AssembleTimeTypes : byte {
    UNDEFINED,  // default
    INT,    // assemble time integer
    STRING, // assemble time string
    
    SCOPE, // scope type
    RT,    // Runtime Variable
    REG,   // Register
    FLAG,  // CPU Status Flag
    PROC,  // Procedure
    INTER, // Interrupt
    BANK,  // Bank
    EXP,   // Expression

    OBJECT, // The Boxed 'AnyType' such as long as its not constant      | Cast Exclusive Type

    FEXP,    // Functional Expression

    IRWN,  // Indexing Register with N             foo[i + 2] situations
    ICRWN, // Indexing Constant Register with N    foo[x + 2] situations

    FUNCTION, // Macro Function
    OPER,     // Operation

    MACRO = 0x80,
    
    TYPE,    // typeof result
    CHARMAP, // for ASCII redirection
    
    INDEX,
    CALL,
    
    TUPLE,   // elems = List<Object>    types = List<AssembleTimeTypes>
    
    OPERATOR,  // for unresolved but declared expressions
    }

// TODO: create constructors for all structs
internal record struct RunTimeVariableFilterType {
    internal uint? size;
    internal bool? signed;
    internal bool? endian;
}

internal record struct RunTimeVariableType {
    internal uint size;   // in bytes
    internal bool signed; // false => unsigned
    internal bool endian; // false => little
}

internal enum ErrorLevels : byte {
    NONE, LOG, WARN, ERROR
}

internal enum ErrorTypes : byte {
    None, SyntaxError, ParsingError, NothingToDo
}

internal enum DecodingPhases : byte {
    TERMINAL, TOKEN, EVALUATION
}

internal enum Architectures {
    None,
        
    NMOS_6502,
    NMOS_6507,                      // 6502 syntax, but limited address range.  '6502'

    RICOH_2A03,                     // NES/Famicom                              '2a03', 'nes', 'fds'
}
    
internal enum ScopeTypes {
    Root      = 0,
    Namespace = 1,
    Macro     = 2,
    Bank      = 3,
    Procedure = 4,
    Interrupt = 5
}

internal enum CheckDirectiveStatus {
    None,
    Success,
    Error
}

internal interface IArchitecture {

    bool MemoryReserve(ref RunTimeVariableType ctx);
    bool MemoryFree(ref    RunTimeVariableType ctx);
    
    
    /// <summary>
    /// Returns 'true' is the string represents a mnemonic for the target architecture.
    /// This includes implicits, synthetics and illegals. 
    /// </summary>
    /// <param name="mnemonic">the string to be checked if it is mnemonic.</param>
    /// <returns></returns>
    bool    IsMnemonic(string mnemonic);

    /// <summary>
    /// Reads ahead to confirm any information that the core will not be able to handle.
    /// For example, 6502 targets include 'a:', 'z:' and '!' as 'operand decorators'.
    /// </summary>
    /// <returns></returns>
    object? GatherAdditionalMnemonicContext();

    /// <summary>
    /// Leaves the implementation to decide if the request is possible based on intense validation.
    /// Mnemonic should be verified at this point and therefore should not be validated further.
    /// Returns the index of the problematic component or null if completely successful.
    /// </summary>
    /// <param name="mnemonic">The instruction attempting to encode</param>
    /// <param name="args">The context that modified the encoding</param>
    /// <returns></returns>
    int? TryCompleteInstruction(string mnemonic, ref List<EvalToken> args);

    void Initalize();

    CheckDirectiveStatus CheckDirective(ref List<EvalToken>                                         args,
                                        ref List<(string token, int StringIndex, int StringLength)> DefineResolveBuffer,
                                        ref int                                                     pStringIndex, ref int pTokenIndex,
                                        ref (string ctx, int StringIndex, int StringLength)         pActiveToken,
                                        ref string                                                  pRepresentation);
}