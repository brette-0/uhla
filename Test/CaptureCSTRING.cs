bool CaptureCSTRING(Func<char, bool> HaltCapturePredicate, ref int ErrorReportLineNumber, ref int ErrorReportStepNumber) {
    int csi = StringIndex;

    for (; i < RegexTokens.Count && !HaltCapturePredicate(RegexTokens[i][0]); i++) {
        LITERAL_CSTRING += RegexTokens[i];
        StringIndex += RegexTokens[i].Length;
    }

    if (i == RegexTokens.Count && ErrorContext.ErrorLevel == default) {
        // Unterminated String
        ErrorContext = new() {
            ErrorLevel      = ErrorLevels.ERROR,
            ErrorType       = ErrorTypes.SyntaxError,
            DecodingPhase   = DecodingPhases.TOKEN,
            Message         = "Unterminated String",
            LineNumber      = ErrorReportLineNumber,
            StepNumber      = ErrorReportStepNumber,
            Context         = () => ApplyWiggle(CollectiveContext, csi + 1, StringIndex - csi)
        };
        return false;
    }

    if (csi != StringIndex) {
        DeltaTermTokens.Add((
            csi,
            csi - StringIndex,
            new Dictionary<string, (object data, AssembleTimeTypes type, AccessLevels access)>() {
                    {"self",    (LITERAL_CSTRING,           AssembleTimeTypes.CSTRING,  AccessLevels.PRIVATE) },
                    {"length",  (LITERAL_CSTRING.Length,    AssembleTimeTypes.CINT,     AccessLevels.PUBLIC) },
            },
            false
        ));

        LITERAL_CSTRING = "";   // wipe string for next capture
    }
}