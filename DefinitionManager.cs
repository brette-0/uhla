using Numinous.Engine;

namespace Numinous;

internal static class DefinitionManager {
    
    internal enum ReferenceType : short {
        Object = 0,     // pass down object reference, not self value.
        
        Absolute = -1,   // pass down the cpu address as 16bit value
        Direct = -2,     // pass down the cpu address as 8-bit value

        // anything else is calling Branch's CPU offset, calculate difference, validate and return
    }

    /// <summary>
    /// Checks declared but undefined expressions in the project
    /// </summary>
    /// <returns></returns>
    internal static bool CheckForTrailingDeclarations() {
        foreach (var entry in UndefinedReferences) {
            if (entry.Value.Count > 0) {
                // error : 'entry.key.scope:entry.key.key' is undefined but is referenced in the program.
                return false;
            }
        }

        if (UndefinedReferences.Keys.Count > 0) {
            // warning : unused declarations left undefined in the program.
            return !Program.WarningLevel.HasFlag(WarningLevels.ERROR);
        }

        return true;
    }

//    internal static bool CheckDeclare(Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> scope, string key, ReferenceType type,  int rom = -1) {
//
//        
//        if (UndefinedReferences.ContainsKey((scope, key))) {
//            // if so, ensure we are creating a constant! Using a variable here is cursed because it makes the value stored an assembler search property instead of user code.
//            // pragma to prevent this? Seriously, don't recommend this.
//            if (rom != -1) UndefinedReferences[(scope, key)].Add((rom, type));
//        }
//
//        return true;
//    }

    /// <summary>
    /// Checks if referred to before to support bwackwards definition
    /// </summary>
    /// <param name="scope">The scope object the member is contained in.</param>
    /// <param name="label">The label the user refers to the object by.</param>
    /// <param name="ctx">The information the user is referring to.</param>
    internal static void Declare(Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> scope, string label, Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> ctx) {
        // attempt to delta evaluate all object references
        // if DE is still unevaluable that's fine, because we can manage dependency subscriptions
        // if error, we can also handle that by returning false
        
        if (UndefinedReferences.ContainsKey((scope, label))) {
            // write to output ROM for each entry in UndefinedReferences[(scope, label)]
        }

        UndefinedReferences.Remove((scope, label));
    }
    
    // (scope, label) => list of references in project
    // when type is 0, its an object reference, when type is -1 it ROM space or -2. Any positive size is table length
    internal static Dictionary<(Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> scope, string key), List<(object target, ReferenceType type)>> UndefinedReferences = [];
}