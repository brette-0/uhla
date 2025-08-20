using Numinous.Engine;

namespace Numinous;

internal static class DefinitionManager {
    
    internal enum ReferenceType{
        Object = 0,     // pass down object reference, not self value.
        
        Absolute = 1,   // pass down the cpu address as 16bit value
        Direct = 2,     // pass down the cpu address as 8-bit value

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

    internal static void CheckDeclare(Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> scope, string key, ReferenceType type,  int rom = -1) {
        if (UndefinedReferences.ContainsKey((scope, key))) {
            if (rom != -1) UndefinedReferences[(scope, key)].Add((rom, type));
        }
    }

    internal static void Declare(Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> scope, string label, Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> ctx) {
        if (UndefinedReferences.ContainsKey((scope, label))) {
            // write to output ROM for each entry in UndefinedReferences[(scope, label)]
        }

        UndefinedReferences.Remove((scope, label));
    }
    
    // (scope, label) => list of references in ROM
    internal static Dictionary<(Dictionary<string, (object data, AssembleTimeTypes, AccessLevels access)> scope, string key), List<(int location, ReferenceType type)>> UndefinedReferences = [];
}