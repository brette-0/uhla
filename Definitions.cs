using UHLA.Engine;

namespace UHLA;

internal static class Definitions {
    
    
    private static Dictionary<(ObjectToken Scope, string alias), List<ObjectToken>> Dependencies = [];
}




/*

    This module is for lazy definition only, other forms of definition are simple.
        TODO: Consider migrating standard definition management into this module
        
    'Check Definition'
        -> 'object being given first value' since lack of definition    || definition != declaration    

*/