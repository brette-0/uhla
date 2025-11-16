namespace uhla.Core;

/// <summary>
/// Database methods
/// </summary>
internal static class Database {
    /// <summary>
    /// Get without specifying target scope.
    /// 
    /// Order may be changed depending on Program.ObjectSearchBuffer. The ActiveScope is ALWAYS searched first, afterwards its down to this.
    /// By default the ObjectSearchBuffer only includes the root scope.
    /// </summary>
    /// <param name="Alias"></param>
    /// <returns></returns>
    internal static AssembleTimeObject? GetObjectFromAlias(string Alias) {
        List<Dictionary<string, AssembleTimeObject?>> LocalObjectSearchBuffer = [Program.ActiveScopeBuffer[^1], .. Program.ObjectSearchBuffer];
        return __GetObjectFromAlias(Alias, LocalObjectSearchBuffer);
    }
    /// <summary>
    /// Only check the specified scope, may be used like rs\foo. Note that the scope used to specify will be the result of the other method being used first.
    /// After this its hierarchy based and therefore rs\foo\foo may not always work.
    /// </summary>
    /// <param name="Alias"></param>
    /// <param name="TargetScope"></param>
    /// <returns></returns>
    /// 
    internal static AssembleTimeObject? GetObjectFromAlias(string Alias, Dictionary<string, AssembleTimeObject?> TargetScope) => __GetObjectFromAlias(Alias, [TargetScope]);
    
    /// <summary>
    /// Internal function iterating over the LocalObjectSearchPath to find the required context if possible.
    /// </summary>
    /// <param name="Alias"></param>
    /// <param name="LocalObjectSearchBuffer"></param>
    /// <returns></returns>
    private  static AssembleTimeObject? __GetObjectFromAlias(string Alias, List<Dictionary<string, AssembleTimeObject?>> LocalObjectSearchBuffer) {
        foreach (var LocalObjectSearchContainer in LocalObjectSearchBuffer) {
            AssembleTimeObject? ctx;
            if (LocalObjectSearchContainer.TryGetValue(Alias, out ctx)) {
                return ctx;
            }
        }
        
        return null;
    }

    // validating constant empty const declare or lazy const declare is not handled here.
    internal static bool Declare(string alias, AssembleTimeTypes type, List<HierarchyTokens_t> Query, bool isconst) {
        if (GetObjectFromAlias(alias) is not null) {
            // error, attempted re-declaration: this language is type static
            return false;
        }
        
        // Declaration
        var OT = new AssembleTimeObject(new Dictionary<string, AssembleTimeObject>() {
            {"#self", new AssembleTimeObject(Query, AssembleTimeTypes.UNDEFINED, false, false)}        
        }, type, false, isconst);

        Program.ActiveScopeBuffer[^1].Add(alias, OT);
        return true;
    }

    internal static bool Declare(string alias, RunTimeVariableType type, List<HierarchyTokens_t> Query, bool isconst) {
        if (GetObjectFromAlias(alias) is not null) {
            // error, attempted re-declaration: this language is type static
            return false;
        }
        
        // Declaration
        var OT = new AssembleTimeObject(new Dictionary<string, AssembleTimeObject>() {
            {"#self",   new AssembleTimeObject(Query, AssembleTimeTypes.UNDEFINED, false, false)},
            {"#offset", new AssembleTimeObject(Program.Architecture.MemoryReserve(ref type), AssembleTimeTypes.INT, true, true)}
        }, AssembleTimeTypes.RT, false, isconst);

        if ((int)((Dictionary<string, AssembleTimeObject>)OT.data)["#offset"].data is -1) {
            // failed memory reserve
            return false;
        }

        Program.ActiveScopeBuffer[^1].Add(alias, OT);
        return true;
    }

    /// <summary>
    /// Invoked from 'del foo'
    /// </summary>
    /// <param name="alias"></param>
    /// <param name="access"></param>
    /// <returns></returns>
    internal static bool DeleteMember(string alias) {
        var ctx = GetObjectFromAlias(alias);
        if (ctx is null) {
            // error, does not exist
            return false;
        }

        if (ctx.type is AssembleTimeTypes.RT) {
            // return based on Memory.Remove?
            var rtv = (RunTimeVariableType)ctx.GetMember("#self")!.data;
            Program.Architecture.MemoryFree(ref rtv);
        } else {
            Program.ActiveScopeBuffer[^1].Remove(alias);
        }

        return true;
    }
    
    /// <summary>
    /// filter for accepting new definitions
    ///     even natives require a target of an object type
    ///     constants cannot be defined if already defined
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="tar"></param>
    /// <returns></returns>
    internal static bool ProvideDefinition(ref AssembleTimeObject ctx, ref AssembleTimeObject tar) {
        if (ctx.constant) {
            if (ctx.defined) {
                // error, cannot redefine
                return false;
            }

            // if ctx is declared lazily-defined constant, target must be undefined unless runtime located
            if (tar.defined) {
                var rt_ptr = tar.GetMember("rt_ptr");
                if (rt_ptr is null) {
                    // target is not a runtime member but is defined, we forbid this code practice
                    return false;
                }

                // never declared as a const int with runtime pointer component
                if (ctx.type is AssembleTimeTypes.UNDEFINED) {
                    ctx = new(tar);         // this is our definition for this member
                } else if (ctx is {type: AssembleTimeTypes.INT, constant: true, defined: false}){
                    
                }
            } else {
                ctx = new(tar);
            }
        }
        
        
        if (ctx is {constant: true, defined: true}) {
            // error, cannot redefine
            return false;
        }

        if (tar.type != ctx.type) {
            // error, type mismatch
            return false;
        }

        ctx = new(tar);                 // weak clone allocation
        return true;
    }
}