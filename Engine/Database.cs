namespace uhla.Engine;

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
    internal static ObjectToken? GetObjectFromAlias(string Alias) {
        List<Dictionary<string, ObjectToken>> LocalObjectSearchBuffer = [Program.ActiveScopeBuffer[^1], .. Program.ObjectSearchBuffer];
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
    internal static ObjectToken? GetObjectFromAlias(string Alias, Dictionary<string, ObjectToken> TargetScope) => __GetObjectFromAlias(Alias, [TargetScope]);
    
    /// <summary>
    /// Internal function iterating over the LocalObjectSearchPath to find the required context if possible.
    /// </summary>
    /// <param name="Alias"></param>
    /// <param name="LocalObjectSearchBuffer"></param>
    /// <returns></returns>
    private  static ObjectToken? __GetObjectFromAlias(string Alias, List<Dictionary<string, ObjectToken>> LocalObjectSearchBuffer) {
        ObjectToken ctx;
        var (found, error) = (false, false);
        foreach (var LocalObjectSearchContainer in LocalObjectSearchBuffer) {
            if (LocalObjectSearchContainer.TryGetValue(Alias, out ctx)) {
                return null;
            }
        }

        return default;
    }


    /// <summary>
    /// Declare member responds to code like 'u8 foo' or 'int bar' or 'const string ash'
    /// it creates an ObjectToken for the member with total emptiness on the stack
    ///     definition will provide it the value it needs
    /// </summary>
    /// <param name="alias">name of the object token</param>
    /// <param name="type">the type of the object token</param>
    /// <param name="constant">its 'constant-ness'</param>
    /// <returns></returns>
    internal static bool DeclareScriptingOrReadOnlyMember(string alias, AssembleTimeTypes type, bool constant = false) {
        if (Program.ActiveScopeBuffer[^1].ContainsKey(alias)) {
            // error, already declared member
            return false;
        }
        
        Program.ActiveScopeBuffer[^1].Add(alias, new ObjectToken(
            new Dictionary<string, ObjectToken>(),
            type,
            false,
            constant
        ));
        
        return true;
    }

    internal static bool DeclareRuntimeVariable(string alias, RunTimeVariableType rtv) {
        if (Program.ActiveScopeBuffer[^1].ContainsKey(alias)) {
            // error, already declared member
            return false;
        }

        if (!Program.Architecture.MemoryReserve(ref rtv)) {
            // pass back error
            return false;
        }
        
        Program.ActiveScopeBuffer[^1].Add(alias, new ObjectToken(
            new Dictionary<string, ObjectToken>() {
                {"#self", new ObjectToken(rtv, default, default)}
            },
            AssembleTimeTypes.RT,
            false,
            false
        ));
        
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
    internal static bool ProvideDefinition(ref ObjectToken ctx, ref ObjectToken tar) {
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