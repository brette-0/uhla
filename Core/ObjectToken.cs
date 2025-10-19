namespace uhla.Core;

internal class ObjectToken {
    [Flags]
    internal enum ObjectTokenFlags : byte {
        defined  = 0x01,
        constant = 0x02
    }
            
    internal ObjectToken (object pData, AssembleTimeTypes pType, bool pDefined = true, bool pConstant = false) {
        data     = pData;
        type     = pType;
        defined  = pDefined;
        constant = pConstant;
    }
            
    /// <summary>
    /// Cloning Constructor.
    /// </summary>
    /// <param name="pOT">Token to be cloned</param>
    internal ObjectToken(ObjectToken pOT) {
        data     = Core.Clone((Dictionary<string, ObjectToken>)pOT.data);    // recursively clone contents
        type     = pOT.type > type ? pOT.type : type;
        defined  = pOT.defined;
        constant = pOT.constant;

        if (!defined) {
            var members = (Dictionary<string, ObjectToken>)data;
            #if DEBUG
            if (!members.Remove("#dependants")) {
                // error, should have the "dependants" key if undefined
                throw new Exception("error, should have dependants key if undefined");
            }
            #else
                    members.Remove("#dependants");
            #endif
                    
            members = (Dictionary<string, ObjectToken>)pOT.data;
            members.Add("this", this);  // entirely broken!! TODO: immediate fix required!
        }
    }

    /// <summary>
    /// 'undefined' constructor.
    /// </summary>
    /// <param name="ctx"></param>
    /// <param name="pType"></param>
    /// <param name="pLevel"></param>
    internal ObjectToken((List<HierarchyTokens_t>? Tokens, int MaxHierarchy, string Representation) ctx, AssembleTimeTypes pType = AssembleTimeTypes.UNDEFINED) {
        data = new Dictionary<string, ObjectToken>{
            {"#self",       new ObjectToken(ctx,                     AssembleTimeTypes.UNDEFINED)},
            {"#dependants", new ObjectToken(new List<ObjectToken>(), AssembleTimeTypes.UNDEFINED)}
        };
        type     = pType;
        defined  = false;
        constant = false;
    }

    internal ObjectToken? GetMember(string name) => ((Dictionary<string, ObjectToken>)data).GetValueOrDefault(name);
            
    internal bool GetMember(string name, out ObjectToken? ctx) {
        var obj = (Dictionary<string, ObjectToken>)data;
        ctx = obj.GetValueOrDefault(name);
                
        return ctx is not null;
    }

    internal bool OnLazyDefinitionComplete(string dependency) {
        #if DEBUG
        if (defined) {
            // error, if defined, there are no dependencies
            return false;
        }
        #endif

        #if DEBUG
        if (!GetMember("#dependencies", out var dependenciesObjectToken)) {
            // error, if undefined should have dependencies
            return false;
        }

        var dependencies = (List<string>)dependenciesObjectToken!.data;
        if (!dependencies.Remove(dependency)) {
            // error, member not found
            return false;
        }
                
        #elif RELEASE
                var dependencies = (List<string>)GetMember("#dependencies")!.data;
                dependencies.Remove(dependency);
        #endif

        if (dependencies.Count == 0) {
            // TODO: make inline
            AttemptDefinitionResolve();
        }

        return true;
    }

    /// <summary>
    /// TODO: implement
    ///
    /// On complete lazy definition, dependants attached to that lazily defined constant will invoke
    /// AttemptDefinitionResolve 
    /// 
    /// </summary>
    /// <returns>success</returns>
    internal bool AttemptDefinitionResolve() {
        if (defined) {
            // error, already defined
        }

        #if DEBUG
        if (type is AssembleTimeTypes.UNDEFINED) {
            // error, should not attempt to resolve the definition of something improperly declared
        }
        #endif
                
        // collect value => Database.Providedefinition
        return true;
    }

    internal bool constant {
        get => flags.HasFlag(ObjectTokenFlags.constant);
        set => flags |= value ? ObjectTokenFlags.constant : 0;
    }

    internal bool defined {
        get => flags.HasFlag(ObjectTokenFlags.defined);
        set => flags |= value ? ObjectTokenFlags.defined : 0;
    }

    private  ObjectTokenFlags  flags;
    internal object            data; // contains members
    internal AssembleTimeTypes type; // type of object
}