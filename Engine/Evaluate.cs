namespace Numinous.Engine {
    internal struct HierarchyTokens {
        internal HierarchyTokens(List<List<EvalToken>> pDeltaTokens, int pHierachy, string pRepresentation) {
            DeltaTokens    = pDeltaTokens;
            Hierarchy      = pHierachy;
            Representation = pRepresentation;
        }
    
    
        internal List<List<EvalToken>> DeltaTokens;
        internal int                   Hierarchy;
        internal string                Representation;
    }
}