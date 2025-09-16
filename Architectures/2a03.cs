using UHLA.Engine;
using UHLA.InterfaceProtocol;

namespace Architectures {
    internal class Ricoh_2a03 : NMOS_6502, IArchitecture {
        public override bool    IsMnemonic(string mnemonic) {
            throw new NotImplementedException();
        }
        public override object? GatherAdditionalMnemonicContext() {
            throw new NotImplementedException();
        }
        public override int? TryCompleteInstruction(string mnemonic, ref List<EvalToken> args) {
            throw new NotImplementedException();
        }
        public override void Initalize() {
            base.Initalize();
            throw new NotImplementedException();
        }

        public override CheckDirectiveStatus CheckDirective(ref List<EvalToken>                                     args,
                                            ref List<(string token, int StringIndex, int StringLength)> DefineResolveBuffer,
                                            ref int                                                 pStringIndex, ref int pTokenIndex,
                                            ref (string ctx, int StringIndex, int StringLength)     pActiveToken,
                                            ref string                                              pRepresentation) {
            
            // check 2a03 directives: mapper setup, memory related 
            
            throw new NotImplementedException();
            return base.CheckDirective(ref args, ref DefineResolveBuffer, ref pStringIndex, ref pTokenIndex, ref pActiveToken, ref pRepresentation);
        }
    }
}