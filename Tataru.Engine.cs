namespace Tataru {
    namespace Engine {
        namespace Types {
            internal struct Macro {         // basically an incredibly strong string, but its callable. We need to preserve this information.
                string[] Code;
                RuntimeTypeHandle Type;
            }

            internal struct Label {
                RuntimeTypeHandle Type;
                object Value;
            }

            internal enum ExitConditions { 
                OK, ERROR, CONTINUE
            }
        }
    }
}
