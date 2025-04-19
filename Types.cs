using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace Tataru {
    namespace Types {
        public enum EvaluationLevel {
            OK,     // ready for evaluation
            WAIT    // needs more information
        }

        public struct Symbol {
            public EvaluationLevel Level;
            public string Context;
        }

        public struct Label {
            public EvaluationLevel Level;
            public RuntimeTypeHandle Type;
            public Union Context;
        }

        public unsafe class Union {
            private object? _value;

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public string? String() {
                return (string?)_value;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public int? Int() {
                return (int?)_value;
            }

            public Union(object? value) {
                _value = value;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public T? Get<T>() {
                return (T?)_value;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public void Set(object value) {
                _value = value;
            }
        }

        struct InterruptableMultiline {
            public int Index;
            public string[] Lines;
        }

        public enum EXIT_CODES {
            OK,
            INVALID_ARGUMENT,
            NO_PARAMETER,
            SYNTAX_ERROR,
            NOTHING_TO_DO
        }

        public struct StatusResponse<T> {
            public EXIT_CODES Status;
            public T? Response;

            public StatusResponse(EXIT_CODES status, T? response) => (Status, Response) = (status, response);
        }
    }
}
