using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;

namespace Tataru {
    namespace Types {
        internal enum EvaluationLevel {
            OK,     // ready for evaluation
            WAIT    // needs more information
        }

        internal class Exp {
            internal string Value;

            internal Exp(string value) {
                Value = value;
            }
        }

        internal enum MemoryTypes {
            Slow, Fast, ZP, MMC, PRG
        }

        internal struct Variable {
            internal int Width, Offset;
            internal bool Endian, Signed, BCD;
            internal MemoryTypes MemoryType;
        }

        internal class Register {
            internal enum Explicit {
                a, x, y
            }

            Explicit Content;

            internal Register (Explicit __Content__) {
                Content = __Content__;
            }
        }

        internal class Flag {
            internal enum Explicit {
                z, c, n, v
            }

            Explicit Content;

            internal Flag(Explicit __Content__) {
                Content = __Content__;
            }
        }

        internal struct Macro {
            internal InterruptableMultiline AssemblyTreeReference;
            internal int LineNumber;
            internal RuntimeTypeHandle ReturnType;
            internal RuntimeTypeHandle[] ParameterTypes;
        }

        internal class Box<T> {
            internal T Value;
            internal Box(T value)     {
                Value = value;
            }
        }

        internal struct Symbol {
            internal EvaluationLevel Level;
            internal string Context;
        }

        internal struct Label {
            internal EvaluationLevel Level;
            internal RuntimeTypeHandle Type;
            internal Union Context;
        }

        internal unsafe class Union {
            private object? _value;

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal string? String() {
                return (string?)_value;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal int? Int() {
                return (int?)_value;
            }

            internal Union(object? value) {
                _value = value;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal T? Get<T>() {
                return (T?)_value;
            }

            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            internal void Set(object value) {
                _value = value;
            }
        }

        internal struct InterruptableMultiline {
            public int Index;
            public Box<string[]> LinesBox;
        }

        internal enum EXIT_CODES {
            OK,
            INVALID_ARGUMENT,
            NO_PARAMETER,
            SYNTAX_ERROR,
            NOTHING_TO_DO
        }

        internal struct StatusResponse<T> {
            internal EXIT_CODES Status;
            internal T? Response;

            internal StatusResponse(EXIT_CODES status, T? response) => (Status, Response) = (status, response);
        }
    }
}
