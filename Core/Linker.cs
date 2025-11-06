using Microsoft.CodeAnalysis.Scripting;

namespace uhla.Core;

using Tomlyn;

internal class Linker {
    internal Linker(string fp) {
        
        var Script = Toml.ToModel<Format>(
            File.ReadAllText(fp),
            null,
            new TomlModelOptions { ConvertPropertyName = name => name }
        );

        var ActiveOffset = 0;
        foreach (var (_, seg) in Script.Segments) {
            if (!seg.Split(ref ActiveOffset, null)) {
                Script = null;
                return;
            }
        }
        return;
    }

    /// <summary>
    /// Performs 'linking'
    /// </summary>
    /// <returns>success</returns>
    internal static bool Link() {
        // uses Program.Objects
        // writes to Program.FileOut (or something)
        return false;
    }

    internal class Format {
        public class Memory {
            public int offset { get; set; }
            public int width  { get; set; }
            public int write  { get; set; }
        }

        public class Segment {
            internal class Slice {
                internal Slice(int Offset, int Width) {
                    offset = Offset;
                    width = Width;
                }
                
                internal int offset;
                internal int width;
            }

            /// <summary>
            /// All the parent needs to do is contribute is their final slice, as the child provides to the parent.
            /// The child knows how to provide to the parent because its entirely to do with localOffset.
            /// localOffset is gradually increased by the offset and width of the segment.
            /// Altogether this makes this a simple process.
            /// 
            /// </summary>
            /// <param name="activeOffset">An 'incrementation' exploring global offsets.</param>
            /// <param name="parent"></param>
            /// <returns></returns>
            internal bool Split(ref int activeOffset, Segment? parent) {
                globalOffset += activeOffset;

                Segments = Segments.OrderBy(s => s.Value.offset).ToDictionary();
                
                foreach (var (_, seg) in Segments) {
                    var result = seg.Split(ref activeOffset,    this);
                    if (!result) {
                        // it means a child process has failed, return with null (error escaping)
                        return false;
                    }
                }

                // ensure that local (child) offset can reside in parent
                if (parent is not null && parent.globalOffset + offset > parent.globalOffset + width) {
                    // error, offset begins outside of parent's containment
                    return false;
                }
                
                // add parent (negative space)
                if (offset is not 0 && parent is not null) {
                    parent.Slices.Add(new Slice(
                        activeOffset,
                        parent.globalOffset + offset - activeOffset
                    ));
                    
                    activeOffset += parent.globalOffset + offset - activeOffset;
                }

                if (parent is not null && parent.globalOffset + offset < activeOffset) {
                    // error, we cannot parse this a previous segment is too large
                    return false;
                }

                // check if we can fit within parent (active node)
                if (parent is not null && activeOffset + width > parent.globalOffset + parent.width) {
                    // error, this node exceeds the size of the parent
                    return false;
                }
                
                // add self (positive space)
                Slices.Add(new Slice(activeOffset, width));
                activeOffset += width;

                // add trail negative space (active token)
                if (parent is not null && activeOffset != parent.globalOffset + width) {
                    parent.Slices.Add(new Slice(
                        activeOffset,
                        parent.offset + parent.width - activeOffset
                    ));
                    
                    activeOffset += parent.offset + parent.width - activeOffset;
                }

                return true;
            }
            
            public int                         offset   { get; set; }
            public int                         width    { get; set; }
            public Dictionary<string, Segment> Segments { get; set; } = [];

            // parameterised by core.Linker, not by toml parser
            internal int         globalOffset;
            internal List<Slice> Slices = [];
        }

        public Dictionary<string, Segment>       Segments { get; set; } = [];
        public Dictionary<string, Memory>        Regions  { get; set; } = [];
        public Dictionary<string, List<Segment>> Rules    { get; set; } = [];
    }
}