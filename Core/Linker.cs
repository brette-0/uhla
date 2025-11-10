using Tomlyn.Model;

namespace uhla.Core;

using Tomlyn;

internal class Linker {
    internal Linker(string fp) {
        var Parse = Toml.ToModel(
            File.ReadAllText(fp),
            fp,
            new TomlModelOptions { ConvertPropertyName = name => name.ToLowerInvariant() }
        );

        // check top level domains
        if (Parse.Any(e => !new List<string> {"dynamic", "static", "rules"}.Contains(e.Key))) {
            // error, erroneous top level segments
            return;
        }
        
        if (Parse["dynamic"] is TomlTable DynamicSegments) foreach (var (key, value) in DynamicSegments) {
            if (value is TomlTable segToml) {
                IDictionary<string, object> segDict = segToml.ToDictionary();
                var                         seg     = ParseSegmentToml(ref segDict);

                if (seg is null) {
                    // error in seg, report back
                    return;
                }
                
                Dynamic.Add(key, seg);
            } else {
                // error, dynamic seg is not a Table
                return;
            }
        } else {
            // error, dynamics isn't a TomlTable invalid type
            return;
        }
        
        if (Parse["static"] is TomlTable StaticSegments) foreach (var (key, value) in StaticSegments) {
            if (value is TomlTable segToml) {
                IDictionary<string, object> segDict = segToml.ToDictionary();
                var                         seg     = ParseStaticSegmentTopLevel(ref segDict);

                if (seg is null) {
                    // error in seg, report back
                    return;
                }
                
                Static.Add(key, seg);
            } else {
                // error, static seg is not a Table
                return;
            }
        } else {
            // error, statics isn't a TomlTable invalid type
            return;
        }
        
        List<Segment> Rule    = [];
        if (Parse["rules"] is TomlTable RuleTable) foreach (var (key, value) in RuleTable) {
            if (value is TomlArray ruleArray) {
                foreach (var seg in ruleArray) {
                    if (seg is string RuleString) {
                        // process rule
                        var bones = RuleString.Split('.');
                        
                        var bone = bones[0] switch {
                            "dynamic" => Dynamic.GetValueOrDefault(bones[1]),
                            "static"  => Static. GetValueOrDefault(bones[1]),

                            _ => null,

                        };

                        foreach (var nbone in bones.Skip(2)) {
                            if (bone is null) {
                                // error - segment does not exist
                                return;
                            }

                            bone = bone.Segments.GetValueOrDefault(nbone);
                        }

                        Rule.Add(bone);

                    } else {
                        // error, rule is not a string
                        return;
                    }
                }
            } else {
                // error, rules arent array
                return;
            }
            Rules.Add(key, Rule);
        } else {
            // error, rules isn't a TomlTable invalid type
            return;
        }
        

        return;

        StaticTopLevel? ParseStaticSegmentTopLevel(ref IDictionary<string, object> Table) {
            var ctx = __ParseSegmentToml(ref Table);

            if (ctx is null) {
                // error pass back
                return null;
            } else if (Table.ContainsKey("address") && Table["address"] is long address) {
                return new StaticTopLevel(ctx.Value.offset, ctx.Value.width, address);
            } else {
                // error, seg offset is not integer or does not exist
                return null;
            }
        }

        (long offset, long width, Dictionary<string, Segment>? segments)? __ParseSegmentToml(ref IDictionary<string, object> Table) {
            var (_offset, _width, _segments) = (0l, 0l, new Dictionary<string, Segment>());
            if (Table.ContainsKey("offset") && Table["offset"] is long offset) {
                _offset = offset;
            } else {
                // error, seg offset is not integer or does not exist
                return null;
            }
            
            if (Table.ContainsKey("width") && Table["width"] is long width) {
                _width = width;
            } else {
                // error, seg width is not integer or does not exist
                return null;
            }

            foreach (var (name, seg) in Table.Where(e => !new[] {"offset", "width", "address"}.Contains(e.Key))) {
                if (seg is TomlTable SegTable) {
                    IDictionary<string, object> SegDict = SegTable.ToDictionary();
                    var                         child   = ParseSegmentToml(ref SegDict);
                    if (child is null) {
                        // error, child had issue - report back
                        return null;
                    }
                    _segments.Add(name, child);    
                } else {
                    // error child segment is not a table
                    return null;
                }
            }
           
            return (_offset, _width, _segments);
        }
        
        Segment? ParseSegmentToml(ref IDictionary<string, object> Table) {
            var ctx = new Segment(Offset: 0, Width: 0);
            if (Table.ContainsKey("offset") && Table["offset"] is long offset) {
                ctx.offset = offset;
            } else {
                // error, seg offset is not integer or does not exist
                return null;
            }
            
            if (Table.ContainsKey("width") && Table["width"] is long width) {
                ctx.width = width;
            } else {
                // error, seg width is not integer or does not exist
                return null;
            }

            foreach (var (name, seg) in Table.Where(e => !new[] {"offset", "width", "address"}.Contains(e.Key))) {
                if (seg is TomlTable SegTable) {
                    IDictionary<string, object> SegDict = SegTable.ToDictionary();
                    var                         child   = ParseSegmentToml(ref SegDict);
                    if (child is null) {
                        // error, child had issue - report back
                        return null;
                    }
                    ctx.Segments.Add(name, child);    
                } else {
                    // error child segment is not a table
                    return null;
                }
            }
           
            return ctx;
        }
        
        StaticTopLevel? AddStaticTopLevel(ref TomlTable Table) {
            (long? Offset, long? Width, long? Address) = (null, null, null);
            foreach (var (key, value) in Table) {
                switch (key) {
                    case "address": if (value is long address) { Address = address;} else address = -1; break;
                    case "offset":  if (value is long offset)  { Offset  = offset;}   else offset = -1; break;
                    case "width":   if (value is long width)   { Width   = width;}     else width = -1; break;
                    
                    default:
                        // error, member should not exist
                        return null;
                }
            }

            if (Offset is null || Width is null || Address is null) {
                // error, incomplete construction
                return null;
            }
            
            return new StaticTopLevel((long)Offset, (long)Width, (long)Address);
        }
        
        Segment? AddSegment(ref TomlTable Table) {
            (long? Offset, long? Width, long? Address) = (null, null, null);
            foreach (var (key, value) in Table) {
                switch (key) {
                    case "offset": if (value is long offset)  { Offset = offset;}    else offset = -1; break;
                    case "width":  if (value is long width)   { Width  = width;}     else width = -1; break;
                    
                    default:
                        // error, member should not exist
                        return null;
                }
            }

            if (Offset is null || Width is null || Address is null) {
                // error, incomplete construction
                return null;
            }
            
            return new Segment((long)Offset, (long)Width);
        }
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

    public class StaticTopLevel : Segment {
        internal StaticTopLevel(long Offset, long Width, long Address) : base (Offset, Width) {
            address = Address;
        }
        public long address { get; set; }
    }
        

    public class Segment {

        internal Segment(long Offset, long Width) {
            offset = Offset; width = Width;
        }
        
        internal class Slice {
            internal Slice(long Offset, long Width) {
                offset = Offset;
                width = Width;
            }
            
            internal long offset;
            internal long width;
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
        internal bool Split(ref long activeOffset, Segment? parent) {
            globalOffset += activeOffset;

            Segments = Segments.OrderBy(s => s.Value.offset).ToDictionary();
            
            foreach (var (n, seg) in Segments) {
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
        
        public  long                        offset { get; set; }
        public  long                        width  { get; set; }
        private long                        globalOffset;
        private List<Slice>                 Slices = [];
        public  Dictionary<string, Segment> Segments { get; set; } = [];
    }

    public Dictionary<string, StaticTopLevel> Static  { get; set; } = [];
    public Dictionary<string, Segment>        Dynamic { get; set; } = [];

    internal readonly Dictionary<string, List<Segment>> Rules = [];
}