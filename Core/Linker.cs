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
        
        // convert parse into Format
        if (Parse.Any(e => e.Key.StartsWith("rules.") && e.Key.Count(c => c == '.') > 1)) {
            // tables found inside the rules, forbidden
            return;
        }

        // check top level domains
        foreach (var (key, value) in Parse.Where(e => e.Key.All(c => c is not '.'))) {
            switch (key) {
                case "static":
                case "dynamic":
                    if (value is not TomlTable) {
                        // error : static must be a TomlTable
                        return;
                    }
                    
                    break;
                
                case "rules":
                    if (value is not TomlArray) {
                        // error : static must be a TomlTable
                        return;
                    }
                    
                    break;
                
                default:
                    // erroneous top level
                    return;
            }
        }
        
        // segments
        var Pairing = new Dictionary<string, Segment>();
        foreach (var (key, value) in Order(Parse).Where(e => e.Key.Count(c => c == '.') > 0 
                                                          && e.Value is TomlTable
                                                          && !e.Key.StartsWith("rules."))) {
            var     body = key.TrimStart('.'); 

            // top level segments
            if (body.Count(c => c == '.') is 1) {
                switch (key.Trim('.')) {
                    case "static":
                        if (value is TomlTable StaticTable) {
                            var resp = AddStaticTopLevel(ref StaticTable);
                            if (resp is null) {
                                // error pass back
                                return;
                            }
                            
                            // add top level segment
                            Static.Add(key.TrimStart('.'), resp);
                            Pairing.Add(body, resp);
                        } else {
                            // error, member of static was not a table
                            return;
                        }
                        
                        continue;
                    
                    case "dynamic":
                        if (value is TomlTable DynamicTable) {
                            var resp = AddSegment(ref DynamicTable);
                            if (resp is null) {
                                // error pass back
                                return;
                            }
                            
                            // add top level segment
                            Pairing.Add(body, resp);
                            Dynamic.Add(key.TrimStart('.'), resp);
                        } else {
                            // error, member of static was not a table
                            return;
                        }
                        
                        continue;
                    
                    #if DEBUG
                    default:
                        // should be impossible, but if we attempt to subscribe an unpermitted top level domain, error
                        return;
                    #endif
                }
            }

            var activeSegment = key.Trim('.') switch {
                "static"  => Static.GetValueOrDefault(body.Trim('.')),
                "dynamic" => Dynamic.GetValueOrDefault(body.Trim('.')),
                #if DEBUG
                _ => throw new Exception($"Unknown segment: {key}"),
                #else
                _ => null,
                #endif
            };
            
            #if RELEASE
            if  (activeSegment is null) {
                // fatal error : report on github
                return;
            }
            #endif

            //activeSegment = activeSegment!;

            var bones = body.Split('.');
            
            foreach (var boneName in bones.Skip(1).SkipLast(1)) {
                // we can make this completely null safe because we ordered it such that we create parents first always
                // invalid parents are destroyed and the process is quit before attempt to subscribe chidlren
                activeSegment = activeSegment!.Segments.GetValueOrDefault(boneName)!;
            }

            if (value is TomlTable Table) {
                var resp = AddSegment(ref Table);
                if (resp is null) {
                    return;
                }

                Pairing.Add(body, resp);
                activeSegment!.Segments.Add(bones[^1], resp);
            }
            #if DEBUG
            else {
                // fatal error
                throw new Exception($"Unknown segment: {body}");
            }
            #endif
        }
        
        foreach (var (_, seg) in Static) {
            // set AO to 0 per top level segment (each top level is its own memory location, physically different)
            var activeOffset = 0;
            if (!seg.Split(ref activeOffset, null)) {
                Rules = [];
                return;
            }
        }
        
        foreach (var (_, seg) in Dynamic) {
            // set AO to 0 per top level segment (each top level is its own memory location, physically different)
            var activeOffset = 0;
            if (!seg.Split(ref activeOffset, null)) {
                Rules = [];
                return;
            }
        }

        if (Parse["rules"] is TomlTable ParsedRules) {
            foreach (var (key, rule) in ParsedRules) {
                Rules.Add(key, []);
                foreach (var path in from seg in (TomlArray)rule select ((string)seg!).Split('.')) {
                    Segment? activeSegment;
                    switch (path[0]) {
                        case "static":
                            activeSegment = Static.GetValueOrDefault(path[1]);
                            if (activeSegment is null) {
                                // error, rule specified non-existent segment
                                Rules = [];
                                return;
                            }
                            break;
                    
                        case "dynamic":
                            activeSegment = Dynamic.GetValueOrDefault(path[1]);
                            if (activeSegment is null) {
                                // error, rule specified non-existent segment
                                Rules = [];
                                return;
                            }
                            break;
                    
                        default:
                            // error
                            Rules = [];
                            return;
                    }
                    
                    foreach (var p in path.Skip(2)) {
                        activeSegment = activeSegment.Segments.GetValueOrDefault(p);
                        if (activeSegment is null) {
                            // error, rule specified non-existent segment
                            Rules = [];
                            return;
                        }
                    }
                
                    Rules[key].Add(activeSegment);
                }
            }
        } else {
            // error : rules isn't a table of rule arrays
            Rules = [];
        }

        return;
        
        IOrderedEnumerable<KeyValuePair<string,object>> Order(TomlTable t) => t.OrderBy(t => t.Key.Split('.').Length).ThenBy(t => t);

        StaticTopLevel? AddStaticTopLevel(ref TomlTable Table) {
            (int? Offset, int? Width, int? Address) = (null, null, null);
            foreach (var (key, value) in Table) {
                switch (key) {
                    case "address": if (value is int address) { Address = address;} else address = -1; break;
                    case "offset":  if (value is int offset)  { Offset = offset;}   else offset  = -1; break;
                    case "width":   if (value is int width)   { Width = width;}     else width   = -1; break;
                    
                    default:
                        // error, member should not exist
                        return null;
                }
            }

            if (Offset is null || Width is null || Address is null) {
                // error, incomplete construction
                return null;
            }
            
            return new StaticTopLevel((int)Offset, (int)Width, (int)Address);
        }
        
        Segment? AddSegment(ref TomlTable Table) {
            (int? Offset, int? Width, int? Address) = (null, null, null);
            foreach (var (key, value) in Table) {
                switch (key) {
                    case "offset":  if (value is int offset)  { Offset  = offset;}   else offset = -1; break;
                    case "width":   if (value is int width)   { Width   = width;}     else width = -1; break;
                    
                    default:
                        // error, member should not exist
                        return null;
                }
            }

            if (Offset is null || Width is null || Address is null) {
                // error, incomplete construction
                return null;
            }
            
            return new Segment((int)Offset, (int)Width);
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
        internal StaticTopLevel(int Offset, int Width, int Address) : base (Offset, Width) {
            adress = Address;
        }
        public int adress { get; set; }
    }
        

    public class Segment {

        internal Segment(int Offset, int Width) {
            offset = Offset; width = Width;
        }
        
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
        
        public int                         offset   { get; set; }
        public int                         width    { get; set; }
        
        // parameterised by core.Linker, not by toml parser
        private  int                         globalOffset;
        private  List<Slice>                 Slices = [];
        internal Dictionary<string, Segment> Segments { get; set; } = [];
    }

    public Dictionary<string, StaticTopLevel> Static  { get; set; } = [];
    public Dictionary<string, Segment>        Dynamic { get; set; } = [];

    internal readonly Dictionary<string, List<Segment>> Rules = [];
}