using HonkSharp.Fluency;
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

        var ETLs = Parse.Where(e => !new List<string> {"dynamic", "static", "rules"}.Contains(e.Key)).ToArray();
        // check top level domains
        if (ETLs.Length > 0) {
            Terminal.Error(new Terminal.ErrorContext {
                ErrorLevel      = ErrorLevels.ERROR,
                ErrorType       = ErrorTypes.LinkerError,
                DecodingPhase   = DecodingPhases.LINKER_INIT,
                Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.ErroneousTopLevelSegment)]([ETLs[0].Key]),
                LineNumber      = -1,
                StepNumber      = 0,
                ContextFileName = fp,
                Context = () => string.Empty
            });
            // error, erroneous top level segments
            return;
        }
        
        if (Parse["dynamic"] is TomlTable DynamicSegments) foreach (var (key, value) in DynamicSegments) {
            if (value is TomlTable segToml) {
                IDictionary<string, object> segDict = segToml.ToDictionary();
                var                      (ctx, ParseErr) = ParseSegmentToml(ref segDict);

                if (ParseErr is not null) {
                    Terminal.Error(ParseErr.Value.ctx($"{key}.{ParseErr.Value.dbgname}"[..^1]));
                    return;
                }
                
                Dynamic.Add(key, ctx!);

                var globalOffset = 0L;
                var err = ctx!.Split(ref globalOffset, null, 0);
                if (err is not null) {
                    Terminal.Error(err.Value.ctx(fp, $"{key}.{err.Value.dbgname}"[..^1]));
                    return;
                }
            } else {
                Terminal.Error(new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonTableSegment)]([key, "dynamic"]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context = () => string.Empty
                });
                return;
            }
        } else {
            Terminal.Error(new Terminal.ErrorContext {
                ErrorLevel      = ErrorLevels.ERROR,
                ErrorType       = ErrorTypes.LinkerError,
                DecodingPhase   = DecodingPhases.LINKER_INIT,
                Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonTableTopLevel)]([]),
                LineNumber      = -1,
                StepNumber      = 0,
                ContextFileName = fp,
                Context         = () => string.Empty
            });
            return;
        }
        
        if (Parse["static"] is TomlTable StaticSegments) foreach (var (key, value) in StaticSegments) {
            if (value is TomlTable segToml) {
                IDictionary<string, object> segDict = segToml.ToDictionary();
                var                      (ctx, ParseErr) = ParseStaticSegmentTopLevel(ref segDict);

                if (ParseErr is not null) {
                    Terminal.Error(ParseErr.Value.ctx($"{key}.{ParseErr.Value.dbgname}"[..^1]));
                    return;
                }
                
                Static.Add(key, ctx!);
                
                var globalOffset = 0L;
                var err = ctx!.Split(ref globalOffset, null, 0);
                if (err is not null) {
                    Terminal.Error(err.Value.ctx(fp, $"{key}.{err.Value.dbgname}"[..^1]));
                    return;
                }
            } else {
                Terminal.Error(new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonTableSegment)]([key, "static"]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context = () => string.Empty
                });
                return;
            }
        } else {
            Terminal.Error(new Terminal.ErrorContext {
                ErrorLevel      = ErrorLevels.ERROR,
                ErrorType       = ErrorTypes.LinkerError,
                DecodingPhase   = DecodingPhases.LINKER_INIT,
                Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonTableTopLevel)]([]),
                LineNumber      = -1,
                StepNumber      = 0,
                ContextFileName = fp,
                Context         = () => string.Empty
            });
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

                        if (bone is null) {
                            Terminal.Error(new Terminal.ErrorContext {
                                ErrorLevel      = ErrorLevels.ERROR,
                                ErrorType       = ErrorTypes.LinkerError,
                                DecodingPhase   = DecodingPhases.LINKER_INIT,
                                Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegment)]([key, seg]),
                                LineNumber      = -1,
                                StepNumber      = 0,
                                ContextFileName = fp,
                                Context = () => string.Empty
                            });
                        }

                        foreach (var nbone in bones.Skip(2)) {
                            if (bone is null) {
                                Terminal.Error(new Terminal.ErrorContext {
                                    ErrorLevel      = ErrorLevels.ERROR,
                                    ErrorType       = ErrorTypes.LinkerError,
                                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegment)]([key, seg]),
                                    LineNumber      = -1,
                                    StepNumber      = 0,
                                    ContextFileName = fp,
                                    Context = () => string.Empty
                                });
                                return;
                            }

                            bone = bone.Segments.GetValueOrDefault(nbone);
                        }

                        if (bone is null) {
                            Terminal.Error(new Terminal.ErrorContext {
                                ErrorLevel      = ErrorLevels.ERROR,
                                ErrorType       = ErrorTypes.LinkerError,
                                DecodingPhase   = DecodingPhases.LINKER_INIT,
                                Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegment)]([key, seg]),
                                LineNumber      = -1,
                                StepNumber      = 0,
                                ContextFileName = fp,
                                Context = () => string.Empty
                            });
                            return;
                        }
                        
                        Rule.Add(bone);

                    } else {
                        Terminal.Error(new Terminal.ErrorContext {
                            ErrorLevel      = ErrorLevels.ERROR,
                            ErrorType       = ErrorTypes.LinkerError,
                            DecodingPhase   = DecodingPhases.LINKER_INIT,
                            Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegment)]([key, seg]),
                            LineNumber      = -1,
                            StepNumber      = 0,
                            ContextFileName = fp,
                            Context = () => string.Empty
                        });
                        return;
                    }
                }
            } else {
                Terminal.Error(new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonArrayRule)]([key]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context = () => string.Empty
                });
                return;
            }
            Rules.Add(key, Rule);
        }


        // error, rules isn't a TomlTable invalid type
        return;

        (StaticTopLevel? ctx, (Func<string, Terminal.ErrorContext> ctx, string dbgname)? err) ParseStaticSegmentTopLevel(ref IDictionary<string, object> Table) {
            var (ctx, err) = __ParseSegmentToml(ref Table);

            if (err is not null) {
                // error pass back
                return (null, err);
            }

            if (Table.ContainsKey("address") && Table["address"] is long address) {
                return (new StaticTopLevel(ctx!.Value.offset, ctx.Value.width, address) {
                    Segments = ctx.Value.segments!
                }, null);
            }

            // error, seg address is not integer or does not exist
            return (null, (name => new Terminal.ErrorContext {
                ErrorLevel      = ErrorLevels.ERROR,
                ErrorType       = ErrorTypes.LinkerError,
                DecodingPhase   = DecodingPhases.LINKER_INIT,
                Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegmentMember)]([name, "address"]),
                LineNumber      = -1,
                StepNumber      = 0,
                ContextFileName = fp,
                Context         = () => string.Empty
            }, string.Empty));
        }

        ((long offset, long width, Dictionary<string, Segment>? segments)? ctx, (Func<string, Terminal.ErrorContext> ctx, string dbgname)? err) __ParseSegmentToml(ref IDictionary<string, object> Table) {
            var (offset, width, segments) = (0l, 0l, new Dictionary<string, Segment>());
            if (Table.ContainsKey("offset") && Table["offset"] is long _offset) {
                offset = _offset;
            } else {
                return (null, (name => new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegmentMember)]([name, "offset"]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context         = () => string.Empty
                }, string.Empty));
            }
            
            if (Table.ContainsKey("width") && Table["width"] is long _width) {
                width = _width;
            } else {
                return (null, (name => new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegmentMember)]([name, "width"]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context         = () => string.Empty
                }, string.Empty));
            }

            foreach (var (name, seg) in Table.Where(e => !new[] {"offset", "width", "address"}.Contains(e.Key))) {
                if (seg is TomlTable SegTable) {
                    IDictionary<string, object>                                           SegDict = SegTable.ToDictionary();
                    var child   = ParseSegmentToml(ref SegDict);
                    if (child.err is not null) {
                        // error, child had issue - report back
                        return (null, (child.err.Value.ctx, $"{name}.{child.err.Value.dbgname}"));
                    }
                    segments.Add(name, child.ctx!);    
                } else {
                    return (null, (child => new Terminal.ErrorContext {
                        ErrorLevel      = ErrorLevels.ERROR,
                        ErrorType       = ErrorTypes.LinkerError,
                        DecodingPhase   = DecodingPhases.LINKER_INIT,
                        Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonTableSegment)]([name, child]),
                        LineNumber      = -1,
                        StepNumber      = 0,
                        ContextFileName = fp,
                        Context         = () => string.Empty
                    }, name));
                }
            }
           
            return ((offset, width, segments), null);
        }
        
        (Segment? ctx, (Func<string, Terminal.ErrorContext> ctx, string dbgname)? err) ParseSegmentToml(ref IDictionary<string, object> Table) {
            var ctx = new Segment(Offset: 0, Width: 0);
            if (Table.ContainsKey("offset") && Table["offset"] is long offset) {
                ctx.offset = offset;
            } else {
                return (null, (name => new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegmentMember)]([name, "offset"]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context         = () => string.Empty
                }, string.Empty));
            }
            
            if (Table.ContainsKey("width") && Table["width"] is long width) {
                ctx.width = width;
            } else {
                return (null, (name => new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.MissingSegmentMember)]([name, "offset"]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context         = () => string.Empty
                }, string.Empty));
            }

            foreach (var (name, seg) in Table.Where(e => !new[] {"offset", "width", "address"}.Contains(e.Key))) {
                if (seg is TomlTable SegTable) {
                    IDictionary<string, object>                                           SegDict = SegTable.ToDictionary();
                    var child   = ParseSegmentToml(ref SegDict);
                    if (child.err is not null) {
                        // error, child had issue - report back
                        return (null, (child.err.Value.ctx, $"{name}.{child.err.Value.dbgname}"));
                    }
                    ctx.Segments.Add(name, child.ctx!);    
                } else {
                    return (null, (child => new Terminal.ErrorContext {
                        ErrorLevel      = ErrorLevels.ERROR,
                        ErrorType       = ErrorTypes.LinkerError,
                        DecodingPhase   = DecodingPhases.LINKER_INIT,
                        Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.NonTableSegment)]([name, child]),
                        LineNumber      = -1,
                        StepNumber      = 0,
                        ContextFileName = fp,
                        Context         = () => string.Empty
                    }, name));
                }
            }
           
            return (ctx, null);
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
        internal (Func<string, string, Terminal.ErrorContext> ctx, string dbgname)? Split(ref long activeOffset, Segment? parent, int nchild) {
            globalOffset += activeOffset;
            Segments = Segments.OrderBy(s => s.Value.offset).ToDictionary();
            
            foreach (var seg in Segments.Enumerate()) {
                var err = seg.Element.Value.Split(ref activeOffset, this, seg.Index);
                if (err is not null) {
                    // it means a child process has failed, return with null (error escaping)
                    return (err.Value.ctx, $"{seg.Element.Key}.{err.Value.dbgname}");
                }
            }

            // ensure that local (child) offset can reside in parent
            if (parent is not null && offset > parent.width) {
                // error, offset begins outside of parent's containment
                return ((fp, name) => new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.SegmentOffsetTooHigh)]([name, offset, offset - parent.width]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context = () => string.Empty
                }, string.Empty);
            }
            
            // add parent (negative space)
            var offsetDelta = (parent?.globalOffset ?? 0) + offset - activeOffset;

            switch (offsetDelta) {
                case < 0 when nchild > 0:
                    // error offset has been used
                    return ((fp, name) => new Terminal.ErrorContext {
                        ErrorLevel      = ErrorLevels.ERROR,
                        ErrorType       = ErrorTypes.LinkerError,
                        DecodingPhase   = DecodingPhases.LINKER_INIT,
                        Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.SegmentOffsetUnusable)]([name, offset, Math.Abs(offsetDelta)]),
                        LineNumber      = -1,
                        StepNumber      = 0,
                        ContextFileName = fp,
                        Context = () => string.Empty
                    }, string.Empty);
                
                case > 0 when parent is not null:
                    // leading data
                    parent.Slices.Add(new Slice(
                        activeOffset,
                        offsetDelta
                    ));
                
                    activeOffset += offsetDelta;
                    break;
            }

            // check if we can fit within parent (active node)
            if (parent is not null && activeOffset + width > parent.globalOffset + parent.width) {
                // error, this node exceeds the size of the parent
                var activeOffsetDereference = activeOffset;
                return ((fp, name) => new Terminal.ErrorContext {
                    ErrorLevel      = ErrorLevels.ERROR,
                    ErrorType       = ErrorTypes.LinkerError,
                    DecodingPhase   = DecodingPhases.LINKER_INIT,
                    Message         = Language.Language.Errors[(Program.ActiveLanguage, ErrorNames.SegmentTooLarge)]([name, width, activeOffsetDereference + width - parent.globalOffset + parent.width]),
                    LineNumber      = -1,
                    StepNumber      = 0,
                    ContextFileName = fp,
                    Context = () => string.Empty
                }, string.Empty);
            }
            
            // add self (positive space - leaf node)
            if (Segments.Count is 0) {
                Slices.Add(new Slice(activeOffset, width));
                activeOffset += width;
            }

            // add trail negative space (parent node)
            if (parent is not null && this == parent.Segments.ElementAt(^1).Value && activeOffset != parent.globalOffset + parent.width) {
                parent.Slices.Add(new Slice(
                    activeOffset,
                    parent.globalOffset + parent.width - activeOffset
                ));

                activeOffset =  parent.globalOffset + parent.width;
            }

            return null;
        }
        
        public  long                        offset { get; set; }
        public  long                        width  { get; set; }
        private long                        globalOffset;
        public  List<Slice>                 Slices = [];
        public  Dictionary<string, Segment> Segments { get; set; } = [];
    }

    public Dictionary<string, StaticTopLevel> Static  { get; set; } = [];
    public Dictionary<string, Segment>        Dynamic { get; set; } = [];

    internal readonly Dictionary<string, List<Segment>> Rules = [];
}