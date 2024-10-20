const std = @import("std");
const xml = @import("./xml.zig");
const dtd = @import("./dtd.zig");

pub const Registry = struct {
    allocator: std.mem.Allocator,
    string_arena: std.heap.ArenaAllocator,
    registry_content: []const u8,
    enumgroups: std.StringHashMapUnmanaged(void) = .{},
    enums: std.StringHashMapUnmanaged(dtd.Enum) = .{},
    commands: std.StringHashMapUnmanaged(dtd.Command) = .{},
    extensions: std.StringHashMapUnmanaged(dtd.Extension) = .{},
    features: std.ArrayListUnmanaged(dtd.Feature) = .{},

    pub fn deinit(self: *@This()) void {
        self.enumgroups.deinit(self.allocator);
        self.enums.deinit(self.allocator);
        {
            var it = self.commands.iterator();
            while (it.next()) |entry| {
                var command: *dtd.Command = entry.value_ptr;
                command.params.deinit(self.allocator);
            }
            self.commands.deinit(self.allocator);
        }
        {
            var it = self.extensions.iterator();
            while (it.next()) |entry| {
                var ext: *dtd.Extension = entry.value_ptr;
                ext.require.deinit(self.allocator);
            }
            self.extensions.deinit(self.allocator);
        }
        for (self.features.items) |*feature| {
            feature.require.deinit(self.allocator);
            feature.remove.deinit(self.allocator);
        }
        self.features.deinit(self.allocator);
        self.string_arena.deinit();
        self.allocator.free(self.registry_content);
    }

    pub const ParseError = error{
        BadXmlVersion,
        InvalidTag,
        SchemaIncorrectTag,
        SchemaRequiredAttribute,
        SchemaRequiredProperty,
        SchemaSelfClose,
        SchemaNonSelfClose,
        SchemaBadVersionMinor,
        SchemaBadVersionMajor,
        SchemaBadFeatureNumberFormat,
        BadTopLevel,
        BadTopLevelClose,
        EndTagMismatch,
    };

    pub fn parse(allocator: std.mem.Allocator, file: std.fs.File) !@This() {
        const registry_content = try file.readToEndAlloc(allocator, 4 * 1024 * 1024);
        var self: Registry = .{
            .allocator = allocator,
            .registry_content = registry_content,
            .string_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
        errdefer self.deinit();

        var fbstream = std.io.fixedBufferStream(registry_content);
        const reader = fbstream.reader();

        var counting_readerst = std.io.countingReader(reader);
        const counting_reader = counting_readerst.reader();

        var element_stack = std.ArrayList(dtd.Element).init(allocator);
        defer element_stack.deinit();
        defer if (element_stack.items.len != 0) {
            var idx: usize = element_stack.items.len - 1;
            while (idx > 0) : (idx -= 1) {
                const elem = &element_stack.items[idx];

                switch (elem.*) {
                    .command => |*command| {
                        command.params.deinit(self.allocator);
                    },
                    else => {},
                }
            }
        };

        while (true) {
            const content_start = fbstream.pos;
            counting_readerst.bytes_read = 0;
            counting_reader.streamUntilDelimiter(std.io.null_writer, '<', null) catch |err| switch (err) {
                error.EndOfStream => break,
                else => return err,
            };
            const content_end = content_start + counting_readerst.bytes_read - 1;

            const tag_slice_start = fbstream.pos;
            counting_readerst.bytes_read = 0;
            try xml.readXmlTag(counting_reader, void);
            const tag_slice_end = tag_slice_start + counting_readerst.bytes_read - 1;

            const content = registry_content[content_start..content_end];
            const tag_slice = registry_content[tag_slice_start..tag_slice_end];

            var xmltag = try xml.parseXmlTag(allocator, tag_slice);
            switch (xmltag) {
                .xml_decl => |xml_decl| {
                    if (xml_decl.version.major != 1 and xml_decl.version.major != 0) {
                        std.log.err("Unsupported XML version {}.{}", .{
                            xml_decl.version.major,
                            xml_decl.version.minor,
                        });
                        return ParseError.BadXmlVersion;
                    }
                },
                .start_tag => |*tag| {
                    defer tag.deinit(allocator);

                    const tag_name: dtd.Element.Tag = std.meta.stringToEnum(dtd.Element.Tag, tag.name) orelse
                        return ParseError.InvalidTag;

                    if (element_stack.items.len == 0) {
                        if (tag_name != .registry) {
                            std.log.err("expected <registry> as top tag, got <{s}>", .{@tagName(tag_name)});
                            return ParseError.SchemaIncorrectTag;
                        }
                        if (tag.self_close) {
                            std.log.err("<registry> must be have content", .{});
                            return ParseError.SchemaNonSelfClose;
                        }
                        try element_stack.append(.{ .registry = .{} });
                        continue;
                    }

                    try handleStartTag(allocator, &self, &element_stack, tag, tag_name, content);
                },
                .comment => {},
                .end_tag => |end_tag| try handleClosingTag(allocator, &self, &element_stack, end_tag, content),
            }
        }

        return self;
    }

    fn handleStartTag(
        allocator: std.mem.Allocator,
        registry: *@This(),
        element_stack: *std.ArrayList(dtd.Element),
        tag: *xml.XmlTag.StartTag,
        tag_name: dtd.Element.Tag,
        content: []const u8,
    ) !void {
        // BUG(zig 0.13.0): Can't return explicit error

        const top_tag: *dtd.Element = &element_stack.items[element_stack.items.len - 1];

        errdefer |err| if (err == ParseError.SchemaIncorrectTag) {
            std.log.err("unexpected tag <{s}>, parent <{s}>", .{
                @tagName(tag_name),
                @tagName(top_tag.*),
            });
        };

        switch (top_tag.*) {
            .registry => |*registry_tag| switch (tag_name) {
                .comment => if (!tag.self_close)
                    try element_stack.append(.{ .comment = "" })
                else {
                    registry_tag.copyright = "";
                },
                .types => if (!tag.self_close) try element_stack.append(.{ .types = {} }),
                .kinds => if (!tag.self_close) try element_stack.append(.{ .kinds = {} }),
                .enums => {
                    const bitmask: bool = if (tag.attributes.get("type")) |type_attr|
                        std.mem.eql(u8, type_attr, "bitmask")
                    else
                        false;
                    const group = tag.attributes.get("group");

                    if (group) |name| {
                        try registry.enumgroups.putNoClobber(allocator, name, {});
                    }

                    if (!tag.self_close) {
                        try element_stack.append(.{ .enums = .{ .bitmask = bitmask, .group = group } });
                    }
                },
                .commands => if (!tag.self_close) try element_stack.append(.{ .commands = {} }),

                .feature => {
                    const api = tag.attributes.get("api") orelse {
                        std.log.err("<feature> has to have 'api' attribute", .{});
                        return error.SchemaRequiredAttribute;
                    };
                    const name = tag.attributes.get("name") orelse {
                        std.log.err("<feature> has to have 'name' attribute", .{});
                        return error.SchemaRequiredAttribute;
                    };
                    const number = tag.attributes.get("number") orelse {
                        std.log.err("<feature> has to have 'number' attribute", .{});
                        return error.SchemaRequiredAttribute;
                    };

                    if (!tag.self_close) try element_stack.append(.{ .feature = .{
                        .api = api,
                        .name = name,
                        .number = dtd.FeatureNumber.parse(number) catch |err| {
                            switch (err) {
                                error.BadFormat => {
                                    std.log.err("<feature> 'number' attribute has to have 'major.minor' format", .{});
                                    return error.SchemaBadFeatureNumberFormat;
                                },
                                error.BadMajor => {
                                    std.log.err("<feature> 'number' major field has to be a positive number", .{});
                                    return error.SchemaBadVersionMajor;
                                },
                                error.BadMinor => {
                                    std.log.err("<feature> 'number' minor field has to be a positive number", .{});
                                    return error.SchemaBadVersionMinor;
                                },
                                else => unreachable,
                            }
                        },
                    } });
                },
                .extensions => if (!tag.self_close) try element_stack.append(.{ .extensions = {} }),
                else => return error.SchemaIncorrectTag,
            },
            .types => switch (tag_name) {
                // TODO: Handle <type> tags
                .type => if (!tag.self_close)
                    try element_stack.append(.{ .type = {} })
                else
                    unreachable,
                else => return error.SchemaIncorrectTag,
            },
            .kinds => switch (tag_name) {
                .kind => {},
                else => return error.SchemaIncorrectTag,
            },
            .enums => switch (tag_name) {
                .@"enum" => if (tag.self_close) {
                    const attr_name = tag.attributes.get("name") orelse {
                        std.log.err("<enum> has to have 'name' attribute", .{});
                        return error.SchemaRequiredAttribute;
                    };
                    const value = tag.attributes.get("value") orelse {
                        std.log.err("<enum name='{s}'> has to have 'value' attribute", .{attr_name});
                        return error.SchemaRequiredAttribute;
                    };
                    const groups = tag.attributes.get("groups") orelse "";
                    const attr_api = tag.attributes.get("api");

                    const name = if (attr_api) |api| blk: {
                        const uppercase_api = uppercaseApiString(api);
                        const alloced = try registry.string_arena.allocator().alloc(u8, uppercase_api.len + attr_name.len - 2);
                        std.mem.copyForwards(u8, alloced, uppercase_api);
                        std.mem.copyForwards(u8, alloced[uppercase_api.len..], attr_name[2..]);
                        break :blk alloced;
                    } else attr_name;

                    try registry.enums.putNoClobber(registry.allocator, name, .{
                        .name = name,
                        .value = value,
                        .groups = groups,
                        .api = attr_api,
                    });
                } else {
                    std.log.err("<enum> must be self-closing", .{});
                    return error.SchemaSelfClose;
                },
                .unused => {},
                else => return error.SchemaIncorrectTag,
            },
            .commands => switch (tag_name) {
                .command => {
                    if (tag.self_close) {
                        std.log.err("<command> cannot be self-closing", .{});
                        return error.SchemaNonSelfClose;
                    }
                    try element_stack.append(.{ .command = .{} });
                },
                else => return error.SchemaIncorrectTag,
            },
            .command => switch (tag_name) {
                .proto => try element_stack.append(.{
                    .proto = .{ .name = "" },
                }),
                .param => try element_stack.append(.{
                    .param = .{ .name = "", .type = "" },
                }),
                .glx => {},
                .vecequiv => {},
                .alias => {
                    const name = tag.attributes.get("name") orelse {
                        std.log.err("command alias has to have 'name' attribute", .{});
                        return error.SchemaRequiredAttribute;
                    };
                    if (name.len == 0) {
                        std.log.err("command alias name can't be empty", .{});
                        return error.SchemaRequiredAttribute;
                    }
                    top_tag.*.command.alias = name;
                },
                else => return error.SchemaIncorrectTag,
            },
            .proto => switch (tag_name) {
                .name => try element_stack.append(.{ .name = "" }),
                .ptype => try element_stack.append(.{ .ptype = "" }),
                else => return error.SchemaIncorrectTag,
            },
            .param => |*param| switch (tag_name) {
                .name => {
                    if (param.type.len == 0) {
                        // Some params don't have ptype, mostly `const void*`
                        // Assume content before <name> is the type
                        param.type = std.mem.trim(u8, content, " ");
                    }
                    try element_stack.append(.{ .name = "" });
                },
                .ptype => try element_stack.append(.{ .ptype = "" }),
                else => return error.SchemaIncorrectTag,
            },
            .type => switch (tag_name) {
                .name => try element_stack.append(.{ .name = "" }),
                .apientry => {},
                else => return error.SchemaIncorrectTag,
            },
            .feature => switch (tag_name) {
                .require => if (!tag.self_close) try element_stack.append(.{ .require = {} }),
                .remove => if (!tag.self_close) try element_stack.append(.{ .remove = {} }),
                else => return error.SchemaIncorrectTag,
            },
            .extensions => switch (tag_name) {
                .extension => if (!tag.self_close) {
                    const name = tag.attributes.get("name") orelse {
                        std.log.err("<extension> tag must have `name` attribute", .{});
                        return error.SchemaRequiredAttribute;
                    };
                    try element_stack.append(.{ .extension = .{ .name = name } });
                },
                else => return error.SchemaIncorrectTag,
            },
            .extension => switch (tag_name) {
                .require => if (!tag.self_close) try element_stack.append(.{ .require = {} }),
                .remove => if (!tag.self_close) try element_stack.append(.{ .remove = {} }),
                else => return error.SchemaIncorrectTag,
            },
            .require => switch (tag_name) {
                .command => {},
                .@"enum" => {},
                .type => {},
                else => return error.SchemaIncorrectTag,
            },
            .remove => switch (tag_name) {
                .command => {},
                .@"enum" => {},
                .type => {},
                else => return error.SchemaIncorrectTag,
            },
            else => std.debug.panic("not handling <{s}> <{s}>", .{ @tagName(top_tag.*), @tagName(tag_name) }),
        }
    }

    fn handleClosingTag(
        allocator: std.mem.Allocator,
        _: *@This(),
        element_stack: *std.ArrayList(dtd.Element),
        end_tag: xml.XmlTag.EndTag,
        content: []const u8,
    ) !void {
        const tag_name = std.meta.stringToEnum(dtd.Element.Tag, end_tag.name) orelse
            return ParseError.InvalidTag;

        if (element_stack.items.len == 0) {
            std.log.err("got {} end-tag when no elements have been processed yet", .{tag_name});
            return ParseError.BadTopLevelClose;
        }
        var element: dtd.Element = element_stack.pop();
        const element_tag = std.meta.activeTag(element);
        if (element_tag != tag_name) {
            std.log.err("got </{s}> but </{s}> expected", .{ @tagName(tag_name), @tagName(element_tag) });
            return ParseError.EndTagMismatch;
        }
        if (element_stack.items.len == 0) {
            if (element_tag != .registry) {
                std.log.err("{s} is not a top-level element", .{@tagName(element_tag)});
                return ParseError.SchemaIncorrectTag;
            }
            return;
        }

        const top = &element_stack.items[element_stack.items.len - 1];
        switch (element) {
            .kinds,
            .kind,
            .types,
            .type,
            .commands,
            .extensions,
            .enums,
            .glx,
            => {},

            .comment => switch (top.*) {
                .registry => |*reg| reg.copyright = content,
                else => return ParseError.SchemaIncorrectTag,
            },
            .name => switch (top.*) {
                .type => {},
                .proto => |*proto| proto.name = content,
                .param => |*param| param.name = content,
                else => return ParseError.SchemaIncorrectTag,
            },
            .ptype => switch (top.*) {
                .param => |*param| param.type = content,
                .proto => |*proto| proto.return_type = content,
                else => return ParseError.SchemaIncorrectTag,
            },
            .alias => |alias| switch (top.*) {
                .command => |*command| command.alias = alias,
                else => return ParseError.SchemaIncorrectTag,
            },

            .unused,
            .@"enum",
            .vecequiv,
            .apientry,
            .registry,
            => std.debug.panic("tried to close self-closing tag '{s}'", .{@tagName(element_tag)}),

            .command => |*command| {
                command.params.deinit(allocator);
            },
            .proto => |proto| switch (top.*) {
                .command => |*command| {
                    if (proto.name.len == 0) {
                        std.log.err("command prototype has to have a name", .{});
                        return ParseError.SchemaRequiredProperty;
                    }
                    command.name = proto.name;
                    command.return_type = proto.return_type;
                },
                else => return ParseError.SchemaIncorrectTag,
            },
            .param => |param| switch (top.*) {
                .command => |*command| {
                    if (param.name.len == 0) {
                        std.log.err("command parameter has to have a name", .{});
                        return ParseError.SchemaRequiredProperty;
                    }
                    if (param.type.len == 0) {
                        std.log.err("command parameter has to have a type", .{});
                        return ParseError.SchemaRequiredProperty;
                    }
                    try command.params.append(allocator, param);
                },
                else => return ParseError.SchemaIncorrectTag,
            },

            .extension => {},
            .feature => {},
            .require => switch (top.*) {
                .feature, .extension => {},
                else => return ParseError.SchemaIncorrectTag,
            },
            .remove => switch (top.*) {
                .feature, .extension => {},
                else => return ParseError.SchemaIncorrectTag,
            },
        }
    }
};

/// Fields uniquely identifying a feature
pub const FeatureKey = struct {
    api: Registry.Feature.Api,
    number: std.SemanticVersion,
};

/// Fields uniquely identifying an extension
pub const ExtensionKey = []const u8;

fn uppercaseApiString(api: []const u8) []const u8 {
    return if (std.mem.eql(u8, api, "gl"))
        "GL"
    else if (std.mem.startsWith(u8, api, "gles"))
        "GLES"
    else
        unreachable;
}

/// Translates a C type into a Zig type.
///
/// `allocator` is used for temporary allocations inside this function.
/// Returned string is allocated using `string_allocator` and must be freed by the caller
fn translateCType(
    allocator: std.mem.Allocator,
    string_allocator: std.mem.Allocator,
    ctype: []const u8,
) ![]const u8 {
    const ctype_trimmed = std.mem.trim(u8, ctype, " ");

    if (std.mem.eql(u8, "void", ctype_trimmed)) {
        return try string_allocator.dupe(u8, "void");
    }

    var tokens = std.ArrayList([]const u8).init(allocator);
    defer tokens.deinit();

    {
        var it = std.mem.splitScalar(u8, ctype_trimmed, ' ');
        while (it.next()) |dirty_token| {
            if (dirty_token.len == 0) {
                continue;
            }
            if (dirty_token.len == 1 and dirty_token[0] == '*') {
                try tokens.append("*");
                continue;
            }
            var it2 = std.mem.splitScalar(u8, dirty_token, '*');
            while (it2.next()) |token| {
                if (token.len == 0) {
                    try tokens.append("*");
                    continue;
                }
                try tokens.append(std.mem.trim(u8, token, " "));
            }
        }
    }

    // left const to right const
    if (std.mem.eql(u8, tokens.items[0], "const")) {
        // find first '*' and insert const before it
        // if not found place const at the end
        const idx: usize = for (tokens.items[1..], 1..) |token, idx| {
            if (std.mem.eql(u8, token, "*")) {
                break idx - 1;
            }
        } else tokens.items.len - 1;

        try tokens.insert(idx, tokens.orderedRemove(0));
    }

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var idx: usize = tokens.items.len;
    while (idx > 0) {
        idx -= 1;
        const token = tokens.items[idx];

        // const void * => ?*anyopaque
        // void const * => ?*anyopaque
        // const GLenum * => [*c]const GLenum
        if (std.mem.eql(u8, token, "*")) {
            const c1 = idx > 0 and std.mem.eql(u8, tokens.items[idx - 1], "void");
            const c2 = idx > 1 and std.mem.eql(u8, tokens.items[idx - 2], "void");
            if (c1 or c2) {
                try buffer.appendSlice("?*");
            } else {
                try buffer.appendSlice("[*c]");
            }
        } else if (std.mem.eql(u8, token, "const")) {
            try buffer.appendSlice("const ");
        } else if (std.mem.eql(u8, token, "void")) {
            try buffer.appendSlice("anyopaque");
        } else if (std.mem.eql(u8, token, "int")) {
            try buffer.appendSlice("c_int");
        } else if (std.mem.eql(u8, token, "short")) {
            try buffer.appendSlice("c_short");
        } else if (std.mem.eql(u8, token, "long")) {
            try buffer.appendSlice("c_long");
        } else {
            try buffer.appendSlice(token);
        }
    }

    if (std.mem.eql(u8, buffer.items, "[*c]const GLubyte")) {
        return try string_allocator.dupe(u8, "?[*:0]const GLubyte");
    }

    return try string_allocator.dupe(u8, buffer.items);
}

test "translateCType" {
    const testing = std.testing;
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "void");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "void", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "GLenum");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "GLenum", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLenum");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "const GLenum", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLuint *");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "[*c]const GLuint", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLuint*");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "[*c]const GLuint", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLubyte *");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "?[*:0]const GLubyte", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLubyte*");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "?[*:0]const GLubyte", translated);
    }
}
