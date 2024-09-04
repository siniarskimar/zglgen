const std = @import("std");
const xml = @import("./xml.zig");
const dtd = @import("./dtd.zig");

pub const Registry = struct {
    allocator: std.mem.Allocator,
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
        self.allocator.free(self.registry_content);
    }

    pub const ParseError = error{
        BadXmlVersion,
        InvalidTag,
        SchemaIncorrectTag,
        SchemaRequiredAttribute,
        SchemaSelfClose,
        SchemaNonSelfClose,
        BadFormat,
        BadMajor,
        BadMinor,
        BadTopLevel,
        BadTopLevelClose,
        EndTagMismatch,
    };

    pub fn parse(allocator: std.mem.Allocator, file: std.fs.File) !@This() {
        const registry_content = try file.readToEndAlloc(allocator, 4 * 1024 * 1024);
        var self: Registry = .{ .allocator = allocator, .registry_content = registry_content };
        errdefer self.deinit();

        var fbstream = std.io.fixedBufferStream(registry_content);
        const reader = fbstream.reader();

        var counting_readerst = std.io.countingReader(reader);
        const counting_reader = counting_readerst.reader();

        var element_stack = std.ArrayList(dtd.Element).init(allocator);
        defer element_stack.deinit();

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
            _ = content;

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

                    if (element_stack.items.len != 0) {
                        const top_tag: *dtd.Element = &element_stack.items[element_stack.items.len - 1];
                        switch (top_tag.*) {
                            .registry => |*registry| switch (tag_name) {
                                .comment => if (!tag.self_close)
                                    try element_stack.append(.{ .comment = "" })
                                else {
                                    registry.copyright = "";
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
                                        try self.enumgroups.putNoClobber(allocator, name, {});
                                    }

                                    if (!tag.self_close) {
                                        try element_stack.append(.{ .enums = .{ .bitmask = bitmask, .group = group } });
                                    }
                                },
                                .commands => if (!tag.self_close) try element_stack.append(.{ .commands = {} }),

                                .feature => {
                                    const api = tag.attributes.get("api") orelse {
                                        std.log.err("<feature> has to have 'api' attribute", .{});
                                        return ParseError.SchemaRequiredAttribute;
                                    };
                                    const name = tag.attributes.get("name") orelse {
                                        std.log.err("<feature> has to have 'name' attribute", .{});
                                        return ParseError.SchemaRequiredAttribute;
                                    };
                                    const number = tag.attributes.get("number") orelse {
                                        std.log.err("<feature> has to have 'number' attribute", .{});
                                        return ParseError.SchemaRequiredAttribute;
                                    };

                                    if (!tag.self_close) try element_stack.append(.{ .feature = .{
                                        .api = api,
                                        .name = name,
                                        .number = dtd.FeatureNumber.parse(number) catch |err| {
                                            switch (err) {
                                                error.BadFormat => {
                                                    std.log.err("<feature> 'number' attribute has to have 'major.minor' format", .{});
                                                },
                                                error.BadMajor => {
                                                    std.log.err("<feature> 'number' major field has to be a positive number", .{});
                                                },
                                                error.BadMinor => {
                                                    std.log.err("<feature> 'number' minor field has to be a positive number", .{});
                                                },
                                            }
                                            return err;
                                        },
                                    } });
                                },
                                .extensions => if (!tag.self_close) try element_stack.append(.{ .extensions = {} }),
                                else => return ParseError.SchemaIncorrectTag,
                            },
                            .types => switch (tag_name) {
                                // TODO: Handle <type> tags
                                .type => if (!tag.self_close)
                                    try element_stack.append(.{ .type = {} })
                                else
                                    unreachable,
                                else => return ParseError.SchemaIncorrectTag,
                            },
                            .kinds => switch (tag_name) {
                                .kind => {},
                                else => return ParseError.SchemaIncorrectTag,
                            },
                            .enums => switch (tag_name) {
                                .@"enum" => if (tag.self_close) {
                                    const name = tag.attributes.get("name") orelse {
                                        std.log.err("<enum> has to have 'name' attribute", .{});
                                        return ParseError.SchemaRequiredAttribute;
                                    };
                                    const value = tag.attributes.get("value") orelse {
                                        std.log.err("<enum name='{s}'> has to have 'value' attribute", .{name});
                                        return ParseError.SchemaRequiredAttribute;
                                    };
                                    const groups = tag.attributes.get("groups") orelse "";
                                    try self.enums.putNoClobber(self.allocator, name, .{
                                        .name = name,
                                        .value = value,
                                        .groups = groups,
                                    });
                                } else {
                                    std.log.err("<enum> must be self-closing", .{});
                                    return ParseError.SchemaSelfClose;
                                },
                                else => return ParseError.SchemaIncorrectTag,
                            },
                            .commands => switch (tag_name) {
                                .command => {
                                    if (tag.self_close) {
                                        std.log.err("<command> cannot be self-closing", .{});
                                        return ParseError.SchemaNonSelfClose;
                                    }
                                    try element_stack.append(.{ .command = .{} });
                                },
                                else => return ParseError.SchemaIncorrectTag,
                            },
                            .command => switch (tag_name) {
                                .proto => {},
                                .param => {},
                                .glx => {},
                                else => return ParseError.SchemaIncorrectTag,
                            },
                            else => std.debug.panic("not handling <{}> <{}>", .{ top_tag, tag_name }),
                        }
                    } else {
                        if (tag_name != .registry) {
                            std.log.err("got {} as top level element of registry", .{tag_name});
                            return ParseError.BadTopLevel;
                        }
                        try element_stack.append(.{ .registry = .{} });
                    }
                },
                .end_tag => |end_tag| {
                    const tag_name = std.meta.stringToEnum(dtd.Element.Tag, end_tag.name) orelse
                        return ParseError.InvalidTag;

                    if (element_stack.items.len == 0) {
                        std.log.err("got {} end-tag when no elements have been processed yet", .{tag_name});
                        return ParseError.BadTopLevelClose;
                    }
                    const top = element_stack.pop();
                    const top_tag = std.meta.activeTag(top);
                    if (top_tag != tag_name) {
                        std.log.err("got </{}> but </{}> expected", .{ tag_name, top_tag });
                        return ParseError.EndTagMismatch;
                    }
                    _ = element_stack.pop();

                    switch (tag_name) {
                        else => unreachable,
                    }
                },
                .comment => {},
            }
        }

        return self;
    }
};

/// Fields uniquely identifying a feature
pub const FeatureKey = struct {
    api: Registry.Feature.Api,
    number: std.SemanticVersion,
};

/// Fields uniquely identifying an extension
pub const ExtensionKey = []const u8;

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
