const std = @import("std");
const xml = @import("./xml.zig");

const ElementTag = enum {
    registry,

    comment,
    name,

    types,
    type,
    apientry,
    kinds,
    kind,

    enums,
    @"enum",
    unused,

    commands,
    command,
    proto,
    param,
    ptype,
    glx,
    alias,
    vecequiv,

    feature,
    extensions,
    extension,
    require,
    remove,
};

pub const Registry = struct {
    allocator: std.mem.Allocator,
    string_arena: std.heap.ArenaAllocator,
    enumgroups: std.StringHashMapUnmanaged(void) = .{},
    enums: std.StringHashMapUnmanaged(Enum) = .{},
    commands: std.StringHashMapUnmanaged(Command) = .{},
    types: std.StringHashMapUnmanaged(Type) = .{},
    extensions: std.StringHashMapUnmanaged(Extension) = .{},
    features: std.ArrayListUnmanaged(Feature) = .{},

    pub const Enum = struct {
        name: []const u8,
        value: usize,
        signed: bool = false,
        groups: std.ArrayListUnmanaged([]const u8) = .{},
    };

    pub const Command = struct {
        name: []const u8,
        return_type: []const u8,
        params: std.ArrayListUnmanaged(Param) = .{},

        pub const Param = struct {
            name: []const u8,
            type: []const u8,
            inner_type: []const u8,
            group: ?[]const u8,
        };
    };

    pub const Type = struct {
        alias: []const u8,
        original: []const u8,
    };

    pub const Extension = struct {
        name: []const u8,
        supported_api: std.BoundedArray(Feature.Api, 8) = .{},
        require_set: std.ArrayListUnmanaged(RequirementRef) = .{},

        const RequirementRef = struct {
            requirement: Requirement,
            api: ?Registry.Feature.Api = null,
        };
    };

    pub const Feature = struct {
        api: Api,
        number: std.SemanticVersion,
        require_set: std.ArrayListUnmanaged(Requirement) = .{},
        remove_set: std.ArrayListUnmanaged(Requirement) = .{},

        pub const Api = enum(u4) {
            gl,
            glcore,
            gles1,
            gles2,
            glsc2,
        };

        pub fn asKey(self: @This()) FeatureKey {
            return .{ .api = self.api, .number = self.number };
        }
    };

    pub const Requirement = union(enum) {
        @"enum": []const u8,
        command: []const u8,
        type: []const u8,

        pub fn name(self: @This()) []const u8 {
            return switch (self) {
                inline else => |n| return n,
            };
        }
    };

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .string_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.enumgroups.deinit(self.allocator);
        {
            var it = self.enums.iterator();
            while (it.next()) |entry| {
                var e: *Registry.Enum = entry.value_ptr;
                e.groups.deinit(self.allocator);
            }
            self.enums.deinit(self.allocator);
        }
        {
            var it = self.commands.iterator();
            while (it.next()) |entry| {
                var command: *Registry.Command = entry.value_ptr;
                command.params.deinit(self.allocator);
            }
            self.commands.deinit(self.allocator);
        }
        {
            var it = self.extensions.iterator();
            while (it.next()) |entry| {
                var ext: *Registry.Extension = entry.value_ptr;
                ext.require_set.deinit(self.allocator);
            }
            self.extensions.deinit(self.allocator);
        }
        for (self.features.items) |*feature| {
            feature.require_set.deinit(self.allocator);
            feature.remove_set.deinit(self.allocator);
        }
        self.features.deinit(self.allocator);
        self.string_arena.deinit();
    }

    pub fn sortFeatures(self: *@This()) void {
        const SortCtx = struct {
            items: []Registry.Feature,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                const a_api_value = @intFromEnum(ctx.items[a].api);
                const b_api_value = @intFromEnum(ctx.items[b].api);
                return a_api_value < b_api_value or
                    (a_api_value == b_api_value and
                    ctx.items[b].number.order(ctx.items[a].number) == .gt);
            }
            pub fn swap(ctx: @This(), a: usize, b: usize) void {
                return std.mem.swap(Registry.Feature, &ctx.items[a], &ctx.items[b]);
            }
        };
        std.sort.heapContext(0, self.features.items.len, SortCtx{ .items = self.features.items });
    }

    pub fn getFeatureRange(self: *@This(), api: Registry.Feature.Api) ?[]Registry.Feature {
        self.sortFeatures();
        const feature_start = for (self.features.items, 0..) |feat, idx| {
            if (feat.api == api) {
                break idx;
            }
        } else return null;

        const feature_end = for (self.features.items[feature_start..], feature_start..) |feat, idx| {
            if (feat.api != api) {
                break idx;
            }
        } else self.features.items.len;

        // inclusive start exclusive end, [start; end)
        return self.features.items[feature_start..feature_end];
    }
    pub fn getFeature(self: @This(), feature: FeatureKey) ?Feature {
        for (self.features.items) |feat| {
            if (feat.api == feature.api and feat.number.order(feature) == .eq) {
                return feat;
            }
        }
        return null;
    }
};

/// Fields uniquely identifying a feature
pub const FeatureKey = struct {
    api: Registry.Feature.Api,
    number: std.SemanticVersion,
};

/// Fields uniquely identifying an extension
pub const ExtensionKey = []const u8;

/// Extracts information about a command from the given XmlTree and stores
/// it in registry
fn extractCommand(registry: *Registry, tree: *xml.XmlTree) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();

    const proto = tree.root.?.findElement("proto") orelse
        return error.CommandWithoutProto;

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    // const return_type_elem = proto.findElement("ptype");

    const name_elem = proto.findElement("name") orelse
        return error.CommandWithoutName;

    try name_elem.collectText(buffer.writer());

    const cmd_name = try string_allocator.dupe(u8, buffer.items);
    buffer.clearRetainingCapacity();

    try proto.collectTextBefore(name_elem, buffer.writer());

    const return_type = try translateCType(allocator, string_allocator, buffer.items);
    // std.debug.print("{s} {s}\n", .{ buffer.items, return_type });
    buffer.clearRetainingCapacity();

    var cmd = Registry.Command{
        .name = cmd_name,
        .return_type = return_type,
    };

    var param_it = tree.root.?.findElements("param");
    while (param_it.next()) |param_elem| {
        const param_group = param_elem.attributes.get("group");
        const param_name_elem = param_elem.findElement("name") orelse
            return error.CommandParamWithoutName;

        try param_name_elem.collectText(buffer.writer());
        const param_name = try string_allocator.dupe(u8, buffer.items);
        buffer.clearRetainingCapacity();

        try param_elem.collectTextBefore(param_name_elem, buffer.writer());

        const param_type = try translateCType(allocator, string_allocator, buffer.items);
        buffer.clearRetainingCapacity();

        if (param_elem.findElement("ptype")) |ptype| {
            try ptype.collectText(buffer.writer());
        }

        const param_inner_type = if (buffer.items.len == 0)
            param_type
        else
            try string_allocator.dupe(u8, buffer.items);
        buffer.clearRetainingCapacity();

        try cmd.params.append(allocator, .{
            .name = param_name,
            .type = param_type,
            .inner_type = param_inner_type,
            .group = if (param_group) |group|
                try string_allocator.dupe(u8, group)
            else
                null,
        });
    }

    try registry.commands.putNoClobber(allocator, cmd.name, cmd);
}

/// Extracts information about a constant from the given tag
/// and stores it in registry
pub fn extractEnum(registry: *Registry, tag: *xml.XmlTag.StartTag) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();

    const value = tag.attributes.get("value") orelse return error.EnumHasNoValue;
    const name = tag.attributes.get("name") orelse return error.EnumHasNoName;
    const maybe_groups = tag.attributes.get("group");

    var en = Registry.Enum{
        .name = try string_allocator.dupe(u8, name),
        .value = 0,
    };

    en.value = std.fmt.parseInt(usize, value, 0) catch |err| switch (err) {
        error.Overflow => blk: {
            const v = try std.fmt.parseInt(isize, value, 0);
            en.signed = true;
            break :blk @bitCast(v);
        },
        else => return err,
    };

    if (maybe_groups) |groups| {
        var it = std.mem.splitScalar(u8, groups, ',');
        while (it.next()) |group| {
            const reg_name = registry.enumgroups.getKey(group);
            const group_regkey = if (reg_name) |_|
                reg_name.?
            else
                try string_allocator.dupe(u8, group);

            if (reg_name) |_| {
                const group_getorput_result = try registry.enumgroups.getOrPut(allocator, group_regkey);
                if (!group_getorput_result.found_existing) {
                    group_getorput_result.value_ptr.* = {};
                }
            }

            try en.groups.append(registry.allocator, group_regkey);
        }
    }

    try registry.enums.put(allocator, en.name, en);
}

/// Extracts information about given enum group and stores it in registry.
/// An enum group is a `<enums group="name">` tag
pub fn extractEnumGroup(registry: *Registry, tag: *xml.XmlTag.StartTag) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();

    const group = tag.attributes.get("group") orelse return;

    if (registry.enumgroups.get(group) != null) {
        return;
    }

    try registry.enumgroups.putNoClobber(
        allocator,
        try string_allocator.dupe(u8, group),
        {},
    );
}

/// Extracts information about an extension, including it's requirements
/// and stores it in the registry.
pub fn extractExtension(registry: *Registry, tree: *xml.XmlTree) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();
    var tag = tree.root.?;
    const name = tag.attributes.get("name") orelse return error.ExtensionWithoutName;
    const supported = tag.attributes.get("supported");

    var ext: Registry.Extension = .{
        .name = try string_allocator.dupe(u8, name),
    };

    if (supported) |supstring| {
        var it = std.mem.tokenizeScalar(u8, supstring, '|');
        while (it.next()) |s| {
            if (std.mem.eql(u8, s, "disabled")) {
                break;
            }
            const api = std.meta.stringToEnum(Registry.Feature.Api, s) orelse {
                std.log.debug("{s}", .{s});
                return error.UnknownApi;
            };
            try ext.supported_api.append(api);
        }
    }

    var req_elem_it = tag.findElements("require");
    while (req_elem_it.next()) |require_elem| {
        const api = if (require_elem.attributes.get("api")) |api_str|
            std.meta.stringToEnum(Registry.Feature.Api, api_str) orelse return error.UnknownApi
        else
            null;

        var req_it = require_elem.elementIterator();
        while (req_it.next()) |req| {
            try ext.require_set.append(allocator, .{
                .requirement = try extractRequirement(string_allocator, req),
                .api = api,
            });
        }
    }

    try registry.extensions.putNoClobber(allocator, ext.name, ext);
}

/// Extracts a requirement from a given XML element
///
/// A requirement is a <enum>, <command> or <type> tag in a <require> element:
/// ```xml
/// <feature ...>
///    <require>
///       <enum name="..."/>
///       <type name="..."/>
///       <command name="..."/>
///    ...
///```
pub fn extractRequirement(allocator: std.mem.Allocator, elem: *xml.XmlTree.Element) !Registry.Requirement {
    const RequirementEnum = @typeInfo(Registry.Requirement).Union.tag_type.?;

    const name = elem.attributes.get("name") orelse return error.RequirementWithoutName;

    const component = std.meta.stringToEnum(RequirementEnum, elem.name) orelse {
        std.debug.print("{s}\n", .{name});
        return error.UnknownRequirement;
    };

    const name_copy = try allocator.dupe(u8, name);

    return switch (component) {
        .@"enum" => .{ .@"enum" = name_copy },
        .command => .{ .command = name_copy },
        .type => .{ .type = name_copy },
    };
}

/// Extracts information about a feature, including it's requirements, from a given XML tree and
/// stores it in the registry
///
/// A feature is a combination of an api and number/version string
pub fn extractFeature(registry: *Registry, tree: *xml.XmlTree) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();
    var tag = tree.root.?;
    const api_attr = tag.attributes.get("api") orelse return error.FeatureWithoutApi;
    const number = tag.attributes.get("number") orelse return error.FeatureWithoutNumber;

    const api = std.meta.stringToEnum(Registry.Feature.Api, api_attr) orelse {
        std.log.err("Unsupported api: {s}\n", .{api_attr});
        return error.UnsupportedFeatureApi;
    };

    var number_it = std.mem.splitScalar(u8, number, '.');
    const version = std.SemanticVersion{
        .major = try std.fmt.parseInt(usize, number_it.next() orelse "0", 10),
        .minor = try std.fmt.parseInt(usize, number_it.next() orelse "0", 10),
        .patch = try std.fmt.parseInt(usize, number_it.next() orelse "0", 10),
    };

    var feature = Registry.Feature{
        .api = api,
        .number = version,
    };

    var require_it = tag.findElements("require");
    while (require_it.next()) |require_elem| {
        var it = require_elem.elementIterator();
        while (it.next()) |component_elem| {
            try feature.require_set.append(
                allocator,
                try extractRequirement(string_allocator, component_elem),
            );
        }
    }

    var remove_it = tag.findElements("remove");
    while (remove_it.next()) |remove_elem| {
        var it = remove_elem.elementIterator();
        while (it.next()) |component_elem| {
            try feature.remove_set.append(
                allocator,
                try extractRequirement(string_allocator, component_elem),
            );
        }
    }

    try registry.features.append(allocator, feature);
}

pub fn parseRegistry(
    allocator: std.mem.Allocator,
    reader: anytype,
) !Registry {
    var registry = Registry.init(allocator);
    errdefer registry.deinit();
    // errdefer {
    //     if (@errorReturnTrace()) |trace| {
    //         std.debug.dumpStackTrace(trace.*);
    //     }
    // }

    const TagOrElement = union(enum) {
        tag: ElementTag,
        element: xml.XmlTree.Element,

        fn getTag(self: @This()) ElementTag {
            return switch (self) {
                .tag => |t| t,
                .element => |element| std.meta.stringToEnum(ElementTag, element.name).?,
            };
        }

        fn deinit(self: *@This(), alloc: std.mem.Allocator) void {
            switch (self.*) {
                .tag => {},
                .element => |*element| element.deinit(alloc),
            }
        }
    };

    var element_stack = std.ArrayList(TagOrElement).init(allocator);
    defer element_stack.deinit();

    var read_buffer = std.ArrayList(u8).init(allocator);
    defer read_buffer.deinit();

    while (true) {
        reader.streamUntilDelimiter(read_buffer.writer(), '<', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        read_buffer.clearRetainingCapacity();
        try reader.streamUntilDelimiter(read_buffer.writer(), '>', null);

        // check if we are inside a comment
        // and make sure we have read a full comment tag in case a comment contains a valid
        // xml tag
        // eg. <!-- <element attr1=""/> -->
        if (read_buffer.items.len > 3 and std.mem.eql(u8, read_buffer.items[0..3], "!--")) {
            while (true) {
                const last_2chars = read_buffer.items[read_buffer.items.len - 2 .. read_buffer.items.len];
                if (std.mem.eql(u8, last_2chars, "--")) {
                    break;
                }
                try read_buffer.append('>');
                try reader.streamUntilDelimiter(read_buffer.writer(), '>', null);
            }
        }

        var xml_tag = try xml.parseXmlTag(allocator, read_buffer.items);
        switch (xml_tag) {
            .xml_decl => |*tag| {
                if (tag.version.order(.{ .major = 1, .minor = 0, .patch = 0 }) == .gt) {
                    std.debug.print("(xml) Unsupported XML version\n", .{});
                    return error.UnsupportedVersion;
                }
            },
            .start_tag => |*tag| {
                defer tag.deinit(allocator);
                const elemtag = std.meta.stringToEnum(ElementTag, tag.name) orelse return error.UnknownTag;
                const parent = element_stack.getLastOrNull();

                if (!tag.self_close) {
                    try element_stack.append(.{ .tag = elemtag });
                }

                if (parent) |top| switch (top.getTag()) {
                    .registry => switch (elemtag) {
                        .enums => try extractEnumGroup(&registry, tag),
                        .feature => {
                            var tree = try xml.parseXml(allocator, reader, tag.*);
                            defer tree.deinit();
                            defer _ = element_stack.pop();

                            try extractFeature(&registry, &tree);
                        },
                        else => {},
                    },
                    .enums => switch (elemtag) {
                        .@"enum" => try extractEnum(&registry, tag),
                        .unused => {},
                        else => unreachable,
                    },
                    .commands => switch (elemtag) {
                        .command => {
                            var tree = try xml.parseXml(allocator, reader, tag.*);
                            defer tree.deinit();
                            defer _ = element_stack.pop();

                            try extractCommand(&registry, &tree);
                        },
                        else => unreachable,
                    },
                    .extensions => switch (elemtag) {
                        .extension => {
                            if (tag.self_close) {
                                continue;
                            }
                            var tree = try xml.parseXml(allocator, reader, tag.*);
                            defer tree.deinit();
                            defer _ = element_stack.pop();

                            try extractExtension(&registry, &tree);
                        },
                        else => unreachable,
                    },
                    else => {},
                };
            },
            .end_tag => |*tag| {
                const elemtag = std.meta.stringToEnum(ElementTag, tag.name) orelse return error.UnknownTag;

                if (element_stack.getLastOrNull()) |element| {
                    if (element.getTag() != elemtag) {
                        return error.EndTagMismatch;
                    }
                    var top = element_stack.pop();
                    top.deinit(allocator);
                }
            },
            .comment => {},
        }
    }
    return registry;
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
