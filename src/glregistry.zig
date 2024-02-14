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
    enumgroups: std.StringHashMapUnmanaged(EnumGroup) = .{},
    enums: std.StringHashMapUnmanaged(Enum) = .{},
    commands: std.StringHashMapUnmanaged(Command) = .{},
    types: std.StringHashMapUnmanaged(Type) = .{},
    extensions: std.StringHashMapUnmanaged(void) = .{},
    features: std.ArrayListUnmanaged(Feature) = .{},

    const EnumGroup = struct {
        bitmask: bool = false,
    };

    const Enum = struct {
        name: []const u8,
        value: usize,
        signed: bool = false,
        groups: std.ArrayListUnmanaged([]const u8) = .{},
    };

    const Command = struct {
        name: []const u8,
        return_type: []const u8,
        params: std.ArrayListUnmanaged(Param) = .{},

        const Param = struct {
            name: []const u8,
            type: []const u8,
            inner_type: []const u8,
            group: ?[]const u8,
        };
    };

    const Type = struct {
        alias: []const u8,
        original: []const u8,
    };

    pub const Feature = struct {
        api: Api,
        number: std.SemanticVersion,
        require_set: std.ArrayListUnmanaged(Requirement) = .{},
        remove_set: std.ArrayListUnmanaged(Requirement) = .{},

        pub const Api = enum(u4) {
            gl,
            gles1,
            gles2,
            glsc2,
        };
    };

    const Requirement = union(enum) {
        @"enum": []const u8,
        command: []const u8,
        type: []const u8,
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
        for (self.features.items) |*feature| {
            feature.require_set.deinit(self.allocator);
            feature.remove_set.deinit(self.allocator);
        }
        self.features.deinit(self.allocator);
        self.extensions.deinit(self.allocator);
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

    pub fn getFeatureRange(self: *@This(), api: Registry.Feature.Api) []Registry.Feature {
        self.sortFeatures();
        const feature_start = for (self.features.items, 0..) |feat, idx| {
            if (feat.api == api) {
                break idx;
            }
        } else unreachable;

        const feature_end = for (self.features.items, feature_start..) |feat, idx| {
            if (feat.api != api) {
                break idx;
            }
        } else unreachable;

        return self.features.items[feature_start..feature_end];
    }
};

fn extractCommand(registry: *Registry, tree: *xml.XmlTree) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();

    const proto = tree.root.?.findElement("proto") orelse
        return error.CommandWithoutProto;

    const return_type_elem = proto.findElement("ptype");

    const name_elem = proto.findElement("name") orelse
        return error.CommandWithoutName;

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    try name_elem.collectText(buffer.writer());

    const cmd_name = try string_allocator.dupe(u8, buffer.items);
    buffer.clearRetainingCapacity();

    if (return_type_elem) |elem| {
        try elem.collectText(buffer.writer());
    } else {
        try buffer.appendSlice("void");
    }

    const return_type = try translateCType(allocator, string_allocator, buffer.items);
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

        // TODO: fill in group parameter
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
            if (registry.enumgroups.getKey(group)) |key| {
                try en.groups.append(registry.allocator, key);
                continue;
            }

            const copy = try string_allocator.dupe(u8, group);
            try registry.enumgroups.putNoClobber(registry.allocator, copy, .{ .bitmask = false });
            try en.groups.append(registry.allocator, copy);
        }
    }

    try registry.enums.put(allocator, en.name, en);
}

pub fn extractEnumGroup(registry: *Registry, tag: *xml.XmlTag.StartTag) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();

    const group = tag.attributes.get("group") orelse return;
    const bitmask = if (tag.attributes.get("type")) |attr_type|
        std.mem.eql(u8, attr_type, "bitmask")
    else
        false;

    if (registry.enumgroups.getPtr(group)) |egroup| {
        egroup.bitmask = bitmask;
        return;
    }

    const egroup = Registry.EnumGroup{
        .bitmask = bitmask,
    };

    try registry.enumgroups.putNoClobber(
        allocator,
        try string_allocator.dupe(u8, group),
        egroup,
    );
}

pub fn extractExtension(registry: *Registry, tree: *xml.XmlTree) !void {
    const allocator = registry.allocator;
    const string_allocator = registry.string_arena.allocator();
    const tag = tree.root.?;
    const name = tag.attributes.get("name") orelse return error.ExtensionWithoutName;

    try registry.extensions.putNoClobber(allocator, try string_allocator.dupe(u8, name), {});
}

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

fn shiftDistance(a: usize, b: usize) u6 {
    if (a == b) {
        return 0;
    }

    const lower = if (a < b) a else b;

    for (1..@bitSizeOf(usize)) |shift| {
        const shift_u6 = @as(u6, @intCast(shift));

        if (@shrExact(a, shift_u6) == lower) {
            return shift_u6;
        }
    }
    unreachable;
}

fn writeEnumGroup(
    is_bitmask: bool,
    groupname: []const u8,
    enums: *std.ArrayListUnmanaged(Registry.Enum),
    writer: anytype,
) !void {
    if (is_bitmask) {
        const max_int = std.math.maxInt(u32);

        const SortCtx = struct {
            items: []Registry.Enum,

            pub fn lessThan(ctx: @This(), a: usize, b: usize) bool {
                return ctx.items[a].value < ctx.items[b].value;
            }
            pub fn swap(ctx: @This(), a: usize, b: usize) void {
                std.mem.swap(Registry.Enum, &ctx.items[a], &ctx.items[b]);
            }
        };
        std.sort.heapContext(0, enums.items.len, SortCtx{ .items = enums.items });

        try writer.print("pub const {s} = packed struct(GLenum) {{", .{groupname});
        var bitgaps: u8 = 0;
        var pow2: u6 = 0;
        var none_bits: ?Registry.Enum = null;
        var max_bits: ?Registry.Enum = null;

        for (enums.items) |e| {
            if (e.value == max_int) {
                max_bits = e;
                continue;
            }
            if (e.value == 0) {
                none_bits = e;
                continue;
            }

            const expected_value = @shlExact(@as(usize, 1), pow2);
            if (e.value > expected_value) {
                bitgaps += 1;
                const padding_bits = shiftDistance(e.value, expected_value);

                for (0..bitgaps) |_| {
                    try writer.writeAll("_");
                }
                try writer.print(": u{} = 0,\n", .{padding_bits});

                pow2 += padding_bits;
            }

            try writer.print("{s}: bool = false, // 0x{X}\n", .{ e.name, e.value });
            pow2 += 1;
        }
        if (pow2 < @bitSizeOf(c_uint)) {
            for (0..bitgaps + 1) |_| {
                try writer.writeAll("_");
            }
            try writer.print(": u{} = 0,\n", .{@bitSizeOf(c_uint) - pow2});
        }
        if (none_bits) |e| {
            try writer.print("pub const {s} = @as(@This(), @bitCast(0x{X}));\n", .{ e.name, e.value });
        }
        if (max_bits) |e| {
            try writer.print("pub const {s} = @as(@This(), @bitCast(0x{X}));\n", .{ e.name, e.value });
        }

        try writer.writeAll("};\n");
    } else {
        try writer.print("pub const {s} = enum(GLenum) {{", .{groupname});

        for (enums.items) |e| {
            try writer.print("{s} = {},", .{ e.name, e.value });
        }

        try writer.writeAll("_,};\n");
    }
}

const FeatureRequirements = struct {
    allocator: std.mem.Allocator,
    enumgroups: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(Registry.Enum)) = .{},
    free_enums: std.ArrayListUnmanaged(Registry.Enum) = .{},
    commands: std.ArrayListUnmanaged(Registry.Command) = .{},

    pub fn deinit(self: *@This()) void {
        {
            var it = self.enumgroups.valueIterator();
            while (it.next()) |v| {
                v.deinit(self.allocator);
            }
            self.enumgroups.deinit(self.allocator);
        }
        self.free_enums.deinit(self.allocator);
        self.commands.deinit(self.allocator);
    }
};

fn resolveFeatureRequirements(
    allocator: std.mem.Allocator,
    registry: *Registry,
    api: Registry.Feature.Api,
    version: ?std.SemanticVersion,
    core: bool,
    // extensions: []Registry.Extension,
) !FeatureRequirements {
    registry.sortFeatures();
    var requirements = std.ArrayList(Registry.Requirement).init(allocator);
    defer requirements.deinit();

    _ = core;

    // TODO: Respect core flag
    for (registry.getFeatureRange(api)) |feature| {
        if (version != null and feature.number.order(version.?) == .gt) {
            break;
        }
        try requirements.appendSlice(feature.require_set.items);
    }

    var enumgroups = std.StringHashMapUnmanaged(std.ArrayListUnmanaged(Registry.Enum)){};
    errdefer {
        var it = enumgroups.valueIterator();
        while (it.next()) |v| {
            v.deinit(allocator);
        }
        enumgroups.deinit(allocator);
    }
    var free_enums_found = std.StringHashMap(void).init(allocator);
    defer free_enums_found.deinit();

    var free_enums = std.ArrayListUnmanaged(Registry.Enum){};
    errdefer free_enums.deinit(allocator);

    var commands_found = std.StringHashMap(void).init(allocator);
    defer commands_found.deinit();

    var commands = std.ArrayListUnmanaged(Registry.Command){};
    errdefer commands.deinit(allocator);

    for (requirements.items) |req| switch (req) {
        .@"enum" => |ename| {
            // TODO: Pay attention to enum's alias attribute

            const e: Registry.Enum = registry.enums.get(ename) orelse unreachable;
            outer: for (e.groups.items) |egroup| {
                if (enumgroups.getPtr(egroup)) |group| {
                    for (group.items) |e2| {
                        if (std.mem.eql(u8, e2.name, ename)) {
                            continue :outer;
                        }
                    }
                    try group.append(allocator, e);
                } else {
                    try enumgroups.putNoClobber(allocator, egroup, std.ArrayListUnmanaged(Registry.Enum){});
                    try enumgroups.getPtr(egroup).?.append(allocator, e);
                }
            }
            if (e.groups.items.len == 0) {
                const result = try free_enums_found.getOrPut(e.name);
                if (!result.found_existing) {
                    try free_enums.append(allocator, e);
                }
            }
        },
        .command => |cname| {
            const command: Registry.Command = registry.commands.get(cname) orelse unreachable;
            const dedup_result = try commands_found.getOrPut(command.name);
            if (!dedup_result.found_existing) {
                try commands.append(allocator, command);
            }
        },
        .type => {},
    };

    return .{
        .allocator = allocator,
        .enumgroups = enumgroups,
        .free_enums = free_enums,
        .commands = commands,
    };
}

fn writeProcedureTable(
    registry: *Registry,
    commands: std.ArrayListUnmanaged(Registry.Command),
    writer: anytype,
) !void {
    try writer.writeAll("pub const ProcTable = struct {\n");

    for (commands.items) |command| {
        try writer.print("{s}: ?*", .{command.name});

        for (command.name) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }
        try writer.writeAll("PROC = null,\n");
    }
    for (commands.items) |command| {
        try writer.writeAll("pub const ");

        for (command.name) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }
        try writer.writeAll("PROC = ?*const fn(\n");

        const param_len = command.params.items.len;

        if (command.params.items.len > 0) {
            for (command.params.items[0 .. param_len - 1]) |param| {
                try writer.print("{s}: {s},", .{ param.name, param.type });
            }
            const last_param = command.params.items[param_len - 1];
            try writer.print("{s}: {s}", .{ last_param.name, last_param.type });
        }
        try writer.print(") callconv(.C) {s};", .{command.return_type});
    }

    try writer.writeAll("};\n");
    _ = registry;
}

const MODULE_TYPE_PREAMPLE =
    \\const std = @import("std");
    \\const builtin = std.builtin;
    \\
    \\pub const FunctionPointer: type = *align(@alignOf(fn (u32) callconv(.C) u32)) const anyopaque;
    \\
    \\pub const GLenum = c_uint;
    \\pub const GLboolean = u8;
    \\pub const GLbitfield = c_uint;
    \\pub const GLbyte = i8;
    \\pub const GLubyte = u8;
    \\pub const GLshort = i16;
    \\pub const GLushort = u16;
    \\pub const GLint = c_int;
    \\pub const GLuint = c_uint;
    \\pub const GLclampx = i32;
    \\pub const GLsizei = c_int;
    \\pub const GLfloat = f32;
    \\pub const GLclampf = f32;
    \\pub const GLdouble = f64;
    \\pub const GLclampd = f64;
    \\pub const GLeglClientBufferEXT = void;
    \\pub const GLeglImageOES = void;
    \\pub const GLchar = u8;
    \\pub const GLcharARB = u8;
    \\
    \\pub const GLhandleARB = if (builtin.os.tag == .macos) *anyopaque else c_uint;
    \\
    \\pub const GLhalf = u16;
    \\pub const GLhalfARB = u16;
    \\pub const GLfixed = i32;
    \\pub const GLintptr = usize;
    \\pub const GLintptrARB = usize;
    \\pub const GLsizeiptr = isize;
    \\pub const GLsizeiptrARB = isize;
    \\pub const GLint64 = i64;
    \\pub const GLint64EXT = i64;
    \\pub const GLuint64 = u64;
    \\pub const GLuint64EXT = u64;
    \\
    \\pub const GLsync = *opaque {};
    \\
    \\pub const _cl_context = opaque {};
    \\pub const _cl_event = opaque {};
    \\
    \\pub const GLDEBUGPROC = *const fn (source: GLenum, _type: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;
    \\pub const GLDEBUGPROCARB = *const fn (source: GLenum, _type: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;
    \\pub const GLDEBUGPROCKHR = *const fn (source: GLenum, _type: GLenum, id: GLuint, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;
    \\
    \\pub const GLDEBUGPROCAMD = *const fn (id: GLuint, category: GLenum, severity: GLenum, length: GLsizei, message: [*:0]const u8, userParam: ?*anyopaque) callconv(.C) void;
    \\
    \\pub const GLhalfNV = u16;
    \\pub const GLvdpauSurfaceNV = GLintptr;
    \\pub const GLVULKANPROCNV = *const fn () callconv(.C) void;
    \\
;

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
            var it2 = std.mem.splitScalar(u8, dirty_token, '*');
            while (it2.next()) |token| {
                if (token.len == 0) {
                    try tokens.append("*");
                    continue;
                }
                try tokens.append(token);
            }
        }
    }

    if (std.mem.eql(u8, tokens.items[0], "const")) {
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
        // const GLenum * => [*c]
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

pub fn generateModule(
    allocator: std.mem.Allocator,
    registry: *Registry,
    api: Registry.Feature.Api,
    version: ?std.SemanticVersion,
    core: bool,
    writer: anytype,
) !void {
    // Write type declarations
    try writer.writeAll(MODULE_TYPE_PREAMPLE);

    var requirements = try resolveFeatureRequirements(allocator, registry, api, version, core);
    defer requirements.deinit();

    {
        std.log.info("Generating {} enum groups", .{requirements.enumgroups.size});
        var it = requirements.enumgroups.iterator();
        while (it.next()) |entry| {
            const is_bitmask = registry.enumgroups.get(entry.key_ptr.*).?.bitmask;
            try writeEnumGroup(is_bitmask, entry.key_ptr.*, entry.value_ptr, writer);
        }
    }

    std.log.info("Generating {} freestanding enums", .{requirements.free_enums.items.len});
    for (requirements.free_enums.items) |e| {
        try writer.print("pub const {s}: GLenum = {};\n", .{ e.name, e.value });
    }

    std.log.info("Generating procedure table entries", .{});
    try writeProcedureTable(registry, requirements.commands, writer);

    // for (requirements.commands.items) |command| {
    //     std.debug.print("{s} {s}\n", .{ command.return_type, command.name });
    //     for (command.params.items) |param| {
    //         std.debug.print("  {s} {s}\n", .{ param.type, param.name });
    //     }
    // }
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
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLuint*");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "[*c]const GLuint", translated);
    }
    {
        const translated = try translateCType(testing.allocator, testing.allocator, "const GLubyte*");
        defer testing.allocator.free(translated);
        try testing.expectEqualSlices(u8, "?[*:0]const GLubyte", translated);
    }
}
