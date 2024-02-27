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
    };

    const Requirement = union(enum) {
        @"enum": []const u8,
        command: []const u8,
        type: []const u8,

        fn name(self: @This()) []const u8 {
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
    pub fn getFeature(self: @This(), feature: FeatureRef) ?Feature {
        for (self.features.items) |feat| {
            if (feat.api == feature.api and feat.number.order(feature) == .eq) {
                return feat;
            }
        }
        return null;
    }
};

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

/// Returns a number of bit shifts it takes for either `a` or `b` be equal to eachother.
/// Expects that `a` and `b` are single bit bitmasks.
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

const ModuleRequirements = struct {
    allocator: std.mem.Allocator,
    enumgroups: std.StringHashMapUnmanaged(void) = .{},
    enums: std.ArrayListUnmanaged(Registry.Enum) = .{},
    commands: std.ArrayListUnmanaged(Registry.Command) = .{},

    pub fn deinit(self: *@This()) void {
        self.enumgroups.deinit(self.allocator);
        self.enums.deinit(self.allocator);
        self.commands.deinit(self.allocator);
    }
};

fn resolveRequirements(
    allocator: std.mem.Allocator,
    registry: *Registry,
    requirement_set: RequirementSet,
) !ModuleRequirements {
    var result = ModuleRequirements{ .allocator = allocator };
    errdefer result.deinit();

    var req_it = requirement_set.valueIterator();
    while (req_it.next()) |req| switch (req.*) {
        .@"enum" => |ename| {
            const e: Registry.Enum = registry.enums.get(ename) orelse unreachable;

            for (e.groups.items) |egroup_name| {
                const seen: bool = result.enumgroups.get(egroup_name) != null;
                if (seen) {
                    continue;
                }
                try result.enumgroups.put(allocator, egroup_name, {});
            }

            try result.enums.append(allocator, e);
        },
        .command => |cname| {
            const command: Registry.Command = registry.commands.get(cname) orelse unreachable;
            for (command.params.items) |param| {
                if (param.group) |egroup_name| {
                    const seen: bool = result.enumgroups.get(egroup_name) != null;

                    if (seen) {
                        continue;
                    }
                    try result.enumgroups.put(allocator, egroup_name, {});
                }
            }
            try result.commands.append(allocator, command);
        },
        .type => {},
    };

    return result;
}

fn writeProcedureTable(
    registry: *const Registry,
    commands: std.ArrayListUnmanaged(Registry.Command),
    writer: anytype,
) !void {
    try writer.writeAll("pub const ProcTable = struct {\n");

    for (commands.items) |command| {
        try writer.print("{s}: ", .{command.name});

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
    \\pub const GETPROCADDRESSPROC = *const fn(procname: [*:0]const u8) callconv(.C) ?*const fn() callconv(.C) void;
;

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

pub fn writeFunctionParameterName(param: Registry.Command.Param, writer: anytype) !void {

    // reserved keywords that OpenGL
    // uses as parameter names
    const keywords = std.ComptimeStringMap(void, .{
        &.{"type"},
        &.{"u1"},
        &.{"u2"},
        &.{"u3"},
        &.{"u4"},
        &.{"u5"},
        &.{"u6"},
        &.{"u7"},
        &.{"u8"},
        &.{"u9"},
        &.{"u10"},
        &.{"i1"},
        &.{"i2"},
        &.{"i3"},
        &.{"i4"},
        &.{"i5"},
        &.{"i6"},
        &.{"i7"},
        &.{"i8"},
        &.{"i9"},
        &.{"i10"},
    });

    if (keywords.has(param.name)) {
        try writer.print("@\"{s}\"", .{param.name});
    } else {
        try writer.writeAll(param.name);
    }
}

const FeatureRef = struct {
    api: Registry.Feature.Api,
    number: std.SemanticVersion,
};

const RequirementSet = std.StringHashMap(Registry.Requirement);

/// Obtains a set of requirements needed to generate
/// a module up to a given feature.
/// The returned value is owned by the caller.
fn getRequirementSet(
    allocator: std.mem.Allocator,
    registry: *Registry,
    feature_ref: FeatureRef,
    extensions: []const []const u8,
) !RequirementSet {
    var requirement_set = RequirementSet.init(allocator);
    errdefer requirement_set.deinit();

    const api_ref: Registry.Feature.Api = if (feature_ref.api == .glcore) .gl else feature_ref.api;

    const feature_range = registry.getFeatureRange(api_ref);
    for (feature_range) |feature| {
        if (feature.number.order(feature_ref.number) == .gt) {
            break;
        }
        for (feature.require_set.items) |req| {
            const getorput_res = try requirement_set.getOrPut(req.name());
            if (getorput_res.found_existing) {
                continue;
            }
            getorput_res.value_ptr.* = req;
        }
        if (feature_ref.api == .glcore and feature.api == .gl) for (feature.remove_set.items) |req| {
            _ = requirement_set.remove(req.name());
        };
    }
    for (extensions) |extname| {
        if (registry.extensions.get(extname)) |extension| {

            // skip if unsupported
            if (std.mem.indexOfScalar(Registry.Feature.Api, extension.supported_api.slice(), feature_ref.api) == null) {
                continue;
            }
            for (extension.require_set.items) |req_ref| {
                if (req_ref.api != null and req_ref.api != feature_ref.api) {
                    continue;
                }
                const req = req_ref.requirement;
                const getorput_res = try requirement_set.getOrPut(req.name());
                if (getorput_res.found_existing) {
                    continue;
                }
                getorput_res.value_ptr.* = req;
            }
        } else {
            std.log.warn("Extension '{s}' not found! Skipping!", .{extname});
        }
    }
    return requirement_set;
}

pub fn writeFunction(command: Registry.Command, writer: anytype) !void {
    try writer.print("pub fn {s} (", .{command.name});

    const param_len = command.params.items.len;
    if (param_len > 0) {
        for (command.params.items[0 .. param_len - 1]) |param| {
            try writeFunctionParameterName(param, writer);
            try writer.writeByte(':');
            if (param.group) |param_group| {
                try writer.writeAll(param_group);
            } else {
                try writer.print("{s}", .{param.type});
            }
            try writer.writeByte(',');
        }

        const last_param = command.params.items[param_len - 1];
        try writeFunctionParameterName(last_param, writer);
        try writer.writeByte(':');
        if (last_param.group) |param_group| {
            try writer.writeAll(param_group);
        } else {
            try writer.print("{s}", .{last_param.type});
        }
    }

    try writer.print(") callconv(.C) {s} {{\nreturn @call(.always_tail, current_proc_table.?.{s}.?, .{{", .{ command.return_type, command.name });

    if (param_len > 0) {
        for (command.params.items[0 .. param_len - 1]) |param| {
            try writeFunctionParameterName(param, writer);
            try writer.writeByte(',');
        }

        const last_param = command.params.items[param_len - 1];
        try writeFunctionParameterName(last_param, writer);
    }
    try writer.writeAll("});\n}\n");
}

pub fn writeFeatureLoaderFunction(feature_ref: FeatureRef, requirements: ModuleRequirements, writer: anytype) !void {
    try writer.writeAll(
        \\threadlocal var current_proc_table: ?ProcTable = null;
        \\
        \\pub fn load
    );

    for (@tagName(feature_ref.api)) |c| {
        try writer.writeByte(std.ascii.toUpper(c));
    }

    try writer.writeAll(
        \\(getProcAddress: GETPROCADDRESSPROC) !ProcTable {
        \\  return ProcTable {
    );

    for (requirements.commands.items) |command| {
        try writer.print(".{0s} = @ptrCast(getProcAddress(\"{0s}\") orelse return error.LoadError),\n", .{command.name});
    }

    try writer.writeAll(
        \\  };
        \\}
    );
}

fn writeEnum(@"enum": Registry.Enum, writer: anytype) !void {
    const e = @"enum";

    try writer.print(
        "pub const {s}: {s} = 0x{X};",
        .{
            e.name,
            if (e.groups.items.len == 1) e.groups.items[0] else "GLenum",
            e.value,
        },
    );
    // Add a comment to make it at least searchable.
    if (e.groups.items.len > 1) {
        try writer.writeAll("// groups:");
        for (e.groups.items) |egroup_name| {
            try writer.writeByte(' ');
            try writer.writeAll(egroup_name);
        }
    }
    try writer.writeByte('\n');
}

/// Given a feature and a registry
/// generates a bindings module and writes it to `writer`
pub fn generateModule(
    allocator: std.mem.Allocator,
    registry: *Registry,
    feature_ref: FeatureRef,
    extensions: []const []const u8,
    writer: anytype,
) !void {
    var requirement_set = try getRequirementSet(allocator, registry, feature_ref, extensions);
    defer requirement_set.deinit();

    var requirements = try resolveRequirements(allocator, registry, requirement_set);
    defer requirements.deinit();

    // Write type declarations
    try writer.writeAll(MODULE_TYPE_PREAMPLE);

    {
        var kit = requirements.enumgroups.keyIterator();
        while (kit.next()) |egroup| {
            try writer.print("pub const {s} = GLenum;\n", .{egroup.*});
        }
    }

    for (requirements.enums.items) |e| {
        try writeEnum(e, writer);
    }

    try writeProcedureTable(registry, requirements.commands, writer);

    for (requirements.commands.items) |command| {
        try writeFunction(command, writer);
    }

    try writeFeatureLoaderFunction(feature_ref, requirements, writer);

    try writer.writeAll(
        \\pub fn makeProcTableCurrent(proc_table: ?ProcTable) void {
        \\      current_proc_table = proc_table;
        \\}
        \\pub fn getProcTablePtr() ?*const ProcTable {
        \\      return &(current_proc_table orelse return null);
        \\}
        \\
        \\/// Returns `true` if extension `name` is supported
        \\/// by current GL context
        \\pub fn extensionSupportedGL(name: []const u8) bool {
        \\    // glGetString fails only when it's parameter names an invalid string
        \\    // here it will never fail unless GL context is broken.
        \\    const ext_str = glGetString(GL_EXTENSIONS) orelse std.debug.panic("glGetString(GL_EXTENSIONS) failed", .{});
        \\    return std.mem.indexOf(u8, std.mem.span(ext_str), name);
        \\}
    );
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
