const std = @import("std");
const glregistry = @import("./glregistry.zig");
const dtd = @import("./dtd.zig");
const Registry = glregistry.Registry;
const FeatureKey = glregistry.FeatureKey;
const ExtensionKey = glregistry.ExtensionKey;

// https://www.khronos.org/opengl/wiki/OpenGL_Type
const MODULE_TYPE_PREAMBLE =
    \\const std = @import("std");
    \\const builtin = @import("builtin");
    \\
    \\pub const FunctionPointer: type = *align(@alignOf(fn (u32) callconv(.C) u32)) const anyopaque;
    \\
    \\pub const GLenum = u32;
    \\pub const GLboolean = u8;
    \\pub const GLbitfield = u32;
    \\pub const GLbyte = i8;
    \\pub const GLubyte = u8;
    \\pub const GLshort = i16;
    \\pub const GLushort = u16;
    \\pub const GLint = i32;
    \\pub const GLuint = u32;
    \\pub const GLclampx = i32;
    \\pub const GLsizei = i32;
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

pub fn writeFunctionParameterName(param: Registry.Command.Param, index: ?usize, writer: anytype) !void {

    // reserved keywords that OpenGL
    // uses as parameter names
    const keywords = std.StaticStringMap(void).initComptime(.{
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
    const is_reserved = keywords.has(param.name);

    if (is_reserved) {
        try writer.writeAll("@\"");
    }
    try writer.writeAll(param.name);
    if (index) |idx| {
        try writer.print("_{}", .{idx});
    }

    if (is_reserved) {
        try writer.writeByte('"');
    }
}

pub fn writeFunctionParameterType(param: Registry.Command.Param, writer: anytype) !void {
    if (param.group != null and std.mem.eql(u8, std.mem.trim(u8, param.type, " "), "GLenum")) {
        try writer.writeAll(param.group.?);
    } else {
        try writer.print("{s}", .{param.type});
    }
}

pub fn writeFunction(command: Registry.Command, writer: anytype) !void {
    const command_name = if (std.mem.startsWith(u8, command.name, "gl"))
        command.name[2..]
    else
        command.name;

    try writer.print(
        "pub fn {c}{s} (",
        .{ std.ascii.toLower(command_name[0]), command_name[1..] },
    );

    const param_len = command.params.items.len;
    for (command.params.items, 0..) |param, idx| {
        try writeFunctionParameterName(param, idx, writer);
        try writer.writeByte(':');
        try writeFunctionParameterType(param, writer);
        if (idx != param_len - 1) {
            try writer.writeByte(',');
        }
    }

    try writer.print(
        \\) callconv(.C) {s} {{
        \\    return @call(.always_tail, current_proc_table.?.{s}.?, .{{
    ,
        .{ command.return_type, command.name },
    );

    for (command.params.items, 0..) |param, idx| {
        try writeFunctionParameterName(param, idx, writer);
        if (idx != param_len - 1) {
            try writer.writeByte(',');
        }
    }

    try writer.writeAll("});\n}\n");
}

fn writeEnum(e: dtd.Enum, writer: std.io.AnyWriter) !void {
    const enum_type = switch (e.value_type) {
        .integer => "i32",
        .unsigned => "u32",
        .unsigned64 => "u64",
    };

    try writer.print(
        "pub const {s}: {s} = {s};",
        .{
            e.name,
            enum_type,
            e.value,
        },
    );
    var groups_it = std.mem.splitScalar(u8, e.groups, ',');
    if (groups_it.next()) |first| {
        try writer.writeAll("// groups: ");
        try writer.writeAll(first);
        while (groups_it.next()) |group| {
            try writer.writeByte(' ');
            try writer.writeAll(group);
        }
    }
    try writer.writeByte('\n');
}

fn writeFeatureApiIndex(
    allocator: std.mem.Allocator,
    feature: dtd.Feature,
    writer: std.io.AnyWriter,
) !void {
    try writer.print("pub const {0s}: ApiInfo = .{{\n .name = \"{0s}\",\n .commands = .{{ \n", .{feature.name});

    var dedup_set = std.StringHashMap(void).init(allocator);
    defer dedup_set.deinit();

    for (feature.require.items) |require| {
        for (require.interfaces.items) |interface| {
            if (interface != .command) {
                continue;
            }
            const cmd = interface.command;
            const getorput = try dedup_set.getOrPut(cmd);
            if (getorput.found_existing) {
                continue;
            }
            try writer.print(".{s} = true,\n", .{cmd});
        }
    }
    try writer.writeAll("},};\n");
    dedup_set.clearRetainingCapacity();

    // WARN: Assumption that features with <remove> are core profiles
    // TODO: Check if this assumption is correct for APIs other than GL
    if (feature.remove.items.len != 0) {
        try writer.print("pub const {0s}_CORE: ApiInfo = .{{\n .name = \"{0s}\",\n .commands = .{{ \n", .{feature.name});

        for (feature.require.items) |require| {
            for (require.interfaces.items) |interface| {
                if (interface != .command) {
                    continue;
                }
                const cmd = interface.command;
                const getorput = try dedup_set.getOrPut(cmd);
                if (getorput.found_existing) {
                    continue;
                }
                try writer.print(".{s} = true,\n", .{cmd});
            }
        }
        try writer.writeAll("},\n .remove_commands = .{ \n");
        dedup_set.clearRetainingCapacity();

        for (feature.remove.items) |remove| {
            for (remove.interfaces.items) |interface| {
                if (interface != .command) {
                    continue;
                }
                const cmd = interface.command;
                const getorput = try dedup_set.getOrPut(cmd);
                if (getorput.found_existing) {
                    continue;
                }
                try writer.print(".{s} = true,\n", .{cmd});
            }
        }

        try writer.writeAll("},};\n");
    }
}

fn writeExtensionApiIndex(
    ext: dtd.Extension,
    writer: std.io.AnyWriter,
) !void {
    try writer.print("pub const {0s}: ApiInfo = .{{\n .name = \"{0s}\",\n .commands = .{{ \n", .{ext.name});
    for (ext.require.items) |require| {
        if (require.profile.len != 0) {
            std.log.warn("TODO: not every command is being exported!!!", .{});
            continue;
        }
        for (require.interfaces.items) |interface| {
            if (interface != .command) {
                continue;
            }
            try writer.print(".{s} = true,", .{interface.command});
        }
    }

    if (ext.remove.items.len != 0) {
        std.debug.panic("extension remove requirements are unsupported (by {s})", .{ext.name});
    }
    try writer.writeAll("},};\n");
}

fn writeApiIndex(
    allocator: std.mem.Allocator,
    registry: *const Registry,
    writer: std.io.AnyWriter,
) !void {
    try writer.writeAll(
        \\ const ApiInfo = struct {
        \\     name: [:0]const u8,
        \\     commands: CommandFlags,
        \\     remove_commands: CommandFlags = .{},
        \\ };
        \\
        \\ pub const apis = struct {
        \\
    );

    for (registry.features.items) |feature| {
        try writeFeatureApiIndex(allocator, feature, writer);
    }

    {
        var it = registry.extensions.valueIterator();
        while (it.next()) |ext| {
            try writeExtensionApiIndex(ext.*, writer);
        }
    }

    try writer.writeAll(
        \\ };
        \\
    );
}

fn writeCommandFlagsStruct(
    registry: *Registry,
    writer: std.io.AnyWriter,
) !void {
    try writer.writeAll(
        \\
        \\ const CommandFlags = struct {
        \\
    );

    var it = registry.commands.valueIterator();
    while (it.next()) |entry| {
        try writer.print("{s}: bool = false, \n", .{entry.name});
    }

    try writer.writeAll(
        \\    pub const Enum = blk: {
        \\        @setEvalBranchQuota(100_000);
        \\        break :blk std.meta.FieldEnum(@This());
        \\    };
        \\ };
    );
}

fn writeCommandPfns(
    registry: *Registry,
    writer: std.io.AnyWriter,
) !void {
    var it = registry.commands.valueIterator();
    while (it.next()) |cmd| {
        const first_uppercase = for (cmd.name, 0..) |c, idx| {
            if (std.ascii.isUpper(c)) break idx;
        } else return error.BadGlCommandName;

        try writer.print("pub const Pfn{s} = *const fn(", .{cmd.name[first_uppercase..]});
        for (cmd.params.items, 0..) |param, idx| {
            try writer.print("@\"{s}\": GLint", .{param.name});
            if (idx + 1 != cmd.params.items.len) {
                try writer.writeAll(", ");
            }
        }

        try writer.print(") callconv(.C) GLint;\n", .{});
    }
    try writer.writeAll(
        \\
        \\fn CommandPfn(comptime cmd: CommandFlags.Enum) type {
        \\    return switch(cmd) {
    );

    it = registry.commands.valueIterator();
    while (it.next()) |cmd| {
        const first_uppercase = for (cmd.name, 0..) |c, idx| {
            if (std.ascii.isUpper(c)) break idx;
        } else return error.BadGlCommandName;

        try writer.print(".{s} => Pfn{s},\n", .{ cmd.name, cmd.name[first_uppercase..] });
    }

    try writer.writeAll(
        \\  };
        \\}
    );
}

/// Given a registry
/// generates a bindings module and writes it to `writer`
pub fn generateModule(
    allocator: std.mem.Allocator,
    registry: *Registry,
    writer: anytype,
) !void {
    try writer.writeAll(MODULE_TYPE_PREAMBLE);

    {
        var it = registry.enums.valueIterator();
        while (it.next()) |e| {
            try writeEnum(e.*, writer.any());
        }
    }

    try writeCommandPfns(registry, writer.any());

    try writeApiIndex(allocator, registry, writer.any());

    try writeCommandFlagsStruct(registry, writer.any());

    try writer.writeAll(
        \\ pub fn DispatchTable(comptime api_list: []const ApiInfo) type {
        \\     // Sort in terms of standard first then extenstions.
        \\     // Standard API must be applied in version order.
        \\     @setEvalBranchQuota(1_000_000);
        \\     const sorted_apis = comptime blk: {
        \\         var standard_count: usize = 0;
        \\         var result: [api_list.len]ApiInfo = undefined;
        \\         var end_idx: usize = api_list.len;
        \\         for (api_list) |api| {
        \\             if (std.mem.indexOfPosLinear(u8, api.name, 0, "_VERSION_") == null) {
        \\                 end_idx -= 1;
        \\                 result[end_idx] = api;
        \\                 continue;
        \\             }
        \\             result[standard_count] = api;
        \\             standard_count += 1;
        \\         }
        \\         std.mem.sortUnstable(
        \\             ApiInfo,
        \\             result[0..standard_count],
        \\             {},
        \\             struct {
        \\                 pub fn lessThan(_: void, lhs: ApiInfo, rhs: ApiInfo) bool {
        \\                     return std.mem.orderZ(u8, lhs.name, rhs.name) == .lt;
        \\                 }
        \\             }.lessThan,
        \\         );
        \\         break :blk result;
        \\     };
        \\
        \\     comptime var field_count: usize = 0;
        \\     var cmds: CommandFlags = .{};
        \\     inline for (sorted_apis) |api| {
        \\         inline for (std.meta.fields(CommandFlags)) |field| {
        \\             const merge = @field(cmds, field.name) or @field(api.commands, field.name);
        \\             @field(cmds, field.name) =
        \\                 !@field(api.remove_commands, field.name) and merge;
        \\         }
        \\     }
        \\
        \\     inline for (std.meta.fields(CommandFlags)) |field| {
        \\         field_count += @intFromBool(@field(cmds, field.name));
        \\     }
        \\
        \\     const StructField = std.builtin.Type.StructField;
        \\     comptime var fields: [field_count]StructField = undefined;
        \\     comptime var field_idx: usize = 0;
        \\
        \\     inline for (std.meta.fields(CommandFlags)) |field| {
        \\         if (!@field(cmds, field.name)) continue;
        \\         const cmd_enum = @field(CommandFlags.Enum, field.name);
        \\
        \\         const T = ?CommandPfn(cmd_enum);
        \\         fields[field_idx] = .{
        \\             .name = field.name,
        \\             .type = T,
        \\             .default_value = @ptrCast(&@as(T, null)),
        \\             .is_comptime = false,
        \\             .alignment = @alignOf(T),
        \\         };
        \\         field_idx += 1;
        \\     }
        \\     const Mixin = struct {
        \\         // pub fn load(getProcAddress: *const fn () void) @This() {}
        \\     };
        \\     var type_info = @typeInfo(Mixin);
        \\     type_info.Struct.fields = type_info.Struct.fields ++ fields;
        \\     return @Type(type_info);
        \\ }
    );
}
