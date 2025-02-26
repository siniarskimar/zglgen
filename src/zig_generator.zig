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
    try writer.print(
        \\ pub const {0s} = .{{
        \\   .name = "{0s}",
        \\   .is_feature = true,
        \\   .commands = .{{
        ++ "\n",
        .{feature.name},
    );

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

    // GL 3.2 is the only profile using <remove>
    // Its used to remove compat commands
    if (feature.remove.items.len != 0) {
        try writer.print(
            \\ pub const {0s}_CORE = .{{
            \\   .name = "{0s}",
            \\   .is_feature = true,
            \\   .commands = .{{
            ++ "\n",
            .{feature.name},
        );

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
                try writer.print(
                    \\ .{s} = true,
                ,
                    .{cmd},
                );
            }
        }
        try writer.writeAll(
            \\   },
            \\   .remove_commands = .{
        ++ "\n");

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

        try writer.writeAll(
            \\ },};
        ++ "\n");
    }
}

fn writeExtensionApiIndex(
    ext: dtd.Extension,
    writer: std.io.AnyWriter,
) !void {
    try writer.print(
        \\ pub const {0s} = .{{
        \\    .name = "{0s}",
        \\    .commands = .{{
    , .{ext.name});

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
    // const ApiInfo = struct {
    //     name: [:0]const u8,
    //     commands: CommandFlags,
    //     remove_commands: CommandFlags = .{},
    //     is_feature: bool = false,
    // };

    try writer.writeAll(
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
    // packed struct is not worth as it will slow down compilation
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
        \\
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
        \\pub const command_pfns_map = struct {
        \\
    );

    it = registry.commands.valueIterator();
    while (it.next()) |cmd| {
        const first_uppercase = for (cmd.name, 0..) |c, idx| {
            if (std.ascii.isUpper(c)) break idx;
        } else return error.BadGlCommandName;

        try writer.print("pub const {s} = Pfn{s};\n", .{ cmd.name, cmd.name[first_uppercase..] });
    }

    try writer.writeAll(
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

    try writer.writeAll(
        \\
        \\fn sortApiList(comptime api_list: anytype) type {
        \\    const Struct = std.builtin.Type.Struct;
        \\    var feature_count: usize = 0;
        \\    var result: [api_list.len]Struct = undefined;
        \\    var end_idx: usize = api_list.len;
        \\    for (api_list) |api| {
        \\        if (@hasField(@TypeOf(api), "is_feature") and api.is_feature) {
        \\            result[feature_count] = api;
        \\            feature_count += 1;
        \\            continue;
        \\        }
        \\        end_idx -= 1;
        \\        result[end_idx] = api;
        \\    }
        \\    std.mem.sortUnstable(
        \\        Struct,
        \\        result[0..feature_count],
        \\        {},
        \\        struct {
        \\            pub fn lessThan(_: void, lhs: Struct, rhs: Struct) bool {
        \\                return std.mem.orderZ(u8, lhs.name, rhs.name) == .lt;
        \\            }
        \\        }.lessThan,
        \\    );
        \\    return result;
        \\}
        \\
        \\fn CommandFlags(comptime api_list: anytype) type {
        \\    const StructField = std.builtin.Type.StructField;
        \\
        \\    var cmd_names: []const [:0]const u8 = std.meta.fieldNames(@TypeOf(api_list[0].commands));
        \\    inline for (1..api_list.len) |idx| {
        \\        cmd_names = cmd_names ++ std.meta.fieldNames(@TypeOf(api_list[idx].commands));
        \\    }
        \\
        \\    var cmds: [cmd_names.len]StructField = undefined;
        \\    for (cmd_names, 0..) |name, idx| {
        \\        cmds[idx] = .{
        \\            .name = name,
        \\            .type = bool,
        \\            .default_value = @ptrCast(&@as(bool, false)),
        \\            .is_comptime = false,
        \\            .alignment = @alignOf(bool),
        \\        };
        \\    }
        // TODO: Dedup commands
        \\
        \\    return @Type(.{ .Struct = .{
        \\        .layout = .auto,
        \\        .fields = &cmds,
        \\        .decls = &.{},
        \\        .is_tuple = false,
        \\    } });
        \\}
        \\
        \\pub fn DispatchTable(comptime api_list: anytype) type {
        \\    const ApiList = @TypeOf(api_list);
        \\    const apilist_typeinfo = @typeInfo(ApiList);
        \\    if (apilist_typeinfo != .Struct or !apilist_typeinfo.Struct.is_tuple) {
        \\        @compileError("api list must be a tuple");
        \\    }
        \\    // Sort in terms of standard first then extenstions.
        \\    // Standard API must be applied in version order.
        \\    return struct {
        \\        pfns: Pfns,
        \\
        \\        const Pfns = blk: {
        \\            @setEvalBranchQuota(1_000_000);
        \\            // const sorted_apis = sortApiList(api_list);
        \\
        \\            var field_count: usize = 0;
        \\            const CmdFlags = CommandFlags(api_list);
        \\            var cmds: CmdFlags = .{};
        \\
        \\            for (api_list) |api| {
        \\                for (std.meta.fields(CmdFlags)) |field| {
        \\                    const merge = @field(cmds, field.name) or (@hasField(@TypeOf(api.commands), field.name) and @field(api.commands, field.name));
        \\                    const remove = @hasField(@TypeOf(api), "remove_commands") and @field(api.remove_commands, field.name);
        \\                    @field(cmds, field.name) = !remove and merge;
        \\                }
        \\            }
        \\
        \\            for (std.meta.fields(CmdFlags)) |field| {
        \\                field_count += @intFromBool(@field(cmds, field.name));
        \\            }
        \\
        \\            const StructField = std.builtin.Type.StructField;
        \\            var fields: [field_count]StructField = undefined;
        \\            var field_idx: usize = 0;
        \\
        \\            for (std.meta.fields(CmdFlags)) |field| {
        \\                if (!@field(cmds, field.name)) continue;
        \\
        \\                const T = @field(command_pfns_map, field.name);
        \\                fields[field_idx] = .{
        \\                    .name = field.name,
        \\                    .type = T,
        \\                    .default_value = null,
        \\                    .is_comptime = false,
        \\                    .alignment = @alignOf(T),
        \\                };
        \\                field_idx += 1;
        \\            }
        \\            break :blk @Type(.{ .Struct = .{
        \\                .layout = .auto,
        \\                .fields = &fields,
        \\                .decls = &.{},
        \\                .is_tuple = false,
        \\            } });
        \\        };
        \\
        \\        pub fn load(getProcAddress: GETPROCADDRESSPROC) error{CommandLoadFail}!@This() {
        \\            var result: @This() = undefined;
        \\            inline for (std.meta.fields(Pfns)) |field| {
        \\                @field(result.pfns, field.name) = @ptrCast(getProcAddress(field.name) orelse return error.CommandLoadFail);
        \\            }
        \\            return result;
        \\        }
        \\    };
        \\}
        \\
    );
}
