const std = @import("std");
const glregistry = @import("./glregistry.zig");
const Registry = glregistry.Registry;
const FeatureKey = glregistry.FeatureKey;
const ExtensionKey = glregistry.ExtensionKey;

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
    commands: std.ArrayListUnmanaged(CommandInfo) = .{},

    const CommandInfo = struct {
        command: Registry.Command,
        feature: ?FeatureKey = null,
        ext: ?ExtensionKey = null,
    };

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
    while (req_it.next()) |req_ref| switch (req_ref.req) {
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
            try result.commands.append(allocator, .{
                .command = command,
                .feature = req_ref.feature_ref,
                .ext = req_ref.ext_ref,
            });
        },
        .type => {},
    };

    return result;
}

fn writeProcedureTable(
    // registry: *const Registry,
    command_refs: std.ArrayListUnmanaged(ModuleRequirements.CommandInfo),
    writer: anytype,
) !void {
    try writer.writeAll("pub const ProcTable = struct {\n");

    for (command_refs.items) |command_ref| {
        const command = command_ref.command;
        try writer.print("{s}: ", .{command.name});

        for (command.name) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }
        try writer.writeAll("PROC = null,\n");
    }
    for (command_refs.items) |command_ref| {
        const command = command_ref.command;
        const params = command.params;
        try writer.writeAll("pub const ");

        for (command.name) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }
        try writer.writeAll("PROC = ?*const fn(\n");

        const param_len = params.items.len;

        if (params.items.len > 0) {
            for (params.items[0 .. param_len - 1]) |param| {
                try writer.print("{s}: {s},", .{ param.name, param.type });
            }
            const last_param = params.items[param_len - 1];
            try writer.print("{s}: {s}", .{ last_param.name, last_param.type });
        }
        try writer.print(") callconv(.C) {s};", .{command.return_type});
    }

    try writer.writeAll("};\n");
    // _ = registry;
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

pub fn writeFunctionParameterName(param: Registry.Command.Param, index: ?usize, writer: anytype) !void {

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

const RequirementInfo = struct {
    req: Registry.Requirement,
    feature_ref: ?FeatureKey = null,
    ext_ref: ?ExtensionKey = null,
};

const RequirementSet = std.StringHashMap(RequirementInfo);

/// Obtains a set of requirements needed to generate
/// a module up to a given feature.
/// The returned value is owned by the caller.
fn getRequirementSet(
    allocator: std.mem.Allocator,
    registry: *Registry,
    feature_ref: FeatureKey,
    extensions: []const []const u8,
) !RequirementSet {
    var requirement_set = RequirementSet.init(allocator);
    errdefer requirement_set.deinit();

    const api_ref: Registry.Feature.Api = if (feature_ref.api == .glcore) .gl else feature_ref.api;

    const feature_range = registry.getFeatureRange(api_ref) orelse return error.FeatureNotFound;
    for (feature_range) |feature| {
        if (feature.number.order(feature_ref.number) == .gt) {
            break;
        }
        for (feature.require_set.items) |req| {
            const getorput_res = try requirement_set.getOrPut(req.name());
            if (getorput_res.found_existing) {
                continue;
            }
            getorput_res.value_ptr.* = .{
                .req = req,
                .feature_ref = .{ .api = feature.api, .number = feature.number },
            };
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
                getorput_res.value_ptr.* = .{
                    .req = req,
                    .ext_ref = extname,
                };
            }
        } else {
            std.log.warn("Extension '{s}' not found! Skipping!", .{extname});
        }
    }
    return requirement_set;
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
    if (param_len > 0) {
        for (command.params.items[0 .. param_len - 1], 0..) |param, idx| {
            try writeFunctionParameterName(param, idx, writer);
            try writer.writeByte(':');
            if (param.group) |param_group| {
                try writer.writeAll(param_group);
            } else {
                try writer.print("{s}", .{param.type});
            }
            try writer.writeByte(',');
        }

        const last_param = command.params.items[param_len - 1];
        try writeFunctionParameterName(last_param, param_len - 1, writer);
        try writer.writeByte(':');
        if (last_param.group) |param_group| {
            try writer.writeAll(param_group);
        } else {
            try writer.print("{s}", .{last_param.type});
        }
    }

    try writer.print(
        \\) callconv(.C) {s} {{
        \\    return @call(.always_tail, current_proc_table.?.{s}.?, .{{
    ,
        .{ command.return_type, command.name },
    );

    if (param_len > 0) {
        for (command.params.items[0 .. param_len - 1], 0..) |param, idx| {
            try writeFunctionParameterName(param, idx, writer);
            try writer.writeByte(',');
        }

        const last_param = command.params.items[param_len - 1];
        try writeFunctionParameterName(last_param, param_len - 1, writer);
    }
    try writer.writeAll("});\n}\n");
}

pub fn writeFeatureLoaderFunction(feature: FeatureKey, requirements: ModuleRequirements, writer: anytype) !void {
    try writer.writeAll(
        \\pub fn load
    );

    for (@tagName(feature.api)) |c| {
        try writer.writeByte(std.ascii.toUpper(c));
    }
    try writer.print("{}{}", .{ feature.number.major, feature.number.minor });

    try writer.writeAll(
        \\(proc_table: *ProcTable, getProcAddress: GETPROCADDRESSPROC) !void {
    );

    for (requirements.commands.items) |command_info| {
        const cmd_feature = command_info.feature orelse continue;
        const feature_order = cmd_feature.number.order(feature.number);

        if (cmd_feature.api != feature.api or feature_order != .eq) {
            continue;
        }
        try writer.print(
            "proc_table.{0s} = @ptrCast(getProcAddress(\"{0s}\") orelse return error.LoadError);\n",
            .{command_info.command.name},
        );
    }

    try writer.writeAll(
        \\}
        \\
    );
}
pub fn writeExtensionLoaderFunction(ext: ExtensionKey, requirements: ModuleRequirements, writer: anytype) !void {
    try writer.writeAll(
        \\pub fn load
    );

    for (ext) |c| {
        try writer.writeByte(std.ascii.toUpper(c));
    }

    try writer.writeAll(
        \\(proc_table: *ProcTable, getProcAddress: GETPROCADDRESSPROC) !void {
    );

    for (requirements.commands.items) |command_info| {
        const cmd_ext = command_info.ext orelse continue;

        if (!std.mem.eql(u8, cmd_ext, ext)) {
            continue;
        }

        try writer.print(
            "proc_table.{0s} = @ptrCast(getProcAddress(\"{0s}\") orelse return error.LoadError);\n",
            .{command_info.command.name},
        );
    }

    try writer.writeAll(
        \\}
        \\
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
    feature_ref: FeatureKey,
    extensions: []const []const u8,
    writer: anytype,
) !void {
    var requirement_set = try getRequirementSet(allocator, registry, feature_ref, extensions);
    defer requirement_set.deinit();

    var requirements = try resolveRequirements(allocator, registry, requirement_set);
    defer requirements.deinit();

    std.log.info("Generating {} enums, {} commands", .{ requirements.enums.items.len, requirements.commands.items.len });

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

    try writeProcedureTable(requirements.commands, writer);

    for (requirements.commands.items) |command_ref| {
        try writeFunction(command_ref.command, writer);
    }

    try writer.writeAll(
        \\threadlocal var current_proc_table: ?ProcTable = null;
        \\
    );

    const feature_range = registry.getFeatureRange(feature_ref.api) orelse return error.FeatureNotFound;
    for (feature_range) |feature| {
        if (feature.number.order(feature_ref.number) == .gt) {
            continue;
        }
        try writeFeatureLoaderFunction(feature.asKey(), requirements, writer);
    }

    try writer.writeAll(
        \\pub fn load
    );

    for (@tagName(feature_ref.api)) |c| {
        try writer.writeByte(std.ascii.toUpper(c));
    }

    try writer.writeAll(
        \\(getProcAddress: GETPROCADDRESSPROC) !ProcTable {
        \\    var table = ProcTable{};
    );

    for (feature_range) |feature| {
        if (feature.number.order(feature_ref.number) == .gt) {
            continue;
        }
        const tagname = @tagName(feature.api);

        try writer.writeAll(
            \\    load
        );
        for (tagname) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }

        try writer.print("{}{}(&table,getProcAddress) catch return error.", .{ feature.number.major, feature.number.minor });

        for (tagname) |c| {
            try writer.writeByte(std.ascii.toUpper(c));
        }
        try writer.print("{}{}LoadFailed;\n", .{ feature.number.major, feature.number.minor });
    }

    try writer.writeAll(
        \\    return table;
        \\}
    );

    for (extensions) |ext| {
        _ = registry.extensions.get(ext) orelse {
            std.log.warn("Extension {s} not found in registry! Cannot write loader function!", .{ext});
            return error.ExtensionNotFound;
        };
        try writeExtensionLoaderFunction(ext, requirements, writer);
    }

    try writer.writeAll(
        \\pub fn makeProcTableCurrent(proc_table: ?ProcTable) void {
        \\      current_proc_table = proc_table;
        \\}
        \\pub fn getProcTablePtr() ?*ProcTable {
        \\      return &(current_proc_table orelse return null);
        \\}
        \\
        \\/// Returns `true` if extension `name` is supported
        \\/// by current GL context
        \\pub fn extensionSupportedGL(name: []const u8) bool {
        // TODO: Cache GL_EXTENSIONS string
        \\    // glGetString fails only when it's parameter names an invalid string
        \\    // here it will never fail unless GL context is broken.
        \\    const ext_str = getString(GL_EXTENSIONS) orelse std.debug.panic("glGetString(GL_EXTENSIONS) failed", .{});
        \\    return std.mem.indexOf(u8, std.mem.span(ext_str), name) != null;
        \\}
    );
}
