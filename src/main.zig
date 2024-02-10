const std = @import("std");
const clap = @import("clap");
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
    arena: std.heap.ArenaAllocator,
    enumgroups: std.StringHashMapUnmanaged(EnumGroup) = .{},
    enums: std.StringHashMapUnmanaged(Enum) = .{},
    commands: std.StringHashMapUnmanaged(Command) = .{},
    types: std.StringHashMapUnmanaged(Type) = .{},
    extensions: std.StringHashMapUnmanaged(void) = .{},

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
            group: ?[]const u8,
        };
    };

    const Type = struct {
        alias: []const u8,
        original: []const u8,
    };

    const Feature = struct {
        name: []const u8,
        number: std.SemanticVersion,
    };

    pub fn init(allocator: std.mem.Allocator) @This() {
        return .{
            .allocator = allocator,
            .arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
    }

    pub fn deinit(self: *@This()) void {
        self.arena.deinit();
    }
};

fn extractCommand(registry: *Registry, tree: *xml.XmlTree) !void {
    const arena_allocator = registry.arena.allocator();

    const proto = tree.root.?.findElement("proto") orelse
        return error.CommandWithoutProto;

    const return_type_elem = proto.findElement("ptype");

    const name_elem = proto.findElement("name") orelse
        return error.CommandWithoutName;

    var buffer = std.ArrayList(u8).init(arena_allocator);
    defer buffer.deinit();

    try name_elem.collectText(buffer.writer());

    const cmd_name = try buffer.toOwnedSlice();

    if (return_type_elem) |elem| {
        try elem.collectText(buffer.writer());
    } else {
        try buffer.appendSlice("void");
    }

    const return_type = try buffer.toOwnedSlice();

    var cmd = Registry.Command{
        .name = cmd_name,
        .return_type = return_type,
    };

    var param_it = tree.root.?.findElements("param");
    while (param_it.next()) |param_elem| {
        const param_group = param_elem.attributes.get("group");
        _ = param_group;
        const param_name_elem = param_elem.findElement("name") orelse
            return error.CommandParamWithoutName;

        try param_name_elem.collectText(buffer.writer());
        const param_name = try buffer.toOwnedSlice();

        if (param_elem.findElement("ptype")) |param_type| {
            try param_type.collectText(buffer.writer());
        } else {
            try param_elem.collectTextBefore(param_name_elem, buffer.writer());
        }

        const param_type = try buffer.toOwnedSlice();

        try cmd.params.append(arena_allocator, .{
            .name = param_name,
            .type = param_type,
            .group = null,
        });
    }

    try registry.commands.putNoClobber(arena_allocator, cmd.name, cmd);
}

pub fn extractEnum(registry: *Registry, tag: *xml.XmlTag.StartTag) !void {
    const arena_allocator = registry.arena.allocator();

    const value = tag.attributes.get("value") orelse return error.EnumHasNoValue;
    const name = tag.attributes.get("name") orelse return error.EnumHasNoName;
    const maybe_groups = tag.attributes.get("group");

    var en = Registry.Enum{
        .name = try arena_allocator.dupe(u8, name),
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
                try en.groups.append(arena_allocator, key);
                continue;
            }

            const copy = try arena_allocator.dupe(u8, group);
            try registry.enumgroups.putNoClobber(arena_allocator, copy, .{ .bitmask = false });
            try en.groups.append(arena_allocator, copy);
        }
    }

    try registry.enums.put(arena_allocator, en.name, en);
}

pub fn extractEnumGroup(registry: *Registry, tag: *xml.XmlTag.StartTag) !void {
    const arena_allocator = registry.arena.allocator();

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
        arena_allocator,
        try arena_allocator.dupe(u8, group),
        egroup,
    );
}

pub fn extractExtension(registry: *Registry, tree: *xml.XmlTree) !void {
    const arena_allocator = registry.arena.allocator();
    const tag = tree.root.?;
    const name = tag.attributes.get("name") orelse return error.ExtensionWithoutName;

    try registry.extensions.putNoClobber(arena_allocator, try arena_allocator.dupe(u8, name), {});
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
            // BUG(zig): In 0.11.0 I can't break up the following
            // std.mem.eql because it causes a miscompilation and a segfault

            while (!std.mem.eql(u8, read_buffer.items[read_buffer.items.len - 2 .. read_buffer.items.len], "--")) {
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

                            _ = element_stack.pop();
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
                    element_stack.items[element_stack.items.len - 1].deinit(allocator);
                    _ = element_stack.pop();
                }
            },
            .comment => {},
        }
    }
    return registry;
}

fn printHelp(params: []const clap.Param(clap.Help)) !void {
    try clap.usage(std.io.getStdErr().writer(), clap.Help, params);
    try std.io.getStdErr().writer().writeByte('\n');

    try clap.help(std.io.getStdErr().writer(), clap.Help, params, .{});
}

pub fn main() !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help          Show this help message
        \\<file>               File path to OpenGL registry
    );
    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = gpalloc.deinit();
    defer arena.deinit();

    const parsers = comptime .{
        .str = clap.parsers.string,
        .file = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpalloc.allocator(),
    }) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return printHelp(&params);
    } else if (res.positionals.len < 1) {
        std.debug.print("error: Insufficient number of positional arguments!\n", .{});
        return printHelp(&params);
    }

    const cwd = std.fs.cwd();
    const registry_buffer =
        try cwd.readFileAlloc(
        gpalloc.allocator(),
        res.positionals[0],
        std.math.maxInt(usize),
    );
    defer gpalloc.allocator().free(registry_buffer);

    var registry_buffer_stream = std.io.fixedBufferStream(registry_buffer);

    var registry = try parseRegistry(
        gpalloc.allocator(),
        registry_buffer_stream.reader(),
    );

    std.debug.print("enum groups: {}, enums: {}, commands: {}, extensions: {}\n", .{
        registry.enumgroups.size,
        registry.enums.size,
        registry.commands.size,
        registry.extensions.size,
    });
}
