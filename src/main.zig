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

const Element = struct {
    tag: ElementTag,
    attributes: std.StringHashMapUnmanaged([]const u8) = .{},
    content: std.ArrayListUnmanaged(Content) = .{},

    const Content = union(enum) {
        element: Element,
        text: []const u8,
    };

    fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
        for (self.content.items) |*content| switch (content.*) {
            .element => content.element.deinit(allocator),
            .text => allocator.free(content.text),
        };
        self.content.deinit(allocator);

        {
            var it = self.attributes.keyIterator();
            while (it.next()) |key| {
                allocator.free(key.*);
            }
        }
        {
            var it = self.attributes.valueIterator();
            while (it.next()) |value| {
                allocator.free(value.*);
            }
        }
        self.attributes.deinit(allocator);
    }
};

pub const Registry = struct {
    const Enum = struct {
        name: []const u8,
        value: usize,
    };
    const Command = struct {};
    const Type = struct {};
};

pub fn parseRegistry(
    arena_allocator: std.mem.Allocator,
    allocator: std.mem.Allocator,
    reader: anytype,
    options: struct {
        core: bool = false,
    },
) !Registry {
    _ = arena_allocator;
    _ = options;

    const TagOrElement = union(enum) {
        tag: ElementTag,
        element: Element,

        fn getTag(self: @This()) ElementTag {
            return switch (self) {
                .tag => |t| t,
                .element => self.tag,
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
        read_buffer.clearAndFree();
        try reader.streamUntilDelimiter(read_buffer.writer(), '>', null);

        // check if we are inside a comment
        // and make sure we have read a full comment tag incase a comment contains a valid
        // xml tag
        // eg. <!-- <element attr1=""/> -->
        if (read_buffer.items.len > 3 and std.mem.eql(u8, read_buffer.items[0..3], "!--")) {
            while (!std.mem.eql(u8, read_buffer.items[read_buffer.items.len - 2 .. read_buffer.items.len], "--")) {
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
                std.debug.print("<?xml version='{}.{}'?>", .{ tag.version.major, tag.version.minor });
            },
            .start_tag => |*tag| {
                defer tag.deinit(allocator);
                // std.debug.print("{s}\n", .{tag.name});
                const elemtag = std.meta.stringToEnum(ElementTag, tag.name) orelse return error.UnknownTag;
                if (!tag.self_close) {
                    try element_stack.append(.{ .tag = elemtag });
                }
                if (element_stack.getLastOrNull()) |top| {
                    switch (top.tag) {
                        .enums => {
                            if (elemtag == .@"enum") {
                                const value = tag.attributes.get("value") orelse return error.EnumHasNoValue;
                                const name = tag.attributes.get("name") orelse return error.EnumHasNoName;

                                std.debug.print("pub const {s}: u32 = {s}\n", .{ name, value });
                            }
                        },
                        else => {
                            // unreachable
                        },
                    }
                }
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
    return .{};
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

    _ = try parseRegistry(
        arena.allocator(),
        gpalloc.allocator(),
        registry_buffer_stream.reader(),
        .{},
    );
}
