const std = @import("std");
const clap = @import("clap");

const ElementTag = enum {
    registry,
    comment,
    types,
    type,
    kinds,
    kind,

    enums,
    @"enum",
    unused,

    commands,
    command,
    feature,
    require,
    remove,
    extensions,
    extension,
    apientry,
    name,
};

const Element = struct {
    tag: ElementTag,
    attributes: std.StringHashMapUnmanaged([]const u8) = .{},
    content: std.ArrayListUnmanaged(Content) = .{},

    const Content = union(enum) {
        element: Element,
        text: []const u8,
    };
};

pub const Registry = struct {
    const Enum = struct {
        name: []const u8,
        value: usize,
    };
    const Command = struct {};
    const Type = struct {};
};

const XmlTag = union(enum) {
    xml_decl: XmlDecl,
    start_tag: StartTag,
    end_tag: EndTag,
    comment,

    const StartTag = struct {
        name: []const u8,
        self_close: bool,
        attributes: std.StringHashMapUnmanaged([]const u8) = .{},

        fn deinit(self: *StartTag, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
        }
    };

    const EndTag = struct {
        name: []const u8,

        fn deinit(self: *EndTag, allocator: std.mem.Allocator) void {
            allocator.free(self.name);
        }
    };

    const XmlDecl = struct {
        version: std.SemanticVersion,
    };
};

fn readXmlName(reader: anytype, buffer: []u8) !usize {
    switch (try reader.readByte()) {
        ':',
        'A'...'Z',
        '_',
        'a'...'z',
        => |c| {
            buffer[0] = c;
        },
        else => return error.InvalidChar,
    }
    var idx: usize = 1;
    while (true) switch (try reader.readByte()) {
        '-',
        '.',
        ':',
        '0'...'9',
        'A'...'Z',
        '_',
        'a'...'z',
        => |c| {
            if (idx >= buffer.len) {
                return error.NameTooLong;
            }
            buffer[idx] = c;
            idx += 1;
        },
        ' ', '\n', '\t', '\r' => break,
        else => return error.InvalidChar,
    };
    return idx;
}

fn skipWhitespace(reader: anytype) !u8 {
    while (true) switch (try reader.readByte()) {
        ' ', '\n', '\t', '\r' => {},
        else => |c| return c,
    };
}

const ParseXmlAttribute = struct {
    key: []const u8,
    value: []const u8,
};

fn parseXmlAttribute(buffer: []const u8) !ParseXmlAttribute {
    _ = buffer;
}

fn parseXmlTag(allocator: std.mem.Allocator, buffer: []const u8) !XmlTag {
    _ = allocator;
    var stream = std.io.fixedBufferStream(buffer);
    const reader = stream.reader();

    var name_buffer = [_]u8{0} ** 512;

    switch (try reader.readByte()) {
        '?' => {
            const name = try reader.readBytesNoEof(3);
            if (!std.mem.eql(u8, &name, "xml")) {
                return error.UnknownTag;
            }
            return .{ .xml_decl = .{
                .version = std.SemanticVersion{ .major = 1, .minor = 0, .patch = 0 },
            } };
        },
        '/' => {
            const name_len = readXmlName(reader, &name_buffer) catch |err| switch (err) {
                error.EndOfStream => buffer.len,
                else => return err,
            };
            const name = buffer[1..name_len];
            return .{ .end_tag = .{ .name = name } };
        },
        '!' => {
            // eg. <!---> is invalid (<!-- -> or <!- -->)
            if (buffer.len < 4) {
                return error.UnknownTag;
            }

            {
                const maybe_dashes = try reader.readBytesNoEof(2);
                if (!std.mem.eql(u8, &maybe_dashes, "--")) {
                    return error.UnknownTag;
                }
            }

            try stream.seekTo(try stream.getEndPos());
            try stream.seekBy(-2);

            {
                const maybe_dashes = try reader.readBytesNoEof(2);
                if (!std.mem.eql(u8, &maybe_dashes, "--")) {
                    return error.UnknownTag;
                }
            }
            return .{ .comment = {} };
        },
        else => |c| {
            if (!std.ascii.isAlphabetic(c)) {
                return error.UnknownTag;
            }
            try stream.seekBy(-1);
            const name_len = readXmlName(reader, &name_buffer) catch |err| switch (err) {
                error.EndOfStream => buffer.len,
                error.InvalidChar => if (stream.pos == buffer.len and buffer[buffer.len - 1] == '/') buffer.len - 1 else return err,
                else => return err,
            };
            const name = buffer[0..name_len];
            const self_closing = buffer[buffer.len - 1] == '/';

            // TODO: Attributes

            return .{ .start_tag = .{
                .name = name,
                .self_close = self_closing,
            } };
        },
    }
}

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

        const xml_tag = try parseXmlTag(allocator, read_buffer.items);
        // catch |err| switch (err) {
        //     error.UnknownTag => {
        //         std.debug.print("bytes_read={}\n", .{bytes_read});
        //         return err;
        //     },
        //     else => return err,
        // };
        switch (xml_tag) {
            .xml_decl => |*tag| {
                std.debug.print("<?xml version='{}.{}'?>", .{ tag.version.major, tag.version.minor });
            },
            .start_tag => |*tag| {
                std.debug.print("<{s}> {}\n", .{ tag.name, tag.self_close });
            },
            .end_tag => |*tag| {
                std.debug.print("</{s}>\n", .{tag.name});
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
