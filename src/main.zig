const std = @import("std");
const clap = @import("clap");

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
            self.attributes.deinit(allocator);
        }
    };

    const EndTag = struct {
        name: []const u8,
    };

    const XmlDecl = struct {
        version: std.SemanticVersion,
    };
};

fn readXmlName(reader: anytype, buffer: ?[]u8) !usize {
    switch (try reader.readByte()) {
        ':',
        'A'...'Z',
        '_',
        'a'...'z',
        => |c| {
            if (buffer) |b| {
                b[0] = c;
            }
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
            if (buffer) |b| {
                if (idx >= b.len) {
                    return error.NameTooLong;
                }
                b[0] = c;
            }
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

fn parseXmlTag(allocator: std.mem.Allocator, buffer: []const u8) !XmlTag {
    var stream = std.io.fixedBufferStream(buffer);
    const reader = stream.reader();

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
            const name_len = readXmlName(reader, null) catch |err| switch (err) {
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
            const name_len = readXmlName(reader, null) catch |err| switch (err) {
                error.EndOfStream => buffer.len,
                error.InvalidChar => if (stream.pos == buffer.len and buffer[buffer.len - 1] == '/')
                    buffer.len - 1
                else
                    return err,
                else => return err,
            };
            const name = buffer[0..name_len];

            // <element attr="v1" .. > or <element attr="v1" .. />
            var tag = XmlTag.StartTag{ .name = name, .self_close = false };
            errdefer tag.deinit(allocator);
            if (stream.pos == buffer.len and buffer[buffer.len - 1] == '/') {
                tag.self_close = true;
                return .{ .start_tag = tag };
            }

            while (true) {
                const ch = skipWhitespace(reader) catch |err| switch (err) {
                    error.EndOfStream => break,
                    else => return err,
                };
                if (ch == '/') {
                    if (stream.pos == buffer.len) {
                        tag.self_close = true;
                        break;
                    }
                    return error.InvalidChar;
                }
                try stream.seekBy(-1);

                const attrname_start = stream.pos;
                const attrname_len = readXmlName(reader, null) catch |err| switch (err) {
                    error.InvalidChar => if (buffer[stream.pos - 1] == '=')
                        stream.pos - attrname_start
                    else
                        return err,
                    else => return err,
                };

                const maybe_quote = try skipWhitespace(reader);
                const quote = if (maybe_quote == '=') try skipWhitespace(reader) else maybe_quote;

                if (quote != '\'' and quote != '"') {
                    return error.InvalidChar;
                }
                const attrvalue_start = stream.pos;
                try reader.skipUntilDelimiterOrEof(quote);
                const attrvalue_len = stream.pos - attrvalue_start;

                const attrname = buffer[attrname_start .. attrname_start + attrname_len];
                const attrvalue = buffer[attrvalue_start .. attrvalue_start + attrvalue_len];

                try tag.attributes.put(allocator, attrname, attrvalue);
            }

            return .{ .start_tag = tag };
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

        var xml_tag = try parseXmlTag(allocator, read_buffer.items);
        switch (xml_tag) {
            .xml_decl => |*tag| {
                std.debug.print("<?xml version='{}.{}'?>", .{ tag.version.major, tag.version.minor });
            },
            .start_tag => |*tag| {
                std.debug.print("<{s}> {}\n", .{ tag.name, tag.self_close });
                tag.deinit(allocator);
                const elemtag = std.meta.stringToEnum(ElementTag, tag.name) orelse return error.UnknownTag;
                if (!tag.self_close) {
                    try element_stack.append(.{ .tag = elemtag });
                }
            },
            .end_tag => |*tag| {
                std.debug.print("</{s}>\n", .{tag.name});
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
