const std = @import("std");

const XmlTag = union(enum) {
    xml_decl: XmlDecl,
    start_tag: StartTag,
    end_tag: EndTag,
    comment,

    const StartTag = struct {
        name: []const u8,
        self_close: bool,
        attributes: std.StringHashMapUnmanaged([]const u8) = .{},

        pub fn deinit(self: *StartTag, allocator: std.mem.Allocator) void {
            self.attributes.deinit(allocator);
        }
    };

    const EndTag = struct {
        name: []const u8,
    };

    const XmlDecl = struct {
        version: std.SemanticVersion = std.SemanticVersion{ .major = 1, .minor = 0, .patch = 0 },
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

fn parseXmlTagAttributes(comptime TagType: type, tag: *TagType, allocator: std.mem.Allocator, stream: anytype, reader: anytype) !void {
    switch (TagType) {
        XmlTag.StartTag, XmlTag.XmlDecl => {},
        else => @compileError("Unsupported tag type"),
    }
    const buffer = stream.buffer;

    while (true) {
        const ch = skipWhitespace(reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (TagType == XmlTag.StartTag and ch == '/') {
            if (stream.pos == buffer.len) {
                tag.self_close = true;
                break;
            }
            return error.InvalidChar;
        }
        if (TagType == XmlTag.XmlDecl and ch == '?') {
            break;
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
        switch (TagType) {
            XmlTag.StartTag => try tag.attributes.put(allocator, attrname, attrvalue),
            XmlTag.XmlDecl => {
                if (std.mem.eql(u8, attrname, "version")) {
                    var it = std.mem.splitScalar(u8, attrvalue, '.');
                    const major = try std.fmt.parseInt(u32, it.next() orelse "0", 10);
                    const minor = try std.fmt.parseInt(u32, it.next() orelse "0", 10);
                    tag.version = std.SemanticVersion{ .major = major, .minor = minor, .patch = 0 };
                }
            },
            else => @compileError("Unsupported tag type"),
        }
    }
}

pub fn parseXmlTag(allocator: std.mem.Allocator, buffer: []const u8) !XmlTag {
    var stream = std.io.fixedBufferStream(buffer);
    const reader = stream.reader();

    switch (try reader.readByte()) {
        '?' => {
            var tag = XmlTag.XmlDecl{};
            const name = try reader.readBytesNoEof(3);
            if (!std.mem.eql(u8, &name, "xml")) {
                return error.UnknownTag;
            }
            try parseXmlTagAttributes(XmlTag.XmlDecl, &tag, allocator, &stream, reader);
            return .{ .xml_decl = tag };
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

            try parseXmlTagAttributes(XmlTag.StartTag, &tag, allocator, &stream, reader);

            return .{ .start_tag = tag };
        },
    }
}
