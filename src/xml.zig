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
        else => return error.InvalidNameChar,
    }
    var idx: usize = 1;
    while (true) {
        const c = try reader.readByte();
        idx += 1;
        switch (c) {
            '-',
            '.',
            ':',
            '0'...'9',
            'A'...'Z',
            '_',
            'a'...'z',
            => {
                if (buffer) |b| {
                    if (idx >= b.len) {
                        return error.NameTooLong;
                    }
                    b[0] = c;
                }
            },
            ' ', '\n', '\t', '\r' => break,
            else => return error.InvalidChar,
        }
    }
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
        else => @compileError(@typeName(TagType) ++ " does not accept attributes"),
    }
    const buffer = stream.buffer;

    while (true) {
        const ch = skipWhitespace(reader) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        if (stream.pos == buffer.len) switch (TagType) {
            XmlTag.StartTag => if (ch == '/') {
                tag.self_close = true;
                break;
            } else return error.InvalidChar,

            XmlTag.XmlDecl => if (ch == '?')
                break
            else
                return error.InvalidChar,
            else => @compileError("unreachable"),
        };
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

        const attrname = buffer[attrname_start .. attrname_start + attrname_len - 1];
        const attrvalue = buffer[attrvalue_start .. attrvalue_start + attrvalue_len - 1];
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
            else => @compileError("unreachable"),
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

pub const XmlTree = struct {
    allocator: std.mem.Allocator,
    xml_decl: ?XmlTag.XmlDecl = null,
    root: ?Element = null,

    pub const Element = struct {
        name: []const u8,
        attributes: std.StringHashMapUnmanaged([]const u8) = .{},
        content: std.ArrayListUnmanaged(TextOrElement) = .{},

        const TextOrElement = union(enum) {
            element: Element,
            text: []const u8,
        };

        pub fn deinit(self: *Element, allocator: std.mem.Allocator) void {
            {
                var it = self.attributes.valueIterator();
                while (it.next()) |v| {
                    allocator.free(v.*);
                }
            }
            {
                var it = self.attributes.keyIterator();
                while (it.next()) |k| {
                    allocator.free(k.*);
                }
            }
            self.attributes.deinit(allocator);

            for (self.content.items) |*content| switch (content.*) {
                .element => |*elem| elem.deinit(allocator),
                .text => |*t| allocator.free(t.*),
            };
            self.content.deinit(allocator);

            allocator.free(self.name);
        }
    };

    pub fn deinit(self: *XmlTree) void {
        if (self.root) |*root| {
            root.deinit(self.allocator);
        }
    }
};

pub fn parseXml(allocator: std.mem.Allocator, reader: anytype) !XmlTree {
    var tree = XmlTree{ .allocator = allocator };
    errdefer tree.deinit();

    var element_stack = std.ArrayList(*XmlTree.Element).init(allocator);
    defer element_stack.deinit();

    var read_buffer = std.ArrayList(u8).init(allocator);
    defer read_buffer.deinit();

    var first_elem = true;

    while (first_elem or element_stack.items.len != 0) {
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
                try read_buffer.append('>');
                try reader.streamUntilDelimiter(read_buffer.writer(), '>', null);
            }
        }

        var xml_tag = try parseXmlTag(allocator, read_buffer.items);

        switch (xml_tag) {
            .xml_decl => |tag| {
                if (tree.xml_decl != null) return error.MultipleXmlDecl;
                tree.xml_decl = tag;
            },
            .start_tag => |*tag| {
                first_elem = false;
                var elem = XmlTree.Element{
                    .name = try allocator.dupe(u8, tag.name),
                };

                var it = tag.attributes.iterator();
                while (it.next()) |pair| {
                    const key = try allocator.dupe(u8, pair.key_ptr.*);
                    const value = try allocator.dupe(u8, pair.value_ptr.*);
                    try elem.attributes.put(allocator, key, value);
                }

                const maybe_top: ?*XmlTree.Element = element_stack.getLastOrNull();

                if (maybe_top) |top| {
                    try top.content.append(allocator, .{ .element = elem });
                    var ptr = &top.content.items[top.content.items.len - 1];
                    if (!tag.self_close) {
                        try element_stack.append(&(ptr.element));
                    }
                } else {
                    tree.root = elem;
                    if (!tag.self_close) {
                        try element_stack.append(&tree.root.?);
                    }
                }
                tag.attributes.deinit(allocator);
            },
            .end_tag => |*tag| {
                const top: *XmlTree.Element = element_stack.getLastOrNull() orelse return error.UnexpectedEndTag;

                if (!std.mem.eql(u8, top.name, tag.name)) return error.EndTagMismatch;
                _ = element_stack.pop();
            },
            .comment => {},
        }
    }

    if (element_stack.items.len != 0) return error.UnenclosedTags;
    return tree;
}
test "xml_decl" {
    {
        const buffer =
            \\<?xml version="1.0"?>
        ;
        var stream = std.io.fixedBufferStream(buffer);
        var tree = try parseXml(std.testing.allocator, stream.reader());
        defer tree.deinit();

        try std.testing.expect(tree.root == null);
        try std.testing.expect(tree.xml_decl != null);
        try std.testing.expectEqual(std.math.Order.eq, tree.xml_decl.?.version.order(.{ .major = 1, .minor = 0, .patch = 0 }));
    }
    {
        const buffer =
            \\<?xml version="9.2"?>
        ;
        var stream = std.io.fixedBufferStream(buffer);
        var tree = try parseXml(std.testing.allocator, stream.reader());
        defer tree.deinit();

        try std.testing.expect(tree.root == null);
        try std.testing.expect(tree.xml_decl != null);
        try std.testing.expectEqual(std.math.Order.eq, tree.xml_decl.?.version.order(.{ .major = 9, .minor = 2, .patch = 0 }));
    }
}

test "attributes" {
    const buffer =
        \\<?xml version="1.0"?>
        \\<root>
        \\ <tag1 attr1 = "value1"/>
        \\</root>
    ;
    var stream = std.io.fixedBufferStream(buffer);
    var tree = try parseXml(std.testing.allocator, stream.reader());
    defer tree.deinit();
    const attr1 = tree.root.?.content.items[0].element.attributes.get("attr1") orelse return error.NoAttribute;

    try std.testing.expectEqualSlices(u8, "value1", attr1);
}
