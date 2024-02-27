const std = @import("std");

pub const XmlTag = union(enum) {
    xml_decl: XmlDecl,
    start_tag: StartTag,
    end_tag: EndTag,
    comment,

    pub const StartTag = struct {
        name: []const u8,
        self_close: bool,
        attributes: std.StringHashMapUnmanaged([]const u8) = .{},

        pub fn deinit(self: *StartTag, allocator: std.mem.Allocator) void {
            self.attributes.deinit(allocator);
        }
    };

    pub const EndTag = struct {
        name: []const u8,
    };

    pub const XmlDecl = struct {
        version: std.SemanticVersion = std.SemanticVersion{ .major = 1, .minor = 0, .patch = 0 },
    };
};

/// Read a tag or attribute name.
/// If `buffer` is null, skips name.
/// regex: `[:_A-Za-z][\-\.:_0-9A-Za-z]*`
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

/// Skips whitespace.
/// regex: `[ \n\t\r]*`
fn skipWhitespace(reader: anytype) !u8 {
    while (true) switch (try reader.readByte()) {
        ' ', '\n', '\t', '\r' => {},
        else => |c| return c,
    };
}

// TODO: Make this method return StringHashMapUnmanaged
// TODO: Do not depend on FixedBufferStream in this method
/// Parse a set of XML tag attributes.
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
            else => unreachable,
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
            else => unreachable,
        }
    }
}

/// Parse a singular XML tag.
///
/// A XML tag has a form of `<name [attrib="value"]* >`.
/// This function expects that the first '<' is not in the buffer.
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
                error.EndOfStream => buffer.len + 1,
                error.InvalidChar => if (stream.pos == buffer.len and buffer[buffer.len - 1] == '/')
                    buffer.len
                else
                    return err,
                else => return err,
            };
            const name = buffer[0 .. name_len - 1];

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

/// Represents a XML document.
/// Manages it's own memory.
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

        /// Collect text content recursively
        pub fn collectText(self: Element, writer: anytype) !void {
            for (self.content.items) |content| switch (content) {
                .element => |elem| {
                    try elem.collectText(writer);
                },
                .text => |text| {
                    try writer.writeAll(text);
                },
            };
        }

        /// Collect text content before an element recursively
        pub fn collectTextBefore(self: @This(), child: *@This(), writer: anytype) !void {
            for (self.content.items, 0..) |content, idx| switch (content) {
                .element => {
                    if (&(self.content.items[idx].element) == child) {
                        // std.debug.print("hit\n", .{});
                        break;
                    }
                    try self.content.items[idx].element.collectText(writer);
                },
                .text => |text| {
                    try writer.writeAll(text);
                },
            };
        }

        const ElementIterator = struct {
            index: usize,
            element: *Element,

            pub fn next(self: *@This()) ?*Element {
                while (true) {
                    if (self.index >= self.element.content.items.len) {
                        return null;
                    }
                    switch (self.element.content.items[self.index]) {
                        .element => |*elem| {
                            self.index += 1;
                            return elem;
                        },
                        else => {
                            self.index += 1;
                        },
                    }
                }
            }
        };

        /// Iterate over child elements
        pub fn elementIterator(self: *@This()) ElementIterator {
            return .{ .index = 0, .element = self };
        }

        /// Finds first child element with specified `name`
        pub fn findElement(self: *@This(), name: []const u8) ?*Element {
            var it = self.elementIterator();
            while (it.next()) |element| {
                if (std.mem.eql(u8, element.name, name)) {
                    return element;
                }
            }
            return null;
        }

        const FindElementsIterator = struct {
            inner: ElementIterator,
            name: []const u8,

            pub fn next(self: *@This()) ?*Element {
                while (self.inner.next()) |elem| {
                    if (std.mem.eql(u8, elem.name, self.name)) {
                        return elem;
                    }
                }
                return null;
            }
        };

        /// Finds all child elements with specified `name`
        pub fn findElements(self: *@This(), name: []const u8) FindElementsIterator {
            return .{ .inner = self.elementIterator(), .name = name };
        }
    };

    pub fn deinit(self: *XmlTree) void {
        if (self.root) |*root| {
            root.deinit(self.allocator);
        }
    }
};

/// Parse a XML document into a XmlTree.
///
/// If `root` is not null, makes an equivalent XmlTree.Element
/// as a root of the tree.
pub fn parseXml(allocator: std.mem.Allocator, reader: anytype, root: ?XmlTag.StartTag) !XmlTree {
    var tree = XmlTree{ .allocator = allocator };
    errdefer tree.deinit();

    var element_stack = std.ArrayList(*XmlTree.Element).init(allocator);
    defer element_stack.deinit();

    var read_buffer = std.ArrayList(u8).init(allocator);
    defer read_buffer.deinit();

    var text_buffer = std.ArrayList(u8).init(allocator);
    defer text_buffer.deinit();

    var first_elem = true;

    if (root) |_| {
        first_elem = false;

        var elem = XmlTree.Element{
            .name = try allocator.dupe(u8, root.?.name),
        };
        errdefer elem.deinit(allocator);

        var it = root.?.attributes.iterator();
        while (it.next()) |pair| {
            const key = try allocator.dupe(u8, pair.key_ptr.*);
            const value = try allocator.dupe(u8, pair.value_ptr.*);
            try elem.attributes.put(allocator, key, value);
        }
        tree.root = elem;
        if (root.?.self_close) {
            return tree;
        }
        try element_stack.append(&(tree.root.?));
    }

    while (first_elem or element_stack.items.len != 0) {
        reader.streamUntilDelimiter(text_buffer.writer(), '<', null) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        if (element_stack.getLastOrNull()) |top| {
            const text = try text_buffer.toOwnedSlice();
            if (std.mem.trim(u8, text, "\n\t\r ").len != 0) {
                try top.content.append(allocator, .{ .text = text });
            } else {
                allocator.free(text);
            }
        } else {
            text_buffer.clearRetainingCapacity();
        }

        read_buffer.clearRetainingCapacity();
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
                errdefer elem.deinit(allocator);

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
        var tree = try parseXml(std.testing.allocator, stream.reader(), null);
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
        var tree = try parseXml(std.testing.allocator, stream.reader(), null);
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
    var tree = try parseXml(std.testing.allocator, stream.reader(), null);
    defer tree.deinit();

    try std.testing.expect(tree.root != null);
    try std.testing.expectEqual(tree.root.?.content.items.len, 1);
    try std.testing.expectEqual(tree.root.?.content.items[0].element.attributes.count(), 1);

    const attr1 = tree.root.?.content.items[0].element.attributes.get("attr1");
    try std.testing.expect(attr1 != null);
    try std.testing.expectEqualSlices(u8, "value1", attr1.?);
}

test "text content" {
    const buffer =
        \\<?xml version="1.0"?>
        \\<root>
        \\Split
        \\<tag attr1 = "value1"/>
        \\text
        \\<tag attr1= "value"> content</tag>
        \\</root>
    ;
    var stream = std.io.fixedBufferStream(buffer);
    var tree = try parseXml(std.testing.allocator, stream.reader(), null);
    defer tree.deinit();

    try std.testing.expect(tree.root != null);

    var text_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer text_buffer.deinit();
    try tree.root.?.collectText(text_buffer.writer());

    try std.testing.expectEqualSlices(u8, "\nSplit\n\ntext\n content", text_buffer.items);
}
