const std = @import("std");

const AttributeMap = std.StringHashMapUnmanaged([]const u8);

pub const XmlTag = union(enum) {
    xml_decl: XmlDecl,
    start_tag: StartTag,
    end_tag: EndTag,
    comment,

    pub const StartTag = struct {
        name: []const u8,
        self_close: bool,
        attributes: AttributeMap = .{},

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
/// Output can be ignored when `writer` is void.
/// Returns a stop character.
/// regex: `[:_A-Za-z][\-\.:_0-9A-Za-z]*`
fn readXmlName(reader: anytype, writer: anytype) !u8 {
    const skip = @TypeOf(writer) == type and writer == void;
    // @compileLog(@TypeOf(writer), skip);

    switch (try reader.readByte()) {
        ':',
        'A'...'Z',
        '_',
        'a'...'z',
        => |c| {
            if (!skip) {
                try writer.writeByte(c);
            }
        },
        else => return error.InvalidChar,
    }
    while (true) {
        const c = try reader.readByte();
        switch (c) {
            '-',
            '.',
            ':',
            '0'...'9',
            'A'...'Z',
            '_',
            'a'...'z',
            => {
                if (!skip) {
                    try writer.writeByte(c);
                }
            },
            ' ', '\n', '\t', '\r', '=', '/' => |stop| return stop,
            else => return error.InvalidChar,
        }
    }
}

test "readXmlName" {
    const testing = std.testing;

    var buffer = [_]u8{0} ** 40;
    {
        const in_buffer = "foo=\"value\"";
        var in_stream = std.io.fixedBufferStream(in_buffer);
        var out_stream = std.io.fixedBufferStream(&buffer);

        try testing.expectEqual(@as(u8, '='), try readXmlName(in_stream.reader(), out_stream.writer()));
        try testing.expectEqualSlices(u8, "foo", buffer[0..out_stream.pos]);
    }
    {
        const in_buffer = "foo/>";
        var in_stream = std.io.fixedBufferStream(in_buffer);
        var out_stream = std.io.fixedBufferStream(&buffer);

        try testing.expectEqual(@as(u8, '/'), try readXmlName(in_stream.reader(), out_stream.writer()));
        try testing.expectEqualSlices(u8, "foo", buffer[0..out_stream.pos]);
    }
    {
        const in_buffer = "foo\n/>";
        var in_stream = std.io.fixedBufferStream(in_buffer);
        var out_stream = std.io.fixedBufferStream(&buffer);

        try testing.expectEqual(@as(u8, '\n'), try readXmlName(in_stream.reader(), out_stream.writer()));
        try testing.expectEqualSlices(u8, "foo", buffer[0..out_stream.pos]);
    }
}

/// Skips whitespace and returns last character
/// this function consumed
/// regex: `[ \n\t\r]*`
fn skipWhitespace(reader: anytype) !u8 {
    while (true) switch (try reader.readByte()) {
        ' ', '\n', '\t', '\r' => {},
        else => |c| return c,
    };
}

/// Parse a set of XML tag attributes.
/// Keys and values in the resulting attribute map are slices of `buffer`.
fn parseXmlTagAttributes(allocator: std.mem.Allocator, buffer: []const u8) !AttributeMap {
    var attr_map: AttributeMap = .{};
    errdefer attr_map.deinit(allocator);

    var stream = std.io.fixedBufferStream(buffer);

    while (true) {
        const ch = skipWhitespace(stream.reader()) catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };

        if (stream.pos == buffer.len) {
            if (ch == '/' or ch == '?')
                break
            else
                return error.InvalidChar;
        }

        if (ch == '=') {
            return error.ExpectedAttrName;
        }

        try stream.seekBy(-1);

        const attrname_start = stream.pos;
        const name_stop_char = try readXmlName(stream.reader(), void);
        const attrname_end = stream.pos - 1;

        if (name_stop_char == '/') {
            return error.AttributeWithoutValue;
        }

        const maybe_quote = skipWhitespace(stream.reader()) catch |err| switch (err) {
            error.EndOfStream => return error.ExpectedAttrValue,
            else => return err,
        };
        const quote = if (maybe_quote == '=') try skipWhitespace(stream.reader()) else maybe_quote;

        if (quote != '\'' and quote != '"') {
            return error.InvalidChar;
        }

        const attrvalue_start = stream.pos;
        try stream.reader().skipUntilDelimiterOrEof(quote);
        const attrvalue_end = stream.pos - 1;

        const attrname = buffer[attrname_start..attrname_end];
        const attrvalue = buffer[attrvalue_start..attrvalue_end];

        try attr_map.put(allocator, attrname, attrvalue);
    }

    return attr_map;
}

test "parseXmlTagAttributes" {
    const test_allocator = std.testing.allocator;
    {
        var attr_map = try parseXmlTagAttributes(test_allocator, " ");
        defer attr_map.deinit(test_allocator);

        try std.testing.expectEqual(@as(usize, 0), attr_map.count());
    }
    {
        var attr_map = try parseXmlTagAttributes(test_allocator, " name='Joe'");
        defer attr_map.deinit(test_allocator);

        const name = attr_map.get("name");

        try std.testing.expectEqual(@as(usize, 1), attr_map.count());
        try std.testing.expect(name != null);
        try std.testing.expectEqualSlices(u8, "Joe", name.?);
    }
    {
        var attr_map = try parseXmlTagAttributes(test_allocator, " attr1= \"foo\" attr2 =\"bar\"");
        defer attr_map.deinit(test_allocator);

        const attr1 = attr_map.get("attr1");
        const attr2 = attr_map.get("attr2");

        try std.testing.expectEqual(@as(usize, 2), attr_map.count());
        try std.testing.expect(attr1 != null);
        try std.testing.expect(attr2 != null);
        try std.testing.expectEqualSlices(u8, "foo", attr1.?);
        try std.testing.expectEqualSlices(u8, "bar", attr2.?);
    }
    {
        var attr_map = try parseXmlTagAttributes(test_allocator, "attr1=\"foo\" ");
        defer attr_map.deinit(test_allocator);

        const attr1 = attr_map.get("attr1");

        try std.testing.expectEqual(@as(usize, 1), attr_map.count());
        try std.testing.expect(attr1 != null);
        try std.testing.expectEqualSlices(u8, "foo", attr1.?);
    }
}

test "(errors) parseXmlTagAttributes" {
    const test_allocator = std.testing.allocator;

    try std.testing.expectError(error.ExpectedAttrName, parseXmlTagAttributes(test_allocator, " ='Joe'"));
    try std.testing.expectError(error.ExpectedAttrValue, parseXmlTagAttributes(test_allocator, " name="));
    try std.testing.expectError(error.InvalidChar, parseXmlTagAttributes(test_allocator, " 'Joe'"));
    try std.testing.expectError(error.InvalidChar, parseXmlTagAttributes(test_allocator, " name = /value/"));
}

/// Parse a singular XML tag.
///
/// A XML tag has a form of `<name [attrib="value"]* >`.
/// This function expects that the first '<' is not in the buffer.
pub fn parseXmlTag(allocator: std.mem.Allocator, buffer: []const u8) !XmlTag {
    var stream = std.io.fixedBufferStream(buffer);
    const reader = stream.reader();

    // var name_buffer = [_]u8{0} ** 50;

    switch (try reader.readByte()) {
        '?' => {
            const name = try reader.readBytesNoEof(3);
            if (!std.mem.eql(u8, &name, "xml")) {
                return error.UnknownTag;
            }
            if (buffer[buffer.len - 1] != '?') {
                return error.UnknownTag;
            }

            var attr_map = try parseXmlTagAttributes(allocator, buffer[4..]);
            defer attr_map.deinit(allocator);

            if (attr_map.get("version")) |version| {
                var it = std.mem.splitScalar(u8, version, '.');
                const major_str = it.next() orelse return error.BadXmlDeclVersion;
                const minor_str = it.next() orelse return error.BadXmlDeclVersion;

                if (it.next()) |_| return error.BadXmlVersion;

                const ver = std.SemanticVersion{
                    .major = std.fmt.parseInt(usize, major_str, 10) catch return error.BadXmlDeclVersion,
                    .minor = std.fmt.parseInt(usize, minor_str, 10) catch return error.BadXmlDeclVersion,
                    .patch = 0,
                };

                return .{ .xml_decl = .{ .version = ver } };
            } else {
                return error.XmlDeclWithoutVersion;
            }
        },
        '/' => {
            const name_len = readXmlName(reader, void) catch |err| switch (err) {
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

            var counting_reader = std.io.countingReader(reader);

            const name_stop = readXmlName(counting_reader.reader(), void) catch |err| switch (err) {
                error.EndOfStream => return .{ .start_tag = .{ .name = buffer[0..counting_reader.bytes_read], .self_close = false } },
                else => return err,
            };
            const name = buffer[0 .. counting_reader.bytes_read - 1];

            if (stream.pos != buffer.len and name_stop == '/') {
                return error.InvalidChar;
            }

            return .{ .start_tag = .{
                .name = name,
                .self_close = std.mem.endsWith(u8, buffer, "/"),
                .attributes = try parseXmlTagAttributes(allocator, buffer[counting_reader.bytes_read..]),
            } };
        },
    }
}

pub fn readXmlTag(
    reader: anytype,
    writer: anytype,
    // read_buffer: *std.ArrayList(u8),
) !void {
    const WriterT = if (@TypeOf(writer) == type and writer != void)
        @compileError("Only void is allowed as type parameter")
    else if (@TypeOf(writer) == type)
        void
    else
        @TypeOf(writer);

    const start_byte = try reader.readByte();

    if (WriterT != void) {
        try writer.writeByte(start_byte);
    }

    switch (start_byte) {
        '?' => {
            var prev_byte: u8 = 0;
            while (true) {
                const current = try reader.readByte();
                if (current == '>' and prev_byte == 0) return error.InvalidTag;
                if (current == '>' and prev_byte == '?') {
                    if (WriterT != void) {
                        try writer.writeByte(current);
                    }
                    break;
                }

                if (WriterT != void and prev_byte != 0) {
                    try writer.writeByte(prev_byte);
                }
                prev_byte = current;
            }
        },
        '!' => {
            const begin_buffer = try reader.readBytesNoEof(3);

            if (WriterT != void) {
                _ = try writer.write(begin_buffer);
            }
            // errdefer @breakpoint();
            if (!std.mem.eql(u8, std.mem.trimRight(u8, &begin_buffer, " \n\t\r"), "--")) return error.UnsupportedDocumentTag;

            var read_buffer = [_]u8{0} ** 4;
            _ = try reader.readAll(&read_buffer);

            while (true) {
                if (WriterT != void and read_buffer[0] != 0) {
                    try writer.writeByte(read_buffer[0]);
                }
                read_buffer[0] = read_buffer[1];
                read_buffer[1] = read_buffer[2];
                read_buffer[2] = read_buffer[3];
                read_buffer[3] = try reader.readByte();
                if (std.mem.eql(u8, std.mem.trimLeft(u8, read_buffer[0..], " \n\t\r"), "-->")) {
                    if (WriterT != void) {
                        try writer.write(read_buffer);
                    }
                    break;
                }
            }
        },
        else => if (WriterT != void) {
            try reader.streamUntilDelimiter(writer, '>', null);
            try writer.writeByte('>');
        } else {
            try reader.streamUntilDelimiter(std.io.null_writer, '>', null);
        },
    }
}

test readXmlTag {
    const testing = std.testing;
    const allocator = testing.allocator;
    var read_buffer = std.ArrayList(u8).init(allocator);
    defer read_buffer.deinit();

    {
        defer read_buffer.clearAndFree();

        const test_case = "hello>";
        var fbs = std.io.fixedBufferStream(test_case);

        try readXmlTag(fbs.reader(), read_buffer.writer());

        try testing.expectEqualSlices(u8, test_case, read_buffer.items);
    }
    {
        defer read_buffer.clearAndFree();

        const test_case = "/hello>";
        var fbs = std.io.fixedBufferStream(test_case);

        try readXmlTag(fbs.reader(), read_buffer.writer());

        try testing.expectEqualSlices(u8, test_case, read_buffer.items);
    }
    {
        defer read_buffer.clearAndFree();

        const test_case = "?xml ?>";
        var fbs = std.io.fixedBufferStream(test_case);

        try readXmlTag(fbs.reader(), read_buffer.writer());

        try testing.expectEqualSlices(u8, test_case, read_buffer.items);
    }
    {
        defer read_buffer.clearAndFree();

        const test_case = "!-- <this should be ignored> -->";
        var fbs = std.io.fixedBufferStream(test_case);

        try readXmlTag(fbs.reader(), read_buffer.writer());

        try testing.expectEqualSlices(u8, test_case, read_buffer.items);
    }
}

test "parseXmlTag" {
    const testing = std.testing;
    const allocator = testing.allocator;
    {
        var tag = try parseXmlTag(allocator, "name/");
        defer if (tag == .start_tag) tag.start_tag.deinit(allocator);

        try testing.expect(tag == .start_tag);
        try testing.expectEqualSlices(u8, "name", tag.start_tag.name);
    }
    {
        var tag = try parseXmlTag(allocator, "name value=\"\"/");
        defer if (tag == .start_tag) tag.start_tag.deinit(allocator);

        try testing.expect(tag == .start_tag);
        try testing.expectEqualSlices(u8, "name", tag.start_tag.name);
        try testing.expectEqual(@as(@TypeOf(tag.start_tag.attributes).Size, 1), tag.start_tag.attributes.count());
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
        if (std.mem.startsWith(u8, read_buffer.items, "!--")) {
            while (!std.mem.endsWith(u8, read_buffer.items, "--")) {
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

test "(xml_decl) parseXml" {
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

test "(attributes) parseXml" {
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

test "(text content) parseXml" {
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
