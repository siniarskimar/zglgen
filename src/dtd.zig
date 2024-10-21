//! Contains structures that are used by the registry parser

const std = @import("std");

pub const Element = union(enum) {
    // top-level
    registry: Registry,

    // containers
    types: void,
    kinds: void,
    //groups: void, // not in use
    enums: Enums,
    commands: void,
    extensions: void,

    type: void,
    kind: void,
    //group: void, // not in use
    command: Command,
    @"enum": Enum,
    extension: Extension,
    feature: Feature,

    // <type> related
    apientry: void,

    // <enums> related
    unused: void,

    // <command> related
    proto: Command.Proto,
    param: Command.Param,
    ptype: []const u8,
    glx: void,
    vecequiv: void,

    // self explanitory
    alias: []const u8,

    // <feature> and <extension> related
    require: Require,
    remove: Remove,

    // Misc tags
    comment: []const u8,
    name: []const u8,

    pub const Tag = @typeInfo(@This()).Union.tag_type.?;
};

pub const Registry = struct {
    copyright: ?[]const u8 = null,
};

// Types are skipped
// pub const Type = struct {};

pub const Enums = struct {
    bitmask: bool = false,
    group: ?[]const u8 = null,
};

pub const Enum = struct {
    name: []const u8,
    value: []const u8,
    groups: []const u8,
    api: ?[]const u8,
    value_type: enum { integer, unsigned, unsigned64 } = .integer,
};

pub const Command = struct {
    name: []const u8 = "",
    return_type: []const u8 = "void",
    params: std.ArrayListUnmanaged(Param) = .{},
    alias: ?[]const u8 = null,
    vecequiv: ?[]const u8 = null,

    pub const Param = struct {
        name: []const u8,
        type: []const u8,
        group: ?[]const u8 = null,
    };

    pub const Proto = struct {
        name: []const u8 = "",
        return_type: []const u8 = "void",
    };
};

pub const Feature = struct {
    api: []const u8,
    name: []const u8,
    number: FeatureNumber,
    require: std.ArrayListUnmanaged(Require) = .{},
    remove: std.ArrayListUnmanaged(Remove) = .{},
};

pub const Require = struct {
    profile: []const u8,
    interfaces: std.ArrayListUnmanaged(InterfaceElement) = .{},
};

pub const Remove = struct {
    profile: []const u8,
    interfaces: std.ArrayListUnmanaged(InterfaceElement) = .{},
};

pub const FeatureNumber = struct {
    major: u8,
    minor: u8,

    pub fn order(lhs: @This(), rhs: @This()) std.math.Order {
        if (lhs.major > rhs.major) return .gt;
        if (lhs.major < rhs.major) return .lt;
        if (lhs.minor > rhs.minor) return .gt;
        if (lhs.minor < rhs.minor) return .lt;
        return .eq;
    }
    pub fn parse(str: []const u8) !@This() {
        var it = std.mem.splitScalar(u8, str, '.');
        const major_str = it.next() orelse return error.BadFormat;
        const minor_str = it.next() orelse return error.BadFormat;
        if (it.next() != null) return error.BadFormat;

        return .{
            .major = std.fmt.parseUnsigned(u8, major_str, 10) catch return error.BadMajor,
            .minor = std.fmt.parseUnsigned(u8, minor_str, 10) catch return error.BadMinor,
        };
    }
};

pub const Extension = struct {
    name: []const u8,
    require: std.ArrayListUnmanaged(Require) = .{},
    remove: std.ArrayListUnmanaged(Remove) = .{},
};

pub const InterfaceElement = union(enum) {
    type: []const u8,
    command: []const u8,
    @"enum": []const u8,
};

pub const Alias = union(enum) {
    command: []const u8,
    @"enum": []const u8,
};
