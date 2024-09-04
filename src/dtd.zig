//! Contains structures that are used by the registry parser

const std = @import("std");

pub const Element = union(enum) {
    // top-level
    registry: Registry,

    // containers
    types: void,
    kinds: void,
    groups: void, // not in use
    enums: Enums,
    commands: void,
    extensions: void,

    type: void,
    kind: void,
    group: void, // not in use
    command: Command,
    @"enum": Enum,
    extension: Extension,
    feature: Feature,

    // <type> related
    apientry: void,

    // <enums> related
    unused: void,

    // <command> related
    proto: void,
    param: Command.Param,
    ptype: []const u8,
    glx: void,
    vecequiv: void,

    alias: void,

    // <feature> and <extension> related
    require: void,
    remove: void,

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
    };
};

pub const Feature = struct {
    api: []const u8,
    name: []const u8,
    number: FeatureNumber,
    require: std.ArrayListUnmanaged(InterfaceElement) = .{},
    remove: std.ArrayListUnmanaged(InterfaceElement) = .{},
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
};

pub const Extension = struct {
    name: []const u8,
    require: std.ArrayListUnmanaged(InterfaceElement) = .{},
    remove: std.ArrayListUnmanaged(InterfaceElement) = .{},
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
