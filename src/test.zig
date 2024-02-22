const std = @import("std");
const glregistry = @import("./glregistry.zig");
const xml = @import("./xml.zig");

test {
    std.testing.refAllDecls(glregistry);
    std.testing.refAllDecls(xml);
}
