const std = @import("std");
const clap = @import("clap");
const builtin = @import("builtin");
const glregistry = @import("./glregistry.zig");

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
    const use_c_allocator = builtin.link_libc and builtin.mode != .Debug;

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpalloc.deinit();

    const gpallocator = if (use_c_allocator)
        std.heap.c_allocator
    else
        gpalloc.allocator();

    const parsers = comptime .{
        .str = clap.parsers.string,
        .file = clap.parsers.string,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpallocator,
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
    const registry_buffer = try cwd.readFileAlloc(
        gpallocator,
        res.positionals[0],
        std.math.maxInt(usize),
    );
    defer gpallocator.free(registry_buffer);

    var registry_buffer_stream = std.io.fixedBufferStream(registry_buffer);

    var registry = try glregistry.parseRegistry(
        gpallocator,
        registry_buffer_stream.reader(),
    );
    defer registry.deinit();

    std.debug.print("enum groups: {}, enums: {}, commands: {}, extensions: {}\n", .{
        registry.enumgroups.size,
        registry.enums.size,
        registry.commands.size,
        registry.extensions.size,
    });
    var out_module = try cwd.createFile("./out.zig", .{});
    defer out_module.close();

    var out_module_stream = std.io.bufferedWriter(out_module.writer());
    defer out_module_stream.flush() catch {
        std.debug.print("Failed to flush\n", .{});
    };

    try glregistry.generateModule(
        gpallocator,
        &registry,
        .gl,
        .{ .major = 1, .minor = 1, .patch = 0 },
        false,
        out_module_stream.writer(),
    );
}
