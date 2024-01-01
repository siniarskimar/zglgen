const std = @import("std");

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
}
