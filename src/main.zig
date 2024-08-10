const std = @import("std");
const clap = @import("clap");
const cli = @import("./cli.zig");
const builtin = @import("builtin");
const glregistry = @import("./glregistry.zig");
const zig_generator = @import("./zig_generator.zig");

pub const std_options = std.Options{
    .log_level = .info,
    .logFn = logFn,
};

var log_verbose: bool = false;

pub fn logFn(
    comptime message_level: std.log.Level,
    comptime scope: @TypeOf(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    if (!log_verbose and message_level != .err) {
        return;
    }
    std.log.defaultLog(message_level, scope, format, args);
}

pub fn main() !u8 {
    const use_c_allocator = builtin.link_libc and builtin.mode != .Debug;

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer if (!use_c_allocator) {
        // this defer should be discarded when use_c_allocator is true
        _ = gpalloc.deinit();
    };

    const gpallocator = if (use_c_allocator)
        std.heap.c_allocator
    else
        gpalloc.allocator();

    var diag = clap.Diagnostic{};
    var clapres = clap.parse(clap.Help, &cli.cli_params, cli.parsers, .{
        .diagnostic = &diag,
        .allocator = gpallocator,
    }) catch |err| {
        cli.reportDiagnostic(diag, err);
        return err;
    };
    defer clapres.deinit();

    if (clapres.args.help != 0) {
        try cli.printHelp(&cli.cli_params);
        return 1;
    }

    if (clapres.positionals.len == 0) {
        cli.log.err("registry argument is required", .{});
        return cli.CliError.MissingRequiredOption;
    }

    const registry_arg: []const u8 = clapres.positionals[0];

    if (clapres.args.verbose != 0) {
        log_verbose = true;
    }

    const registry_file = std.fs.cwd().openFile(registry_arg, .{}) catch |err| {
        std.log.err("failed to open registry xml: {}", .{err});
        if (err == error.FileNotFound) {
            return 1;
        }
        return err;
    };
    defer registry_file.close();

    var registry = try glregistry.Registry.parse(gpallocator, registry_file);
    defer registry.deinit();

    return 0;
}
