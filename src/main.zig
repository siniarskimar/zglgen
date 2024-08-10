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

pub fn main() !void {
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
    var res = clap.parse(clap.Help, &cli.cli_params, cli.parsers, .{
        .diagnostic = &diag,
        .allocator = gpallocator,
    }) catch |err| {
        cli.reportDiagnostic(diag, err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return cli.printHelp(&cli.cli_params);
    }

    if (res.args.verbose != 0) {
        log_verbose = true;
    }

    const apispec: cli.ApiSpec = res.args.api orelse {
        std.log.err("'--api' option is required!", .{});
        return cli.CliError.MissingRequiredOption;
    };

    const cwd = std.fs.cwd();
    var registry_stream = try getGlRegistry(gpallocator, res.args.registry, res.args.@"no-cache" != 0);
    defer switch (registry_stream) {
        .const_buffer => |*fbs| gpallocator.free(fbs.buffer),
        .file => |file| file.close(),
        else => {},
    };

    var buffered_registry_stream = std.io.bufferedReader(registry_stream.reader());

    var registry = try glregistry.parseRegistry(
        gpallocator,
        buffered_registry_stream.reader(),
    );
    defer registry.deinit();

    std.log.info("Loaded registry (enum groups: {}, enums: {}, commands: {}, extensions: {})", .{
        registry.enumgroups.size,
        registry.enums.size,
        registry.commands.size,
        registry.extensions.size,
    });

    var out_module = if (res.args.output) |output|
        try cwd.createFile(output, .{})
    else
        std.io.getStdOut();

    defer out_module.close();

    var out_module_stream = std.io.bufferedWriter(out_module.writer());
    defer out_module_stream.flush() catch |err| {
        std.log.err(
            "Failed to flush {s} ({s})\n",
            .{
                if (res.args.output) |output|
                    output
                else
                    "stdout",
                @errorName(err),
            },
        );
    };

    zig_generator.generateModule(
        gpallocator,
        &registry,
        apispec.toFeatureKey(),
        res.positionals,
        out_module_stream.writer(),
    ) catch |err| switch (err) {
        error.FeatureNotFound => {
            std.log.err(
                "Feature '{} {}.{}' not found in the registry",
                .{ apispec.api, apispec.version.major, apispec.version.minor },
            );
            return err;
        },
        else => return err,
    };

    if (res.args.output) |output| {
        std.log.info("Generated '{s}'", .{output});
    }
}
