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

fn getCacheDirPath(allocator: std.mem.Allocator) ![]const u8 {
    var envmap = try std.process.getEnvMap(allocator);
    defer envmap.deinit();

    switch (builtin.os.tag) {
        .macos => {
            // $HOME/.cache/zglgen
            const home_path = envmap.get("HOME") orelse return error.CacheDirNotFound;
            return try std.fs.path.join(allocator, &[_][]const u8{ home_path, ".cache", "zglgen" });
        },
        .linux, .freebsd => {
            // $XDG_CACHE_DIR/zglgen
            // $HOME/.cache/zglgen
            if (envmap.get("XDG_CACHE_DIR")) |xdg_cache_path| {
                return try std.fs.path.join(allocator, &[_][]const u8{ xdg_cache_path, "zglgen" });
            }
            const home_path = envmap.get("HOME") orelse return error.CacheDirNotFound;
            return try std.fs.path.join(allocator, &[_][]const u8{ home_path, ".cache", "zglgen" });
        },
        .windows => {
            // $LOCALAPPDATA/zglgen
            // $APPDATA/zglgen
            // $HOMEPATH/.cache/zglgen
            // $USERPROFILE/.cache/zglgen
            const appdata_path: ?[]const u8 = envmap.get("LOCALAPPDATA") orelse envmap.get("APPDATA");
            if (appdata_path) |path| {
                return try std.fs.path.join(allocator, &[_][]const u8{ path, "zglgen" });
            }
            const home_path = envmap.get("HOMEPATH") orelse envmap.get("USERPROFILE") orelse return error.CacheDirNotFound;
            return try std.fs.path.join(allocator, &[_][]const u8{ home_path, ".cache", "zglgen" });
        },
        else => return error.CacheDirNotFound,
    }
}

fn fetchGlRegistry(allocator: std.mem.Allocator) !std.ArrayList(u8) {
    const url = "https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml";
    var response_buffer = std.ArrayList(u8).init(allocator);
    errdefer response_buffer.deinit();

    std.log.info("Fetching {s}", .{url});

    const FetchProgram = struct {
        name: []const u8,
        argv: []const []const u8,
    };

    const programs = [_]FetchProgram{
        .{ .name = "curl", .argv = &[_][]const u8{url} },
        .{ .name = "wget", .argv = &[_][]const u8{ url, "-O", "-" } },
    };

    var argv = std.ArrayList([]const u8).init(allocator);
    defer argv.deinit();

    for (programs) |program| {
        try argv.append(program.name);
        try argv.appendSlice(program.argv);

        const result = std.process.Child.run(.{
            .argv = argv.items,
            .allocator = allocator,
            .expand_arg0 = .expand,
            .max_output_bytes = 5 * 1024 * 1024,
        }) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };

        defer {
            allocator.free(result.stdout);
            allocator.free(result.stderr);
        }

        const term = result.term;
        if (term != .Exited or term == .Exited and term.Exited != 0) {
            std.log.err("{s} exited abnormaly: \n{s}", .{ program.name, result.stderr });
            return error.FetchFail;
        }

        try response_buffer.appendSlice(result.stdout);
        break;
    } else {
        std.log.err("Could not find 'curl' or 'wget' in $PATH", .{});
        return error.HelperNotFound;
    }

    return response_buffer;
}

fn openCacheDir(allocator: std.mem.Allocator) !std.fs.Dir {
    const cache_dir_path = try getCacheDirPath(allocator);
    defer allocator.free(cache_dir_path);

    if (std.fs.openDirAbsolute(cache_dir_path, .{})) |dir| {
        return dir;
    } else |err| switch (err) {
        std.fs.File.OpenError.FileNotFound => {
            if (std.fs.cwd().makeOpenPath(cache_dir_path, .{})) |dir|
                return dir
            else |errr| {
                std.log.warn(
                    "Failed to create cache directory {s}, cache disabled {s}",
                    .{ cache_dir_path, @errorName(errr) },
                );
                return err;
            }
        },
        else => {
            std.log.warn(
                "Failed to open cache directory {s}, cache disabled ({s})",
                .{ cache_dir_path, @errorName(err) },
            );
            return err;
        },
    }
}

fn openCachedGlRegistry(dir: std.fs.Dir) !std.fs.File {
    const cached_file = try dir.openFile("gl.xml", .{});
    errdefer cached_file.close();
    const stat = try cached_file.stat();

    const up_to_date = (stat.mtime + std.time.ns_per_day) > std.time.nanoTimestamp();
    if (!up_to_date) {
        return error.OutOfDate;
    }
    return cached_file;
}

fn getGlRegistry(allocator: std.mem.Allocator, filepath: ?[]const u8, no_cache: bool) !std.io.StreamSource {
    if (filepath) |fp| {
        std.log.info("Using '{s}' as registry", .{fp});
        const file = try std.fs.cwd().openFile(fp, .{});

        return .{ .file = file };
    }

    var cache_dir: ?std.fs.Dir = if (no_cache)
        null
    else if (openCacheDir(allocator)) |dir|
        dir
    else |_|
        null;

    defer if (cache_dir) |*dir| dir.close();

    if (cache_dir) |dir| {
        if (openCachedGlRegistry(dir)) |file| {
            return .{ .file = file };
        } else |err| switch (err) {
            error.FileNotFound, error.OutOfDate => {},
            else => return err,
        }
    }

    var response_buffer = try fetchGlRegistry(allocator);
    defer response_buffer.deinit();

    if (cache_dir) |dir| {
        const file = try dir.createFile("gl.xml", .{});
        defer file.close();

        try file.writeAll(response_buffer.items);
    }

    return .{
        .const_buffer = std.io.FixedBufferStream([]const u8){
            .buffer = try response_buffer.toOwnedSlice(),
            .pos = 0,
        },
    };
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
