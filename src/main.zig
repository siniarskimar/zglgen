const std = @import("std");
const clap = @import("clap");
const builtin = @import("builtin");
const glregistry = @import("./glregistry.zig");
const zig_generator = @import("./zig_generator.zig");

pub const std_options = std.Options{
    .log_level = .info,
};

fn printHelp(params: []const clap.Param(clap.Help)) !void {
    const stderr = std.io.getStdErr();
    const writer = stderr.writer();

    try clap.usage(writer, clap.Help, params);
    try writer.writeByte('\n');

    try clap.help(writer, clap.Help, params, .{});

    try writer.writeAll(
        \\<apispec> is a combination of <api>:<version>
        \\<api> has to be one of:
        \\
    );
    inline for (std.meta.fields(glregistry.Registry.Feature.Api)) |field| {
        try writer.writeAll(std.fmt.comptimePrint(" - {s}\n", .{field.name}));
    }
}

const clap_parsers = struct {
    usingnamespace clap.parsers;

    const ApiSpec = struct {
        api: glregistry.Registry.Feature.Api,
        version: std.SemanticVersion,
    };

    pub const ApiSpecError = error{
        ApiFieldRequired,
        ApiVersionRequired,
        BadApiTypeField,
        BadApiVersionField,
        TooManyFields,
    };

    pub fn apiSpec(in: []const u8) ApiSpecError!ApiSpec {
        var it = std.mem.splitScalar(u8, in, ':');
        const api_str = it.next() orelse return ApiSpecError.ApiFieldRequired;
        const api_ver = it.next() orelse return ApiSpecError.ApiVersionRequired;

        const api = std.meta.stringToEnum(glregistry.Registry.Feature.Api, api_str) orelse
            return ApiSpecError.BadApiTypeField;

        var vit = std.mem.splitScalar(u8, api_ver, '.');
        const major = std.fmt.parseInt(
            usize,
            vit.next() orelse return ApiSpecError.BadApiVersionField,
            10,
        ) catch return ApiSpecError.BadApiVersionField;

        const minor = std.fmt.parseInt(
            usize,
            vit.next() orelse return ApiSpecError.BadApiVersionField,
            10,
        ) catch return ApiSpecError.BadApiVersionField;

        if (vit.index != null)
            return ApiSpecError.BadApiVersionField;

        if (it.index != null) {
            return ApiSpecError.TooManyFields;
        }

        return .{
            .api = api,
            .version = std.SemanticVersion{ .major = major, .minor = minor, .patch = 0 },
        };
    }
};

fn getCacheDirPath(allocator: std.mem.Allocator) ![]const u8 {
    var envmap = try std.process.getEnvMap(allocator);
    defer envmap.deinit();

    switch (builtin.os.tag) {
        .macos => {
            const home_path = envmap.get("HOME") orelse return error.CacheDirNotFound;
            return try std.fs.path.join(allocator, &[_][]const u8{ home_path, ".cache", "zglgen" });
        },
        .linux, .freebsd => {
            if (envmap.get("XDG_CACHE_DIR")) |xdg_cache_path| {
                return try std.fs.path.join(allocator, &[_][]const u8{ xdg_cache_path, "zglgen" });
            }
            const home_path = envmap.get("HOME") orelse return error.CacheDirNotFound;
            return try std.fs.path.join(allocator, &[_][]const u8{ home_path, ".cache", "zglgen" });
        },
        .windows => {
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

fn findProgramInPath(allocator: std.mem.Allocator, program_name: []const u8) !?[]const u8 {
    var envmap = try std.process.getEnvMap(allocator);
    defer envmap.deinit();

    var buffer = std.ArrayList(u8).init(allocator);
    defer buffer.deinit();

    var fba_buffer = [_]u8{0} ** 512;
    var fba = std.heap.FixedBufferAllocator.init(&fba_buffer);

    const env_path = envmap.get("PATH") orelse return error.EnvPathNotSet;

    try buffer.appendSlice(program_name);
    if (builtin.os.tag == .windows) {
        try buffer.appendSlice(".exe");
    }

    var it = std.mem.splitScalar(
        u8,
        env_path,
        if (builtin.os.tag == .windows) ';' else ':',
    );

    while (it.next()) |path_seg| {
        const candidate = try std.fs.path.join(fba.allocator(), &[_][]const u8{ path_seg, buffer.items });
        defer fba.allocator().free(candidate);

        const file = std.fs.openFileAbsolute(candidate, .{}) catch |err| switch (err) {
            error.FileNotFound => continue,
            else => return err,
        };
        defer file.close();

        buffer.clearRetainingCapacity();
        try buffer.appendSlice(candidate);
        break;
    }

    return try buffer.toOwnedSlice();
}

fn getGlRegistry(allocator: std.mem.Allocator, filepath: ?[]const u8, no_cache: bool) !std.io.StreamSource {
    const MAX_SIZE = 5 * 1024 * 1024; // 5 MiB

    if (filepath) |fp| {
        std.log.info("Using '{s}' as registry", .{fp});
        const file = try std.fs.cwd().openFile(fp, .{});

        return .{ .file = file };
    }
    const cache_dir_path = try getCacheDirPath(allocator);
    defer allocator.free(cache_dir_path);

    var cache_dir: ?std.fs.Dir = std.fs.openDirAbsolute(cache_dir_path, .{}) catch |err| switch (err) {
        error.FileNotFound => try std.fs.cwd().makeOpenPath(cache_dir_path, .{}),
        else => blk: {
            if (!no_cache) {
                std.log.warn("Failed to open cache directory {s}, cache disabled {s}", .{ cache_dir_path, @errorName(err) });
            }
            break :blk null;
        },
    };
    defer if (cache_dir) |*dir| dir.close();

    blk: {
        if (!no_cache) if (cache_dir) |dir| {
            const cached_file = dir.openFile("gl.xml", .{}) catch |err| switch (err) {
                error.FileNotFound => break :blk,
                else => return err,
            };
            errdefer cached_file.close();
            const stat = try cached_file.stat();

            const up_to_date = stat.mtime + std.time.ns_per_day > std.time.nanoTimestamp();
            if (up_to_date) {
                std.log.info("Using cached GL registry", .{});
                return .{ .file = cached_file };
            }
            cached_file.close();
        };
    }

    var http = std.http.Client{ .allocator = allocator };
    defer http.deinit();

    // In 0.12.0 the following code is replaced by std.http.Client.fetch

    const uri = comptime try std.Uri.parse("https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml");
    var response_buffer = std.ArrayList(u8).init(allocator);
    defer response_buffer.deinit();

    var stderr = std.ArrayList(u8).init(allocator);
    defer stderr.deinit();

    std.log.info("Fetching {s}://{s}{s}", .{ uri.scheme, uri.host.?, uri.path });

    const curl_path = try findProgramInPath(allocator, "curl");
    defer if (curl_path) |p| allocator.free(p);

    if (curl_path) |prog_path| {
        var process = std.process.Child.init(&[_][]const u8{
            prog_path,
            std.fmt.comptimePrint("{s}://{s}{s}", .{ uri.scheme, uri.host.?, uri.path }),
        }, allocator);
        process.stdout_behavior = .Pipe;
        process.stderr_behavior = .Pipe;

        try process.spawn();
        try process.collectOutput(&response_buffer, &stderr, MAX_SIZE);

        const term = try process.wait();
        if (term != .Exited or term == .Exited and term.Exited != 0) {
            std.log.err("curl exited abnormaly: \n{s}", .{stderr.items});
            return error.FetchFail;
        }
    } else {
        const wget_path = try findProgramInPath(allocator, "wget") orelse {
            std.log.err("Cannot download GL registry, no 'curl' or 'wget' found in $PATH", .{});
            return error.PathSearchFail;
        };
        defer allocator.free(wget_path);

        var process = std.process.Child.init(&[_][]const u8{
            wget_path,
            std.fmt.comptimePrint("{s}://{s}{s}", .{ uri.scheme, uri.host.?, uri.path }),
            "-O",
            "-",
        }, allocator);
        process.stdout_behavior = .Pipe;
        process.stderr_behavior = .Pipe;

        try process.spawn();
        try process.collectOutput(&response_buffer, &stderr, MAX_SIZE);

        const term = try process.wait();
        if (term != .Exited or term == .Exited and term.Exited != 0) {
            std.log.err("wget exited abnormaly: \n{s}", .{stderr.items});
            return error.FetchFail;
        }
    }

    if (cache_dir) |dir| {
        const file = try dir.createFile("gl.xml", .{});
        defer file.close();

        try file.writeAll(response_buffer.items);
    }

    return .{ .buffer = std.io.fixedBufferStream(try response_buffer.toOwnedSlice()) };
}

pub fn main() !void {
    const stderr = std.io.getStdErr();
    const params = comptime clap.parseParamsComptime(
        \\-h, --help           Show this help message
        \\-o, --output <file>  Destination path for the generated module (default: prints to stdout)
        \\--api <apispec>      Api to generate
        \\--registry <file>    File path to OpenGL registry (default: downloads https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml)
        \\-c, --no-cache       Disables caching of GL registry
        \\<extension>...       Additional extensions
    );
    const use_c_allocator = builtin.link_libc and builtin.mode != .Debug;

    var gpalloc = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpalloc.deinit();

    const gpallocator = if (use_c_allocator)
        std.heap.c_allocator
    else
        gpalloc.allocator();

    const parsers = comptime .{
        .str = clap_parsers.string,
        .file = clap_parsers.string,
        .extension = clap_parsers.string,
        .apispec = clap_parsers.apiSpec,
    };

    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, parsers, .{
        .diagnostic = &diag,
        .allocator = gpallocator,
    }) catch |err| {
        diag.report(stderr.writer(), err) catch {};
        return;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        return printHelp(&params);
    }

    const apispec = res.args.api orelse {
        std.log.err("'--api' Option is required!", .{});
        return error.MissingOption;
    };

    const cwd = std.fs.cwd();
    var registry_stream = try getGlRegistry(gpallocator, res.args.registry, res.args.@"no-cache" != 0);
    defer switch (registry_stream) {
        .buffer => |*fba| gpallocator.free(fba.buffer),
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
    defer out_module_stream.flush() catch {
        std.log.err(
            "Failed to flush {s}\n",
            .{
                if (res.args.output) |output| output else "stdout",
            },
        );
    };

    try zig_generator.generateModule(
        gpallocator,
        &registry,
        .{ .api = apispec.api, .number = apispec.version },
        res.positionals,
        out_module_stream.writer(),
    );

    if (res.args.output) |output| {
        std.log.info("Generated '{s}'", .{output});
    }
}
