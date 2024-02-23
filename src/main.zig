const std = @import("std");
const clap = @import("clap");
const builtin = @import("builtin");
const glregistry = @import("./glregistry.zig");

pub const log_level: std.log.Level = .err;

fn printHelp(params: []const clap.Param(clap.Help)) !void {
    try clap.usage(std.io.getStdErr().writer(), clap.Help, params);
    try std.io.getStdErr().writer().writeByte('\n');

    try clap.help(std.io.getStdErr().writer(), clap.Help, params, .{});
}

const clap_parsers = struct {
    usingnamespace clap.parsers;

    const ApiSpec = struct {
        api: glregistry.Registry.Feature.Api,
        version: std.SemanticVersion,
        core: bool = false,
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

fn loadGlRegistry(allocator: std.mem.Allocator, filepath: ?[]const u8) ![]const u8 {
    const MAX_SIZE = 5 * 1024 * 1024; // 5 MiB

    if (filepath) |fp| {
        std.log.info("Using '{s}' as registry", .{fp});
        var file = try std.fs.cwd().openFile(fp, .{});
        defer file.close();

        return try file.readToEndAlloc(allocator, MAX_SIZE);
    }

    var http = std.http.Client{ .allocator = allocator };
    defer http.deinit();

    // In 0.12.0 the following code is replaced by std.http.Client.fetch

    const uri = comptime try std.Uri.parse("https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml");

    std.log.info("Fetching {s}://{s}/{s}", .{ uri.scheme, uri.host.?, uri.path });

    var headers = std.http.Headers{ .allocator = allocator };
    defer headers.deinit();

    try headers.append("accept", "*/*");

    var request = try http.request(.GET, uri, headers, .{});
    defer request.deinit();

    try request.start();
    try request.wait();

    std.log.info("Fetch done", .{});

    const reader = request.reader();
    const response = try reader.readAllAlloc(allocator, MAX_SIZE);

    return response;
}

pub fn main() !void {
    const stderr = std.io.getStdErr();
    const params = comptime clap.parseParamsComptime(
        \\-h, --help           Show this help message
        \\-o, --output <file>  Destination path for the generated module (default: prints to stdout)
        \\--api <apispec>      Api to generate
        \\--registry <file>    File path to OpenGL registry (default: downloads https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml)
        \\<extension>...        Additional extensions
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
    const registry_buffer = try loadGlRegistry(gpallocator, res.args.registry);
    defer gpallocator.free(registry_buffer);

    var registry_buffer_stream = std.io.fixedBufferStream(registry_buffer);

    var registry = try glregistry.parseRegistry(
        gpallocator,
        registry_buffer_stream.reader(),
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

    try glregistry.generateModule(
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
