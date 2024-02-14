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
        version: ?std.SemanticVersion = null,
        core: bool = false,
    };

    pub const ApiSpecError = error{
        ApiFieldRequired,
        BadApiTypeField,
        BadApiCompatField,
        BadApiVersionField,
        TooManyFields,
    };

    pub fn apiSpec(in: []const u8) ApiSpecError!ApiSpec {
        var spec = ApiSpec{ .api = .gl };

        var it = std.mem.splitScalar(u8, in, ':');
        const api_str = it.next() orelse return ApiSpecError.ApiFieldRequired;

        spec.api = std.meta.stringToEnum(glregistry.Registry.Feature.Api, api_str) orelse
            return ApiSpecError.BadApiTypeField;

        const field2 = std.mem.trim(u8, it.next() orelse return spec, " ");
        var compat_field = false;

        if (std.mem.eql(u8, field2, "core") or std.mem.eql(u8, field2, "compat")) {
            spec.core = std.mem.eql(u8, field2, "core");
            compat_field = true;
        }

        const version_field = if (compat_field)
            it.next() orelse return spec
        else
            field2;

        var vit = std.mem.splitScalar(u8, version_field, '.');
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

        spec.version = std.SemanticVersion{ .major = major, .minor = minor, .patch = 0 };

        return spec;
    }
};

pub fn main() !void {
    const stderr = std.io.getStdErr();
    const params = comptime clap.parseParamsComptime(
        \\-h, --help           Show this help message
        \\-o, --output <file>  Destination path for the generated module (default: prints to stdout)
        \\--api <apispec>      Api to generate
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
        .str = clap_parsers.string,
        .file = clap_parsers.string,
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

    std.log.info("Loaded registry (enum groups: {}, enums: {}, commands: {}, extensions: {})\n", .{
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
        std.debug.print("Failed to flush\n", .{});
    };

    try glregistry.generateModule(
        gpallocator,
        &registry,
        res.args.api.api,
        res.args.api.version,
        res.args.api.core,
        out_module_stream.writer(),
    );
}
