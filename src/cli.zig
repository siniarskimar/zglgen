const std = @import("std");
const clap = @import("clap");
const glregistry = @import("./glregistry.zig");

pub const cli_params = clap.parseParamsComptime(
    \\-h, --help           Show this help message
    \\-o, --output <file>  Destination path for the generated module (default: prints to stdout)
    \\--api <apispec>      (required) Specify the kind of GL API to generate 
    \\--registry <file>    File path to OpenGL registry (default: downloads https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml)
    \\-c, --no-cache       Disable caching of GL registry
    \\-v, --verbose        Print info and debug messages
    \\<extension>...       Additional extensions
);

pub const parsers = .{
    .str = clap.parsers.string,
    .file = filepathParser,
    .extension = clap.parsers.string,
    .apispec = apiSpecParser,
};

pub const ApiSpec = struct {
    api: glregistry.Registry.Feature.Api,
    version: std.SemanticVersion,

    pub fn toFeatureKey(self: ApiSpec) glregistry.FeatureKey {
        return .{ .api = self.api, .number = self.version };
    }
};

pub const CliError = error{
    MissingRequiredOption,
};

pub const ApiSpecParseError = error{
    ApiFieldRequired,
    ApiVersionRequired,
    BadApiTypeField,
    BadApiVersionField,
    TooManyFields,
};

pub fn apiSpecParser(in: []const u8) ApiSpecParseError!ApiSpec {
    var it = std.mem.splitScalar(u8, in, ':');
    const api_str = it.next() orelse return ApiSpecParseError.ApiFieldRequired;
    const api_ver = it.next() orelse return ApiSpecParseError.ApiVersionRequired;

    const api = std.meta.stringToEnum(glregistry.Registry.Feature.Api, api_str) orelse
        return ApiSpecParseError.BadApiTypeField;

    var vit = std.mem.splitScalar(u8, api_ver, '.');
    const major = std.fmt.parseInt(
        usize,
        vit.next() orelse return ApiSpecParseError.BadApiVersionField,
        10,
    ) catch return ApiSpecParseError.BadApiVersionField;

    const minor = std.fmt.parseInt(
        usize,
        vit.next() orelse return ApiSpecParseError.BadApiVersionField,
        10,
    ) catch return ApiSpecParseError.BadApiVersionField;

    if (vit.index != null)
        return ApiSpecParseError.BadApiVersionField;

    if (it.index != null) {
        return ApiSpecParseError.TooManyFields;
    }

    return .{
        .api = api,
        .version = std.SemanticVersion{ .major = major, .minor = minor, .patch = 0 },
    };
}

pub const FileParseError = error{FilepathEmpty};
pub fn filepathParser(in: []const u8) FileParseError![]const u8 {
    if (std.mem.trim(u8, in, " ").len == 0) {
        return FileParseError.FilepathEmpty;
    }
    return in;
}

pub fn printHelp(params: []const clap.Param(clap.Help)) !void {
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

const log = std.log.scoped(.cli);

pub fn reportDiagnostic(diag: clap.Diagnostic, err: anyerror) void {
    var longest = diag.name.longest();
    if (longest.kind == .positional)
        longest.name = diag.arg;

    switch (err) {
        ApiSpecParseError.ApiFieldRequired => log.err(
            "'{s}{s}': <apispec> requires an API name",
            .{ longest.kind.prefix(), longest.name },
        ),
        ApiSpecParseError.ApiVersionRequired => log.err(
            "'{s}{s}': <apispec> requires an API version field",
            .{ longest.kind.prefix(), longest.name },
        ),
        ApiSpecParseError.BadApiTypeField => log.err(
            "'{s}{s}': Invalid API name",
            .{ longest.kind.prefix(), longest.name },
        ),
        ApiSpecParseError.BadApiVersionField => log.err(
            "'{s}{s}': Invalid API version",
            .{ longest.kind.prefix(), longest.name },
        ),
        ApiSpecParseError.TooManyFields => log.err(
            "<apispec> of '{s}{s}' recieved too many fields",
            .{ longest.kind.prefix(), longest.name },
        ),
        FileParseError.FilepathEmpty => log.err(
            "Filepath of '{s}{s}' cannot be empty",
            .{ longest.kind.prefix(), longest.name },
        ),
        clap.streaming.Error.DoesntTakeValue => log.err(
            "The argument '{s}{s}' does not take a value",
            .{ longest.kind.prefix(), longest.name },
        ),
        clap.streaming.Error.MissingValue => log.err(
            "The argument '{s}{s}' requires a value but none was supplied",
            .{ longest.kind.prefix(), longest.name },
        ),
        clap.streaming.Error.InvalidArgument => log.err(
            "Invalid argument '{s}{s}'",
            .{ longest.kind.prefix(), longest.name },
        ),
        else => std.log.err("Error while parsing arguments: {s}", .{@errorName(err)}),
    }
}
