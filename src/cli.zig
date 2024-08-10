const std = @import("std");
const clap = @import("clap");
const glregistry = @import("./glregistry.zig");

pub const cli_params = clap.parseParamsComptime(
    \\-h, --help           Show this help message
    \\-o, --output <file>  Destination path for the generated module (default: prints to stdout)
    \\-v, --verbose        Print info and debug messages
    \\<registry>
);

pub const parsers = .{
    .str = clap.parsers.string,
    .file = clap.parsers.string,
    .registry = clap.parsers.string,
};

pub const CliError = error{
    MissingRequiredOption,
};

pub fn printHelp(params: []const clap.Param(clap.Help)) !void {
    const stderr = std.io.getStdErr();
    const writer = stderr.writer();

    try clap.usage(writer, clap.Help, params);
    try writer.writeByte('\n');

    try clap.help(writer, clap.Help, params, .{});
}

pub const log = std.log.scoped(.cli);

pub fn reportDiagnostic(diag: clap.Diagnostic, err: anyerror) void {
    var longest = diag.name.longest();
    if (longest.kind == .positional)
        longest.name = diag.arg;

    switch (err) {
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
