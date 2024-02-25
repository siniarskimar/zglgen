const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zig_clap = b.dependency("clap", .{});

    const exe = b.addExecutable(.{
        .name = "zglgen",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.addModule("clap", zig_clap.module("clap"));
    exe.linkLibC();
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    buildExamples(b, target, optimize, exe);

    const test_step = b.step("test", "Run unit tests");
    const unit_test = b.addTest(.{
        .root_source_file = .{ .path = "src/test.zig" },
        .target = target,
        .optimize = optimize,
    });
    const test_run = b.addRunArtifact(unit_test);
    test_step.dependOn(&test_run.step);
}

fn buildExamples(
    b: *std.Build,
    target: anytype,
    optimize: anytype,
    zglgen: anytype,
) void {
    const zglgen_cmd = b.addRunArtifact(zglgen);
    zglgen_cmd.addArg("-o");
    const generated_path = zglgen_cmd.addPrefixedOutputFileArg("", "gl_3_2.zig");
    zglgen_cmd.addArgs(&[_][]const u8{ "--api", "gl:3.2" });

    const gen_module = b.createModule(.{
        .source_file = generated_path,
    });

    const example_triangle = b.addExecutable(.{
        .name = "example-triangle",
        .root_source_file = .{ .path = "examples/traingle.zig" },
        .target = target,
        .optimize = optimize,
    });
    example_triangle.linkSystemLibrary("glfw");
    example_triangle.addSystemIncludePath(.{ .path = "/usr/local/include" });
    example_triangle.linkLibC();
    example_triangle.addModule("gl", gen_module);
    b.installArtifact(example_triangle);

    const run_example_triangle = b.addRunArtifact(example_triangle);
    run_example_triangle.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_example_triangle.addArgs(args);
    }

    const run_example_triangle_step = b.step("example-triangle", "Run examples/triangle.zig");
    run_example_triangle_step.dependOn(&run_example_triangle.step);
}
