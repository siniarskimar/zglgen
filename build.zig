const std = @import("std");

pub fn build(b: *std.Build) void {
    const build_examples = b.option(bool, "build-examples", "Whenever to build examples") orelse true;
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const zig_clap = b.dependency("clap", .{
        .target = target,
        .optimize = optimize,
    });

    const exe = b.addExecutable(.{
        .name = "zglgen",
        .root_source_file = .{ .path = "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    exe.root_module.addImport("clap", zig_clap.module("clap"));
    exe.linkLibC();
    b.installArtifact(exe);

    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());

    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_cmd.step);

    if (build_examples) {
        buildExamples(b, target, optimize, exe);
    }

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
    const install_examples = b.option(bool, "install-examples", "Whenever to put examples into install directory") orelse false;

    const zglgen_cmd = b.addRunArtifact(zglgen);
    zglgen_cmd.addArg("-o");
    const generated_path = zglgen_cmd.addOutputFileArg("gl_3_2.zig");
    zglgen_cmd.addArgs(&[_][]const u8{ "--api", "gl:3.2" });
    zglgen_cmd.addArg("GL_KHR_debug");

    const gen_module = b.createModule(.{
        .root_source_file = generated_path,
    });

    const example_triangle = b.addExecutable(.{
        .name = "example-triangle",
        .root_source_file = .{ .path = "examples/triangle.zig" },
        .target = target,
        .optimize = optimize,
    });
    example_triangle.linkSystemLibrary("glfw");
    example_triangle.linkLibC();
    example_triangle.root_module.addImport("gl", gen_module);

    const run_example_triangle = b.addRunArtifact(example_triangle);
    run_example_triangle.cwd = .{ .path = b.pathFromRoot("examples/") };

    if (install_examples) {
        b.installArtifact(example_triangle);
    }

    if (b.args) |args| {
        run_example_triangle.addArgs(args);
    }

    const run_example_triangle_step = b.step("example-triangle", "Run examples/triangle.zig");
    run_example_triangle_step.dependOn(&run_example_triangle.step);
}
