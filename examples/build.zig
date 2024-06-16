const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dep_zglgen = b.dependency("zglgen", .{ .optimize = .ReleaseSafe });
    const zglgen = dep_zglgen.artifact("zglgen");

    {
        const run_zglgen = b.addRunArtifact(zglgen);
        run_zglgen.addArg("-o");
        const bindings_path = run_zglgen.addOutputFileArg("gl_3_2.zig");
        run_zglgen.addArgs(&.{ "--api", "gl:3.2", "GL_KHR_debug" });

        const bindings_mod = b.createModule(.{ .root_source_file = bindings_path });

        const exe_triangle = b.addExecutable(.{
            .name = "example-triangle",
            .root_source_file = b.path("triangle.zig"),
            .target = target,
            .optimize = optimize,
        });
        exe_triangle.linkSystemLibrary("glfw");
        exe_triangle.linkLibC();
        exe_triangle.root_module.addImport("gl", bindings_mod);

        b.installArtifact(exe_triangle);
    }
}
