const std = @import("std");

pub fn build(b: *std.Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const dep_zglgen = b.dependency("zglgen", .{ .optimize = .ReleaseSafe });

    // Get OpenGL zglgen generator executable
    const zglgen = dep_zglgen.artifact("zglgen");

    // Use zglgen as part of the build process
    const run_zglgen = b.addRunArtifact(zglgen);
    run_zglgen.addArg("-o");
    const bindings_path = run_zglgen.addOutputFileArg("gl.zig");

    // You can also add https://github.com/KhronosGroup/OpenGL-Registry as a dependency
    // and dep.path("xml/gl.xml") to obtain GL registry file
    run_zglgen.addFileArg(try downloadGlRegistry(b));

    // Create a module from the generated file
    const bindings_mod = b.createModule(.{ .root_source_file = bindings_path });

    // To use it, add an import to the root module
    // exe.root_module.addImport("gl", bindings_mod);

    const exe_triangle = b.addExecutable(.{
        .name = "example-triangle",
        .root_source_file = b.path("triangle.zig"),
        .target = target,
        .optimize = optimize,
    });
    exe_triangle.linkSystemLibrary("glfw");
    exe_triangle.linkLibC();
    exe_triangle.root_module.addImport("gl", bindings_mod); // <<

    b.installArtifact(exe_triangle);
}

fn downloadGlRegistry(b: *std.Build) !std.Build.LazyPath {
    const curl_path = b.findProgram(&.{"curl"}, &.{}) catch |err| {
        std.log.err("program 'curl' not found", .{});
        return err;
    };

    const run_curl = b.addSystemCommand(&.{
        curl_path,
        "https://github.com/KhronosGroup/OpenGL-Registry/raw/a959d350b027246018e18a0cf7e2144f8a113def/xml/gl.xml",
        "-o",
    });
    return run_curl.addOutputFileArg("gl.xml");
}
