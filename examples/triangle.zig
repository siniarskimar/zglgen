const std = @import("std");
const gl = @import("gl");
const glfw = @cImport({
    @cInclude("GLFW/glfw3.h");
});

const VS_SOURCE: [*c]const u8 =
    \\#version 150
    \\in vec3 Pos;
    \\in vec3 Color;
    \\out vec3 in_fs_color;
    \\void main() {
    \\ gl_Position = vec4(Pos.xyz, 1.0);
    \\ in_fs_color = Color;
    \\}
;
const VS_SOURCE_LEN = @as(gl.GLint, @intCast(std.mem.len(VS_SOURCE)));
const FS_SOURCE: [*c]const u8 =
    \\#version 150
    \\in vec3 in_fs_color;
    \\out vec4 FragColor;
    \\void main() {
    \\ FragColor = vec4(in_fs_color, 1);
    \\}
;
const FS_SOURCE_LEN = @as(gl.GLint, @intCast(std.mem.len(FS_SOURCE)));

fn glfwErrCallback(_: c_int, error_msg: [*c]const u8) callconv(.C) void {
    std.log.err("[GLFW] {s} ", .{error_msg});
}

fn isExtensionSupported(str: []const u8) bool {
    const ext_str = std.mem.span(gl.glGetString(gl.GL_EXTENSIONS) orelse unreachable);
    return std.mem.indexOf(u8, str, ext_str) != null;
}

fn glMessageCallback(
    _: gl.GLenum,
    _: gl.GLenum,
    _: gl.GLuint,
    _: gl.GLenum,
    _: gl.GLsizei,
    message: [*:0]const u8,
    _: ?*const anyopaque,
) callconv(.C) void {
    std.log.debug("[GL] {s}\n", .{message});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    _ = glfw.glfwSetErrorCallback(glfwErrCallback);

    if (glfw.glfwInit() == 0) {
        return error.GLFWInitFailed;
    }
    defer glfw.glfwTerminate();

    glfw.glfwWindowHint(glfw.GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfw.glfwWindowHint(glfw.GLFW_CONTEXT_VERSION_MINOR, 2);
    glfw.glfwWindowHint(glfw.GLFW_OPENGL_PROFILE, glfw.GLFW_OPENGL_CORE_PROFILE);
    glfw.glfwWindowHint(glfw.GLFW_OPENGL_DEBUG_CONTEXT, glfw.GLFW_TRUE);

    const window = glfw.glfwCreateWindow(800, 600, "Hello", null, null) orelse {
        return error.WindowCreationFailed;
    };
    defer glfw.glfwDestroyWindow(window);

    glfw.glfwMakeContextCurrent(window);
    gl.makeProcTableCurrent(try gl.loadGL(glfw.glfwGetProcAddress));

    glfw.glfwSwapInterval(1);
    {
        const version_str = gl.getString(gl.GL_VERSION) orelse "<no version string>";
        std.log.info("OpenGL {s}\n", .{version_str});
    }

    gl.enable(gl.GL_DEBUG_OUTPUT);
    gl.debugMessageCallback(glMessageCallback, null);
    gl.debugMessageControl(gl.GL_DONT_CARE, gl.GL_DONT_CARE, gl.GL_DONT_CARE, 0, null, gl.GL_TRUE);

    var program: gl.GLuint = gl.createProgram();
    {
        var vs: gl.GLuint = gl.createShader(gl.GL_VERTEX_SHADER);
        defer gl.deleteShader(vs);

        var fs: gl.GLuint = gl.createShader(gl.GL_FRAGMENT_SHADER);
        defer gl.deleteShader(fs);

        gl.shaderSource(
            vs,
            1,
            &[_][*c]const u8{VS_SOURCE},
            &[_:0]gl.GLint{ VS_SOURCE_LEN, 0 },
        );
        gl.shaderSource(
            fs,
            1,
            &[_][*c]const u8{FS_SOURCE},
            &[_:0]gl.GLint{ FS_SOURCE_LEN, 0 },
        );
        gl.compileShader(vs);
        gl.compileShader(fs);

        var status: gl.GLuint = 0;
        gl.getShaderiv(vs, gl.GL_COMPILE_STATUS, @ptrCast(&status));
        if (status == 0) {
            var infolog_len: gl.GLint = 0;
            gl.getShaderiv(vs, gl.GL_INFO_LOG_LENGTH, @ptrCast(&infolog_len));

            var buffer = try allocator.alloc(u8, @intCast(infolog_len));
            defer allocator.free(buffer);

            gl.getShaderInfoLog(vs, infolog_len, null, buffer.ptr);
            std.log.err("[GL] Vertex shader compilation failed:\n{s}", .{buffer});
            return error.ShaderCompilationFailed;
        }

        gl.getShaderiv(fs, gl.GL_COMPILE_STATUS, @ptrCast(&status));
        if (status == 0) {
            var infolog_len: gl.GLint = 0;
            gl.getShaderiv(fs, gl.GL_INFO_LOG_LENGTH, @ptrCast(&infolog_len));

            var buffer = try allocator.alloc(u8, @intCast(infolog_len));
            defer allocator.free(buffer);

            gl.getShaderInfoLog(fs, infolog_len, null, buffer.ptr);
            std.log.err("[GL] Fragment shader compilation failed:\n{s}", .{buffer});
            return error.ShaderCompilationFailed;
        }

        gl.attachShader(program, vs);
        gl.attachShader(program, fs);

        gl.bindAttribLocation(program, 0, "Pos");
        gl.bindAttribLocation(program, 1, "Color");
        gl.linkProgram(program);

        gl.getProgramiv(program, gl.GL_LINK_STATUS, @ptrCast(&status));
        if (status == 0) {
            var infolog_len: gl.GLint = 0;
            gl.getProgramiv(program, gl.GL_INFO_LOG_LENGTH, @ptrCast(&infolog_len));

            var buffer = try allocator.alloc(u8, @intCast(infolog_len));
            defer allocator.free(buffer);

            gl.getProgramInfoLog(program, infolog_len, null, buffer.ptr);
            std.log.err("[GL] Program linking failed!\n{s}", .{buffer});
            return error.ProgramLinkingFailed;
        }
        gl.detachShader(program, vs);
        gl.detachShader(program, fs);
    }

    const verticies = [_]f32{
        // position:vec3, color:vec3
        -1, -1, 0.0, 0.0, 0.0, 1.0,
        1,  -1, 0.0, 0.0, 1.0, 0.0,
        0,  1,  0.0, 1.0, 0.0, 0.0,
    };
    const indecies = [_]gl.GLuint{ 0, 1, 2 };

    var vao: gl.GLuint = 0;
    var vbo: gl.GLuint = 0;
    gl.genVertexArrays(1, &vao);
    gl.bindVertexArray(vao);

    gl.genBuffers(1, &vbo);
    gl.bindBuffer(gl.GL_ARRAY_BUFFER, vbo);
    gl.bufferData(gl.GL_ARRAY_BUFFER, @sizeOf([verticies.len]f32), &verticies, gl.GL_STATIC_DRAW);

    gl.enableVertexAttribArray(0);
    gl.vertexAttribPointer(0, 3, gl.GL_FLOAT, gl.GL_FALSE, @sizeOf(f32) * 6, @ptrFromInt(0));

    gl.enableVertexAttribArray(1);
    gl.vertexAttribPointer(1, 3, gl.GL_FLOAT, gl.GL_FALSE, @sizeOf(f32) * 6, @ptrFromInt(@sizeOf(f32) * 3));

    gl.useProgram(program);
    gl.clearColor(0.0, 0.0, 0.0, 1.0);
    while (glfw.glfwWindowShouldClose(window) == 0) {
        glfw.glfwPollEvents();

        gl.clear(gl.GL_COLOR_BUFFER_BIT);
        // gl.drawArrays(gl.GL_TRIANGLES, 0, 3);
        gl.drawElements(gl.GL_TRIANGLES, 3, gl.GL_UNSIGNED_INT, &indecies);

        glfw.glfwSwapBuffers(window);
    }
}
