# zglgen

OpenGL bindings generator fully written in Zig. Currently targets Zig 0.11.0.

![hello world triangle](./examples/triangle.png)

## Usage

### Command line

Up-to-date documentation on command line options can be found by running `zglgen -h`.

```
[-h] [-o <file>] [--api <apispec>] [--registry <file>] <extension>...
    -h, --help
            Show this help message

    -o, --output <file>
            Destination path for the generated module (default: prints to stdout)

        --api <apispec>
            Api to generate

        --registry <file>
            File path to OpenGL registry (default: downloads https://raw.githubusercontent.com/KhronosGroup/OpenGL-Registry/main/xml/gl.xml)

    <extension>...
            Additional extensions
```

### Generating bindings

After adding zglgen to your `build.zig.zon` use these two lines to get zglgen executable

```zig
const zglgen_dep = b.dependency("zglgen", .{});
const zglgen_exe = zglgen_dep.artifact("zglgen");
```

And use `std.Build.addRunArtifact` to generate bindings during build

```zig
const zglgen_cmd = b.addRunArtifact(zglgen_exe);
zglgen_cmd.addArg("-o");
const gl_bindings_path = zglgen_cmd.addPrefixedOutputFileArg("", "gl.zig");

zglgen_cmd.addArgs(&[_][]const u8{
// zig fmt off
  "--api", "gl:4.6",
  "GL_KHR_debug",
// zig fmt on
});

// ...

const gl_bindings = b.createModule(.{
  .source_file = .{.path = gl_bindings_path },
});

your_exe.addModule("gl", gl_bindings);

// ...
```

For example on how to use a generated module see [examples/triangle.zig](./examples/triangle.zig)

**NOTE**: Consider building zglgen with `ReleaseSafe` optimization.
In `Debug`, zglgen uses `std.GeneralPurposeAllocator` which is **really slow**.
In `ReleaseSafe` mode, zglgen will link with C standard library and use `std.heap.c_allocator` which is substantially faster.

## FAQ

### Why the procedure table is `thread_local`?

As per [Khronos OpenGL Wiki](https://www.khronos.org/opengl/wiki/OpenGL_Context):

> The current context is a thread-local variable, so a single process can have several threads
> each of which has its own current context. However, a single context cannot be current in
> multiple threads at the same time. 

A non-thread-local procedure table is unusable for people that want to have multiple GL contexts.

1. Multiple contexts may refer to different APIs.
2. Changing contexts does not guarantee that function pointers stay the same.

### Why OpenGL constants aren't Zig enums?

Reason no. 1: **Aliases**. In Zig multiple tag names cannot refer to the same value.
I've experimented with making them as public declerations but ultimately decided to keep API consistant
and opted out to include a comment near a constant about the enum groups it belongs to.
Using LSP's `goto reference` you can find constants that uniquely belong to a single enum group.

I've also experimented with representing bitmasks as `packed structs`.
The upside is that bitfield values were easly discoverable.
The downside is that we lost the ability to perform tail calls.
Considering the bindings can potentialy be a part of a hot code path (which they most likely are), I removed them.
