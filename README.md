# zglgen

OpenGL bindings generator written in Zig. Currently targets Zig 0.11.0.

For example on how to use a generated module see [examples/triangle.zig](./examples/triangle.zig)

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

## FAQ

### Why the procedure table is `thread_local`?

As per [Khronos OpenGL Wiki](https://www.khronos.org/opengl/wiki/OpenGL_Context):

> The current context is a thread-local variable, so a single process can have several threads
> each of which has its own current context. However, a single context cannot be current in
> multiple threads at the same time. 

Using a single procedure table is unreliable. Multiple contexts may refer to different API versions
and changing contexts does not guarantee that function pointers stay the same.

### Why OpenGL constants aren't Zig enums?

Reason no. 1: **Aliases**. In Zig multiple tag names cannot refer to the same value.
I've experimented with making them as public declerations but ultimately decided to keep API consistant
and opted out to include a comment near a constant about the enum groups it belongs to.
Using LSP's `goto reference` you can find constants that uniquely belong to a single enum group.

I've also experimented with representing bitmasks as `packed structs`.
The upside is that bitfield values were easly discoverable.
The downside is that we lost the ability to perform tail calls.
Considering the bindings can potentialy be a part of a hot code path, I removed them.
