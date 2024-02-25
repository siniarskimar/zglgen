# zglgen

OpenGL bindings generator written in Zig.

Currently targets Zig 0.11.0.

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

