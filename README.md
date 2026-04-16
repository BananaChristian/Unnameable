# Unnameable

A procedural, statically compiled programming language for Linux x86-64.

![Linux only](https://img.shields.io/badge/platform-Linux%20x86--64-blue)
![LLVM 18](https://img.shields.io/badge/LLVM-18.1.3-orange)

## Overview

Unnameable is a procedural language with a C-like feel. It compiles directly to native binaries via an LLVM backend with no dependency on the C runtime the language ships its own minimal runtime instead. This is intentional, but it is also why the compiler currently only targets Linux x86-64.

Source files use the `.unn` extension. The compiler binary is `unnc` (Unnameable Compiler).

## Requirements

- Linux x86-64
- LLVM 18.1.x (`llvm-config`, `ld.lld`)

## Quick start

**Hello World** — `main.unn`

```
func main:i32 {
    trace "Hello World"
    return 0
}
```

```bash
$ unnc main.unn
$ ./main
Hello World
```

**Functions and variables**

```
func add(i32 a, i32 b):i32 {
    return a + b
}

func main:i32 {
    i32 result = add(10, 12)
    trace result
    return 0
}
```

```bash
$ ./main
22
```

## Usage

```
unnc <source.unn> [options]
```

| Flag | Description |
|------|-------------|
| `-build <name>` | Compile and link to executable |
| `-compile <file>` | Compile to object file only |
| `-static` | Generate a static library |
| `-stub <file>` | Generate a stub file |
| `-check <file>` | Run the front end only (no codegen) |
| `-verbose` | Enable verbose internal logs |

### Optimization levels

| Flag | Description |
|------|-------------|
| `--debug` | No optimizations, fastest compile, best for debugging |
| `--basic` | Basic optimizations |
| `--release` | Standard optimizations (recommended) |
| `--aggressive` | Aggressive optimizations (may increase compile time) |
| `--size` | Optimize for binary size |

### Examples

```bash
unnc main.unn -build app
unnc main.unn --release -build app
unnc main.unn -compile main.o
```

## Building from source

> Build instructions coming soon.

## Status

Unnameable is an active personal project. The compiler has a working LLVM backend and is under active development. Expect rough edges.

## Platform support

| Platform | Supported |
|----------|-----------|
| Linux x86-64 | Yes |
| Other | Not yet |

Unnameable does not link against the C runtime. It uses a custom minimal runtime built specifically for Linux x86-64. Expanding platform support requires porting the runtime to each target.
