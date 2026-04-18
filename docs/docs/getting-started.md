# Getting started

## Requirements

- Linux x86-64
- LLVM 18.1.x (`llvm-config`, `ld.lld`)

## Installation

> Build instructions coming soon.

## Your first program

Create a file called `main.unn`:

```unn
func main:i32 {
    trace "Hello World"
    return 0
}
```

Compile and run:

```bash
unnc main.unn
./main
```

Output: Hello World

## Compiler flags

| Flag | Description |
|------|-------------|
| `-build <n>` | Compile and link to executable |
| `-compile <file>` | Compile to object file only |
| `-static` | Generate a static library |
| `-stub <file>` | Generate a stub file |
| `-check <file>` | Run the front end only (no codegen) |
| `-verbose` | Enable verbose internal logs |

## Optimization levels

| Flag | Description |
|------|-------------|
| `--debug` | No optimizations, fastest compile |
| `--basic` | Basic optimizations |
| `--release` | Standard optimizations (recommended) |
| `--aggressive` | Aggressive optimizations |
| `--size` | Optimize for binary size |