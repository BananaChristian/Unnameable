# Unnameable

A procedural, statically compiled programming language for Linux x86-64.

Unnameable compiles directly to native binaries via an LLVM backend with no dependency on the C runtime — the language ships its own minimal runtime instead.

Source files use the `.unn` extension. The compiler binary is `unnc`.

## Quick example

```unn
func main:i32 {
    trace "Hello World"
    return 0
}
```

---

Use the navigation to explore the language. Start with [Getting Started](getting-started.md) if you're new.