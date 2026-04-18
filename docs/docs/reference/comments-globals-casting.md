# Comments, globals and casting

## Comments

Single line comments use `#`:

```unn
# This is a single line comment
i32 x = 10  # Inline comment
```

Multiline comments are wrapped in `##`:

```unn
## 
This is a 
multiline comment 
##
```

---

## Globals

Variables declared outside any function are global. They follow the same declaration syntax as local variables with a few restrictions.

### Basic globals

```unn
const i32 MAX_VALUE = 100
i32 my_var = 88
mut i32 another_var = 87
i32 uninit
```

Uninitialized globals are zero-initialized by the compiler. `const` globals must be compile-time literals.

### Pointers and references at global scope

Globals can be pointers or references bound to other globals:

```unn
i32 x = 188
ptr i32 px -> addr x
ref i32 rx -> x
```

### Arrays at global scope

Arrays are allowed at global scope. The size must be a compile-time constant or inferred from an initializer:

```unn
arr[3] i32 nums
arr i32 primes = [2, 3, 5, 7]
```

### Record globals

Record types can be instantiated at global scope using the `Record{field=val}` syntax:

```unn
record Color {
    i32 r
    i32 g
    i32 b
    i32 a
}

Color WHITE = Color{r=255, g=255, b=255, a=255}
Color BLACK = Color{r=0, g=0, b=0, a=255}
Color RED   = Color{r=255, g=0, b=0, a=255}
```

Field values follow the same rules as all global initializers — compile-time literals or references to other globals only.

### Initializers

Global initializers must be compile-time literals or other globals. Runtime expressions are not valid at global scope:

```unn
i32 x = 100
i32 y = x        
```

### Export

Globals can be marked `export` to expose them to external linkage and the stub system. There are two distinct behaviors depending on whether an initializer is provided:

**With initializer** — the compiler initializes the value and exposes it externally:

```unn
export i32 MAX = 100
export Color WHITE = Color{r=255, g=255, b=255, a=255}
```

**Without initializer** — the compiler does not zero-initialize. It assumes the linker will satisfy the symbol from another translation unit. This is effectively an external global declaration:

```unn
export i32 MAX
```

If nothing satisfies the symbol the linker will complain. This mirrors the behavior of `extern` declarations in C.

`export` on a global variable behaves the same as `export` on functions — external linkage, stub system exposure, and modular availability.

### Rules and restrictions

- Assignments are not allowed at global scope — only inside functions
- `heap` declarations are not allowed at global scope
- Function calls are not allowed at global scope
- Only record types can be instantiated at global scope — components are not allowed
- `export` without an initializer hands the symbol to the linker — make sure something satisfies it

---

## Type casting

Unnameable has two casting operators — `cast` and `bitcast`. Both use the same syntax:

```unn
operator<dest_type>(src)
```

### cast

`cast` is the safe casting operator. It works on scalar types only:

```unn
cast<i32>(x)
cast<f64>(x)
```

Custom types are not valid destinations for `cast`:

```unn
cast<MyType>(x)    
```

### bitcast

`bitcast` is a raw reinterpretation of the underlying bits. It is more powerful than `cast` and works on pointers and custom types behind a pointer:

```unn
bitcast<ptr i32>(x)
bitcast<ptr MyType>(x)
```

Use `bitcast` carefully — it bypasses type safety entirely and reinterprets memory as-is.

