# Functions

## Declaration

Functions are declared with `func`, followed by the name, optional parameters, and a mandatory return type after the colon:

```unn
func add(i32 a, i32 b):i32 {
    return a + b
}
```

The return type is not optional. Functions that return nothing must explicitly declare `:void`:

```unn
func greet:void {
    trace "Hello"
}
```

All functions must exist at global scope unless they are inside a seal or a component.

## Calling functions

```unn
func main:i32 {
    i32 result = add(10, 12)
    trace result
    return 0
}
```

## Forward declarations

A function can be declared without a body by omitting it entirely. The same syntax rules apply — return type is mandatory, parameters follow the same form:

```unn
func add(i32 a, i32 b):i32
```

There are two use cases:

**Local forward declaration** — the implementation exists later in the same file. Do not mark it `export`:

```unn
func add(i32 a, i32 b):i32

func main:i32 {
    trace add(10, 12)
    return 0
}

func add(i32 a, i32 b):i32 {
    return a + b
}
```

**External declaration** — the implementation lives in another file or library. Mark it `export` to tell the linker to find it externally:

```unn
export func add(i32 a, i32 b):i32
```

Omitting `export` on an external declaration will result in a linker error — the compiler will look for the implementation in the same file and not find it.

> Seals only accept function definitions. Forward declarations cannot be placed inside a seal.

## Limitations

- No multiple return values
- No variadic functions
- No function overloading — each function name must be unique in its scope
- No higher order functions
- No function pointers (planned)

## The export modifier

`export` is a function modifier that does three things at once:

1. Exposes the function to external linkage
2. Makes the function available to the stub system
3. Serializes its signature into binary format for modular use

```unn
export func add(i32 a, i32 b):i32 {
    return a + b
}
```

## Seals

A seal is a function namespace. It groups related functions together and prevents name collisions:

```unn
seal Maths {
    func add(i32 a, i32 b):i32 {
        return a + b
    }

    func subtract(i32 a, i32 b):i32 {
        return a - b
    }
}
```

Functions inside a seal are called using dot notation:

```unn
func main:i32 {
    trace "Sealed Add res:", Maths.add(19, 7)
    trace "Sub res:", Maths.subtract(7, 8)
    return 0
}
```

A sealed function and a global function can share the same name without conflict:

```unn
seal Maths {
    func add(i32 a, i32 b):i32 {
        return a + b
    }
}

func add(i32 x, i32 y):i32 {
    return x + y
}

func main:i32 {
    trace "Global:", add(77, 88)
    trace "Sealed:", Maths.add(19, 7)
    return 0
}
```

### Name mangling

The compiler mangles all function names inside a seal regardless of export status. A function `add` inside `seal Maths` becomes `Maths_add` at the linker level. This is how the compiler guarantees no collisions between sealed and global functions.

### Exporting seals

You can apply `export` to an entire seal, which exports all functions inside it at once:

```unn
export seal Maths {
    func add(i32 a, i32 b):i32 {
        return a + b
    }

    func subtract(i32 a, i32 b):i32 {
        return a - b
    }
}
```

Or you can target specific functions inside a seal while leaving the rest unexported:

```unn
seal Maths {
    export func add(i32 a, i32 b):i32 {
        return a + b
    }

    func subtract(i32 a, i32 b):i32 {
        return a - b
    }
}
```

In both cases name mangling still applies to all functions in the seal.

## trace

`trace` prints values to stdout. It accepts multiple arguments separated by commas — each argument is printed on its own line:

```unn
trace "Hello World"
trace x, y, z
trace "Result:", result
```

> Note: when passing a label and a value as separate arguments they will each print on their own line.