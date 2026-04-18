# Basics

## Variables and mutability

By default all variables in Unnameable are immutable once assigned. To allow reassignment you must explicitly mark a variable with `mut`:

```unn
i32 x = 10        
mut i32 y = 10    
y = 20            
```

For values known at compile time you can use `const`:

```unn
const i32 MAX = 100
```

`const` requires a compile-time literal. Runtime values are not allowed. Currently `const` is only supported for scalar types.

## Types

Unnameable is statically typed. Every variable declaration follows this general shape:

```
[modifiers] type name = value
```

### Scalar types

| Type | Description |
|------|-------------|
| `i8` | 8-bit signed integer |
| `i16` | 16-bit signed integer |
| `i32` | 32-bit signed integer |
| `i64` | 64-bit signed integer |
| `i128` | 128-bit signed integer |
| `u8` | 8-bit unsigned integer |
| `u16` | 16-bit unsigned integer |
| `u32` | 32-bit unsigned integer |
| `u64` | 64-bit unsigned integer |
| `u128` | 128-bit unsigned integer |
| `usize` | Unsigned pointer-sized integer (native width) |
| `isize` | Signed pointer-sized integer (native width) |
| `char8` | 8-bit character |
| `char16` | 16-bit character |
| `char32` | 32-bit character |
| `string` | String type |
| `f32` | 32-bit floating point |
| `f64` | 64-bit floating point |

User-defined types are also supported. The compiler has no naming restrictions on custom types — any identifier that does not collide with a reserved keyword is valid.

### Type inference

If you don't want to write the type explicitly you can use `auto`. The compiler infers the type from the initializer — an initializer is required:

```unn
auto y = 100
```

`auto` only infers the base type. Other modifiers such as `mut`, `heap`, and `const` must still be written explicitly:

```unn
mut heap auto x = some_value
```

### Pointers

A pointer is declared using the `ptr` modifier:

```unn
ptr i32 p
```

Pointers are bound using `->` with `addr` to take the address of a variable:

```unn
i32 x = 10
ptr i32 p -> addr x
```

Pointers can be nested to any depth:

```unn
ptr<ptr> i32 p
ptr<ptr<ptr>> i32 p
```

#### Opaque pointers

`opaque` is a typeless pointer, the equivalent of `void*` in C. It must always appear behind a pointer modifier and cannot be dereferenced:

```unn
ptr opaque p
```

### References

A reference is an alias — another name for the same variable with no memory of its own. Mutating through a reference mutates the original:

```unn
mut heap i32 x = 100
ref i32 rx -> x
rx = 200            
```

References inherit mutability from the variable they are bound to. Writing `mut ref` or `const ref` is silently ignored by the compiler.

References can only be bound to heap or global variables. This restriction exists to prevent dangling references.

They can be passed to and accepted by functions:

```unn
func ref_test(ref i32 r):void {
    trace r
}

func main:i32 {
    mut heap i32 x = 100
    ref i32 rx -> x
    ref_test(rx)
    return 0
}
```

### Arrays

Arrays require either an explicit size or an initializer list:

```unn
arr[2] i32 x
arr i32 x = [1, 2]
```

Multidimensional arrays are expressed by nesting array modifiers:

```unn
arr[2]<arr[3]> i32 x
```

### Mixing modifiers

Modifiers can be combined. For example a pointer to an array of two `i32` values:

```unn
ptr<arr[2]> i32 x
```

### Heap allocation

Adding the `heap` modifier places a variable on the heap and hands memory management to the compiler's built-in allocator:

```unn
heap i32 x = 10
```

Without `heap` the compiler makes no assumptions about memory — you are responsible. The details of heap allocation and custom allocators are covered in their own section.

## Functions

Functions are declared with `func`, followed by the name, optional parameters, and a return type after the colon:

```unn
func add(i32 a, i32 b):i32 {
    return a + b
}

func main:i32 {
    i32 result = add(10, 12)
    trace result
    return 0
}
```

## Output

The `trace` keyword prints a value to stdout:

```unn
trace "Hello World"
trace result
```