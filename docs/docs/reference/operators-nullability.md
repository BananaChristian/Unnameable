# Operators and nullability

## Arithmetic operators

| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction |
| `*` | Multiplication |
| `/` | Division |
| `%` | Modulo |

## Comparison operators

| Operator | Description |
|----------|-------------|
| `==` | Equal |
| `!=` | Not equal |
| `<` | Less than |
| `>` | Greater than |
| `<=` | Less than or equal |
| `>=` | Greater than or equal |

## Logical operators

| Operator | Description |
|----------|-------------|
| `&&` | Logical AND |
| `\|\|` | Logical OR |
| `!` | Logical NOT |

## Bitwise operators

| Operator | Description |
|----------|-------------|
| `&` | Bitwise AND |
| `\|` | Bitwise OR |
| `^` | Bitwise XOR |
| `~` | Bitwise NOT |
| `<<` | Left shift |
| `>>` | Right shift |

## Compound assignment

Unnameable does not have compound assignment operators. `+=`, `-=`, and similar are not supported — write the full expression instead:

```unn
x = x + 1
x = x - 1
```

## Increment and decrement

Both prefix and postfix forms are supported with the same semantics as C:

```unn
x++    
x--    
++x    
--x    
```

## Memory operators

### addr

`addr` takes the address of a variable — equivalent to `&` in C:

```unn
i32 x = 10
ptr i32 p -> addr x
```

`addr` can only be applied to a named variable in memory. The following are illegal:

```unn
addr call()        
addr 42            
addr (x + y)       
```

### deref

`deref` dereferences a pointer — equivalent to `*` in C:

```unn
i32 val = deref p
```

Nesting is allowed for multiple levels of indirection:

```unn
i32 val = deref deref p
```

### The binding operator `->`

`->` is used exclusively when assigning to a pointer or reference — both at declaration and reassignment. Using `=` for pointer binding is not allowed:

```unn
i32 x = 10
ptr i32 p -> addr x

i32 y = 20
p -> addr y
```

---

## Nullability

Unnameable has a first-class nullability system. By default all types are non-nullable — a variable cannot hold `null` unless its type is explicitly marked nullable with `?`:

```unn
i32? x = null
```

Nullable types work with all modifiers:

```unn
ptr i32? p
arr[3] i32? x
```

### Passing nullable values

A nullable type cannot be passed to a function expecting a non-nullable type. The compiler enforces this strictly — you must resolve the nullability before passing:

```unn
func takes_int(i32 val):void {
    trace val
}

i32? x = 10
i32 safe = unwrap x
takes_int(safe)
```

Passing `x` directly to `takes_int` is illegal — unwrap it into a non-nullable variable first.

### Nullable return types

Functions can return nullable types:

```unn
func maybe_value(i32 input):i32? {
    if(input == 0) {
        return null
    }
    return input
}

func main:i32 {
    i32? result = maybe_value(0)
    trace result ?? -1
    return 0
}
```

### The coalesce operator `??`

`??` returns the value if it is not null, or a fallback if it is:

```unn
func main:i32 {
    i32? x = null
    trace x ?? 5      
    return 0
}
```

If `x` holds a value that value is returned instead:

```unn
i32? x = 10
trace x ?? 5          
```

### unwrap

`unwrap` extracts the value from a nullable type. If the value is null the program traps immediately:

```unn
i32? x = null
trace unwrap x        
```

To use the value safely unwrap it into a non-nullable variable first. If unwrapping succeeds the resulting variable is clean and can be used freely:

```unn
i32? x = 10
i32 safe = unwrap x
trace safe
```

If `x` is null the trap fires at the `unwrap` line — `safe` is never assigned.
