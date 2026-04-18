# Memory management

Unnameable manages heap memory automatically through a system called **baton**. When you declare a variable with `heap` you are opting into this system — the compiler tracks the variable's lifetime and inserts frees for you.

> **Stability notice:** The baton system is under active development. Known issues include double frees and memory leaks in certain patterns involving assignments and `persist`. The examples on this page represent cases that currently work correctly.

## The heap modifier

Declaring a variable with `heap` tells the compiler three things — allocate this on the heap, use the built-in allocator, and manage the memory for me:

```unn
heap i32 x
```

The compiler calculates the size of the type, requests memory from the allocator, and will free it automatically.

## Last-use freeing

Baton frees memory at the **last use** of a variable, not at the end of its scope. This is more aggressive than scope-based cleanup — memory is released as early as possible.

```unn
func main:i32 {
    heap i32 x
    return 0
}
```

Here `x` is never used after its declaration. The compiler frees it immediately at the declaration point rather than waiting for the function to return.

```unn
func main:i32 {
    heap i32 x
    heap i32 y
    heap ptr i32 p -> addr x
    y = x + 1
    return 0
}
```

In this example `p` is freed as soon as it is no longer needed. `x` and `y` are freed at the assignment `y = x + 1` which is their last use.

## Pointers and lifetime extension

When a pointer is bound to a heap variable it takes responsibility for that variable's memory. The variable will not be freed while the pointer is alive:

```unn
func main:i32 {
    heap i32 x
    heap ptr i32 p -> addr x
    return 0
}
```

`p` holds responsibility for `x`. When `p` is freed it also frees `x`. You do not need to manage `x` separately.

## persist

If you want a heap variable to live until the end of its scope rather than being freed at last use, mark it with `persist`:

```unn
func main:i32 {
    heap i32 x
    persist heap i32 y
    return 0
}
```

`x` is freed at last use. `y` is held until the scope ends.

> **Note:** `persist` has known issues with memory leaking in some patterns. Use with caution until this is resolved.

## Branches and loops

The compiler classifies heap variables as either **native** or **foreigner** relative to each block.

A **native** is a heap variable declared inside the block. It is treated normally — freed at last use within that block:

```unn
func main:i32 {
    mut heap i32 x = 1
    if(x < 10) {
        mut heap i32 z = 1   
        x++
        z                    
    }
    return 0
}
```

`z` is native to the `if` block. It is freed inside the block at its last use.

A **foreigner** is a heap variable that was declared outside the block but used inside it. The compiler guarantees a foreigner is never freed inside the block — it waits until after the block exits:

```unn
func main:i32 {
    mut heap i32 x = 1
    if(x < 10) {
        x++                  
    }
    return 0
}
```

`x` is a foreigner to the `if` block. No matter which branch executes, `x` is guaranteed to be freed after the branch merges — never inside it.

The same native/foreigner rules apply to `while` and `for` loops.

## Bunkering

When the compiler cannot safely determine where to free a variable it falls back to **bunkering** — the variable is held until the end of its scope and freed there. This is the safe fallback and guarantees no dangling pointers, at the cost of holding memory slightly longer than necessary.

You will not see bunkering directly — it happens automatically when the compiler needs it.

## Custom allocators

The default `heap` modifier uses the compiler's built-in allocator. Custom allocators can be plugged in via an interface:

```unn
heap<MyAllocator> auto x = some_value
```

Custom allocators are covered in depth in the advanced memory section.