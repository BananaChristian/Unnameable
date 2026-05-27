# Allocator interfaces

By default `heap` uses the compiler's built-in allocator called **GPA**. If you want to control how memory is allocated and freed you can provide your own allocator through an allocator interface.

## Defining an allocator interface

An allocator interface is declared with the `allocator` keyword. It must contain exactly two function declarations one for allocation and one for deallocation. The signatures must match `malloc` and `free` exactly or the compiler will reject the interface:

```unn
allocator MyAllocator {
    func alloc(usize size):ptr opaque
    func free(ptr opaque p):void
}
```

The compiler only cares about the signatures — it does not care what your allocator does internally as long as the signatures match.

## Using a custom allocator

Pass the allocator interface name in angle brackets after `heap`:

```unn
heap<MyAllocator> i32 x
```

The baton system works exactly the same as with the default allocator. The only difference is that your `alloc` and `free` functions are called instead of the compiler's built-in ones. You can verify this in the generated IR the compiler emits calls to your named functions directly:

```llvm
%x_heap_raw = call ptr @alloc(i64 4)
store i32 0, ptr %x_heap_raw, align 4
call void @free(ptr %x_heap_raw)
```

## All heap features apply

Every modifier and behavior that works with the default `heap` also works with a custom allocator:

```unn
persist heap<MyAllocator> i32 x
mut heap<MyAllocator> MyType obj = new MyType()
heap<MyAllocator> arr[4] f32 vec
```

The baton system, last-use freeing, bunkering, robbery all behavior is identical. The allocator interface is purely a substitution at the call site.

## Exporting allocator interfaces

Allocator interfaces can be marked `export` to expose them to the stub system:

```unn
export allocator MyAllocator {
    func alloc(usize size):ptr opaque
    func free(ptr opaque p):void
}
```

This makes the interface available for modular use across translation units.
