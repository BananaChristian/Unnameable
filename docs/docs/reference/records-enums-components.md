# Records, enums and components

## Records

A record is Unnameable's struct equivalent — a named collection of fields with no methods.

```unn
record Rect {
    i32 x
    i32 y
}
```

Fields are immutable by default. To make all fields mutable mark the record with `mut`:

```unn
mut record Random {
    i32 x = 77
    i32 y = 78
}
```

Fields can carry default values. Without defaults the compiler zero-initializes.

### Volatile records

`volatile` tells the compiler not to optimize away reads and writes to the record's fields — useful for memory-mapped hardware registers or shared memory:

```unn
volatile record Random {
    i32 x = 77
    i32 y = 78
}
```

This behaves the same as `volatile` in C.

### Instantiation

There are exactly two ways to instantiate a record:

```unn
Rect rectangle = Rect{x=100, y=50}
```

```unn
Rect rectangle
```

The first form initializes fields explicitly. The second default-initializes — zeros unless the record declared default values.

### Passing records to functions

Records can be passed to and returned from functions. Fields are accessed with dot notation:

```unn
record Bounds {
    i32 width
    i32 height
}

record Rect {
    i32 x
    i32 y
}

func dimensions(Rect r):Bounds {
    trace r.x
    return Bounds{width=r.x, height=r.y}
}
```

### Limitations

- No methods
- No nesting — a record cannot contain another record directly
- No `heap` on individual fields — storage is tied to the instance

### Export

Records can be marked `export` to expose them to external linkage and the stub system:

```unn
export record Rect {
    i32 x
    i32 y
}
```

---

## Enums

Enums in Unnameable are scoped — variants cannot leak into the surrounding namespace. They only work on integer types.

```unn
enum HttpStatus {
    OK = 200,
    NotFound = 404,
    InternalServerError = 500
}
```

The default underlying type is `i32`. To specify a different integer type declare it after the enum name:

```unn
enum HttpStatus:u16 {
    OK = 200u16,
    NotFound = 404u16,
    InternalServerError = 500u16
}
```

### Accessing variants

Variants are accessed using `::` notation:

```unn
auto code = HttpStatus::OK
```

### Using enums in functions

```unn
func get_http_code(HttpStatus status):i32 {
    if(status == HttpStatus::OK) {
        return 200
    } elif(status == HttpStatus::NotFound) {
        return 404
    } elif(status == HttpStatus::InternalServerError) {
        return 500
    }
    return 0
}

func main:i32 {
    auto code = HttpStatus::OK
    i32 raw_code = get_http_code(code)
    trace raw_code
    return 0
}
```

### Export

Enums can be marked `export` to expose them to the stub system:

```unn
export enum HttpStatus {
    OK = 200,
    NotFound = 404,
    InternalServerError = 500
}
```

---

## Components

A component is a record with additional capabilities — methods, field injection, an optional `init` constructor, and access to `self`.

```unn
component Player {
    i32 speed
    i32 strength

    func get_player_speed():i32 {
        return self.speed
    }

    func buff_player:void {
        self.speed = self.speed + 10
        self.strength = self.strength + 10
    }

    init(i32 player_speed, i32 player_strength) {
        self.speed = player_speed
        self.strength = player_strength
    }
}
```

### Self

Inside a method, fields and other methods are accessible via `self`:

```unn
return self.speed
```

`self.` is optional — the compiler will resolve bare field names inside a method automatically. Both forms are valid:

```unn
return self.speed
return speed
```

### The init constructor

`init` is a special function that lets you construct a component with custom starting values. It always returns void and is not mandatory — if you don't define one, the `new` initialization syntax is unavailable.

```unn
init(i32 player_speed, i32 player_strength) {
    self.speed = player_speed
    self.strength = player_strength
}
```

### Instantiation

If the component has an `init`:

```unn
Player player_0 = new Player(100, 78)
```

Despite the `new` keyword this is stack allocated. For heap allocation:

```unn
heap Player player_0 = new Player(100, 78)
```

Default initialization (zeros or declared defaults):

```unn
Player player_1
heap Player player_1
```

### Methods

Methods follow the same syntax as global functions. They live inside the component body and have access to `self`:

```unn
func get_player_speed():i32 {
    return self.speed
}
```

### Field injection

Injection pulls all fields from a record directly into the component at the frontend — no extra indirection involved:

```unn
record Buffs {
    i32 super_speed = 10
    i32 super_strength = 10
}

component Player {
    i32 speed
    i32 strength

    inject record Buffs

    func buff_player:void {
        self.speed = self.speed + self.super_speed
        self.strength = self.strength + self.super_strength
    }
}
```

After injection `super_speed` and `super_strength` become direct fields of `Player` as if they were declared there.

To inject a single field only use `@`:

```unn
inject record Buffs@super_speed
```

> **Warning:** The compiler treats field name collisions as a hard error regardless of which records the fields came from. If two injected records share a field name the compiler will reject it. To avoid this embed the record as a field instead — `Buffs buffs` — and access it via dot notation.

### Only records can be injected

You cannot inject from another component — only records are valid injection sources.

### Export

Marking a component `export` exposes it and all its methods to external linkage and the stub system:

```unn
export component Player {
    ...
}
```

Individual methods can also be targeted:

```unn
component Player {
    export func get_player_speed():i32 {
        return self.speed
    }

    func buff_player:void {
        ...
    }
}
```
