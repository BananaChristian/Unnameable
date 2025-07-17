# Unnameable Compiler

**Unnameable** is a statically typed, ahead-of-time compiled modern programming language written in C++. It is designed to be fast, minimal, and expressive.

It exists to make low-level development **clear**, **accessible**, and **fun** for everyone especially for beginners and builders working on modest machines.

Born from the frustration with the complexity of C++, the strictness of Rust, and the rawness of C, Unnameable aims to strike a balance:

> **Fast and powerful, yet simple and predictable.**

No matter what you’re building — an OS, a game engine, or your first kernel — you shouldn't need a PhD or wrestle with books just to understand memory management.

Unnameable is here to bring the joy back to low-level programming.

---

This project contains the core implementation of the Unnameable compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, semantic analyzer.

---

## ✨ Features

- Custom Lexer and Tokenizer *(in progress)*
- Custom Parser *(in progress)*
- Semantic Analyzer *(active development)*
- Component system (OOP-like support)
- Support for constructors and method dispatch
- Support for `self` keyword and instance field access
- LLVM IR Codegen *(planned)*

---

## 🧠 Data Types

- `int` — 32-bit integers
- `bool` — true/false values
- `string` — UTF-8 strings *(basic support)*
- `char` — single characters
- `float` — 32-bit floating-point numbers

---

## 💬 Comments

```unn
# This is a comment in Unnameable
```


## 📦Variables and type inference
```unn
int x;
x = 2;

string name = "Iron";

float pi = 3.14;

auto y = 42;  # Type inferred as int


```

##  ⚒️ Functions
```unn
work greet(string name): string {
    return "Hello" + name;
}

```

## ⚠️ Error handling
```unn
work greet(string name): string {
    return "Hello" + name, error("Got an error");
}

```


## 🧬Function calls
```
greet("John");
add(1, 2);
config();

```

## 🔁 Control flow
```
if (age > 18) {
    return "adult";
} elseif (age < 18) {
    return "minor";
} else {
    return "unknown";
}

```

## 🔀 Switch statements
```
string name="Mellisa";

switch (name) {
    case "Brenda":
        return "That is not her";
    case "Charity":
        return "Getting close";
    case "Mellisa":
        return "That is her";
    default:
        return "You are way off";
}

```

## 🔚Loops
*While loops*
```
while (x > 5) {
    return x;
}

```

*For loops*
```
for (int i; i < 10; i++) {
    int x;
    x = x + 1;
}

```

## 🧱 Component System (OOP-like structure)
Unnameable supports components, clean structures for organizing data and behavior, like classes, but lightweight and predictable.

```unn
behavior combat{
      work kick() :void;
      work punch(): void;
      work super(): void;
}

data attributes{
      int health_extra;
      int speed;
}

component Player {
    int health;

    #This is the constructor for the component
    init(int h) {
        self.health = h;
    }

    use behavior combat;
    use behavior combat.super();

    use data attributes;

    work greet(): string {
        return "Hello, I have " + self.health + " HP";
    }
}

let p = new Player(100);
p.greet();

```

## 🔒Immutability and Mutability
Unnameable supports explicit control over variable mutability with the keywords *const* and *mut*
- `const` declares an immutable variable. It must be initialized at declaration and cannot be reassigned later. This ensures safety and predictability.

```
const int x = 42;  # Immutable, cannot be changed
```
- `mut` declares a mutable variable. It can be reassigned multiple times after declaration.
```
mut int y;       # Mutable, can assign later
y = 10;
y = 20;          # Allowed
```
If no mutability keyword is given, variables are immutable by default, and must be assigned a value only once, either at declaration or later.
Type inference with auto works together with *mut* and *const* for example
```
mut auto z = 5;    # Mutable variable with inferred type int
const auto w = 3;  # Immutable variable with inferred type int

```

## 🔮 Future Lexer additions
- Unicode support
- UTF-8 multibyte characters
- UTF-16 / UTF-32 decoding

## 🧠 Future additions
- High order functions
- Pattern matching to switch statements
- Function parameters as first-class expressions

## 🧰 Requirements
- C++17 or later
- g++ or clang
- Make(optional for building)

## 🌍Philosophy
Unnameable is designed to make systems programming less painful and soul draining.
The language upholds the principles of clarity and predictability to allow it's user to build powerful things.

## ⚙️ Current status
This project is under active and early development and is still highly experimental. If you want to explore,contribute, or just follow the journey feel free to fork,play or reach out.
