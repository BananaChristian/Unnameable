# Unnameable Compiler

**Unnameable** is a statically typed, ahead-of-time compiled modern programming language written in C++. It is designed to be fast, minimal, and expressive.

It exists to make low-level development **clear**, **accessible**, and **fun** for everyone especially for beginners and builders working on modest machines.

Born from the frustration with the complexity of C++, the strictness of Rust, and the rawness of C, Unnameable aims to strike a balance:

> **Fast and powerful, yet simple and predictable.**

No matter what you’re building an OS, a game engine, or your first kernel you shouldn't need a PhD or wrestle with books just to understand memory management.

Unnameable is here to bring the joy back to low-level programming.

---

This project contains the core implementation of the Unnameable compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, semantic analyzer.

---

## ✨ Features

- Custom Lexer and Tokenizer *(in progress)*
- Custom Parser *(in progress)*
- Semantic Analyzer *(active development)*
- LLVM IR Codegen *(active development)*

---

## 🧠 Data Types

- `int` — 32-bit integers
- `bool` — true/false values
- `string` — UTF-8 encoded sequence of characters.
May contain Unicode text. (Basic support: no slicing or advanced string ops yet.)
- `char` — single characters
- `float` — 32-bit floating-point numbers
-  `double`— 64-bit floating point number
---

## 💬 Comments

```unn
# This is a comment in Unnameable

## 
This is a multiline comment
In unnameable
##
```


## 📦Variables and type inference
```unn
int x;
x = 2;

string name = "Iron";

float pi = 3.14;

double pi=3.14d;

auto y = 42;  # Type inferred as int


```

## Null Safety
By default all the variables, function return types are not nullable but with the ? on a data type this acknowledges that the variable or data type are nullable.
This means the compiler will force you to explicitly deal with these but It wont allow null to be passed into variables or returned from a function
```
string? name= null;
work greet(string? name): string?{
    return "Hello" + name;
}

#Function declarations
work greet(string? name): string?;

```

##  ⚒️ Functions
```unn
work greet(string name): string {
    return "Hello" + name;
}

```

## Generic and multigenerics
```
work <T>greet(T name): T{
    return T;
};

work <T,M> info(T name, M age): T{
    return T;
}
```

## ⚠️ Error handling
```unn
work greet(string name): string {
    return "Hello" + name, error("Got an error");
}

```

## 🌐Multilingual identifier support
Unnameable supports identifiers in:
- Latin (English, French, German, etc.)
- Cyrillic (e.g., Russian, Ukrainian)
- Greek
- Hebrew
- Arabic
- Devanagari (Hindi, Marathi, Sanskrit)
- Thai, Lao, Tibetan
- Georgian, Hangul (Korean)
- **Chinese (all major CJK ideograph ranges)**
- Japanese (Hiragana, Katakana, Kanji)
- Full emoji support 😎🔥🧠
  
```
auto test="😂";
string 萌="cute";

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
} elif (age < 18) {
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
for (mut int i; i < 10; i++) {
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

## 🎭Enums
Unnameable supports `enum class` as the default way to define enums a clean and type-safe way to group related symbolic values under a single name.

```
enum class Animal {
    CAT,
    DOG,
}

auto pet = Animal::CAT;

enum class HttpStatus {
    OK = 200,
    NotFound = 404,
    InternalServerError = 500,
}

auto code = HttpStatus::NotFound;

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
- Even more unicode support for identifiers

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

---

📜 License

Unnameable is dual-licensed under the MIT License and Apache License 2.0.
You may choose either license to use this project.

> See LICENSE-MIT and LICENSE-APACHE for full details
