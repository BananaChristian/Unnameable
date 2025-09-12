# Unnameable Compiler

**Unnameable** is a statically typed, ahead-of-time compiled modern programming language written in C++. It is designed to be fast, minimal, and expressive.

It exists to make low-level development **clear**, **accessible**, and **fun** for everyone especially for beginners and builders working on modest machines.

> **Fast and powerful, yet simple and predictable.**

No matter what you’re building an OS, a game engine, or your first kernel you shouldn't need a PhD or wrestle with books just to understand memory management.

Unnameable is here to bring the joy back to low-level programming.

---

This project contains the core implementation of the Unnameable compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, semantic analyzer and LLVM IR codegen.

---

## Features

- Custom Lexer and Tokenizer *(in progress)*
- Custom Parser *(in progress)*
- Semantic Analyzer *(active development)*
- LLVM IR Codegen *(active development)*

---

## Data Types
- `short` — 16-bit signed integers
- `ushort` — 16-bit usigned integers
- `int` — 32-bit signed integers
- `uint` — 32-bit unsigned integers
- `long` — 64-bit signed integers
- `ulong` — 64-bit unsigned integers 
- `extra` — 128-bit signed integers
- `uextra` — 128-bit unsigned integers

- `bool` — true/false values
- `string` — UTF-8 encoded sequence of characters.
May contain Unicode text. (Basic support: no slicing or advanced string ops yet.)

- `char` — 8 bit chars
- `char16` — 16 bit chars
- `char32` — 32 bit chars

- `float` — 32-bit floating-point numbers
-  `double`— 64-bit floating point number
---

## Comments

```unn
# This is a comment in Unnameable

## 
This is a multiline comment
In unnameable
##
```


## Variables and type inference
```unn
int x;
uint x=8u;
x = 2;

short y=8s;
ushort m=8us;

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
func greet(string? name): string?{
    return "Hello" + name;
}

#Function declarations
func greet(string? name): string?;

```

##  Functions
```unn
func greet(string name): string {
    return "Hello" + name;
}

```

## Generic and multigenerics
Unnameable uses a system of generics called Explicitly Instantiated Generics(EIG) where the user defines what they want inside a generic block which takes arguments of the types. Then an instantiate statement allows the user to tell the compiler what sort of types they would like to create generics for.
The alias on the instantiate statement is to uphold the langauge's rules of no overloading
This means when the compiler generates the generic functions after the user instantiates so for examole the add for int will not be the same as the one for float the compiler will automatically add the aliases to the name generating something like IntOps_add and FloatOps_add as seen below. 
This system also supports multigenerics as shown below
```
generic MathOps(T){
    func add(T a, T b): T{
        return a+b;
    };

    func subtract(T a, T b){
        return a-b;
    }
}

instantiate MathOps(int) as IntOps;

##Here the user is explicity telling the compiler the types he wants the compiler to generate 
Hence the functions generated will look something like this

func IntOps_add(int a,int b): int{
    return a+b;
}

func IntOps_subtract(int a,int b): int{
    return a-b;
}
##

instantiate MathOps(float) as FloatOps;

##Same story here the generated functions will be

func FloatOps_add(float a, float b): float{
    return a+b;
}

func FloatOps_subtract(float a, float b): float{
    return a-b;
}
##

#Multigeneric example

generic MathOps(T,M){
    func add(T a,M b): M{
        return a+b;
    };

    func subtract(T a,M b): M{
        return a-b;
    }
}

instantiate MathOps(int, float) as TestOps;

##Here the function generated will be
func TestOps_add(int a,float b): float{
    return a+b;
}

func TestOps_subtract(int a ,float b): float{
    return a-b;
}
##
```

## Error handling
```unn
func greet(string name): string {
    return "Hello" + name, error("Got an error");
}

```

## Multilingual identifier support
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
auto laugh="😂";
string 萌="cute";

```

## Function calls
```
greet("John");
add(1, 2);
config();

```

## Control flow
```
if (age > 18) {
    return "adult";
} elif (age < 18) {
    return "minor";
} else {
    return "unknown";
}

```

## Switch statements
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

## Loops
*While loops*
```
int x=5;
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

## Component System (OOP-like structure)
Unnameable supports components, clean structures for organizing data and behavior, like classes, but lightweight and predictable.
```unn
behavior combat{
      func kick() :void;
      func punch(): void;
      func super(): void;
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
    use behavior combat.super;

    use data attributes;
    use data attributes::speed;

    func greet(): string {
        return "Hello, I have " + self.health + " HP";
    }
}

auto p = new Player(100);
p.greet();

```
## Data blocks
The data blocks are a way to group data that might be related the mutability qualifier means all the data inside the block is mutable but someone can specify the exact data they want to be mutable
Note: By default the data block is immutable and all the data inside is immutable unless you explicitly say 'mut' on the whole block or the individual data inside
Data blocks only allow for variable declarations can have values initialized or not
```
mut data battery_specs{
    uint health=100u;
    uint capacity=5000u;
}

data phone_specs{
    mut string name;
    string color;
}

phone_specs::health=90u; #This will be allowed since all the members of the data block are mutable
```

## Enums
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

enum class TokenType: uint{
    ADDITION,
    SUBTRACTION,
}

enum class TokenType: uint{
    ADDITION=10u,
    SUBTRACTION=20u,
}

```


## Immutability and Mutability
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

## Future Lexer additions
- Even more unicode support for identifiers

## Future additions
- Pattern matching to switch statements
- Function parameters as first-class expressions

## Requirements
- C++17 or later
- g++ or clang
- Make(optional for building)

## Philosophy
Unnameable is designed to make systems programming less painful and soul draining.
The language upholds the principles of clarity and predictability to allow it's user to build powerful things.

## Current status
This project is under active and early development and is still highly experimental. If you want to explore,contribute, or just follow the journey feel free to fork,play or reach out.

---

License

Unnameable is dual-licensed under the MIT License and Apache License 2.0.
You may choose either license to use this project.

> See LICENSE-MIT and LICENSE-APACHE for full details
