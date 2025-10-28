# Unnameable Compiler

**Unnameable** is a statically typed, ahead-of-time compiled modern programming language . It is designed to be fast, minimal, and expressive.

It exists to make low-level development **clear**, **accessible**, and **fun** for everyone especially for beginners and builders working on modest machines.

> **Fast and powerful, yet simple and predictable.**

Unnameable's goal is to bring the joy back to low-level programming.

---
This is the base compiler for the Unnameable Programming langauge written in C++ for now many features will not added till bootstrap but I want to make this base compiler as powerful as possible to make the bootstrap extremely easy without alot of rewriting

This project contains the core implementation of the Unnameable compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, semantic analyzer, layout calculator,sentinel layer and LLVM IR codegen.

---

## Features

- Lexer *(in progress)*
- Parser *(in progress)*
- Semantic Analyzer *(in progress)*
- Layout Calculator *(active development)*
- Sentinel layer *(active development)*
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

char test='A';

char16 test=u'A';


```

## Null Safety
By default all the variables, function return types are not nullable but with the ? on a data type this acknowledges that the variable or data type are nullable.
This means the compiler will force you to explicitly deal with these via coalescing or using if statements but It wont allow null to be passed into variables or get used anywhere
```
string? name= null;
func greet(string? name): string?{
    return "Hello" + name;
}

int? x=null;
int y=x;#This throws an error because x has a null value

##Correct code
int? x=8;
int y=x;#This will run since x now has a value
##

int? a;
a=null;
a+1;#This will error out since u cannot use null in operations

#Usage of uninitialized variables is not allowed in the langauge
int? c;
c+1; #BOOM error
#OR
int test;
int y=test;#Error since 'test' is not initialized

#Coalescing 
int? x=null;
(x??1)+1;

#NOTE: Coalescing only works if the the variable is nullable

#Function declarations
func greet(string? name): string?;

```

##  Functions
Functions in unnameable are written using the func keyword,the function name and the optional parameters and the return type
```unn
func greet(string name): string {
    return "Hello" + name;
}

func test(arr[int] my_array): arr[int]{
    return [1,3];
}

```
The return type must correspond to what is being returned
Functions can also return pointers for the concrete types but void pointers arent support
```unn
func test(int x=0):ptr int{
    ptr int y=> &x;
    return y;
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
## Arrays
Unnameable supports single dimensional and multidimensional arrays, The user must explicity list the length of the array if they dont provide initialize but if they  initialize the array the compiler will just count the items inside
```
#Single dimensional arrays
#Case 1: No initialization
arr[int] [2] my_array;

#Case 2: With initialization
arr [int?] test_array=[1,2,3];

#Accesing array members
test_array[0]=7;

```


## Error handling
```unn
func greet(string name): string {
    return "Hello" + name, error("Got an error");
}

```

## Multi file support
Unnameable supports multi file usage with two systems merge and module
For now only merge is supported and as the name implies it merges multiple files into one compilation unit 
If you wrote a global variable or a function name in the another file and you are importing it be careful on the variable and function names as the compiler sees them as one
Below is an example of the merge syntax
```
merge "test"  #The compiler will append the .unn file extension
#You can also just add the extension manually
##
merge "test.unn"
##
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
int x=0;
while (x > 5) {
    int y=10;
    y+2;
    x=x+1;
}

```

*For loops*
```
for (mut int i | i < 10 | i++) {
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
    use behavior combat@super;

    use data attributes;
    use data attributes@speed;

    func greet(): string {
        return "Hello, I have " + self.health + " HP";
    }
}

func main: int{
    auto p = new Player(100);
    shout! p.speed;
    return 0;
}

```
## Data blocks
The data blocks are a way to group data that might be related the mutability qualifier means all the data inside the block is mutable but someone can specify the exact data they want to be mutable
Note: By default the data block is immutable and all the data inside is immutable unless you explicitly say 'mut' on the whole block or the individual data inside
Data blocks only allow for variable declarations can have values initialized or not
```
data PhoneSpecs {
    mut string name;       # Field 0 (Mutable)
    string color = "Blue"; # Field 1 (Immutable)
}

func main(): int {
    # 1. Instantiation
    PhoneSpecs p1 = PhoneSpecs { name= "Initial", color= "Red" }; 
    
    # 2. Mutating the 'mut' field
    p1.name = "Samsung"; 
    
    # 3. Accessing all fields
    shout! p1.name;
    shout! p1.color;
    
    return 0;
}

```

## Enums
Unnameable supports `enum class` as the default way to define enums a clean and type-safe way to group related symbolic values under a single name.

```
enum class Animal {
    CAT,
    DOG,
}

Animal pet = Animal::CAT;

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

## SAGE memory model
Unnameable uses a memory model called Stack Aligned Garbage Elimination(SAGE). 
SAGE is a deterministic memory model designed for maximum speed and predictability. The responsibility for memory correctness entirely depends on the compiler and is enforced by layers like `sentinel` and the `layout layer` 

Core philosophy
- Single contigous heap: All heap-raised objects live in a single, preallocated memory block
- Stack like allocation: Objects are allocated and freed in strict Last-in-First-Out(LIFO) order,mimicking a stack inside a heap
- Deterministic memory layout: Memory is fully calculated at compile-time, This ensures exact object placement and zero surprises at runtime
- No runtime overhead: After allocation, SAGE simply executes, no hidden checks, garbage collection, or automatic reference comunting

How it works
- Allocation: The compiler calculates the total memory of all heap raised objects, and then it requests for the memory at once from the OS it the creates the SAGE heap which has a 3 pointer i.e. base pointer, frame pointer and end pointer the base pointer points to the address of the memory the compiler received it is used to check underflows, the frame pointer is the one we move around when we allocate objects it points to the address next free part in the SAGE heap, the end pointer points to the address at the end of memory we were given it is used to check for overflows and also we slide it back with the frame pointer when we free memory this is to avoid memory hogging, then heap raised are are allocated sequentially on the SAGE heap.

- Deallocation: Objects are freed in LIFO order according to their last use, automatically tracked by the compiler, the programmers job is to ensure this order holds 

- Safety enforcement: The `Sentinel layer` checks that objects are used and freed in the correct order, blocking illegal operations at runtime all this is checked at compile time

Key features(This is all in theory as I havent yet done robust testing)
- Extreme perfomance: The contigous memory layout and stack like access maximize cache effeciency
- Predictability: No hidden memory spikes or pauses, allocation and freeing are deterministic
- Compile-time safety: *Sentinel* prevents out-of-order freeing without runtime checks

## Heap raising
Since Unnameable is using a custom memory model(SAGE) components and data blocks by default shall be heap allocated but a user might want some to manually promote some features to the heap themselves 
They can do this using the `heap` keyword which is a heap promoter it will tell the compiler to treat that variable as a full blown component and place it on the heap
Although it is only allowed on let statements and has some special rules applying to it as seen below
```
#Normal use of the heap promoter
heap int x=10;
heap mut int y=67;
heap const int z=78;

heap int x; #This will not be allowed as the compiler will ask for an initialization(If its important to be placed on the heap atleast initialize it)

heap int? x=null; #This will be rejected by the compiler as we dont want to account for nulls in SAGE
heap int? x; #Same story not allowed 

#Freeing order of heap raised values
##The compiler will block the code below as it violates LIFO(Last In First Out) rules 
z was the last to be declared and so should be freed first and not y
The error message looks like this

[SENTINEL ERROR] Non LIFO free detected, Tried to free 'y' which is not on top of the SAGE stack on line: 4, column: 1##
heap mut int y=10; 
heap mut short z=11;

y=y+1;
z=z+1s;

```

## References in Unnameable
Unnameable provides a safe and explicit way of refering to existing symbols without copying their values 
They act as symbolic aliases
They have a syntax of `ref <type> <name> => &<target> `
- ref :introduces a reference variable
- &<target> : specifies the address of the variable being referenced
- The type after ref must match the target type unless type inference is used(You can infer by just not adding the type)
```
heap int a=10;
ref int b -> &a;
```
Here b refers to a. Any modification through b directly affects a

*Type inference*: 
If you omit the type, the compiler infers it from the target 
```
ref b -> &a; #Infered as int 
```

*Mutability rules*: 
By default refrences are immutable but the user must specify if they want it to be mutable, a mutable reference cannot reference an immutable target 
```
heap mut int a = 6;
ref mut int b -> &a;

heap int x=7;
ref mut int y -> &x; #Error: Since a mutable reference cannot be made to an immutable target

```

*Heap rule*: 
References must be made to heap raised values this way the compiler can guarantee that ur not making a reference to a non existant variable. So basically if u want to use references heap raise them
```
int a=9;
ref int b -> &a; #Error since u can't reference a non-heap raised variable
```

*Usage of reference variables*
Reference variables must reference one target, you cannot reassign the target to which they point 
```
heap mut int x=19;
ref mut int y-> &x;

heap mut int z=23;
y = &z; #Error since u cannot change the variable reference y is already pointing to

#This is not to be confused with this
y=z; #Here I am simply using the value of z to change x since y is referencing x
#It is essentially the same thing as saying 
y=23; #This is allowed and very okay
``` 
*Quick note* :You cannot heap raise a reference itself this basically means a reference remains as a call stack variable 

*Quick note* : References affect the lifetime of heap raised value the compiler will not free an object whose reference count isn't zero doesnt matter where u last used it the compiler will not free that object until all the references are popped off the call stack



## Pointers in Unnameable
Pointers in Unnameable are low level constructs that store memory addresses. 
They allow direct access and manipulation of values in memory, with no hidden behaviors or implicit conversations
They have a syntax of `ptr <type> <name> = &<target>`
- ptr :introduces a pointer variable
- <type>: This is the type of the pointer if the user for example write int it is converted to an int_ptr(The type can be infered)
- &<target>: This is address of the variable that you want to store
```
int x=10;
ptr int p => &x; #pointer 'p' stores the address of x
```

The type can be infered for the user if you simply just dont add the type
But if u add the type then you must ensure it matches the address type
What I am trying to say is pointer types are very strict there is no casting between pointer types and non pointer types 
```
ptr p=> &x; #pointer 'p' will be infered to a int_ptr
uint y=18u;
ptr int z -> &y; #Error since you told the compiler 'z' is an int_ptr and now you are giving it a uint_ptr
```

Inorder to use pointers in Unnameable you must always initialize the pointer with an address this is to atleast tell the compiler that you're pointer is not null atleast in the beginning(Some safety is better than none)
```
ptr int p; #Error since you must always initialize the pointer
```
Unlike references where u can only refer to a heap raise variable for safety, Pointers do not have this rule as it would be very limiting, inherently they are not that safe because of a lack of this rule since the heap system(SAGE, and Sentinel) cannot track ur pointer for you unless it is pointing to a heap raised address then in that case you are safe 
Quick note you cannot directly heap raise a pointer it inherits its heapiness from the address it stores 
```
heap int x=10;
ptr p -> &x; #This is a heap raised pointer since x is heap raised it is safe 

##
int x =10;
ptr p -> &x; #This is a stack pointer it is not safe the user must be careful when using this as they could dereference it and yet x doesnt exist
##
```

You cannot reassign to an immutable pointer by the way if you want to do that you must specify that the pointer is mutable
```
ptr mut p -> &x;
p=&y; #This reassignment is allowed since pointer 'p' is mutable
```

Note: I will add more safety nets on them as I go along but for now that is it 

## Dereferencing pointers in Unnameable
Okay so to dereference a pointer in Unnameable I decided the syntax to be `deref <pointer_name>` I decided to use that because I hate the asterisk confusion
Anyways dereferencing is the usual it is a way to access the contents of the address the pointer is storing 
```
int x= 10;
ptr p -> &x;
deref p= 16; # I am dereferencing 'p' so I can manipulate the value of x 
```
A dereference has the same mutability and heap rules  as the target
```
mut int x=7;
ptr int p -> &x;
deref p=10; #This is allowed since x is mutable
```



## Requirements
- C++17 or later
- g++ or clang
- LLVM 18
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
