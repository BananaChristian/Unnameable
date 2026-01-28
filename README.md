# Unnameable Compiler

**Unnameable** is a statically typed, ahead-of-time compiled modern programming language . It is designed to be fast, clear, and expressive.

---

This is the base compiler for the Unnameable Programming langauge written in C++ for now many features will not added till bootstrap but I want to make this base compiler as powerful as I could to make the bootstrap extremely easy without alot of rewriting(If I am to bootstrap that is)

This project contains the core implementation of the Unnameable compiler written in C++. It includes a custom lexer, parser,deserializer, semantic analyzer, layout calculator,sentinel layer, stubGen,and LLVM for the backend.

---

## Features

- Lexer
- Parser 
- Deserializer(It is just the stub deserializer if you import stuff)
- Semantic Analyzer 
- Layout Calculator 
- Sentinel layer 
- StubGen(This is the stub generator if you export stuff)
- LLVM (For the backend)

---

## Basic Data Types

- `i8` — 8-bit signed integers
- `u8` — 8-bit unsigned integers
- `i16` — 16-bit signed integers
- `u16` — 16-bit usigned integers
- `i32` — 32-bit signed integers
- `u32` — 32-bit unsigned integers
- `i64` — 64-bit signed integers
- `u64` — 64-bit unsigned integers
- `i128` — 128-bit signed integers
- `u128` — 128-bit unsigned integers
- `isize` — CPU native width-bit signed integers
- `usize` — CPU native width-bit unsigned integers

- `bool` — true/false values
- `string` — UTF-8 encoded sequence of characters.
  May contain Unicode text. (Basic support: no slicing or advanced string ops yet.)

- `char8` — 8 bit chars
- `char16` — 16 bit chars
- `char32` — 32 bit chars

- `f32` — 32-bit floating-point numbers
- `f64`— 64-bit floating point number

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
i32 a
u32 x=8u32
a = 2

i16 y=8i16
u16 m=8u16

string name = "Iron"

float pi = 3.14

double pi=3.14d

auto y = 42  # Type inferred as int

char8 test='A'

char16 test=u'A'

usize x= 10uz
isize y=18iz


```

## Global scope and visibility rules
Unnameable is very strict about what can be in global scope for cases like a normal variable the compiler will force you to make it a constant otherwise it shall not be allowed in the global scope.
By default variables in the global scope are private to prevent name collisions and issues like that to make a global variable public u must use the `export` keyword this will make that variable global and as the flag implies mark it for export by the export system 
But note that it must strictly be in the global scope and also must be a constant otherwise u will be blocked from marking it for export
```
const bool PRIVATE_VAL = true #This value is private by default so its local to that file only
export const i32 PUBLIC_VAL = 200 #This value is public 
```

## Functions

Functions in unnameable are written using the func keyword,the function name and the optional parameters and the return type

```unn
func greet(string name): string {
    return name
}

func test(arr[i32] my_array): arr[i32]{
    return [1,3]
}
```

The return type must correspond to what is being returned
Functions can also return pointers for the concrete types but void pointers arent allowed

```unn
i32 x=100
func test():ptr i32{
    ptr i32 y -> addr x
    return y
}

func main(): i32{
    test()
    return 0
}
```

## Seals

Unnameable is extremely strict on naming as it doesnt allow overloading. Seals exist to prevent name collisions for functions across compilation units.
Seals create isolated scopes for functions. You cannot directly access a sealed function you must access it through its seal.
At code generation the sealed function will get name mangled so for example add in a seal called Ops will become Ops_add.
Exportable functions must only be inside seals or components.
If a seal is exportable then every function in that seal is exportable but you can make individual functions in a seal exportable and others private to a current compilation unit.

```
seal Test{
    #add isn't seen globally and cannot be accessed globally
    func add(i32 x,i32 y):i32{
        return x+y
    }
}

export seal Food{
    #eat will become exportable
    func eat:string{
        return "Eat"
    }
}

seal AnotherTest{
    #test is a private function
    func test:i32{
        return 1
    }

    #otherTest is an exportable function
    export func otherTest:i32{
        return 1
    }
}

func main:i32{
    Test.add(10,10) #To the compiler this is Test_add
    return 0
}
```

## Generic and multigenerics

Unnameable uses a system of generics called Explicitly Instantiated Generics(EIG) where the user defines what the functions they want inside a generic block which takes arguments of the types. Then an instantiate statement allows the user to tell the compiler what sort of types they would like to create generics for.
The alias on the instantiate statement is to uphold the langauge's rules of no overloading.

This means when the compiler generates the generic functions after the user instantiates so for example the add for `i32` will not be the same as the one for `f32` the compiler will automatically add the aliases to the name generating something like `IntOps_add` and `FloatOps_add` as seen below.

Note: The only top level statements allowed are function statements which means you can define or declare functions inside the generic blocks but other stuff is prohibited

This system also supports multigenerics as shown below

```
generic MathOps(T){
    func add(T a, T b): T{
        return a+b
    };

    func subtract(T a, T b):T{
        return a-b
    }
}

instantiate MathOps(i32) as IntOps

##
Here the user is explicity telling the compiler the types he wants the compiler to generate
Hence the functions generated will look something like this

func IntOps_add(i32 a,i32 b): i32{
    return a+b
}

func IntOps_subtract(i32 a,i32 b): i32{
    return a-b
}
##

instantiate MathOps(f32) as FloatOps

##Same story here the generated functions will be

func FloatOps_add(f32 a, f32 b): f32{
    return a+b
}

func FloatOps_subtract(f32 a, f32 b): float{
    return a-b
}
##

#Multigeneric example

generic MathMultiOps(T,M){
    func add(T a,M b): M{
        return a+b
    };

    func subtract(T a,M b): M{
        return a-b
    }
}

instantiate MathMultiOps(i32, f32) as MultiOps

##Here the function generated will be
func MultiOps_add(i32 a,f32 b): f32{
    return a+b
}

func MultiOps_subtract(i32 a ,f32 b): f32{
    return a-b
}
##

func main: i32{
    i32 first_result=IntOps_add(10,18)
    float second_result=MultiOps_add(14,9.0)


    return 0
}`
```

## Arrays

## Arrays in Unnameable

Unnameable provides robust support for both single-dimensional and multidimensional arrays. Under the hood, arrays are handled as continuous memory buffers (pointers), ensuring high performance while maintaining a simple syntax for the user.

### Declaration & Dimensioning

The number of square brackets following the type defines the **dimensionality** of the array. You have two ways to define the size:

1. **Explicit Length:** Provide the size within the brackets.
2. **Inferred Length:** Leave the brackets empty and provide an initializer; the compiler will automatically count the elements for you.

```unn
# Explicit 2D array: 2 rows, 3 columns
arr[i32] [2][3] matrix

# Inferred 2D array: The compiler sees 2 rows of 3
arr[i32] matrix = [
    [10, 20, 30],
    [40, 50, 60]
]

```

### Type Resolution & Dimension Matching

Dimensions are strictly enforced. The number of nested levels in your literal must match the number of dimension brackets in your declaration. A mismatch here will trigger a compiler error because the dimensions define the structural type of the array.

> **Note:** While dimension counts and sizes must match, the compiler currently can only safe guard if you are using a constant integer literal any width is okay the compiler will just convert to 64 bit for example
`arr[i32] [2] x=[1,2,3]` the compiler will block this but saying `arr[i32] [mySize] x=[1,2,3]` the compiler currently cannot guard you so take caution with this

### Nullable Arrays and Unwrapping

Arrays can be declared as **nullable** by adding a `?` to the type. This is useful for arrays that might not be initialized immediately. However, before you can use or copy a nullable array into a standard array, you must use the `unwrap` keyword.

```unn
func main: i32 {
    # A nullable 2D array
    arr[i32]? [2][2] x = null
    
    # To use it, you must unwrap it
    # If x is still null, the program will safely crash (panic)
    arr[i32] y = unwrap x 
    
    return 0
}

```

**Safety Warning:** Unwrapping a `null` value will cause an immediate "Illegal Instruction" (core dump). This prevents your program from running with "garbage" data or causing silent corruption.
Indexing for now can bound check on single dimensional if you index more than the size your compiled program will generate an illegal instruction


### Mutability and Assignment

By default, arrays are **immutable**.

* **To reassign the whole array:** You must use the `mut` keyword.

```unn
func main: i32 {
    # Declare a mutable 2D array
    mut arr[i32] [2][3] matrix = [
        [10, 20, 30],
        [40, 50, 60]
    ]

    # Reassign a single element
    matrix[1][2] = 99

    # Reassign the entire array structure
    matrix = [
        [1, 1, 1],
        [2, 2, 2]
    ]

    shout! matrix[1][2] # Prints 2
    return 0
}

```

## Null Safety & Error Handling
By default, all variables, parameters, and function return types are **non-nullable**. The compiler treats a null value as a completely different "shape" than a standard value, preventing accidental usage through strict semantic checks and an explicit IR-level boxing system.

### The Optional Type (`?`)

To allow a variable to hold a null value, you must explicitly mark it with the `?` suffix. This tells the compiler to wrap the data in an **Optional Box**.

```unn
string? name = null
i32? x = 8

func greet(string? name): string? {
    return name
}

```

### Strict Usage Rules

The compiler enforces safety by preventing "leaky" nulls from entering the logic of your program:

1. **No Direct Assignment:** You cannot assign an `i32?` to a regular `i32` directly.
2. **No Null Operations:** You cannot perform arithmetic or logic on nullable types (e.g., `a + 1` where `a` is `i32?` will fail).
3. **Initialization Tracking:** Usage of uninitialized variables is strictly forbidden, whether they are nullable or not.

```unn
i32? x = null
i32 y = x       # ERROR: Type mismatch (i32? vs i32)

i32? a = null
a + 1           # ERROR: Cannot perform math on a nullable type

i32 test
i32 y = test    # ERROR: 'test' is not initialized

```

### Enforcement: Unwrapping & Coalescing

When you have a nullable value, you must "open the box" to get to the data inside. unnameable provides two primary mechanisms for this:

#### 1. The `unwrap` Operator (Trap on Null)

The `unwrap` operator is the most direct way to handle an optional. It performs a **Runtime Reality Check**. If the value is present, it extracts it; if the value is null, the compiler triggers an immediate **deterministic trap** , terminating the program to prevent a segfault.

```unn
func test() : i32? {
    return null
}

func main() : i32 {
    # If test() returns null, the program traps here.
    # If it returns a value, x becomes a standard i32.
    i32 x = unwrap test() 
    shout! x
    return x
}

```

#### 2. The Coalescing Operator (`??`)

For more graceful recovery, the `??` operator allows you to provide a fallback value. This ensures that the resulting expression is always non-nullable.

```unn
func main(): i32 {
    i32? a = null
    i32? b = 42
    
    # a ?? (b ?? 100) resolves to 42 because a is null, but b is not.
    i32 y = a ?? (b ?? 100) 
    shout! y
    return 0
}

```

### Physical Implementation

Under the hood, the compiler does not use "Null Pointers." Instead, it uses a **Box and Flag** model.

* **The Flag (`i1`):** A boolean indicating if the value is "Present."
* **The Payload (`i32`):** The actual data.

When you `unwrap` or use `??`, the generated IR inspects the flag first. This architecture ensures that your code never touches "garbage" memory, turning silent segfaults into explicit, manageable errors.

## Casting
The language supports two types of casting i.e `cast` and `bitcast`
A cast converts a value from one type to another (e.g., f32 to i32). This may change the underlying bit pattern to preserve the numerical value.
For example
```
func test_math_cast():void {
    float pi = 3.14
    i32 rounded = cast<i32>(pi)
    shout! rounded
}

func main:i32{
    test_math_cast()
    return 0
}
```
Bitcasting reinterprets the raw bits of a value as a different type. The underlying bits never change. This is primarily used for low-level memory manipulation and converting `opaque(I am gonna add this soon)` pointers back to concrete types.
For example
```
func test_memory_bitcast():void {
    dheap float pi = 3.14
    ptr float p_float -> addr pi
    ptr i32 p_int -> bitcast<ptr i32>(p_float)
    
    i32 bit_pattern = deref p_int
    shout! bit_pattern
}

func main:i32 {
    test_memory_bitcast()
    return 0
}
```

## Multi file support

Unnameable supports multi file usage with two systems merge and import
To link the user must explictly use a link statement I am yet to add it though but thats the plan.

Import allows a user to import content from an external file that had been marked as exportable
The compiler loads a stub file and uses the info from the stub file in its analysis
For now imports only support seals,components and records but I plan on expanding them to enums now and looking at how the EIG system can fit in(PS: this EIG isnt a guarantee when it comes to this system but I will try) etc.
Below is an example of the syntax

```
import "test"
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
auto laugh="😂"
string 萌="cute"

```

## Function calls

```
greet("John")
add(1, 2)
config()

```

## Control flow

```
if (age > 18) {
    return "adult"
} elif (age < 18) {
    return "minor"
} else {
    return "unknown"
}

```

## Switch statements
The switch statements are similar to the ones used in other langueages like C or C++ but difference the compiler will force you to give a default case 
```
enum Status {
    PENDING,
    ACTIVE,
    CLOSED
}

func get_status_msg(Status s): string {
    switch (s) {
        case Status::PENDING: { return "Wait for it..." }
        case Status::ACTIVE:  { return "It is live!" }
        case Status::CLOSED:  { return "Game over." }
        default:              { return "Unknown state" }
    }
}

func main:i32{
    shout! get_status_msg(Status::PENDING)
    return 0
}

```

## Loops

_While loops_

```
i32 x=0;
while (x > 5) {
    i32 y=10;
    y+2;
    x=x+1;
}

```

_For loops_

```
func test_for_logic(): i32 {
    mut i32 total = 0

    for (mut i32 i = 0 ; i < 5 ; i = i + 1) {
        if (i == 1) {
            continue 
        }
        
        if (i == 3) {
            break 
        }
        
        total = total + 10
    }
    return total 
}

func main:i32{
    shout! test_for_logic()
    return 0
}

```

## Component System (OOP-like structure)

Unnameable supports components, clean structures for organizing data and behavior, like classes, but lightweight and predictable. Please not that you cannot nest these 

```unn

# --- RECORD ---
record Attributes {
    mut i32 max_value
}

# --- COMPONENT DEFINITION ---
component Entity {
    # Component Fields (Field declarations use explicit types)
    i32 health

     # Import Data Block
    use record Attributes

    # Component Method
    func check_health_ratio(): i32 {
        # Explicit type declarations for local variables
        i32 current_health = self.health
        self.max_value=100
        i32 maximum = self.max_value

        return (current_health * 10 )/ maximum
    }

    # Component Constructor
    init(i32 h) {
        # Initialize the health field
        self.health = h
    }
}

# --- MAIN EXECUTION ---
func main(): i32 {
    # Explicit type declaration for player object
    Entity player = new Entity(70)

    # Explicit type declaration for the local variable
    i32 player_ratio = player.check_health_ratio()

    # Output results:
    shout! "Player Ratio: "
    shout! player_ratio;

    shout! "Player Health: "
    shout! player.health

    return 0
}

```

## Records

Records are a way to group data that might be related the mutability qualifier means all the data inside the block is mutable but someone can specify the exact data they want to be mutable
Note: By default the record is immutable and all the data inside is immutable unless you explicitly say 'mut' on the whole block or the individual data inside
Records only allow for variable declarations and can have values initialized or not

```
record PhoneSpecs {
    mut string name       # Field 0 (Mutable)
    string color = "Blue" # Field 1 (Immutable)
}

func main(): i32 {
    #Instantiation
    PhoneSpecs p1 = PhoneSpecs { name= "Initial", color= "Red" }

    # Mutating the 'mut' field
    p1.name = "Samsung"

    #Accessing all fields
    shout! p1.name
    shout! p1.color

    return 0
}

```

## Enums

Unnameable supports `enum` as the default way to define enums a clean and type-safe way to group related symbolic values under a single name.

```
enum Animal {
    CAT,
    DOG,
}

Animal pet = Animal::CAT

enum HttpStatus {
    OK = 200,
    NotFound = 404,
    InternalServerError = 500,
}

auto code = HttpStatus::NotFound;

enum TokenType: u32{
    ADDITION,
    SUBTRACTION,
}

enum AnotherTokenType: u32{
    ADDITION=10u32,
    SUBTRACTION=20u32,
}

```

## Immutability and Mutability

Unnameable supports explicit control over variable mutability with the keywords _const_ and _mut_

- `const` declares an immutable variable. It must be initialized at declaration and cannot be reassigned later. This ensures safety and predictability.

```
const i32 x = 42  # Immutable, cannot be changed
```

- `mut` declares a mutable variable. It can be reassigned multiple times after declaration.

```
mut i32 y       # Mutable, can assign later
y = 10
y = 20          # Allowed
```

If no mutability keyword is given, variables are immutable by default, and must be assigned a value only once, either at declaration or later.
Type inference with auto works together with _mut_ and _const_ for example

```
mut auto z = 5    # Mutable variable with inferred type int
const auto w = 3  # Immutable variable with inferred type int
```

## The SAGE memory model

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
- Compile-time safety: _Sentinel_ prevents out-of-order freeing without runtime checks

## Heap raising

Unnameable is using a custom memory model called (SAGE) a user might want some to manually promote some features to the SAGE heap themselves us
They can do this using the `heap` keyword which is a heap promoter it will tell the compiler place it on the heap
Although it is only allowed on let statements(for now, I look to extend it to other declarations like pointers and arrays and so on) and has some special rules applying to it as seen below

```
#Normal use of the heap promoter
heap i32 x=10
heap mut i32 y=67
heap const i32 z=78

heap i32 x #This will not be allowed as the compiler will ask for an initialization(If its important to be placed on the heap atleast initialize it)

heap int? x=null #This will be rejected by the compiler as we dont want to account for nulls in SAGE
heap int? x #Same story not allowed

#Freeing order of heap raised values
##The compiler will block the code below as it violates LIFO(Last In First Out) rules
z was the last to be declared and so should be freed first and not y
The error message looks like this

[SENTINEL ERROR] Non LIFO free detected, Tried to free 'y' which is not on top of the SAGE stack on line: 4, column: 1##
heap mut i32 y=10
heap mut i16 z=11i16

y=y+1
z=z+1i16

```

NOTE: I am still struggling with heap raised pointers heck this entire SAGE thing mostly paired with the compiler's last use analysis as I am experiencing memory leaks and who knows what I havent seen so yeah, It is really not ready

## Dynamic heap raising and custom allocators

Unnameanle allows the user to create their own custom allocators and plug them in for the compiler to use them
It uses allocator blocks where the user must satisfy the allocator contract, the contract exists because the compiler calls the corresponding allocator and deallocator in a certain way so it expects a standard signature
This signature is the same signature as the one used by `malloc` and `free` in C. the user must satisfy this for the compiler to allow this custom allocator
The compiler doesnt care about your logic in these functions it just needs you to fulfill that signature

```
allocator MyAllocator{
    func alloc(usize size):ptr usize{
        #Whatever your logic is
    }

    func dealloc(ptr usize p):void{
        #Whatever your logic is
    }
}

```

Now you can use this custom allocator by doing a dynamic heap raise using the `dheap` keyword
This will tell the compiler that you wanna use the dynamic heap but using a certain allocator which you must tell the compiler as seen below

```
dheap<MyAllocator> i32 x=100
```

So the compiler will allocate using the allocator function you provided and free using the deallocater function that u provided
The compiler will still use its last use analysis to inject your free to avoid memory bugs just like the normal heap raising with SAGE works difference is here the compiler doesnt care about how you use your stuff there is no LIFO to follow
Now one last thing to add is that there is a default dheap allocator if you do not specify the compiler will use a default allocator I added and the regular free for example

```
dheap i32 x=100 #This is GPA under the hood

```

## Scoped Memory Management (The Loop Rules)

Unnameable's memory management is strictly enforced by the compiler to prevent leaks in long-running processes like loops. The compiler categorizes heap-allocated variables into two types to determine their lifespan.

### 1. Residents (Born in Loop)

Variables born **inside** a loop are considered **Residents**.

- **The Sweep:** To prevent memory from stacking up with every iteration, the compiler performs a mandatory "Body Sweep" at the end of every loop lap.
- **LIFO for SAGE:** If using the SAGE heap, Residents are allocated and freed in a strict Last-In-First-Out order during each iteration.
- **Fail-Safe:** Even if a Resident isn't explicitly used until the very end of the loop, the compiler injects a cleanup call before the loop repeats to ensure zero-leak performance.

```unn
while (counter > 0) {
    # Resident: Allocated every lap
    dheap i32 lap_data = 999

    shout! lap_data
    counter = counter - 1
    # Memory is automatically freed HERE before next lap
}

```

### 2. Elders (The Sanctuary Rule)

Variables born **outside** a loop but used within it are considered **Elders**.

- **The Sanctuary:** The compiler grants these variables Sanctuary, meaning it will **not** attempt to free them inside the loop body, even if their "last use" appears to be inside the loop.
- **Post-Loop Cleanup:** The memory is only released once the loop has fully terminated and the code execution reaches the end of the block.

```unn
heap i32 elder = 100 # Born outside

mut i32 counter=5
while (counter > 0) {
    shout! elder # Used inside
    counter = counter - 1
}

# Elder is safely freed HERE, after the loop finishes

```

## Memory Safety Guards

To ensure the integrity of the SAGE memory model and prevent pointer corruption, the following rules are strictly enforced:

### SAGE Loop Mutation Ban

Because SAGE is a stack-based allocator, mutating or referencing an **external** SAGE variable inside a loop is prohibited as it risks breaking the LIFO (Last-In-First-Out) order.

- If you need to mutate a variable across loop iterations, use `dheap` (Dynamic Heap) or a standard stack variable (`mut i32`).
- The compiler will throw a `sentinel error` if it detects a potential stack-smashing pattern in your SAGE usage.

### Last-Use Analysis

Unnameable does not use a Garbage Collector. Instead, it uses **Compile-Time Last-Use Analysis**.

- The compiler tracks exactly where a variable is used for the final time.
- It automatically injects the appropriate free call at that exact point.
- If a variable is never used, the compiler "nukes" it immediately after declaration to keep the memory footprint at absolute zero.

## Address operator in Unnameable

In unnameable the `addr` operator is strictly for obtaining the memory address of a variable, It is used in pointers to show the target it is similar to the ampersand operator in C but I wanted it to be clear 
For example

```
heap i32 a =10
ptr p-> addr a
```

_Quick note_:
The compiler sees `addr <variable>` as a pointer so the type of the variable matters if the variable `x` is an integer then `addr x` has a type of `int_ptr`

## References in Unnameable

Unnameable provides a safe and explicit way of refering to existing symbols without copying their values
They act as symbolic aliases
They have a syntax of `ref <type> <name> -> <target> `

- ref :introduces a reference variable
- <target> : specifies the the variable being referenced
- The type after ref must match the target type unless type inference is used(You can infer by just not adding the type)

```
heap i32 a=10
ref i32 b -> a
```

Here b refers to a. Any modification through b directly affects a

_Type inference_:
If you omit the type, the compiler infers it from the target

```
ref b -> a #Infered as int_ref
```

_Mutability rules_:
By default refrences are immutable but the user must specify if they want it to be mutable, a mutable reference cannot reference an immutable target

```
heap mut i32 a = 6
mut ref i32 b -> a

heap i32 x=7;
mut ref i32 y -> x #Error: Since a mutable reference cannot be made to an immutable target

```

_Heap and Global rule_:
References must be made to only heap raised or global values this way the compiler can guarantee that ur not making a reference to a non existant variable. So basically if u want to use references heap raise the variables you want to reference or place the in global scope

```
i32 a=9
ref i32 b -> a #Error since u can't reference a non-heap raised variable
```

_Usage of reference variables_
Reference variables must reference one target, you cannot reassign the target to which they point

```
heap mut i32 x=19
mut ref i32 y->  x

heap mut i32 z=23
y = z #Error since u cannot change the variable reference y is already pointing to

#This is not to be confused with this
y=z #Here I am simply using the value of z to change x since y is referencing x
#It is essentially the same thing as saying
y=23 #This is allowed and very okay
```

_Quick note_ :You cannot heap raise a reference itself this basically means a reference remains as a call stack variable

_Quick note_ : References affect the lifetime of heap raised value the compiler will not free an object whose reference count isn't zero doesnt matter where u last used it the compiler will not free that object until all the references are popped off the call stack

Example

```
func main(): i32{
    #Target MUST be heap raised
    heap mut i32 a = 10

    # Reference creation (type inferred)
    mut ref b -> a;

    #Write 70 to the memory location of 'a' via 'b'
    i32 c=70
    b = c #This will place 70 into the location of a

    # Read 'a's value (50) via 'b' and add 1
    i32 result = b + 1
    shout! result # Result will be 71
    return result
}
```

## Pointers in Unnameable

Pointers in Unnameable are low level constructs that store memory addresses.
They allow direct access and manipulation of values in memory, with no hidden behaviors or implicit conversations
They have a syntax of `ptr <type> <name> -> addr <target>`

- ptr :introduces a pointer variable
- <type>: This is the type of the pointer if the user for example write int it is converted to an int_ptr(The type can be infered)
- addr <target>: This is address of the variable that you want to store

```
i32 x=10
ptr i32 p -> addr x #pointer 'p' stores the address of x
```

The type can be infered for the user if you simply just dont add the type
But if u add the type then you must ensure it matches the address type
What I am trying to say is pointer types are very strict there is no casting between pointer types and non pointer types

```
ptr p -> addr x #pointer 'p' will be infered to a i32_ptr
u32 y=18u32
ptr i32 z -> addr y #Error since you told the compiler 'z' is an i32_ptr and now you are giving it a u32_ptr
```

Inorder to use pointers in Unnameable you must always initialize the pointer with an address this is to atleast tell the compiler that you're pointer is not null atleast in the beginning(Some safety is better than none)

```
ptr i32 p #Error since you must always initialize the pointer
```

_Heap and Global rule_:
Pointers must point to only heap raised or global variables this way the compiler can guarantee that ur not going to use a dangling pointer. So basically if u want to use pointers heap raise the variables you want to point to or place the in global scope

```
heap i32 x=10
ptr p -> addr x #This is a heap raised pointer since x is heap raised it is safe

##
i32 x =10
ptr p -> addr x #This is a stack pointer it is not safe the compiler will immediately stop compiling
##
```

You cannot reassign to an immutable pointer by the way if you want to do that you must specify that the pointer is mutable

```
mut ptr p -> addr x
p -> addr y #This reassignment is allowed since pointer 'p' is mutable
```

Examples

```
i32 x = 10

func main(): i32 {
    mut ptr i32 p -> addr x

    # Write 50 to the memory location pointed to by p (i.e., update x)
    deref p = 50

    # Read the value at p (50) and add 1
    i32 result = deref p + 1

    # The original variable x should now also hold 50
    shout! result; #Should be 51
    return result;
}

```
Note: I will add more safety nets on them as I go along but for now that is it


## The opaque pointer 
This is a special pointer type that has no type it is meant to be a carrier it is generic in nature but the compiler enforces some rules on it
For example you cannot dereference it, or do pointer arithmetic on it or even access what is inside them as the compiler doesnt know the type 
You can point to anything with it but you must bitcast it to a typed pointer to use it normally otherwise the compiler will not allow you to use it normally
```
component Data {
    i32 id
    init(i32 id){
        self.id=id
    }
}

component Player {
    i32 score
    init(i32 score){
        self.score=score
    }
}

# The Dispatcher
func generic_shout(ptr opaque item,i32 tag):void {
    if (tag == 1) {
        # Interpret as Data
        ptr d -> bitcast<ptr Data>(item)
        shout! d.id
    }
    elif (tag == 2) {
        # Interpret as Player
        ptr p -> bitcast<ptr Player>(item)
        shout! p.score
    }
}

func main():void {
    # 1. Prepare Data
    heap Data d = new Data(777)
    
    # 2. Prepare Player
    heap Player p = new Player(999)

    # 3. Use the same function for both!
    generic_shout(addr d, 1)
    generic_shout(addr p, 2)
}
```

## Dereferencing pointers in Unnameable

Okay so to dereference a pointer in Unnameable I decided the syntax to be `deref <pointer_name>` I decided to use that because I hate the asterisk confusion
Anyways dereferencing is the usual it is a way to access the contents of the address the pointer is storing

```
i32 x= 10
ptr p -> addr x
deref p= 16 # I am dereferencing 'p' so I can manipulate the value of x
```

A dereference has the same mutability and heap rules as the target

```
mut i32 x=7
ptr i32 p -> addr x
deref p=10 #This is allowed since x is mutable
```

## Requirements

- C++17 or later
- g++ or clang
- LLVM 18
- Make(optional for building)
Oh yeah and you might also need NASM and gcc to build the core runtime as they are in assembly and C
Also the compiler uses lld to link so you might need it 
Currently it can only work on linux x86_64 because that is what I coded the runtime towards but I will add support for other architectures and Operating systems soon

## Philosophy

I built Unnameable because I really hate a language hiding stuff from me, I would rather see the complexity upfront and try to understand it than the compiler babysitting me,Yes sometimes it is nice to get babysat but if I really want to understand what is going on that babysitting tends to be a recipe for disaster mostly in a systems language. 
I believe the compiler is a helper but I dont want to the compiler to think for me,instead it should guide me. Clarity above all else its just better for you to know that okay this is what I am doing right now than some smart compiler hiding stuff from you
But again sometimes hiding stuff is good it depends on what you are doing after all but in some cases it isnt and that is where Unnameable would work best. 

Basically what I am saying is at some point you will ask yourself why must I write 4 different 1's like 1u32 and again 1u64 just to match the types well that is exactly the point the compiler isnt hiding anything it will not implicitly convert your types for you or assume that this is what you wanted it just obeys if you play by the language rules of course. It is just an upfront tax for you to know every detail of what your doing and that detail must be communicated directly in the code 
I hope this is clear enough I will of course talk more about  the language philosophy but this is the gist of it clarity above anything else

## Current status

This project is under active and early development and is still highly experimental. If you want to explore,contribute, or just follow the journey feel free to fork,play or reach out.

---

## License

Unnameable is dual-licensed under the MIT License and Apache License 2.0.
You may choose either license to use this project.

> See LICENSE-MIT and LICENSE-APACHE for full details
