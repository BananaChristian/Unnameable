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

- Lexer _(in progress)_
- Parser _(in progress)_
- Semantic Analyzer _(in progress)_
- Layout Calculator _(active development)_
- Sentinel layer _(active development)_
- LLVM IR Codegen _(active development)_

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

- `float` — 32-bit floating-point numbers
- `double`— 64-bit floating point number

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
i32 a;
u32 x=8u32;
a = 2;

i16 y=8i16;
u16 m=8u16;

string name = "Iron";

float pi = 3.14;

double pi=3.14d;

auto y = 42;  # Type inferred as int

char8 test='A';

char16 test=u'A';

usize x= 10uz;
isize y=18iz;


```

## Null Safety

By default all the variables, function return types are not nullable but with the ? on a data type this acknowledges that the variable or data type are nullable.
This means the compiler will force you to explicitly deal with these via coalescing or using if statements but It wont allow null to be passed into variables or get used anywhere

```
string? name= null;
func greet(string? name): string?{
    return "Hello" + name;
}

i32? x=null;
i32 y=x;#This throws an error because x has a null value

##Correct code
i32? x=8;
i32 y=x;#This will run since x now has a value
##

i32? a;
a=null;
a+1;#This will error out since u cannot use null in operations

#Usage of uninitialized variables is not allowed in the langauge
i32? c;
c+1; #BOOM error
#OR
i32 test;
i32 y=test;#Error since 'test' is not initialized

#Coalescing
i32? x=null;
(x??1)+1;

#NOTE: Coalescing only works if the the variable is nullable

#Function declarations
func greet(string? name): string?;

```

## Functions

Functions in unnameable are written using the func keyword,the function name and the optional parameters and the return type

```unn
func greet(string name): string {
    return "Hello" + name;
}

func test(arr[i32] my_array): arr[i32]{
    return [1,3];
}

```

The return type must correspond to what is being returned
Functions can also return pointers for the concrete types but void pointers arent allowed

```unn
i32 x=100;
func test():ptr i32{
    ptr int y -> addr x;
    return y;
}

func main(): i32{
    test();
    return 0;
}
```

## Seals
Unnameable is extremely strict on naming as it doesnt allow overloading. Seals exist to prevent name collisions for functions across compilation units and across mergers.
Seals create isolated scopes for functions. You cannot directly access a sealed function you must access it through its seal.
At code generation the sealed function will get name mangled so for example add in a seal called Ops will become Ops_add.
Exportable functions must only be inside seals or components.
If a seal is exportable then every function in that seal is exportable but you can make individual functions in a seal exportable and others private to a current compilation unit or mergers
```
seal Test{
    #add isn't seen globally and cannot be accessed globally
    func add(i32 x,i32 y):i32{
        return x+y;
    }
}

export seal Food{
    #eat will become exportable
    func eat:string{
        return "Eat";
    }
}

seal AnotherTest{
    #test is a private function
    func test:i32{
        return 1;
    }

    #otherTest is an exportable function
    export func otherTest:i32{
        return 1;
    }
}

func main:i32{
    Test.add(10,10);#To the compiler this is Test_add
    return 0;
}
```

## Generic and multigenerics

Unnameable uses a system of generics called Explicitly Instantiated Generics(EIG) where the user defines what the functions they want inside a generic block which takes arguments of the types. Then an instantiate statement allows the user to tell the compiler what sort of types they would like to create generics for.
The alias on the instantiate statement is to uphold the langauge's rules of no overloading.

This means when the compiler generates the generic functions after the user instantiates so for example the add for `int` will not be the same as the one for `float` the compiler will automatically add the aliases to the name generating something like `IntOps_add` and `FloatOps_add` as seen below.

Note: The only top level statements allowed are function statements which means you can define or declare functions inside the generic blocks but other stuff is prohibited

This system also supports multigenerics as shown below

```
generic MathOps(T){
    func add(T a, T b): T{
        return a+b;
    };

    func subtract(T a, T b):T{
        return a-b;
    }
}

instantiate MathOps(i32) as IntOps;

##
Here the user is explicity telling the compiler the types he wants the compiler to generate
Hence the functions generated will look something like this

func IntOps_add(i32 a,i32 b): i32{
    return a+b;
}

func IntOps_subtract(i32 a,i32 b): i32{
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

generic MathMultiOps(T,M){
    func add(T a,M b): M{
        return a+b;
    };

    func subtract(T a,M b): M{
        return a-b;
    }
}

instantiate MathMultiOps(i32, float) as MultiOps;

##Here the function generated will be
func MultiOps_add(int a,float b): float{
    return a+b;
}

func MultiOps_subtract(i32 a ,float b): float{
    return a-b;
}
##

func main: int{
    int first_result=IntOps_add(10,18);
    float second_result=MultiOps_add(14,9.0);


    return 0;
}
```

## Arrays

Unnameable supports single dimensional and multidimensional arrays, The user must explicity list the length of the array if they dont provide initialize but if they initialize the array the compiler will just count the items inside
The dimension count is what tells the compiler if an array is multideminsional or not for example
```
arr[i32] [2][3] matrix;
#The compiler will know that this a nested array of type arr[arr[int]] because it has 2 dimension
```
The dimension count are those square brackets that follow the array type like in our example above they were two so those are two dimensions

The compiler can also infer the dimensions of the array but only and only if the array has been initialized  for example
```
arr[i32] matrix = [
        [10, 20, 30],
        [40, 50, 60]
];
#The compiler will infer this to be 2 dimensional
```
Currently the compiler doesnt do bounds checking and all that so it cant know the length of the array so if you declare different lengths in a dimension and use something else in the array literal the compiler cannot guard you it will just take the length of the literal instead(I plan on finding a solution to this but for now it is what it is)  for example
```
func main: i32 {
    arr[i32] [2] test=[5,6,7]; #The compiler doesnt warn u it will just use the literal length of 3 so be careful
    
    return 0;
}
```
Dimension counts must match unlike lengths the dimension counts must match because here the compiler checks dimensions and it actually uses them to know the type of an array(arr[i32] or arr[arr[i32]]) so a dimension mismatch will cause errors for example

```
func main: i32 {
    arr[i32] [2][3] test=[5,6,7];#This will trigger a dimension mismatch and later a type error 
    return 0;
}
```
You cannot reassign to an immutable array so be careful there if you want to reassign you must use the `mut` keyword otherwise it will cause errors for example

```
func main: i32 {
    mut arr[i32] [2] my_array=[5,6];
    my_array=[8,7];

    
    return 0;
}
```

Also by default all the values in the array are immutable unless a `mut` keyword is used on the array declaration itself to allow for reassignment via array access for example
```
func main: i32 {
    # 2D Array: 2 rows of 3 integers (Total 6 elements)
    mut arr[i32] [2][3] matrix = [
        [10, 20, 30],
        [40, 50, 60]
    ];
    
    # Assign a new value to the element at Row 1, Column 2 (which holds 60)
    matrix[1][2]=99;

    #This will print 99
    shout! matrix[1][2]
    
    return 0;
}

```

## Error handling

This is the current system for error handling but its not concrete yet I look to redesigning it and making it better and more powerful

```unn
func greet(string name): string? {
    return "Hello" + name, error!"Got an error";
}

```

## Multi file support
Unnameable supports multi file usage with two systems merge and import
To link the user must explictly use a link statement I am yet to add it though but thats the plan.
Merge as the name implies merges multiple files into one compilation unit
If you wrote a global variable or a function name in the another file and you are importing it, be careful on the variable and function names as the compiler sees them as one  PS: Use seals to avoid collisions
Below is an example of the merge syntax

```
merge "test"  #The compiler will append the .unn file extension
#You can also just add the extension manually
##
merge "test.unn"
##
```
Import allows a user to import content from an external file that had been marked as exportable
The compiler loads a stub file and uses the info from the stub file in its analysis
For now imports only support seals but I plan on expanding them to type builders like components etc.
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
for (mut i32 i | i < 10 | i++) {
    int x;
    x = x + 1;
}

```

## Component System (OOP-like structure)

Unnameable supports components, clean structures for organizing data and behavior, like classes, but lightweight and predictable.

```unn

# --- DATA BLOCK ---
data Attributes {
    mut i32 max_value;
}

# --- COMPONENT DEFINITION ---
component Entity {
    # Component Fields (Field declarations use explicit types)
    i32 health;

     # Import Data Block
    use data Attributes;

    # Component Method
    func check_health_ratio(): i32 {
        # Explicit type declarations for local variables
        i32 current_health = self.health;
        self.max_value=100;
        i32 maximum = self.max_value;

        return (current_health * 10 )/ maximum;
    }

    # Component Constructor
    init(i32 h) {
        # Initialize the health field
        self.health = h;
    }
}

# --- MAIN EXECUTION ---
func main(): i32 {
    # Explicit type declaration for player object
    Entity player = new Entity(70);

    # Explicit type declaration for the local variable
    int player_ratio = player.check_health_ratio();

    # Output results:
    shout! "Player Ratio: ";
    shout! player_ratio;

    shout! "Player Health: ";
    shout! player.health;

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

enum class TokenType: u32{
    ADDITION,
    SUBTRACTION,
}

enum class TokenType: u32{
    ADDITION=10u,
    SUBTRACTION=20u,
}

```

## Immutability and Mutability

Unnameable supports explicit control over variable mutability with the keywords _const_ and _mut_

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
Type inference with auto works together with _mut_ and _const_ for example

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
- Compile-time safety: _Sentinel_ prevents out-of-order freeing without runtime checks

## Heap raising

Unnameable is using a custom memory model called (SAGE) a user might want some to manually promote some features to the SAGE heap themselves us
They can do this using the `heap` keyword which is a heap promoter it will tell the compiler place it on the heap
Although it is only allowed on let statements(for now, I look to extend it to other declarations like pointers and arrays and so on) and has some special rules applying to it as seen below 

```
#Normal use of the heap promoter
heap i32 x=10;
heap mut i32 y=67;
heap const i32 z=78;

heap i32 x; #This will not be allowed as the compiler will ask for an initialization(If its important to be placed on the heap atleast initialize it)

heap int? x=null; #This will be rejected by the compiler as we dont want to account for nulls in SAGE
heap int? x; #Same story not allowed

#Freeing order of heap raised values
##The compiler will block the code below as it violates LIFO(Last In First Out) rules
z was the last to be declared and so should be freed first and not y
The error message looks like this

[SENTINEL ERROR] Non LIFO free detected, Tried to free 'y' which is not on top of the SAGE stack on line: 4, column: 1##
heap mut i32 y=10;
heap mut i16 z=11i16;

y=y+1;
z=z+1s;

```

## Dynamic heap raising and custom allocators
Unnameanle allows the user to create their own custom allocators and plug them in for the compiler to use them 
It uses allocator blocks where the user must satisfy the allocator contract, the contract exists because the compiler calls the corresponding allocator and deallocator in  a certain way so it expects a standard signature 
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
Now you can use this custom allocator by doing a dynamic heap raise using the  `dheap` keyword
This will tell the compiler that you wanna use the dynamic heap but using a certain allocator which you must tell the compiler as seen below 
```
dheap<MyAllocator> i32 x=100;
``` 
So the compiler will allocate using the allocator function you provided and free using the deallocater function that u provided 
The compiler will still use its last use analysis to inject your free to avoid memory bugs just like the normal heap raising with SAGE works difference is here the compiler doesnt care about how you use your stuff there is no LIFO to follow 
Now one last thing to add is that there is a default dheap allocator if you do not specify the compiler will use malloc and the regular free for now atleast for example
```
dheap i32 x=100; #This is malloc under the hood

```
Note: I am ironing out some bugs on the last use analysis when it comes to assignment statements as they are proving to be a headache

## Address operator in Unnameable
In unnameable the  `addr` operator is strictly for obtaining the memory address of a variable, It is used in  pointers to show the target
For example 
```
heap i32 a =10;
ptr p-> addr a;
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
heap i32 a=10;
ref i32 b -> a;
```

Here b refers to a. Any modification through b directly affects a

_Type inference_:
If you omit the type, the compiler infers it from the target

```
ref b -> a; #Infered as int_ref
```

_Mutability rules_:
By default refrences are immutable but the user must specify if they want it to be mutable, a mutable reference cannot reference an immutable target

```
heap mut i32 a = 6;
ref mut i32 b -> a;

heap i32 x=7;
ref mut i32 y -> x; #Error: Since a mutable reference cannot be made to an immutable target

```

_Heap and Global rule_:
References must be made to only heap raised or global values this way the compiler can guarantee that ur not making a reference to a non existant variable. So basically if u want to use references heap raise the variables you want to reference or place the in global scope

```
i32 a=9;
ref i32 b -> a; #Error since u can't reference a non-heap raised variable
```

_Usage of reference variables_
Reference variables must reference one target, you cannot reassign the target to which they point

```
heap mut i32 x=19;
ref mut i32 y->  x;

heap mut i32 z=23;
y = z; #Error since u cannot change the variable reference y is already pointing to

#This is not to be confused with this
y=z; #Here I am simply using the value of z to change x since y is referencing x
#It is essentially the same thing as saying
y=23; #This is allowed and very okay
```

_Quick note_ :You cannot heap raise a reference itself this basically means a reference remains as a call stack variable

_Quick note_ : References affect the lifetime of heap raised value the compiler will not free an object whose reference count isn't zero doesnt matter where u last used it the compiler will not free that object until all the references are popped off the call stack

Example

```
func main(): i32{
    #Target MUST be heap raised
    heap mut i32 a = 10;

    # Reference creation (type inferred)
    ref mut b -> a;

    #Write 70 to the memory location of 'a' via 'b'
    i32 c=70;
    b = c; #This will place 70 into the location of a

    # Read 'a's value (50) via 'b' and add 1
    int result = b + 1;
    shout! result;# Result will be 71
    return result;
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
int x=10;
ptr int p -> addr x; #pointer 'p' stores the address of x
```

The type can be infered for the user if you simply just dont add the type
But if u add the type then you must ensure it matches the address type
What I am trying to say is pointer types are very strict there is no casting between pointer types and non pointer types

```
ptr p -> addr x; #pointer 'p' will be infered to a i32_ptr
u32 y=18u32;
ptr int z -> addr y; #Error since you told the compiler 'z' is an i32_ptr and now you are giving it a u32_ptr
```

Inorder to use pointers in Unnameable you must always initialize the pointer with an address this is to atleast tell the compiler that you're pointer is not null atleast in the beginning(Some safety is better than none)

```
ptr i32 p; #Error since you must always initialize the pointer
```

_Heap and Global rule_:
Pointers must point to only heap raised or global variables this way the compiler can guarantee that ur not going to use a dangling pointer. So basically if u want to use pointers heap raise the variables you want to point to or place the in global scope

```
heap int x=10;
ptr p -> addr x; #This is a heap raised pointer since x is heap raised it is safe

##
int x =10;
ptr p -> addr x; #This is a stack pointer it is not safe the compiler will immediately stop compiling
##
```

You cannot reassign to an immutable pointer by the way if you want to do that you must specify that the pointer is mutable

```
ptr mut p -> addr x;
p=addr y; #This reassignment is allowed since pointer 'p' is mutable
```

Examples

```
i32 x = 10;

func main(): i32 {
    ptr mut i32 p -> addr x;

    # Write 50 to the memory location pointed to by p (i.e., update x)
    deref p = 50;

    # Read the value at p (50) and add 1
    i32 result = deref p + 1;

    # The original variable x should now also hold 50
    shout! result; #Should be 51
    return result;
}
```

Note: I will add more safety nets on them as I go along but for now that is it

## Dereferencing pointers in Unnameable

Okay so to dereference a pointer in Unnameable I decided the syntax to be `deref <pointer_name>` I decided to use that because I hate the asterisk confusion
Anyways dereferencing is the usual it is a way to access the contents of the address the pointer is storing

```
i32 x= 10;
ptr p -> addr x;
deref p= 16; # I am dereferencing 'p' so I can manipulate the value of x
```

A dereference has the same mutability and heap rules as the target

```
mut i32 x=7;
ptr i32 p -> addr x;
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
