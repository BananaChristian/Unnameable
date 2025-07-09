# Unnameable Compiler

**Unnameable** is a statically type, Ahead of time compiled modern, lightweight programming language written in C++. It is designed to be fast, minimal, and expressive.
It exists to make low level development **clear**,**accesible** and **fun** for everyone, especially for beginners and builders on modest machines.
Born from frustration with the complexity of C++, the strictness of Rust, and rawness of C, Unnameable aims to strike a balance **fast and powerful, yet simple and predictable**

No matter what you’re building an OS, a game engine, or your first kernel 
you shouldn't need a PhD or wrestle with books just to understand memory management.

Unnameable is here to bring the joy back to low-level programming.

This project contains the core implementation of the Unnameable compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, and a simple REPL for testing.

## Features

- Custom lexer and tokenizer *(To be extended)*
- Custom parser *(To be extended)*
- Semantic analysis *(In development)*
- LLVM IR *(Planned)*

## Data types in Unnameable

- Integers
- Booleans
- Strings
- Chars
- Floats 

## Comments in Unnameable

```
#This is a comment in Unnameable
```

## Variable declarations 
```
int x;
x=2;

string name="Iron";

float pi=3.14;
```

## Type inference
```
auto x=2;
```

## Functions in Unnameable
```
work greet(string name): string{
      return "Hello"+ name;
}
```
## Error handling
```
work greet(string name): string{
      return "Hello"+ name,error("Got an error");
}
```

## Function calls in Unnameable
```
greet("Blank");
add(1,2);
config();
```
## Control flow in Unnameable
```
if(age>18){
      return "adult";
}elseif(x<18){
      return "Not an adult";
}else{
      return "Don't know";
}
```

## While loops
```
while(x>5){
      return x;
}
```

## For loops
```
for(int i;i>10;i++){
    int x;
    x=x+1;
}
```
## FUTURE ADDITIONS FOR LEXER SUPPORT

- Unicode
- UTF-8 multibyte characters 
- UTF-16 
- UTF-32
  
## FUTURE ADDITIONS FOR PARSER SUPPORT

- High order functions
- Pattern matching
- Function parameters as expressions

## REQUIREMENTS 

- C++17 or later
- g++ or clang 
