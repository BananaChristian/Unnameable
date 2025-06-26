# Iron Compiler

**Iron** is a modern, lightweight programming language designed to be fast, minimal, and expressive.  
This project contains the core implementation of the Iron compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, and a simple REPL for testing.

---

## Features

- Custom lexer and tokenizer
- Custom parser *(In development)*
- Semantic analysis *(Planned)*
- LLVM IR *(Planned)*

---

## Data types in Iron
-Integers
-Booleans
-Strings
-Chars
-Floats 

---

## Comments in Iron
```
#This is a comment in iron
```

## Variable declarations and AST output
```
int x;
x=2;

string name="Iron";

float pi=3.14;
```

## Type inference
```
auto x;
x=2;
```

## Functions in iron
```
work greet(string name): string{return "Hello"+ name;}
```

## Function calls in Iron
```
greet("Iron");
add(1,2);
config();
```
## Control flow in Iron
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
---
## FUTURE ADDITIONS FOR LEXER SUPPORT
-Unicode
-UTF-8 multibyte characters 
-UTF-16 
-UTF-32

---
## FUTURE ADDITIONS FOR PARSER SUPPORT
-High order functions
-For loops
-Pattern matching
-Function parameters as expressions


---
## REQUIREMENTS 
-C++17 or later
-g++ or clang 
