# Quil Compiler

Quil is a modern, lightweight programming language designed to be fast, minimal, and expressive.  
This project contains the implementation of the Quil compiler in C++, built from scratch with a custom lexer, parser,abstract syntax tree (AST) and a simple REPL for testing.

## ✨ Features

- [x] Custom lexer and tokenizer
- [x] Pratt parser for expression parsing
- [x] Infix and prefix expression handling
- [x] Return statements and expression evaluation
- [ ] Semantic analysis (WIP)
- [ ] Code generation / bytecode / VM

## 🔧 Example Input and output
Quil is running (type 'exit' to quit)
>> return x-5+y*8-6*y;

--- Tokens ---
  Type: Token Type: RETURN, Literal: "return"
  Type: Token Type: IDENTIFIER, Literal: "x"
  Type: Token Type: MINUS, Literal: "-"
  Type: Token Type:INTEGER, Literal: "5"
  Type: Token Type: PLUS, Literal: "+"
  Type: Token Type: IDENTIFIER, Literal: "y"
  Type: Token Type: ASTERISK, Literal: "*"
  Type: Token Type:INTEGER, Literal: "8"
  Type: Token Type: MINUS, Literal: "-"
  Type: Token Type:INTEGER, Literal: "6"
  Type: Token Type: ASTERISK, Literal: "*"
  Type: Token Type: IDENTIFIER, Literal: "y"
  Type: Token Type: SEMICOLON, Literal: ";"
  Type: Token Type:END, Literal: ""
Parsing token: return
[DEBUG] Parsing return expression token: x
[DEBUG] Initial left expression: Identifier Expression: x
[DEBUG] Looping for token: -
[DEBUG] parsing infix with operator: -
[DEBUG] Initial left expression: Identifier Expression: 5
[DEBUG] Updated left expression: Infix Expression: (Identifier Expression: x - Identifier Expression: 5)
[DEBUG] Looping for token: +
[DEBUG] parsing infix with operator: +
[DEBUG] Initial left expression: Identifier Expression: y
[DEBUG] Looping for token: *
[DEBUG] parsing infix with operator: *
[DEBUG] Initial left expression: Identifier Expression: 8
[DEBUG] Updated left expression: Infix Expression: (Identifier Expression: y * Identifier Expression: 8)
[DEBUG] Updated left expression: Infix Expression: (Infix Expression: (Identifier Expression: x - Identifier Expression: 5) + Infix Expression: (Identifier Expression: y * Identifier Expression: 8))
[DEBUG] Looping for token: -
[DEBUG] parsing infix with operator: -
[DEBUG] Initial left expression: Identifier Expression: 6
[DEBUG] Looping for token: *
[DEBUG] parsing infix with operator: *
[DEBUG] Initial left expression: Identifier Expression: y
[DEBUG] Updated left expression: Infix Expression: (Identifier Expression: 6 * Identifier Expression: y)
[DEBUG] Updated left expression: Infix Expression: (Infix Expression: (Infix Expression: (Identifier Expression: x - Identifier Expression: 5) + Infix Expression: (Identifier Expression: y * Identifier Expression: 8)) - Infix Expression: (Identifier Expression: 6 * Identifier Expression: y))
[DEBUG] return_value EXISTS: ;
Parsing token: 
Parser finished

--- AST ---
 Node ->  Return Statement: ( Token: return Value: Infix Expression: (Infix Expression: (Infix Expression: (Identifier Expression: x - Identifier Expression: 5) + Infix Expression: (Identifier Expression: y * Identifier Expression: 8)) - Infix Expression: (Identifier Expression: 6 * Identifier Expression: y)))

>> 

