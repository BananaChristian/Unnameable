# Quil Compiler

**Quil** is a modern, lightweight programming language designed to be fast, minimal, and expressive.  
This project contains the core implementation of the Quil compiler written in C++. It includes a custom lexer, parser, abstract syntax tree (AST) builder, and a simple REPL for testing.

---

## ✨ Features

- ✅ Custom lexer and tokenizer
- ✅ Pratt parser for precedence-based expression parsing
- ✅ Infix and prefix expression handling
- ✅ Return statements and expression evaluation
- 🚧 Semantic analysis *(Work In Progress)*
- 🛠️ Code generation / bytecode / VM *(Planned)*

---

## 🔧 Example: Input → Tokens → AST

```quil
>> return x-5+y*8-6*y;
Type: RETURN, Literal: "return"
Type: IDENTIFIER, Literal: "x"
Type: MINUS, Literal: "-"
Type: INTEGER, Literal: "5"
Type: PLUS, Literal: "+"
Type: IDENTIFIER, Literal: "y"
Type: ASTERISK, Literal: "*"
Type: INTEGER, Literal: "8"
Type: MINUS, Literal: "-"
Type: INTEGER, Literal: "6"
Type: ASTERISK, Literal: "*"
Type: IDENTIFIER, Literal: "y"
Type: SEMICOLON, Literal: ";"

[DEBUG] Parsing return expression token: x
[DEBUG] Initial left expression: Identifier Expression: x
[DEBUG] parsing infix with operator: -
[DEBUG] Updated left expression: (x - 5)
[DEBUG] parsing infix with operator: +
[DEBUG] Updated left expression: ((x - 5) + (y * 8))
[DEBUG] parsing infix with operator: -
[DEBUG] Updated left expression: (((x - 5) + (y * 8)) - (6 * y))

Return Statement:
  └── Infix Expression:
        └── Infix Expression:
              └── Infix Expression:
                    └── x - 5
              └── + (y * 8)
        └── - (6 * y)


RAW OUTPUT
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
