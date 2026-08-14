# Unnameable

> **Status: Experimental**  
> Syntax, APIs, and compiler internals are unstable and subject to breaking changes without notice.

`Unnameable` is an experimental programming language compiler written in Rust. It features a complete multi-stage pipeline from source text down to middle intermediate representations, with code generation currently under active development.

---

## Syntax at a Glance

Here is what a basic program with comments looks like in Unnameable right now:

```text
## 
   This is a multiline comment
   for the Unnameable language.
##

func main(): u32 {
    # This is a single line comment
    var u32 MAX := 100u32;
    return 0u32;
}
```

---

## Compiler Pipeline Status

- [x] **Lexer & Parser**: Converts source code into an Abstract Syntax Tree (AST).
- [x] **Diagnostics**: Basic compiler error reporting.
- [x] **HIR Lowering**: Transforms AST into High-level Intermediate Representation.
- [x] **Semantic Analysis**: Scope resolution, type checking, and control flow validation.
- [ ] **MIR Lowering**: Mid-level Intermediate Representation generation (In progress).
- [ ] **Code Generation**: LLVM backend compilation *(In Progress)*.

---

## Quickstart

### Prerequisites
Make sure you have the [Rust toolchain](https://rust-lang.org) installed (supports **Rust 2021 edition**).

### Building & Running
Clone the repository and run a sample file using Cargo:

```bash
# Clone the repository
git clone https://github.com/BananaChristian/Unnameable
cd Unnameable

# Run the compiler on a source file
cargo run -- path/to/sample.unn
```

---

## License

This project is dual-licensed under:
* **Apache License, Version 2.0** ([LICENSE-APACHE](LICENSE-APACHE) or http://apache.org)
* **MIT License** ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org)

