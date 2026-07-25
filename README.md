# [Unnameable] (WIP)

> **Status:** Experimental  
> Syntax, APIs, and compiler internals are unstable and subject to breaking changes without notice.

A experimental programming language compiler written in Rust.

---

## Features Implemented So Far

- [x] **Lexer & Parser** (AST generation)
- [x] Basic Diagnostics 
- [x] HIR lowering
- [x] **Name and Scope resolution, Type Checker, Control flow checker, and other basic checks**
- [x] **MIR (Mid-level Intermediate Representation) Lowering**
- [ ] Code Generation / Backend Execution (In Progress)

---

## Quickstart

### Prerequisites
- [Rust](https://www.rust-lang.org/) (edition 2021)

### Building & Running
Clone the repository and build using Cargo:

```bash
git clone [https://github.com/BananaChristian/Unnameable.git](https://github.com/BananaChristian/Unnameable.git)
cd your-repo
cargo run -- path/to/sample.unn
```
```

### Licenses
- Apache License, Version 2.0 (LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license (LICENSE-MIT or http://opensource.org/licenses/MIT)


