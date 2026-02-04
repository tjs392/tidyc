# CVM - C Virtual Machine with Garbage Collection

A C compiler targeting a custom VM with automatic garbage collection, written in Rust.

**Status:** Early development. Lexer, AST, and parser are complete. Semantic analysis, code generation, VM, and GC are in progress.

## Roadmap

#### Done
- Lexer
- AST design
- Parser

#### TODO
- Semantic analysis (type checking, symbol tables)
- IR generation
- VM implementation
- Garbage collector
- Standard library subset

## Language Specification

CVM implements "Almost C" - a practical subset of C99 with some extensions.

### Supported Syntax

**Types:**
- Primitives: int, char, short, long, float, double, void
- Signed/unsigned modifiers
- Pointers, arrays (fixed-size), function pointers
- Structs, unions, enums
- typedef
- const qualifier

**Control Flow:**
- if/else, while, do-while, for
- switch/case/default
- break, continue, goto, labels
- return

**Operators:**
- Arithmetic: +, -, *, /, %, ++, --
- Comparison: ==, !=, <, >, <=, >=
- Logical: &&, ||, !
- Bitwise: &, |, ^, ~, <<, >>
- Assignment: =, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=
- Ternary: ? :
- Member access: ., ->
- Pointer: *, &
- Array indexing: []
- Cast, sizeof

**Declarations:**
- Functions (with and without definitions)
- Global and local variables
- Storage classes: static, extern
- Struct, union, enum definitions
- typedef

**Extensions:**
- Native bool type (true/false keywords) - C23 feature backported

### Not Supported

- Variable-length arrays (VLAs)
- Compound literals
- Overly complex declarators (limit 3 levels of nesting)
- volatile, restrict, inline keywords
- Full preprocessor (no #define macros, #ifdef, etc.)
- Complex, Imaginary types
- Designated initializers
- Flexible array members