# volette

A compiler for the volette(language i'm designing) programming language, written in Rust.

## what is this?

volette is an expression-oriented, statically-typed systems programming language that compiles to native code using Cranelift. It's designed to be simple and straightforward. I intend it to have a powerful type system. Will probably be garbage collected

## how it works

### the lexer

The lexer takes raw source code and breaks it down into tokens. It works by scanning through characters one at a time and maintaining state about what it's currently reading.

It is a state machine:

- **State machine based**: The lexer switches between states like Normal, Number, Comment, MultilineComment, etc. depending on what it's reading
- **Lookahead support**: Uses a sliding window of current and next characters to handle multi-character tokens (like `==`, `//`, `/*`)
- **String interning**: All identifiers and strings get interned to avoid duplicate allocations
- **Pattern tables**: Uses dedicated modules for recognizing keywords, punctuation, and type literals without a bunch of if-else chains
- **Character buffer**: Maintains a VecDeque of characters being processed, which lets it backtrack when needed

The lexer outputs a `Vec<Token>`, where each token has a kind (like `IntLiteral`, `Keyword`, `Punctuation`) and a `span` (file location info).

### the parser

The parser takes the flat list of tokens from the lexer and builds an Abstract Syntax Tree (AST). It uses a generational arena to store nodes, which is efficient since the tree structure has a lot of refrences to each other.

characteristics:

- **Recursive descent**: The parser calls different methods for different language constructs
- **Operator precedence**: Binary operations respect precedence rules
- **Arena allocation**: Nodes are stored in a generational arena and referenced by Index values
- **Single-pass**: Builds the tree in one go while collecting errors
- **Error recovery**: Tries to keep parsing even after hitting errors, so you can see multiple issues at once

The parser outputs a tree of `Node` structs, where each node has a `NodeKind` (`Root`, `Expr`, `Def`) and `span` information.

## currently implemented features

### language features

- **Functions**: Define functions with typed parameters and return values
- **Variables**: Let bindings with optional type annotations
- **Primitive types**: Integers (i8, i16, i32, i64, u8, u16, u32, u64), floats (f32, f64), booleans, and special types (nil, never, unit)
- **Type inference**: The type checker can infer types when not explicitly annotated
- **Type casting**: Explicit type conversions with `as`

### expressions

- **Literals**: Integer, float, and boolean literals
- **Binary operations**: Arithmetic (+, -, \*, /, %, \*\*), comparison (==, !=, <, <=, >, >=), logical (&&, ||)
- **Unary operations**: Negation (-), logical not (!)
- **Assignment**: Variable assignment with `=`
- **Function calls**: Call functions with arguments
- **Blocks**: Block expressions with multiple statements
- **If expressions**: Conditional expressions with optional else branches
- **Loops**: Infinite loops with `loop`
- **Control flow**: `return` and `break` statements

### compiler phases

- **Lexing**: Tokenization with detailed error reporting
- **Parsing**: AST construction with span tracking
- **Analysis**: Type checking and function table generation
- **Validation**: AST validation pass
- **Code generation**: Native code generation via Cranelift

### developer features

- **Error reporting**: Detailed error messages with file locations
- **Comments**: Single-line (`//`) and multi-line (`/* */`) comments
- **Diagnostics**: Colored output for compiler phases and errors

## building and running

```bash
# build the compiler
cargo build

# compile a volette program
cargo run build main.vt

# or just run the default file (currently hardcoded to main.vt)
cargo run
```

## example program

(it looks a lot like rust)

```rust
fn w(k: i32): i32 {
    let x = let y = k; // let returns an expression
    x = 12;
    return x;
}
```

## todo

Stuff that's not implemented yet:

- Strings (lexer has a String state but it's not implemented)
- Structs (parsed but not fully implemented in codegen)
- Generics
- Pattern matching
- Standard library
- Module system
- More complete type system (references, pointers, arrays)
