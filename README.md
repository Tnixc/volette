# Volette

A compiler for the Volette programming language, written in Rust.

Volette is an expression-oriented, statically-typed systems programming language that compiles to native code using Citadel. It's designed to be simple and straightforward. I intend it to have a powerful type system.

Only aarch64 macOS is supported

to run

```
 cargo run -- build program.vt
 as -o program.o output.asm
 gcc main.c program.o -o prog
 ./prog
```
