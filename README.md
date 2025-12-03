# Volette

A compiler for the Volette programming language, written in Rust.

Volette is an expression-oriented, statically-typed programming language that compiles to native code using a heavily modified [Citadel](https://github.com/Isible/citadel) backend. It's designed to be simple, straightforward, with a focus on a powerful type system.

Currently only **aarch64 macOS** is supported.

Here's a simple Volette program that calculates factorial:

```rs
fn factorial(n: i32): i32 {
    let result = 1
    let i = 1
    while i <= n {
        result *= i
        i += 1
    }
    return result
}

fn fib(n: i32): i32 {
    let a = 0
    let b = 1
    let i = 0
    while i < n {
        let tmp = a + b
        a = b
        b = tmp
        i += 1
    }
    return a
}
```

### Building and Running

```bash
cargo run -- build subjects/example.vt -o subjects/example.s
as -o subjects/example.o subjects/example.s
```

Create a C wrapper to call functions:

```c
// subjects/main.c
#include <stdio.h>

extern int factorial(int n);
extern int fib(int n);

int main() {
    printf("factorial(5) = %d\n", factorial(5));
    printf("fib(10) = %d\n", fib(10));
    return 0;
}
```

Link and compile:

```bash
gcc subjects/main.c subjects/example.o -o subjects/example
```

# Language Features

Note that semicolons are optional

### Types

- **Integers**: `i32`, `i64`, `isize`, `usize` etc
- **Floats**: `f32`, `f64`
- **Booleans**: `bool` (with `true` and `false` literals)
- **Unit type**: `unit` (represents empty/void, value as `{}`)
- **Structs**: User-defined types with named fields, anonymous structs

### Variables

Variables are declared with `let` and are immutable by default:

```volette
let x: i32 = 42
let y = 3.14;  // Type inference
```

Mutable variables use `let mut` (when supported).

### Functions

Functions are declared with the `fn` keyword:

```rs
fn add(a: i32, b: i32): i32 {
    return a + b
}

// Expression-oriented: last expression is the return value
fn multiply(a: i32, b: i32): i32 {
    a * b
}
```

### Control Flow

**If expressions:**

```res
let max = if a > b { a } else { b }
```

**While loops:**

```rs
while condition {
    // loop body
    if done { break }
    if skip { continue }
}
```

### Operators

- Arithmetic: `+`, `-`, `*`, `/`, `%`
- Comparison: `==`, `!=`, `<`, `>`, `<=`, `>=`
- Logical: `&&`, `||`, `!`
- Assignment: `=`, `+=`, `-=`, `*=`, `/=`, `%=`
- Type casting: `as` (e.g., `x as f64`)

### Structs

```ts
struct Point {
    x: f32,
    y: f32
}

struct Entity {
    id: i32,
    position: {
        x: f32,
        y: f32
    }
}
```

