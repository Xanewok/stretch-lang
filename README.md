# Stretch

Every self-respecting hobby/student programming language should have a name -
this one is called **Stretch**. File extensions for programs in this language
are *.str*.

It is very much inspired (at least syntax-wise) by the [Rust](http://rust-lang)
programming language.

## Motivation

This project started as a university assignment during
[Programming Languages and Paradigms](https://usosweb.mimuw.edu.pl/kontroler.php?_action=katalog2%2Fprzedmioty%2FpokazPrzedmiot&kod=1000-216bJPP&lang=en)
course at University of Warsaw.

There, students are tasked with writing their own interpreter of a programming
language, of their own design, meeting certain criteria. This is an attempt at
statically-typed expression-based imperative programming language, heavily
inspired by Rust.

## Expressions

Despite the language being inherently imperative, one of the main
characteristics will be an attempt to treat everything as an expression
(although it won't be Lisp!... I think). For example, `if true { 1 } else { 0 }`
is an expression with type `int` which returns value `1`.

In general, blocks always return a certain value. The value returned will be the
last expression inside a given block, e.g. `{ [Stmt]; Exp }` will return value
from evaluating the `Exp` expression. If no expression is present, a block
implicitly returns a unit value `()`, same with expressions such as `if .. { ..
}` (without else). A unit value `()` is analogous to the C `void` type.

Additionally, since control flow constructs are expressions, expressions with
side-effects (e.g. `{ a = a + 2 }` are obviously supported.

## First-class functions
Furthermore, the language supports functions as typed values. An example
function definition looks like this:
```rust
fn my_function(arg1: bool, arg2: int) -> int {
	42
}
```
which is a function accepting 2 arguments and returning an `int` value. Here,
`my_function` is of type `Fn(bool, int) -> int`. If the type is not specified,
it implicitly returns a unit value `()`.

Since the functions are first-class citizens, the language supports anonymous
functions. An example syntax is given below:
```rust
let anon: Fn(bool, int) -> int = |arg1: bool, arg2: int| { 42 };
```
Here, the value can be called with `anon()` as an ordinary function and has the
same type as the above `my_function` functiom.

## Static type system
The language will be interpreted, however it is meant to be statically typed -
this means that the type-checking phase will always be resolvable before
actually interpreting and running the program.

## Linear type system, or what I'd like to toy with next
The language above should meet requirements given at the university course,
however it wouldn't be a real inspiration after Rust, at least not only
syntax-wise, if it didn't support at least a simple linear type system.
Currently the language grammar allows for a special `drop Ident;` statement used
to consume values, however it is not used at the moment.

## Examples
Small syntax examples are inside the `examples/` directory.
