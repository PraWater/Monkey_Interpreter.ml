# Monkey Interpreter 🐵

Tree walking interpreter written in OCaml for [Monkey Language](https://monkeylang.org) - A simple language with C-like syntax.

Implemented by following the book [Writing an Interpreter in Go](https://interpreterbook.com) by Thorsten Ball

## Features

- Integers, booleans
- A REPL
- Arithmetic expressions
- Let statements
- First-class and higher-order functions
- Recursion

## REPL

### Requirements

- OCaml
- opam
- dune

### Run

```
dune exec bin/main.exe
```

## TODO

- [ ] Errors like in the book
- [ ] Tests?
- [ ] Closures

## Resources Used

- https://ocaml.org/docs
- https://youtu.be/dycsRSOQjho?si=34wkCgtWgknZNQT5
- https://github.com/ryo-imai-bit/Writing-An-Interpreter-In-Go-In-OCaml
