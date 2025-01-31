# Monkey Interpreter 🐵

Tree walking interpreter written in OCaml for [Monkey Language](https://monkeylang.org) - A simple language with C-like syntax.

Implemented by following the book [Writing an Interpreter in Go](https://interpreterbook.com) by Thorsten Ball

## Features

- Integers, booleans, strings, arrays, hash maps
- A REPL
- Arithmetic expressions
- Let statements
- First-class and higher-order functions
- Built-in functions
- Recursion
- Single line comments

## REPL

### Requirements

- OCaml
- opam
- dune

### Run

```
dune exec bin/main.exe
```

## Code Examples

### Map function

```
let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };
    iter(arr, []);
};

let arr = [1, 2, 3, 4];
let double = fn(x) { x * 2 };
map(arr, double);

# Output
[2, 4, 6, 8]
```

### Reduce function

```
let reduce = fn(arr, initial, f) {
    let iter = fn(arr, result) {
        if (len(arr) == 0) {
            result
        } else {
            iter(rest(arr), f(result, first(arr)));
        }
    };
    iter(arr, initial);
};

# Can be used for making sum function
let sum = fn(arr) {
    reduce(arr, 0, fn(initial, el) { initial + el });
};

sum([1, 2, 3, 4, 5]);

# Output
15
```

## TODO

- [ ] Errors like in the book
- [ ] Tests?
- [ ] Closures
- [ ] Read from a file

## Resources Used

- https://interpreterbook.com
- https://ocaml.org/docs
- https://youtu.be/dycsRSOQjho?si=34wkCgtWgknZNQT5
- https://github.com/ryo-imai-bit/Writing-An-Interpreter-In-Go-In-OCaml
