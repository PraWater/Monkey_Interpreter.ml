# Monkey Interpreter 🐵

Tree walking interpreter written in OCaml for [Monkey Language](https://monkeylang.org) - A simple language with C-like syntax.

Implemented by following the book [Writing an Interpreter in Go](https://interpreterbook.com) by Thorsten Ball

## Features

- Integers, booleans, strings, arrays, hash maps
- A REPL
- Arithmetic expressions
- Let statements
- First-class and higher-order functions
- Closures
- Built-in functions
- Recursion
- Single line comments
- Macros

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

### Macro - unless

```
let unless = macro(condition, consequence, alternative) {
    quote(if (!(unquote(condition))) {
        unquote(consequence);
    } else {
        unquote(alternative);
    });
};

unless(10 > 5, puts("not greater"), puts("greater"));

# Output
greater
```

### Closure - newAdder

```
let newAdder = fn(x) {
    fn(y) { x + y };
};

let addTwo = newAdder(2);
let addThree = newAdder(3);

[addTwo(2), addThree(3)];

# Output
[4, 6]
```

## TODO

- [x] Closures
- [ ] Errors like in the book
- [ ] Tests?
- [ ] Read from a file

## Resources Used

- https://interpreterbook.com
- https://ocaml.org/docs
- https://youtu.be/dycsRSOQjho?si=34wkCgtWgknZNQT5
- https://github.com/ryo-imai-bit/Writing-An-Interpreter-In-Go-In-OCaml
