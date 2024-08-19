# `lisp-in-macros`


A simple Lisp interpreter that operates fully in Rust's declarative macros. This means that `lisp!(CAR (CONS (QUOTE A) (QUOTE (B))))` expands to the string "A", all computation happens at compile time by rustc expanding macros. You can imagine that the lisp is a purely functional variant, except for the DISPLAY form which prints a value.


## Example
```rust
let output = lisp!(CAR (LIST (QUOTE A) (QUOTE B) (QUOTE C)));
assert_eq!(output, "A");

lisp!(PROGN
(DEFINE message (QUOTE "hello there"))
(DISPLAY message)
(DEFINE NOT (LAMBDA (X) (COND (X NIL) (TRUE TRUE))) )
(DISPLAY (NOT NIL))
); // will print "hello there" and "TRUE"

```
## Supported forms
DEFINE
QUOTE
LAMBDA
PROGN
CAR 
CDR 
CONS
LIST
EQ
ATOM

Note: dotted lists are not supported, CONS assumes its latter argument is a list


## Metacircular evaluator
It wouldn't be a proper lisp without me writing a metacircular evaluator for it: 
TODO



## Technical explanation

Look at EXPLANATION.md. The macro essentially simulates a SECD machine, which is a simple stack-basd abstract machine for evaulating lambda calculus terms. 


## Awesome resources
- Functional Programming: Application and Implementation by Peter Henderson
- Ager, Mads Sig, et al. "A functional correspondence between evaluators and abstract machines." Proceedings of the 5th ACM SIGPLAN international conference on Principles and practice of declaritive programming. 2003.

## TODO

- Add proper checking that functions are being used with correct arity
- Write metacircular interpreter



