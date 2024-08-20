# `lisp-in-rs-macros`


A simple, lexically scoped Lisp interpreter that operates fully in Rust's declarative macros. This means that `lisp!(CAR (CONS (QUOTE A) (QUOTE (B))))` expands to the string "A" and that all this computation happens at compile time by rustc expanding macros. There's no hidden proc macros, no actual rust code walking an AST, just Rust macros enjoying being turing complete.
 You can imagine that the lisp is a purely functional variant, except for the DISPLAY form which prints a value.


## Why

It's a lisp interpreter written fully in Rust's macros, I think that's pretty cool


## Example
```rust
let output = lisp!(CAR (LIST (QUOTE A) (QUOTE B) (QUOTE C)));
assert_eq!(output, "A");

lisp!(PROGN
(DEFINE message (LAMBDA () (QUOTE "hello there")))
(DISPLAY (message))
(DEFINE NOT (LAMBDA (X) (COND (X NIL) (TRUE TRUE))) )
(DISPLAY (NOT NIL))
); // will print "hello there" and "TRUE"

```




## Supported forms
```DEFINE
QUOTE
LAMBDA
LET
PROGN
CAR 
CDR 
CONS
LIST
EQ
ATOM
```

Note: dotted lists are not supported, CONS assumes its latter argument is a list. Define does not handle recursive definitions.


## Metacircular evaluator
TODO


## Technical explanation

Look at EXPLANATION.md. The macro essentially simulates a SECD machine, which is a simple stack-basd abstract machine for evaulating lambda calculus terms. 


## Awesome resources
- Functional Programming: Application and Implementation by Peter Henderson
- Ager, Mads Sig, et al. "A functional correspondence between evaluators and abstract machines." Proceedings of the 5th ACM SIGPLAN international conference on Principles and practice of declaritive programming. 2003.

## TODO

- Add proper checking that functions are being used with correct arity (at the moment, functions just slurp the number of arguments they need right off the stack, which hilariously means that writing (CONS (QUOTE A)) is immediate UB)
- Add let
- Add letrec
- Add recursive defines
- Write metacircular interpreter



