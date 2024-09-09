# `lisp-in-rs-macros`


A simple, lexically scoped Lisp interpreter that operates fully in Rust's declarative macros. The `lisp!` macro expands to the lisp value computed by the code, and then stringifies it. This means that `lisp!(CAR (CONS (QUOTE A) (QUOTE (B))))` expands to the string "A" and that all this computation happens at compile time by rustc expanding macros. There's no hidden proc macros, no actual rust code walking an AST, just Rust macros enjoying being turing complete. 
The lisp implemented is close to being purely functional in the programming sense, except for the DISPLAY form which expands to a `println!("{}", stringify(...)` call.


## Why

It's a lisp interpreter written fully in Rust's macros, I think that's pretty cool. It's also less than 300 lines, which is neat.


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

As another fun example, here is a quine:

```rust
lisp!
       ((LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s)))
       (QUOTE (LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s)))));
```
This code expands to:
```rust
stringify!(((LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s)))
       (QUOTE (LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s))))));
```
In other words, the code evaluates to itself. Isn't that wonderful?





## Recursion

This lisp does not support any explicit form of recursion. When we write `(define name exp)` the name binding is not visible in `exp`; all define does is evaluate the exp in the current environment and then bind that value to name. We don't have recursion, but secretly we had it all along. All we need is lambda.

Let's take the example of writing a function `append` which takes two lists, and returns the concatenation of the lists. If we try and define this naively in our lisp:


```rust
(DEFINE append 
    (LAMBDA (self X Y) 
        (COND 
            ((EQ X NIL) Y) 
            (TRUE (CONS (CAR X) (self self (CDR X) Y))) 
        )))




## Supported forms
```rust
DEFINE
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

Note: dotted lists are not supported, CONS assumes its latter argument is a list. Define does not handle recursive definitions, it's more like Schemes internal definitions than a true lispy define.


## Metacircular evaluator
TODO


## Technical explanation

Look at EXPLANATION.md. The macro essentially simulates a SECD machine, which is a simple stack-basd abstract machine for evaulating lambda calculus terms. 


## Awesome resources
- Functional Programming: Application and Implementation by Peter Henderson
- Ager, Mads Sig, et al. "A functional correspondence between evaluators and abstract machines." Proceedings of the 5th ACM SIGPLAN international conference on Principles and practice of declaritive programming. 2003.
- Anything Matt Might has ever written about lisp on his blog (https://matt.might.net)

## TODO

- Add proper error message for when primitives are called with the wrong arguments?
- Add letrec
- Add recursive defines
- Write metacircular interpreter



