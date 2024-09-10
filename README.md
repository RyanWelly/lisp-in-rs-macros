# `lisp-in-rs-macros`


A simple, lexically scoped Lisp interpreter that operates fully in Rust's declarative macros. The `lisp!` macro expands to the lisp value computed by the code, and then stringifies it. This means that `lisp!(CAR (CONS (QUOTE A) (QUOTE (B))))` expands to the string "A" and that all this computation happens at compile time by rustc expanding macros. 

## Why

It's a lisp interpreter written fully in Rust's macros, I think that's pretty cool. It's also less than 250 lines, which is neat. 


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

This lisp does not currently support any explicit form of recursion. Luckily, explicit recursion is not needed, all we need is lambda.

You can write a simple function that appends two lists by using self application:


```rust
lisp!(PROGN
(DEFINE append 
    (LAMBDA (self X Y) 
        (COND 
            ((EQ X NIL) Y) 
            (TRUE (CONS (CAR X) (self self (CDR X) Y))) 
        )))
(append append (QUOTE (A B)) (QUOTE (C D)))

)
```
This results in "(A B C D)". Wonderful!


## Notes for use
The lisp! macro only evaluates a single expression; if you want to evaluate multiple expressions, use `(PROGN expr1 expr2 expr3)`. This evaluates all the expressions, and returns the value of the last expression. The DISPLAY form evaluates a single expression, then expands to `println!(stringify!(...))`. The empty list is not self evaluating, you can use NIL or (QUOTE ()) to obtain an empty list value. The empty list is the sole "falsy" object. 
Dotted lists aren't supported, cons assumes its last argument is a list.


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
APPLY
```

Note: dotted lists are not supported, CONS assumes its latter argument is a list. Define does not handle recursive definitions, it's more like internal definitions in Scheme than a true lispy define.


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



