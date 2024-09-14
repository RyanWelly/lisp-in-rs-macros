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
// "DISPLAY" forms first evaluate their arguments, then expand to a println!("{}", stringify!(evaled_argument))

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
This results in "(A B C D)". The append function does not mention `append` in its body, yet we can call it recursively. Wonderful!


## Notes for use
The lisp! macro only evaluates a single expression; if you want to evaluate multiple expressions, use `(PROGN expr1 expr2 expr3)`. This evaluates all the expressions, and returns the value of the last expression. The DISPLAY form evaluates a single expression, then generates a `println!("{}", stringify!(...))` statement which prints the stringified version of the tokens. The empty list is not self evaluating, you can use `NIL` or `(QUOTE ())` to obtain an empty list value. The empty list is the sole "falsy" object. 
Dotted lists aren't supported, cons assumes its last argument is a list. The define form can be used anywhere and evaluates to the empty list, but does not support recursion. TRUE is the only self evaluating atom (that isn't a function).


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


## Metacircular interpreter

Here is a lisp interpreter written in my lisp:

```rust
lisp!(PROGN
            // Y "combinator" for two arguments
        (DEFINE Y2 
                        (LAMBDA (h)
                            ((LAMBDA (x) (h (LAMBDA (a b) ((x x) a b))))
                                (LAMBDA (x) (h (LAMBDA (a b) ((x x) a b)))))))
        
        (DEFINE CADR (LAMBDA (X) (CAR (CDR X))))
        (DEFINE CAAR (LAMBDA (X) (CAR (CAR X))))
        (DEFINE CADAR (LAMBDA (X) (CAR (CDR (CAR X)))))
        (DEFINE CADDR (LAMBDA (X) (CAR (CDR (CDR X)))))
        (DEFINE CADDAR (LAMBDA (X) (CAR (CDR (CDR (CAR X))))))
        (DEFINE CAADAR (LAMBDA (X) (CAR (CAR (CDR (CAR X))))))

        (DEFINE ASSOC (Y2 (LAMBDA (ASSOC) (LAMBDA (X ENV) 
                        (IF (EQ (CAAR ENV) X) (CADAR ENV) (ASSOC X (CDR ENV)))
                    )))
                )


            
        (DEFINE eval (Y2 (LAMBDA (EVAL) (LAMBDA (E A) 
                (COND
                    ((ATOM E) (ASSOC E A))
                    ((ATOM (CAR E)) 
                        (COND 
                            ((EQ (CAR E) (QUOTE quote)) (CADR E))
                            ((EQ (CAR E) (QUOTE atom)) (ATOM (EVAL (CADR E) A)))
                            ((EQ (CAR E) (QUOTE car)) (CAR (EVAL (CADR E) A)))
                            ((EQ (CAR E) (QUOTE cdr)) (CDR (EVAL (CADR E) A)))
                            ((EQ (CAR E) (QUOTE equal)) (EQ (EVAL (CADR E) A) (EVAL (CADDR E) A)))
                            ((EQ (CAR E) (QUOTE cons)) (CONS (EVAL (CADR E) A) (EVAL (CADDR E) A)))
                            (TRUE (EVAL (CONS (ASSOC (CAR E) A) (CDR E)) A)) 
                        )
                    )
                    ((EQ (CAAR E) (QUOTE lambda)) (EVAL (CADDAR E) (CONS (LIST (CAADAR E) (EVAL (CADR E) A)) A)  )) //Evaluate the inner expression of the lambda, in the environment with the argument bound to the parameter
                
                )
            ))))

        (eval (QUOTE (quote (A))) NIL)
        // (eval (QUOTE (atom (quote A))) NIL )
        // (eval (QUOTE (cdr (cdr (quote (A B))))) NIL)
        // (eval (QUOTE (cons (quote a) (quote (a)))) NIL)
        // (eval (QUOTE ((lambda (x) (quote a)) (quote b))) NIL)
        (eval (QUOTE ((lambda (X) X) (quote a))) NIL)

        );
```
It appears to work, but trying to evaluate `((lambda (X) X) (quote a))` in the interpreter takes more than 30 seconds and generates far more than a million tokens before cargo gets sigkilled. Using the explicit y combinator for recursion isn't particularly efficient here! To fix this, I should add an explicit recursion primitive. If you want a wonderful walktrhough of how to write something like a metacircular evaluator, Paul Graham's "Roots of Lisp" is awesome.



## Technical explanation

Look at EXPLANATION.md. The macro essentially simulates a SECD machine, which is a simple stack-basd abstract machine for evaulating lambda calculus terms. 


## Awesome resources
- Functional Programming: Application and Implementation by Peter Henderson
- Ager, Mads Sig, et al. "A functional correspondence between evaluators and abstract machines." Proceedings of the 5th ACM SIGPLAN international conference on Principles and practice of declaritive programming. 2003.
- The Implementation of Functional Programming Languages by Simon Peyton Jones
- Anything Matt Might has ever written about lisp on his blog (https://matt.might.net)

## TODO

- Add letrec
- Add recursive defines



