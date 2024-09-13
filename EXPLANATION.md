## Overview

The internal_lisp! macro emulates a [SECD machine](https://en.wikipedia.org/wiki/SECD_machine). Lisp lists are represented by ($($elem:tt)*) in Rust macros, ie a list of token trees.
Using the SECD machine for a Lisp is a very old idea; Lispkit Lisp, originally written in the 80s, famously used a SECD machine as a compiler target. However, we are not really compiling to target the SECD machine, we're just directly interpreting lisp expressions with one.
The SECD machine has four components: 
- a stack to store intermediate results,
- an environement which stores values bound to names,
- a control stack, the top of which is the expression currently being evaluated and determines the next state of the machine.
- a dump, which saves the current state of the machine before evaluating the body of a closure. It's the "call stack" of the machine.

Rust macro's make it surprisingly easy to write state transitions as macro match arms. 

## Internals

In our SECD machine, we have the stack, the environment, the control, and the dump (which explains the name!).
Intermediate results are stored on the stack, the environment is an associative array of variable bindings, the control is a stack of lisp expressions to evaluate, and the dump holds state when we're evaluating the inner expression of a lambda.

When you write `lisp!(CONS (QUOTE A) (QUOTE (B)))` it gets expanded to  
`internal_lisp!(stack: [] env: {} control: ((ATOM (QUOTE A))) dump: []).  `


First, we evaluate the arguments to functions before we evaluate the actual function. So we rewrite this to:


`internal_lisp!(stack: [] env: {} control: ( (QUOTE A) CONS ap) dump: []).  `

Which means that now the two arguments to the CONS function are at the top of the control stack. The next expression to evaluate is `(QUOTE A)`. Now, QUOTE is a special form; this means that we do something different than our usual method of evaluating the arguments. QUOTE will place its unevalated argument straight onto the stack.

`internal_lisp!(stack: [A] env: {} control: ( ) CONS ap) dump: []).  `

Now, ATOM will evaluate to some kind of primitive function token. In my lisp I call it __ATOM:

`internal_lisp!(stack: [__ATOM A] env: {} control: ( ap) dump: []).  `
Now, we have ap (short for apply) on the top of the control stack. This means that we try and apply the function on top of the stack, to its arguments. In this case, CONS is a primitive so it's quite straightforward. The macro arm for this would look like:

```rust
    //ATOM 
    (stack: [__ATOM ($atom:ident) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    (stack: [__ATOM ($not_atom:tt) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ () $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };
```
macro_rules! tries to match rules sequentially. So if we apply ATOM to an atom (an ident) then we push TRUE onto the top of the stack, and that rule doesn't match, it will instead match on the next rule (atom applied to a token tree, ie a list) and place the empty list on top of the stack. So our final expansion would look like 

`internal_lisp!(stack: [TRUE] env: {} control: () dump: []).  `

And both the dump and control are empty, so we halt. TRUE is the evaluated output of our lisp code, so we stringify that and return it as our final expanded value. 




For evaluating lisp code that only involves simple primitives like quote, cons, cdr and friends, the control and the stack is all we need. For evaluating lambdas, we add an environment and a dump. Let's walk through how we evaluate `((LAMBDA (X) X) (QUOTE A))`, ie applying the identity function to an argument.




`internal_lisp!(stack: [] env: [] control: ( ((LAMBDA (X) X) (QUOTE A))  ) dump: []).  `




As usual, we first evaluate the arguments before we evaluate the function. I'll skip that step:


`internal_lisp!(stack: [A] env: [] control: ( (LAMBDA (X) X) ap  ) dump: []).  `


So we have the atom A on the stack, and we want to apply the identity function to it. Our first step is to evalute the lambda itself, which results in a closure which stores the code, current environment, and a variable reference:


`internal_lisp!(stack: [ [{X}, X, []] A] env: [] control: ( ap  ) dump: []).  `


Now the top of the stack is a closure. How do we apply the closure to the argument? First we save the current stack, current control, and current env to the dump, and then we replace the control stack with the inner "code" of the closure, replace the env with our closure's env, and bind the variable X to our argument A.



`internal_lisp!(stack: [] env: [X: A] control: ( X ) dump: [((), (), ())]).  `


Now we have a single atom on top of the control; so we look it up in the environment. This evaluates to A, so:


`internal_lisp!(stack: [A] env: [X: A] control: ( ) dump: [((), (), ())]).  `


Now control is empty, so we go up a level and return to the previous code by slurping it up from the dump (this is how our machine supports calling arbitary nested lambdas).


`internal_lisp!(stack: [A] env: [] control: ( ) dump: []).  `



We finish with a single value on the stack, and our control and dumps are empty, so we stop execution. We've just successfully used the identity function!

## Some macro hacks

One lisp function that is slightly tricky to implement is EQ, testing if two lisp objects are equal. In theory, we want something like
```rs
macro_rules! lisp_equality {
    ($x:tt $x:tt) => {TRUE} 
    ($x:tt $y:tt) => {FALSE}
} 
```
where the first arm matches if the two token trees are equal, and the second matches otherwise. But this is not possible in rust macros; you cannot have duplicate bindings. We get around this quite easily by generating a macro in our macro:

```rs
macro_rules! lisp_equality {
    ($x:tt $y:tt) => {
        macro_rules! inner {
            ($x $x) => {TRUE},
            ($x $y) => {FALSE},
        };
        inner!($x $y)
    };
}
```

In the actual interpreter, we use this trick to "branch" on the truth value of a value; the rule in question looks like:

```rs
 // EQ 
    (stack: [__EQ ($val1:tt $val2:tt) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {{
        macro_rules! __internal_eq {
            ($val1 $val1) => {internal_lisp!(stack: [TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
            ($val1 $val2) => {internal_lisp!(stack: [() $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
        }
        __internal_eq!($val1 $val2)}
    };

```
So if the top two values on the stack are equal, we expand to an `internal_lisp!` call where the top two operands have been consumed and replaced with a TRUE; otherwise we expand to a call where it's been replaced with an empty list (our version of false). Crucially, this internal macro trick works even with many EQ calls; Rust allows us to redefine a macro many times, 
each time we invoke the `__internal_eq!` rust uses the most recently defined macro, which is the one we just defined and we just expanded to.


The other macro hack is not needed, but is probably more "efficient"; looking up values in our enviroment. You could of course do this recursively (lisp style, think association lists) but it's nicer to use rust macros as designed:

```rs
macro_rules! env_example {
    ($arg:ident [$($var_binding:ident : $exp:tt),*]) => {
        {macro_rules! inner_test {
            $(
            ($var_binding) => {$exp};
            )*
            ($not_found:ident) => {error!("couldn't find value in env ")}; 
        }
        inner_test!($arg)}
    }
}
env_example!(test [x: 5, y: 7, test: "hello"]) //evaluates to "hello"
```
We use another trick, generating yet another inner macro. Just like the previous trick with EQ, in the actual implmentation we use this trick to branch.
In practise, this does lead to a explosion in the size of the generated, since the corpses of many generated macros litter the final result.



## Metacircular evaluation and Recursion

Currently, the lisp implemented seems to be missing a key element; recursion. We can get around this by using the Y combinator, and implement a lisp interpreter in our lisp, like the one listed in README.md. But this is brutally inefficient; everytime our lisp creates a closure, it copies the entire environment into that closure. This quickly becomes too much to handle for rustc, as for some reason rustc isn't optimised for declarative macros generating millions of tokens and immediately passing it to a macro.

