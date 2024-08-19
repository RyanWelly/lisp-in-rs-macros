## Overview

The internal_lisp! macro emulates a [SECD machine](https://en.wikipedia.org/wiki/SECD_machine). Lisp lists are represented by ($($elem:tt)*) in Rust macros, ie a list of token trees.
The SECD machine uses a stack to store intermediate results, and uses a dump data structure as a sort of "call stack" when evaluating lambda expressions.




## Internals

In our SECD machine, we have the stack, the environment, the control, and the dump (which explains the name!).
Intermediate results are stored on the stack, the environment is an associative array of variable bindings, the control is a stack of lisp expressions to evaluate, and the dump holds state when we're evaluating the inner expression of a lambda.


The SECD machine is wonderfully simple; what the machine does is fully dependent on what is on top of the control stack. The actual Lisp functions are mostly fluff; what we really need to do is to be able to evaluate the lambda calculus.

Think of the lambda calculus as Lisp with only atoms and lambdas that only take a single argument.

Let's consider a simple version of lisp, where all we have are atoms, single argument lambdas, and application. Atoms we can identify with Rust idents; things like `hello` and `A` would be atoms. So we can write simple expressions like `((LAMBDA (X) X) A )`. Essentially, we'll be writing an evaluator for the lambda calculus. Let's step through a SECD machine for this language, evaluating the expression ((LAMBDA (X) X) A ).



`stack: [], env: [], control: [((LAMBDA (X) X) A )], dump: []`
Firstly we start with the full expression in the control. Since this is in the form (t1 t2), t1 is considered a function that takes the argument t2. First we'll evaluate the argument, then the function, then apply them together. The next state of the machine is:


`stack: [] env: [] control: [A, (LAMBDA (X) X), ap] dump: []`
So now we have three elements on our control stack. We try evaluate the top:

Unlike in actual Lisp, we'll assume that A evaluates to itself here (In Lisp, you'd have to quote A).  So it evaluates to A, which gets put on the stack:


`stack: [A] env: [] control: [(LAMBDA (X) X) ap] dump: []`
Now we see that we have a (LAMBDA (X) T) on top of the control stack. Evaluating a function results in a closure, which we'll think of an expression, T, paired with an environment. 


`stack: [ closure[X, X, {}] A] env: [] control: [ap] dump: []`
We've pushed the closure onto the stack. It stores the expression and the env, and records what variable it uses. Now all we have is the `ap` on top of the control, which tells us we must apply a closure on the stack to its argument on the stack. How do we do this? This is where we use the dump. When we're evaluating a closure, we first save the stack, env and control onto the dump to deal with later. and we shift the closure's expression to control, and replace the current env with the closure's env.


`stack: [ closure[X, X, {}], A] env: [] control: [ap] dump: []`




TLDR: 
- If an atom is on top of the control we find its value in the enviroment
- If a lambda is on top of the control we wrap it into a closure with the current env and push the closure on top of the stack

If we call the Stack S, the Environment E, the control C, and the Dump D, and use the notation X :: C for the list formed by appending X to the front of a list C, then we can express some of the possible state transition rules as follows:

(S, E, (LAMBDA (X) T)::C, D) => ([X, T, E]::S, E, C, D) // When we encounter a lambda, we wrap it into a closure that stores the expression and the current env.

(S, E, X :: C, D) => (E[X] :: S, E, C, D) // we look up unknown variables in the enviroment

(S, E, (X Y) :: C, D) => (S, E, Y :: X :: ap :: C, D) // We split up applications, evaluating the arguments first



Once we can evaluate our variant of the lambda calculus, evaluating lisp is relatively trivial. We just need to adjust our machine so that apply works on more than just lambdas/closures, and add support for "special forms", which are lisp functions where the arguments aren't evaluated like normal. These include quote, cond, and define.



When you write `lisp!(some lisp Expr)` it gets expanded to  
`internal_lisp!(stack: [] env: {} control: (some lisp Expr) dump: []).  

The definition of internal_lisp! looks like: 
```rust
macro_rules! internal_lisp {


}
```


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
    (stack: [__EQ $val1:tt $val2:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {{
        macro_rules! __internal_eq {
            ($val1 $val1) => {internal_lisp!(stack: [TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
            ($val1 $val2) => {internal_lisp!(stack: [() $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
        }
        __internal_eq!($val1 $val2)}
    };

```
So if the top two values on the stack are equal, we expand to an `internal_lisp!` call where the top two operands have been consumed and replaced with a TRUE; otherwise we expand to a call where it's been replaced with an empty list (our version of false). Crucially, this internal macro trick works even with many EQ calls; Rust allows us to redefine a macro many times, 
each time we invoke the `__internal_eq!` rust uses the most recently defined macro, which is the one we just defined and we just expanded to.






The other macro hack is not needed, but is probably more "efficient"; looking up values in our enviroment. You could of course do this recursively (lisp style, think association lists) but it's far nicer to use rust macros as designed:

```rs
macro_rules! env_test {
    ($arg:ident [$($var_binding:ident : $exp:tt),*]) => {
        macro_rules! inner_test {
            $(
            ($var_binding) => {$exp};
            )*
            ($not_found:ident) => {error!("couldn't find value in env ")}; // maybe use concat to include some debug info
        }
        inner_test!($arg)
    }
}
```
We use another trick, generating yet another inner macro. Just like the previous trick with EQ, in the actual implmentation we use this trick to branch.