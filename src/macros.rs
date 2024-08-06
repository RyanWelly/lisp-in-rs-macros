// And God spoke all these words:
// Quote of x is x
// Atom of x is true if x is an atom
// Car of a list returns the head
// Cdr of a list returns the tail
// Cons of x and y is the list (x y)

//can use lambda for most things except for quote https://news.ycombinator.com/item?id=8715655

// also ahhhhhh calling a function with the wrong number of elements is undefined behaviour in our scheme
// ((IF X CONS CAR) (QUOTE (x y))) is immediate ub if X is truthy. We do check the number of args being given straight to a primitive though
// eg (CONS x y z) will result in an error

// instances of undefined behavior:
// - assigning a special form to a variable (if, quote, etc)
// - using the wrong number of arguments to a function

//a thing to consider; how to handle dotted lists? maybe convert all lists of the form (x y z) into (x y z nil) etc. Or introduce {a b c} === (a b . c)
macro_rules! internal_lisp {

    // Evaluate primitives/special forms
    // for primitives, when you encounter the primitive CONS, the @CONS token gets put on the stack (to differentiate between CONS as data and CONS as proc).
    // we do it this way instead of just scanning for `CONS ap`` in the control so that (define f CONS) (f 'a 'b) has the intended effect.
    // maybe do something to stop people doing (quote @CONS), like use {CONS} and specialise on QUOTE to stop anyone doing (QUOTE {$t:tt})?
    // or for thematic consistency, do a similar format to closures and use [CONS]

    // QUOTE special form
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [(QUOTE $x:tt)$($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$x $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };



    // LAMBDA special form - saves closure onto stack with current env
    (stack: [$($stacks:tt)*] env: $env:tt control: [(LAMBDA ($name:ident) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [[$name, $T, $env] $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };


    // IF special form
    // (IF exp a1 a2) => exp :: (IFS a1 a2) and IFS branches based on the truthiness of the top value on stack (the evaluated exp).


    // Define special form
    // semantics: can use at top level or any level at all, (define name exp) just evalutes exp in the current env, then binds it to `name` in the current env.
    // (DEFINE name expr) => expr :: (__DEFINE name )


    // (t1 t2) => t2::t1::ap
    // figure out a nice way to extend this to more args automatically
    // TODO: expand (op arg1 arg2 ... argn) -> arg1 :: arg2 ... :: argn :: op :: ap
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $arg:tt) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ $($stack_entries)*] env: $env control: [$arg $op ap $($rest)*] dump: $dump)
    };
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $($args:tt)*) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ $($stack_entries)*] env: $env control: [$($args)* $op ap $($rest)*] dump: $dump)
    };


    // pop primitives onto the stack as their own special tokens
    // (stack: [$($stack_entries:tt)*] env: $env:tt control: [CAR $($rest:tt)*] dump: $dump:tt) => {
    //     internal_lisp!(stack: [__CAR $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    // };


    // Evaluate primitives - top of the stack
    // TODO:
    // - EQ
    // - CONS
    // - ATOM

    // TODO: maybe evalute () to NIL? Decide properly how I handle NIL vs () vs '()


    // CAR
    (stack: [__CAR ($car:tt $($cdr:tt)*) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$car $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // CDR
    (stack: [__CDR ($car:tt) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [NIL $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [__CDR ($car:tt $($cdrs:tt)* ) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($cdrs)*) $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // DISPLAY
    (stack: [__DISPLAY $val:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        {println!("{}", stringify!($val));
        internal_lisp!(stack: [$car $($stacks)*] env: $env control: [$($controls)*] dump: $dump)}
    };

    //ATOM -- WORKING ON, HAVEN'T TESTED YET
    (stack: [__ATOM $atom:ident $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    (stack: [__ATOM $not_atom:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ NIL $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    // EQ -- WORKING ON, HAVEN'T TESTED YET
    (stack: [__EQ $val1:tt $val2:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {{
        macro_rules! __internal_eq {
            ($val1 $val1) => {internal_lisp!(stack: [TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
            ($val1 $val2) => {internal_lisp!(stack: [NIL $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
        }
        __internal_eq!($val1 $val2)}
    };


    // pop closure from the top of the stack, and the closure's variable is mapped to the value.
    // (Closure :: v :: Stack, e, ap::c, d) => ([], Closure's env extended, Closure's code, (Stack, e, c)::dump)
    (stack: [[$var:ident, $T:tt, [$($key:ident : $value:tt)*]] $v:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: [$($dump:tt)*]) => {
        internal_lisp!(stack: [] env: [$var:$v $($key:value)*] control: [$T] dump: [([$($stacks)*], $env, [$($controls)*]) $($dump)*])
    };

    // done processing the current closure, pop stack/env/control of the dump and continue evaluating

    (stack:[$val:tt $($stack_entries:tt)*] env: $env:tt control: [] dump: [ ([$($prev_stack:tt)*], $prev_env:tt, $prev_control:tt) $($rest_of_dump:tt)* ]  ) => {
        internal_lisp!(stack: [$val $($prev_stack)*] env: $prev_env control: $prev_control dump: [$($rest_of_dump)*])
    }; //working on this



    // Evaluate symbol in environment
    (stack: [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$symb:ident $($rest:tt)*] dump: $dump:tt) => {
        {macro_rules! evaluate_in_env {
            $(
                ($key, $stack:tt, $env: tt, $control:tt) => {internal_lisp!(@fix stack: $val $stack env: $env control: $control dump: $dump)};
            )*
            ($symb, $stack:tt, $env: tt, $control:tt) => {error!("val not found in environment")}
        }
        evaluate_in_env!($symb, [$($stack_entries)*], [$($key : $val )*], [$($rest)*]) } // we do this cursed expansion here so we can pass the entire $($rest)* capture groups along, instead of repeating one by one.
    };
    (@fix stack: $top:tt [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$($rest:tt)*] dump: $dump:tt) => { //needed to avoid some fuckery in the first statement, fixes up the formatting.
        internal_lisp!(stack: [$top $($stack_entries)*] env: [$($key : $val)*] control: [$($rest)*] dump: $dump)
        // println!("{}", stringify!($top)); //for debugging purposes only
    };


    (stack: [$top:tt $($rest:tt)*] env: [$($envs:tt)*] control: [] dump: []  ) => {
        stringify!($top)
    }; //Termination
}

// either causes a compiler error with the error message, or evalutes program to a string of the error message
macro_rules! error {
    ($($toks:tt)*) => {println!("{}", stringify!($($toks)*))}; // program evaluates to a single print statement with the error message
    ($($toks:tt)+) => {compile_error!(concat!("Lisp execution failure: ", stringify!($($toks)+)))}; //program halts rust compilation with the error message

}

macro_rules! lisp { //call internal_lisp! with the default env
    ($($toks:tt)*) => {internal_lisp!(stack: []
        env: [CAR: __CAR
        CDR: __CDR
        ATOM: __ATOM
        DISPLAY:__DISPLAY
        EQ: __EQ]
        control: [($($toks)*)]
        dump: [] )};
}

// we store an environment as an association list ([var_name_1: val, var_name_2: other_val])
// to retrieve a value, we create an inner macro with an arm for each key
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

#[test]
fn stack_lisp_test() {
    internal_lisp!(stack: [random nonsense] env: [a: (A B)] control: [a] dump: []);
    dbg!(internal_lisp!(stack: [] env: [] control: [(CDR (QUOTE (X Y)))] dump: []));
    let hello = internal_lisp!(stack: [] env: [a: (A B)] control: [ (DISPLAY(CDR a))] dump: []); // TODO: add rejig test now that I put primitives into environment
                                                                                                 // let lask = dbg!(
                                                                                                 //     internal_lisp!(stack: [] env: [] control: [((LAMBDA (x) (CDR X)) (QUOTE (A B)))] dump: [])
                                                                                                 // );
    let hello =
        dbg!(internal_lisp!(stack: [] env: [] control: [((LAMBDA (x) x)(QUOTE y))] dump: []));
    let hello = dbg!(
        internal_lisp!(stack: [] env: [] control: [((LAMBDA (y) (CDR y))(QUOTE (x)))] dump: [])
    );
    let invalid = dbg!(
        internal_lisp!(stack: [] env: [] control: [((LAMBDA (y) (CDR x))(QUOTE (x)))] dump: [])
    );
    let top_level_test = lisp!(QUOTE X);
    lisp!(LAMBDA (x) (CDR x));
    lisp!((LAMBDA(x)x)(QUOTE X));
    let hello = dbg!(internal_lisp!(stack: [] env: [] control: [((LAMBDA(x)x)(QUOTE X))] dump: []));
    let test = dbg!(lisp!((LAMBDA (x) (x (QUOTE (A B)))) CAR)); //this leads to an error, since CAR is not found in environment. TODO: rejig so that the primitives are stored in env too
    dbg!(internal_lisp!(stack: [] env: [CAR: __CAR] control: [(CAR (QUOTE (a)))] dump: []));
    lisp!(ATOM (QUOTE X));
}
#[test]
fn primitive_tests() {
    assert_eq!(lisp!(ATOM(QUOTE X)), "TRUE");
    assert_eq!(lisp!(ATOM(QUOTE(X))), "NIL");
}

#[test]
fn multi_args() {
    assert_eq!(lisp!(EQ (QUOTE A) (QUOTE A)), "TRUE");
    assert_eq!(lisp!(EQ (QUOTE A) (QUOTE B)), "NIL");
}

#[test]
fn non_terminating() {
    // This lisp term should never terminate.
    // TODO: add to doc comment or something
    // lisp!((LAMBDA (X) (X X))(LAMBDA (X) (X X)));
}
// Desription of my lisp:

// a simple LISP with lexical scoping, implemented fully in the Rust macro system.
// Avaliable primitives: atom, if, eq, cons, car, cdr, lambda with a single argument, display
// hopefully soon: lambda with arbitary arguments, eval primitive, define primitive.
// maybe add macros?
// for proof of concept, write a meta circular interpreter (ie just steal Graham's).
// check if this is lexical https://stackoverflow.com/questions/32344615/program-to-check-if-the-scoping-is-lexical-or-dynamic, I believe it is
