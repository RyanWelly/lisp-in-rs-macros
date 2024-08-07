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

    // Evaluate special forms

    // QUOTE special form
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [(QUOTE $x:tt)$($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$x $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    // Maybe use ^ as a quote shorthand, since ' is not a valid rust token?



    // LAMBDA special form - saves closure onto stack with current env
    (stack: [$($stacks:tt)*] env: $env:tt control: [(LAMBDA ($name:ident) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [[$name, $T, $env] $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };


    // IF special form
    // (IF exp a1 a2) => exp :: (IFS a1 a2) and IFS branches based on the truthiness of the top value on stack (the evaluated exp).
    // Although, I could totally just implement `cond` at this point;
    // (COND (p1 e1) .... (pn en)) => p1 :: (IFS e1 (COND (p2 e2) ... (pn en)) )
    // (COND (p1 e)) => p1 :: (IFS e nil)


    // Define special form
    // semantics: can use at top level or any level at all, (define name exp) just evalutes exp in the current env, then binds it to `name` in the current env.
    // (DEFINE name expr) => expr :: {__DEFINE name}




    // (t1 t2) => t2::t1::ap
    // figure out a nice way to extend this to more args automatically
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $arg:tt) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ $($stack_entries)*] env: $env control: [$arg $op ap $($rest)*] dump: $dump)
    };
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $($args:tt)*) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ $($stack_entries)*] env: $env control: [$($args)* $op ap $($rest)*] dump: $dump)
    };



    // Evaluate primitives - top of the stack
    // TODO:
    // - EQ
    // - ATOM

    // TODO: maybe evalute () to NIL? Decide properly how I handle NIL vs () vs '()


    // CAR
    (stack: [__CAR ($car:tt $($cdr:tt)*) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$car $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // CDR
    (stack: [__CDR ($car:tt) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [() $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [__CDR ($car:tt $($cdrs:tt)* ) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($cdrs)*) $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // DISPLAY
    (stack: [__DISPLAY $val:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        {println!("{}", stringify!($val));
        internal_lisp!(stack: [$val $($stacks)*] env: $env control: [$($controls)*] dump: $dump)}
    };

    //ATOM -- WORKING ON, HAVEN'T TESTED YET
    (stack: [__ATOM $atom:ident $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    (stack: [__ATOM $not_atom:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ () $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    // EQ -- WORKING ON, HAVEN'T FULLY TESTED YET
    (stack: [__EQ $val1:tt $val2:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {{
        macro_rules! __internal_eq {
            ($val1 $val1) => {internal_lisp!(stack: [TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
            ($val1 $val2) => {internal_lisp!(stack: [() $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
        }
        __internal_eq!($val1 $val2)}
    };

    // CONS - working on, haven't fully tested yet. Might need to reverse order of args
    // (CONS a b) expands to a :: b :: CONS :: ap on the control, then [CONS b a ...] on the stack. Therefor:
    // __Cons nil c => (c)
    // __Cons (b c) a => (a b c)
    (stack: [__CONS  () $val:tt $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ ($val)  $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [__CONS ($($list:tt)*) $val:tt $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ ($val $($list)*)  $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    //TODO: decide on my representation of NIL; will I use the empty list here and just a binding of NIL: () to the env?
    // I've decided to use the internal representation of the empty list for nil, so that NIL evalutes to (), but that () is also not self evaluating.


    // pop closure from the top of the stack, and the closure's variable is mapped to the value.
    // (Closure :: v :: Stack, e, ap::c, d) => ([], Closure's env extended, Closure's code, (Stack, e, c)::dump)
    (stack: [[$var:ident, $T:tt, [$($key:ident : $value:tt)*]] $v:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: [$($dump:tt)*]) => {
        internal_lisp!(stack: [] env: [$var:$v $($key:value)*] control: [$T] dump: [([$($stacks)*], $env, [$($controls)*]) $($dump)*])
    };

    // done processing the current closure, pop stack/env/control of the dump and continue evaluating

    (stack:[$val:tt $($stack_entries:tt)*] env: $env:tt control: [] dump: [ ([$($prev_stack:tt)*], $prev_env:tt, $prev_control:tt) $($rest_of_dump:tt)* ]  ) => {
        internal_lisp!(stack: [$val $($prev_stack)*] env: $prev_env control: $prev_control dump: [$($rest_of_dump)*])
    };



    // Evaluate symbol in environment
    (stack: [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$symb:ident $($rest:tt)*] dump: $dump:tt) => {
        {macro_rules! evaluate_in_env {
            $(
                ($key, $stack:tt, $env: tt, $control:tt) => {internal_lisp!(@fix stack: $val $stack env: $env control: $control dump: $dump)};
            )*
            ($symb, $stack:tt, $env: tt, $control:tt) => {error!("can't find" $symb "in env")}
        }
        evaluate_in_env!($symb, [$($stack_entries)*], [$($key : $val )*], [$($rest)*]) } // we do this cursed expansion here so we can pass the entire $($rest)* capture groups along, instead of repeating one by one.
    };
    (@fix stack: $top:tt [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$($rest:tt)*] dump: $dump:tt) => { //needed to avoid some fuckery in the first statement, fixes up the formatting.
        internal_lisp!(stack: [$top $($stack_entries)*] env: [$($key : $val)*] control: [$($rest)*] dump: $dump)
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
        EQ: __EQ
        CONS: __CONS
        NIL: ()]
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
    let top_level_test = lisp!(QUOTE X);
    lisp!(LAMBDA (x) (CDR x));
    let hello = dbg!(internal_lisp!(stack: [] env: [] control: [((LAMBDA(x)x)(QUOTE X))] dump: []));
    let test = dbg!(lisp!((LAMBDA (x) (x (QUOTE (A B)))) CAR));
    dbg!(internal_lisp!(stack: [] env: [CAR: __CAR] control: [(CAR (QUOTE (a)))] dump: []));
    lisp!(ATOM (QUOTE X));
}

// https://doc.rust-lang.org/book/ch11-03-test-organization.html

#[cfg(test)]
mod tests {
    //TODO: maybe use stringify for the correct output instead of raw string literals
    #[test]
    fn primitive_tests() {
        assert_eq!(lisp!(ATOM(QUOTE X)), stringify!(TRUE));
        assert_eq!(lisp!(ATOM(QUOTE(X))), stringify!(()));
        assert_eq!(
            lisp!(ATOM
            (CONS 
            (QUOTE X) NIL)),
            stringify!(())
        );
        assert_eq!(lisp!(QUOTE X), "X");
        assert_eq!(lisp!((LAMBDA(x)x)(QUOTE gibbergabber)), "gibbergabber");
        assert_eq!(lisp!(CAR(QUOTE(X))), "X");
        assert_eq!(lisp!(CDR(QUOTE(X))), "()");
        assert_eq!(
            lisp!(CAR(CDR(QUOTE(COMPLEX(QUOTE(A)(B)))))),
            stringify!((QUOTE(A)(B)))
        );

        assert_eq!(lisp!(CDR (QUOTE (A B C))), stringify!((B C)));
        assert_eq!(lisp!(ATOM(QUOTE(QUOTE(A)(B)))), "()");

        assert_eq!(lisp!(DISPLAY(CAR(QUOTE(X)))), "X");
    }

    #[test]
    fn multi_args() {
        assert_eq!(
            lisp!(EQ 
            (QUOTE A) (QUOTE A)),
            "TRUE"
        );
        assert_eq!(lisp!(EQ (QUOTE A) (QUOTE B)), "()");
        assert_eq!(lisp!(EQ CAR CAR), "TRUE");
        assert_eq!(lisp!(EQ (LAMBDA (X) X) (LAMBDA (X) X)), "TRUE");
        assert_eq!(lisp!(EQ (LAMBDA (Y) Y) (LAMBDA (X) X)), "()");

        assert_eq!(lisp!(CONS (QUOTE A) (QUOTE ())), stringify!((A)));
        assert_eq!(lisp!(CONS (QUOTE A) (QUOTE (B C))), stringify!((A B C)));
        assert_eq!(lisp!(CONS (QUOTE A) (QUOTE (B))), "(A B)");
        assert_eq!(
            lisp!(CONS (QUOTE A) (CONS (QUOTE B) NIL)),
            stringify!((A B))
        );
    }

    #[test]
    fn conditionals() {
        // assert_eq!(lisp!(COND( NIL TRUE) (TRUE NIL)), "NIL");
    }
    #[test]
    fn non_terminating() {
        // This lisp term should never terminate.

        // lisp!((LAMBDA (X) (X X))(LAMBDA (Y) (Y Y )));
    }
}

// Desription of my lisp:

// a simple LISP with lexical scoping, implemented fully in the Rust macro system.
// Avaliable primitives: atom, cons, eq, cons, car, cdr, lambda with a single argument, display.
// NIL is a shorthand for the empty list, and the empty list also represents falsity. TRUE and every other value is truthy
// hopefully soon: lambda with arbitary arguments, eval primitive, define primitive.
// maybe add macros?
// for proof of concept, write a meta circular interpreter (ie just steal Graham's).
// check if this is lexical https://stackoverflow.com/questions/32344615/program-to-check-if-the-scoping-is-lexical-or-dynamic, I believe it is
