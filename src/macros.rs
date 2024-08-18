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
#[macro_export]
macro_rules! internal_lisp {

    // Evaluate special forms

    // QUOTE special form
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [(QUOTE $x:tt)$($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$x $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    // Maybe use ^ as a quote shorthand, since ' is not a valid rust token?



    // LAMBDA special form - saves closure onto stack with current env
    (stack: [$($stacks:tt)*] env: $env:tt control: [(LAMBDA ($name:ident) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [[{$name}, $T, $env] $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };
    //TODO: make multiple args work for lambdas
    // probably just automatically curry, ie make (lambda (x y z) T) => (lambda (x) (lambda (y) (lambda (z) T)))
    // actually, that kinda sucks

    // IDEA: if you have eg
    // macro_rules! test {
    //     ($($repeat:ident)* : $($other_repeat) ) => { $($repeat, $other_repeat)*}
    // }
    // it will only work if there are the same number of repeats in both matchings. Use this somehow to implement?
    // probably have to construct a list of all the args to be fed to the closure
    // which probably means that I refactor so every opcode operates on lists instead of discrete values on the stack
    // (stack: [$($stacks:tt)*] env: $env:tt control: [(LAMBDA ($(vars:ident)*) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
    //     internal_lisp!(stack: [[{$($vars)*}, $T, $env] $($stacks)*] env: $env control: [$($controls)*] dump: $dump)

    // };

    (stack: [$($stacks:tt)*] env: $env:tt control: [(LOMBDA ($($names:ident)*) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [[{$($names)*}, $T, $env] $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };




    // COND special form
    // (COND (p1 e1) .... (pn en)) => p1 :: (IFS e1 (COND (p2 e2) ... (pn en)) )
    // (COND (p1 e)) => p1 :: (IFS e nil)
    // TODO - working on cond



    // TODO - test this works
    (stack: $stack:tt env: $env:tt control: [(COND ($p:tt $e:tt) $(($pn:tt $en:tt))+ )  $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$p {__IFS $e (COND $(($pn $en))+  ) } $($control)*] dump: $dump)
    }; //TODO - maybe have {IFS ...} instead of (IFS ...)
    (stack: $stack:tt env: $env:tt control: [(COND ($p:tt $e:tt)) $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$p {__IFS $e NIL } $($control)*] dump: $dump)
    }; // If we reach the end of the COND without a predicate matching, we return nil

    (stack: [() $($stack:tt)*] env: $env:tt control: [{__IFS $p:tt $e:tt } $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$($stack)*] env: $env control: [ $e $($control)*] dump: $dump)
    }; // NIL/false case for the __IFS

    (stack: [$val:tt $($stack:tt)*] env: $env:tt control: [{__IFS $p:tt $e:tt } $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$($stack)*] env: $env control: [ $p $($control)*] dump: $dump)
    }; // truthy case for the __IFS - everything but nil/empty list is true










    // Define special form
    // semantics: can use at top level or any level at all, (define name exp) just evalutes exp in the current env, then binds it to `name` in the current env.
    // Returns nil.
    // (DEFINE name expr) => expr :: {__DEFINE name}
    // TODO - enable recursive defines somehow - maybe specialise when there is a closure on tbe stack and {__DEFINE: $name} on the control



    (stack: $stack:tt env: $env:tt control: [ (DEFINE $name:ident $exp:tt) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$exp {__DEFINE:$name} $($rest)*] dump: $dump)
    };


    // TODO - wrap defined lambdas in the z combinator to get them working recursively 

    (stack: [$value:tt $($stack:tt)*] env: [$($key:ident : $val:tt)*] control: [{__DEFINE:$name:ident} $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [() $($stack)*] env: [$name: $value $($key : $val)*] control: [$($rest)*] dump: $dump )
    };

    //PROGN special form
    // TODO
    // basically a hack to be able to write multiple lisp expressions
    // (PROGN e1 e2 .. eN) evalutes to the value of eN, but evaluates each expression in order


    (stack: $stack:tt env: $env:tt control: [(PROGN $($forms:tt)*) $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [(LIST $($forms)*) {__LAST} $($control)* ] dump: $dump)
    };

    (stack: [($last:tt) $($stack:tt)*] env: $env:tt control: [{__LAST} $($control:tt)*] dump: $dump:tt ) => {
        internal_lisp!(stack: [$last $($stack)*] env: $env control: [$($control)*] dump: $dump)
    };
    (stack: [($first:tt $($rest:tt)+) $($stack:tt)* ] env: $env:tt control: [{__LAST} $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($rest)+) $($stack)*] env: $env control: [{__LAST} $($control)*] dump: $dump)
    };



    // List - not a special form, but needs special handling

    (stack: $stack:tt env: $env:tt control: [(LIST $head:tt $($args:tt)*)  $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [(CONS $head (LIST $($args)*)) $($control)*] dump: $dump)
    };

    (stack: $stack:tt env: $env:tt control: [(LIST)  $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [NIL $($control)*] dump: $dump)
    };


    // (t1 t2) => t2::t1::ap
    // figure out a nice way to extend this to more args automatically
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $arg:tt) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ $($stack_entries)*] env: $env control: [$arg $op ap $($rest)*] dump: $dump)
    };
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $($args:tt)*) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ $($stack_entries)*] env: $env control: [$($args)* $op ap $($rest)*] dump: $dump)
    };



    // Evaluate primitives - top of the stack


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

    // EQ 
    (stack: [__EQ $val1:tt $val2:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {{
        macro_rules! __internal_eq {
            ($val1 $val1) => {internal_lisp!(stack: [TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
            ($val1 $val2) => {internal_lisp!(stack: [() $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
        }
        __internal_eq!($val1 $val2)}
    };

    // CONS 
    (stack: [__CONS  () $val:tt $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ ($val)  $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [__CONS ($($list:tt)*) $val:tt $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ ($val $($list)*)  $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };


    // pop closure from the top of the stack, and the closure's variable is mapped to the value.
    // (Closure :: v :: Stack, e, ap::c, d) => ([], Closure's env extended, Closure's code, (Stack, e, c)::dump)
    (stack: [[{$var:ident}, $T:tt, [$($key:ident : $value:tt)*]] $v:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: [$($dump:tt)*]) => {
        internal_lisp!(stack: [] env: [$var:$v $($key:$value)*] control: [$T] dump: [([$($stacks)*], $env, [$($controls)*]) $($dump)*])
    };

    // order of args stored in the closure is in the wrong order compared to the order of operands on the stack, so first we reverse.
    // TODO - find a better way of doing this than recursively reversing and then zipping
    (stack: [[{$($vars:ident)*}, $T:tt, [$($key:ident : $value:tt)*]] $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: [$($dump:tt)*]) => {
        internal_lisp!(@rev ($($vars)*) () stack: [$($stacks)*] env: [ $($key:$value)*] control: [$T] dump: [([$($stacks)*], $env, [$($controls)*]) $($dump)*])
    };

    //problem: consume the correct number of elements from the stack. Annoying as it is, probably just gonna do it step by step recursively.
    // so reverse the list of idents to match the order on the stack, then pop one by one
    // TODO: refactor system so every opcode takes a list, then this might be easier
    (@rev ($var:ident $($vars:ident)*) ($($reversed:ident)*) stack: $stack:tt env: $env:tt control: $control:tt dump: $dump:tt) => {
        internal_lisp!(@rev ($($vars)*) ($var $($reversed)*) stack: $stack env: $env control: $control dump: $dump)
    };
    (@rev () ($($vars:ident)*) stack: $stack:tt env: $env:tt control: $control:tt dump: $dump:tt) => {
        internal_lisp!(@load ($($vars)*) stack: $stack env: $env control: $control dump: $dump)
    };
    (@load  ($var:ident $($vars:ident)*) stack: [$top:tt $($stacks:tt)*] env: [$($key:ident : $value:tt)*] control: $control:tt dump: $dump:tt  ) => {
        internal_lisp!(@load ($($vars)*) stack: [$($stacks)*] env: [$var: $top $($key : $value)* ] control: $control dump: $dump)
    };
    (@load  () stack: $stack:tt env: $env:tt control: $control:tt dump: $dump:tt  ) => {
        internal_lisp!(stack: $stack env: $env control: $control dump: $dump)
    };
    

    // (stack: [[{$($var:ident)*}, $T:tt, [$($key:ident : $value:tt)*]] $v:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: [$($dump:tt)*]) => {
    //     internal_lisp!(stack: [] env: [$var:$v $($key:$value)*] control: [$T] dump: [([$($stacks)*], $env, [$($controls)*]) $($dump)*])
    // }; // TODO: working on this bit to support multi arg lambda

    // done processing the current closure, pop stack/env/control off the dump and continue evaluating

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
        NIL: ()
        TRUE: TRUE]
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

        assert_eq!(lisp!((LAMBDA (Y) (CAR Y) ) (LIST NIL NIL NIL)), stringify!(()));
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
    fn list() {
        assert_eq!(lisp!(LIST (QUOTE A) (QUOTE B)), stringify!((A B)));
        assert_eq!(
            lisp!(LIST (QUOTE A) (QUOTE B)),
            lisp!(CONS (QUOTE A) (CONS (QUOTE B) NIL))
        );
        assert_eq!(
            lisp!(LIST (QUOTE A) (LIST (QUOTE (A B C)))),
            stringify!((A ((A B C))))
        );
        assert_eq!(lisp!(CAR (LIST (QUOTE A) (QUOTE B))), stringify!(A));

    }

    #[test]
    fn progn() {
        assert_eq!(lisp!(PROGN (QUOTE A) (QUOTE B)), stringify!(B));
        let test = lisp!(PROGN (DEFINE A (QUOTE B)) (CONS A NIL)); // TODO: figure out why the define doesn't work, and A does not have a binding in the env.
        dbg!(test);
    }

    #[test]
    fn conditionals() {
        // assert_eq!(
        //     lisp!(COND(
        //         (EQ (QUOTE A ) (QUOTE B))   (QUOTE B)
        //         (EQ (QUOTE A ) (QUOTE A))   (QUOTE A)
        //     )

        //     ),
        //     "B"
        // );
        assert_eq!(
            lisp!(COND
                ((EQ (QUOTE A ) (QUOTE B))  (QUOTE B)) 
                ((EQ (QUOTE A ) (QUOTE A))  (QUOTE A))),
            "A"
        );
        assert_eq!(
            lisp!(COND(
                (EQ (QUOTE A ) (QUOTE A))   (QUOTE B))),
            "B"
        );
        assert_eq!(
            lisp!(COND ((EQ (QUOTE A) (QUOTE B)) (QUOTE FIRST))
                                ((ATOM (QUOTE A)) TRUE)),
            "TRUE"
        );

        // let test = lisp!(COND(EQ)); //incorrect forms will usually just fail with some kind of error.
    }

    #[test]
    fn define_tests() {
        assert_eq!(lisp!(DEFINE A NIL), stringify!(()));
    }
    #[test]
    fn non_terminating() {
        // This lisp term should never terminate.

        // lisp!((LAMBDA (X) (X X))(LAMBDA (Y) (Y Y )));
    }

    #[test]
    fn lombda() {
        let test = lisp!((LOMBDA (X Y) (CONS X Y)) (QUOTE A) NIL);
        lisp!(PROGN 
            (DEFINE list3 (LOMBDA (X Y Z) (LIST X Y Z)))
            (DISPLAY (list3 NIL NIL NIL))
            (DEFINE print_a (LOMBDA () (DISPLAY (QUOTE A))))
            (print_a)
        );
        dbg!(test);
    }
}

#[cfg(test)]
mod metacircular {
    #[test]
    fn metacircular() {
        let test = lisp!(PROGN
            (DEFINE tost (LAMBDA (X) (DISPLAY (QUOTE t))))
            (DISPLAY (tost NIL) )

            (DEFINE NOT (LAMBDA (X) (COND (X NIL) (TRUE TRUE))) )
            (NOT NIL)

        ); 
        dbg!(test);


        let recursive_define = lisp!(PROGN
        (DEFINE IS_NULL (LAMBDA (X) (EQ X NIL)))
        (DEFINE eternity (LAMBDA (Y) (eternity Y)))
        (eternity NIL)
        );
    }

    // TODO: enable lambdas to have multiple args, by changing it so that (f a b c) evaluates (LIST a b c), puts that on stack, and then evalutes f. ie so that
    // primitives work on a list of a certain length, instead of a certain number of elements on the stack. Then make CONS special, ie that
    // (CONS a b) basically gets treated like a special form and rewritten directly, instead of just looked up in env like all the other primitives. 
    // This gets around the recursive problem in every LIST call using CONS and therefore another LIST call. Dirty hack, but still. 
}

// Desription of my lisp:

// a simple LISP with lexical scoping, implemented fully in the Rust macro system.
// Avaliable primitives: atom, cons, eq, cons, car, cdr, lambda with a single argument, display.
// NIL is a shorthand for the empty list, and the empty list also represents falsity. TRUE and every other value is truthy.
// The define form is a bit weird in my lisp; (define name expr) evaluates expr and then binds it to name in the current env, so there's no restriction to
// only using it at the top level like there is in Scheme. It does not allow recursive definitions, so use the Y combinator or similar for recursion.

// hopefully soon: lambda with arbitary arguments, eval primitive, define primitive.
// maybe add macros?
// for proof of concept, write a meta circular interpreter (ie just steal Graham's).
// check if this is lexical https://stackoverflow.com/questions/32344615/program-to-check-if-the-scoping-is-lexical-or-dynamic, I believe it is
