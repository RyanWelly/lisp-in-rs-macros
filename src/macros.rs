// And God spoke all these words:
// Quote of x is x
// Atom of x is true if x is an atom
// Car of a list returns the head
// Cdr of a list returns the tail
// Cons of x and y is the list (x y)

// rewrite!(QUOTE x) => x
// rewrite!()

macro_rules! eval {
    (QUOTE $rest:tt) => {
        $rest
    };
    (ATOM ) => {}; //For atom, just check if input is an ident or not.
    (CAR $rest:tt) => {};
    (CONS $x:tt $y:tt) => {
        ($x, $y)
    };
}

macro_rules! eval_callback {
    (QUOTE $rest:tt) => {
        $rest
    };
    (ATOM ) => {}; //For atom, just check if input is an ident or not.
    (CAR $rest:tt) => {};
    (CONS $x:tt $y:tt) => {
        ($x, $y)
    };
}
macro_rules! callback {
    ($callback1:ident $($args:tt)*) => {$callback1!($($args)*)};
}
macro_rules! callback2 {
    ($callback1:ident $callback2:ident $($args:tt)*) => {};
}

macro_rules! stringify_test {
    ($($args:tt)*) => {
        stringify!($($args)*)
    };
}

const test: ((&str, &str), &str) = eval!(CONS ("hello", "hel") "hell");

const tost: &str = callback!(stringify(eval(QUOTE)));
const blah: &str = stringify_test!(hello hi "hi");
#[test]
fn quick_test() {
    println!("{blah}");
    //to avoid dealing with stringify! being eager, just use trace_macros nightly feature
}

//probably need to interleave this with the actual computation
//ie so that
//eval!(eq a b)
//expands to something like

//macro_rules! __inner_eq {..}
//eval!(rest of stuff and uses __inner_eq)
//NOTE: rust macros are fine with shadowing, and will use the most recently defined one; abuse this to all hell.
macro_rules! eq {
    ($lhs:tt $rhs:tt) => {
        macro_rules! __eq {
                                            ($lhs $lhs) => "true";
                                            ($lhs $rhs) => "false";
                                        }
    __eq($lhs $rhs)
    };

}

macro_rules! car {
    () => {};
}

//minimal viable example - no recursive eval (args essentially all quoted by default)

macro_rules! min_eval {
    (QUOTE $rest:tt) => {
        $rest
    };

    (CAR ($head:tt $rest:tt)) => {
        $head
    };

    //might need a (CDR ($head:tt $($rest)+:tt)) or whatever
    (CDR ($head:tt $rest:tt)) => {
        $rest
    };

    (ATOM $atom:ident) => {
        TRUE
    };
    (ATOM $t:tt) => {
        NIL
    };

    (CONS $x:tt $y:tt) => {
        ($x $y)
    };
}

//top level

macro_rules! rec_eval {
    (QUOTE $rest:tt) => {
        $rest
    };

    //So, what do we actually want here?
    //ideally, we want something like:
    // (ATOM $t:tt) => {
    //     rec_eval!(@ ATOM rec_eval!($t));
    // };
    // but doing this directly is not possible, since rust macros aren't evaulated eagerly (unless you're proc macro or doing Yandros shit).
    // so instead, we call inner
    (ATOM $t:tt) => {
        macro_rules! inner_atom {
            ()
        }
    };

    (@ ATOM $atom:ident) => {
        TRUE
    };
    (@ ATOM $t:tt) => {
        NIL
    };
}

//I want

macro_rules! with_evaled {
    () => {};
}

//words from the yandros above:

// This is the question of eager expansion.
// Basically, it depends on whether you're the author of a! or b!.

// If you're the author of b!, then there is a trick to have the invocation of your macro be the outer one (so that it happens first), and then give whatever it had to give to a!:
// macro_rules! with_b {( /* args of b ,*/ $($rules:tt)* ) => (
//     macro_rules! __emit__ { $($rules)* }
//     __emit__! {
//         /* your macro "output" */
//     }
// )}

// so that you can then write:
// with_b! {( /* args of b ,*/ ( $($output_of_b:tt)* ) => (
//     a! { $($output_of_b)* }
// )}

//vaguely working example of what I want

// #![feature(trace_macros)]

// macro_rules! rec_eval {
//     (QUOTE $rest:tt) => {
//         $rest
//     };

//     (ATOM $t:tt) => {
//         callback!(rec_eval(@ ATOM rec_eval!$t:tt))
//     };

//     (@ ATOM $atom:ident) => {
//         TRUE
//     };
//     (@ ATOM $t:tt) => {
//         NIL
//     };
// }

// macro_rules! callback {
//     ($callback:ident( $($args:tt)* )) => {
//         $callback!( $($args)* )
//     };
// }

// fn main() {
//     trace_macros!(true);
//     rec_eval!(ATOM (QUOTE x))
// }

// NEW PLAN
// STACK MACHINES MY BELOVED

// implementing a simple lisp with QUOTE, ATOM, EQ.
// Quote: pushes entire next s expression onto the stack, without breaking it up.
// Atom: pop top from data stack, if it is an atom put TRUE on top of the data stack, otherwise NIL.
// EQ: pop two top from data stack, if equal push TRUE, else push NIL. Generate an interior macro to test equality.

// format/decoding:
// stack_lisp!(  (ATOM X) [ ]  [ ])
//                   1     2    3
// 1. S-expression
// 2. Operand/control stack
// 3. Data stack

// For primitives that have a single arg, it is relatively easy; eg `(ATOM X)` => `X` [Atom] [] => `` [ATOM] [X] => `` [] [T]
// once we have fully parsed the lisp into a stack based form, we can then interpret the result:
// - pop the top operator, and then pop however many operands the operator needs from the data stack, compute result, put on top of data stack.

// unresolved questions:
// - handling recursion? Lambda? label?
// - handling proper lists vs dotted lists? investigate handling Cons
// - maybe use a different token to deliminate the stacks than '[' and ']'
// - can this scheme deal with things like cond and weird orders of evaluation?
// - can I split up 'lifetime into ' and lifetime? That way I could use a "lisp macro" to implement that 'a == (quote x). Might have to use a different character.

// for eq, instead of expanding a macro above and using the macro inside, I can instead branch the whole thing inside a macro.
// maybe use https://veykril.github.io/tlborm/decl-macros/patterns/tt-bundling.html instead of having $($token_tree:tt)* everywhere

//can use lambda for most things except for quote https://news.ycombinator.com/item?id=8715655

//first iteration: only has instructions which accept a single argument

macro_rules! stack_lisp_v1 {


    // use another macro as top level ie
    // macro_rules! lisp {($($sexprs:tt)* ) => stack_lisp!($($sexprs)* [] [])}
    // maybe split up parsing, executing into different macros?

    ( (ATOM $toks:tt) [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!( $toks [ATOM $($operands)*] [$($data)*])};
    ( (QUOTE $toks:tt) [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!( [$($operands)*] [$toks $($data)*])}; //just puts the rest of the args onto the data stack (is this right???)
    ( (CAR $toks:tt) [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!( $toks [CAR $($operands)*] [$($data)*])};
    ( (CDR $toks:tt) [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!( $toks [CDR $($operands)*] [$($data)*])};

    ( $val:ident [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!( [ $($operands)*] [$val $($data)*])}; //need to figure out how to do this in the presence of variable bindings. Maybe don't need two stacks?
    ( $toks:tt [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [$toks $($data)*])};
    ( [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [$($data)*])};



    // ATOM operation
    (@eval [ATOM $($operands:tt)*] [ $atom:ident $($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [ TRUE $($data)*])};
    (@eval [ATOM $($operands:tt)*] [ $list:tt $($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [ NIL $($data)*])};

    // CAR operation
    (@eval [CAR $($operands:tt)*] [ ($list:tt) $($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [ $list $($data)*])};              // (car (list)) = list
    (@eval [CAR $($operands:tt)*] [ ($list:tt $($rest:tt)*) $($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [ $list $($data)*])}; // (car(list ....)) = list

    // CDR operation
    (@eval [CDR $($operands:tt)*] [ ($list:tt) $($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [ NIL $($data)*])};                // (cdr (list)) = nil
    (@eval [CDR $($operands:tt)*] [ ($list:tt $($rest:tt)*) $($data:tt)*]) => {stack_lisp_v1!(@eval [$($operands)*] [ $($operand)* $($data)*])}; // (cdr (list ....)) = ....
    (@eval [CDR $($operands:tt)*] [ $invalid:ident $($data:tt)*]) => {"error: cdr cannot be called on atoms"};


    // CONS operation (not implemented yet)
    (@eval [CONS $($operands:tt)*] [ $a:tt $b:tt $($data:tt)*]) => {stack_lisp!(@eval [$($operands)*] [ ($a $b) $($data)*])};              // (cons x y) = (x y)


    //finish and print final evaluation
    (@eval [] [$val:tt]) => {stringify!($val)};


}

//TODO: change the stacks to be something other than open and closed brackets, otherwise $($rest:tt)* will be too greedy.
// might need something to denote whether we're consuming arguments, to ensure our stack_lisp macro is only consuming a single lisp expression.
macro_rules! stack_lisp_v2 {
    ( (ATOM $toks:tt) $($rest:tt)* @ $($operands:tt)* @ [$($data:tt)*]) => {stack_lisp_v2!( $toks $($rest)* @ATOM $($operands)*@ [$($data)*])};
    ( (QUOTE $toks:tt) $($rest:tt)* @$($operands:tt)*@ [$($data:tt)*]) => {stack_lisp_v1!( $($rest:tt)* @$($operands)*@ [$toks $($data)*])};

    ( $val:ident $($rest:tt)* [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v2!($($rest:tt)* [ $($operands)*] [$val $($data)*])}; //need to figure out how to do this in the presence of variable bindings. Maybe don't need two stacks?
    ( $toks:tt [$($operands:tt)*] [$($data:tt)*]) => {stack_lisp_v2!(@eval [$($operands)*] [$toks $($data)*])};
}

macro_rules! lisp {
    ($($toks:tt)+) => {internal_lisp!(stack: [] env: []  control: ($($toks)+)  dump: [] )};
}

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



    // LAMBDA special form
    (stack: [$($stacks:tt)*] env: $env:tt control: [(LAMBDA ($name:ident) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [[$name, $T, $env] $($stacks)*] env: $env control: [$($controls:tt)*] dump: $dump)
    };


    // IF special form

    // (t1 t2) => t2::t1::ap
    // figure out a nice way to extend this to more args automatically
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [($op:tt $arg:tt) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$x $($stack_entries)*] env: $env control: [$arg $op ap $($rest)*] dump: $dump)
    };

    // pop primitives onto the stack as procs
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [CAR $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [@CAR $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    (stack: [$($stack_entries:tt)*] env: $env:tt control: [CDR $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [@CDR $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [ATOM $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [@ATOM $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [DISPLAY $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [@DISPLAY $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };


    // Evaluate primitives


    // CAR takes a list off the top of the stack, and returns its car.
    // (CAR (x y z)) == x
    (stack: [@CAR ($car:tt $($cdr:tt)*) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$car $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [@CDR ($car:tt) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [NIL $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [@CDR ($car:tt $($cdrs:tt)* ) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($cdrs)*) $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [@DISPLAY $val:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        println!("{}", stringify!($val));
        internal_lisp!(stack: [$car $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    (stack: [[$var:ident, ] $val:tt $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        println!("{}", stringify!($val));
        internal_lisp!(stack: [$car $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };


    // evalute closure
    (stack: [ [$x:ident, $T:tt, ] $($stacks:tt)*]) => {

    };





    // Split list into stack (CAR X) => X::CAR::ap



    // Evaluate symbol in environment - Rule 1 in notes
    (stack: [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$symb:ident $($rest:tt)*] dump: $dump:tt) => {
        macro_rules! evaluate_in_env {
            $(
                ($key, $stack:tt, $env: tt, $control:tt) => {internal_lisp!(@fix stack: $val [$stack] env: $env control: $control dump: $dump)};
            )*
            ($not_found:ident) => {error!("val not found in environment")}
        };
        evaluate_in_env!($symb, [$($stack_entries)*], [$($key : $val )*], [$($rest)*]) // we do this cursed expansion here so we can pass the entire $($rest)* capture groups along, instead of repeating one by one.
    };
    (@fix stack: $top:tt [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$($rest:tt)*] dump: $dump:tt) => { //needed to avoid some fuckery in the first statement, fixes up the formatting.
        // internal_lisp!(stack: [$top $($stack_entries)*] env: [$($key : $val)*] control: [$($rest)*] dump: $dump)
        println!("{}", stringify!($top)); //for debugging purposes only
    };
    // (stack: [$($stack:tt)*] env: [$($key:ident : $val:tt)*] control: [$symb:ident $($rest:tt)*] dump: $dump:tt) => {
    //     macro_rules! evaluate_in_env {
    //         $(
    //             ($key) => {internal_lisp!(stack: [$val $($stack)*] env: [$($key : $val)*] control: [$($rest)*] dump: $dump)};
    //         )*
    //         ($not_found:ident) => {error!("val not found in environment")}
    //     }
    //     evaluate_in_env!($symb)
    // };


    (stack: [$top:tt $($rest:tt)*] env: [$($envs:tt)*] control: [] dump: []  ) => {
        stringify!($top)
    }; //Rule 6: termination
}

// either causes a compiler error with the error message, or evalutes program to a string of the error message
macro_rules! error {
    ($($toks:tt)*) => {println!("{}", stringify!($($toks)*))}; // program evaluates to a single print statement with the error message
    ($($toks:tt)+) => {compile_error!(concat!("Lisp execution failure: ", stringify!($($toks)+)))}; //program halts rust compilation with the error message

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
    let hello = stack_lisp_v1!((ATOM (QUOTE hello)) [] []);
    println!("{hello}");
    let hello = stack_lisp_v1!((CDR (QUOTE (hello))) [] []);
    println!("{hello}");
    let hello = stack_lisp_v1!((QUOTE (QUOTE (hello))) [] []);
    println!("{hello}");
    internal_lisp!(stack: [random nonsense] env: [a: (A B)] control: [a] dump: []);
    dbg!(internal_lisp!(stack: [] env: [] control: [(CDR (QUOTE (X Y)))] dump: []));
}

// Desription of my lisp:

// a simple LISP with lexical scoping, implemented fully in the Rust macro system.
// Avaliable primitives: atom, if, eq, cons, car, cdr, lambda with a single argument, display
// hopefully soon: lambda with arbitary arguments, eval primitive, define primitive.
// maybe add macros?
// for proof of concept, write a meta circular interpreter (ie just steal Graham's).
// check if this is lexical https://stackoverflow.com/questions/32344615/program-to-check-if-the-scoping-is-lexical-or-dynamic, I believe it is
