#[macro_export]
macro_rules! internal_lisp {

    // Evaluate special forms

    // QUOTE special form
    (stack: [$($stack_entries:tt)*] env: $env:tt control: [(QUOTE $x:tt)$($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$x $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // LAMBDA special form - saves closure onto stack with current env
    // When we encounter a lambda, we wrap it up into a closure and push it to the stack.
    // closures store the names of any arguments, the inner expression, and the current env.

    (stack: [$($stacks:tt)*] env: $env:tt control: [(LAMBDA ($($names:ident)*) $T:tt) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [[{$($names)*}, $T, $env] $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };


    // Let form
    // (LET (x1 e1) ... (xn en) e) === ((LAMBDA (x1 ... xn ) e ) e1 ... en)
    // simple transformation, nothing weird.

    // since the naive "(LET $(($xn:tt $en:tt))* $e:tt)" leads to local ambiguity, we need to split it up into the two possibilities.

    (stack: $stack:tt env: $env:tt control: [(LET ($(($xn:tt $en:tt))*) $e:ident) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [  ((LAMBDA ($($xn)* ) $e) $($en)*) $($controls)* ] dump: $dump )
    };
    (stack: $stack:tt env: $env:tt control: [(LET ($(($xn:tt $en:tt))*) ($($forms:tt)*)) $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [  ((LAMBDA ($($xn)* ) ($($forms)*)) $($en)*) $($controls)* ] dump: $dump )
    };




    // COND special form
    // (COND (p1 e1) .... (pn en)) => p1 :: (IFS e1 (COND (p2 e2) ... (pn en)) )
    // (COND (p1 e)) => p1 :: (IFS e nil)



    (stack: $stack:tt env: $env:tt control: [(COND ($p:tt $e:tt) $(($pn:tt $en:tt))+ )  $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$p {__IFS $e (COND $(($pn $en))+  ) } $($control)*] dump: $dump)
    }; 
    (stack: $stack:tt env: $env:tt control: [(COND ($p:tt $e:tt)) $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$p {__IFS $e NIL } $($control)*] dump: $dump)
    }; // If we reach the end of the COND without a predicate matching, we return nil

    (stack: [() $($stack:tt)*] env: $env:tt control: [{__IFS $p:tt $e:tt } $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$($stack)*] env: $env control: [ $e $($control)*] dump: $dump)
    }; // NIL/false case for the __IFS

    (stack: [$val:tt $($stack:tt)*] env: $env:tt control: [{__IFS $p:tt $e:tt } $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$($stack)*] env: $env control: [ $p $($control)*] dump: $dump)
    }; // truthy case for the __IFS - everything but nil/empty list is true


    // If special form - simple desugaring to internal primitive
    // (IF p e1 e2) === (COND (p e1) (TRUE e2))


    (stack: $stack:tt env: $env:tt control: [(IF $p:tt $e1:tt $e2:tt)  $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$p {__IFS $e1 $e2 } $($control)*] dump: $dump)
    };







    // Define special form
    // semantics: can use at top level or any level at all, (define name exp) just evalutes exp in the current env, then binds it to `name` in the current env.
    // Returns nil.
    // (DEFINE name expr) => expr :: {__DEFINE name}
    // TODO - enable recursive defines somehow - maybe specialise when there is a closure on tbe stack and {__DEFINE: $name} on the control
    // maybe return a "recursive closure" (fixpoint?)




    (stack: $stack:tt env: $env:tt control: [ (DEFINE $name:ident $exp:tt) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [$exp {__DEFINE:$name} $($rest)*] dump: $dump)
    };


    // TODO - wrap defined lambdas in the z combinator to get them working recursively 

    (stack: [$value:tt $($stack:tt)*] env: [$($key:ident : $val:tt)*] control: [{__DEFINE:$name:ident} $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [() $($stack)*] env: [$name: $value $($key : $val)*] control: [$($rest)*] dump: $dump )
    };

    //PROGN special form
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

    // (t1 t2) => t2::t1::ap
    // (f a1 a2 a3 ..) expands to essentially (LIST a1 a2 a3 ...) :: f :: ap

    (stack: $stack:tt env: $env:tt control: [($op:tt $($args:tt)*) $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: $stack env: $env control: [NIL $($args {cons})* $op ap $($rest)* ] dump: $dump)
    };


    //quick dirty hack for fitting arguments into a list; add a new `cons` opcode, similar to the ap, which just cons the top two values on the stack.
    // (f a1 a2 a3) => NIL :: a1 :: {cons} :: a2 :: {cons} :: a3 :: {cons} :: f :: ap
    
    
    (stack: [ $x:tt () $($stacks:tt)*] env: $env:tt control: [{cons} $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($x) $($stacks)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [$y:tt ($($vals:tt)*)  $($stacks:tt)*] env: $env:tt control: [{cons} $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($vals)* $y) $($stacks)* ] env: $env control: [$($rest)*] dump: $dump)
    };

    // Evaluate primitives - top of the stack


    // CAR
    (stack: [__CAR (($car:tt $($cdr:tt)*)) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$car $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // CDR
    (stack: [__CDR (($car:tt)) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [() $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [__CDR (($car:tt $($cdrs:tt)* )) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($cdrs)*) $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // DISPLAY
    // TODO - prettify the printed output, especially for closures
    (stack: [__DISPLAY ($val:tt) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        {println!("{}", prettify_lisp!($val));
        internal_lisp!(stack: [$val $($stacks)*] env: $env control: [$($controls)*] dump: $dump)}
    };

    //ATOM 
    (stack: [__ATOM ($atom:ident) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    (stack: [__ATOM ($not_atom:tt) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ () $($stacks)*] env: $env control: [$($controls)*] dump: $dump)
    };

    // EQ 
    (stack: [__EQ ($val1:tt $val2:tt) $($stacks:tt)*] env: $env:tt control: [ap $($controls:tt)*] dump: $dump:tt) => {{
        macro_rules! __internal_eq {
            ($val1 $val1) => {internal_lisp!(stack: [TRUE $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
            ($val1 $val2) => {internal_lisp!(stack: [() $($stacks)*] env: $env control: [$($controls)*] dump: $dump)};
        }
        __internal_eq!($val1 $val2)}
    };

    // CONS 
    (stack: [__CONS ($val:tt ()) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ ($val)  $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };
    (stack: [__CONS ($val:tt ($($list:tt)*)) $($stack_entries:tt)*] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [ ($val $($list)*)  $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // LIST
    (stack: [__LIST ($($vals:tt)*) $($stack_entries:tt)* ] env: $env:tt control: [ap $($rest:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [($($vals)*) $($stack_entries)*] env: $env control: [$($rest)*] dump: $dump)
    };

    // APPLY
    (stack: [__APPLY ($f:tt ($($args:tt)*))  $($rest:tt)* ] env: $env:tt control: [ap $($control:tt)*] dump: $dump:tt) => {
        internal_lisp!(stack: [$f ($($args)*) $($rest)*] env: $env control: [ap $($control)*] dump: $dump )
    };



    // Deal with closures


    //errors if the number of vars is not the same as the number of arguments given.
    (stack: [ [{$($vars:ident)*}, $T:tt, [$($key:ident : $value:tt)*]] ($($args_list_entry:tt)*) $($stacks:tt)*]  env: $env:tt control: [ap $($controls:tt)*] dump: [$($dump:tt)*] ) => {
        internal_lisp!(stack: [] env: [$($vars : $args_list_entry )* $($key: $value)*] control: [$T] dump: [  ([$($stacks)*], $env, [$($controls)*]) $($dump)*])
        
    };

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
            ($symb, $stack:tt, $env: tt, $control:tt) => {error!(Lisp error: cannot find $symb in env)}
        }
        evaluate_in_env!($symb, [$($stack_entries)*], [$($key : $val )*], [$($rest)*]) } // we do this cursed expansion here so we can pass the entire $($rest)* capture groups along, instead of repeating one by one.
    };
    (@fix stack: $top:tt [$($stack_entries:tt)*] env: [$($key:ident : $val:tt)*] control: [$($rest:tt)*] dump: $dump:tt) => { //needed to avoid some fuckery in the first statement, fixes up the formatting.
        internal_lisp!(stack: [$top $($stack_entries)*] env: [$($key : $val)*] control: [$($rest)*] dump: $dump)
    };


    (stack: [$top:tt $($rest:tt)*] env: [$($envs:tt)*] control: [] dump: []  ) => {
        prettify_lisp!($top)
    }; //Termination
}


// Pretty printing to make the printed representation of closures nicer. 
macro_rules! prettify_lisp {
    ([{$($vars:ident)*}, $T:tt, $env:tt]) => {stringify!([PROC ($($vars)*) $T ])};
    ($($toks:tt)*) => {stringify!($($toks)*)}; //for anything other than a closure, just return 
}

// Helper error handler macro
// either causes a compiler error with the error message, or evalutes program to a string of the error message. 
macro_rules! error {
    ($($toks:tt)+) => {compile_error!(stringify!($($toks)+))}; //program halts rust compilation with the error message
    ($($toks:tt)*) => {println!("{}", stringify!($($toks)*))};  

}

macro_rules! lisp { //call internal_lisp! with the default env
    ($($toks:tt)*) => {internal_lisp!(stack: []
        env: [CAR: __CAR
        CDR: __CDR
        ATOM: __ATOM
        DISPLAY:__DISPLAY
        EQ: __EQ
        CONS: __CONS
        LIST: __LIST
        APPLY: __APPLY
        NIL: ()
        TRUE: TRUE]
        control: [($($toks)*)]
        dump: [] )};
}


#[cfg(test)]
mod tests {
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
        assert_eq!(lisp!(CONS (QUOTE A) (QUOTE (B))), stringify!((A B)));
        assert_eq!(
            lisp!(CONS (QUOTE A) (CONS (QUOTE B) NIL)),
            stringify!((A B))
        );
        assert_eq!(lisp!(PROGN
        (DEFINE test NIL)
        (EQ test NIL)
        (EQ (QUOTE A) (QUOTE A))
        ), "TRUE");
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
        lisp!(LIST (CAR (QUOTE (A B C))));

    }

    #[test]
    fn progn() {
        assert_eq!(lisp!(PROGN (QUOTE A) (QUOTE B)), stringify!(B));
        let test = lisp!(PROGN (DEFINE A (QUOTE B)) (CONS A NIL)); // TODO: figure out why the define doesn't work, and A does not have a binding in the env.
        dbg!(test);
    }

    #[test]
    fn conditionals() {
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

        dbg!(lisp!(IF (EQ NIL NIL) (QUOTE A) NIL));

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
        assert_eq!(lisp!((LAMBDA (X Y) (CONS X Y)) (QUOTE A) NIL), stringify!((A)));
        
        assert_eq!(lisp!(PROGN 
            (DEFINE list3 (LAMBDA (X Y Z) (LIST X Y Z)))
            (DEFINE constant (QUOTE A))
            (DISPLAY (list3 constant NIL constant))
            (DEFINE print_a (LAMBDA () (DISPLAY (QUOTE A))))
            (print_a)
        ), "A");

        assert_eq!(lisp!(DISPLAY (LET ((x (QUOTE BANANA))) x)), "BANANA");
        assert_eq!(lisp!(LET ((X (QUOTE A)) (Y NIL)) (CONS X Y)), stringify!((A)));

        assert_eq!(lisp!((CAR(LIST (LAMBDA (X) X) (LAMBDA (X) NIL))) (QUOTE swim)), stringify!(swim));
        dbg!(lisp!(LAMBDA () (DISPLAY NIL))); 
    }

    #[test]
    fn apply() {
        assert_eq!(lisp!(APPLY LIST (QUOTE (A B))), stringify!((A B)));
    }


    #[test]
    fn append_example() {

        let test = lisp!(LET ((APPEND (LAMBDA (s X Y) (IF (EQ X NIL) Y (CONS (CAR X) (s s (CDR X) Y)))))) (APPEND APPEND (QUOTE (A B)) (QUOTE (C D))));
        assert_eq!(test, stringify!((A B C D)));

    }
    #[test]
    fn utility_functions() {
        let test = lisp!(PROGN
            (DEFINE IS_NULL (LAMBDA (X) (EQ X NIL)))
            (DEFINE append (LAMBDA (s X Y) 
            (PROGN
                (IF (EQ X NIL)
                Y  
                (CONS (DISPLAY (CAR X)) (s s (CDR X) Y))
                )
            )))
            (append append (QUOTE (ma da) ) (QUOTE (A B)))
        );
        assert_eq!(test, stringify!((ma da A B)));
        assert_eq!(lisp!((LAMBDA (s X Y) 
                (IF (EQ X NIL)
                Y  
                (CONS (DISPLAY (CAR X)) (s s (CDR X) Y))
                )
            ) (LAMBDA (s X Y) 
                (IF (EQ X NIL)
                Y  
                (CONS (DISPLAY (CAR X)) (s s (CDR X) Y))
                )
            ) (QUOTE (A B) ) (QUOTE (h a))), stringify!((A B h a)));
    }
    #[test]
    fn recursion() {
        let test = lisp!(PROGN
            (DEFINE Y
                (LAMBDA (h)
                    ((LAMBDA (x) (h (LAMBDA (a b) ((x x) a b))))
                    (LAMBDA (x) (h (LAMBDA (a b) ((x x) a b)))))))
            (DEFINE APPEND (Y (LAMBDA (append) (LAMBDA (w z) (IF (EQ w NIL) z (CONS (CAR w) (append (CDR w) z)))))))
            (APPEND (QUOTE (A)) (QUOTE (B C)))
            
        ); 
        dbg!(test);

        lisp!(PROGN
        (DEFINE TEST NIL)
        );
    }
}

#[cfg(test)]
mod metacircular {
    


    #[test]
    fn meta() {
        // // simple lisp interpreter - only supports lambda with one parameter
        // // At the moment, using y combinator for recursion causes an explosion in code size, exceeding far more than a million tokens and leading to cargo getting sigkilled
        // let test: &str = lisp!(PROGN
        //     // Y "combinator" for two arguments
        // (DEFINE Y2 
        //                 (LAMBDA (h)
        //                     ((LAMBDA (x) (h (LAMBDA (a b) ((x x) a b))))
        //                         (LAMBDA (x) (h (LAMBDA (a b) ((x x) a b)))))))
        
        // (DEFINE CADR (LAMBDA (X) (CAR (CDR X))))
        // (DEFINE CAAR (LAMBDA (X) (CAR (CAR X))))
        // (DEFINE CADAR (LAMBDA (X) (CAR (CDR (CAR X)))))
        // (DEFINE CADDR (LAMBDA (X) (CAR (CDR (CDR X)))))
        // (DEFINE CADDAR (LAMBDA (X) (CAR (CDR (CDR (CAR X))))))
        // (DEFINE CAADAR (LAMBDA (X) (CAR (CAR (CDR (CAR X))))))

        // (DEFINE ASSOC (Y2 (LAMBDA (ASSOC) (LAMBDA (X ENV) 
        //                 (IF (EQ (CAAR ENV) X) (CADAR ENV) (ASSOC X (CDR ENV)))
        //             )))
        //         )


        // (DEFINE UNIMPLEMENTED (LAMBDA () (DISPLAY (QUOTE UNIMPLEMENTED))))


            
        // // using lowercase for the interpreted language, just to be clearer.
        // (DEFINE eval (Y2 (LAMBDA (EVAL) (LAMBDA (E A) 
        //         (COND
        //             ((ATOM E) (ASSOC E A))
        //             ((ATOM (CAR E)) 
        //                 (COND 
        //                     ((EQ (CAR E) (QUOTE quote)) (CADR E))
        //                     ((EQ (CAR E) (QUOTE atom)) (ATOM (EVAL (CADR E) A)))
        //                     ((EQ (CAR E) (QUOTE car)) (CAR (EVAL (CADR E) A)))
        //                     ((EQ (CAR E) (QUOTE cdr)) (CDR (EVAL (CADR E) A)))
        //                     ((EQ (CAR E) (QUOTE equal)) (EQ (EVAL (CADR E) A) (EVAL (CADDR E) A)))
        //                     ((EQ (CAR E) (QUOTE cons)) (CONS (EVAL (CADR E) A) (EVAL (CADDR E) A)))
        //                     (TRUE (EVAL (CONS (ASSOC (CAR E) A) (CDR E)) A)) //replace with looking up the value of (CAR E) in the environment
        //                 )
        //             )
        //             ((EQ (CAAR E) (QUOTE lambda)) (EVAL (CADDAR E) (CONS (LIST (CAADAR E) (EVAL (CADR E) A)) A)  )) //Evaluate the inner expression of the lambda, in the environment with the argument bound to the parameter
                
        //         )
        //     ))))

        // // (eval (QUOTE (quote (A))) NIL)
        // // (eval (QUOTE (atom (quote A))) NIL )
        // // (eval (QUOTE (cdr (cdr (quote (A B))))) NIL)
        // // (eval (QUOTE (cons (quote a) (quote (a)))) NIL)
        // // (eval (QUOTE ((lambda (x) (quote a)) (quote b))) NIL)
        // (eval (QUOTE ((lambda (X) X) (quote a))) NIL)

        // );
        // assert_eq!(test, stringify!((a a)));
        


        // let test = lisp!(PROGN
        //     (DEFINE Y2 
        //         (LAMBDA (h)
        //             ((LAMBDA (x) (h (LAMBDA (a b) ((x x) a b))))
        //                 (LAMBDA (x) (h (LAMBDA (a b) ((x x) a b)))))))
        // );
    }

        
    
    #[test]
    fn quine() {
       assert_eq!(
        lisp!
        ((LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s)))
        (QUOTE (LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s))))), 
        stringify!
        (((LAMBDA (s) (LIST s (LIST (QUOTE QUOTE) s)))
        (QUOTE (LAMBDA (s)(LIST s (LIST (QUOTE QUOTE) s)))))));
    }


    #[test]
    fn lexical() {
        // https://stackoverflow.com/questions/32344615/program-to-check-if-the-scoping-is-lexical-or-dynamic
        assert_eq!(lisp!(PROGN
            (DEFINE test (LET ((scope (QUOTE lexical))) (LAMBDA () scope)))
            (LET ((scope (QUOTE dynamic))) (test))
        ), "lexical");
    }



}

// Desription of my lisp:

// a simple LISP with lexical scoping, implemented fully in the Rust macro system.
// Avaliable primitives: atom, cons, eq, cons, car, cdr, lambda, let, display.
// NIL is a shorthand for the empty list, and the empty list also represents falsity. TRUE and every other value is truthy.
// The define form is a bit weird in my lisp; (define name expr) evaluates expr and then binds it to name in the current env, so there's no restriction to
// only using it at the top level like there is in Scheme. It does not allow recursive definitions, so use the Y combinator or similar fixpoint operators for recursion.

// hopefully soon:  eval primitive, apply primitve, (recursive) define primitive.
// maybe add macros?
// for proof of concept, write a meta circular interpreter (ie just steal Graham's).
