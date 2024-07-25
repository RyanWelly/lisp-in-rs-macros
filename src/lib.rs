use std::marker::PhantomData;

mod macros;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        assert_eq!(result, 4);
    }
}

// ((lambda (param_1) expression ) applied_to)

// for the sake of ease, I might define a (lamb x e a) in order to simplify? then you can define lambda proper

//Let there by light!

struct Nil;

struct Cons<Head, Tails>(PhantomData<(Head, Tails)>);

//Symbols
struct A;
struct B;
trait Symbol {}
impl Symbol for A {}
impl Symbol for B {}

// self evaluating

trait Eval {
    type Result;
}

impl Eval for A {
    type Result = A;
}
impl Eval for B {
    type Result = B;
}

impl Eval for Nil {
    type Result = Nil;
}

// PRIMITIVES

//cond
//use an intermediate trait

//lambda
struct Lambda<x, body, arg>(PhantomData<(x, body, arg)>);

//eq
// will get around the need for specilisation by just defining it on Symbols. Then just have to generate an exponetial implementations for Symbols?
// https://github.com/Dragon-Hatcher/type-system-chess just goes the exponential route for comparing pieces
