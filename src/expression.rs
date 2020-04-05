use std::convert::From;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub enum Expression<'a> {
    Empty,
    Integer(&'a [u8]),
    Real(&'a [u8]),
    Symbol(&'a [u8]),
    Wildcard(Symbol<'a>),
    Many0Wildcard(Symbol<'a>),
    Plus(Vec<Expression<'a>>),
    Minus(Vec<Expression<'a>>),
    Times(Vec<Expression<'a>>),
    Divide(Vec<Expression<'a>>),
    Compound(Vec<Expression<'a>>),
    Coefficient(Vec<Expression<'a>>),
    Sequence(Vec<Expression<'a>>),
    Equals(Vec<Expression<'a>>),
    Dot(Vec<Expression<'a>>),
    Power(Vec<Expression<'a>>),
    Ellipsis,
    ScalarProduct,
    Function(Box<Function<'a>>),
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Integer<'a>(pub &'a [u8]);

impl<'a> From<Integer<'a>> for Expression<'a> {
    fn from(int: Integer<'a>) -> Self {
        Expression::Integer(int.0)
    }
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Real<'a>(pub &'a [u8]);

impl<'a> From<Real<'a>> for Expression<'a> {
    fn from(real: Real<'a>) -> Self {
        Expression::Real(real.0)
    }
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Symbol<'a>(pub &'a [u8]);

impl<'a> From<Symbol<'a>> for Expression<'a> {
    fn from(sym: Symbol<'a>) -> Self {
        Expression::Symbol(sym.0)
    }
}

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Function<'a> {
    pub head: Expression<'a>,
    pub args: Vec<Expression<'a>>,
}

impl<'a> From<Function<'a>> for Expression<'a> {
    fn from(f: Function<'a>) -> Self {
        Expression::Function(Box::new(f))
    }
}

#[derive(Copy,Clone,Eq,Default,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Ellipsis {}

impl<'a> From<Ellipsis> for Expression<'a> {
    fn from(_dots: Ellipsis) -> Self {
        Expression::Ellipsis
    }
}

#[derive(Copy,Clone,Eq,Default,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct ScalarProduct {}

impl<'a> From<ScalarProduct> for Expression<'a> {
    fn from(_dots: ScalarProduct) -> Self {
        Expression::ScalarProduct
    }
}

#[derive(Copy,Clone,Eq,Default,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Wildcard<'a>(pub Symbol<'a>);

impl<'a> From<Wildcard<'a>> for Expression<'a> {
    fn from(wildcard: Wildcard<'a>) -> Self {
        Expression::Wildcard(wildcard.0)
    }
}

#[derive(Copy,Clone,Eq,Default,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Many0Wildcard<'a>(pub Symbol<'a>);

impl<'a> From<Many0Wildcard<'a>> for Expression<'a> {
    fn from(wildcard: Many0Wildcard<'a>) -> Self {
        Expression::Many0Wildcard(wildcard.0)
    }
}

// #[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
// pub enum Builtin<'a> {
//     Ellipsis,
//     Wildcard(Symbol<'a>),
// }
