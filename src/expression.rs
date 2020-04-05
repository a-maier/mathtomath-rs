use std::convert::From;

type Args<'a> = Box<(Expression<'a>, Expression<'a>)>;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub enum Expression<'a> {
    Empty,
    Integer(&'a [u8]),
    Real(&'a [u8]),
    Symbol(&'a [u8]),
    Wildcard(Symbol<'a>),
    Many0Wildcard(Symbol<'a>),
    UPlus(Box<Expression<'a>>),
    UMinus(Box<Expression<'a>>),
    Plus(Args<'a>),
    Minus(Args<'a>),
    Times(Args<'a>),
    Divide(Args<'a>),
    Compound(Args<'a>),
    Coefficient(Args<'a>),
    Sequence(Args<'a>),
    Equals(Args<'a>),
    Dot(Args<'a>),
    Power(Args<'a>),
    Ellipsis,
    Function(Args<'a>),
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

#[derive(Copy,Clone,Eq,Default,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Ellipsis {}

impl<'a> From<Ellipsis> for Expression<'a> {
    fn from(_dots: Ellipsis) -> Self {
        Expression::Ellipsis
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
