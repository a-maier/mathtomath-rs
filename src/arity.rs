
#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub enum Arity {
    Nullary,
    Unary,
    Binary,
    Function,
}
