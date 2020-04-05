use std::{error, fmt};

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct SyntaxError {
    kind: ErrorKind,
    pos: usize,
}

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub enum ErrorKind {
    BadWildcardArgument(&'static str),
    EarlyEOF(&'static str),
    ExpectLeft(&'static str),
    ExpectNull(&'static str),
    NotAToken,
    Unmatched(&'static str),
}

impl error::Error for SyntaxError {
}

impl SyntaxError {
    pub fn new(kind: ErrorKind, pos: usize) -> SyntaxError {
        SyntaxError{kind, pos}
    }

    pub fn pos(&self) -> usize {
        self.pos
    }
}

impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "syntax error at position {}", self.pos())
    }
}
