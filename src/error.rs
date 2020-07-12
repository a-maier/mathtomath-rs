use std::{str, error, fmt};

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
pub struct SyntaxError {
    kind: ErrorKind,
    pos: usize,
}

#[derive(Copy,Clone,Eq,PartialEq,Debug)]
pub enum ErrorKind {
    EarlyEOF(&'static str),
    ExpectLeft(&'static str),
    ExpectNull(&'static str),
    NotAToken,
    Unmatched(&'static str),
    Utf8Error(str::Utf8Error),
    RemainingToken,
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

impl std::convert::From<str::Utf8Error> for SyntaxError {
    fn from(err: str::Utf8Error) -> Self {
        let pos = err.valid_up_to();
        let kind = ErrorKind::Utf8Error(err);
        SyntaxError::new(kind, pos)
    }
}
