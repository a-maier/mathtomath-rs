use std::{str, error, io, fmt};

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

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub enum FormatError {
    IoError,
    InvalidExpression,
}

impl fmt::Display for FormatError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "formatting error: {:?}", self)
    }
}

impl error::Error for FormatError {
}

impl std::convert::From<io::Error> for FormatError {
    fn from(_err: io::Error) -> Self {
        FormatError::IoError
    }
}
