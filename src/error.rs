use std::{str, error, fmt};

#[derive(Clone,Eq,PartialEq,Debug)]
pub struct SyntaxError {
    kind: ErrorKind,
    pos: usize,
}

#[derive(Clone,Eq,PartialEq,Debug)]
pub enum ErrorKind {
    EarlyEOF(&'static str),
    ExpectLeft(&'static str),
    ExpectNull(&'static str),
    NotAToken,
    Unmatched(String),
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
        use ErrorKind::*;
        match self.kind {
            EarlyEOF(s) => write!(f, "unexpected end of expression, expected {}", s)?,
            ExpectLeft(s) | ExpectNull(s) => write!(f, "unexpected token, expected {}", s)?,
            NotAToken => write!(f, "unknown token")?,
            Unmatched(ref b) => write!(f, "unmatched bracket {}", b)?,
            Utf8Error(err) => write!(f, "utf8 conversion error: {}", err)?,
            RemainingToken => write!(f, "remaining text after parsing expression")?,
        };
        write!(f, " at position {}", self.pos())
    }
}

impl std::convert::From<str::Utf8Error> for SyntaxError {
    fn from(err: str::Utf8Error) -> Self {
        let pos = err.valid_up_to();
        let kind = ErrorKind::Utf8Error(err);
        SyntaxError::new(kind, pos)
    }
}

#[derive(Clone,Eq,PartialEq,Debug)]
pub struct ParseError{
    pub context: Option<(String, String)>,
    err: SyntaxError,
}

impl ParseError {
    pub fn new(context: Option<(String, String)>, err: SyntaxError) -> Self {
        ParseError{context, err}
    }
}

impl error::Error for ParseError {
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error parsing expression: {}", self.err)
    }
}

#[derive(Debug)]
pub enum Error {
    Parse(ParseError),
    Io(std::io::Error),
    Utf8(std::str::Utf8Error),
}

impl error::Error for Error {
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;
        match self {
            Parse(err) => err.fmt(f),
            Io(err) => err.fmt(f),
            Utf8(err) => err.fmt(f),
        }
    }
}

impl std::convert::From<ParseError> for Error {
    fn from(err: ParseError) -> Self {
        Error::Parse(err)
    }
}

impl std::convert::From<std::io::Error> for Error {
    fn from(err: std::io::Error) -> Self {
        Error::Io(err)
    }
}

impl std::convert::From<std::str::Utf8Error> for Error {
    fn from(err: std::str::Utf8Error) -> Self {
        Error::Utf8(err)
    }
}
