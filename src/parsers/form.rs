use crate::expression::{self, Expression};
use std::{
    error,
    fmt,
    str::from_utf8
};
use nom::{
    IResult,
    bytes::complete::{is_not, tag, take_while, take_while1},
    branch::alt,
    character::{
        complete::{char, one_of},
        is_alphabetic, is_alphanumeric, is_digit,
    },
    multi::many0,
    sequence::{delimited, tuple},
    combinator::opt,
};

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
enum Token<'a> {
    Symbol(&'a [u8]),
    Integer(&'a [u8]),
    Ellipsis,
    LeftBracket,
    RightBracket,
    LeftSquareBracket,
    RightSquareBracket,
    LeftBrace,
    RightBrace,
    Plus,
    Minus,
    Times,
    Divide,
    Power,
    Dot,
    Equals,
    Wildcard,
    Comma,
    Semicolon,
}

const STR_TO_TOKEN: phf::Map<&'static [u8], Token<'static>> = phf_map!{
    b"..." => Token::Ellipsis,
    b"(" => Token::LeftBracket,
    b")" => Token::RightBracket,
    b"[" => Token::LeftSquareBracket,
    b"]" => Token::RightSquareBracket,
    b"{" => Token::LeftBrace,
    b"}" => Token::RightBrace,
    b"+" => Token::Plus,
    b"-" => Token::Minus,
    b"*" => Token::Times,
    b"/" => Token::Divide,
    b"^" => Token::Power,
    b"." => Token::Dot,
    b"=" => Token::Equals,
    b"?" => Token::Wildcard,
    b"," => Token::Comma,
    b";" => Token::Semicolon,
};

#[derive(Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct SyntaxError {
    pos: usize,
}

impl error::Error for SyntaxError { }

impl SyntaxError {
    pub fn at(pos: usize) -> SyntaxError {
        SyntaxError{pos}
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

fn integer(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(is_digit)(i)
}

fn word_symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let maybe_dollar = opt(char('$'));
    let letters = take_while1(is_alphabetic);
    let rest = take_while(is_alphanumeric);
    let maybe_underscore = opt(char('_'));
    let word_symbol = tuple((maybe_dollar, letters, rest, maybe_underscore));

    let (_, (maybe_dollar, letters, alnum, maybe_underscore)) = word_symbol(i)?;
    let dollar_len = if maybe_dollar.is_some() {1} else {0};
    let underscore_len = if maybe_underscore.is_some() {1} else {0};
    let len = dollar_len + letters.len() + alnum.len() + underscore_len;
    let (symbol, rest) = i.split_at(len);
    trace!("parsed symbol '{}'", from_utf8(symbol).unwrap());
    Ok((rest, symbol))
}

fn bracket_symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let symbol = delimited(
        char('['), many0(alt((is_not("[]"), bracket_symbol))), char(']')
    );
    let (_rest, symbol) = symbol(i)?;
    let symbol_len = symbol.into_iter().map(|s| s.len()).sum::<usize>() + 2;
    let (symbol, rest) = i.split_at(symbol_len);
    trace!("parsed symbol '{}'", from_utf8(symbol).unwrap());
    Ok((rest, symbol))
}

fn symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((word_symbol, bracket_symbol))(i)
}

fn ellipsis(i: &[u8]) -> IResult<&[u8], &[u8]> {
    tag("...")(i)
}

fn whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|u: u8| u.is_ascii_whitespace())(i)
}

fn reverse<T,U>(tuple: (T, U)) -> (U, T) {
    (tuple.1, tuple.0)
}

fn operator_or_bracket(i: &[u8]) -> IResult<&[u8], &[u8]> {
    if let Ok(result) = ellipsis(i) {
        trace!("parsed ellipsis");
        Ok(result)
    } else {
        // we have to change the return type from char to &[u8]
        // otherwise a simple alt combinator would do
        one_of("*+-/^.?,=;(){}[]")(i).and_then(
            |_| {
                trace!("parsed {}", i[0]);
                Ok(reverse(i.split_at(1)))
            }
        )
    }
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
struct Lexer<'a> {
    remaining_input: &'a [u8],
    pos: usize,
    last_token_was_a_symbol: bool,
}

impl<'a> Lexer<'a> {
    pub fn for_input(input: &'a [u8]) -> Lexer<'a> {
        Lexer{
            remaining_input: input,
            pos: 0,
            last_token_was_a_symbol: false,
        }
    }

    fn parse_success(&mut self, token: &'a [u8], new_remaining: &'a [u8]) {
        debug!("parsed '{}'", from_utf8(token).unwrap());
        self.pos += token.len();
        self.remaining_input = new_remaining;
    }

    fn pos(&self) -> usize {
        self.pos
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<Token<'a>, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        trace!("lexer called on {}", from_utf8(self.remaining_input).unwrap());
        let (remaining_input, ws) = whitespace(self.remaining_input).unwrap();
        if !ws.is_empty() {
            self.parse_success(ws, remaining_input);
        }
        if self.remaining_input.is_empty() {
            return None;
        }
        // a square bracket directly following a symbol indicates the cofficient
        // of some expression, not a new symbol
        if self.last_token_was_a_symbol && self.remaining_input.first() == Some(& b'[') {
            self.last_token_was_a_symbol = false;
            let (token, remaining_input) = self.remaining_input.split_at(1);
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::LeftSquareBracket))
        }
        if let Ok((remaining_input, token)) = symbol(self.remaining_input) {
            self.parse_success(token, remaining_input);
            self.last_token_was_a_symbol = true;
            return Some(Ok(Token::Symbol(token)))
        }
        self.last_token_was_a_symbol = false;
        if let Ok((remaining_input, token)) = operator_or_bracket(remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(STR_TO_TOKEN[token]))
        }
        if let Ok((remaining_input, token)) = integer(remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Integer(token)))
        }
        let err = SyntaxError::at(self.pos());
        self.remaining_input = b"";
        debug!("{}", err);
        Some(Err(err))
    }
}

struct Parser<'a> {
    lexer: Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn on(input: &'a [u8]) -> Self {
        let lexer = Lexer::for_input(input);
        Parser{lexer}
    }

    fn parse(&mut self) -> Result<Expression<'a>, SyntaxError> {
        let mut next = self.lexer.next().transpose()?;
        self.parse_with(&mut next, 0)
    }

    fn parse_with(
        &mut self,
        mut next: &mut Option<Token<'a>>,
        right_binding_power: usize
    ) -> Result<Expression<'a>, SyntaxError> {
        debug!("parser called with rbp {}", right_binding_power);
        let mut token = *next;
        *next = self.lexer.next().transpose()?;
        let mut left = self.null(token, &mut next)?;
        while right_binding_power < self.left_binding_power(*next)?
        {
            token = *next;
            *next = self.lexer.next().transpose()?;
            left = self.left(token, &mut next, left)?;
        }
        debug!("end parser call with rbp {}", right_binding_power);
        Ok(left)
    }

    fn null(
        &mut self,
        token: Option<Token<'a>>,
        next: &mut Option<Token<'a>>
    ) -> Result<Expression<'a>, SyntaxError> {
        use Expression::*;
        debug!("null called on token {:?}", token);
        if let Some(token) = token {
            match token {
                Token::Symbol(name) => Ok(Symbol(name)),
                Token::Integer(int) => Ok(Integer(int)),
                Token::Plus => {
                    let arg = self.parse_with(next, PREC_UPLUS)?;
                    Ok(Plus(vec![arg]))
                },
                Token::Minus => {
                    let arg = self.parse_with(next, PREC_UMINUS)?;
                    Ok(Minus(vec![arg]))
                },
                Token::Wildcard => {
                    let arg = self.parse_with(next, PREC_WILDCARD)?;
                    if let Symbol(name) = arg {
                        let sym = expression::Symbol(name);
                        Ok(Many0Wildcard(sym))
                    } else {
                        Err(SyntaxError::at(self.pos()))
                    }
                },
                _ => Err(SyntaxError::at(self.pos()))
            }
        } else {
            Ok(Expression::Empty)
        }
    }

    fn left(
        &mut self,
        token: Option<Token<'a>>,
        next: &mut Option<Token<'a>>,
        left: Expression<'a>,
    ) -> Result<Expression<'a>, SyntaxError> {
        debug!("left called on token {:?}", token);
        use Expression::*;
        if let Some(t) = token {
            match t {
                // TODO: code duplication
                // TODO: only accept Many0Wildcard in Sequence
                // left-associative operators with two or more arguments
                Token::Semicolon => {
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Compound(mut args) = left {
                        args.push(right);
                        Ok(Compound(args))
                    } else {
                        Ok(Compound(vec![left, right]))
                    }
                },
                Token::Comma => {
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Sequence(mut args) = left {
                        args.push(right);
                        Ok(Sequence(args))
                    } else {
                        Ok(Sequence(vec![left, right]))
                    }
                },
                Token::Plus => {
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Plus(mut args) = left {
                        args.push(right);
                        Ok(Plus(args))
                    } else {
                        Ok(Plus(vec![left, right]))
                    }
                },
                Token::Times => {
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Times(mut args) = left {
                        args.push(right);
                        Ok(Times(args))
                    } else {
                        Ok(Times(vec![left, right]))
                    }
                },
                Token::Minus => {
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Minus(mut args) = left {
                        args.push(right);
                        Ok(Minus(args))
                    } else {
                        Ok(Minus(vec![left, right]))
                    }
                },
                Token::Divide => {
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Divide(mut args) = left {
                        args.push(right);
                        Ok(Divide(args))
                    } else {
                        Ok(Divide(vec![left, right]))
                    }
                },
                // non-associative operators
                Token::Equals => {
                    let pos = self.pos();
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Equals(_) = right {
                        Err(SyntaxError::at(pos))
                    } else {
                        Ok(Equals(vec![left, right]))
                    }
                },
                Token::Power => {
                    let pos = self.pos();
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Power(_) = right {
                        Err(SyntaxError::at(pos))
                    } else {
                        Ok(Power(vec![left, right]))
                    }
                },
                Token::Dot => {
                    let pos = self.pos();
                    let right_binding_power = self.left_binding_power(token)?;
                    let right = self.parse_with(next, right_binding_power)?;
                    if let Dot(_) = right {
                        Err(SyntaxError::at(pos))
                    } else {
                        Ok(Dot(vec![left, right]))
                    }
                },
                Token::Wildcard => {
                    if let Symbol(name) = left {
                        let sym = expression::Symbol(name);
                        Ok(Wildcard(sym))
                    } else {
                        Err(SyntaxError::at(self.pos()))
                    }
                },
                _ => Err(SyntaxError::at(self.pos()))
            }
        } else {
            Err(SyntaxError::at(self.pos()))
        }
    }

    fn left_binding_power(
        &self,
        token: Option<Token<'a>>
    ) -> Result<usize, SyntaxError>  {
        debug!("look up left binding power of {:?}", token);
        use Token::*;
        if let Some(token) = token {
            match token {
                Symbol(_) => Ok(0),
                Integer(_) => Ok(0),
                Semicolon => Ok(10),
                Comma => Ok(20),
                Equals => Ok(30),
                Plus => Ok(40),
                Minus => Ok(40),
                Times => Ok(50),
                Divide => Ok(50),
                Power => Ok(70),
                Dot => Ok(80),
                Wildcard => Ok(PREC_WILDCARD),
                _ => Err(SyntaxError::at(self.pos()))
            }
        } else {
            Ok(0)
        }
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }
}

const PREC_UPLUS: usize = 60;
const PREC_UMINUS: usize = PREC_UPLUS;
const PREC_WILDCARD: usize = 90;

#[cfg(test)]
mod tests {
    use super::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn tst_lexer() {
        log_init();

        use Token::*;
        let expr =
            "+ 35 - [iπs[a]f]?.q / den(4*a^-3, $a) + ln_(x1,...,x6);
foo = [bar][as^3];
#include- i_cannot_parse_this anymore
".as_bytes();
        let mut p = Lexer::for_input(&expr);
        assert_eq!(p.next(), Some(Ok(Plus)));
        let slice: &[u8] = b"35";
        assert_eq!(p.next(), Some(Ok(Integer(slice))));
        assert_eq!(p.next(), Some(Ok(Minus)));
        let slice: &[u8] = "[iπs[a]f]".as_bytes();
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(Wildcard)));
        assert_eq!(p.next(), Some(Ok(Dot)));
        let slice: &[u8] = b"q";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(Divide)));
        let slice: &[u8] = b"den";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(LeftBracket)));
        let slice: &[u8] = b"4";
        assert_eq!(p.next(), Some(Ok(Integer(slice))));
        assert_eq!(p.next(), Some(Ok(Times)));
        let slice: &[u8] = b"a";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(Power)));
        assert_eq!(p.next(), Some(Ok(Minus)));
        let slice: &[u8] = b"3";
        assert_eq!(p.next(), Some(Ok(Integer(slice))));
        assert_eq!(p.next(), Some(Ok(Comma)));
        let slice: &[u8] = b"$a";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(RightBracket)));
        assert_eq!(p.next(), Some(Ok(Plus)));
        let slice: &[u8] = b"ln_";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(LeftBracket)));
        let slice: &[u8] = b"x1";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(Comma)));
        assert_eq!(p.next(), Some(Ok(Ellipsis)));
        assert_eq!(p.next(), Some(Ok(Comma)));
        let slice: &[u8] = b"x6";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(RightBracket)));
        assert_eq!(p.next(), Some(Ok(Semicolon)));
        let slice: &[u8] = b"foo";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(Equals)));
        let slice: &[u8] = b"[bar]";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(LeftSquareBracket)));
        let slice: &[u8] = b"as";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_eq!(p.next(), Some(Ok(Power)));
        let slice: &[u8] = b"3";
        assert_eq!(p.next(), Some(Ok(Integer(slice))));
        assert_eq!(p.next(), Some(Ok(RightSquareBracket)));
        assert_eq!(p.next(), Some(Ok(Semicolon)));
        assert_matches!(p.next(), Some(Err(_)));

        assert_eq!(p.next(), None);

        let expr: &[u8] = b"+ ";
        let mut p = Lexer::for_input(&expr);
        assert_eq!(p.next(), Some(Ok(Plus)));
        assert_eq!(p.next(), None);
    }

    #[test]
    fn tst_parse_empty() {
        log_init();
        use Expression::*;

        let expr = b"";
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), Empty);
    }

    #[test]
    fn tst_parse_symbols() {
        log_init();
        use Expression::*;

        let expr: &[u8] = "[αs]".as_bytes();
        let symbol = Symbol(expr);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), symbol);

        let expr: &[u8] = " \n[αs]".as_bytes();
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), symbol);

        let expr: &[u8] = " [αs]  ".as_bytes();
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), symbol);

        let expr: &[u8] = b"$ascpn";
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), Symbol(&expr));
    }

    #[test]
    fn tst_parse_int() {
        log_init();
        use Expression::*;

        let expr: &[u8] = b"1294239933299328";
        let int = Integer(expr);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), int);

        let expr: &[u8] = b"  \n\t  1294239933299328  ";
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), int);
    }

    #[test]
    fn tst_parse_unary() {
        log_init();
        use Expression::*;

        let expr: &[u8] = b"+ 1294239933299328";
        let res = Plus(vec![Integer(&expr[2..])]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"-1294239933299328";
        let res = Minus(vec![Integer(&expr[1..])]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"?a";
        let res = Many0Wildcard(expression::Symbol(&expr[1..]));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"?8";
        let mut parser = Parser::on(expr);
        assert_matches!(parser.parse(), Err(_));
    }

    #[test]
    fn tst_parse_binary() {
        log_init();
        use Expression::*;

        let expr: &[u8] = b" a + 1 ";
        let a: &[u8] = b"a";
        let int: &[u8] = b"1";
        let res = Plus(vec![Symbol(a), Integer(int)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 + b";
        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";
        let res = Plus(vec![Symbol(a), Integer(int), Symbol(b)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 - b";
        let res = Minus(vec![Plus(vec![Symbol(a), Integer(int)]), Symbol(b)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a * 1 - b";
        let res = Minus(vec![Times(vec![Symbol(a), Integer(int)]), Symbol(b)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a * 1 ^ b";
        let res = Times(vec![Symbol(a), Power(vec![Integer(int), Symbol(b)])]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);
    }


}
