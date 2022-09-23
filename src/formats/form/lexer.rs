use crate::error::{ErrorKind::*, SyntaxError};
use crate::range::Range;

use std::str::from_utf8;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_until, take_while, take_while1},
    character::{
        complete::{char, one_of},
        is_alphabetic, is_alphanumeric, is_digit,
    },
    combinator::opt,
    multi::{many0, many1},
    sequence::{delimited, preceded, terminated, tuple},
    IResult,
};

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum Token<'a> {
    Symbol(&'a [u8]),
    Integer(&'a [u8]),
    Static(StaticToken),
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) enum StaticToken {
    Ellipsis,
    LeftBracket,
    RightBracket,
    LeftSquareBracket,
    RightSquareBracket,
    // LeftBrace,
    // RightBrace,
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
    I,
    Pi,
    Log,
    Exp,
    Sign,
    Sin,
    Cos,
    Tan,
    Sinh,
    Cosh,
    Tanh,
    ASin,
    ACos,
    ATan,
    ASinh,
    ACosh,
    ATanh,
    Sqrt,
}

const STR_TO_TOKEN: phf::Map<&'static [u8], StaticToken> = phf_map! {
    b"..." => StaticToken::Ellipsis,
    b"(" => StaticToken::LeftBracket,
    b")" => StaticToken::RightBracket,
    b"[" => StaticToken::LeftSquareBracket,
    b"]" => StaticToken::RightSquareBracket,
    // b"{" => StaticToken::LeftBrace,
    // b"}" => StaticToken::RightBrace,
    b"+" => StaticToken::Plus,
    b"-" => StaticToken::Minus,
    b"*" => StaticToken::Times,
    b"/" => StaticToken::Divide,
    b"^" => StaticToken::Power,
    b"." => StaticToken::Dot,
    b"=" => StaticToken::Equals,
    b"?" => StaticToken::Wildcard,
    b"," => StaticToken::Comma,
    b";" => StaticToken::Semicolon,
};

fn integer(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(is_digit)(i)
}

fn word_symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let maybe_dollar = opt(char('$'));
    let letters = take_while1(is_alphabetic);
    let rest = take_while(is_alphanumeric);
    let maybe_underscore = opt(char('_'));
    let mut word_symbol =
        tuple((maybe_dollar, letters, rest, maybe_underscore));

    let (_, (maybe_dollar, letters, alnum, maybe_underscore)) = word_symbol(i)?;
    let dollar_len = if maybe_dollar.is_some() { 1 } else { 0 };
    let underscore_len = if maybe_underscore.is_some() { 1 } else { 0 };
    let len = dollar_len + letters.len() + alnum.len() + underscore_len;
    let (symbol, rest) = i.split_at(len);
    trace!("parsed symbol '{}'", from_utf8(symbol).unwrap());
    Ok((rest, symbol))
}

fn bracket_symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut symbol = delimited(
        char('['),
        many0(alt((is_not("[]"), bracket_symbol))),
        char(']'),
    );
    let (_rest, symbol) = symbol(i)?;
    let symbol_len = symbol.into_iter().map(|s| s.len()).sum::<usize>() + 2;
    let (symbol, rest) = i.split_at(symbol_len);
    trace!("parsed symbol '{}'", from_utf8(symbol).unwrap());
    Ok((rest, symbol))
}

pub(crate) fn symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((word_symbol, bracket_symbol))(i)
}

fn ellipsis(i: &[u8]) -> IResult<&[u8], &[u8]> {
    tag("...")(i)
}

fn reverse<T, U>(tuple: (T, U)) -> (U, T) {
    (tuple.1, tuple.0)
}

// only call this at the start of the input or if the last byte was a linebreak
fn comment(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rest, _) = many1(terminated(
        preceded(char('*'), take_until("\n")),
        char('\n'),
    ))(i)?;
    let comment_len = i.len() - rest.len();
    Ok(reverse(i.split_at(comment_len)))
}

fn whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rest, space) = take_while(|u: u8| u.is_ascii_whitespace())(i)?;
    if Some(&b'\n') == space.last() {
        if let Ok((rest, _)) = comment(rest) {
            let total_len = if let Ok((rest, _)) = whitespace(rest) {
                i.len() - rest.len()
            } else {
                i.len() - rest.len()
            };
            return Ok(reverse(i.split_at(total_len)));
        }
    }
    Ok((rest, space))
}

fn operator_or_bracket(i: &[u8]) -> IResult<&[u8], &[u8]> {
    if let Ok(result) = ellipsis(i) {
        trace!("parsed ellipsis");
        Ok(result)
    } else {
        // we have to change the return type from char to &[u8]
        // otherwise a simple alt combinator would do
        one_of("*+-/^.?,=;(){}[]")(i).map(|_| {
            trace!("parsed {}", i[0]);
            reverse(i.split_at(1))
        })
    }
}

#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct Lexer<'a> {
    remaining_input: &'a [u8],
    pos: usize,
    last_token_was_a_symbol: bool,
}

impl<'a> Lexer<'a> {
    pub(crate) fn for_input(input: &'a [u8]) -> Lexer<'a> {
        if let Ok((rest, comment)) = comment(input) {
            Lexer {
                remaining_input: rest,
                pos: comment.len(),
                last_token_was_a_symbol: false,
            }
        } else {
            Lexer {
                remaining_input: input,
                pos: 0,
                last_token_was_a_symbol: false,
            }
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    fn parse_success(&mut self, token: &'a [u8], new_remaining: &'a [u8]) {
        debug!("parsed '{}'", from_utf8(token).unwrap());
        self.pos += token.len();
        self.remaining_input = new_remaining;
    }

    fn next_nospace(&mut self) -> Option<Result<Token<'a>, SyntaxError>> {
        use StaticToken::*;
        use Token::*;
        if self.remaining_input.is_empty() {
            return None;
        }
        // a square bracket directly following a symbol indicates the cofficient
        // of some expression, not a new symbol
        if self.last_token_was_a_symbol
            && self.remaining_input.first() == Some(&b'[')
        {
            self.last_token_was_a_symbol = false;
            let (token, remaining_input) = self.remaining_input.split_at(1);
            self.parse_success(token, remaining_input);
            return Some(Ok(Static(LeftSquareBracket)));
        }
        if let Ok((remaining_input, token)) = symbol(self.remaining_input) {
            self.parse_success(token, remaining_input);
            if let Some(token) = BUILTIN_SYMBOL.get(token) {
                return Some(Ok(Token::Static(*token)));
            } else {
                self.last_token_was_a_symbol = true;
                return Some(Ok(Token::Symbol(token)));
            }
        }
        self.last_token_was_a_symbol = false;
        if let Ok((remaining_input, token)) =
            operator_or_bracket(self.remaining_input)
        {
            self.parse_success(token, remaining_input);
            return Some(Ok(Static(STR_TO_TOKEN[token])));
        }
        if let Ok((remaining_input, token)) = integer(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Integer(token)));
        }
        let err = SyntaxError::new(NotAToken, self.pos());
        self.remaining_input = b"";
        debug!("{}", err);
        Some(Err(err))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(Token<'a>, Range<usize>), SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        trace!("lexer called on {:?}", from_utf8(self.remaining_input));
        let (remaining_input, ws) = whitespace(self.remaining_input).unwrap();
        if !ws.is_empty() {
            self.parse_success(ws, remaining_input);
        }
        let old_pos = self.pos;
        if let Some(t) = self.next_nospace() {
            let res = match t {
                Ok(token) => Ok((
                    token,
                    Range {
                        start: old_pos,
                        end: self.pos,
                    },
                )),
                Err(err) => Err(err),
            };
            Some(res)
        } else {
            None
        }
    }
}

pub(crate) const BUILTIN_SYMBOL: phf::Map<&'static [u8], StaticToken> = phf_map! {
    b"i_" => StaticToken::I,
    b"pi_" => StaticToken::Pi,
    b"ln_" => StaticToken::Log,
    b"exp_" => StaticToken::Exp,
    b"sig_" => StaticToken::Sign,
    b"sin_" => StaticToken::Sin,
    b"cos_" => StaticToken::Cos,
    b"tan_" => StaticToken::Tan,
    b"sinh_" => StaticToken::Sinh,
    b"cosh_" => StaticToken::Cosh,
    b"tanh_" => StaticToken::Tanh,
    b"asin_" => StaticToken::ASin,
    b"acos_" => StaticToken::ACos,
    b"atan_" => StaticToken::ATan,
    b"asinh_" => StaticToken::ASinh,
    b"acosh_" => StaticToken::ACosh,
    b"atanh_" => StaticToken::ATanh,
    b"sqrt_" => StaticToken::Sqrt,
};

#[cfg(test)]
mod tests {
    use super::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn tst_lexer() {
        log_init();

        use StaticToken::*;
        use Token::*;
        let expr = "* this is a comment
 + 35 - [iπs[a]f]?.q / den(4*a^-3, $a) + ln_(x1,...,x6);
* [ this is also a comment
foo = [bar][as^3];
 * [this is
*not a comment]
#include- << parse fails here
"
        .as_bytes();
        let mut p = Lexer::for_input(&expr);
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        let slice: &[u8] = b"35";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Minus));
        let slice: &[u8] = "[iπs[a]f]".as_bytes();
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Wildcard));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Dot));
        let slice: &[u8] = b"q";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Divide));
        let slice: &[u8] = b"den";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftBracket));
        let slice: &[u8] = b"4";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Times));
        let slice: &[u8] = b"a";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Power));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Minus));
        let slice: &[u8] = b"3";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        let slice: &[u8] = b"$a";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Log));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftBracket));
        let slice: &[u8] = b"x1";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Ellipsis));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        let slice: &[u8] = b"x6";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Semicolon));
        let slice: &[u8] = b"foo";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Equals));
        let slice: &[u8] = b"[bar]";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftSquareBracket));
        let slice: &[u8] = b"as";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Power));
        let slice: &[u8] = b"3";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightSquareBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Semicolon));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Times));
        let slice: &[u8] = b"[this is\n*not a comment]";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert!(matches!(p.next().unwrap(), Err(_)));

        assert_eq!(p.next(), None);

        let expr: &[u8] = b"+ ";
        let mut p = Lexer::for_input(&expr);
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        assert_eq!(p.next(), None);
    }

    #[test]
    fn tst_comment() {
        log_init();

        let expr = b"* this is a
* multiline comment
* with an empty line in the middle
*

****
";
        let mut p = Lexer::for_input(expr as _);
        assert_eq!(p.next(), None);
    }
}
