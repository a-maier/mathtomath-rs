use super::error::{SyntaxError, ErrorKind::*};

use std::str::from_utf8;

use nom::{
    IResult,
    bytes::complete::{is_not, tag, take_while, take_while1, take_until},
    branch::alt,
    character::{
        complete::{char, one_of},
        is_alphabetic, is_alphanumeric, is_digit,
    },
    multi::many0,
    sequence::{delimited, preceded, terminated, tuple},
    combinator::opt,
};

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub(crate) enum Token<'a> {
    Symbol(&'a [u8]),
    Integer(&'a [u8]),
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
}

const STR_TO_TOKEN: phf::Map<&'static [u8], Token<'static>> = phf_map!{
    b"..." => Token::Ellipsis,
    b"(" => Token::LeftBracket,
    b")" => Token::RightBracket,
    b"[" => Token::LeftSquareBracket,
    b"]" => Token::RightSquareBracket,
    // b"{" => Token::LeftBrace,
    // b"}" => Token::RightBrace,
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

fn reverse<T,U>(tuple: (T, U)) -> (U, T) {
    (tuple.1, tuple.0)
}

// only call this at the start of the input or if the last byte was a linebreak
fn comment(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rest, _) = terminated(
        preceded(char('*'), take_until("\n")), char('\n')
    )(i)?;
    let comment_len = i.len() - rest.len();
    Ok(reverse(i.split_at(comment_len)))
}

fn whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rest, space) = take_while(|u: u8| u.is_ascii_whitespace())(i)?;
    if let Some(b'\n') = space.last() {
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
        one_of("*+-/^.?,=;(){}[]")(i).and_then(
            |_| {
                trace!("parsed {}", i[0]);
                Ok(reverse(i.split_at(1)))
            }
        )
    }
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub(crate) struct Lexer<'a> {
    remaining_input: &'a [u8],
    pos: usize,
    last_token_was_a_symbol: bool,
}

impl<'a> Lexer<'a> {
    pub(crate) fn for_input(input: &'a [u8]) -> Lexer<'a> {
        if let Ok((rest, comment)) = comment(input) {
            Lexer{
                remaining_input: rest,
                pos: comment.len(),
                last_token_was_a_symbol: false,
            }
        } else {
            Lexer{
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
        let err = SyntaxError::new(NotAToken, self.pos());
        self.remaining_input = b"";
        debug!("{}", err);
        Some(Err(err))
    }
}

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
            "* this is a comment
 + 35 - [iπs[a]f]?.q / den(4*a^-3, $a) + ln_(x1,...,x6);
* [ this is also a comment
foo = [bar][as^3];
 * [this is
*not a comment]
#include- << parse fails here
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
        assert_eq!(p.next(), Some(Ok(Times)));
        let slice: &[u8] = b"[this is\n*not a comment]";
        assert_eq!(p.next(), Some(Ok(Symbol(slice))));
        assert_matches!(p.next(), Some(Err(_)));

        assert_eq!(p.next(), None);

        let expr: &[u8] = b"+ ";
        let mut p = Lexer::for_input(&expr);
        assert_eq!(p.next(), Some(Ok(Plus)));
        assert_eq!(p.next(), None);
    }
}
