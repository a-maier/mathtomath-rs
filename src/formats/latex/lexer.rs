use crate::error::{SyntaxError, ErrorKind::*};
use crate::range::Range;

use super::tokens::{BUILTIN, MAX_TOKEN_STR_LEN, Token};

use std::str::from_utf8;
use nom::{
    IResult,
    branch::alt,
    character::complete::{char},
    bytes::complete::{tag, take_while, take_while1, take_until},
    sequence::{delimited, preceded},
};

fn trim_left(i: &[u8]) -> &[u8] {
    let pos = i.iter().position(|b| !b.is_ascii_whitespace());
    if let Some(pos) = pos {
        &i[pos..]
    } else {
        i
    }
}

fn parse_arg(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let i = trim_left(i);
    let mut bytes = i.iter();
    match bytes.next() {
        None => Ok((b"", b"")),
        Some(b'{') => {
            let mut open_braces = 1;
            for (n, byte) in bytes.enumerate() {
                match byte {
                    b'{' => open_braces += 1,
                    b'}' => {
                        open_braces -= 1;
                        if open_braces == 0 {
                            return Ok((&i[n+2..], &i[1..=n]))
                        }
                    },
                    _ => {}
                }
            }
            tag("DON'T KNOW HOW TO CREATE AN IRESULT ERROR OTHERWISE")(b"")
        },
        Some(_) => Ok(reverse(i.split_at(1)))
    }
}

fn integer(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c: u8| c.is_ascii_digit())(i)
}

fn real(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (rest, int) = integer(i).unwrap_or((i, b""));
    let (rest, _) = char('.')(rest)?;
    let (_rest, fract) = integer(rest).unwrap_or((rest, b""));
    let len = int.len() + fract.len() + 1;
    if len == 1 {
        // evil hack: I don't know how to create an error, so just use
        // an arbitrary failing parser
        tag("DON'T KNOW HOW TO CREATE AN IRESULT ERROR OTHERWISE")(b"")
    } else {
        Ok(reverse(i.split_at(len)))
    }
}

pub(crate) fn symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    match i.split_first() {
        Some((c, _)) if c.is_ascii_alphabetic() => Ok(reverse(i.split_at(1))),
        _ => {
            let (rest, c) = preceded(char('\\'), take_while(|c: u8| c.is_ascii_alphabetic()))(i)?;
            if c.starts_with(b"text") {
                parse_arg(rest)
            } else {
                Ok(reverse(i.split_at(c.len() + 1)))
            }
        }
    }
}

fn reverse<T,U>(tuple: (T, U)) -> (U, T) {
    (tuple.1, tuple.0)
}

fn comment(i: &[u8]) -> IResult<&[u8], &[u8]> {
    preceded(char('%'), take_until("\n"))(i)
}

fn ignored_command(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let mut bytes = i.iter();
    if bytes.next() == Some(&b'\\') {
        if let Some(next) = bytes.next() {
            if b" ;<,!".contains(next) {
                return Ok(reverse(i.split_at(2)))
            }
            let rest = &i[2..];
            // TODO: use static set
            let ignored: [&[u8]; 10] = [
                b"notag",
                b"nonumber",
                b"quad",
                b"qquad",
                b"big",
                b"Big",
                b"bigg",
                b"Bigg",
                b"left",
                b"right",
            ];
            for ignored in ignored.iter() {
                if rest.starts_with(ignored) {
                    return Ok(reverse(i.split_at(2 + ignored.len())))
                }
            }
        }
    }
    tag("DON'T KNOW HOW TO CREATE AN IRESULT ERROR OTHERWISE")(b"")
}

fn braced_whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (_, ws) = delimited(char('{'), whitespace, char('}'))(i)?;
    Ok(reverse(i.split_at(ws.len() + 2)))
}

fn whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        braced_whitespace,
        tag("&"),
        comment,
        ignored_command,
        take_while(|u: u8| u.is_ascii_whitespace()),
    ))(i)
}

fn builtin(i: &[u8]) -> Option<(Token<'static>, usize)>  {
    let max_len = std::cmp::min(1 + *MAX_TOKEN_STR_LEN, i.len());
    for token_str_len in (1..max_len).rev() {
        trace!("looking for token with length {}", token_str_len);
        if let Some(val) = BUILTIN.get(&i[..token_str_len]) {
            return Some((Token::Static(*val), token_str_len))
        }
    }
    None
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub(crate) struct Lexer<'a> {
    remaining_input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn for_input(input: &'a [u8]) -> Lexer<'a> {
        Lexer{
            remaining_input: input,
            pos: 0,
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    fn parse_success(&mut self, token: &'a [u8], new_remaining: &'a [u8]) {
        debug!("parsed '{:?}'", from_utf8(token));
        self.pos += token.len();
        self.remaining_input = new_remaining;
    }

    fn next_nospace(&mut self) -> Option<Result<Token<'a>, SyntaxError>> {
        if self.remaining_input.is_empty() {
            return None;
        }
        if let Ok((remaining_input, token)) = real(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Real(token)))
        }
        if let Ok((remaining_input, token)) = integer(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Integer(token)))
        }
        if let Some((token, token_str_len)) = builtin(self.remaining_input) {
            let (token_str, rest) = self.remaining_input.split_at(token_str_len);
            self.parse_success(token_str, rest);
            return Some(Ok(token))
        }
        if let Ok((remaining_input, token)) = symbol(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Symbol(token)))
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
        loop {
            let (remaining_input, ws) = whitespace(self.remaining_input).unwrap();
            if ws.is_empty() { break }
            self.parse_success(ws, remaining_input);
        }
        let old_pos = self.pos;
        if let Some(t) = self.next_nospace() {
            let res = match t {
                Ok(token) => Ok((token, Range{start: old_pos, end: self.pos})),
                Err(err) => Err(err),
            };
            Some(res)
        } else {
            None
        }
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
        use super::super::tokens::StaticToken::*;

        let expr =
            br" + 35\,,\qquad  - \textnormal{is{[a]}f]}_.q {}/ { }\text{den}(4\times a^-3, &a[[1]]) + \ln(x_1,x_6);
% [ this is a comment
\text{foo} =& \langle a^.23\rangle;
";
        let mut p = Lexer::for_input(&*expr);
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        assert_eq!(p.next().unwrap().unwrap().0, Integer(b"35" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Minus));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"is{[a]}f]" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Subscript));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Dot));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"q" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Divide));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"den" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Integer(b"4" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Times));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"a" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Power));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Minus));
        assert_eq!(p.next().unwrap().unwrap().0, Integer(b"3" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"a" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Integer(b"1" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Log));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"x" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Subscript));
        assert_eq!(p.next().unwrap().unwrap().0, Integer(b"1" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"x" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Subscript));
        assert_eq!(p.next().unwrap().unwrap().0, Integer(b"6" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Semicolon));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"foo" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Equal));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftAngleBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(b"a" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Power));
        assert_eq!(p.next().unwrap().unwrap().0, Real(b".23" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightAngleBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Semicolon));
        assert_eq!(p.next(), None);
    }
}
