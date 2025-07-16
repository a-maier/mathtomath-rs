use crate::error::{ErrorKind::*, SyntaxError};
use crate::range::Range;

use super::tokens::{
    BUILTIN, BUILTIN_BACKSLASHED, MAX_TOKEN_STR_LEN, StaticToken, Token,
};

use nom::Parser;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{tag, take, take_until, take_while, take_while1},
    character::complete::char,
    sequence::{delimited, preceded, separated_pair},
};
use std::str::from_utf8;

fn trim_left(i: &[u8]) -> &[u8] {
    let pos = i.iter().position(|b| !b.is_ascii_whitespace());
    if let Some(pos) = pos { &i[pos..] } else { i }
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
                            return Ok((&i[n + 2..], &i[1..=n]));
                        }
                    }
                    _ => {}
                }
            }
            tag("DON'T KNOW HOW TO CREATE AN IRESULT ERROR OTHERWISE")(b"")
        }
        Some(_) => Ok(reverse(i.split_at(1))),
    }
}

fn integer(i: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while1(|c: u8| c.is_ascii_digit())(i)
}

fn real(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (_, (ipart, fpart)) =
        separated_pair(integer, char('.'), integer).parse(i)?;
    Ok(reverse(i.split_at(ipart.len() + fpart.len() + 1)))
}

pub(crate) fn symbol(i: &[u8]) -> IResult<&[u8], &[u8]> {
    match i.split_first() {
        Some((c, _)) if c.is_ascii_alphabetic() => Ok(reverse(i.split_at(1))),
        _ => {
            let (rest, c) = preceded(
                char('\\'),
                take_while(|c: u8| c.is_ascii_alphabetic()),
            )
            .parse(i)?;
            if c.starts_with(b"text") {
                parse_arg(rest)
            } else {
                Ok(reverse(i.split_at(c.len() + 1)))
            }
        }
    }
}

fn reverse<T, U>(tuple: (T, U)) -> (U, T) {
    (tuple.1, tuple.0)
}

fn comment(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let rem = std::cmp::max(i.len(), 1) - 1;
    let (_rest, comment) =
        preceded(char('%'), alt((take_until("\n"), take(rem)))).parse(i)?;
    Ok(reverse(i.split_at(comment.len() + 1)))
}

lazy_static! {
    pub(crate) static ref MAX_WS_LEN: usize =
        BUILTIN_WS.iter().map(|k| k.len()).max().unwrap();
}

pub(crate) const BUILTIN_WS: phf::Set<&'static [u8]> = phf_set! {
    b"displaybreak",
    b"notag",
    b"nonumber",
    b"quad",
    b"qquad",
    b"null",
};

lazy_static! {
    pub(crate) static ref MAX_BRACKET_SIZE_LEN: usize =
        BRACKET_SIZES.iter().map(|k| k.len()).max().unwrap();
}

pub(crate) const BRACKET_SIZES: phf::Set<&'static [u8]> = phf_set! {
    b"big",
    b"Big",
    b"bigg",
    b"Bigg",
    b"bigl",
    b"Bigl",
    b"biggl",
    b"Biggl",
    b"bigr",
    b"Bigr",
    b"biggr",
    b"Biggr",
    b"left",
    b"right",
};

fn ignored_command(i: &[u8]) -> IResult<&[u8], &[u8]> {
    use regex::bytes::Regex;
    lazy_static! {
        static ref DISPLAYBREAK_ARG: Regex = Regex::new(r"^\[[0-4]\]").unwrap();
    }

    let mut bytes = i.iter();
    if bytes.next() == Some(&b'\\') {
        let rest = &i[1..];
        match bytes.next() {
            Some(next) if br" ;<,!\".contains(next) => {
                return Ok(reverse(i.split_at(2)));
            }
            Some(_) => {
                let (_, tag) =
                    take_while1(|u: u8| u.is_ascii_alphabetic())(rest)?;
                if BUILTIN_WS.contains(tag) {
                    let (tag, rest) = i.split_at(1 + tag.len());
                    if tag == br"\displaybreak"
                        && DISPLAYBREAK_ARG.is_match(rest)
                    {
                        return Ok(reverse(i.split_at(3 + tag.len())));
                    } else {
                        return Ok((rest, tag));
                    }
                } else if BRACKET_SIZES.contains(tag) {
                    let rest = &rest[tag.len()..];
                    let (rest, ws) =
                        whitespace(rest).unwrap_or((rest, b"" as _));
                    let split_pos = if rest.starts_with(b".") {
                        1 + tag.len() + ws.len() + 1
                    } else {
                        1 + tag.len() + ws.len()
                    };
                    return Ok(reverse(i.split_at(split_pos)));
                }
            }
            None => {}
        }
    }
    tag("DON'T KNOW HOW TO CREATE AN IRESULT ERROR OTHERWISE")(b"")
}

fn braced_whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    let (_, ws) = delimited(char('{'), whitespace, char('}')).parse(i)?;
    Ok(reverse(i.split_at(ws.len() + 2)))
}

fn whitespace(i: &[u8]) -> IResult<&[u8], &[u8]> {
    alt((
        braced_whitespace,
        tag("&"),
        tag("~"),
        comment,
        ignored_command,
        take_while(|u: u8| u.is_ascii_whitespace()),
    ))
    .parse(i)
}

fn builtin(i: &[u8]) -> Option<(Token<'static>, usize)> {
    if let Some((b'\\', rest)) = i.split_first() {
        let wtf: IResult<&[u8], &[u8]> =
            take_while(|u: u8| u.is_ascii_alphabetic())(rest);
        let (_rest, cmd) = wtf.unwrap();
        if let Some(val) = BUILTIN_BACKSLASHED.get(cmd) {
            return Some((Token::Static(*val), 1 + cmd.len()));
        } else if let Some(b'*') = rest.first() {
            return Some((Token::Static(StaticToken::Times), 2));
        }
    }
    let max_len = 1 + std::cmp::min(*MAX_TOKEN_STR_LEN, i.len());
    for token_str_len in (1..max_len).rev() {
        trace!("looking for token with length {}", token_str_len);
        if let Some(val) = BUILTIN.get(&i[..token_str_len]) {
            return Some((Token::Static(*val), token_str_len));
        }
    }
    None
}

#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct Lexer<'a> {
    remaining_input: &'a [u8],
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn for_input(input: &'a [u8]) -> Lexer<'a> {
        Lexer {
            remaining_input: input,
            pos: 0,
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn skip_bytes(&mut self, pos: usize) {
        let pos = std::cmp::min(pos, self.remaining_input.len());
        self.remaining_input = &self.remaining_input[pos..];
        self.pos += pos
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
            return Some(Ok(Token::Real(token)));
        }
        if let Ok((remaining_input, token)) = integer(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Integer(token)));
        }
        if let Some((token, token_str_len)) = builtin(self.remaining_input) {
            let (token_str, rest) =
                self.remaining_input.split_at(token_str_len);
            self.parse_success(token_str, rest);
            return Some(Ok(token));
        }
        if let Ok((remaining_input, token)) = symbol(self.remaining_input) {
            // dirty fix in case not everything that was digested is actually
            // part of the token (e.g. for \text{token})
            if remaining_input.len() + token.len() < self.remaining_input.len()
            {
                self.pos += self.remaining_input.len()
                    - remaining_input.len()
                    - token.len();
            }
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Symbol(token)));
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
            let (remaining_input, ws) =
                whitespace(self.remaining_input).unwrap();
            if ws.is_empty() {
                break;
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn tst_lexer() {
        log_init();

        use super::super::tokens::StaticToken::*;
        use Token::*;

        let expr =
            br" + 35\,,\qquad  - \textnormal{is{[a]}f]}_.q {}/ { }\text{den}(4\times a^-3, &a[[1]]) + \ln(x_1,x_6);
% [ this is a comment
\text{foo} =& \langle a^0.23\rangle;
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
        assert_eq!(p.next().unwrap().unwrap().0, Real(b"0.23" as _));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightAngleBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Semicolon));
        assert_eq!(p.next(), None);
    }

    #[test]
    fn tst_comment() {
        log_init();

        let expr = br"";
        let mut p = Lexer::for_input(&*expr);
        assert_eq!(p.next(), None);

        let expr = br"% \nu \Bigg[ \frac{1}{\varepsilon}";
        let mut p = Lexer::for_input(&*expr);
        assert_eq!(p.next(), None);
    }
}
