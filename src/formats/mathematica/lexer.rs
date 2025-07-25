use super::tokens::*;
use crate::error::{ErrorKind::*, SyntaxError};
use crate::range::Range;

use nom::Parser;
use nom::{
    IResult,
    branch::alt,
    bytes::complete::{
        escaped, is_not, tag, take_until, take_while, take_while1,
    },
    character::complete::{char, none_of},
    combinator::opt,
    sequence::delimited,
};

fn integer(i: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_ascii_digit())(i)
}

fn real(i: &str) -> IResult<&str, &str> {
    let (rest, int) = integer(i).unwrap_or((i, ""));
    let (rest, _) = char('.')(rest)?;
    let (_rest, fract) = integer(rest).unwrap_or((rest, ""));
    let len = int.len() + fract.len() + 1;
    if len == 1 {
        // evil hack: I don't know how to create an error, so just use
        // an arbitrary failing parser
        tag("WOT")("")
    } else {
        Ok(reverse(i.split_at(len)))
    }
}

fn not_quote(i: &str) -> IResult<&str, &str> {
    escaped(is_not("\"\\"), '\\', none_of(""))(i)
}

fn string(i: &str) -> IResult<&str, &str> {
    delimited(char('"'), opt(not_quote), char('"'))
        .parse(i)
        .map(|(rest, string)| (rest, string.unwrap_or("")))
}

pub(crate) fn symbol(i: &str) -> IResult<&str, &str> {
    let (_rest, (a, b)) = (
        take_while1(|c: char| c.is_alphabetic() || c == '$'),
        take_while(|c: char| c.is_alphanumeric() || c == '$'),
    )
        .parse(i)?;
    Ok(reverse(i.split_at(a.len() + b.len())))
}

fn reverse<T, U>(tuple: (T, U)) -> (U, T) {
    (tuple.1, tuple.0)
}

fn comment(i: &str) -> IResult<&str, &str> {
    let (_rest, (a, b, c)) =
        (tag("(*"), take_until("*)"), tag("*)")).parse(i)?;
    Ok(reverse(i.split_at(a.len() + b.len() + c.len())))
}

fn whitespace(i: &str) -> IResult<&str, &str> {
    alt((comment, take_while(|u: char| u.is_ascii_whitespace()))).parse(i)
}

fn builtin(i: &str) -> Option<(Token<'static>, usize)> {
    let boundaries: Vec<_> = (1..=i.len())
        .filter(|pos| i.is_char_boundary(*pos))
        .take(MAX_TOKEN_STR_LEN)
        .collect(); // can't reverse a filter iterator
    for &token_str_len in boundaries.iter().rev() {
        trace!("looking for token with string length {}", token_str_len);
        if let Some(val) = STR_TO_TOKEN.get(&i[..token_str_len]) {
            return Some((Token::Static(*val), token_str_len));
        }
    }
    None
}

#[derive(Copy, Clone, Default, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub(crate) struct Lexer<'a> {
    remaining_input: &'a str,
    pos: usize,
    open_part_brackets: u64,
}

impl<'a> Lexer<'a> {
    pub(crate) fn for_input(input: &'a str) -> Lexer<'a> {
        Lexer {
            remaining_input: input,
            pos: 0,
            open_part_brackets: 0,
        }
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    fn parse_success(&mut self, token: &'a str, new_remaining: &'a str) {
        debug!("parsed '{}'", token);
        self.pos += token.len();
        self.remaining_input = new_remaining;
    }

    fn next_nospace(&mut self) -> Option<Result<Token<'a>, SyntaxError>> {
        if self.remaining_input.is_empty() {
            return None;
        }
        if let Ok((remaining_input, token)) = symbol(self.remaining_input) {
            self.parse_success(token, remaining_input);
            if let Some(token) = BUILTIN_SYMBOL.get(token) {
                return Some(Ok(Token::Static(*token)));
            } else {
                return Some(Ok(Token::Symbol(token.as_bytes())));
            }
        }
        if let Ok((remaining_input, token)) = real(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Real(token.as_bytes())));
        }
        if let Ok((remaining_input, token)) = integer(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::Integer(token.as_bytes())));
        }
        if let Ok((remaining_input, token)) = string(self.remaining_input) {
            self.parse_success(token, remaining_input);
            return Some(Ok(Token::String(token.as_bytes())));
        }
        if let Some((mut token, mut token_str_len)) =
            builtin(self.remaining_input)
        {
            if token == Token::Static(StaticToken::LeftPart) {
                self.open_part_brackets += 1;
                debug!(
                    "Opening Part brackets, now {}",
                    self.open_part_brackets
                );
            } else if token == Token::Static(StaticToken::RightPart) {
                debug!("{} open Part brackets", self.open_part_brackets);
                if self.open_part_brackets > 0 {
                    self.open_part_brackets -= 1;
                } else {
                    // we are not inside a Part bracket,
                    // so this is actually two closing square brackets
                    // and not a single token
                    token = Token::Static(StaticToken::RightSquareBracket);
                    debug_assert_eq!(token_str_len, 2);
                    token_str_len = 1;
                }
            }
            let (token_str, rest) =
                self.remaining_input.split_at(token_str_len);
            self.parse_success(token_str, rest);
            return Some(Ok(token));
        }
        let err = SyntaxError::new(NotAToken, self.pos());
        self.remaining_input = "";
        debug!("{}", err);
        Some(Err(err))
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Result<(Token<'a>, Range<usize>), SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        trace!("lexer called on {}", self.remaining_input);
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

        use StaticToken::*;
        use Token::*;

        let expr = r#" + 35 - "iπs\"[a]f]"_.q / den[4*a^-3, $a[[1]]] + ln[x1,x6];
(* [ this is a comment
*)
foo = 〈as^.238〉;
"#;
        let mut p = Lexer::for_input(&expr);
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        let slice: &[u8] = b"35";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Subtract));
        let slice: &[u8] = r#"iπs\"[a]f]"#.as_bytes();
        assert_eq!(p.next().unwrap().unwrap().0, String(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Blank));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Dot));
        let slice: &[u8] = b"q";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Divide));
        let slice: &[u8] = b"den";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftSquareBracket));
        let slice: &[u8] = b"4";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Times));
        let slice: &[u8] = b"a";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Power));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Subtract));
        let slice: &[u8] = b"3";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        let slice: &[u8] = b"$a";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftPart));
        let slice: &[u8] = b"1";
        assert_eq!(p.next().unwrap().unwrap().0, Integer(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightPart));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightSquareBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        let slice: &[u8] = b"ln";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftSquareBracket));
        let slice: &[u8] = b"x1";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Comma));
        let slice: &[u8] = b"x6";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightSquareBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(CompoundExpression));
        let slice: &[u8] = b"foo";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Set));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftAngleBracket));
        let slice: &[u8] = b"as";
        assert_eq!(p.next().unwrap().unwrap().0, Symbol(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(Power));
        let slice: &[u8] = b".238";
        assert_eq!(p.next().unwrap().unwrap().0, Real(slice));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightAngleBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(CompoundExpression));
        assert_eq!(p.next(), None);

        let mut p = Lexer::for_input("+ ");
        assert_eq!(p.next().unwrap().unwrap().0, Static(Plus));
        assert_eq!(p.next(), None);
    }

    #[test]
    fn tst_part() {
        log_init();

        use StaticToken::*;
        use Token::*;

        let mut p = Lexer::for_input("[[]]");
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftPart));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightPart));
        assert_eq!(p.next(), None);

        let mut p = Lexer::for_input("[ []]");
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftSquareBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(LeftSquareBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightSquareBracket));
        assert_eq!(p.next().unwrap().unwrap().0, Static(RightSquareBracket));
        assert_eq!(p.next(), None);
    }
}
