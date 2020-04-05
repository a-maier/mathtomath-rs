use super::lexer::{Lexer, Token};
use crate::error::{SyntaxError, ErrorKind::*};
use crate::expression::{self, Expression};

pub fn parse<'a>(input: &'a [u8]) -> Result<Expression<'a>, SyntaxError> {
    let mut parser = Parser::on(input);
    parser.parse()
}

struct Parser<'a> {
    lexer: Lexer<'a>,
}

const LEFT_TOKENS: &'static str =
    "'+', '-' , '*', '/', '^', '.', '=', ',', ';', '?', '(', or '['";

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
        while right_binding_power < left_binding_power(*next) {
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
                Token::Ellipsis => Ok(Ellipsis),
                Token::Plus => {
                    let arg = self.parse_with(next, PREC_UPLUS)?;
                    Ok(UPlus(Box::new(arg)))
                },
                Token::Minus => {
                    let arg = self.parse_with(next, PREC_UMINUS)?;
                    Ok(UMinus(Box::new(arg)))
                },
                Token::Wildcard => {
                    let arg = self.parse_with(next, PREC_WILDCARD)?;
                    if let Symbol(name) = arg {
                        let sym = expression::Symbol(name);
                        Ok(Many0Wildcard(sym))
                    } else {
                        Err(SyntaxError::new(BadWildcardArgument("?"), self.pos()))
                    }
                },
                Token::LeftBracket => {
                    let pos = self.pos();
                    let arg = self.parse_with(next, 0)?;
                    if *next == Some(Token::RightBracket) {
                        *next = self.lexer.next().transpose()?;
                        Ok(arg)
                    } else {
                        Err(SyntaxError::new(Unmatched("("), pos))
                    }
                },
                _ => Err(SyntaxError::new(
                    ExpectNull("a symbol, an integer number, '+', '-', '?', '(', or '...'"),
                    self.pos()
                ))
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
        let pos = self.pos();
        let right_binding_power = left_binding_power(token);
        let right = self.parse_with(next, right_binding_power)?;
        if let Some(t) = token {
            match t {
                //TODO: code duplication
                Token::Semicolon => Ok(Compound(Box::new((left, right)))),
                Token::Comma => Ok(Sequence(Box::new((left, right)))),
                Token::Plus => Ok(Plus(Box::new((left, right)))),
                Token::Times => Ok(Times(Box::new((left, right)))),
                Token::Minus => Ok(Minus(Box::new((left, right)))),
                Token::Divide => Ok(Divide(Box::new((left, right)))),
                Token::Equals => Ok(Equals(Box::new((left, right)))),
                Token::Power => Ok(Power(Box::new((left, right)))),
                Token::Dot => Ok(Dot(Box::new((left, right)))),
                Token::Wildcard => {
                    if let Symbol(name) = left {
                        let sym = expression::Symbol(name);
                        Ok(Wildcard(sym))
                    } else {
                        Err(SyntaxError::new(BadWildcardArgument("?"), pos))
                    }
                },
                Token::LeftBracket => {
                    if *next == Some(Token::RightBracket) {
                        *next = self.lexer.next().transpose()?;
                        Ok(Function(Box::new((left, right))))
                    } else {
                        Err(SyntaxError::new(Unmatched("("), pos))
                    }
                },
                Token::LeftSquareBracket => {
                    if *next == Some(Token::RightSquareBracket) {
                        *next = self.lexer.next().transpose()?;
                        Ok(Coefficient(Box::new((left, right))))
                    } else {
                        Err(SyntaxError::new(Unmatched("["), pos))
                    }
                },
                _ => Err(SyntaxError::new(ExpectLeft(LEFT_TOKENS), pos))
            }
        } else {
            Err(SyntaxError::new(EarlyEOF(LEFT_TOKENS), pos))
        }
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }
}

const PREC_UPLUS: usize = 60;
const PREC_UMINUS: usize = PREC_UPLUS;
const PREC_WILDCARD: usize = 110;

fn left_binding_power<'a>(token: Option<Token<'a>>) -> usize {
    debug!("look up left binding power of {:?}", token);
    use Token::*;
    if let Some(token) = token {
        match token {
            Symbol(_) => 0,
            Integer(_) => 0,
            Ellipsis => 0,
            RightBracket => 0,
            RightSquareBracket => 0,
            Semicolon => 10,
            Comma => 20,
            Equals => 30,
            Plus => 40,
            Minus => 40,
            Times => 50,
            Divide => 50,
            Power => 70,
            Dot => 80,
            LeftBracket => 90,
            LeftSquareBracket => 100,
            Wildcard => PREC_WILDCARD,
        }
    } else {
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn tst_empty() {
        log_init();
        use Expression::*;

        let expr = b"";
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), Empty);
    }

    #[test]
    fn tst_symbols() {
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
    fn tst_int() {
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
    fn tst_unary() {
        log_init();
        use Expression::*;

        let expr: &[u8] = b"+ 1294239933299328";
        let res = UPlus(Box::new(Integer(&expr[2..])));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"-1294239933299328";
        let res = UMinus(Box::new(Integer(&expr[1..])));
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
    fn tst_binary() {
        log_init();
        use Expression::*;

        let expr: &[u8] = b" a + 1 ";
        let a: &[u8] = b"a";
        let int: &[u8] = b"1";
        let res = Plus(Box::new((Symbol(a), Integer(int))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 + b";
        let b: &[u8] = b"b";
        let res = Plus(Box::new((Plus(Box::new((Symbol(a), Integer(int)))), Symbol(b))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"a,...,b";
        let res = Sequence(Box::new((Sequence(Box::new((Symbol(a), Ellipsis))), Symbol(b))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 - b";
        let res = Minus(Box::new((Plus(Box::new((Symbol(a), Integer(int)))), Symbol(b))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a * 1 - b";
        let res = Minus(Box::new((Times(Box::new((Symbol(a), Integer(int)))), Symbol(b))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a * 1 ^ b";
        let res = Times(Box::new((Symbol(a), Power(Box::new((Integer(int), Symbol(b)))))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);
    }

    #[test]
    fn tst_brackets() {
        log_init();
        use Expression::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let expr: &[u8] = b" ( a + 1 )";
        let res = Plus(Box::new((Symbol(a), Integer(int))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + (1 - b)";
        let res = Plus(Box::new((Symbol(a), Minus(Box::new((Integer(int), Symbol(b)))))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" (a * 1) ^ b";
        let res = Power(Box::new((Times(Box::new((Symbol(a), Integer(int)))), Symbol(b))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);
    }

    #[test]
    fn tst_square_brackets() {
        log_init();
        use Expression::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let expr: &[u8] = b"a[1]";
        let res = Coefficient(Box::new((Symbol(a), Integer(int))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" b^a[1]";
        let res = Power(Box::new((Symbol(b), Coefficient(Box::new((Symbol(a), Integer(int)))))));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

    }

    #[test]
    fn tst_function() {
        log_init();
        use Expression::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let fun = Function(Box::new((Symbol(a), Integer(int))));
        let expr: &[u8] = b"a(1)";
        let res = fun.clone().into();
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" b^a(1)";
        let res = Power(Box::new((Symbol(b), fun.into())));
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

    }
}
