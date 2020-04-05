use crate::expression::{self, Expression};
use super::lexer::{Lexer, SyntaxError, Token};

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
                Token::LeftBracket => {
                    let pos = self.pos();
                    let arg = self.parse_with(next, 0)?;
                    if *next == Some(Token::RightBracket) {
                        *next = self.lexer.next().transpose()?;
                        Ok(arg)
                    } else {
                        Err(SyntaxError::at(pos))
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
                Token::LeftBracket => {
                    let pos = self.pos();
                    let head = left;
                    let args = self.parse_with(next, 0)?;
                    if *next == Some(Token::RightBracket) {
                        *next = self.lexer.next().transpose()?;
                        if let Sequence(args) = args {
                            Ok(expression::Function{head, args}.into())
                        } else {
                            let args = vec![args];
                            Ok(expression::Function{head, args}.into())
                        }
                    } else {
                        Err(SyntaxError::at(pos))
                    }
                },
                Token::LeftSquareBracket => {
                    let pos = self.pos();
                    if let Symbol(_) = left {
                        let right = self.parse_with(next, 0)?;
                        if *next == Some(Token::RightSquareBracket) {
                            *next = self.lexer.next().transpose()?;
                            Ok(Coefficient(vec![left, right]))
                        } else {
                            Err(SyntaxError::at(pos))
                        }
                    } else {
                        Err(SyntaxError::at(pos))
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
                RightBracket => Ok(0),
                RightSquareBracket => Ok(0),
                Semicolon => Ok(10),
                Comma => Ok(20),
                Equals => Ok(30),
                Plus => Ok(40),
                Minus => Ok(40),
                Times => Ok(50),
                Divide => Ok(50),
                Power => Ok(70),
                Dot => Ok(80),
                LeftBracket => Ok(90),
                LeftSquareBracket => Ok(100),
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
const PREC_WILDCARD: usize = 110;

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
    fn tst_binary() {
        log_init();
        use Expression::*;

        let expr: &[u8] = b" a + 1 ";
        let a: &[u8] = b"a";
        let int: &[u8] = b"1";
        let res = Plus(vec![Symbol(a), Integer(int)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 + b";
        let b: &[u8] = b"b";
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

    #[test]
    fn tst_brackets() {
        log_init();
        use Expression::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let expr: &[u8] = b" ( a + 1 )";
        let res = Plus(vec![Symbol(a), Integer(int)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + (1 - b)";
        let res = Plus(vec![Symbol(a), Minus(vec![Integer(int), Symbol(b)])]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" (a * 1) ^ b";
        let res = Power(vec![Times(vec![Symbol(a), Integer(int)]), Symbol(b)]);
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
        let res = Coefficient(vec![Symbol(a), Integer(int)]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" b^a[1]";
        let res = Power(vec![Symbol(b), Coefficient(vec![Symbol(a), Integer(int)])]);
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

        let fun = expression::Function{head: Symbol(a), args: vec![Integer(int)]};
        let expr: &[u8] = b"a(1)";
        let res = fun.clone().into();
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" b^a(1)";
        let res = Power(vec![Symbol(b), fun.into()]);
        let mut parser = Parser::on(expr);
        assert_eq!(parser.parse().unwrap(), res);

    }
}
