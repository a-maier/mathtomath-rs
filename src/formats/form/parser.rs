// TODO: unify parsers and offload everything that's format-dependent into grammar
use super::grammar::*;
use super::lexer::{Lexer, Token, StaticToken};
use crate::arity::Arity;
use crate::error::{SyntaxError, ErrorKind::*};
use crate::expression::*;

pub fn parse<'a>(input: &'a [u8]) -> Result<Expression<'a>, SyntaxError> {
    let mut parser = Parser::new(input);
    parser.parse()
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
}

const LEFT_TOKENS: &'static str =
    "'+', '-' , '*', '/', '^', '.', '=', ',', ';', '?', '(', or '['";

impl<'a> Parser<'a> {
    fn new(input: &'a [u8]) -> Self {
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
        right_binding_power: u32
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
        use NullaryOp::*;
        debug!("null called on token {:?}", token);
        if let Some(token) = token {
            match token {
                Token::Symbol(name) => Ok(Nullary(Symbol(name))),
                Token::Integer(int) => Ok(Nullary(Integer(int))),
                Token::Static(s) => {
                    match NULL_ARITY.get(&s) {
                        Some(Arity::Nullary) => Ok(Nullary(TOKEN_EXPRESSION[&s].clone())),
                        Some(Arity::Unary) => if let Some(closing) = CLOSING_BRACKET.get(&s) {
                            // this is actually a bracket
                            let pos = self.pos();
                            let arg = self.parse_with(next, 0)?;
                            if *next == Some(Token::Static(*closing)) {
                                *next = self.lexer.next().transpose()?;
                                Ok(bracket_to_expr(s, arg))
                            } else {
                                Err(SyntaxError::new(Unmatched(""), pos))
                            }
                        } else {
                            // standard unary prefix operator
                            // TODO: differentiate between prefix and infix/postfix precedence
                            let prec = left_binding_power(Some(token));
                            let arg = self.parse_with(next, prec)?;
                            Ok(prefix_op_to_expr(s, arg))
                        },
                        Some(arity) => unreachable!(
                            "Internal error: {:?} has NULL_ARITY {:?}", s, arity
                        ),
                        None => Err(SyntaxError::new(
                            ExpectNull("an atom, a unary prefix operator, or a bracket"),
                            self.pos()
                        )),
                    }
                }
            }
        } else {
            Ok(Expression::Nullary(NullaryOp::Empty))
        }
    }

    fn left(
        &mut self,
        token: Option<Token<'a>>,
        next: &mut Option<Token<'a>>,
        left: Expression<'a>,
    ) -> Result<Expression<'a>, SyntaxError> {
        debug!("left called on token {:?}", token);
        let pos = self.pos();
        if let Some(Token::Static(s)) = token {
            use Arity::*;
            match LEFT_ARITY.get(&s) {
                Some(Unary) => Ok(postfix_op_to_expr(s, left)),
                Some(Binary) => {
                    let right_binding_power = left_binding_power(token);
                    let right = self.parse_with(next, right_binding_power)?;
                    Ok(binary_op_to_expr(s, left, right))
                },
                Some(Function) => {
                    let right = self.parse_with(next, 0)?;
                    if *next == Some(Token::Static(CLOSING_BRACKET[&s])) {
                        *next = self.lexer.next().transpose()?;
                        Ok(function_to_expr(s, left, right))
                    } else {
                        Err(SyntaxError::new(Unmatched(""), pos))
                    }
                },
                Some(arity) => unreachable!(
                    "Internal error: {:?} has LEFT_ARITY {:?}", s, arity
                ),
                None => Err(SyntaxError::new(ExpectLeft(LEFT_TOKENS), pos))
            }
        } else {
            Err(SyntaxError::new(EarlyEOF(LEFT_TOKENS), pos))
        }
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }
}

fn left_binding_power<'a>(token: Option<Token<'a>>) -> u32 {
    debug!("look up left binding power of {:?}", token);
    use Token::*;
    if let Some(token) = token {
        match token {
            Symbol(_) => PREC_SYMBOL,
            Integer(_) => PREC_INTEGER,
            Static(token) => TOKEN_PREC[&token],
        }
    } else {
        0
    }
}

lazy_static! {
    pub(crate) static ref TOKEN_PREC: std::collections::HashMap<StaticToken, u32> = hashmap!{
        StaticToken::RightBracket => PREC_RIGHT_BRACKET,
        StaticToken::RightSquareBracket => PREC_RIGHT_SQUARE_BRACKET,
        StaticToken::Ellipsis => PREC_ELLIPSIS,
        StaticToken::Semicolon => PREC_SEMICOLON,
        StaticToken::Comma => PREC_COMMA,
        StaticToken::Equals => PREC_EQUALS,
        StaticToken::Plus => PREC_PLUS,
        StaticToken::Minus => PREC_MINUS,
        StaticToken::Times => PREC_TIMES,
        StaticToken::Divide => PREC_DIVIDE,
        StaticToken::Power => PREC_POWER,
        StaticToken::Dot => PREC_DOT,
        StaticToken::LeftBracket => PREC_LEFT_BRACKET,
        StaticToken::LeftSquareBracket => PREC_LEFT_SQUARE_BRACKET,
        StaticToken::Wildcard => PREC_WILDCARD,
        StaticToken::Log => PREC_SYMBOL,
        StaticToken::Exp => PREC_SYMBOL,
        StaticToken::Sin => PREC_SYMBOL,
        StaticToken::Cos => PREC_SYMBOL,
        StaticToken::Tan => PREC_SYMBOL,
        StaticToken::Sinh => PREC_SYMBOL,
        StaticToken::Cosh => PREC_SYMBOL,
        StaticToken::Tanh => PREC_SYMBOL,
        StaticToken::ASin => PREC_SYMBOL,
        StaticToken::ACos => PREC_SYMBOL,
        StaticToken::ATan => PREC_SYMBOL,
        StaticToken::ASinh => PREC_SYMBOL,
        StaticToken::ACosh => PREC_SYMBOL,
        StaticToken::ATanh => PREC_SYMBOL,
    };
}


#[cfg(test)]
mod tests {
    use super::*;

    use crate::expression::{Expression, NullaryOp, UnaryOp, BinaryOp};

    fn log_init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn tst_empty() {
        log_init();
        use Expression::*;
        use NullaryOp::*;

        let expr = b"";
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), Nullary(Empty));
    }

    #[test]
    fn tst_symbols() {
        log_init();
        use Expression::*;
        use NullaryOp::*;

        let expr: &[u8] = "[αs]".as_bytes();
        let symbol = Nullary(Symbol(expr));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), symbol);

        let expr: &[u8] = " \n[αs]".as_bytes();
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), symbol);

        let expr: &[u8] = " [αs]  ".as_bytes();
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), symbol);

        let expr: &[u8] = b"$ascpn";
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), Nullary(Symbol(&expr)));
    }

    #[test]
    fn tst_int() {
        log_init();
        use Expression::*;
        use NullaryOp::*;

        let expr: &[u8] = b"1294239933299328";
        let int = Nullary(Integer(expr));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), int);

        let expr: &[u8] = b"  \n\t  1294239933299328  ";
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), int);
    }

    #[test]
    fn tst_unary() {
        log_init();
        use Expression::*;
        use NullaryOp::*;
        use UnaryOp::*;

        let expr: &[u8] = b"+ 1294239933299328";
        let res = Unary(UPlus, Box::new(Nullary(Integer(&expr[2..]))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"-1294239933299328";
        let res = Unary(UMinus, Box::new(Nullary(Integer(&expr[1..]))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"?a";
        let res = Unary(Many0Wildcard, Box::new(Nullary(Symbol(&expr[1..]))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

    }

    #[test]
    fn tst_binary() {
        log_init();
        use Expression::*;
        use NullaryOp::*;
        use BinaryOp::*;

        let expr: &[u8] = b" a + 1 ";
        let a: &[u8] = b"a";
        let int: &[u8] = b"1";
        let res = Binary(Plus, Box::new((Nullary(Symbol(a)), Nullary(Integer(int)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 + b";
        let b: &[u8] = b"b";
        let res = Binary(Plus, Box::new((Binary(Plus, Box::new((Nullary(Symbol(a)), Nullary(Integer(int))))), Nullary(Symbol(b)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b"a,...,b";
        let res = Binary(Sequence, Box::new((Binary(Sequence, Box::new((Nullary(Symbol(a)), Nullary(Ellipsis)))), Nullary(Symbol(b)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + 1 - b";
        let res = Binary(Minus, Box::new((Binary(Plus, Box::new((Nullary(Symbol(a)), Nullary(Integer(int))))), Nullary(Symbol(b)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a * 1 - b";
        let res = Binary(Minus, Box::new((Binary(Times, Box::new((Nullary(Symbol(a)), Nullary(Integer(int))))), Nullary(Symbol(b)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a * 1 ^ b";
        let res = Binary(Times, Box::new((Nullary(Symbol(a)), Binary(Power, Box::new((Nullary(Integer(int)), Nullary(Symbol(b))))))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);
    }

    #[test]
    fn tst_brackets() {
        log_init();
        use Expression::*;
        use NullaryOp::*;
        use BinaryOp::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let expr: &[u8] = b" ( a + 1 )";
        let res = Binary(Plus, Box::new((Nullary(Symbol(a)), Nullary(Integer(int)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" a + (1 - b)";
        let res = Binary(Plus, Box::new((Nullary(Symbol(a)), Binary(Minus, Box::new((Nullary(Integer(int)), Nullary(Symbol(b))))))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" (a * 1) ^ b";
        let res = Binary(Power, Box::new((Binary(Times, Box::new((Nullary(Symbol(a)), Nullary(Integer(int))))), Nullary(Symbol(b)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);
    }

    #[test]
    fn tst_square_brackets() {
        log_init();
        use Expression::*;
        use NullaryOp::*;
        use BinaryOp::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let expr: &[u8] = b"a[1]";
        let res = Binary(Coefficient, Box::new((Nullary(Symbol(a)), Nullary(Integer(int)))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" b^a[1]";
        let res = Binary(Power, Box::new((Nullary(Symbol(b)), Binary(Coefficient, Box::new((Nullary(Symbol(a)), Nullary(Integer(int))))))));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

    }

    #[test]
    fn tst_function() {
        log_init();
        use NullaryOp::*;
        use BinaryOp::*;
        use Expression::*;

        let a: &[u8] = b"a";
        let b: &[u8] = b"b";
        let int: &[u8] = b"1";

        let fun = Binary(Function, Box::new((Nullary(Symbol(a)), Nullary(Integer(int)))));
        let expr: &[u8] = b"a(1)";
        let res = fun.clone().into();
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

        let expr: &[u8] = b" b^a(1)";
        let res = Binary(Power, Box::new((Nullary(Symbol(b)), fun.into())));
        let mut parser = Parser::new(expr);
        assert_eq!(parser.parse().unwrap(), res);

    }
}
