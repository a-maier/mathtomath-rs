//TODO: code duplication
use super::grammar::*;
use super::lexer::Lexer;
use super::tokens::{Token, StaticToken, TOKEN_PREC, TOKEN_EXPRESSION, UNKNOWN_TOKEN_PREC, NULL_ARITY, LEFT_ARITY, CLOSING_BRACKET, PREFIX_OP_TO_EXPR, POSTFIX_OP_TO_EXPR, BINARY_OP_TO_EXPR};
use crate::error::{SyntaxError, ErrorKind::*};
use crate::expression::*;
use crate::arity::Arity;
use crate::range::Range;

pub fn parse<'a>(input: &'a str) -> Result<Expression<'a>, SyntaxError> {
    let mut parser = Parser::new(&input);
    parser.parse()
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
}

const LEFT_TOKENS: &'static str =
    "a postfix operator, a binary operator, or an opening bracket";

impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Self {
        let lexer = Lexer::for_input(input);
        Parser{lexer}
    }

    fn parse(&mut self) -> Result<Expression<'a>, SyntaxError> {
        let mut next = self.lexer.next().transpose()?;
        let res = self.parse_with(&mut next, 0)?;
        debug!("remaining token: {:?}", next);
        match next {
            None => Ok(res),
            Some((token, pos)) => {
                use Token::Static;
                use StaticToken::*;
                match token {
                    Static(RightAngleBracket)
                        | Static(RightAssociation)
                        | Static(RightBracket)
                        | Static(RightCeiling)
                        | Static(RightFloor)
                        | Static(RightSquareBracket)
                        | Static(RightPart)
                        | Static(RightList)
                        | Static(RightTee)
                        => Err(SyntaxError::new(Unmatched(""), pos.start)),
                    _ => Err(SyntaxError::new(RemainingToken, pos.start))
                }
            }
        }
    }

    fn parse_with(
        &mut self,
        mut next: &mut Option<(Token<'a>, Range<usize>)>,
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
        token: Option<(Token<'a>, Range<usize>)>,
        next: &mut Option<(Token<'a>, Range<usize>)>
    ) -> Result<Expression<'a>, SyntaxError> {
        use Expression::*;
        use NullaryOp::*;
        debug!("null called on token {:?}", token);
        if let Some((tok, pos)) = token {
            match tok {
                Token::Symbol(name) => Ok(Nullary(Symbol(name))),
                Token::Integer(int) => Ok(Nullary(Integer(int))),
                Token::String(s) => Ok(Nullary(String(s))),
                Token::Real(x) => Ok(Nullary(Real(x))),
                Token::Static(s) => {
                    use Arity::*;
                    match NULL_ARITY.get(&s) {
                        Some(Nullary) => Ok(
                            Expression::Nullary(TOKEN_EXPRESSION[&s].clone())
                        ),
                        Some(Unary) => if let Some(closing) = CLOSING_BRACKET.get(&s) {
                            // this is actually a bracket
                            let arg = self.parse_with(next, 0)?;
                            if let Some((c, _)) = *next {
                                if c == Token::Static(*closing) {
                                    *next = self.lexer.next().transpose()?;
                                    Ok(bracket_to_expr(s, arg))
                                } else {
                                    Err(SyntaxError::new(Unmatched(""), pos.start))
                                }
                            } else {
                                Err(SyntaxError::new(Unmatched(""), pos.start))
                            }
                        } else {
                            // standard unary prefix operator
                            let prec = null_binding_power(token);
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
            Ok(Nullary(Empty))
        }
    }

    fn left(
        &mut self,
        token: Option<(Token<'a>, Range<usize>)>,
        next: &mut Option<(Token<'a>, Range<usize>)>,
        left: Expression<'a>,
    ) -> Result<Expression<'a>, SyntaxError> {
        debug!("left called on token {:?}", token);
        if let Some((Token::Static(s), ref pos)) = token {
            use Arity::*;
            match LEFT_ARITY.get(&s) {
                Some(Unary) => Ok(postfix_op_to_expr(s, left)),
                Some(Binary) => {
                    let right_binding_power = left_binding_power(token);
                    let right = self.parse_with(next, right_binding_power)?;
                    Ok(binary_op_to_expr(s, left, right))
                },
                Some(Function) => {
                    let next_token = next.as_ref().map(|(t, _pos)| t);
                    if next_token == Some(&Token::Static(CLOSING_BRACKET[&s])) {
                        *next = self.lexer.next().transpose()?;
                        return Ok(function_to_expr(s, left, Expression::Nullary(NullaryOp::Empty)));
                    }
                    let right = self.parse_with(next, 0)?;
                    let next_token = next.as_ref().map(|(t, _pos)| t);
                    if next_token == Some(&Token::Static(CLOSING_BRACKET[&s])) {
                        *next = self.lexer.next().transpose()?;
                        Ok(function_to_expr(s, left, right))
                    } else {
                        Err(SyntaxError::new(Unmatched(""), pos.start))
                    }
                },
                Some(arity) => unreachable!(
                    "Internal error: {:?} has LEFT_ARITY {:?}", s, arity
                ),
                None => Err(SyntaxError::new(ExpectLeft(LEFT_TOKENS), pos.start))
            }
        } else {
            Err(SyntaxError::new(EarlyEOF(LEFT_TOKENS), self.pos()))
        }
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }
}

fn left_binding_power<'a>(token: Option<(Token<'a>, Range<usize>)>) -> u32 {
    debug!("look up left binding power of {:?}", token);
    use Token::*;
    if let Some((token, _pos)) = token {
        match token {
            Symbol(_) => PREC_SYMBOL,
            Integer(_) => PREC_INTEGER,
            Static(other) => {
                if let Some(prec) = TOKEN_PREC.get(&other) {
                    *prec
                } else {
                    if UNKNOWN_TOKEN_PREC.contains(&other) {
                        panic!("Internal error: unknown precedence of token {:?}", other)
                    } else {
                        unreachable!("Internal error: token {:?}", other)
                    }
                }
            },
            _ => unimplemented!(),
        }
    } else {
        0
    }
}

fn null_binding_power<'a>(token: Option<(Token<'a>, Range<usize>)>) -> u32 {
    debug!("look up null binding power of {:?}", token);
    if let Some((tok, _pos)) = token {
        match tok {
            Token::Static(StaticToken::Subtract) => PREC_UMINUS,
            Token::Static(StaticToken::Plus) => PREC_UPLUS,
            Token::Static(StaticToken::PlusMinus) => PREC_UPLUS_MINUS,
            Token::Static(StaticToken::MinusPlus) => PREC_UMINUS_PLUS,
            Token::Static(StaticToken::Not) => PREC_NOT,
            _ => left_binding_power(token)
        }
    } else {
        left_binding_power(token)
    }
}

fn bracket_to_expr<'a>(
    opening: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    use Expression::Unary;
    use UnaryOp::*;
    match opening {
        StaticToken::LeftAngleBracket  => Unary(Angle, Box::new(arg)),
        StaticToken::LeftAssociation   => Unary(Association, Box::new(arg)),
        StaticToken::LeftBracket       => arg,
        StaticToken::LeftCeiling       => Unary(Ceiling, Box::new(arg)),
        StaticToken::LeftFloor         => Unary(Floor, Box::new(arg)),
        StaticToken::LeftList          => Unary(List, Box::new(arg)),
        _ => panic!("Internal error: {:?} is not a bracket", opening)
    }
}

fn prefix_op_to_expr<'a>(
    op: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    if op == StaticToken::Sqrt {
        Expression::Binary(BinaryOp::Function, Box::new((
            Expression::Nullary(NullaryOp::Sqrt), arg
        )))
    } else {
        let op = *PREFIX_OP_TO_EXPR.get(&op).expect(
            "Internal error: prefix operator to expression"
        );
        Expression::Unary(op, Box::new(arg))
    }
}

fn postfix_op_to_expr<'a>(
    op: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    let op = *POSTFIX_OP_TO_EXPR.get(&op).expect(
        "Internal error: postfix operator to expression"
    );
    Expression::Unary(op, Box::new(arg))
}

fn function_to_expr<'a>(
    op: StaticToken,
    head: Expression<'a>,
    arg: Expression<'a>,
) -> Expression<'a> {
    use Expression::Binary;
    use BinaryOp::*;
    let args = Box::new((head, arg));
    match op {
        StaticToken::LeftSquareBracket => Binary(Function, args),
        StaticToken::LeftPart => Binary(Part, args),
        _ => unreachable!("Internal error: function-like operator {:?} to expression", op)
    }
}

fn binary_op_to_expr<'a>(
    op: StaticToken,
    left: Expression<'a>,
    right: Expression<'a>,
) -> Expression<'a> {
    let op = *BINARY_OP_TO_EXPR.get(&op).expect(
        "Internal error: postfix operator to expression"
    );
    Expression::Binary(op, Box::new((left, right)))
}
