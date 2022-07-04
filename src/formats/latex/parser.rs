//TODO: code duplication
use std::collections::HashMap;

use super::grammar::*;
use super::lexer::Lexer;
use super::tokens::{Token, StaticToken, TOKEN_PREC, TOKEN_EXPRESSION, NULL_ARITY, LEFT_ARITY, CLOSING_BRACKET, PREFIX_OP_TO_EXPR, POSTFIX_OP_TO_EXPR, BINARY_OP_TO_EXPR};
use crate::error::{SyntaxError, ErrorKind::*};
use crate::expression::*;
use crate::assoc::Assoc;
use crate::arity::Arity;
use crate::range::Range;
use crate::error::ErrorKind;

#[derive(Clone,Eq,PartialEq,Debug)]
enum ParseError<'a> {
    Syntax(SyntaxError),
    MulParsedAsFunction(Expression<'a>),
}

// impl std::error::Error for ParseError {
// }

impl<'a> std::convert::From<SyntaxError> for ParseError<'a> {
    fn from(err: SyntaxError) -> Self {
        ParseError::Syntax(err)
    }
}

pub fn parse(input: &[u8]) -> Result<Expression<'_>, SyntaxError> {
    let mut parser = Parser::new(input);
    parser.parse()
}

#[derive(Copy,Clone,Default,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
struct Parser<'a> {
    lexer: Lexer<'a>,
    input: &'a [u8],
}

const LEFT_TOKENS: &str =
    "a postfix operator, a binary operator, or an opening bracket";

impl<'a> Parser<'a> {
    fn new(input: &'a [u8]) -> Self {
        let lexer = Lexer::for_input(input);
        Parser{lexer, input}
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
                        | Static(RightCeiling)
                        | Static(RightFloor)
                        | Static(RightBrace)
                        | Static(RightList)
                        => {
                            let bracket = &self.input[pos.start..pos.end];
                            let bracket = std::str::from_utf8(bracket).unwrap();
                            Err(SyntaxError::new(Unmatched(bracket.to_owned()), pos.start))
                        },
                    _ => Err(SyntaxError::new(RemainingToken, pos.start))
                }
            }
        }
    }

    fn parse_with(
        &mut self,
        next: &mut Option<(Token<'a>, Range<usize>)>,
        right_binding_power: u32
    ) -> Result<Expression<'a>, SyntaxError> {
        debug!("parser called with rbp {}", right_binding_power);
        let mut token = *next;
        *next = self.lexer.next().transpose()?;
        let mut left = self.null(token, next)?;
        while right_binding_power < left_binding_power(*next) {
            token = *next;
            *next = self.lexer.next().transpose()?;
            left = match self.left(token, next, left) {
                Err(ParseError::MulParsedAsFunction(left)) => {
                    debug!("ambiguous function parse, backtrack and parse as multiplication");
                    self.lexer = Lexer::for_input(self.input);
                    let (_, pos) = token.unwrap();
                    self.lexer.skip_bytes(pos.start);
                    let times = Token::Static(StaticToken::Times);
                    let range = Range{start: pos.start, end: pos.start};
                    *next = Some((times, range));
                    left
                },
                Err(ParseError::Syntax(err)) => return Err(err),
                Ok(expr) => expr,
            };
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
                Token::Symbol(name) => {
                    let name = LATEX_SYMBOLS.get(&name).unwrap_or(&name);
                    Ok(Nullary(Symbol(name)))
                },
                Token::Integer(int) => Ok(Nullary(Integer(int))),
                Token::Real(x) => Ok(Nullary(Real(x))),
                Token::Static(StaticToken::Frac) => {
                    let num = self.parse_with(next, PREC_FRAC)?;
                    trace!("fraction num: {:?}", num);
                    let den = self.parse_with(next, PREC_FRAC)?;
                    trace!("fraction den: {:?}", num);
                    Ok(Expression::Binary(BinaryOp::Divide, Box::new((num, den))))
                },
                Token::Static(s) => {
                    use Arity::*;
                    match NULL_ARITY.get(&s) {
                        Some(Nullary) => Ok(
                            Expression::Nullary(TOKEN_EXPRESSION[&s])
                        ),
                        Some(Unary) => if let Some(closing) = CLOSING_BRACKET.get(&s) {
                            trace!("looking for {:?}", closing);
                            // this is actually a bracket
                            let arg = self.parse_with(next, 0)?;
                            trace!("argument {:?}", arg);
                            let next_token = next.as_ref().map(|(t, _pos)| t);
                            if next_token == Some(&Token::Static(*closing)) {
                                *next = self.lexer.next().transpose()?;
                                Ok(bracket_to_expr(s, arg))
                            } else {
                                let bracket = &self.input[pos.start..pos.end];
                                let bracket = std::str::from_utf8(bracket).unwrap();
                                Err(SyntaxError::new(Unmatched(bracket.to_owned()), pos.start))
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

    fn parse_bracket(
        &mut self,
        next: &mut Option<(Token<'a>, Range<usize>)>,
        closing: StaticToken,
        pos: &Range<usize>,
    ) -> Result<Expression<'a>, SyntaxError> {
        let next_token = next.as_ref().map(|(t, _pos)| t);
        if next_token == Some(&Token::Static(closing)) {
            *next = self.lexer.next().transpose()?;
            return Ok(Expression::Nullary(NullaryOp::Empty));
        }
        let arg = self.parse_with(next, 0)?;
        let next_token = next.as_ref().map(|(t, _pos)| t);
        if next_token == Some(&Token::Static(closing)) {
            *next = self.lexer.next().transpose()?;
            Ok(arg)
        } else {
            let bracket = &self.input[pos.start..pos.end];
            let bracket = std::str::from_utf8(bracket).unwrap();
            Err(SyntaxError::new(Unmatched(bracket.to_owned()), pos.start))
        }
    }

    fn left(
        &mut self,
        token: Option<(Token<'a>, Range<usize>)>,
        next: &mut Option<(Token<'a>, Range<usize>)>,
        left: Expression<'a>,
    ) -> Result<Expression<'a>, ParseError<'a>> {
        debug!("left called on token {:?}", token);
        match token {
            Some((Token::Static(StaticToken::Subscript), _)) => {
                let right = self.parse_with(next, PREC_SUBSCRIPT)?;
                let arg = Expression::Binary(BinaryOp::Sequence, Box::new((left, right)));
                let head = Expression::Nullary(NullaryOp::Subscript);
                Ok(Expression::Binary(BinaryOp::Function, Box::new((head, arg))))
            },
            Some((Token::Static(s), ref pos)) => {
                use Arity::*;
                trace!("left arity for {:?}: {:?}", s, LEFT_ARITY.get(&s));
                match LEFT_ARITY.get(&s) {
                    Some(Unary) => Ok(postfix_op_to_expr(s, left)),
                    Some(Binary) => {
                        let right_binding_power = left_binding_power(token);
                        let right = self.parse_with(next, right_binding_power)?;
                        binary_op_to_expr(s, left, right).map_err(
                            |e| ParseError::Syntax(SyntaxError::new(e, pos.start))
                        )
                    },
                    Some(Function) => {
                        let arg = self.parse_bracket(next, CLOSING_BRACKET[&s], pos)?;
                        let op = if let Expression::Binary(BinaryOp::Sequence, _) = arg {
                            BinaryOp::Function
                        } else {
                            match left {
                                Expression::Nullary(NullaryOp::Symbol(_))
                                    | Expression::Nullary(NullaryOp::Log)
                                    | Expression::Nullary(NullaryOp::Exp)
                                    | Expression::Nullary(NullaryOp::Sin)
                                    | Expression::Nullary(NullaryOp::Sign)
                                    | Expression::Nullary(NullaryOp::Cos)
                                    | Expression::Nullary(NullaryOp::Tan)
                                    | Expression::Nullary(NullaryOp::Sinh)
                                    | Expression::Nullary(NullaryOp::Cosh)
                                    | Expression::Nullary(NullaryOp::Tanh)
                                    | Expression::Nullary(NullaryOp::ASin)
                                    | Expression::Nullary(NullaryOp::ACos)
                                    | Expression::Nullary(NullaryOp::ATan)
                                    | Expression::Nullary(NullaryOp::ASinh)
                                    | Expression::Nullary(NullaryOp::ACosh)
                                    | Expression::Nullary(NullaryOp::ATanh)
                                    | Expression::Nullary(NullaryOp::Sqrt)
                                    | Expression::Nullary(NullaryOp::OverHat)
                                    | Expression::Nullary(NullaryOp::OverTilde)
                                    => BinaryOp::Function,
                                Expression::Binary(BinaryOp::Function, ref arg)
                                    if arg.0 == Expression::Nullary(NullaryOp::Subscript)
                                    ||arg.0 == Expression::Nullary(NullaryOp::Superscript)
                                    => BinaryOp::Function,
                                _ => return Err(ParseError::MulParsedAsFunction(left))
                            }
                        };
                        Ok(Expression::Binary(op, Box::new((left, arg))))
                    },
                    Some(arity) => unreachable!(
                        "Internal error: {:?} has LEFT_ARITY {:?}", s, arity
                    ),
                    None => {
                        debug!("no operator found: treat as multiplication");
                        trace!("left multiplier: {:?}", left);
                        let rhs = if let Some(closing) = CLOSING_BRACKET.get(&s) {
                            self.parse_bracket(next, *closing, pos)?
                        } else {
                            // rewind lexer
                            self.lexer = Lexer::for_input(self.input);
                            self.lexer.skip_bytes(pos.start);
                            *next = self.lexer.next().transpose()?;
                            self.parse_with(next, PREC_TIMES)?
                        };
                        trace!("right multiplier: {:?}", rhs);
                        Ok(Expression::Binary(BinaryOp::Times, Box::new((left, rhs))))
                    }
                }
            },
            Some((_, ref pos)) => {
                // rewind lexer
                self.lexer = Lexer::for_input(self.input);
                self.lexer.skip_bytes(pos.start);
                *next = self.lexer.next().transpose()?;
                if is_operator_like_fn(&left) {
                    debug!("no operator found: treat as application of {left:?} to argument");
                    let arg = self.parse_with(next, PREC_SQRT)?;
                    trace!("argument: {arg:?}");
                    if let Expression::Binary(BinaryOp::Power, args) = left {
                        let (fun, pow) = *args;
                        let fun = Expression::Binary(BinaryOp::Function, Box::new((fun, arg)));
                        Ok(Expression::Binary(BinaryOp::Power, Box::new((fun, pow))))
                    } else {
                        Ok(Expression::Binary(BinaryOp::Function, Box::new((left, arg))))
                    }
                } else {
                    debug!("no operator found: treat as multiplication");
                    trace!("left multiplier: {:?}", left);
                    let rhs = self.parse_with(next, PREC_TIMES)?;
                    trace!("right multiplier: {:?}", rhs);
                    Ok(Expression::Binary(BinaryOp::Times, Box::new((left, rhs))))
                }
            },
            None => Err(SyntaxError::new(EarlyEof(LEFT_TOKENS), self.pos()).into())
        }
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }
}

fn is_operator_like_fn(expr: &Expression<'_>) -> bool {
    use Expression::*;
    use NullaryOp::*;
    match expr {
        Nullary(
            Log | Sin | Cos | Tan | Sinh | Cosh | Tanh | ASin | ACos |
            ATan | ASinh | ACosh | ATanh
        ) => true,
        Binary(BinaryOp::Power, args) => matches!(args.0, Nullary(
            Log | Sin | Cos | Tan | Sinh | Cosh | Tanh | ASin | ACos |
            ATan | ASinh | ACosh | ATanh
        )),
        _ => false
    }
}

fn left_binding_power(token: Option<(Token<'_>, Range<usize>)>) -> u32 {
    debug!("look up left binding power of {:?}", token);
    use Token::*;
    if let Some((token, _pos)) = token {
        match token {
            Symbol(_) | Integer(_) | Real(_) => PREC_TIMES,
            Static(other) => {
                if let Some(prec) = TOKEN_PREC.get(&other) {
                    *prec
                } else {
                    // default to multiplication
                    PREC_TIMES
                }
            },
        }
    } else {
        0
    }
}

fn null_binding_power(token: Option<(Token<'_>, Range<usize>)>) -> u32 {
    debug!("look up null binding power of {:?}", token);
    if let Some((tok, _pos)) = token {
        match tok {
            Token::Static(StaticToken::Minus) => PREC_UMINUS,
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

fn bracket_to_expr(
    opening: StaticToken,
    arg: Expression<'_>
) -> Expression<'_> {
    use Expression::Unary;
    use UnaryOp::*;
    let op = match opening {
        StaticToken::LeftAngleBracket  => Angle,
        StaticToken::LeftBracket       => Bracket,
        StaticToken::LeftCeiling       => Ceiling,
        StaticToken::LeftFloor         => Floor,
        StaticToken::LeftList          => List,
        StaticToken::LeftBrace         => return arg,
        _ => panic!("Internal error: {:?} is not a bracket", opening)
    };
    Unary(op, Box::new(arg))
}

fn prefix_op_to_expr(
    op: StaticToken,
    arg: Expression<'_>
) -> Expression<'_> {
    match op {
        StaticToken::Sqrt
            | StaticToken::OverHat
            | StaticToken::OverTilde
            => {
                trace!("{op:?} with arg {:?}", arg);
                Expression::Binary(BinaryOp::Function, Box::new((
                    Expression::Nullary(*TOKEN_EXPRESSION.get(&op).unwrap()), arg
                )))
            },
        _ => {
            let op = *PREFIX_OP_TO_EXPR.get(&op).expect(
                "Internal error: prefix operator to expression"
            );
            Expression::Unary(op, Box::new(arg))
        }
    }
}

fn postfix_op_to_expr(
    op: StaticToken,
    arg: Expression<'_>
) -> Expression<'_> {
    let op = *POSTFIX_OP_TO_EXPR.get(&op).expect(
        "Internal error: postfix operator to expression"
    );
    Expression::Unary(op, Box::new(arg))
}

fn binary_op_to_expr<'a>(
    op: StaticToken,
    left: Expression<'a>,
    right: Expression<'a>,
) -> Result<Expression<'a>, ErrorKind> {
    use Expression::Binary;
    let op = *BINARY_OP_TO_EXPR.get(&op).expect(
        "Internal error: postfix operator to expression"
    );
    match assoc(op) {
        Assoc::Left => {},
        Assoc::Right => unreachable!(),
        Assoc::None => if let Binary(left_op, left_args) = left {
            if left_op == op {
                return Err(ErrorKind::NonAssocOpChain);
            } else {
                // restore left arg
                let left = Binary(left_op, left_args);
                return Ok(Binary(op, Box::new((left, right))));
            }
        },
    };
    Ok(Binary(op, Box::new((left, right))))
}

lazy_static! {
    pub(crate) static ref LATEX_SYMBOLS: HashMap<&'static [u8], &'static [u8]> = {
        let mut map: HashMap<&'static [u8], &'static [u8]>= super::formatter::LATEX_SYMBOLS
            .iter()
            .map(|(k, v)| (*v, *k))
            .collect();
        //\varGamma \varDelta \varTheta \varLambda \varXi \varPi \varSigma \varPhi \varUpsilon \varOmega
        //\varepsilon \varkappa \varpi \varrho \varsigma \vartheta \varphi
        map.insert(br"\varGamma",  map[&br"\Gamma"[..]]);
        map.insert(br"\varDelta",  map[&br"\Delta"[..]]);
        map.insert(br"\varTheta",  map[&br"\Theta"[..]]);
        map.insert(br"\varLambda",  map[&br"\Lambda"[..]]);
        map.insert(br"\varXi",  map[&br"\Xi"[..]]);
        map.insert(br"\varPi",  map[&br"\Pi"[..]]);
        map.insert(br"\varSigma",  map[&br"\Sigma"[..]]);
        map.insert(br"\varPhi",  map[&br"\Phi"[..]]);
        map.insert(br"\varUpsilon",  map[&br"\Upsilon"[..]]);
        map.insert(br"\varOmega",  map[&br"\Omega"[..]]);
        map.insert(br"\varepsilon",  map[&br"\epsilon"[..]]);
        map.insert(br"\varkappa",  map[&br"\kappa"[..]]);
        map.insert(br"\varpi",  map[&br"\pi"[..]]);
        map.insert(br"\varrho",  map[&br"\rho"[..]]);
        map.insert(br"\varsigma",  map[&br"\sigma"[..]]);
        map.insert(br"\vartheta",  map[&br"\theta"[..]]);
        map.insert(br"\varphi",  map[&br"\phi"[..]]);
        map
    };
}
