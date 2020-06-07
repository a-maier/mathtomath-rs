use std::collections::HashMap;

use super::lexer::{self, StaticToken};
use crate::expression::*;
use crate::arity::Arity;

pub(crate) fn bracket_to_expr<'a>(
    opening: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    debug_assert_eq!(opening, StaticToken::LeftBracket);
    arg
}

pub(crate) fn prefix_op_to_expr<'a>(
    op: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    let op = *PREFIX_OP_TO_EXPR.get(&op).expect(
        "Internal error: prefix operator to expression"
    );
    Expression::Unary(op, Box::new(arg))
}

pub(crate) fn postfix_op_to_expr<'a>(
    op: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    debug_assert_eq!(op, StaticToken::Wildcard);
    Expression::Unary(UnaryOp::Wildcard, Box::new(arg))
}

pub(crate) fn function_to_expr<'a>(
    op: StaticToken,
    head: Expression<'a>,
    arg: Expression<'a>,
) -> Expression<'a> {
    use Expression::Binary;
    use BinaryOp::*;
    let args = Box::new((head, arg));
    match op {
        StaticToken::LeftBracket => Binary(Function, args),
        StaticToken::LeftSquareBracket => Binary(Coefficient, args),
        _ => unreachable!("Internal error: function-like operator {:?} to expression", op)
    }
}

pub(crate) fn binary_op_to_expr<'a>(
    op: StaticToken,
    left: Expression<'a>,
    right: Expression<'a>,
) -> Expression<'a> {
    let op = *BINARY_OP_TO_EXPR.get(&op).expect(
        "Internal error: postfix operator to expression"
    );
    Expression::Binary(op, Box::new((left, right)))
}

pub const PREC_SYMBOL: u32 = 0;
pub const PREC_INTEGER: u32 = 0;
pub const PREC_ELLIPSIS: u32 = 0;
pub const PREC_RIGHT_BRACKET: u32 = 0;
pub const PREC_RIGHT_SQUARE_BRACKET: u32 = 0;
pub const PREC_SEMICOLON: u32 = 10;
pub const PREC_COMMA: u32 = 20;
pub const PREC_EQUALS: u32 = 30;
pub const PREC_PLUS: u32 = 40;
pub const PREC_MINUS: u32 = PREC_PLUS;
pub const PREC_TIMES: u32 = 50;
pub const PREC_DIVIDE: u32 = PREC_TIMES;
pub const PREC_UPLUS: u32 = 60;
pub const PREC_UMINUS: u32 = PREC_UPLUS;
pub const PREC_POWER: u32 = 70;
pub const PREC_DOT: u32 = 80;
pub const PREC_LEFT_BRACKET: u32 = 90;
pub const PREC_LEFT_SQUARE_BRACKET: u32 = 100;
pub const PREC_WILDCARD: u32 = 110;
pub const PREC_ATOM: u32 = std::u32::MAX;

pub fn is_symbol(i: &[u8]) -> bool {
    match lexer::symbol(i) {
        Ok((rest, _)) => rest.is_empty(),
        Err(_) => false,
    }
}

// arity for "operators" that can appear in the call to the `null`  method
// of the Pratt parser, i.e. nullary, unary prefix, or brackets
lazy_static! {
    pub(crate) static ref NULL_ARITY: HashMap<StaticToken, Arity> = hashmap!{
        StaticToken::Ellipsis => Arity::Nullary,
        StaticToken::I => Arity::Nullary,
        StaticToken::Pi => Arity::Nullary,
        StaticToken::Log => Arity::Nullary,
        StaticToken::Exp => Arity::Nullary,
        StaticToken::Sin => Arity::Nullary,
        StaticToken::Cos => Arity::Nullary,
        StaticToken::Tan => Arity::Nullary,
        StaticToken::Sinh => Arity::Nullary,
        StaticToken::Cosh => Arity::Nullary,
        StaticToken::Tanh => Arity::Nullary,
        StaticToken::ASin => Arity::Nullary,
        StaticToken::ACos => Arity::Nullary,
        StaticToken::ATan => Arity::Nullary,
        StaticToken::ASinh => Arity::Nullary,
        StaticToken::ACosh => Arity::Nullary,
        StaticToken::ATanh => Arity::Nullary,
        StaticToken::Sqrt => Arity::Nullary,

        StaticToken::Plus => Arity::Unary,
        StaticToken::Minus => Arity::Unary,
        StaticToken::Wildcard => Arity::Unary,

        StaticToken::LeftBracket => Arity::Unary,
    };
}

// arity for operators that can appear in the call to the `left`  method
// of the Pratt parser, i.e. unary postfix, binary, or function-like
lazy_static! {
    pub(crate) static ref LEFT_ARITY: HashMap<StaticToken, Arity> = hashmap!{
        StaticToken::Wildcard => Arity::Unary,

        StaticToken::LeftBracket => Arity::Function,
        StaticToken::LeftSquareBracket => Arity::Function,

        StaticToken::Semicolon => Arity::Binary,
        StaticToken::Comma => Arity::Binary,
        StaticToken::Plus => Arity::Binary,
        StaticToken::Times => Arity::Binary,
        StaticToken::Minus => Arity::Binary,
        StaticToken::Divide =>Arity::Binary,
        StaticToken::Equals =>Arity::Binary,
        StaticToken::Power => Arity::Binary,
        StaticToken::Dot => Arity::Binary,
    };
}

lazy_static! {
    pub(crate) static ref PREFIX_OP_TO_EXPR: HashMap<StaticToken, UnaryOp> = hashmap!{
        StaticToken::Plus => UnaryOp::UPlus,
        StaticToken::Minus => UnaryOp::UMinus,
        StaticToken::Wildcard => UnaryOp::Many0Wildcard,
    };
}

lazy_static! {
    pub(crate) static ref BINARY_OP_TO_EXPR: HashMap<StaticToken, BinaryOp> = hashmap!{
        StaticToken::Semicolon => BinaryOp::Compound,
        StaticToken::Comma => BinaryOp::Sequence,
        StaticToken::Plus => BinaryOp::Plus,
        StaticToken::Times => BinaryOp::Times,
        StaticToken::Minus => BinaryOp::Minus,
        StaticToken::Divide => BinaryOp::Divide,
        StaticToken::Equals => BinaryOp::Equals,
        StaticToken::Power => BinaryOp::Power,
        StaticToken::Dot => BinaryOp::Dot,
    };
}

lazy_static! {
    pub(crate) static ref CLOSING_BRACKET: HashMap<StaticToken, StaticToken> = hashmap!{
        StaticToken::LeftBracket => StaticToken::RightBracket,
        StaticToken::LeftSquareBracket => StaticToken::RightSquareBracket,
    };
}

lazy_static! {
    pub(crate) static ref TOKEN_EXPRESSION: HashMap<StaticToken, NullaryOp<'static>> = hashmap!{
        StaticToken::Ellipsis => NullaryOp::Ellipsis,
        StaticToken::I => NullaryOp::I,
        StaticToken::Pi => NullaryOp::Pi,
        StaticToken::Log => NullaryOp::Log,
        StaticToken::Exp => NullaryOp::Exp,
        StaticToken::Sin => NullaryOp::Sin,
        StaticToken::Cos => NullaryOp::Cos,
        StaticToken::Tan => NullaryOp::Tan,
        StaticToken::Sinh => NullaryOp::Sinh,
        StaticToken::Cosh => NullaryOp::Cosh,
        StaticToken::Tanh => NullaryOp::Tanh,
        StaticToken::ASin => NullaryOp::ASin,
        StaticToken::ACos => NullaryOp::ACos,
        StaticToken::ATan => NullaryOp::ATan,
        StaticToken::ASinh => NullaryOp::ASinh,
        StaticToken::ACosh => NullaryOp::ACosh,
        StaticToken::ATanh => NullaryOp::ATanh,
        StaticToken::Sqrt => NullaryOp::Sqrt,
    };
}
