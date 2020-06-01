use super::grammar::*;
use crate::error::FormatError;
use crate::expression::*;

use std::io;

pub type Result = std::result::Result<(), FormatError>;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Formatter<'a> {
    expression: Expression<'a>
}

impl<'a> Formatter<'a> {
    pub fn new(expression: Expression<'a>) -> Self {
        Formatter{expression}
    }

    pub fn format<W: io::Write>(self, w: &mut W) -> Result {
        format(w, properties(self.expression)?, false)
    }
}

fn format<W: io::Write>(
    w: &mut W,
    prop: ExpressionProperties<'_>,
    with_paren: bool
) -> Result {
    let prec = prop.prec;
    if with_paren {
        w.write_all(b"(")?;
    }
    use ExpressionKind::*;
    match prop.kind {
        Empty => {()},
        Symbol(sym) => {
            if is_symbol(sym) {
                w.write_all(sym)?;
            } else {
                w.write_all(b"[")?;
                w.write_all(sym)?;
                w.write_all(b"]")?;
            }
        },
        Integer(i) => w.write_all(i)?,
        Nullary(op) => w.write_all(op)?,
        Prefix(op, arg) => {
            w.write_all(op)?;
            let arg = properties(arg)?;
            let arg_prec = arg.prec;
            format(w, arg, arg_prec < prec)?;
        },
        Postfix(arg, op) => {
            let arg = properties(arg)?;
            let arg_prec = arg.prec;
            format(w, arg, arg_prec < prec)?;
            w.write_all(op)?;
        },
        Infix(left_arg, op, right_arg) => {
            let left_arg = properties(left_arg)?;
            let left_arg_prec = left_arg.prec;
            format(w, left_arg, left_arg_prec < prec)?;
            w.write_all(op)?;
            let right_arg = properties(right_arg)?;
            let right_arg_prec = right_arg.prec;
            // add bracket if operand on the right is of the same time
            // e.g. a - (b - c)
            let need_bracket = if let Infix(_, right_op, _) = right_arg.kind {
                right_op == op || right_arg_prec < prec
            } else {
                right_arg_prec < prec
            };
            format(w, right_arg, need_bracket)?;
        },
        Circumfix(left, arg, right) => {
            let arg = properties(arg)?;
            w.write_all(left)?;
            format(w, arg, false)?;
            w.write_all(right)?;
        },
        Function(head, left, arg, right) => {
            let head = properties(head)?;
            let head_prec = head.prec;
            format(w, head, head_prec < prec)?;
            w.write_all(left)?;
            let arg = properties(arg)?;
            format(w, arg, false)?;
            w.write_all(right)?;
        },
    };
    if with_paren {
        w.write_all(b")")?;
    }
    Ok(())
}

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
struct ExpressionProperties<'a> {
    prec: u32,
    kind: ExpressionKind<'a>,
}

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
enum ExpressionKind<'a> {
    Empty,
    Integer(&'a [u8]),
    Symbol(&'a [u8]),
    Nullary(&'static [u8]),
    Prefix(&'static [u8], Expression<'a>),
    Infix(Expression<'a>, &'static [u8], Expression<'a>),
    Postfix(Expression<'a>, &'static [u8]),
    Circumfix(&'static [u8], Expression<'a>, &'static [u8]),
    Function(Expression<'a>, &'static [u8], Expression<'a>, &'static [u8]),
}

fn properties(
    expression: Expression<'_>
) -> std::result::Result<ExpressionProperties<'_>, FormatError> {
    use Expression::{Unary, Binary};
    use ExpressionKind::*;
    let (prec, kind) = match expression {
        Expression::Nullary(nullary) => match nullary {
            NullaryOp::Empty => (PREC_ATOM, Empty),
            NullaryOp::Integer(i) => (PREC_ATOM, Integer(i)),
            NullaryOp::Symbol(s) => (PREC_ATOM, Symbol(s)),
            NullaryOp::Ellipsis => (PREC_ATOM, Nullary(b"...")),
            _ => return Err(FormatError::InvalidExpression)
        },
        Unary(unary, arg) => match unary {
            UnaryOp::Wildcard => (PREC_ATOM, Postfix(*arg, b"?")),
            UnaryOp::Many0Wildcard => (PREC_ATOM, Prefix(b"?", *arg)),
            UnaryOp::UPlus => (PREC_UPLUS, Prefix(b"+",*arg)),
            UnaryOp::UMinus => (PREC_UMINUS, Prefix(b"-",*arg)),
            _ => return Err(FormatError::InvalidExpression)
        },
        Binary(binary, args) => {
            let (left, right) = *args;
            match binary {
                BinaryOp::Plus => (PREC_PLUS, Infix(left, b"+", right)),
                BinaryOp::Minus => (PREC_MINUS, Infix(left, b"-", right)),
                BinaryOp::Times => (PREC_TIMES, Infix(left, b"*", right)),
                BinaryOp::Divide => (PREC_DIVIDE, Infix(left, b"/", right)),
                BinaryOp::Compound => (PREC_SEMICOLON, Infix(left, b";", right)),
                BinaryOp::Sequence => (PREC_COMMA, Infix(left, b",", right)),
                BinaryOp::Equals => (PREC_EQUALS, Infix(left, b"=", right)),
                BinaryOp::Dot => (PREC_DOT, Infix(left, b".", right)),
                BinaryOp::Power => (PREC_POWER, Infix(left, b"^", right)),
                BinaryOp::Coefficient => (PREC_LEFT_SQUARE_BRACKET, Function(left, b"[", right, b"]")),
                BinaryOp::Function => (PREC_LEFT_BRACKET, Function(left, b"(", right, b")")),
                _ => return Err(FormatError::InvalidExpression)
            }
        }
    };
    Ok(ExpressionProperties{prec, kind: kind})
}
