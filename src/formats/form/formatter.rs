use super::grammar::*;
use crate::expression::{self, Expression};

use std::{fmt, str};

#[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Formatter<'a, 'b: 'a> {
    expression: &'a Expression<'b>
}

impl<'a, 'b: 'a> Formatter<'a, 'b> {
    pub fn new(expression: &'a Expression<'b>) -> Self {
        Formatter{expression}
    }
}

impl<'a, 'b: 'a> fmt::Display for Formatter<'a, 'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format(f, properties(self.expression.clone())?, false)
    }
}

fn format(
    f: &mut fmt::Formatter<'_>,
    prop: ExpressionProperties<'_>,
    with_paren: bool
) -> fmt::Result {
    let prec = prop.prec;
    if with_paren {
        write!(f, "(")?;
    }
    use ExpressionKind::*;
    match prop.kind {
        Empty => {()},
        Symbol(sym) => {
            let sym_str = str::from_utf8(sym).unwrap();
            if is_symbol(sym) {
                write!(f, "{}", sym_str)?;
            } else {
                write!(f, "[{}]", sym_str)?;
            }
        },
        Integer(i) => {
            let i = str::from_utf8(i).unwrap();
            write!(f, "{}", i)?;
        },
        Nullary(op) => {
            write!(f, "{}", op)?;
        },
        Prefix(op, arg) => {
            write!(f, "{}", op)?;
            let arg = properties(arg)?;
            let arg_prec = arg.prec;
            format(f, arg, arg_prec < prec)?;
        },
        Postfix(arg, op) => {
            let arg = properties(arg)?;
            let arg_prec = arg.prec;
            format(f, arg, arg_prec < prec)?;
            write!(f, "{}", op)?;
        },
        Infix(left_arg, op, right_arg) => {
            let left_arg = properties(left_arg)?;
            let left_arg_prec = left_arg.prec;
            format(f, left_arg, left_arg_prec < prec)?;
            write!(f, "{}", op)?;
            let right_arg = properties(right_arg)?;
            let right_arg_prec = right_arg.prec;
            format(f, right_arg, right_arg_prec < prec)?;
        },
        Circumfix(left, arg, right) => {
            let arg = properties(arg)?;
            write!(f, "{}", left)?;
            format(f, arg, false)?;
            write!(f, "{}", right)?;
        },
        Function(head, left, arg, right) => {
            let head = properties(head)?;
            let head_prec = head.prec;
            format(f, head, head_prec < prec)?;
            write!(f, "{}", left)?;
            let arg = properties(arg)?;
            format(f, arg, false)?;
            write!(f, "{}", right)?;
        },
    };
    if with_paren {
        write!(f, "(")
    } else {
        Ok(())
    }
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
    Nullary(&'static str),
    Prefix(&'static str, Expression<'a>),
    Infix(Expression<'a>, &'static str, Expression<'a>),
    Postfix(Expression<'a>, &'static str),
    Circumfix(&'static str, Expression<'a>, &'static str),
    Function( Expression<'a>, &'static str, Expression<'a>, &'static str),
}

fn properties(
    expression: Expression<'_>
) -> Result<ExpressionProperties, fmt::Error> {
    use ExpressionKind::*;
    let (prec, kind) = match expression {
        Expression::Empty => (PREC_ATOM, Empty),
        Expression::Integer(i) => (PREC_ATOM, Integer(i)),
        Expression::Symbol(s) => (PREC_ATOM, Symbol(s)),
        Expression::Ellipsis => (PREC_ATOM, Nullary("...")),
        Expression::Wildcard(expression::Symbol(sym)) =>
            (PREC_ATOM, Postfix(Expression::Symbol(sym), "?")),
        Expression::Many0Wildcard(expression::Symbol(sym)) =>
            (PREC_ATOM, Prefix("?", Expression::Symbol(sym))),
        Expression::UPlus(arg)=> (PREC_UPLUS, Prefix("+",*arg)),
        Expression::UMinus(arg) => (PREC_UMINUS, Prefix("-",*arg)),
        Expression::Plus(args) => {
            let (left, right) = *args;
            (PREC_PLUS, Infix(left, "+", right))
        },
        Expression::Minus(args) => {
            let (left, right) = *args;
            (PREC_MINUS, Infix(left, "-", right))
        },
        Expression::Times(args) => {
            let (left, right) = *args;
            (PREC_TIMES, Infix(left, "*", right))
        },
        Expression::Divide(args) => {
            let (left, right) = *args;
            (PREC_DIVIDE, Infix(left, "/", right))
        },
        Expression::Compound(args) => {
            let (left, right) = *args;
            (PREC_SEMICOLON, Infix(left, ";", right))
        },
        Expression::Sequence(args) => {
            let (left, right) = *args;
            (PREC_COMMA, Infix(left, ",", right))
        },
        Expression::Equals(args) => {
            let (left, right) = *args;
            (PREC_EQUALS, Infix(left, "=", right))
        },
        Expression::Dot(args) => {
            let (left, right) = *args;
            (PREC_DOT, Infix(left, ".", right))
        },
        Expression::Power(args) => {
            let (left, right) = *args;
            (PREC_POWER, Infix(left, "^", right))
        },
        Expression::Coefficient(args) => {
            let (head, arg) = *args;
            (PREC_LEFT_SQUARE_BRACKET, Function(head, "[", arg, "]"))
        },
        Expression::Function(args) => {
            let (head, arg) = *args;
            (PREC_LEFT_BRACKET, Function(head, "(", arg, ")"))
        },
        _ => return Err(fmt::Error{})
    };
    Ok(ExpressionProperties{prec, kind})
}
