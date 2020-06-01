use super::grammar::*;
use crate::expression::*;

use std::io;

pub type Result = std::result::Result<(), std::io::Error>;

#[derive(Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
pub struct Formatter<'a> {
    expression: Expression<'a>
}

impl<'a> Formatter<'a> {
    pub fn new(expression: Expression<'a>) -> Self {
        Formatter{expression}
    }

    pub fn format<W: io::Write>(self, w: &mut W) -> Result {
        format(w, properties(self.expression), false)
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
                warn!("{:?} is not a legal symbol name", std::str::from_utf8(sym));
                w.write_all(b"\"")?;
                w.write_all(sym)?;
                w.write_all(b"\"")?;
            }
        },
        Integer(i) => w.write_all(i)?,
        Real(r) => w.write_all(r)?,
        String(s) => {
            w.write_all(b"\"")?;
            w.write_all(s)?;
            w.write_all(b"\"")?;
        },
        Nullary(op) => w.write_all(op.as_bytes())?,
        Prefix(op, arg) => {
            w.write_all(op.as_bytes())?;
            let arg = properties(arg);
            let arg_prec = arg.prec;
            format(w, arg, arg_prec < prec)?;
        },
        Postfix(arg, op) => {
            let arg = properties(arg);
            let arg_prec = arg.prec;
            format(w, arg, arg_prec < prec)?;
            w.write_all(op.as_bytes())?;
        },
        Infix(left_arg, op, right_arg) => {
            let left_arg = properties(left_arg);
            let left_arg_prec = left_arg.prec;
            format(w, left_arg, left_arg_prec < prec)?;
            w.write_all(op.as_bytes())?;
            let right_arg = properties(right_arg);
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
            let arg = properties(arg);
            w.write_all(left.as_bytes())?;
            format(w, arg, false)?;
            w.write_all(right.as_bytes())?;
        },
        Function(head, left, arg, right) => {
            let head = properties(head);
            let head_prec = head.prec;
            format(w, head, head_prec < prec)?;
            w.write_all(left.as_bytes())?;
            let arg = properties(arg);
            format(w, arg, false)?;
            w.write_all(right.as_bytes())?;
        },
        UnknownNullary(sym) => {
            warn!("Symbol '{:?}' does not exist in Mathematica", sym);
            write!(w, "{:?}", sym)?;
        },
        UnknownUnary(sym, arg) => {
            warn!("Unary operator '{:?}' does not exist in Mathematica", sym);
            write!(w, "{:?}[", sym)?;
            let arg = properties(arg);
            format(w, arg, false)?;
            write!(w, "]")?;
        },
        UnknownBinary(sym, left, right) => {
            warn!("Binary operator '{:?}' does not exist in Mathematica", sym);
            write!(w, "{:?}[", sym)?;
            let arg = Expression::Binary(BinaryOp::Sequence, Box::new((left, right)));
            let arg = properties(arg);
            format(w, arg, false)?;
            write!(w, "]")?;
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
    Real(&'a [u8]),
    String(&'a [u8]),
    Nullary(&'static str),
    Prefix(&'static str, Expression<'a>),
    Infix(Expression<'a>, &'static str, Expression<'a>),
    Postfix(Expression<'a>, &'static str),
    Circumfix(&'static str, Expression<'a>, &'static str),
    Function(Expression<'a>, &'static str, Expression<'a>, &'static str),
    UnknownNullary(NullaryOp<'a>),
    UnknownUnary(UnaryOp, Expression<'a>),
    UnknownBinary(BinaryOp, Expression<'a>, Expression<'a>),
}

fn properties(
    expression: Expression<'_>
) -> ExpressionProperties<'_> {
    use Expression::{Unary, Binary};
    use ExpressionKind::*;
    let (prec, kind) = match expression {
        Expression::Nullary(nullary) => match nullary {
            NullaryOp::Empty => (PREC_ATOM, Empty),
            NullaryOp::Integer(i) => (PREC_ATOM, Integer(i)),
            NullaryOp::Real(r) => (PREC_ATOM, Real(r)),
            NullaryOp::Symbol(s) => (PREC_ATOM, Symbol(s)),
            NullaryOp::String(s) => (PREC_ATOM, String(s)),
            NullaryOp::E => (PREC_ATOM, Nullary("E")),
            NullaryOp::I => (PREC_ATOM, Nullary("I")),
            NullaryOp::Pi => (PREC_ATOM, Nullary("Pi")),
            NullaryOp::Infinity => (PREC_ATOM, Nullary("Infinity")),
            unknown => (PREC_ATOM, UnknownNullary(unknown)),
        },
        Unary(unary, arg) => match unary {
            UnaryOp::Wildcard => (PREC_BLANK, Postfix(*arg, "_")),
            UnaryOp::ManyWildcard => (PREC_BLANK, Postfix(*arg, "__")),
            UnaryOp::Many0Wildcard => (PREC_BLANK, Postfix(*arg, "___")),
            UnaryOp::UPlus => (PREC_PLUS, Prefix("+", *arg)),
            UnaryOp::UMinus => (PREC_UMINUS, Prefix("-", *arg)),
            UnaryOp::Angle => (PREC_LEFT_ANGLE_BRACKET, Circumfix("〈", *arg, "〉")),
            UnaryOp::Ceiling => (PREC_LEFT_CEILING, Circumfix("⌈", *arg, "⌉")),
            UnaryOp::Floor => (PREC_LEFT_FLOOR, Circumfix("⌊", *arg, "⌋")),
            UnaryOp::List => (PREC_LEFT_CURLY_BRACKET, Circumfix("{", *arg, "}")),
            UnaryOp::PrefixIncrement => (PREC_INCREMENT, Prefix("++", *arg)),
            UnaryOp::PrefixDecrement => (PREC_DECREMENT, Prefix("--", *arg)),
            UnaryOp::Del => (PREC_DEL, Prefix("∇", *arg)),
            UnaryOp::Exists => (PREC_EXISTS, Prefix("∃", *arg)),
            UnaryOp::ForAll => (PREC_FOR_ALL, Prefix("∀", *arg)),
            UnaryOp::Get => (PREC_GET, Prefix("<<", *arg)),
            UnaryOp::UMinusPlus => (PREC_UMINUS_PLUS, Prefix("∓", *arg)),
            UnaryOp::Not => (PREC_NOT, Prefix("!", *arg)),
            UnaryOp::NotExists => (PREC_NOT_EXISTS, Prefix("∄", *arg)),
            UnaryOp::UPlusMinus => (PREC_UPLUS_MINUS, Prefix("±", *arg)),
            // Slot => (PREC_SLOT, fix(*arg)),
            // SlotSequence => (PREC_SLOT_SEQUENCE, fix(*arg)),
            UnaryOp::Sqrt => (PREC_SQRT, Function(
                Expression::Nullary(NullaryOp::Symbol(b"Sqrt")),
                "[", *arg, "]"
            )),
            UnaryOp::Transpose => (PREC_SQRT, Function(
                Expression::Nullary(NullaryOp::Symbol(b"Transpose")),
                "[", *arg, "]"
            )),
            UnaryOp::Conjugate => (PREC_SQRT, Function(
                Expression::Nullary(NullaryOp::Symbol(b"Conjugate")),
                "[", *arg, "]"
            )),
            UnaryOp::ConjugateTranspose => (
                PREC_FUNCTION,
                Function(
                    Expression::Nullary(NullaryOp::Symbol(b"Conjugate")),
                    "[", Unary(UnaryOp::Transpose, Box::new(*arg)), "]"
                )
            ),
            UnaryOp::PostfixIncrement => (PREC_INCREMENT, Postfix(*arg, "++")),
            UnaryOp::PostfixDecrement => (PREC_DECREMENT, Postfix(*arg, "--")),
            //Degree => (PREC_DEGREE, Postfix(*arg, "°")),
            UnaryOp::Factorial => (PREC_FACTORIAL, Postfix(*arg, "!")),
            //            SuperDagger => (PREC_SUPER_DAGGER, fix(*arg)),
            UnaryOp::PureFunction => (PREC_FUNCTION_AMP, Postfix(*arg, "&")),
            unknown => (PREC_ATOM, UnknownUnary(unknown, *arg)),
        },
        Binary(binary, args) => {
            let (left, right) = *args;
            match binary {
                BinaryOp::Plus => (PREC_PLUS, Infix(left, "+", right)),
                BinaryOp::Minus => (PREC_MINUS, Infix(left, "-", right)),
                BinaryOp::Times => (PREC_TIMES, Infix(left, "*", right)),
                BinaryOp::Divide => (PREC_DIVIDE, Infix(left, "/", right)),
                BinaryOp::Compound => (PREC_COMPOUND_EXPRESSION, Infix(left, ";", right)),
                BinaryOp::Sequence => (PREC_SEQUENCE, Infix(left, ",", right)),
                BinaryOp::Equals => (PREC_EQUAL, Infix(left, "=", right)),
                BinaryOp::Dot => (PREC_DOT, Infix(left, ".", right)),
                BinaryOp::Power => (PREC_POWER, Infix(left, "^", right)),
                BinaryOp::Function => (PREC_LEFT_BRACKET, Function(left, "[", right, "]")),
                unknown => (PREC_ATOM, UnknownBinary(unknown, left, right)),
            }
        }
    };
    ExpressionProperties{prec, kind}
}
