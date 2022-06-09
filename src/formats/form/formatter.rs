use super::grammar::*;
use crate::expression::*;
use crate::assoc::Assoc;

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
        Empty => (),
        String(sym) => {
            warn!("Encountered string {:?}, which FORM does not support", std::str::from_utf8(sym));
            w.write_all(b"[\"")?;
            w.write_all(sym)?;
            w.write_all(b"\"]")?;
        },
        Symbol(sym) => {
            if is_symbol(sym) {
                w.write_all(sym)?;
            } else {
                warn!("{:?} is not a legal symbol name", std::str::from_utf8(sym));
                w.write_all(b"[")?;
                w.write_all(sym)?;
                w.write_all(b"]")?;
            }
        },
        Integer(i) => w.write_all(i)?,
        Nullary(op) => w.write_all(op)?,
        Prefix(op, arg) => {
            w.write_all(op)?;
            let arg = properties(arg);
            let arg_prec = arg.prec;
            format(w, arg, arg_prec < prec)?;
        },
        Postfix(arg, op) => {
            let arg = properties(arg);
            let arg_prec = arg.prec;
            format(w, arg, arg_prec < prec)?;
            w.write_all(op)?;
        },
        Infix(left_arg, op, right_arg) => {
            // TODO: this assumes left-associativity
            let left_arg = properties(left_arg);
            let left_arg_prec = left_arg.prec;
            format(w, left_arg, left_arg_prec < prec)?;
            w.write_all(op)?;
            let right_arg = properties(right_arg);
            let right_arg_prec = right_arg.prec;
            format(w, right_arg, right_arg_prec <= prec)?;
        },
        Circumfix(left, arg, right) => {
            let arg = properties(arg);
            w.write_all(left)?;
            format(w, arg, false)?;
            w.write_all(right)?;
        },
        Function(head, left, arg, right) => {
            let head = properties(head);
            let head_prec = head.prec;
            format(w, head, head_prec < prec)?;
            w.write_all(left)?;
            let arg = properties(arg);
            format(w, arg, false)?;
            w.write_all(right)?;
        },
        UnknownNullary(sym) => {
            warn!("Symbol '{:?}' does not exist in FORM", sym);
            write!(w, "{:?}", sym)?;
        },
        UnknownUnary(sym, arg) => {
            warn!("Unary operator '{:?}' does not exist in FORM", sym);
            write!(w, "{:?}(", sym)?;
            let arg = properties(arg);
            format(w, arg, false)?;
            write!(w, ")")?;
        },
        UnknownBinary(sym, left, right) => {
            warn!("Binary operator '{:?}' does not exist in FORM", sym);
            write!(w, "{:?}(", sym)?;
            let arg = Expression::Binary(BinaryOp::Sequence, Box::new((left, right)));
            let arg = properties(arg);
            format(w, arg, false)?;
            write!(w, ")")?;
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
    String(&'a [u8]),
    Symbol(&'a [u8]),
    Nullary(&'static [u8]),
    Prefix(&'static [u8], Expression<'a>),
    Infix(Expression<'a>, &'static [u8], Expression<'a>),
    Postfix(Expression<'a>, &'static [u8]),
    Circumfix(&'static [u8], Expression<'a>, &'static [u8]),
    Function(Expression<'a>, &'static [u8], Expression<'a>, &'static [u8]),
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
            NullaryOp::Symbol(s) => (PREC_ATOM, Symbol(s)),
            NullaryOp::String(s) => (PREC_ATOM, String(s)),
            NullaryOp::Ellipsis => (PREC_ATOM, Nullary(b"...")),
            NullaryOp::Pi => (PREC_ATOM, Nullary(b"pi_")),
            NullaryOp::I => (PREC_ATOM, Nullary(b"i_")),
            NullaryOp::Log => (PREC_ATOM, Nullary(b"ln_")),
            NullaryOp::Exp => (PREC_ATOM, Nullary(b"exp_")),
            NullaryOp::Sign => (PREC_ATOM, Nullary(b"sig_")),
            NullaryOp::Sin => (PREC_ATOM, Nullary(b"sin_")),
            NullaryOp::Cos => (PREC_ATOM, Nullary(b"cos_")),
            NullaryOp::Tan => (PREC_ATOM, Nullary(b"tan_")),
            NullaryOp::Sinh => (PREC_ATOM, Nullary(b"sinh_")),
            NullaryOp::Cosh => (PREC_ATOM, Nullary(b"cosh_")),
            NullaryOp::Tanh => (PREC_ATOM, Nullary(b"tanh_")),
            NullaryOp::ASin => (PREC_ATOM, Nullary(b"asin_")),
            NullaryOp::ACos => (PREC_ATOM, Nullary(b"acos_")),
            NullaryOp::ATan => (PREC_ATOM, Nullary(b"atan_")),
            NullaryOp::ASinh => (PREC_ATOM, Nullary(b"asinh_")),
            NullaryOp::ACosh => (PREC_ATOM, Nullary(b"acosh_")),
            NullaryOp::ATanh => (PREC_ATOM, Nullary(b"atanh_")),
            NullaryOp::Sqrt => (PREC_ATOM, Nullary(b"sqrt_")),
            unknown => (PREC_ATOM, UnknownNullary(unknown)),
        },
        Unary(unary, arg) => match unary {
            UnaryOp::Bracket => (PREC_LEFT_BRACKET, Circumfix(b"(",*arg,b")")),
            UnaryOp::Wildcard => (PREC_ATOM, Postfix(*arg, b"?")),
            UnaryOp::Many0Wildcard => (PREC_ATOM, Prefix(b"?", *arg)),
            UnaryOp::UPlus => (PREC_UPLUS, Prefix(b"+",*arg)),
            UnaryOp::UMinus => (PREC_UMINUS, Prefix(b"-",*arg)),
            unknown => (PREC_ATOM, UnknownUnary(unknown, *arg)),
        },
        Binary(binary, args) => {
            let (left, right) = *args;
            match assoc(binary) {
                Assoc::Right | Assoc::None =>
                    if let Binary(left_op, _) = left {
                        if left_op == binary {
                            let left = Expression::Unary(UnaryOp::Bracket, Box::new(left));
                            return properties(
                                Expression::Binary(binary, Box::new((left, right)))
                            );
                        }
                    },
                Assoc::Left => { },
            };
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
                unknown => (PREC_ATOM, UnknownBinary(unknown, left, right)),
            }
        }
    };
    ExpressionProperties{prec, kind}
}
