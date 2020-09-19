use super::grammar::*;
use crate::assoc::Assoc;
use crate::expression::*;

use std::io;
use std::collections::HashMap;

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
        Printer::new().format(w, properties(self.expression), false)
    }

}

struct Printer{
    inside_sub_or_super: bool,
}

impl Printer {
    fn new() -> Self {
        Printer{
            inside_sub_or_super: false
        }
    }

    fn inside_sub_or_super() -> Self {
        Printer{
            inside_sub_or_super: false
        }
    }

    fn write_bracket<W: io::Write>(&self, w: &mut W, bracket: &[u8]) -> Result {
        w.write_all(bracket)
    }

    fn write_maybe_bracket<W: io::Write>(&self, w: &mut W, expr: &[u8]) -> Result {
        if is_bracket(expr) {
            self.write_bracket(w, expr)
        } else {
            w.write_all(expr)
        }
    }

    fn format<W: io::Write>(
        &self,
        w: &mut W,
        prop: ExpressionProperties<'_>,
        with_paren: bool
    ) -> Result {
        let prec = prop.prec;
        if with_paren {
            self.write_bracket(w, b"(")?;
        }
        use ExpressionKind::*;
        match prop.kind {
            Empty => (),
            Number(n) => w.write_all(n)?,
            String(s) => {
                w.write_all(b"\\text{")?;
                w.write_all(s)?;
                w.write_all(b"}")?;
            },
            Symbol(s) => {
                if let Some(s) = LATEX_SYMBOLS.get(s) {
                    w.write_all(s)?
                } else if s.first() == Some(&b'\\') || s.len() == 1 {
                    w.write_all(s)?
                } else {
                    w.write_all(b"\\text{")?;
                    w.write_all(s)?;
                    w.write_all(b"}")?;
                }
            },
            Nullary(op) => w.write_all(op)?,
            Prefix(op, arg) => {
                w.write_all(op)?;
                let arg = properties(arg);
                let arg_prec = arg.prec;
                self.format(w, arg, arg_prec < prec)?;
            },
            Postfix(arg, op) => {
                let arg = properties(arg);
                let arg_prec = arg.prec;
                self.format(w, arg, arg_prec < prec)?;
                w.write_all(op)?;
            },
            Infix(left_arg, b"^", right_arg) => {
                let left_arg = properties(left_arg);
                let left_arg_prec = left_arg.prec;
                w.write_all(b"{")?;
                self.format(w, left_arg, left_arg_prec < prec)?;
                w.write_all(b"}^{")?;
                let right_arg = properties(right_arg);
                Self::inside_sub_or_super().format(w, right_arg, false)?;
                w.write_all(b"}")?;
            },
            Infix(left_arg, op, right_arg) => {
                let left_arg = properties(left_arg);
                let left_arg_prec = left_arg.prec;
                self.format(w, left_arg, left_arg_prec < prec)?;
                w.write_all(op)?;
                let right_arg = properties(right_arg);
                let right_arg_prec = right_arg.prec;
                self.format(w, right_arg, right_arg_prec <= prec)?;
            },
            Circumfix(left, arg, right) => {
                let arg = properties(arg);
                self.write_maybe_bracket(w, left)?;
                self.format(w, arg, false)?;
                self.write_maybe_bracket(w, right)?;
            },
            Function(head, left, arg, right) => {
                let head = properties(head);
                let head_prec = head.prec;
                self.format(w, head, head_prec < prec)?;
                self.write_maybe_bracket(w, left)?;
                let arg = properties(arg);
                self.format(w, arg, false)?;
                self.write_maybe_bracket(w, right)?;
            },
            Frac(head, num, sep, den, term) => {
                w.write_all(head)?;
                let num = remove_bracket(num);
                Self::inside_sub_or_super().format(w, properties(num), false)?;
                w.write_all(sep)?;
                let den = remove_bracket(den);
                Self::inside_sub_or_super().format(w, properties(den), false)?;
                w.write_all(term)?;
            },
            UnknownNullary(sym) => {
                write!(w, "\\text{{{:?}}}", sym)?;
            },
            UnknownUnary(sym, arg) => {
                write!(w, "\\text{{{:?}}}", sym)?;
                self.write_bracket(w, b"(")?;
                let arg = properties(arg);
                self.format(w, arg, false)?;
                self.write_bracket(w, b")")?;
            },
            UnknownBinary(sym, left, right) => {
                write!(w, "\\text{{{:?}}}", sym)?;
                self.write_bracket(w, b"(")?;
                let arg = Expression::Binary(BinaryOp::Sequence, Box::new((left, right)));
                let arg = properties(arg);
                self.format(w, arg, false)?;
                self.write_bracket(w, b")")?;
            },
        };
        if with_paren {
            self.write_bracket(w, b")")?;
        }
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
    Number(&'a [u8]),
    Symbol(&'a [u8]),
    String(&'a [u8]),
    Nullary(&'static[u8]),
    Prefix(&'static[u8], Expression<'a>),
    Infix(Expression<'a>, &'static[u8], Expression<'a>),
    Postfix(Expression<'a>, &'static[u8]),
    Circumfix(&'static[u8], Expression<'a>, &'static[u8]),
    Function(Expression<'a>, &'static[u8], Expression<'a>, &'static[u8]),
    Frac(&'static[u8], Expression<'a>, &'static[u8], Expression<'a>, &'static[u8]),
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
            NullaryOp::Integer(n) | NullaryOp::Real(n) => (PREC_ATOM, Number(n)),
            NullaryOp::Symbol(s) => (PREC_ATOM, Symbol(s)),
            NullaryOp::String(s) => (PREC_ATOM, String(s)),
            NullaryOp::E => (PREC_ATOM, Nullary(b"e")),
            NullaryOp::I => (PREC_ATOM, Nullary(b"i")),
            NullaryOp::Pi => (PREC_ATOM, Nullary(b"\\pi")),
            NullaryOp::Infinity => (PREC_ATOM, Nullary(b"\\infty")),
            NullaryOp::Log => (PREC_ATOM, Nullary(b"\\log")),
            NullaryOp::Exp => (PREC_ATOM, Nullary(b"\\exp")),
            NullaryOp::Sin => (PREC_ATOM, Nullary(b"\\sin")),
            NullaryOp::Cos => (PREC_ATOM, Nullary(b"\\cos")),
            NullaryOp::Tan => (PREC_ATOM, Nullary(b"\\tan")),
            NullaryOp::Sinh => (PREC_ATOM, Nullary(b"\\sinh")),
            NullaryOp::Cosh => (PREC_ATOM, Nullary(b"\\cosh")),
            NullaryOp::Tanh => (PREC_ATOM, Nullary(b"\\tanh")),
            NullaryOp::ASin => (PREC_ATOM, Nullary(b"\\arcsin")),
            NullaryOp::ACos => (PREC_ATOM, Nullary(b"\\arccos")),
            NullaryOp::ATan => (PREC_ATOM, Nullary(b"\\arctan")),
            NullaryOp::ASinh => (PREC_ATOM, Nullary(b"\\arcsinh")),
            NullaryOp::ACosh => (PREC_ATOM, Nullary(b"\\arccosh")),
            NullaryOp::ATanh => (PREC_ATOM, Nullary(b"\\arctanh")),
            NullaryOp::Sqrt => (PREC_ATOM, Nullary(b"\\sqrt")),
            unknown => (PREC_ATOM, UnknownNullary(unknown)),
        },
        Unary(unary, arg) => match unary {
            UnaryOp::Bracket => (PREC_LEFT_BRACKET, Circumfix(b"(", *arg, b")")),
            // ignore wildcard modifieres
            UnaryOp::Wildcard
                | UnaryOp::ManyWildcard
                | UnaryOp::Many0Wildcard
                => (PREC_WILDCARD, Postfix(*arg, b"")),
            UnaryOp::UPlus => (PREC_PLUS, Prefix(b"+", *arg)),
            UnaryOp::UMinus => (PREC_UMINUS, Prefix(b"-", *arg)),
            UnaryOp::Angle => (PREC_LEFT_BRACKET, Circumfix(b"\\langle", *arg, b"\\rangle")),
            UnaryOp::Ceiling => (PREC_LEFT_BRACKET, Circumfix(b"\\lceil", *arg, b"\\rceil")),
            UnaryOp::Floor => (PREC_LEFT_BRACKET, Circumfix(b"\\lfloor", *arg, b"\\rfloor")),
            UnaryOp::List => (PREC_LEFT_BRACKET, Circumfix(b"\\{", *arg, b"\\}")),
            UnaryOp::Del => (PREC_DEL, Prefix(b"\\nabla", *arg)),
            UnaryOp::Exists => (PREC_EXISTS, Prefix(b"\\exists", *arg)),
            UnaryOp::ForAll => (PREC_FOR_ALL, Prefix(b"\\forall", *arg)),
            UnaryOp::UMinusPlus => (PREC_UMINUS_PLUS, Prefix(b"\\mp", *arg)),
            UnaryOp::Not => (PREC_NOT, Prefix(b"!", *arg)),
            UnaryOp::NotExists => (PREC_NOT_EXISTS, Prefix(b"\\nexists", *arg)),
            UnaryOp::UPlusMinus => (PREC_UPLUS_MINUS, Prefix(b"\\pm", *arg)),
            UnaryOp::Transpose => (
                PREC_POWER,
                Infix(*arg, b"^", Expression::Nullary(NullaryOp::Symbol(b"T")))
            ),
            UnaryOp::Conjugate => (
                PREC_ATOM,
                Circumfix(b"\\overline{", *arg, b"}")
            ),
            UnaryOp::ConjugateTranspose => (
                PREC_POWER,
                Infix(*arg, b"^", Expression::Nullary(NullaryOp::Symbol(b"\\dagger")))
            ),
            UnaryOp::Degree => (PREC_DEGREE, Postfix(*arg, b"\\deg")),
            UnaryOp::Factorial => (PREC_FACTORIAL, Postfix(*arg, b"!")),
            UnaryOp::DoubleFactorial => (PREC_FACTORIAL2, Postfix(*arg, b"!!")),
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
                BinaryOp::Times => (PREC_TIMES, Infix(left, b"\\*", right)),
                BinaryOp::Divide => (PREC_DIVIDE, Frac(b"\\frac{", left, b"}{", right, b"}")),
                BinaryOp::Compound => (PREC_COMPOUND_EXPRESSION, Infix(left, b";", right)),
                BinaryOp::Sequence => (PREC_SEQUENCE, Infix(left, b",", right)),
                BinaryOp::Equals => (PREC_EQUAL, Infix(left, b"=", right)),
                BinaryOp::Dot => (PREC_DOT, Infix(left, b".", right)),
                BinaryOp::Power => (PREC_POWER, Infix(left, b"^", right)),
                BinaryOp::Function => {
                    if left == Expression::Nullary(NullaryOp::Sqrt) {
                        (PREC_LEFT_BRACKET, Circumfix(b"\\sqrt{", right, b"}"))
                    } else {
                        (PREC_LEFT_BRACKET, Function(left, b"(", right, b")"))
                    }
                },
                unknown => (PREC_ATOM, UnknownBinary(unknown, left, right)),
            }
        }
    };
    ExpressionProperties{prec, kind}
}

fn remove_bracket(expr: Expression<'_>) -> Expression<'_> {
    if let Expression::Unary(UnaryOp::Bracket, arg) = expr {
        *arg
    } else {
        expr
    }
}

lazy_static! {
    pub(crate) static ref LATEX_SYMBOLS: HashMap<&'static [u8], &'static [u8]> = hashmap!{
        "α".as_bytes() => b"\\alpha" as &'static [u8],
        "β".as_bytes() => b"\\beta",
        "γ".as_bytes() => b"\\gamma",
        "δ".as_bytes() => b"\\delta",
        "ε".as_bytes() => b"\\epsilon",
        "ζ".as_bytes() => b"\\zeta",
        "η".as_bytes() => b"\\eta",
        "θ".as_bytes() => b"\\theta",
        "ι".as_bytes() => b"\\iota",
        "κ".as_bytes() => b"\\kappa",
        "λ".as_bytes() => b"\\lambda",
        "μ".as_bytes() => b"\\mu",
        "ν".as_bytes() => b"\\nu",
        "ξ".as_bytes() => b"\\xi",
        "ο".as_bytes() => b"\\omicron",
        "π".as_bytes() => b"\\pi",
        "ρ".as_bytes() => b"\\rho",
        "σ".as_bytes() => b"\\sigma",
        "τ".as_bytes() => b"\\tau",
        "υ".as_bytes() => b"\\upsilon",
        "φ".as_bytes() => b"\\phi",
        "χ".as_bytes() => b"\\chi",
        "ψ".as_bytes() => b"\\psi",
        "ω".as_bytes() => b"\\omega",

        "Α".as_bytes() => b"\\Alpha",
        "Β".as_bytes() => b"\\Beta",
        "Γ".as_bytes() => b"\\Gamma",
        "Δ".as_bytes() => b"\\Delta",
        "Ε".as_bytes() => b"\\Epsilon",
        "Ζ".as_bytes() => b"\\Zeta",
        "θ".as_bytes() => b"\\Theta",
        "Η".as_bytes() => b"\\Eta",
        "Ι".as_bytes() => b"\\Iota",
        "Κ".as_bytes() => b"\\Kappa",
        "Λ".as_bytes() => b"\\Lambda",
        "Μ".as_bytes() => b"\\Mu",
        "Ν".as_bytes() => b"\\Nu",
        "Ξ".as_bytes() => b"\\Xi",
        "Ο".as_bytes() => b"\\Omicron",
        "Π".as_bytes() => b"\\Pi",
        "Ρ".as_bytes() => b"\\Rho",
        "Σ".as_bytes() => b"\\Sigma",
        "Τ".as_bytes() => b"\\Tau",
        "Υ".as_bytes() => b"\\Upsilon",
        "Φ".as_bytes() => b"\\Phi",
        "Χ".as_bytes() => b"\\Chi",
        "Ψ".as_bytes() => b"\\Psi",
        "Ω".as_bytes() => b"\\Omega",
    };
}
