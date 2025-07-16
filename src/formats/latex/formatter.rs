// TODO: line length inside \frac
use super::grammar::*;
use crate::assoc::Assoc;
use crate::cfg::{BracketSizing, CFG, LatexOutputCfg};
use crate::expression::*;

use std::collections::HashMap;
use std::io::{self, Write};
use std::mem::swap;

use aho_corasick::AhoCorasick;

const NEWLINE: &[u8] = b"\\\\\n";

pub type Result = std::result::Result<(), std::io::Error>;

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct Formatter<'a> {
    expression: Expression<'a>,
}

impl<'a> Formatter<'a> {
    pub fn new(expression: Expression<'a>) -> Self {
        Formatter { expression }
    }

    pub fn format<W: io::Write>(self, w: &mut W) -> Result {
        Printer::new().format(w, self.expression)
    }
}

#[derive(Clone, Debug)]
struct Printer {
    cfg: &'static LatexOutputCfg,
    linebreak_allowed: bool,
    subscript_level: usize,
    open_brackets: usize,
    line: Vec<u8>,
    cur_line_len: f64,
    align_finder: AhoCorasick,
}

const MIN_FRAC_BRACKET_LEVEL: u32 = 3;

impl Printer {
    fn new() -> Self {
        Printer {
            cfg: &CFG.latex_output,
            linebreak_allowed: true,
            subscript_level: 0,
            open_brackets: 0,
            line: Vec::with_capacity(2 * CFG.latex_output.line_length),
            cur_line_len: 0.,
            align_finder: AhoCorasick::new(&CFG.latex_output.align_at).unwrap(),
        }
    }

    fn into_unbreakable(mut self) -> Self {
        self.linebreak_allowed = false;
        self
    }

    pub(super) fn write_line<W: io::Write>(&mut self, w: &mut W) -> Result {
        debug!("dumping line {:?}", std::str::from_utf8(&self.line));
        let align_pos = if let Some(mat) = self.align_finder.find(&self.line) {
            mat.end()
        } else if let Some(pos) =
            self.line.windows(NEWLINE.len()).position(|w| w == NEWLINE)
        {
            pos + NEWLINE.len()
        } else {
            0
        };
        self.line.insert(align_pos, b'&');
        w.write_all(&self.line)?;
        //TODO: adjust indentation level depending on number of open brackets
        Ok(())
    }

    fn add_linebreak(&mut self) -> Result {
        use BracketSizing::LeftRight;
        self.line.clear();
        if self.cfg.bracket_sizing == LeftRight {
            for _ in 0..self.open_brackets {
                self.line.write_all(br"\right.")?;
            }
        }
        if !self.cfg.tags {
            self.line.write_all(br"\notag")?;
        }
        self.line.write_all(NEWLINE)?;
        for _ in 0..self.open_brackets {
            self.line.write_all(self.cfg.indent_with.as_bytes())?;
        }
        if self.cfg.bracket_sizing == LeftRight {
            for _ in 0..self.open_brackets {
                self.line.write_all(br"\left.")?;
            }
        }
        self.cur_line_len = 0.;
        Ok(())
    }

    fn write_all<W: io::Write>(&mut self, w: &mut W, buf: &[u8]) -> Result {
        if self.cur_line_len >= self.cfg.line_length as f64
            && self
                .cfg
                .line_break_before
                .iter()
                .any(|s| s.as_bytes() == buf)
            && self.linebreak_allowed
            && self.subscript_level == 0
        {
            self.write_line(w)?;
            self.add_linebreak()?;
        }
        self.line.write_all(buf)?;
        Ok(())
    }

    fn bracket_size(&self, bracket_level: u32) -> &'static [u8] {
        let size = (2 * bracket_level as usize) / self.cfg.bracket_types.len();
        match size {
            0 => b"",
            1 => br"\big",
            2 => br"\Big",
            3 => br"\bigg",
            _ => br"\Bigg",
        }
    }

    fn write_left_bracket<W: io::Write>(
        &mut self,
        w: &mut W,
        bracket: &[u8],
        bracket_level: u32,
    ) -> Result {
        match self.cfg.bracket_sizing {
            BracketSizing::LeftRight => self.write_all(w, br"\left")?,
            BracketSizing::Incremental => {
                self.write_all(w, self.bracket_size(bracket_level))?
            }
            BracketSizing::None => {}
        };
        self.write_all(w, bracket)?;
        self.open_brackets += 1;
        Ok(())
    }

    fn write_right_bracket<W: io::Write>(
        &mut self,
        w: &mut W,
        bracket: &[u8],
        bracket_level: u32,
    ) -> Result {
        match self.cfg.bracket_sizing {
            BracketSizing::LeftRight => self.write_all(w, br"\right")?,
            BracketSizing::Incremental => {
                self.write_all(w, self.bracket_size(bracket_level))?
            }
            BracketSizing::None => {}
        };
        self.write_all(w, bracket)?;
        self.open_brackets -= 1;
        Ok(())
    }

    fn write_maybe_left_bracket<W: io::Write>(
        &mut self,
        w: &mut W,
        expr: &[u8],
        bracket_level: u32,
    ) -> Result {
        if is_left_bracket(expr) {
            self.write_left_bracket(w, expr, bracket_level)
        } else {
            self.write_all(w, expr)
        }
    }

    fn write_maybe_right_bracket<W: io::Write>(
        &mut self,
        w: &mut W,
        expr: &[u8],
        bracket_level: u32,
    ) -> Result {
        if is_right_bracket(expr) {
            self.write_right_bracket(w, expr, bracket_level)
        } else {
            self.write_all(w, expr)
        }
    }

    fn format<W: io::Write>(
        &mut self,
        w: &mut W,
        prop: Expression<'_>,
    ) -> Result {
        self.write_buf(w, self.properties(prop))?;
        // ensure last line is also written
        // if it's also the first line (first character is &),
        // then skip the alignment character
        let mut buf = Vec::new();
        self.write_line(&mut buf)?;
        if let Some(b'&') = buf.first() {
            w.write_all(&buf[1..])
        } else {
            w.write_all(&buf)
        }
    }

    fn add_to_line_len(&mut self, len: usize) {
        self.cur_line_len +=
            self.cfg.subscript_size.powi(self.subscript_level as i32)
                * (len as f64);
    }

    fn write_buf<W: io::Write>(
        &mut self,
        w: &mut W,
        prop: ExpressionProperties<'_>,
    ) -> Result {
        use ExpressionKind::*;
        match prop.kind {
            Empty => (),
            Number(n) => {
                self.write_all(w, n)?;
                self.add_to_line_len(n.len())
            }
            String(s) => {
                self.write_all(w, b"\\text{")?;
                self.write_all(w, s)?;
                self.write_all(w, b"}")?;
                self.add_to_line_len(s.len())
            }
            Symbol(s) => {
                if let Some(s) = LATEX_SYMBOLS.get(s) {
                    self.write_all(w, s)?;
                    self.add_to_line_len(s.len())
                } else if s.first() == Some(&b'\\') || s.len() == 1 {
                    self.write_all(w, s)?;
                    self.add_to_line_len(s.len())
                } else {
                    self.write_all(w, b"\\text{")?;
                    self.write_all(w, s)?;
                    self.write_all(w, b"}")?;
                    self.add_to_line_len(s.len())
                }
            }
            Nullary(op) => self.write_all(w, op)?,
            Prefix(op, arg) => {
                self.write_all(w, op)?;
                self.add_to_line_len(op.len());
                self.write_buf(w, *arg)?;
            }
            Postfix(arg, op) => {
                self.write_buf(w, *arg)?;
                self.write_all(w, op)?;
                self.add_to_line_len(op.len());
            }
            Infix(left_arg, op, right_arg) => {
                self.write_buf(w, *left_arg)?;
                self.write_all(w, op)?;
                self.add_to_line_len(op.len());
                self.write_buf(w, *right_arg)?;
            }
            Circumfix(left, arg, right) => {
                let bracket_level = arg.bracket_level;
                self.write_maybe_left_bracket(w, left, bracket_level)?;
                self.write_buf(w, *arg)?;
                self.write_maybe_right_bracket(w, right, bracket_level)?;
            }
            Function(head, left, arg, right) => {
                let bracket_level = arg.bracket_level;
                if self.cfg.line_break_in_argument {
                    self.write_buf(w, *head)?;
                    self.write_maybe_left_bracket(w, left, bracket_level)?;
                    self.write_buf(w, *arg)?;
                    self.write_maybe_right_bracket(w, right, bracket_level)?;
                } else {
                    let mut printer = self.clone().into_unbreakable();
                    printer.write_buf(w, *head)?;
                    printer.write_maybe_left_bracket(w, left, bracket_level)?;
                    printer.write_buf(w, *arg)?;
                    printer.write_maybe_right_bracket(
                        w,
                        right,
                        bracket_level,
                    )?;
                    swap(&mut self.line, &mut printer.line);
                    swap(&mut self.cur_line_len, &mut printer.cur_line_len);
                }
            }
            SubOrSuper(left_arg, op, right_arg) => {
                //self.write_all(w, b"{")?;
                self.write_buf(w, *left_arg)?;
                //self.write_all(w, b"}")?;
                self.write_all(w, op)?;
                self.subscript_level += 1;
                self.write_all(w, b"{")?;
                self.write_buf(w, *right_arg)?;
                self.write_all(w, b"}")?;
                self.subscript_level -= 1;
            }
            Frac(head, num, sep, den, term) => {
                let len_before = self.cur_line_len;
                let mut printer = self.clone().into_unbreakable();
                printer.write_all(w, head)?;
                printer.write_buf(w, *num)?;
                let num_len = printer.cur_line_len - len_before;
                printer.write_all(w, sep)?;
                printer.write_buf(w, *den)?;
                printer.write_all(w, term)?;
                let den_len = printer.cur_line_len - len_before - num_len;
                swap(&mut self.line, &mut printer.line);
                self.cur_line_len += num_len.max(den_len);
            }
            UnknownNullary(sym) => {
                let mut buf = Vec::new();
                write!(buf, "\\text{{{sym:?}}}")?;
                self.write_all(w, &buf)?;
                self.add_to_line_len(buf.len() - 7);
            }
            UnknownUnary(sym, arg) => {
                let bracket_level = arg.bracket_level;
                let mut buf = Vec::new();
                write!(buf, "\\text{{{sym:?}}}")?;
                self.write_all(w, &buf)?;
                self.add_to_line_len(buf.len() - 7);
                self.write_left_bracket(w, b"(", bracket_level)?;
                self.write_buf(w, *arg)?;
                self.write_right_bracket(w, b")", bracket_level)?;
            }
            UnknownBinary(sym, left, right) => {
                let mut buf = Vec::new();
                write!(buf, "\\text{{{sym:?}}}")?;
                self.write_all(w, &buf)?;
                self.add_to_line_len(buf.len() - 7);
                let bracket_level =
                    std::cmp::max(left.bracket_level, right.bracket_level);
                self.write_left_bracket(w, b"(", bracket_level)?;
                let arg = ExpressionProperties {
                    prec: PREC_SEQUENCE,
                    kind: Infix(left, b",", right),
                    bracket_level,
                };
                self.write_buf(w, arg)?;
                self.write_right_bracket(w, b")", bracket_level)?;
            }
        };
        Ok(())
    }

    fn properties<'a>(
        &self,
        expression: Expression<'a>,
    ) -> ExpressionProperties<'a> {
        use Expression::{Binary, Unary};
        use ExpressionKind::*;
        let (prec, kind, bracket_level) = match expression {
            Expression::Nullary(nullary) => {
                let kind = match nullary {
                    NullaryOp::Empty => Empty,
                    NullaryOp::Integer(n) | NullaryOp::Real(n) => Number(n),
                    NullaryOp::Symbol(s) => Symbol(s),
                    NullaryOp::String(s) => String(s),
                    NullaryOp::E => Nullary(b"e"),
                    NullaryOp::I => Nullary(b"i"),
                    NullaryOp::Pi => Nullary(b"\\pi"),
                    NullaryOp::Infinity => Nullary(b"\\infty"),
                    NullaryOp::Log => Nullary(b"\\log"),
                    NullaryOp::Exp => Nullary(b"\\exp"),
                    NullaryOp::Sign => Nullary(b"\\sgn"),
                    NullaryOp::Sin => Nullary(b"\\sin"),
                    NullaryOp::Cos => Nullary(b"\\cos"),
                    NullaryOp::Tan => Nullary(b"\\tan"),
                    NullaryOp::Sinh => Nullary(b"\\sinh"),
                    NullaryOp::Cosh => Nullary(b"\\cosh"),
                    NullaryOp::Tanh => Nullary(b"\\tanh"),
                    NullaryOp::ASin => Nullary(b"\\arcsin"),
                    NullaryOp::ACos => Nullary(b"\\arccos"),
                    NullaryOp::ATan => Nullary(b"\\arctan"),
                    NullaryOp::ASinh => Nullary(b"\\arcsinh"),
                    NullaryOp::ACosh => Nullary(b"\\arccosh"),
                    NullaryOp::ATanh => Nullary(b"\\arctanh"),
                    NullaryOp::Sqrt => Nullary(b"\\sqrt"),
                    NullaryOp::Ellipsis => Nullary(b"\\dots"),
                    NullaryOp::OverHat => Nullary(b"\\hat"),
                    NullaryOp::OverTilde => Nullary(b"\\tilde"),
                    unknown => UnknownNullary(unknown),
                };
                (PREC_ATOM, kind, 0)
            }
            Unary(unary, arg) => {
                let arg_prop = Box::new(self.properties(*arg));
                let mut bracket_level = outer_bracket_level(&arg_prop);
                let (prec, kind) = match unary {
                    UnaryOp::Bracket => (
                        PREC_LEFT_BRACKET,
                        Circumfix(
                            self.left_bracket(bracket_level),
                            arg_prop,
                            self.right_bracket(bracket_level),
                        ),
                    ),
                    // ignore wildcard modifieres
                    UnaryOp::Wildcard
                    | UnaryOp::ManyWildcard
                    | UnaryOp::Many0Wildcard => {
                        (PREC_WILDCARD, Postfix(arg_prop, b""))
                    }
                    UnaryOp::UPlus => (PREC_PLUS, Prefix(b"+", arg_prop)),
                    UnaryOp::UMinus => (PREC_UMINUS, Prefix(b"-", arg_prop)),
                    UnaryOp::Angle => (
                        PREC_LEFT_BRACKET,
                        Circumfix(b"\\langle", arg_prop, b"\\rangle"),
                    ),
                    UnaryOp::Ceiling => (
                        PREC_LEFT_BRACKET,
                        Circumfix(b"\\lceil", arg_prop, b"\\rceil"),
                    ),
                    UnaryOp::Floor => (
                        PREC_LEFT_BRACKET,
                        Circumfix(b"\\lfloor", arg_prop, b"\\rfloor"),
                    ),
                    UnaryOp::List => {
                        (PREC_LEFT_BRACKET, Circumfix(b"\\{", arg_prop, b"\\}"))
                    }
                    UnaryOp::Del => (PREC_DEL, Prefix(b"\\nabla ", arg_prop)),
                    UnaryOp::Exists => {
                        (PREC_EXISTS, Prefix(b"\\exists ", arg_prop))
                    }
                    UnaryOp::ForAll => {
                        (PREC_FOR_ALL, Prefix(b"\\forall ", arg_prop))
                    }
                    UnaryOp::UMinusPlus => {
                        (PREC_UMINUS_PLUS, Prefix(b"\\mp ", arg_prop))
                    }
                    UnaryOp::Not => (PREC_NOT, Prefix(b"!", arg_prop)),
                    UnaryOp::NotExists => {
                        (PREC_NOT_EXISTS, Prefix(b"\\nexists ", arg_prop))
                    }
                    UnaryOp::UPlusMinus => {
                        (PREC_UPLUS_MINUS, Prefix(b"\\pm ", arg_prop))
                    }
                    UnaryOp::Transpose => (
                        PREC_POWER,
                        Infix(
                            arg_prop,
                            b"^",
                            Box::new(self.properties(Expression::Nullary(
                                NullaryOp::Symbol(b"T"),
                            ))),
                        ),
                    ),
                    UnaryOp::Conjugate => {
                        (PREC_ATOM, Circumfix(b"\\overline{", arg_prop, b"}"))
                    }
                    UnaryOp::ConjugateTranspose => (
                        PREC_POWER,
                        Infix(
                            arg_prop,
                            b"^",
                            Box::new(self.properties(Expression::Nullary(
                                NullaryOp::Symbol(b"\\dagger"),
                            ))),
                        ),
                    ),
                    UnaryOp::Degree => {
                        (PREC_DEGREE, Postfix(arg_prop, b"\\deg"))
                    }
                    UnaryOp::Factorial => {
                        (PREC_FACTORIAL, Postfix(arg_prop, b"!"))
                    }
                    UnaryOp::DoubleFactorial => {
                        (PREC_FACTORIAL2, Postfix(arg_prop, b"!!"))
                    }
                    unknown => (PREC_ATOM, UnknownUnary(unknown, arg_prop)),
                };
                // add brackets if necessary
                let kind = match kind {
                    Prefix(op, arg) => {
                        if arg.prec >= prec {
                            Prefix(op, arg)
                        } else {
                            bracket_level += 1;
                            Prefix(op, self.add_bracket(arg))
                        }
                    }
                    Postfix(arg, op) => {
                        if arg.prec >= prec {
                            Postfix(arg, op)
                        } else {
                            bracket_level += 1;
                            Postfix(self.add_bracket(arg), op)
                        }
                    }
                    other => other,
                };
                (prec, kind, bracket_level)
            }
            Binary(binary, args) => {
                let (left, right) = *args;
                if let Assoc::Right | Assoc::None = assoc(binary) {
                    if let Binary(left_op, _) = left {
                        if left_op == binary {
                            let left = Expression::Unary(
                                UnaryOp::Bracket,
                                Box::new(left),
                            );
                            return self.properties(Expression::Binary(
                                binary,
                                Box::new((left, right)),
                            ));
                        }
                    }
                };
                let left = Box::new(self.properties(left));
                let right = Box::new(self.properties(right));
                let mut bracket_level = std::cmp::max(
                    outer_bracket_level(&left),
                    outer_bracket_level(&right),
                );
                let (prec, kind) = match binary {
                    BinaryOp::Plus => (PREC_PLUS, Infix(left, b"+", right)),
                    BinaryOp::Minus => (PREC_MINUS, Infix(left, b"-", right)),
                    BinaryOp::Times => (
                        PREC_TIMES,
                        Infix(
                            left,
                            self.cfg.multiplication_symbol.as_bytes(),
                            right,
                        ),
                    ),
                    BinaryOp::Divide => {
                        let left = self.remove_bracket(left);
                        let right = self.remove_bracket(right);
                        bracket_level = std::cmp::max(
                            outer_bracket_level(&left),
                            outer_bracket_level(&right),
                        );
                        let min_bracket_level = MIN_FRAC_BRACKET_LEVEL
                            * (self.cfg.bracket_types.len() as u32)
                            / 2;
                        let min_bracket_level = self
                            .cfg
                            .subscript_size
                            .powi(self.subscript_level as i32)
                            * (min_bracket_level as f64);
                        let min_bracket_level = min_bracket_level.round();
                        if bracket_level < min_bracket_level as _ {
                            bracket_level = min_bracket_level as _;
                        }
                        (
                            PREC_DIVIDE,
                            Frac(b"\\frac{", left, b"}{", right, b"}"),
                        )
                    }
                    BinaryOp::Compound => {
                        (PREC_COMPOUND_EXPRESSION, Infix(left, b";", right))
                    }
                    BinaryOp::Sequence => {
                        (PREC_SEQUENCE, Infix(left, b",", right))
                    }
                    BinaryOp::Equals => (PREC_EQUAL, Infix(left, b"=", right)),
                    BinaryOp::Dot => (PREC_DOT, Infix(left, b".", right)),
                    BinaryOp::Power | BinaryOp::Superscript => {
                        let right = self.remove_bracket(right);
                        (PREC_POWER, SubOrSuper(left, b"^", right))
                    }
                    BinaryOp::Subscript => {
                        let right = self.remove_bracket(right);
                        (PREC_ATOM, SubOrSuper(left, b"_", right))
                    }
                    BinaryOp::Function => match left.kind {
                        Nullary(b"\\sqrt") => (
                            PREC_LEFT_BRACKET,
                            Circumfix(b"\\sqrt{", right, b"}"),
                        ),
                        Nullary(b"\\hat") => (
                            PREC_LEFT_BRACKET,
                            Circumfix(b"\\hat{", right, b"}"),
                        ),
                        Nullary(b"\\tilde") => (
                            PREC_LEFT_BRACKET,
                            Circumfix(b"\\tilde{", right, b"}"),
                        ),
                        _ => (
                            PREC_LEFT_BRACKET,
                            Function(left, b"(", right, b")"),
                        )
                    }
                    unknown => {
                        (PREC_LEFT_BRACKET, UnknownBinary(unknown, left, right))
                    }
                };
                let kind = match kind {
                    Infix(mut left, op, mut right) => {
                        if left.prec < prec {
                            left = self.add_bracket(left);
                        }
                        if right.prec <= prec {
                            right = self.add_bracket(right);
                        }
                        bracket_level = std::cmp::max(
                            outer_bracket_level(&left),
                            outer_bracket_level(&right),
                        );
                        Infix(left, op, right)
                    }
                    Function(head, open, arg, close) => {
                        if head.prec >= prec {
                            Function(head, open, arg, close)
                        } else {
                            Function(self.add_bracket(head), open, arg, close)
                        }
                    }
                    SubOrSuper(base, op, arg) => {
                        if base.prec >= prec {
                            SubOrSuper(base, op, arg)
                        } else {
                            SubOrSuper(self.add_bracket(base), op, arg)
                        }
                    }
                    other => other,
                };
                (prec, kind, bracket_level)
            }
        };
        ExpressionProperties {
            prec,
            kind,
            bracket_level,
        }
    }

    fn left_bracket(&self, bracket_level: u32) -> &'static [u8] {
        let nbrackets = self.cfg.bracket_types.len();
        self.cfg.bracket_types[(2 * bracket_level as usize) % nbrackets]
            .as_bytes()
    }

    fn right_bracket(&self, bracket_level: u32) -> &'static [u8] {
        let nbrackets = self.cfg.bracket_types.len();
        self.cfg.bracket_types[(1 + 2 * bracket_level as usize) % nbrackets]
            .as_bytes()
    }

    fn remove_bracket<'a>(
        &self,
        expr: Box<ExpressionProperties<'a>>,
    ) -> Box<ExpressionProperties<'a>> {
        let bracket_level = expr.bracket_level;
        match expr.kind {
            ExpressionKind::Circumfix(left, arg, right)
                if left == self.left_bracket(bracket_level)
                    && right == self.right_bracket(bracket_level) =>
            {
                arg
            }
            _ => expr,
        }
    }

    fn add_bracket<'a>(
        &self,
        expr: Box<ExpressionProperties<'a>>,
    ) -> Box<ExpressionProperties<'a>> {
        let bracket_level = expr.bracket_level;
        Box::new(ExpressionProperties {
            prec: PREC_LEFT_BRACKET,
            kind: ExpressionKind::Circumfix(
                self.left_bracket(bracket_level),
                expr,
                self.right_bracket(bracket_level),
            ),
            bracket_level,
        })
    }
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
struct ExpressionProperties<'a> {
    prec: u32,
    kind: ExpressionKind<'a>,
    bracket_level: u32,
}

#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
enum ExpressionKind<'a> {
    Empty,
    Number(&'a [u8]),
    Symbol(&'a [u8]),
    String(&'a [u8]),
    Nullary(&'static [u8]),
    Prefix(&'static [u8], Box<ExpressionProperties<'a>>),
    Infix(
        Box<ExpressionProperties<'a>>,
        &'static [u8],
        Box<ExpressionProperties<'a>>,
    ),
    SubOrSuper(
        Box<ExpressionProperties<'a>>,
        &'static [u8],
        Box<ExpressionProperties<'a>>,
    ),
    Postfix(Box<ExpressionProperties<'a>>, &'static [u8]),
    Circumfix(&'static [u8], Box<ExpressionProperties<'a>>, &'static [u8]),
    Function(
        Box<ExpressionProperties<'a>>,
        &'static [u8],
        Box<ExpressionProperties<'a>>,
        &'static [u8],
    ),
    Frac(
        &'static [u8],
        Box<ExpressionProperties<'a>>,
        &'static [u8],
        Box<ExpressionProperties<'a>>,
        &'static [u8],
    ),
    UnknownNullary(NullaryOp<'a>),
    UnknownUnary(UnaryOp, Box<ExpressionProperties<'a>>),
    UnknownBinary(
        BinaryOp,
        Box<ExpressionProperties<'a>>,
        Box<ExpressionProperties<'a>>,
    ),
}

fn outer_bracket_level(expr: &ExpressionProperties<'_>) -> u32 {
    if expr.prec == PREC_LEFT_BRACKET {
        expr.bracket_level + 1
    } else {
        expr.bracket_level
    }
}

lazy_static! {
    pub(crate) static ref LATEX_SYMBOLS: HashMap<&'static [u8], &'static [u8]> = hashmap! {
        "α".as_bytes() => br"\alpha" as &'static [u8],
        "β".as_bytes() => br"\beta",
        "γ".as_bytes() => br"\gamma",
        "δ".as_bytes() => br"\delta",
        "ε".as_bytes() => br"\epsilon",
        "ζ".as_bytes() => br"\zeta",
        "η".as_bytes() => br"\eta",
        "θ".as_bytes() => br"\theta",
        "ι".as_bytes() => br"\iota",
        "κ".as_bytes() => br"\kappa",
        "λ".as_bytes() => br"\lambda",
        "μ".as_bytes() => br"\mu",
        "ν".as_bytes() => br"\nu",
        "ξ".as_bytes() => br"\xi",
        "ο".as_bytes() => br"\omicron",
        "π".as_bytes() => br"\pi",
        "ρ".as_bytes() => br"\rho",
        "σ".as_bytes() => br"\sigma",
        "τ".as_bytes() => br"\tau",
        "υ".as_bytes() => br"\upsilon",
        "φ".as_bytes() => br"\varphi",
        "ϕ".as_bytes() => br"\phi",
        "χ".as_bytes() => br"\chi",
        "ψ".as_bytes() => br"\psi",
        "ω".as_bytes() => br"\omega",

        "Α".as_bytes() => br"\Alpha",
        "Β".as_bytes() => br"\Beta",
        "Γ".as_bytes() => br"\Gamma",
        "Δ".as_bytes() => br"\Delta",
        "Ε".as_bytes() => br"\Epsilon",
        "Ζ".as_bytes() => br"\Zeta",
        "Θ".as_bytes() => br"\Theta",
        "Η".as_bytes() => br"\Eta",
        "Ι".as_bytes() => br"\Iota",
        "Κ".as_bytes() => br"\Kappa",
        "Λ".as_bytes() => br"\Lambda",
        "Μ".as_bytes() => br"\Mu",
        "Ν".as_bytes() => br"\Nu",
        "Ξ".as_bytes() => br"\Xi",
        "Ο".as_bytes() => br"\Omicron",
        "Π".as_bytes() => br"\Pi",
        "Ρ".as_bytes() => br"\Rho",
        "Σ".as_bytes() => br"\Sigma",
        "Τ".as_bytes() => br"\Tau",
        "Υ".as_bytes() => br"\Upsilon",
        "Φ".as_bytes() => br"\Phi",
        "Χ".as_bytes() => br"\Chi",
        "Ψ".as_bytes() => br"\Psi",
        "Ω".as_bytes() => br"\Omega",
    };
}
