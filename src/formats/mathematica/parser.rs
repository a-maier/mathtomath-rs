//TODO: code duplication
use super::grammar::*;
use super::lexer::Lexer;
use super::tokens::{Token, StaticToken, TOKEN_PREC, TOKEN_EXPRESSION, UNKNOWN_TOKEN_PREC, NULL_ARITY, LEFT_ARITY, CLOSING_BRACKET};
use crate::error::{SyntaxError, ErrorKind::*};
use crate::expression::{Expression};
use crate::arity::Arity;

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
        while right_binding_power < binding_power(*next) {
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
        debug!("null called on token {:?}", token);
        if let Some(token) = token {
            match token {
                Token::Symbol(name) => Ok(Symbol(name)),
                Token::Integer(int) => Ok(Integer(int)),
                Token::String(s) => Ok(String(s)),
                Token::Real(x) => Ok(Real(x)),
                Token::Static(s) => {
                    use Arity::*;
                    match NULL_ARITY.get(&s) {
                        Some(Nullary) => Ok(TOKEN_EXPRESSION[&s].clone()),
                        Some(Unary) => if let Some(closing) = CLOSING_BRACKET.get(&s) {
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
                            let prec = binding_power(Some(token));
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
            Ok(Expression::Empty)
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
                    let right_binding_power = binding_power(token);
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

fn binding_power<'a>(token: Option<Token<'a>>) -> u32 {
    debug!("look up left binding power of {:?}", token);
    use Token::*;
    if let Some(token) = token {
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

fn bracket_to_expr<'a>(
    opening: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    use Expression::*;
    match opening {
        StaticToken::LeftAngleBracket  => Angle(Box::new(arg)),
        StaticToken::LeftAssociation   => Association(Box::new(arg)),
        StaticToken::LeftBracket       => arg,
        StaticToken::LeftCeiling       => Ceiling(Box::new(arg)),
        StaticToken::LeftFloor         => Floor(Box::new(arg)),
        StaticToken::LeftList          => List(Box::new(arg)),
        _ => panic!("Internal error: {:?} is not a bracket", opening)
    }
}

fn prefix_op_to_expr<'a>(
    op: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    use Expression::*;
    let arg = Box::new(arg);
    match op {
        StaticToken::Decrement => PrefixDecrement(arg),
        StaticToken::Increment => PrefixIncrement(arg),
        StaticToken::Del => Del(arg),
        StaticToken::Exists => Exists(arg),
        StaticToken::ForAll => ForAll(arg),
        StaticToken::Get => Get(arg),
        StaticToken::MinusPlus => UMinusPlus(arg),
        StaticToken::Not => Not(arg),
        StaticToken::NotExists => NotExists(arg),
        StaticToken::Plus => UPlus(arg),
        StaticToken::PlusMinus => UPlusMinus(arg),
        StaticToken::Slot => Slot(arg),
        StaticToken::SlotSequence => SlotSequence(arg),
        StaticToken::Sqrt => Sqrt(arg),
        StaticToken::Square => Square(arg),
        StaticToken::Transpose => Transpose(arg),
        _ => unreachable!("Internal error: prefix operator {:?} to expression", op)
    }
}

fn postfix_op_to_expr<'a>(
    op: StaticToken,
    arg: Expression<'a>
) -> Expression<'a> {
    use Expression::*;
    let arg = Box::new(arg);
    match op {
        StaticToken::Blank => Wildcard(arg),
        StaticToken::BlankNullSequence => Many0Wildcard(arg),
        StaticToken::BlankSequence => ManyWildcard(arg),
        StaticToken::Conjugate => Conjugate(arg),
        StaticToken::ConjugateTranspose => ConjugateTranspose(arg),
        StaticToken::Decrement => PostfixDecrement(arg),
        StaticToken::Degree => Degree(arg),
        StaticToken::Function => PureFunction(arg),
        StaticToken::Increment => PostfixIncrement(arg),
        StaticToken::Not => Factorial(arg),
        StaticToken::SuperDagger => SuperDagger(arg),
        _ => unreachable!("Internal error: postfix operator {:?} to expression", op)
    }
}

fn function_to_expr<'a>(
    op: StaticToken,
    head: Expression<'a>,
    arg: Expression<'a>,
) -> Expression<'a> {
    use Expression::*;
    let args = Box::new((head, arg));
    match op {
        StaticToken::LeftSquareBracket => Function(args),
        StaticToken::LeftPart => Part(args),
        _ => unreachable!("Internal error: function-like operator {:?} to expression", op)
    }
}

fn binary_op_to_expr<'a>(
    op: StaticToken,
    left: Expression<'a>,
    right: Expression<'a>,
) -> Expression<'a> {
    use Expression::*;
    let args = Box::new((left, right));
    match op {
        StaticToken::Pattern => Pattern(args),
        StaticToken::TagSet => TagSet(args),
        StaticToken::TagSetDelayed => TagSetDelayed(args),
        StaticToken::TagUnset => TagUnset(args),
        StaticToken::AddTo => AddTo(args),
        StaticToken::Alternatives => Alternatives(args),
        StaticToken::And => And(args),
        StaticToken::Apply => Apply(args),
        StaticToken::Apply1 => Apply1(args),
        StaticToken::Backslash => Backslash(args),
        StaticToken::Because => Because(args),
        StaticToken::Cap => Cap(args),
        StaticToken::CenterDot => CenterDot(args),
        StaticToken::CircleDot => CircleDot(args),
        StaticToken::CircleMinus => CircleMinus(args),
        StaticToken::CirclePlus => CirclePlus(args),
        StaticToken::CircleTimes => CircleTimes(args),
        StaticToken::Colon => Colon(args),
        StaticToken::Comma => Sequence(args),
        StaticToken::Composition => Composition(args),
        StaticToken::CompoundExpression => Compound(args),
        StaticToken::Condition => Condition(args),
        StaticToken::Conditioned => Conditioned(args),
        StaticToken::Congruent => Congruent(args),
        StaticToken::Coproduct => Coproduct(args),
        StaticToken::Cross => Cross(args),
        StaticToken::Cup => Cup(args),
        StaticToken::CupCap => CupCap(args),
        StaticToken::Diamond => Diamond(args),
        StaticToken::DifferenceDelta => DifferenceDelta(args),
        StaticToken::DirectedEdge => DirectedEdge(args),
        StaticToken::DiscreteRatio => DiscreteRatio(args),
        StaticToken::DiscreteShift => DiscreteShift(args),
        StaticToken::Distributed => Distributed(args),
        StaticToken::Divide => Divide(args),
        StaticToken::DivideBy => DivideBy(args),
        StaticToken::Dot => Dot(args),
        StaticToken::DotEqual => DotEqual(args),
        StaticToken::DoubleDownArrow => DoubleDownArrow(args),
        StaticToken::DoubleLeftTee => DoubleLeftTee(args),
        StaticToken::DoubleRightTee => DoubleRightTee(args),
        StaticToken::DoubleUpArrow => DoubleUpArrow(args),
        StaticToken::DoubleVerticalBar => DoubleVerticalBar(args),
        StaticToken::DownArrow => DownArrow(args),
        StaticToken::DownArrowBar => DownArrowBar(args),
        StaticToken::DownLeftRightVector => DownLeftRightVector(args),
        StaticToken::DownLeftTeeVector => DownLeftTeeVector(args),
        StaticToken::DownLeftVector => DownLeftVector(args),
        StaticToken::DownLeftVectorBar => DownLeftVectorBar(args),
        StaticToken::DownRightTeeVector => DownRightTeeVector(args),
        StaticToken::DownRightVector => DownRightVector(args),
        StaticToken::DownRightVectorBar => DownRightVectorBar(args),
        StaticToken::DownTee => DownTee(args),
        StaticToken::DownTeeArrow => DownTeeArrow(args),
        StaticToken::Element => Element(args),
        StaticToken::Equal => Equal(args),
        StaticToken::EqualTilde => EqualTilde(args),
        StaticToken::Equilibrium => Equilibrium(args),
        StaticToken::Equivalent => Equivalent(args),
        StaticToken::Greater => Greater(args),
        StaticToken::GreaterEqual => GreaterEqual(args),
        StaticToken::GreaterEqualLess => GreaterEqualLess(args),
        StaticToken::GreaterFullEqual => GreaterFullEqual(args),
        StaticToken::GreaterGreater => GreaterGreater(args),
        StaticToken::GreaterLess => GreaterLess(args),
        StaticToken::GreaterSlantEqual => GreaterSlantEqual(args),
        StaticToken::GreaterTilde => GreaterTilde(args),
        StaticToken::HumpDownHump => HumpDownHump(args),
        StaticToken::HumpEqual => HumpEqual(args),
        StaticToken::Implies => Implies(args),
        StaticToken::Increment => Increment(args),
        StaticToken::Infix => Infix(args),
        StaticToken::Intersection => Intersection(args),
        StaticToken::LeftDownTeeVector => LeftDownTeeVector(args),
        StaticToken::LeftDownVector => LeftDownVector(args),
        StaticToken::LeftDownVectorBar => LeftDownVectorBar(args),
        StaticToken::LeftRightVector => LeftRightVector(args),
        StaticToken::LeftTee => LeftTee(args),
        StaticToken::LeftTeeVector => LeftTeeVector(args),
        StaticToken::LeftTriangle => LeftTriangle(args),
        StaticToken::LeftTriangleBar => LeftTriangleBar(args),
        StaticToken::LeftTriangleEqual => LeftTriangleEqual(args),
        StaticToken::LeftUpDownVector => LeftUpDownVector(args),
        StaticToken::LeftUpTeeVector => LeftUpTeeVector(args),
        StaticToken::LeftUpVector => LeftUpVector(args),
        StaticToken::LeftUpVectorBar => LeftUpVectorBar(args),
        StaticToken::LeftVector => LeftVector(args),
        StaticToken::LeftVectorBar => LeftVectorBar(args),
        StaticToken::Less => Less(args),
        StaticToken::LessEqual => LessEqual(args),
        StaticToken::LessEqualGreater => LessEqualGreater(args),
        StaticToken::LessFullEqual => LessFullEqual(args),
        StaticToken::LessGreater => LessGreater(args),
        StaticToken::LessLess => LessLess(args),
        StaticToken::LessSlantEqual => LessSlantEqual(args),
        StaticToken::LessTilde => LessTilde(args),
        StaticToken::Limit => Limit(args),
        StaticToken::LeftList => LeftList(args),
        StaticToken::RightList => RightList(args),
        StaticToken::Map => Map(args),
        StaticToken::MapAll => MapAll(args),
        StaticToken::MaxLimit => MaxLimit(args),
        StaticToken::MessageName => MessageName(args),
        StaticToken::MinLimit => MinLimit(args),
        StaticToken::MinusPlus => MinusPlus(args),
        StaticToken::Nand => Nand(args),
        StaticToken::NestedGreaterGreater => NestedGreaterGreater(args),
        StaticToken::NestedLessLess => NestedLessLess(args),
        StaticToken::NonCommutativeMultiply => NonCommutativeMultiply(args),
        StaticToken::Nor => Nor(args),
        StaticToken::NotCongruent => NotCongruent(args),
        StaticToken::NotCupCap => NotCupCap(args),
        StaticToken::NotDoubleVerticalBar => NotDoubleVerticalBar(args),
        StaticToken::NotElement => NotElement(args),
        StaticToken::NotGreater => NotGreater(args),
        StaticToken::NotGreaterEqual => NotGreaterEqual(args),
        StaticToken::NotGreaterFullEqual => NotGreaterFullEqual(args),
        StaticToken::NotGreaterGreater => NotGreaterGreater(args),
        StaticToken::NotGreaterLess => NotGreaterLess(args),
        StaticToken::NotGreaterSlantEqual => NotGreaterSlantEqual(args),
        StaticToken::NotGreaterTilde => NotGreaterTilde(args),
        StaticToken::NotHumpDownHump => NotHumpDownHump(args),
        StaticToken::NotHumpEqual => NotHumpEqual(args),
        StaticToken::NotLeftTriangle => NotLeftTriangle(args),
        StaticToken::NotLeftTriangleBar => NotLeftTriangleBar(args),
        StaticToken::NotLeftTriangleEqual => NotLeftTriangleEqual(args),
        StaticToken::NotLess => NotLess(args),
        StaticToken::NotLessEqual => NotLessEqual(args),
        StaticToken::NotLessFullEqual => NotLessFullEqual(args),
        StaticToken::NotLessGreater => NotLessGreater(args),
        StaticToken::NotLessLess => NotLessLess(args),
        StaticToken::NotLessSlantEqual => NotLessSlantEqual(args),
        StaticToken::NotLessTilde => NotLessTilde(args),
        StaticToken::NotNestedGreaterGreater => NotNestedGreaterGreater(args),
        StaticToken::NotNestedLessLess => NotNestedLessLess(args),
        StaticToken::NotPrecedes => NotPrecedes(args),
        StaticToken::NotPrecedesEqual => NotPrecedesEqual(args),
        StaticToken::NotPrecedesSlantEqual => NotPrecedesSlantEqual(args),
        StaticToken::NotPrecedesTilde => NotPrecedesTilde(args),
        StaticToken::NotReverseElement => NotReverseElement(args),
        StaticToken::NotRightTriangle => NotRightTriangle(args),
        StaticToken::NotRightTriangleBar => NotRightTriangleBar(args),
        StaticToken::NotRightTriangleEqual => NotRightTriangleEqual(args),
        StaticToken::NotSquareSubset => NotSquareSubset(args),
        StaticToken::NotSquareSubsetEqual => NotSquareSubsetEqual(args),
        StaticToken::NotSquareSuperset => NotSquareSuperset(args),
        StaticToken::NotSquareSupersetEqual => NotSquareSupersetEqual(args),
        StaticToken::NotSubset => NotSubset(args),
        StaticToken::NotSubsetEqual => NotSubsetEqual(args),
        StaticToken::NotSucceeds => NotSucceeds(args),
        StaticToken::NotSucceedsEqual => NotSucceedsEqual(args),
        StaticToken::NotSucceedsSlantEqual => NotSucceedsSlantEqual(args),
        StaticToken::NotSucceedsTilde => NotSucceedsTilde(args),
        StaticToken::NotSuperset => NotSuperset(args),
        StaticToken::NotSupersetEqual => NotSupersetEqual(args),
        StaticToken::NotTilde => NotTilde(args),
        StaticToken::NotTildeEqual => NotTildeEqual(args),
        StaticToken::NotTildeFullEqual => NotTildeFullEqual(args),
        StaticToken::NotTildeTilde => NotTildeTilde(args),
        StaticToken::NotVerticalBar => NotVerticalBar(args),
        StaticToken::Optional => Optional(args),
        StaticToken::Or => Or(args),
        StaticToken::PatternTest => PatternTest(args),
        StaticToken::Piecewise => Piecewise(args),
        StaticToken::Plus => Plus(args),
        StaticToken::PlusMinus => PlusMinus(args),
        StaticToken::Postfix => Postfix(args),
        StaticToken::Power => Power(args),
        StaticToken::Precedes => Precedes(args),
        StaticToken::PrecedesEqual => PrecedesEqual(args),
        StaticToken::PrecedesSlantEqual => PrecedesSlantEqual(args),
        StaticToken::PrecedesTilde => PrecedesTilde(args),
        StaticToken::Prefix => Prefix(args),
        StaticToken::Proportion => Proportion(args),
        StaticToken::Proportional => Proportional(args),
        StaticToken::Repeated => Repeated(args),
        StaticToken::RepeatedNull => RepeatedNull(args),
        StaticToken::ReplaceAll => ReplaceAll(args),
        StaticToken::ReplaceRepeated => ReplaceRepeated(args),
        StaticToken::ReverseElement => ReverseElement(args),
        StaticToken::ReverseEquilibrium => ReverseEquilibrium(args),
        StaticToken::ReverseUpEquilibrium => ReverseUpEquilibrium(args),
        StaticToken::RightDownTeeVector => RightDownTeeVector(args),
        StaticToken::RightDownVector => RightDownVector(args),
        StaticToken::RightDownVectorBar => RightDownVectorBar(args),
        StaticToken::RightTee => RightTee(args),
        StaticToken::RightTeeVector => RightTeeVector(args),
        StaticToken::RightTriangle => RightTriangle(args),
        StaticToken::RightTriangleBar => RightTriangleBar(args),
        StaticToken::RightTriangleEqual => RightTriangleEqual(args),
        StaticToken::RightUpDownVector => RightUpDownVector(args),
        StaticToken::RightUpTeeVector => RightUpTeeVector(args),
        StaticToken::RightUpVector => RightUpVector(args),
        StaticToken::RightUpVectorBar => RightUpVectorBar(args),
        StaticToken::RightVector => RightVector(args),
        StaticToken::RightVectorBar => RightVectorBar(args),
        StaticToken::Rule => Rule(args),
        StaticToken::RuleDelayed => RuleDelayed(args),
        StaticToken::SameQ => SameQ(args),
        StaticToken::Set => Equals(args),
        StaticToken::SetDelayed => SetDelayed(args),
        StaticToken::SmallCircle => SmallCircle(args),
        StaticToken::Span => Span(args),
        StaticToken::SquareIntersection => SquareIntersection(args),
        StaticToken::SquareSubset => SquareSubset(args),
        StaticToken::SquareSubsetEqual => SquareSubsetEqual(args),
        StaticToken::SquareSuperset => SquareSuperset(args),
        StaticToken::SquareSupersetEqual => SquareSupersetEqual(args),
        StaticToken::SquareUnion => SquareUnion(args),
        StaticToken::Star => Star(args),
        StaticToken::StringExpression => StringExpression(args),
        StaticToken::StringJoin => StringJoin(args),
        StaticToken::Subset => Subset(args),
        StaticToken::SubsetEqual => SubsetEqual(args),
        StaticToken::Subtract => Subtract(args),
        StaticToken::SubtractFrom => SubtractFrom(args),
        StaticToken::Succeeds => Succeeds(args),
        StaticToken::SucceedsEqual => SucceedsEqual(args),
        StaticToken::SucceedsSlantEqual => SucceedsSlantEqual(args),
        StaticToken::SucceedsTilde => SucceedsTilde(args),
        StaticToken::SuchThat => SuchThat(args),
        StaticToken::Superset => Superset(args),
        StaticToken::SupersetEqual => SupersetEqual(args),
        StaticToken::Therefore => Therefore(args),
        StaticToken::Tilde => Tilde(args),
        StaticToken::TildeEqual => TildeEqual(args),
        StaticToken::TildeFullEqual => TildeFullEqual(args),
        StaticToken::TildeTilde => TildeTilde(args),
        StaticToken::Times => Times(args),
        StaticToken::TimesBy => TimesBy(args),
        StaticToken::TwoWayRule => TwoWayRule(args),
        StaticToken::UndirectedEdge => UndirectedEdge(args),
        StaticToken::Unequal => Unequal(args),
        StaticToken::Union => Union(args),
        StaticToken::UnionPlus => UnionPlus(args),
        StaticToken::UnsameQ => UnsameQ(args),
        StaticToken::Unset => Unset(args),
        StaticToken::UpArrow => UpArrow(args),
        StaticToken::UpArrowBar => UpArrowBar(args),
        StaticToken::UpEquilibrium => UpEquilibrium(args),
        StaticToken::UpTee => UpTee(args),
        StaticToken::VectorGreater => VectorGreater(args),
        StaticToken::VectorGreaterEqual => VectorGreaterEqual(args),
        StaticToken::VectorLess => VectorLess(args),
        StaticToken::VectorLessEqual => VectorLessEqual(args),
        StaticToken::Vee => Vee(args),
        StaticToken::VerticalBar => VerticalBar(args),
        StaticToken::VerticalSeparator => VerticalSeparator(args),
        StaticToken::VerticalTilde => VerticalTilde(args),
        StaticToken::Wedge => Wedge(args),
        StaticToken::Xnor => Xnor(args),
        StaticToken::Xor => Xor(args),
        _ => unreachable!("Internal error: binary operator {:?} to expression", op)
    }
}
