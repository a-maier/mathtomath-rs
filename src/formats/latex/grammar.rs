use crate::assoc::Assoc;
use crate::expression::BinaryOp;

pub const PREC_AND: u32 = 44;
// pub const PREC_BACKSLASH: u32 = 68;
pub const PREC_BECAUSE: u32 = 24;
pub const PREC_WILDCARD: u32 = 95;
pub const PREC_CIRCLE_MINUS: u32 = 56;
pub const PREC_CIRCLE_PLUS: u32 = 56;
pub const PREC_CIRCLE_TIMES: u32 = 64;
pub const PREC_COLON: u32 = 27;
pub const PREC_COMPOUND_EXPRESSION: u32 = 21;
pub const PREC_CONJUGATE_TRANSPOSE: u32 = 82;
pub const PREC_D: u32 = 76;
pub const PREC_DEL: u32 = 76;
pub const PREC_DIVIDE: u32 = 69;
pub const PREC_DOT: u32 = 71;
pub const PREC_ELEMENT: u32 = 47;
pub const PREC_EQUAL: u32 = 49;
pub const PREC_EQUIVALENT: u32 = 41;
pub const PREC_EXISTS: u32 = 46;
pub const PREC_FACTORIAL: u32 = 83;
pub const PREC_FACTORIAL2: u32 = 83;
pub const PREC_FOR_ALL: u32 = 46;
pub const PREC_FRAC: u32 = 100;
pub const PREC_GREATER: u32 = 49;
pub const PREC_GREATER_EQUAL: u32 = 49;
pub const PREC_IMPLIES: u32 = 40;
pub const PREC_INTEGRATE: u32 = 55;
pub const PREC_INTERSECTION: u32 = 52;
pub const PREC_LESS: u32 = 49;
pub const PREC_LESS_EQUAL: u32 = 49;
pub const PREC_LIMIT: u32 = 54;
pub const _PREC_NAND: u32 = 44;
pub const _PREC_NOR: u32 = 42;
pub const PREC_NOT: u32 = 45;
pub const PREC_NOT_EXISTS: u32 = 46;
pub const PREC_OR: u32 = 42;
pub const PREC_PLUS: u32 = 53;
pub const PREC_PRODUCT: u32 = 60;
pub const PREC_SQRT: u32 = 78;
pub const PREC_SUBSET: u32 = 47;
pub const _PREC_SUCH_THAT: u32 = 38;
pub const PREC_SUPERSET: u32 = 47;
pub const PREC_THEREFORE: u32 = 24;
pub const PREC_UNEQUAL: u32 = 49;
pub const PREC_UNION: u32 = 51;
pub const PREC_WEDGE: u32 = 66;
pub const _PREC_XNOR: u32 = 43;
pub const _PREC_XOR: u32 = 43;
pub const PREC_SYMBOL: u32 = 0;
pub const PREC_RIGHT_BRACKET: u32 = 0;
pub const PREC_RIGHT_CURLY_BRACKET: u32 = 0;
pub const PREC_RIGHT_ANGLE_BRACKET: u32 = 0;
pub const PREC_RIGHT_CEILING: u32 = 0;
pub const PREC_RIGHT_FLOOR: u32 = 0;
pub const PREC_LEFT_BRACKET: u32 = 100;
pub const PREC_LEFT_CURLY_BRACKET: u32 = 100;
pub const PREC_LEFT_ANGLE_BRACKET: u32 = 100;
pub const PREC_LEFT_CEILING: u32 = 100;
pub const PREC_LEFT_FLOOR: u32 = 100;
pub const PREC_ATOM: u32 = std::u32::MAX;
pub const PREC_SEQUENCE: u32 = 10;
pub const PREC_MINUS: u32 = PREC_PLUS;
pub const PREC_UMINUS_PLUS: u32 = 70;
pub const PREC_MINUS_PLUS: u32 = 53;
pub const PREC_UPLUS_MINUS: u32 = 70;
pub const PREC_PLUS_MINUS: u32 = 53;
pub const PREC_POWER: u32 = 79;
pub const PREC_UMINUS: u32 = 70;
pub const PREC_UPLUS: u32 = PREC_UMINUS;
pub const PREC_TIMES: u32 = 62;
pub const PREC_DEGREE: u32 = 90;
pub const PREC_SUBSCRIPT: u32 = 100;

pub fn assoc(op: BinaryOp) -> Assoc {
    use Assoc::*;
    match op {
        BinaryOp::Power => None,
        _ => Left
    }
}

pub fn is_left_bracket(expr: &[u8]) -> bool {
    matches!(
        expr,
        b"(" | b"[" | b"\\{" | b"\\langle" | b"\\lceil" | b"\\lfloor"
            | b"|" | b"\\|"
    )
}

pub fn is_right_bracket(expr: &[u8]) -> bool {
    matches!(
        expr,
        b"|" | b"\\|" | b")" | b"]" | b"\\}"
            | b"\\rangle" | b"\\rceil" | b"\\rfloor"
    )
}
