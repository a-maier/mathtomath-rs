use super::lexer;

use crate::assoc::Assoc;
use crate::expression::BinaryOp;
use std::str;

// obtained from Mathematica 12.0:
//
// ```Mathematica
// Select[
//     WolframLanguageData[
//         WolframLanguageData["Entities"],
//         {"Name", "PrecedenceRanks"}
//     ],
//     #[[2]] =!= Missing["NotApplicable"] &
// ]
// ```
// see https://www.robertjacobson.dev/defining-the-wolfram-language-part-1-finding-operators
//
// we want higher precedence to bind _stronger_, so we use
// 100 - (precedence from mathematica)
pub const PREC_ADD_TO: u32 = 29;
pub const PREC_ALTERNATIVES: u32 = 36;
pub const PREC_AND: u32 = 44;
pub const PREC_APPLY: u32 = 84;
pub const PREC_BACKSLASH: u32 = 68;
pub const PREC_BECAUSE: u32 = 24;
pub const PREC_BLANK: u32 = 95;
pub const PREC_BLANK_NULL_SEQUENCE: u32 = 95;
pub const PREC_BLANK_SEQUENCE: u32 = 95;
pub const PREC_CAP: u32 = 57;
pub const PREC_CIRCLE_MINUS: u32 = 56;
pub const PREC_CIRCLE_PLUS: u32 = 56;
pub const PREC_CIRCLE_TIMES: u32 = 64;
pub const PREC_COLON: u32 = 27;
pub const PREC_COMPOSITION: u32 = 87;
pub const PREC_COMPOUND_EXPRESSION: u32 = 21;
pub const PREC_CONDITION: u32 = 33;
pub const PREC_CONJUGATE: u32 = 82;
pub const PREC_CONJUGATE_TRANSPOSE: u32 = 82;
pub const PREC_COPRODUCT: u32 = 58;
pub const PREC_CROSS: u32 = 72;
pub const PREC_CUP: u32 = 57;
pub const PREC_D: u32 = 76;
pub const PREC_DECREMENT: u32 = 89;
pub const PREC_DEL: u32 = 76;
pub const _PREC_DERIVATIVE: u32 = 81;
pub const PREC_DIAMOND: u32 = 67;
pub const PREC_DIFFERENCE_DELTA: u32 = 76;
pub const _PREC_DIFFERENTIAL_D: u32 = 77;
pub const PREC_DISCRETE_RATIO: u32 = 76;
pub const PREC_DISCRETE_SHIFT: u32 = 76;
pub const PREC_DIVIDE: u32 = 69;
pub const PREC_DIVIDE_BY: u32 = 29;
pub const PREC_DOT: u32 = 71;
pub const _PREC_DOUBLE_LEFT_TEE: u32 = 39;
pub const _PREC_DOUBLE_RIGHT_TEE: u32 = 39;
pub const PREC_DOUBLE_VERTICAL_BAR: u32 = 49;
pub const PREC_DOWN_TEE: u32 = 39;
pub const PREC_ELEMENT: u32 = 47;
pub const PREC_EQUAL: u32 = 49;
pub const PREC_EQUIVALENT: u32 = 41;
pub const PREC_EXISTS: u32 = 46;
pub const PREC_FACTORIAL: u32 = 83;
pub const PREC_FACTORIAL2: u32 = 83;
pub const PREC_FOR_ALL: u32 = 46;
pub const _PREC_FORM_BOX: u32 = 20;
pub const PREC_GET: u32 = 94;
pub const PREC_GREATER: u32 = 49;
pub const PREC_GREATER_EQUAL: u32 = 49;
pub const PREC_IMPLIES: u32 = 40;
pub const PREC_INCREMENT: u32 = 89;
pub const PREC_INTEGRATE: u32 = 55;
pub const PREC_INTERSECTION: u32 = 52;
pub const PREC_LEFT_TEE: u32 = 39;
pub const PREC_LESS: u32 = 49;
pub const PREC_LESS_EQUAL: u32 = 49;
pub const PREC_LIMIT: u32 = 54;
pub const PREC_MAP: u32 = 84;
pub const PREC_MAP_ALL: u32 = 84;
pub const PREC_MAX_LIMIT: u32 = 54;
pub const PREC_MESSAGE_NAME: u32 = 98;
pub const PREC_MIN_LIMIT: u32 = 54;
pub const PREC_NAND: u32 = 44;
pub const PREC_NON_COMMUTATIVE_MULTIPLY: u32 = 73;
pub const PREC_NOR: u32 = 42;
pub const PREC_NOT: u32 = 45;
pub const _PREC_NOT_DOUBLE_VERTICAL_BAR: u32 = 49;
pub const _PREC_NOT_ELEMENT: u32 = 47;
pub const PREC_NOT_EXISTS: u32 = 46;
pub const _PREC_NOT_VERTICAL_BAR: u32 = 49;
pub const _PREC_NULL: u32 = 21;
pub const PREC_OR: u32 = 42;
pub const PREC_OUT: u32 = 96;
pub const _PREC_OVERSCRIPT: u32 = 93;
pub const _PREC_PART: u32 = 90;
pub const PREC_PATTERN_TEST: u32 = 91;
pub const PREC_PIECEWISE: u32 = 99;
pub const PREC_PLUS: u32 = 53;
pub const _PREC_PRE_INCREMENT: u32 = 88;
pub const _PREC_PRE_DECREMENT: u32 = 88;
pub const PREC_PRODUCT: u32 = 60;
pub const _PREC_PUT: u32 = 22;
pub const _PREC_PUT_APPEND: u32 = 22;
pub const PREC_REPEATED: u32 = 37;
pub const PREC_REPEATED_NULL: u32 = 37;
pub const PREC_REPLACE_ALL: u32 = 30;
pub const PREC_REPLACE_REPEATED: u32 = 30;
pub const _PREC_RIGHT_COMPOSITION: u32 = 87;
pub const PREC_RIGHT_TEE: u32 = 39;
pub const PREC_RULE: u32 = 31;
pub const PREC_RULE_DELAYED: u32 = 31;
pub const PREC_SAME_Q: u32 = 48;
pub const PREC_SET: u32 = 23;
pub const PREC_SET_DELAYED: u32 = 23;
pub const PREC_SLOT: u32 = 97;
pub const PREC_SLOT_SEQUENCE: u32 = 97;
pub const PREC_SMALL_CIRCLE: u32 = 75;
pub const PREC_SPAN: u32 = 50;
pub const PREC_SQRT: u32 = 78;
pub const PREC_SQUARE: u32 = 75;
pub const PREC_STAR: u32 = 61;
pub const PREC_STRING_EXPRESSION: u32 = 34;
pub const PREC_STRING_JOIN: u32 = 80;
pub const PREC_SUBSET: u32 = 47;
pub const PREC_SUBTRACT_FROM: u32 = 29;
pub const PREC_SUCH_THAT: u32 = 38;
pub const _PREC_SUM: u32 = 54;
pub const PREC_SUPERSET: u32 = 47;
pub const PREC_TAG_SET: u32 = 23;
pub const PREC_TAG_SET_DELAYED: u32 = 23;
pub const PREC_TAG_UNSET: u32 = 23;
pub const PREC_THEREFORE: u32 = 24;
pub const PREC_TIMES_BY: u32 = 29;
pub const PREC_TRANSPOSE: u32 = 82;
pub const PREC_TWO_WAY_RULE: u32 = 32;
pub const _PREC_UNDEROVERSCRIPT: u32 = 93;
pub const _PREC_UNDERSCRIPT: u32 = 93;
pub const PREC_UNEQUAL: u32 = 49;
pub const PREC_UNION: u32 = 51;
pub const PREC_UNSAME_Q: u32 = 48;
pub const PREC_UNSET: u32 = 23;
pub const _PREC_UP_SET: u32 = 23;
pub const _PREC_UP_SET_DELAYED: u32 = 23;
pub const PREC_UP_TEE: u32 = 39;
pub const PREC_VERTICAL_BAR: u32 = 49;
pub const PREC_VERTICAL_SEPARATOR: u32 = 25;
pub const PREC_VERTICAL_TILDE: u32 = 59;
pub const PREC_WEDGE: u32 = 66;
pub const PREC_XNOR: u32 = 43;
pub const PREC_XOR: u32 = 43;

// operators with more than one precedence level
pub const PREC_FUNCTION_AMP: u32 = 28;
pub const _PREC_FUNCTION_ARR: u32 = 23;
pub const PREC_UMINUS_PLUS: u32 = 70;
pub const PREC_MINUS_PLUS: u32 = 53;
pub const PREC_OPTIONAL: u32 = 95;
pub const _PREC_OPTIONAL_COL: u32 = 35;
pub const PREC_PATTERN: u32 = 95;
pub const _PREC_PATTERN_COL: u32 = 35;
pub const PREC_UPLUS_MINUS: u32 = 70;
pub const PREC_PLUS_MINUS: u32 = 53;
pub const PREC_POWER: u32 = 79;
pub const _PREC_SUBSCRIPT: u32 = 92;
pub const PREC_UMINUS: u32 = 70;
pub const PREC_UPLUS: u32 = PREC_UMINUS;
pub const PREC_TIMES: u32 = 62;

// no idea how to enter these anyway in input form
// pub const PREC_POWER_@ = 78;
// pub const PREC_POWER_SUBSCRIPT = 92;

// standard entries
pub const PREC_SYMBOL: u32 = 0;
pub const PREC_INTEGER: u32 = 0;
pub const PREC_RIGHT_BRACKET: u32 = 0;
pub const PREC_RIGHT_SQUARE_BRACKET: u32 = 0;
pub const PREC_RIGHT_PART: u32 = 0;
pub const PREC_RIGHT_CURLY_BRACKET: u32 = 0;
pub const PREC_RIGHT_ANGLE_BRACKET: u32 = 0;
pub const PREC_RIGHT_ASSOCIATION: u32 = 0;
pub const PREC_RIGHT_CEILING: u32 = 0;
pub const PREC_RIGHT_FLOOR: u32 = 0;
pub const PREC_FUNCTION: u32 = 100;
pub const PREC_LEFT_BRACKET: u32 = 100;
pub const PREC_LEFT_SQUARE_BRACKET: u32 = 100;
pub const PREC_LEFT_PART: u32 = 100;
pub const PREC_LEFT_CURLY_BRACKET: u32 = 100;
pub const PREC_LEFT_ANGLE_BRACKET: u32 = 100;
pub const PREC_LEFT_ASSOCIATION: u32 = 100;
pub const PREC_LEFT_CEILING: u32 = 100;
pub const PREC_LEFT_FLOOR: u32 = 100;
pub const PREC_ATOM: u32 = u32::MAX;

// guesses
pub const PREC_SEQUENCE: u32 = 10; // lower than COMPOUND_EXPRESSION (';')
pub const PREC_POSTFIX: u32 = 25; // higher than Therefore, lower than Colon
pub const PREC_MINUS: u32 = PREC_PLUS;

pub fn is_symbol(i: &[u8]) -> bool {
    if let Ok(i) = str::from_utf8(i) {
        match lexer::symbol(i) {
            Ok((rest, _)) => rest.is_empty(),
            Err(_) => false,
        }
    } else {
        false
    }
}

pub fn assoc(op: BinaryOp) -> Assoc {
    use Assoc::*;
    match op {
        BinaryOp::Power => Right,
        _ => Left,
    }
}
