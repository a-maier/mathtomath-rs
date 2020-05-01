//use super::lexer;

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

pub const PREC_ADD_TO: u32 = 71;
pub const PREC_ALTERNATIVES: u32 = 64;
pub const PREC_AND: u32 = 56;
pub const PREC_APPLY: u32 = 16;
pub const PREC_BACKSLASH: u32 = 32;
pub const PREC_BECAUSE: u32 = 76;
pub const PREC_BLANK: u32 = 5;
pub const PREC_BLANK_NULL_SEQUENCE: u32 = 5;
pub const PREC_BLANK_SEQUENCE: u32 = 5;
pub const PREC_CAP: u32 = 43;
pub const PREC_CIRCLE_MINUS: u32 = 44;
pub const PREC_CIRCLE_PLUS: u32 = 44;
pub const PREC_CIRCLE_TIMES: u32 = 36;
pub const PREC_COLON: u32 = 73;
pub const PREC_COMPOSITION: u32 = 13;
pub const PREC_COMPOUND_EXPRESSION: u32 = 79;
pub const PREC_CONDITION: u32 = 67;
pub const PREC_CONJUGATE: u32 = 18;
pub const PREC_CONJUGATE_TRANSPOSE: u32 = 18;
pub const PREC_COPRODUCT: u32 = 42;
pub const PREC_CROSS: u32 = 28;
pub const PREC_CUP: u32 = 43;
pub const PREC_D: u32 = 24;
pub const PREC_DECREMENT: u32 = 11;
pub const PREC_DEL: u32 = 24;
pub const PREC_DERIVATIVE: u32 = 19;
pub const PREC_DIAMOND: u32 = 33;
pub const PREC_DIFFERENCE_DELTA: u32 = 24;
pub const PREC_DIFFERENTIAL_D: u32 = 23;
pub const PREC_DISCRETE_RATIO: u32 = 24;
pub const PREC_DISCRETE_SHIFT: u32 = 24;
pub const PREC_DIVIDE: u32 = 31;
pub const PREC_DIVIDE_BY: u32 = 71;
pub const PREC_DOT: u32 = 29;
pub const PREC_DOUBLE_LEFT_TEE: u32 = 61;
pub const PREC_DOUBLE_RIGHT_TEE: u32 = 61;
pub const PREC_DOUBLE_VERTICAL_BAR: u32 = 51;
pub const PREC_DOWN_TEE: u32 = 61;
pub const PREC_ELEMENT: u32 = 53;
pub const PREC_EQUAL: u32 = 51;
pub const PREC_EQUIVALENT: u32 = 59;
pub const PREC_EXISTS: u32 = 54;
pub const PREC_FACTORIAL: u32 = 17;
pub const PREC_FACTORIAL2: u32 = 17;
pub const PREC_FOR_ALL: u32 = 54;
pub const PREC_FORM_BOX: u32 = 80;
pub const PREC_GET: u32 = 6;
pub const PREC_GREATER: u32 = 51;
pub const PREC_GREATER_EQUAL: u32 = 51;
pub const PREC_IMPLIES: u32 = 60;
pub const PREC_INCREMENT: u32 = 11;
pub const PREC_INTEGRATE: u32 = 45;
pub const PREC_INTERSECTION: u32 = 48;
pub const PREC_LEFT_TEE: u32 = 61;
pub const PREC_LESS: u32 = 51;
pub const PREC_LESS_EQUAL: u32 = 51;
pub const PREC_LIMIT: u32 = 46;
pub const PREC_MAP: u32 = 16;
pub const PREC_MAP_ALL: u32 = 16;
pub const PREC_MAX_LIMIT: u32 = 46;
pub const PREC_MESSAGE_NAME: u32 = 2;
pub const PREC_MIN_LIMIT: u32 = 46;
pub const PREC_NAND: u32 = 56;
pub const PREC_NON_COMMUTATIVE_MULTIPLY: u32 = 27;
pub const PREC_NOR: u32 = 58;
pub const PREC_NOT: u32 = 55;
pub const PREC_NOT_DOUBLE_VERTICAL_BAR: u32 = 51;
pub const PREC_NOT_ELEMENT: u32 = 53;
pub const PREC_NOT_EXISTS: u32 = 54;
pub const PREC_NOT_VERTICAL_BAR: u32 = 51;
pub const PREC_NULL: u32 = 79;
pub const PREC_OR: u32 = 58;
pub const PREC_OUT: u32 = 4;
pub const PREC_OVERSCRIPT: u32 = 7;
pub const PREC_PART: u32 = 10;
pub const PREC_PATTERN_TEST: u32 = 9;
pub const PREC_PIECEWISE: u32 = 1;
pub const PREC_PLUS: u32 = 47;
pub const PREC_PRE_DECREMENT: u32 = 12;
pub const PREC_PRE_INCREMENT: u32 = 12;
pub const PREC_PRODUCT: u32 = 40;
pub const PREC_PUT: u32 = 78;
pub const PREC_PUT_APPEND: u32 = 78;
pub const PREC_REPEATED: u32 = 63;
pub const PREC_REPEATED_NULL: u32 = 63;
pub const PREC_REPLACE_ALL: u32 = 70;
pub const PREC_REPLACE_REPEATED: u32 = 70;
pub const PREC_RIGHT_COMPOSITION: u32 = 13;
pub const PREC_RIGHT_TEE: u32 = 61;
pub const PREC_RULE: u32 = 69;
pub const PREC_RULE_DELAYED: u32 = 69;
pub const PREC_SAME_Q: u32 = 52;
pub const PREC_SET: u32 = 77;
pub const PREC_SET_DELAYED: u32 = 77;
pub const PREC_SLOT: u32 = 3;
pub const PREC_SLOT_SEQUENCE: u32 = 3;
pub const PREC_SMALL_CIRCLE: u32 = 25;
pub const PREC_SPAN: u32 = 50;
pub const PREC_SQRT: u32 = 22;
pub const PREC_SQUARE: u32 = 25;
pub const PREC_STAR: u32 = 39;
pub const PREC_STRING_EXPRESSION: u32 = 66;
pub const PREC_STRING_JOIN: u32 = 20;
pub const PREC_SUBSET: u32 = 53;
pub const PREC_SUBTRACT_FROM: u32 = 71;
pub const PREC_SUCH_THAT: u32 = 62;
pub const PREC_SUM: u32 = 46;
pub const PREC_SUPERSET: u32 = 53;
pub const PREC_TAG_SET: u32 = 77;
pub const PREC_TAG_SET_DELAYED: u32 = 77;
pub const PREC_TAG_UNSET: u32 = 77;
pub const PREC_THEREFORE: u32 = 76;
pub const PREC_TIMES_BY: u32 = 71;
pub const PREC_TRANSPOSE: u32 = 18;
pub const PREC_TWO_WAY_RULE: u32 = 68;
pub const PREC_UNDEROVERSCRIPT: u32 = 7;
pub const PREC_UNDERSCRIPT: u32 = 7;
pub const PREC_UNEQUAL: u32 = 51;
pub const PREC_UNION: u32 = 49;
pub const PREC_UNSAME_Q: u32 = 52;
pub const PREC_UNSET: u32 = 77;
pub const PREC_UP_SET: u32 = 77;
pub const PREC_UP_SET_DELAYED: u32 = 77;
pub const PREC_UP_TEE: u32 = 61;
pub const PREC_VERTICAL_BAR: u32 = 51;
pub const PREC_VERTICAL_SEPARATOR: u32 = 75;
pub const PREC_VERTICAL_TILDE: u32 = 41;
pub const PREC_WEDGE: u32 = 34;
pub const PREC_XNOR: u32 = 57;
pub const PREC_XOR: u32 = 57;

// operators with more than one precedence level
pub const PREC_FUNCTION_AMP: u32 = 72;
pub const PREC_FUNCTION_ARR: u32 = 77;
pub const PREC_UMINUS_PLUS: u32 = 30;
pub const PREC_MINUS_PLUS: u32 = 47;
pub const PREC_OPTIONAL: u32 = 5;
pub const PREC_OPTIONAL_COL: u32 = 65;
pub const PREC_PATTERN: u32 = 5;
pub const PREC_PATTERN_COL: u32 = 65;
pub const PREC_UPLUS_MINUS: u32 = 30;
pub const PREC_PLUS_MINUS: u32 = 47;
pub const PREC_POWER: u32 = 21;
pub const PREC_SUBSCRIPT: u32 = 8;
pub const PREC_UMINUS: u32 = 30; // listed under Times
pub const PREC_TIMES: u32 = 38;

// no idea how to enter these anyway in input form
// pub const PREC_POWER_@ = 22;
// pub const PREC_POWER_SUBSCRIPT = 8;

// pub fn is_symbol(i: &[u8]) -> bool {
//     match lexer::symbol(i) {
//         Ok((rest, _)) => rest.is_empty(),
//         Err(_) => false,
//     }
// }
