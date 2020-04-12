use super::lexer;

pub const PREC_SYMBOL: u32 = 0;
pub const PREC_INTEGER: u32 = 0;
pub const PREC_ELLIPSIS: u32 = 0;
pub const PREC_RIGHT_BRACKET: u32 = 0;
pub const PREC_RIGHT_SQUARE_BRACKET: u32 = 0;
pub const PREC_SEMICOLON: u32 = 10;
pub const PREC_COMMA: u32 = 20;
pub const PREC_EQUALS: u32 = 30;
pub const PREC_PLUS: u32 = 40;
pub const PREC_MINUS: u32 = PREC_PLUS;
pub const PREC_TIMES: u32 = 50;
pub const PREC_DIVIDE: u32 = PREC_TIMES;
pub const PREC_UPLUS: u32 = 60;
pub const PREC_UMINUS: u32 = PREC_UPLUS;
pub const PREC_POWER: u32 = 70;
pub const PREC_DOT: u32 = 80;
pub const PREC_LEFT_BRACKET: u32 = 90;
pub const PREC_LEFT_SQUARE_BRACKET: u32 = 100;
pub const PREC_WILDCARD: u32 = 110;
pub const PREC_ATOM: u32 = std::u32::MAX;

pub fn is_symbol(i: &[u8]) -> bool {
    match lexer::symbol(i) {
        Ok((rest, _)) => rest.is_empty(),
        Err(_) => false,
    }
}
