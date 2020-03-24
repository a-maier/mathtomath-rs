#[macro_use]
extern crate log;
#[macro_use]
extern crate assert_matches;
#[macro_use]
extern crate phf;

mod expression;
mod parsers;

fn main() {
    env_logger::init();
}
