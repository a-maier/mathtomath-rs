#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate phf;

#[cfg(test)]
#[macro_use]
extern crate assert_matches;

mod error;
mod expression;
mod arity;
mod formats;

use expression::Expression;
use error::{FormatError, SyntaxError};
use subslice::SubsliceExt;

use std::{
    io::{self, Read},
    fs::File,
    path::PathBuf
};
use clap::arg_enum;
use structopt::StructOpt;

arg_enum! {
    #[derive(Copy,Clone,Eq,PartialEq,Ord,PartialOrd,Hash,Debug)]
    enum Format {
        Form,
        Mathematica,
    }
}

#[derive(Debug, StructOpt)]
#[structopt(name = "mathtomath", about = "Convert mathematical expressions between formats.")]
struct Opt {
    /// Suppress output messages.
    #[structopt(short, long)]
    quiet: bool,

    /// Specify the format of the input expressions.
    #[structopt(short, long, aliases = &["if", "in", "from"], possible_values = &Format::variants(), case_insensitive = true)]
    informat: Format,

    /// Specify the format to which the input expressions will be converted.
    #[structopt(short, long, aliases = &["of", "out", "to"], possible_values = &Format::variants(), case_insensitive = true)]
    outformat: Format,

    /// File from which to read expressions. If omitted, expressions will be read from the command line.
    #[structopt(parse(from_os_str))]
    file: Option<PathBuf>,
}

fn read_expression(filepath: &Option<PathBuf>) -> io::Result<Vec<u8>> {
    info!("reading expression");
    let mut input = Vec::new();
    if let Some(filepath) = filepath {
        let mut file = File::open(filepath)?;
        file.read_to_end(&mut input)?;
    } else {
        io::stdin().read_to_end(&mut input)?;
    }
    Ok(input)
}

fn parse<'a>(
    input: &'a [u8],
    format: Format
) -> Result<Expression<'a>, SyntaxError> {
    use Format::*;
    info!("parsing in format {:?}", format);
    match format {
        Form => formats::form::parser::parse(input),
        Mathematica => {
            let input = std::str::from_utf8(input)?;
            formats::mathematica::parser::parse(input)
        },
    }
}

fn write_expression(
    expr: Expression<'_>,
    format: Format
) -> Result<(), FormatError> {
    use Format::*;
    info!("writing expression");
    let expr = match format {
        Form => formats::form::formatter::Formatter::new(expr),
        _ => unimplemented!()
    };
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    expr.format(&mut handle)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    env_logger::init();

    let opt = Opt::from_args();

    if opt.file == None && !opt.quiet {
        println!("Please enter an expression (finish with <Enter><Ctrl-d>):");
    }
    let input = read_expression(&opt.file)?;
    let transformed;
    let expression = if opt.informat == Format::Mathematica
        && input.find(b"\\[") != None
    {
        transformed = formats::mathematica::unicode::mathematica_to_utf8(&input)?;
        parse(&transformed, opt.informat)?
    } else {
        parse(&input, opt.informat)?
    };
    write_expression(expression, opt.outformat)?;
    println!("");
    Ok(())
}
