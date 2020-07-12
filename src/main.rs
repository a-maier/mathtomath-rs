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
use error::{Error, ParseError, SyntaxError};
use subslice::SubsliceExt;
use std::{
    io::{self, Read, Write},
    fs::File,
    path::PathBuf,
    str::from_utf8,
};
use clap::arg_enum;
use structopt::StructOpt;
use unicode_segmentation::UnicodeSegmentation;

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


static CONTEXT_SIZE: usize = 10;

fn get_context(input: &[u8], pos: usize) -> Option<(String, String)> {
    let (before, after) = input.split_at(pos);
    let before = from_utf8(before).ok()?;
    let after = from_utf8(after).ok()?;

    let before: Vec<&str> = UnicodeSegmentation::graphemes(before, true).collect();
    let before = before.into_iter().rev().take(CONTEXT_SIZE).rev().collect();
    let after = UnicodeSegmentation::graphemes(after, true)
        .take(CONTEXT_SIZE).collect();
    Some((before, after))
}

fn parse<'a>(
    input: &'a [u8],
    format: Format
) -> Result<Expression<'a>, ParseError> {
    use Format::*;
    info!("parsing in format {:?}", format);
    let res = match format {
        Form => formats::form::parser::parse(input),
        Mathematica => match from_utf8(input) {
            Ok(input) => formats::mathematica::parser::parse(input),
            Err(err) => Err(SyntaxError::from(err))
        },
    };
    res.map_err(|err| ParseError::new(get_context(input, err.pos()), err))
}

fn write_expression(
    expr: Expression<'_>,
    format: Format
) -> Result<(), std::io::Error> {
    use Format::*;
    info!("writing expression");
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    match format {
        Form => formats::form::formatter::Formatter::new(expr).format(&mut handle),
        Mathematica => formats::mathematica::formatter::Formatter::new(expr).format(&mut handle)
    }
}

fn main() {
    let exit_code = if let Err(err) = run() {

        eprintln!("{}", err);
        if let Error::Parse(err) = err {
            if let Some((before, after)) = err.context {
                use termcolor::{Color, ColorChoice, ColorSpec, StandardStream, WriteColor};
                let mut stderr = StandardStream::stderr(ColorChoice::Always);
                let _ = write!(&mut stderr, " Here in input expression: {}", before);
                stderr.set_color(ColorSpec::new().set_fg(Some(Color::Red))).unwrap();
                let _ = writeln!(&mut stderr, "{}", after);
            }
        }
        1
    } else {
        0
    };
    std::process::exit(exit_code);
}

fn run() -> Result<(), Error> {
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
