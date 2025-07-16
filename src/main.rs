#[macro_use]
extern crate log;
#[macro_use]
extern crate lazy_static;
#[macro_use]
extern crate maplit;
#[macro_use]
extern crate phf;

mod arity;
mod assoc;
mod cfg;
mod error;
mod expression;
mod formats;
mod range;

use ansi_term::Colour::Red;
use clap::{Parser, ValueEnum};
use error::{Context, Error, ParseError, SyntaxError};
use expression::Expression;
use std::{
    fs::File,
    io::{self, Read},
    path::PathBuf,
    str::from_utf8,
};
use subslice::SubsliceExt;

#[derive(
    ValueEnum, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug,
)]
enum Format {
    Form,
    Latex,
    Mathematica,
}

#[derive(Debug, Parser)]
#[clap(about = "Convert mathematical expressions between formats.")]
struct Opt {
    /// Suppress output messages.
    #[clap(short, long)]
    quiet: bool,

    /// Specify the format of the input expressions.
    #[clap(short, long, aliases = &["if", "in", "from"])]
    informat: Format,

    /// Specify the format to which the input expressions will be converted.
    #[clap(short, long, aliases = &["of", "out", "to"])]
    outformat: Format,

    /// File from which to read expressions. If omitted, expressions will be read from the command line.
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

fn get_context(input: &[u8], pos: usize) -> Option<Context> {
    let (before, after) = input.split_at(pos);
    let line = before.iter().filter(|&&c| c == b'\n').count() + 1;
    let before: Vec<u8> = before
        .iter()
        .rev()
        .take_while(|&&c| c != b'\n')
        .copied()
        .collect();
    let before: Vec<u8> = before.into_iter().rev().collect();
    let before = from_utf8(&before).ok()?.to_owned();
    let after: Vec<_> =
        after.iter().take_while(|&&c| c != b'\n').copied().collect();
    let after = from_utf8(&after).ok()?.to_owned();

    Some(Context {
        line,
        before,
        after,
    })
}

fn parse(input: &[u8], format: Format) -> Result<Expression<'_>, ParseError> {
    use Format::*;
    info!("parsing in format {:?}", format);
    let res = match format {
        Form => formats::form::parser::parse(input),
        Mathematica => match from_utf8(input) {
            Ok(input) => formats::mathematica::parser::parse(input),
            Err(err) => Err(SyntaxError::from(err)),
        },
        Latex => formats::latex::parser::parse(input),
    };
    res.map_err(|err| ParseError::new(get_context(input, err.pos()), err))
}

fn write_expression(
    expr: Expression<'_>,
    format: Format,
) -> Result<(), std::io::Error> {
    use Format::*;
    info!("writing expression");
    let stdout = io::stdout();
    let mut handle = stdout.lock();
    match format {
        Form => {
            formats::form::formatter::Formatter::new(expr).format(&mut handle)
        }
        Mathematica => formats::mathematica::formatter::Formatter::new(expr)
            .format(&mut handle),
        Latex => {
            formats::latex::formatter::Formatter::new(expr).format(&mut handle)
        }
    }
}

fn main() {
    let exit_code = if let Err(err) = run() {
        eprintln!("{err}");
        if let Error::Parse(err) = err {
            if let Some(context) = err.context {
                eprintln!(
                    " In line {} in input expression: {}{}",
                    context.line,
                    context.before,
                    Red.bold().paint(context.after)
                )
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

    let opt = Opt::parse();

    if opt.file.is_none() && !opt.quiet {
        println!("Please enter an expression (finish with <Enter><Ctrl-d>):");
    }
    let input = read_expression(&opt.file)?;
    let transformed;
    let expression = if opt.informat == Format::Mathematica
        && input.find(b"\\[").is_some()
    {
        transformed =
            formats::mathematica::unicode::mathematica_to_utf8(&input)?;
        parse(&transformed, opt.informat)?
    } else {
        parse(&input, opt.informat)?
    };
    write_expression(expression, opt.outformat)?;
    println!();
    Ok(())
}
