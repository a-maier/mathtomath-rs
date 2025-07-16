mathtomath
==========

Convert formulas between different formats. This crate is a successor
to the [original mathtomath Perl
program](https://github.com/a-maier/mathtomath).

Installation
------------

The easiest way to get started is to [download the executable from the
latest github
release](https://github.com/a-maier/mathtomath-rs/releases).

Alternatively, if you have [Rust and
Cargo](https://www.rust-lang.org/) installed, run

    cargo install mathtomath

Usage
-----

The currently supported formats are
[FORM](https://github.com/form-dev/form),
[LaTeX](https://en.wikipedia.org/wiki/LaTeX), and [Mathematica, aka
Wolfram Language](https://en.wikipedia.org/wiki/Wolfram_Language).

For interactive use, run e.g.

    mathtomath -i mathematica -o latex

Then enter an expression in Mathematica format and hit `<enter>
<ctrl-d>`. Alternatively, save your expression in a file `expr.m` and
run

    mathtomath -i mathematica -o latex expr.m

Caveats
-------

Expect lots of bugs, especially when parsing LaTeX. Bug reports and
pull requests are welcome.
