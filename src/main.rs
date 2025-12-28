#![feature(formatting_options)]
#![allow(incomplete_features)]
// needed for `Kw`
// tbh that could easily be replaced by a kw module
#![feature(unsized_const_params)]
#![feature(adt_const_params)]

// INCREDIBLY weird fix but this does fix macro resolution errors
include!("macros.rs");

pub mod combo;
pub mod error;
// pub mod exec;
pub mod lang;
pub mod tok;

use std::fs;

use tactical::{ParseContext, Span, Syntax, cursor};

use crate::{
    error::{CompilerError, ParseError}, lang::Items, tok::tokenise
};

fn compiler_main() -> std::result::Result<(), CompilerError> {
    let source = fs::read_to_string("source.taf")?;
    let tokens = tokenise(&source).map_err(|err| (source.clone(), err))?;
    let eof_span = tokens
        .last()
        .map(|(span, _)| span.next_column())
        .unwrap_or(Span::start());
    let mut iter = tokens.iter().cloned();
    let items = Items::from_tokens(
        &mut iter,
        ParseContext {
            eof_row: eof_span.end_row(),
            eof_col: eof_span.end_col(),
            data: (),
        },
    )
    .map_err(|err| (source, err))?;

    if let Some(token) = iter.next() {
        panic!("leftover token: {token:?}"); // TODO better erroring on this?
    }

    println!("Resultant syntax tree: {:?}", items);
    Ok(())
}

fn main() {
    match compiler_main() {
        Err(err) => eprintln!("{err}"),
        _ => {}
    }
}
