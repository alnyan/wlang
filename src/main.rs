#![feature(let_chains, trace_macros)]

use std::{iter::Peekable, fs::File};

use compiler::{pass1::{pass1_expr, pass1_module}, Context};
use input::{Input, StrInput};
use lexer::{token::Token, LexerInput, Lexer};

use crate::parser::program::parse_program;

pub mod parser;
pub mod compiler;
pub mod lexer;
pub mod input;

fn main() {
    let text = std::fs::read_to_string("test.rl").unwrap();
    let input = StrInput::new(&text);
    let mut lexer_input = LexerInput::new(Lexer::new(input));
    let ast = parse_program(&mut lexer_input).unwrap();

    dbg!(&ast);

    let mut ctx = Context::new();
    // Setup context
    pass1_module(&mut ctx, &ast).unwrap();
}
