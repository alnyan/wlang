#![feature(let_chains, trace_macros)]

#[macro_use]
extern crate derivative;

use input::StrInput;
use lexer::{Lexer, LexerInput};

use crate::parser::program::parse_program;

pub mod compiler;
pub mod input;
pub mod lexer;
pub mod parser;

fn main() {
    let text = std::fs::read_to_string("test.rl").unwrap();
    let input = StrInput::new(&text);
    let mut lexer_input = LexerInput::new(Lexer::new(input));
    let ast = parse_program(&mut lexer_input).unwrap();

    // Setup context
    let pass0 = compiler::pass0_program(&ast).unwrap();
    let pass1 = compiler::pass1_program(pass0, &ast).unwrap();
    compiler::compile_module(pass1, "main").unwrap();
}
