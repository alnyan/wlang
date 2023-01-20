use std::process;

use backend::CompilerOperation;
use frontend::{
    input::StrInput,
    lexer::{Lexer, LexerInput},
    parser::{program::parse_program, ParserError},
};

use clap::Parser;

#[derive(Parser, Debug)]
pub struct Args {
    #[arg(short)]
    bitcode: bool,

    #[arg(short)]
    out_file: Option<String>,

    src_file: String,
}

fn print_parser_error(err: ParserError) {
    match err {
        ParserError::LexerError(_e) => todo!(),
        ParserError::UnexpectedToken(t, e) => {
            eprintln!(
                "{}:{}: unexpected token: {}",
                t.position.line + 1,
                t.position.column + 1,
                t.value
            );
            if e.len() > 1 {
                eprintln!("  Expected one of the following:");
                for i in e {
                    eprintln!("* {i}");
                }
            } else {
                eprintln!("  Expected {}", e[0]);
            }
        }
        ParserError::UnexpectedEof => eprintln!("Unexpected EOF"),
    }
}

fn main() {
    let args = Args::parse();

    let text = std::fs::read_to_string(&args.src_file).unwrap();
    let input = StrInput::new(&text);
    let mut lexer_input = LexerInput::new(Lexer::new(input));
    let ast = match parse_program(&mut lexer_input) {
        Ok(v) => v,
        Err(e) => {
            print_parser_error(e);
            process::exit(1);
        }
    };

    // Setup context
    let pass0 = backend::pass0_program(&ast).unwrap();
    let pass1 = backend::pass1_program(pass0, &ast).unwrap();

    let out_file = args.out_file.clone().unwrap_or("out.bc".to_owned());

    backend::compile_module(
        pass1,
        if args.bitcode {
            CompilerOperation::EmitIntermediateBitcode
        } else {
            CompilerOperation::EmitIntermediateSourceCode
        },
        "main",
        out_file,
    )
    .unwrap();
}
