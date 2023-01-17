use backend::CompilerOperation;
use frontend::{
    input::StrInput,
    lexer::{Lexer, LexerInput},
    parser::program::parse_program,
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

fn main() {
    let args = Args::parse();

    let text = std::fs::read_to_string(&args.src_file).unwrap();
    let input = StrInput::new(&text);
    let mut lexer_input = LexerInput::new(Lexer::new(input));
    let ast = parse_program(&mut lexer_input).unwrap();

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
        &out_file,
    )
    .unwrap();
}
