use frontend::{
    input::StrInput,
    lexer::{Lexer, LexerInput},
    parser::program::parse_program,
};

fn main() {
    let text = std::fs::read_to_string("test.rl").unwrap();
    let input = StrInput::new(&text);
    let mut lexer_input = LexerInput::new(Lexer::new(input));
    let ast = parse_program(&mut lexer_input).unwrap();

    // Setup context
    let pass0 = backend::pass0_program(&ast).unwrap();
    let pass1 = backend::pass1_program(pass0, &ast).unwrap();
    backend::compile_module(pass1, "main", "out/main.bc1").unwrap();
}
