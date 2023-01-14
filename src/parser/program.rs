use std::rc::Rc;

use crate::lexer::token::{BasicOperator, Keyword, Punctuation, Token};

use super::{
    combinator::{parse_delimited, parse_many0},
    expr::{parse_expr, parse_statement_expr},
    Function, Node, ParserError,
};

def_parser!(pub maybe_call<S>(input: &mut S, left: Rc<Node>) -> Rc<Node> {
    if let Some(token) = input.peek()? && token == Token::Punctuation(Punctuation::LParen) {
        input.next()?.unwrap();
        let args = parse_delimited(
            input,
            parse_expr,
            Token::Punctuation(Punctuation::RParen),
            Token::Punctuation(Punctuation::Comma))?;
        Ok(Rc::new(Node::Call(left, args)))
    } else {
        Ok(left)
    }
});

def_parser!(pub parse_type<S>(input: &mut S) -> Rc<Node> {
    expect!(input, Token::Ident(name));

    if let Some(token) = input.peek()? && token == Token::BasicOperator(BasicOperator::Lt) {
        todo!()
    } else {
        Ok(Rc::new(Node::Type(name)))
    }
});

def_parser!(pub parse_typed<S>(input: &mut S) -> (Rc<Node>, Rc<Node>) {
    expect!(input, Token::Ident(name));
    expect!(input, Token::BasicOperator(BasicOperator::Colon));
    let ty = parse_type(input)?;

    Ok((Rc::new(Node::Ident(name)), ty))
});

def_parser!(pub parse_block<S>(input: &mut S) -> Rc<Node> {
    expect!(input, Token::Punctuation(Punctuation::LBrace));

    let items = parse_many0(
        input,
        parse_statement_expr,
        Token::Punctuation(Punctuation::RBrace))?;

    Ok(Rc::new(Node::Block(items)))
});

def_parser!(pub parse_fn<S>(input: &mut S) -> Rc<Node> {
    expect!(input, Token::Ident(name));
    expect!(input, Token::Punctuation(Punctuation::LParen));
    let args = parse_delimited(
        input,
        parse_typed,
        Token::Punctuation(Punctuation::RParen),
        Token::Punctuation(Punctuation::Comma))?;

    let ret_type = if let Some(token) = input.peek()? && token == Token::BasicOperator(BasicOperator::Arrow) {
        input.next()?.unwrap();
        Some(parse_type(input)?)
    } else {
        None
    };

    let body = parse_block(input)?;

    Ok(Rc::new(Node::Function(Function {
        args,
        name,
        ret_type,
        body
    })))
});

def_parser!(pub parse_item<S>(input: &mut S) -> Rc<Node> {
    let Some(token) = input.next()? else {
        return Err(ParserError::UnexpectedEof);
    };

    match token {
        Token::Keyword(Keyword::Fn) => parse_fn(input),
        _ => Err(ParserError::UnexpectedToken(token))
    }
});

def_parser!(pub parse_program<S>(input: &mut S) -> Vec<Rc<Node>> {
    let mut res = vec![];
    loop {
        if input.peek()?.is_none() {
            break;
        }

        res.push(parse_item(input)?);
    }
    Ok(res)
});
