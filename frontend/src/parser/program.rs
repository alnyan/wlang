use std::rc::Rc;

use ast::{
    node::TypeNode,
    token::{BasicOperator, Keyword, Punctuation},
    Node, Token,
};

use super::{
    combinator::{parse_delimited, parse_many0},
    expr::{parse_expr, parse_statement_expr},
    ParserError,
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
    if let Some(token) = input.peek()? {
        if token == Token::Punctuation(Punctuation::LBracket) {
            input.next()?.unwrap();
            let element = parse_type(input)?;

            expect!(input, Token::Punctuation(Punctuation::Semicolon), "Semicolon".to_owned());
            expect!(input, Token::IntegerLiteral(value, extra), "IntegerLiteral".to_owned());

            if !extra.is_empty() {
                todo!()
            }

            expect!(input, Token::Punctuation(Punctuation::RBracket), "RBracket".to_owned());
            return Ok(Rc::new(Node::Type(TypeNode::SizedArray(element, value as usize))));
        } else if token == Token::BasicOperator(BasicOperator::Mul) {
            input.next()?.unwrap();
            // TODO *const/*mut
            let inner = parse_type(input)?;

            return Ok(Rc::new(Node::Type(TypeNode::Pointer(inner))));
        }
    }

    expect!(input, Token::Ident(name), "Identifier".to_owned());

    if let Some(token) = input.peek()? && token == Token::BasicOperator(BasicOperator::Lt) {
        todo!()
    } else {
        Ok(Rc::new(Node::Type(TypeNode::Simple(name))))
    }
});

def_parser!(pub parse_typed<S>(input: &mut S) -> (Rc<Node>, Rc<Node>) {
    expect!(input, Token::Ident(name), "Identifier".to_owned());
    expect!(input, Token::BasicOperator(BasicOperator::Colon), "Colon".to_owned());
    let ty = parse_type(input)?;

    Ok((Rc::new(Node::Ident(name)), ty))
});

def_parser!(pub parse_block<S>(input: &mut S) -> Rc<Node> {
    expect!(input, Token::Punctuation(Punctuation::LBrace), "LBrace".to_owned());

    let items = parse_many0(
        input,
        parse_statement_expr,
        Token::Punctuation(Punctuation::RBrace))?;

    Ok(Rc::new(Node::Block(items)))
});

def_parser!(pub parse_fn<S>(input: &mut S) -> Rc<Node> {
    expect!(input, Token::Ident(name), "Identifier".to_owned());
    expect!(input, Token::Punctuation(Punctuation::LParen), "LParen".to_owned());
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

    Ok(Rc::new(Node::Function {
        args,
        name,
        ret_type,
        body
    }))
});

def_parser!(pub parse_global_definition<S>(input: &mut S, is_const: bool) -> Rc<Node> {
    expect!(input, Token::Ident(name), "Identifier".to_owned());
    expect!(input, Token::BasicOperator(BasicOperator::Colon), "Colon".to_owned());
    let ty = parse_type(input)?;
    expect!(input, Token::BasicOperator(BasicOperator::Assign), "Assign".to_owned());
    let value = parse_expr(input)?;
    expect!(input, Token::Punctuation(Punctuation::Semicolon), "Semicolon".to_owned());

    Ok(Rc::new(Node::GlobalDefinition {
        is_const,
        name,
        ty,
        value
    }))
});

def_parser!(pub parse_extern<S>(input: &mut S) -> Rc<Node> {
    expect!(input, Token::Keyword(Keyword::Fn), "Fn".to_owned());
    expect!(input, Token::Ident(name), "Identifier".to_owned());
    expect!(input, Token::Punctuation(Punctuation::LParen), "LParen".to_owned());
    let arg_types = parse_delimited(
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

    expect!(input, Token::Punctuation(Punctuation::Semicolon), "Semicolon".to_owned());

    Ok(Rc::new(Node::ExternFunction {
        name,
        arg_types,
        ret_type
    }))
});

def_parser!(pub parse_item<S>(input: &mut S) -> Rc<Node> {
    let Some(token) = input.next()? else {
        return Err(ParserError::UnexpectedEof);
    };

    match token {
        Token::Keyword(Keyword::Fn) => parse_fn(input),
        Token::Keyword(Keyword::Extern) => parse_extern(input),
        Token::Keyword(Keyword::Static) => parse_global_definition(input, false),
        Token::Keyword(Keyword::Const) => parse_global_definition(input, true),
        _ => Err(ParserError::UnexpectedToken(token, "Item: fn/static/const".to_owned()))
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
