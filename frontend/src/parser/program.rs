use std::rc::Rc;

use ast::{
    node::TypeNode,
    token::{BasicOperator, Keyword, Punctuation, TokenValue},
    Node,
};

use super::{
    combinator::{parse_delimited, parse_many0},
    expr::{parse_expr, parse_statement_expr},
    ParserError,
};

def_parser!(pub maybe_call<S>(input: &mut S, left: Rc<Node>) -> Rc<Node> {
    if let Some(token) = input.peek()? &&
        token.value == TokenValue::Punctuation(Punctuation::LParen) {
        input.next()?.unwrap();
        let args = parse_delimited(
            input,
            parse_expr,
            TokenValue::Punctuation(Punctuation::RParen),
            TokenValue::Punctuation(Punctuation::Comma))?;
        Ok(Rc::new(Node::Call(left, args)))
    } else {
        Ok(left)
    }
});

def_parser!(pub parse_type<S>(input: &mut S) -> Rc<Node> {
    if let Some(token) = input.peek()? {
        if token.value == TokenValue::Punctuation(Punctuation::LBracket) {
            input.next()?.unwrap();
            let element = parse_type(input)?;

            expect!(input, TokenValue::Punctuation(Punctuation::Semicolon), vec!["`;'"]);
            expect!(input, TokenValue::IntegerLiteral(value, extra), vec!["array size"]);

            if !extra.is_empty() {
                todo!()
            }

            expect!(input, TokenValue::Punctuation(Punctuation::RBracket), vec!["`}'"]);
            return Ok(Rc::new(Node::Type(TypeNode::SizedArray(element, value as usize))));
        } else if token.value == TokenValue::BasicOperator(BasicOperator::Mul) {
            input.next()?.unwrap();
            // TODO *const/*mut
            let inner = parse_type(input)?;

            return Ok(Rc::new(Node::Type(TypeNode::Pointer(inner))));
        }
    }

    expect!(input, TokenValue::Ident(name), vec!["identifier"]);

    if let Some(token) = input.peek()? &&
        token.value == TokenValue::BasicOperator(BasicOperator::Lt) {
        todo!()
    } else {
        Ok(Rc::new(Node::Type(TypeNode::Simple(name))))
    }
});

def_parser!(pub parse_typed<S>(input: &mut S) -> (Rc<Node>, Rc<Node>) {
    expect!(input, TokenValue::Ident(name), vec!["type name"]);
    expect!(input, TokenValue::BasicOperator(BasicOperator::Colon), vec!["`:'"]);
    let ty = parse_type(input)?;

    Ok((Rc::new(Node::Ident(name)), ty))
});

def_parser!(pub parse_block<S>(input: &mut S) -> Rc<Node> {
    expect!(input, TokenValue::Punctuation(Punctuation::LBrace), vec!["`{'"]);

    let items = parse_many0(
        input,
        parse_statement_expr,
        TokenValue::Punctuation(Punctuation::RBrace))?;

    Ok(Rc::new(Node::Block(items)))
});

def_parser!(pub parse_fn<S>(input: &mut S) -> Rc<Node> {
    expect!(input, TokenValue::Ident(name), vec!["function name"]);
    expect!(input, TokenValue::Punctuation(Punctuation::LParen), vec!["`('"]);
    let args = parse_delimited(
        input,
        parse_typed,
        TokenValue::Punctuation(Punctuation::RParen),
        TokenValue::Punctuation(Punctuation::Comma))?;

    let ret_type = if let Some(token) = input.peek()? &&
        token.value == TokenValue::BasicOperator(BasicOperator::Arrow) {
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
    expect!(input, TokenValue::Ident(name), vec!["global name"]);
    expect!(input, TokenValue::BasicOperator(BasicOperator::Colon), vec!["`:'"]);
    let ty = parse_type(input)?;
    expect!(input, TokenValue::BasicOperator(BasicOperator::Assign), vec!["`='"]);
    let value = parse_expr(input)?;
    expect!(input, TokenValue::Punctuation(Punctuation::Semicolon), vec!["`;'"]);

    Ok(Rc::new(Node::GlobalDefinition {
        is_const,
        name,
        ty,
        value
    }))
});

def_parser!(pub parse_extern<S>(input: &mut S) -> Rc<Node> {
    expect!(input, TokenValue::Keyword(Keyword::Fn), vec!["`fn' keyword"]);
    expect!(input, TokenValue::Ident(name), vec!["function name"]);
    expect!(input, TokenValue::Punctuation(Punctuation::LParen), vec!["`('"]);
    let arg_types = parse_delimited(
        input,
        parse_typed,
        TokenValue::Punctuation(Punctuation::RParen),
        TokenValue::Punctuation(Punctuation::Comma))?;

    let ret_type = if let Some(token) = input.peek()? &&
        token.value == TokenValue::BasicOperator(BasicOperator::Arrow) {
        input.next()?.unwrap();
        Some(parse_type(input)?)
    } else {
        None
    };

    expect!(input, TokenValue::Punctuation(Punctuation::Semicolon), vec!["`;'"]);

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

    match token.value {
        TokenValue::Keyword(Keyword::Fn) => parse_fn(input),
        TokenValue::Keyword(Keyword::Extern) => parse_extern(input),
        TokenValue::Keyword(Keyword::Static) => parse_global_definition(input, false),
        TokenValue::Keyword(Keyword::Const) => parse_global_definition(input, true),
        _ => Err(ParserError::UnexpectedToken(token, vec!["`fn'", "`static'", "`extern'", "`const'"]))
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
