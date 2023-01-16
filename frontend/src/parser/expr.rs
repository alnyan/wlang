use std::rc::Rc;

use ast::{
    token::{BasicOperator, Keyword, Punctuation},
    Node, Token,
};

use super::{
    combinator::parse_many0,
    program::{maybe_call, parse_block, parse_type},
    ParserError,
};

fn precedence(op: &Token) -> u32 {
    match op {
        Token::CustomOperator(_) => 1,
        Token::BasicOperator(BasicOperator::Add) => 5,
        Token::BasicOperator(BasicOperator::Sub) => 5,
        Token::BasicOperator(BasicOperator::Mul) => 10,
        Token::BasicOperator(BasicOperator::Assign) => 0,
        _ => panic!(),
    }
}

def_parser!(pub parse_local_definition<S>(input: &mut S) -> Rc<Node> {
    // TODO maybe mut
    expect!(input, Token::Ident(name), "Identifier".to_owned());
    expect!(input, Token::BasicOperator(BasicOperator::Colon), "Colon".to_owned());
    let ty = parse_type(input)?;
    expect!(input, Token::BasicOperator(BasicOperator::Assign), "Assign".to_owned());
    let value = parse_expr(input)?;

    Ok(Rc::new(Node::LocalDefinition {
        is_mutable: false,
        name,
        ty,
        value
    }))
});

def_parser!(pub parse_condition<S>(input: &mut S) -> Rc<Node> {
    let condition = parse_expr(input)?;
    let if_true = parse_block(input)?;

    let if_false = if let Some(token) = input.peek()? && token == Token::Keyword(Keyword::Else) {
        input.next()?.unwrap();
        Some(parse_block(input)?)
    } else {
        None
    };

    Ok(Rc::new(Node::Condition { condition, if_true, if_false }))
});

def_parser!(pub parse_while_loop<S>(input: &mut S) -> Rc<Node> {
    let condition = parse_expr(input)?;
    let body = parse_block(input)?;

    Ok(Rc::new(Node::Loop {
        condition: Some(condition),
        body
    }))
});

def_parser!(pub parse_atom<S>(input: &mut S) -> Rc<Node> {
    let Some(token) = input.next()? else {
        return Err(ParserError::UnexpectedEof);
    };

    match token {
        Token::Ident(name) => Ok(Rc::new(Node::Ident(name))),
        Token::IntegerLiteral(value) => Ok(Rc::new(Node::IntegerLiteral(value))),
        Token::Keyword(Keyword::Let) => parse_local_definition(input),
        Token::Keyword(Keyword::If) => parse_condition(input),
        Token::Keyword(Keyword::While) => parse_while_loop(input),
        Token::Punctuation(Punctuation::LBrace) => {
            let items = parse_many0(
                input,
                parse_statement_expr,
                Token::Punctuation(Punctuation::RBrace))?;

            Ok(Rc::new(Node::Block(items)))
        },
        _ => Err(ParserError::UnexpectedToken(token, "Ident/IntegerLiteral/Keyword/LBrace".to_owned()))
    }
});

def_parser!(pub maybe_binary<S>(input: &mut S, this_left: Rc<Node>) -> Rc<Node> {
    let Some(this_op) = input.peek()? else {
        return Ok(this_left);
    };

    if !this_op.is_operator() {
        return Ok(this_left);
    }

    input.next()?.unwrap();

    let this_right = parse_expr(input)?;

    if let Node::Binary(that_op, that_left, that_right) = this_right.as_ref() {
        let this_prec = precedence(&this_op);
        let that_prec = precedence(that_op);

        if this_prec > that_prec {
            let inner = Rc::new(Node::Binary(this_op, this_left, that_left.clone()));
            Ok(Rc::new(Node::Binary(that_op.clone(), inner, that_right.clone())))
        } else {
            Ok(Rc::new(Node::Binary(this_op, this_left, this_right)))
        }
    } else {
        Ok(Rc::new(Node::Binary(this_op, this_left, this_right)))
    }
});

def_parser!(pub parse_expr<S>(input: &mut S) -> Rc<Node> {
    let atom = parse_atom(input)?;
    let call = maybe_call(input, atom)?;
    maybe_binary(input, call)
});

def_parser!(pub parse_statement_expr<S>(input: &mut S) -> Rc<Node> {
    let expr = parse_expr(input)?;

    if let Some(token) = input.peek()? && token == Token::Punctuation(Punctuation::Semicolon) {
        input.next()?.unwrap();
        Ok(Rc::new(Node::Statement(expr)))
    } else {
        Ok(expr)
    }
});
