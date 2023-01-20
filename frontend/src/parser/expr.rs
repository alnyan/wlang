use std::rc::Rc;

use ast::{
    token::{BasicOperator, Keyword, Punctuation, TokenValue},
    Node,
};

use super::{
    combinator::{parse_delimited, parse_many0},
    program::{maybe_call, parse_block, parse_type},
    ParserError,
};

fn precedence(op: &TokenValue) -> u32 {
    match op {
        TokenValue::CustomOperator(_) => 1,
        TokenValue::BasicOperator(BasicOperator::Add) => 5,
        TokenValue::BasicOperator(BasicOperator::Sub) => 5,
        TokenValue::BasicOperator(BasicOperator::Mul) => 10,
        TokenValue::BasicOperator(BasicOperator::Assign) => 0,
        TokenValue::BasicOperator(BasicOperator::Div) => 10,
        TokenValue::BasicOperator(BasicOperator::Mod) => 10,
        TokenValue::BasicOperator(BasicOperator::As) => 1,
        TokenValue::BasicOperator(BasicOperator::And) => 2,
        TokenValue::BasicOperator(BasicOperator::Or) => 2,
        TokenValue::BasicOperator(BasicOperator::Eq) => 3,
        _ => panic!("{op:?}"),
    }
}

def_parser!(pub parse_local_definition<S>(input: &mut S) -> Rc<Node> {
    // TODO maybe mut
    expect!(input, TokenValue::Ident(name), vec!["variable name"]);
    expect!(input, TokenValue::BasicOperator(BasicOperator::Colon), vec!["`:'"]);
    let ty = parse_type(input)?;
    expect!(input, TokenValue::BasicOperator(BasicOperator::Assign), vec!["`='"]);
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

    let if_false = if let Some(token) = input.peek()? && token.value == TokenValue::Keyword(Keyword::Else) {
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

def_parser!(pub parse_return<S>(input: &mut S) -> Rc<Node> {
    let return_expr = if let Some(token) = input.peek()? &&
        token.value != TokenValue::Punctuation(Punctuation::Semicolon) {
        input.next()?.unwrap();
        Some(parse_expr(input)?)
    } else {
        None
    };

    Ok(Rc::new(Node::Return(return_expr)))
});

def_parser!(pub parse_array<S>(input: &mut S) -> Rc<Node> {
    if let Some(token) = input.peek() ? &&
        token.value == TokenValue::Punctuation(Punctuation::RBracket) {
        input.next()?.unwrap();
        return Ok(Rc::new(Node::Array(vec![])));
    }

    let first = parse_expr(input)?;

    let Some(token) = input.next()? else {
        return Err(ParserError::UnexpectedEof);
    };

    match token.value {
        TokenValue::Punctuation(Punctuation::Semicolon) => {
            let count = parse_expr(input)?;
            expect!(input, TokenValue::Punctuation(Punctuation::RBracket), vec!["`]'"]);

            Ok(Rc::new(Node::ArrayRepeat(first, count)))
        },
        TokenValue::Punctuation(Punctuation::Comma) => {
            let mut items = parse_delimited(
                input,
                parse_expr,
                TokenValue::Punctuation(Punctuation::RBracket),
                TokenValue::Punctuation(Punctuation::Comma))?;
            items.insert(0, first);

            Ok(Rc::new(Node::Array(items)))
        }
        TokenValue::Punctuation(Punctuation::RBracket) => {
            Ok(Rc::new(Node::Array(vec![first])))
        }
        _ => Err(ParserError::UnexpectedToken(token, vec!["`;'", "`,'", "`]'"]))
    }
});

def_parser!(pub parse_atom<S>(input: &mut S) -> Rc<Node> {
    let Some(token) = input.next()? else {
        return Err(ParserError::UnexpectedEof);
    };

    match token.value {
        TokenValue::Ident(name) => Ok(Rc::new(Node::Ident(name))),
        TokenValue::IntegerLiteral(value, extra) => Ok(Rc::new(Node::IntegerLiteral(value, extra))),
        TokenValue::StringLiteral(value) => Ok(Rc::new(Node::StringLiteral(value))),
        TokenValue::Keyword(Keyword::Let) => parse_local_definition(input),
        TokenValue::Keyword(Keyword::If) => parse_condition(input),
        TokenValue::Keyword(Keyword::While) => parse_while_loop(input),
        TokenValue::Keyword(Keyword::Break) => Ok(Rc::new(Node::BreakLoop)),
        TokenValue::Keyword(Keyword::Return) => parse_return(input),
        TokenValue::BasicOperator(BasicOperator::Mul) => {
            let target = parse_expr_non_binary(input)?;
            Ok(Rc::new(Node::Dereference(target)))
        }
        TokenValue::BasicOperator(BasicOperator::BitAnd) => {
            let source = parse_expr_non_binary(input)?;
            Ok(Rc::new(Node::Reference(source)))
        }
        TokenValue::Punctuation(Punctuation::LBrace) => {
            let items = parse_many0(
                input,
                parse_statement_expr,
                TokenValue::Punctuation(Punctuation::RBrace))?;

            Ok(Rc::new(Node::Block(items)))
        },
        TokenValue::Punctuation(Punctuation::LParen) => {
            // TODO tuples
            let inner = parse_expr(input)?;
            expect!(input, TokenValue::Punctuation(Punctuation::RParen), vec!["`)'"]);
            Ok(inner)
        }
        TokenValue::Punctuation(Punctuation::LBracket) => parse_array(input),
        _ => Err(ParserError::UnexpectedToken(token, vec!["atom"]))
    }
});

def_parser!(pub maybe_binary<S>(input: &mut S, this_left: Rc<Node>) -> Rc<Node> {
    let Some(this_op) = input.peek()? else {
        return Ok(this_left);
    };

    if !this_op.value.is_operator() {
        return Ok(this_left);
    }

    input.next()?.unwrap();

    if let Some(maybe_lparen) = input.peek()? &&
        maybe_lparen.value == TokenValue::Punctuation(Punctuation::LParen) {
        input.next()?.unwrap();
        let this_right = parse_expr(input)?;
        expect!(input, TokenValue::Punctuation(Punctuation::RParen), vec!["`)'"]);
        return Ok(Rc::new(Node::Binary(this_op, this_left, this_right)));
    }

    let this_right = match this_op.value {
        TokenValue::BasicOperator(BasicOperator::As) => parse_type(input)?,
        _ => parse_expr(input)?
    };

    if let Node::Binary(that_op, that_left, that_right) = this_right.as_ref() {
        let this_prec = precedence(&this_op.value);
        let that_prec = precedence(&that_op.value);

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

def_parser!(pub maybe_array_element<S>(input: &mut S, expr: Rc<Node>) -> Rc<Node> {
    if let Some(token) = input.peek()? &&
        token.value == TokenValue::Punctuation(Punctuation::LBracket) {
        input.next()?.unwrap();
        let index = parse_expr(input)?;
        expect!(input, TokenValue::Punctuation(Punctuation::RBracket), vec!["`]'"]);
        Ok(Rc::new(Node::ArrayElement(expr, index)))
    } else {
        Ok(expr)
    }
});

def_parser!(pub parse_expr_non_binary<S>(input: &mut S) -> Rc<Node> {
    let atom = parse_atom(input)?;
    let array = maybe_array_element(input, atom)?;
    maybe_call(input, array)
});

def_parser!(pub parse_expr<S>(input: &mut S) -> Rc<Node> {
    let expr = parse_expr_non_binary(input)?;
    maybe_binary(input, expr)
});

def_parser!(pub parse_statement_expr<S>(input: &mut S) -> Rc<Node> {
    let expr = parse_expr(input)?;

    if let Some(token) = input.peek()? &&
        token.value == TokenValue::Punctuation(Punctuation::Semicolon) {
        input.next()?.unwrap();
        Ok(Rc::new(Node::Statement(expr)))
    } else {
        Ok(expr)
    }
});
