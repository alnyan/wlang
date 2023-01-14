use std::rc::Rc;

use crate::{
    lexer::token::{BasicOperator, Token},
    parser::Node,
};

use super::{type_eq, CompileError, Context, TypeInfo};

fn basic_arithm(a: &Rc<TypeInfo>, b: &Rc<TypeInfo>) -> Result<Rc<TypeInfo>, CompileError> {
    if !type_eq(&a, &b) {
        todo!()
    }

    if a.is_primitive_numeric() {
        Ok(a.clone())
    } else {
        todo!()
    }
}

pub fn pass1_binary(
    ctx: &mut Context,
    op: Token,
    left: Rc<Node>,
    right: Rc<Node>,
) -> Result<Rc<TypeInfo>, CompileError> {
    let left = pass1_expr(ctx, left)?;
    let right = pass1_expr(ctx, right)?;

    match op {
        Token::BasicOperator(BasicOperator::Add)
        | Token::BasicOperator(BasicOperator::Sub)
        | Token::BasicOperator(BasicOperator::Mul)
        | Token::BasicOperator(BasicOperator::Div)
        | Token::BasicOperator(BasicOperator::Mod) => basic_arithm(&left, &right),
        _ => todo!(),
    }
}

pub fn pass1_integer_literal(ctx: &mut Context, value: u64) -> Result<Rc<TypeInfo>, CompileError> {
    let ty = ctx.get_type("u64").unwrap();

    Ok(ty)
}

pub fn pass1_expr(ctx: &mut Context, expr: Rc<Node>) -> Result<Rc<TypeInfo>, CompileError> {
    match expr.as_ref() {
        Node::IntegerLiteral(value) => pass1_integer_literal(ctx, *value),
        Node::Binary(op, left, right) => pass1_binary(ctx, op.clone(), left.clone(), right.clone()),
        _ => todo!(),
    }
}

pub fn pass1_module(ctx: &mut Context, items: &[Rc<Node>]) -> Result<(), CompileError> {
    todo!()
}
