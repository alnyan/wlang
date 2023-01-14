use std::rc::Rc;

use crate::{parser::Node, lexer::token::{Token, BasicOperator}};

use super::{Location, CompileError, Context};

pub fn emit_mov(dst: Location, src: Location) -> Result<Location, CompileError> {
    if dst == src {
        Ok(dst)
    } else {
        println!("mov {:?}, {:?}", dst, src);
        Ok(dst)
    }
}

pub fn emit_binary(
    ctx: &mut Context,
    op: Token,
    left: Rc<Node>,
    right: Rc<Node>,
) -> Result<Location, CompileError> {
    let left_loc = emit_expr(ctx, left)?;
    let right_loc = emit_expr(ctx, right)?;

    let a = emit_mov(Location::Register('A'), left_loc)?;

    match op {
        Token::BasicOperator(BasicOperator::Add) => {
            println!("add {:?}, {:?}", a, right_loc);
        },
        Token::BasicOperator(BasicOperator::Mul) => {
            println!("mov {:?}, {:?}", Location::Register('B'), right_loc);
            println!("mul");
        },
        _ => todo!()
    };
    println!();

    Ok(Location::Register('A'))
}

pub fn emit_call(ctx: &mut Context, func: Rc<Node>, args: &[Rc<Node>]) -> Result<Location, CompileError> {
    let func_loc = emit_expr(ctx, func)?;
    let arg_locs = args
        .iter()
        .map(|arg| emit_expr(ctx, arg.clone()))
        .collect::<Result<Vec<_>, _>>()?;
    let res_loc = Location::Temporary(ctx.alloc_temp());

    for (i, arg_loc) in arg_locs.iter().enumerate() {
        println!("mov arg{}, {:?}", i, arg_loc);
    }

    println!("call {:?}", func_loc);
    println!("mov {:?}, {:?}", res_loc, Location::Register('A'));
    println!();

    Ok(res_loc)
}

pub fn emit_ident(ctx: &mut Context, name: &str) -> Result<Location, CompileError> {
    Ok(Location::Variable(name.to_owned()))
}

pub fn emit_expr(ctx: &mut Context, expr: Rc<Node>) -> Result<Location, CompileError> {
    match expr.as_ref() {
        Node::Binary(op, left, right) => emit_binary(ctx, op.clone(), left.clone(), right.clone()),
        Node::Call(func, args) => emit_call(ctx, func.clone(), args),
        Node::Ident(name) => emit_ident(ctx,name),
        _ => todo!(),
    }
}
