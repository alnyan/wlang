use std::{cell::RefCell, rc::Rc};

use ast::{token::BasicOperator, Node, Token};

use crate::{CompilerError, LangType, LocalValue, TaggedExpr, TaggedExprValue};

use super::{Pass1Program, Scope};

pub fn pass1_type(pass1: &Pass1Program, ty: &Rc<Node>) -> Result<Rc<LangType>, CompilerError> {
    let Node::Type(name) = ty.as_ref() else {
        return Err(CompilerError::UnhandledNode(ty.clone()));
    };

    pass1
        .pass0
        .named_type(name)
        .ok_or(CompilerError::UndefinedType(name.to_string()))
}

pub fn pass1_local_definition(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
    name: &str,
    is_mutable: bool,
    ty: &Rc<Node>,
    value: &Rc<Node>,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let ty = pass1_type(pass1, ty)?;

    // Check value
    let value = pass1_expr(pass1, scope, value)?;

    if value.ty != ty {
        return Err(CompilerError::TypeMismatchUnary(ty, value.ty.clone()));
    }

    let scope_index = scope.borrow().index();
    let fn_index = scope.borrow().function_index();

    scope.borrow_mut().add_local(
        name,
        LocalValue {
            ty: ty.clone(),
            is_mutable,
            scope_index,
            fn_index,
        },
    );

    Ok(Rc::new(TaggedExpr {
        ty: pass1.pass0.void_type(),
        ast_node: expr.clone(),
        scope_index: scope.borrow().index(),
        fn_index: scope.borrow().function_index(),
        value: TaggedExprValue::LocalDefinition {
            ty,
            name: name.to_owned(),
            value,
        },
    }))
}

fn pass1_basic_binary(
    _pass1: &Pass1Program,
    _scope: &Rc<RefCell<dyn Scope>>,
    op: BasicOperator,
    lhs: &Rc<LangType>,
    rhs: &Rc<LangType>,
) -> Result<Rc<LangType>, CompilerError> {
    match op {
        BasicOperator::Add
        | BasicOperator::Sub
        | BasicOperator::Mul
        | BasicOperator::Div
        | BasicOperator::Mod => {
            if lhs == rhs {
                Ok(lhs.clone())
            } else {
                todo!();
            }
        }
        _ => todo!(),
    }
}

pub fn pass1_binary(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
    op: &Token,
    lhs: &Rc<Node>,
    rhs: &Rc<Node>,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let lhs = pass1_expr(pass1, scope, lhs)?;
    let rhs = pass1_expr(pass1, scope, rhs)?;

    let ty = match op {
        Token::BasicOperator(op) => pass1_basic_binary(pass1, scope, *op, &lhs.ty, &rhs.ty)?,
        _ => panic!(),
    };

    Ok(Rc::new(TaggedExpr {
        ty,
        ast_node: expr.clone(),
        scope_index: scope.borrow().index(),
        fn_index: scope.borrow().function_index(),
        value: TaggedExprValue::Binary {
            op: op.clone(),
            lhs,
            rhs,
        },
    }))
}

pub fn pass1_block(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
    items: &[Rc<Node>],
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let block_scope = scope.borrow_mut().add_scope(scope.clone());

    let mut tagged_items = vec![];
    let ty = if !items.is_empty() {
        let terminal = &items[items.len() - 1];
        let non_terminal = &items[..items.len() - 1];

        for item in non_terminal {
            if !matches!(item.as_ref(), Node::Statement(_)) {
                return Err(CompilerError::InvalidOperation(
                    item.clone(),
                    "Expected a statement, not an expression".to_string(),
                ));
            }

            let value = pass1_expr(pass1, &block_scope, item)?;

            assert_eq!(value.ty, pass1.pass0.void_type());
            tagged_items.push(value);
        }

        let value = pass1_expr(pass1, &block_scope, terminal)?;
        let value_ty = value.ty.clone();

        tagged_items.push(value);

        value_ty
    } else {
        todo!()
    };

    let scope_index = block_scope.borrow().index();
    Ok(Rc::new(TaggedExpr {
        ty,
        ast_node: expr.clone(),
        fn_index: scope.borrow().function_index(),
        scope_index,
        value: TaggedExprValue::Block(tagged_items),
    }))
}

pub fn pass1_call(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
    callee: &Rc<Node>,
    args: &[Rc<Node>],
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let callee_func = match callee.as_ref() {
        Node::Ident(name) => pass1.function(name).unwrap(),
        _ => todo!(),
    };
    let tagged_callee = pass1_expr(pass1, scope, callee)?;
    let callee_return_ty = callee_func.signature.return_type.clone();
    let args = args
        .iter()
        .map(|arg| pass1_expr(pass1, scope, arg))
        .collect::<Result<Vec<_>, _>>()?;

    if args.len() != callee_func.signature.arg_types.len() {
        todo!();
    }

    args.iter()
        .zip(callee_func.signature.arg_types.iter())
        .map(|(arg, (_, expected))| {
            if &arg.ty == expected {
                Ok(())
            } else {
                Err(CompilerError::TypeMismatchUnary(
                    expected.clone(),
                    arg.ty.clone(),
                ))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(Rc::new(TaggedExpr {
        ty: callee_return_ty,
        fn_index: scope.borrow().function_index(),
        scope_index: scope.borrow().index(),
        ast_node: expr.clone(),
        value: TaggedExprValue::Call(tagged_callee, args),
    }))
}

pub fn pass1_expr(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    match expr.as_ref() {
        Node::Ident(name) => {
            let ty = if let Some(local) = scope.borrow().local(name) {
                local.ty
            } else if let Some(global) = pass1.globals.get(name) {
                global.ty.clone()
            } else if pass1.function(name).is_some() {
                pass1.pass0.void_type()
            } else {
                todo!()
            };

            Ok(Rc::new(TaggedExpr {
                ty,
                ast_node: expr.clone(),
                scope_index: scope.borrow().index(),
                fn_index: scope.borrow().function_index(),
                value: TaggedExprValue::Ident(name.to_owned()),
            }))
        }
        Node::Block(items) => pass1_block(pass1, scope, expr, items),
        Node::Statement(inner) => {
            let value = pass1_expr(pass1, scope, inner)?;

            Ok(Rc::new(TaggedExpr {
                ty: pass1.pass0.void_type(),
                ast_node: expr.clone(),
                scope_index: scope.borrow().index(),
                fn_index: scope.borrow().function_index(),
                value: TaggedExprValue::Statement(value),
            }))
        }
        Node::IntegerLiteral(value) => Ok(Rc::new(TaggedExpr {
            ty: pass1.pass0.named_type("u64").unwrap(),
            ast_node: expr.clone(),
            scope_index: scope.borrow().index(),
            fn_index: scope.borrow().function_index(),
            value: TaggedExprValue::IntegerLiteral(*value),
        })),
        Node::Binary(op, lhs, rhs) => pass1_binary(pass1, scope, expr, op, lhs, rhs),
        Node::LocalDefinition {
            name,
            is_mutable,
            ty,
            value,
        } => pass1_local_definition(pass1, scope, expr, name, *is_mutable, ty, value),
        Node::Call(callee, args) => pass1_call(pass1, scope, expr, callee, args),
        _ => todo!("{:?}", expr),
    }
}
