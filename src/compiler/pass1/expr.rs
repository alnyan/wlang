use std::{cell::RefCell, rc::Rc};

use crate::{
    compiler::{CompilerError, LangType, LocalValue, TaggedExpr, TaggedExprValue},
    lexer::token::{BasicOperator, Token},
    parser::{LocalDefinition, Node},
};

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
    def: &LocalDefinition,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let is_mutable = def.is_mutable;
    let ty = pass1_type(pass1, &def.ty)?;

    // Check value
    let value = pass1_expr(pass1, scope, &def.value)?;

    if value.ty != ty {
        return Err(CompilerError::TypeMismatchUnary(ty, value.ty.clone()));
    }

    scope.borrow_mut().add_local(
        def.name.as_str(),
        LocalValue {
            ty: ty.clone(),
            is_mutable,
        },
    );

    Ok(Rc::new(TaggedExpr {
        ty: pass1.pass0.void_type(),
        ast_node: expr.clone(),
        scope_index: scope.borrow().index(),
        fn_index: scope.borrow().function_index(),
        value: TaggedExprValue::LocalDefinition { ty, value },
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

    Ok(Rc::new(TaggedExpr {
        ty,
        ast_node: expr.clone(),
        scope_index: scope.borrow().index(),
        fn_index: scope.borrow().function_index(),
        value: TaggedExprValue::Block(tagged_items),
    }))
}

pub fn pass1_expr(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    match expr.as_ref() {
        Node::Ident(name) => {
            let ty = if let Some(local) = scope.borrow().local(name.as_str()) {
                local.ty.clone()
            } else if let Some(global) = pass1.globals.get(name.as_str()) {
                global.ty.clone()
            } else {
                todo!();
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
        Node::LocalDefinition(def) => pass1_local_definition(pass1, scope, expr, def),
        _ => todo!("{:?}", expr),
    }
}
