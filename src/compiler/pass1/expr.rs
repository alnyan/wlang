use std::{rc::Rc, cell::RefCell, collections::HashMap};

use crate::{compiler::{LangType, CompilerError, LocalValue}, parser::{LocalDefinition, Node}, lexer::token::{BasicOperator, Token}};

use super::{Scope, Pass1Program, BlockScope};


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
    def: &LocalDefinition,
) -> Result<Rc<LangType>, CompilerError> {
    let is_mutable = def.is_mutable;
    let ty = pass1_type(pass1, &def.ty)?;

    // Check value
    let value_ty = pass1_expr(pass1, scope, &def.value)?;
    if value_ty != ty {
        return Err(CompilerError::TypeMismatchUnary(ty, value_ty));
    }

    scope
        .borrow_mut()
        .add_local(def.name.as_str(), LocalValue { ty, is_mutable });
    Ok(pass1.pass0.void_type())
}

fn pass1_basic_binary(
    _pass1: &Pass1Program,
    _scope: &Rc<RefCell<dyn Scope>>,
    op: BasicOperator,
    lhs_ty: Rc<LangType>,
    rhs_ty: Rc<LangType>,
) -> Result<Rc<LangType>, CompilerError> {
    match op {
        BasicOperator::Add
        | BasicOperator::Sub
        | BasicOperator::Mul
        | BasicOperator::Div
        | BasicOperator::Mod => {
            if lhs_ty == rhs_ty {
                Ok(lhs_ty)
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
    op: &Token,
    lhs: &Rc<Node>,
    rhs: &Rc<Node>,
) -> Result<Rc<LangType>, CompilerError> {
    let lhs_ty = pass1_expr(pass1, scope, lhs)?;
    let rhs_ty = pass1_expr(pass1, scope, rhs)?;

    match op {
        Token::BasicOperator(op) => pass1_basic_binary(pass1, scope, *op, lhs_ty, rhs_ty),
        _ => panic!(),
    }
}

pub fn pass1_expr(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
) -> Result<Rc<LangType>, CompilerError> {
    match expr.as_ref() {
        Node::Ident(name) => {
            if let Some(local) = scope.borrow().local(name.as_str()) {
                return Ok(local.ty);
            }

            if let Some(global) = pass1.globals.get(name.as_str()) {
                return Ok(global.ty.clone());
            }

            todo!()
        }
        Node::Block(items) => {
            let block_scope: Rc<RefCell<dyn Scope>> = Rc::new(RefCell::new(BlockScope {
                locals: HashMap::new(),
                parent: scope.clone(),
            }));
            for (i, item) in items.iter().enumerate() {
                if i != items.len() - 1 && !matches!(item.as_ref(), Node::Statement(_)) {
                    return Err(CompilerError::InvalidOperation(
                        item.clone(),
                        "Expected a statement, not an expression".to_string(),
                    ));
                }

                let ty = pass1_expr(pass1, &block_scope, item)?;

                if i != items.len() - 1 {
                    assert_eq!(ty, pass1.pass0.void_type());
                } else {
                    return Ok(ty);
                }
            }
            // Empty block
            Ok(pass1.pass0.void_type())
        }
        Node::Statement(expr) => {
            pass1_expr(pass1, scope, expr)?;
            Ok(pass1.pass0.void_type())
        }
        Node::IntegerLiteral(_) => Ok(pass1.pass0.named_type("u64").unwrap()),
        Node::Binary(op, lhs, rhs) => pass1_binary(pass1, scope, op, lhs, rhs),
        Node::LocalDefinition(def) => pass1_local_definition(pass1, scope, def),
        _ => todo!("{:?}", expr),
    }
}
