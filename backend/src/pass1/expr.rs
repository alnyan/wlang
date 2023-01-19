use std::{cell::RefCell, rc::Rc};

use ast::{node::TypeNode, token::BasicOperator, Node, Token};

use crate::{CompilerError, LangType, LocalValue, TaggedExpr, TaggedExprValue};

use super::{lvalue::pass1_lvalue, Pass1Program, Scope};

pub fn pass1_type(pass1: &Pass1Program, ty: &Rc<Node>) -> Result<Rc<LangType>, CompilerError> {
    let Node::Type(ty) = ty.as_ref() else {
        return Err(CompilerError::UnhandledNode(ty.clone()));
    };

    match ty {
        TypeNode::Simple(name) => Ok(pass1.pass0.named_type(name).unwrap()),
        TypeNode::SizedArray(element_ty, size) => {
            Ok(pass1_type(pass1, element_ty)?.make_sized_array_type(*size))
        }
        TypeNode::Pointer(inner) => Ok(pass1_type(pass1, inner)?.make_pointer_type()),
    }
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

    if !value.ty.is_compatible(&ty) {
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
    pass1: &Pass1Program,
    _scope: &Rc<RefCell<dyn Scope>>,
    op: BasicOperator,
    lhs: &Rc<LangType>,
    rhs: &Rc<LangType>,
) -> Result<Rc<LangType>, CompilerError> {
    match op {
        BasicOperator::Add if lhs.is_pointer() => {
            if !rhs.is_compatible(&pass1.pass0.i64_type()) {
                todo!()
            } else {
                Ok(lhs.clone())
            }
        }
        BasicOperator::Add
        | BasicOperator::Sub
        | BasicOperator::Mul
        | BasicOperator::Div
        | BasicOperator::Mod
        | BasicOperator::BitOr
        | BasicOperator::BitAnd
        | BasicOperator::Shl
        | BasicOperator::Shr => {
            if lhs.is_compatible(rhs) && lhs.is_integer() {
                Ok(lhs.clone())
            } else {
                todo!();
            }
        }
        BasicOperator::Le
        | BasicOperator::Lt
        | BasicOperator::Ge
        | BasicOperator::Gt
        | BasicOperator::Ne
        | BasicOperator::Eq => {
            if lhs.is_compatible(rhs) && lhs.is_integer() {
                Ok(pass1.pass0.bool_type())
            } else {
                todo!()
            }
        }
        BasicOperator::And | BasicOperator::Or => {
            if lhs.is_compatible(rhs) && lhs.is_compatible(&pass1.pass0.bool_type()) {
                Ok(pass1.pass0.bool_type())
            } else {
                todo!()
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
    if let Token::BasicOperator(BasicOperator::As) = op {
        let lhs = pass1_expr(pass1, scope, lhs)?;
        let target_ty = pass1_type(pass1, rhs)?;
        Ok(Rc::new(TaggedExpr {
            ty: target_ty.clone(),
            fn_index: scope.borrow().function_index(),
            scope_index: scope.borrow().index(),
            ast_node: expr.clone(),
            value: TaggedExprValue::Cast(lhs, target_ty),
        }))
    } else if let Token::BasicOperator(BasicOperator::Assign) = op {
        let lhs = pass1_lvalue(pass1, scope, lhs)?;
        let rhs = pass1_expr(pass1, scope, rhs)?;

        Ok(Rc::new(TaggedExpr {
            ty: pass1.pass0.void_type(),
            fn_index: scope.borrow().function_index(),
            scope_index: scope.borrow().index(),
            ast_node: expr.clone(),
            value: TaggedExprValue::Assign(lhs, rhs)
        }))
    } else {
        let lhs = pass1_expr(pass1, scope, lhs)?;
        let rhs = pass1_expr(pass1, scope, rhs)?;

        let ty = match op {
            Token::BasicOperator(op) => pass1_basic_binary(pass1, scope, *op, &lhs.ty, &rhs.ty)?,
            _ => panic!("{expr:?}"),
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
            if !matches!(item.as_ref(), Node::Statement(_))
                && !matches!(item.as_ref(), Node::Condition { .. })
                && !matches!(item.as_ref(), Node::Loop { .. })
            {
                return Err(CompilerError::InvalidOperation(
                    item.clone(),
                    "Expected a statement, not an expression".to_string(),
                ));
            }

            let value = pass1_expr(pass1, &block_scope, item)?;

            assert!(value.ty.is_compatible(&pass1.pass0.void_type()));
            tagged_items.push(value);
        }

        let value = pass1_expr(pass1, &block_scope, terminal)?;
        let value_ty = value.ty.clone();

        tagged_items.push(value);

        value_ty
    } else {
        pass1.pass0.void_type()
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

pub fn pass1_condition(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
    condition: &Rc<Node>,
    if_true: &Rc<Node>,
    if_false: &Option<Rc<Node>>,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let condition = pass1_expr(pass1, scope, condition)?;

    if !condition.ty.is_compatible(&pass1.pass0.bool_type()) {
        todo!()
    }

    let if_true = pass1_expr(pass1, scope, if_true)?;
    let if_false = if let Some(if_false) = if_false {
        Some(pass1_expr(pass1, scope, if_false)?)
    } else {
        None
    };
    // TODO check types
    let ty = if let Some(if_false) = if_false.as_ref() {
        if !if_false.ty.is_compatible(&if_true.ty) {
            todo!();
        }

        if_true.ty.clone()
    } else {
        pass1.pass0.void_type()
    };

    Ok(Rc::new(TaggedExpr {
        ty,
        fn_index: scope.borrow().function_index(),
        scope_index: scope.borrow().index(),
        ast_node: expr.clone(),
        value: TaggedExprValue::Condition {
            condition,
            if_true,
            if_false,
        },
    }))
}

pub fn pass1_loop(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
    condition: &Option<Rc<Node>>,
    body: &Rc<Node>,
) -> Result<Rc<TaggedExpr>, CompilerError> {
    let condition = if let Some(condition) = condition {
        let value = pass1_expr(pass1, scope, condition)?;
        assert!(value.ty.is_compatible(&pass1.pass0.bool_type()));
        Some(value)
    } else {
        None
    };
    let body = pass1_expr(pass1, scope, body)?;

    Ok(Rc::new(TaggedExpr {
        ty: pass1.pass0.void_type(),
        fn_index: scope.borrow().function_index(),
        scope_index: scope.borrow().index(),
        ast_node: expr.clone(),
        value: TaggedExprValue::Loop { condition, body },
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
            if arg.ty.is_compatible(expected) {
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
            let ty = pass1_ident_type(pass1, scope, name)?;
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
        Node::IntegerLiteral(value, extra) => Ok(Rc::new(TaggedExpr {
            ty: pass1.pass0.integer_literal_extra_type(extra).unwrap(),
            ast_node: expr.clone(),
            scope_index: scope.borrow().index(),
            fn_index: scope.borrow().function_index(),
            value: TaggedExprValue::IntegerLiteral(*value),
        })),
        Node::StringLiteral(value) => Ok(Rc::new(TaggedExpr {
            ty: pass1.pass0.i8_type().make_pointer_type(),
            ast_node: expr.clone(),
            fn_index: scope.borrow().function_index(),
            scope_index: scope.borrow().index(),
            value: TaggedExprValue::StringLiteral(value.clone())
        })),
        Node::Condition {
            condition,
            if_true,
            if_false,
        } => pass1_condition(pass1, scope, expr, condition, if_true, if_false),
        Node::Loop { condition, body } => pass1_loop(pass1, scope, expr, condition, body),
        Node::Binary(op, lhs, rhs) => pass1_binary(pass1, scope, expr, op, lhs, rhs),
        Node::LocalDefinition {
            name,
            is_mutable,
            ty,
            value,
        } => pass1_local_definition(pass1, scope, expr, name, *is_mutable, ty, value),
        Node::Call(callee, args) => pass1_call(pass1, scope, expr, callee, args),
        // TODO check if actually in a loop
        Node::BreakLoop => Ok(Rc::new(TaggedExpr {
            ty: pass1.pass0.void_type(),
            fn_index: scope.borrow().function_index(),
            scope_index: scope.borrow().index(),
            ast_node: expr.clone(),
            value: TaggedExprValue::BreakLoop,
        })),
        Node::ArrayElement(array, index) => {
            let array = pass1_expr(pass1, scope, array)?;
            let index = pass1_expr(pass1, scope, index)?;

            if !index.ty.is_compatible(&pass1.pass0.i64_type()) {
                todo!("Index type is not an i64 (TODO: usize)");
            }

            let LangType::SizedArrayType(elem_ty, _) = array.ty.as_ref() else {
                todo!("Indexed value is not an array");
            };

            Ok(Rc::new(TaggedExpr {
                ty: elem_ty.clone(),
                fn_index: scope.borrow().function_index(),
                scope_index: scope.borrow().index(),
                ast_node: expr.clone(),
                value: TaggedExprValue::ArrayElement(array, index),
            }))
        }
        Node::Array(elements) => {
            let elements = elements
                .iter()
                .map(|elem| pass1_expr(pass1, scope, elem))
                .collect::<Result<Vec<_>, _>>()?;

            if elements.is_empty() {
                todo!();
            }

            let first_ty = elements[0].ty.clone();
            for elem in elements.iter() {
                if !elem.ty.is_compatible(&first_ty) {
                    todo!();
                }
            }

            Ok(Rc::new(TaggedExpr {
                ty: first_ty.make_sized_array_type(elements.len()),
                fn_index: scope.borrow().function_index(),
                scope_index: scope.borrow().index(),
                ast_node: expr.clone(),
                value: TaggedExprValue::Array(elements),
            }))
        }
        Node::Dereference(ptr) => {
            let ptr = pass1_expr(pass1, scope, ptr)?;

            match ptr.ty.as_ref() {
                LangType::Pointer(inner_ty) => Ok(Rc::new(TaggedExpr {
                    ty: inner_ty.clone(),
                    fn_index: scope.borrow().function_index(),
                    scope_index: scope.borrow().index(),
                    ast_node: expr.clone(),
                    value: TaggedExprValue::Dereference(ptr),
                })),
                _ => todo!(),
            }
        }
        Node::Reference(lvalue) => {
            let lvalue = pass1_lvalue(pass1, scope, lvalue)?;

            Ok(Rc::new(TaggedExpr {
                ty: lvalue.ty.clone().make_pointer_type(),
                fn_index: scope.borrow().function_index(),
                scope_index: scope.borrow().index(),
                ast_node: expr.clone(),
                value: TaggedExprValue::Reference(lvalue)
            }))
        }
        Node::Return(return_expr) => {
            let parent_ty = scope.borrow().function_return_type();

            let ret_value = if let Some(return_expr) = return_expr {
                let ret_value = pass1_expr(pass1, scope, return_expr)?;

                if !ret_value.ty.is_compatible(&parent_ty) {
                    todo!();
                }

                Some(ret_value)
            } else {
                if !pass1.pass0.void_type().is_compatible(&parent_ty) {
                    todo!();
                }

                None
            };

            Ok(Rc::new(TaggedExpr {
                ty: pass1.pass0.void_type(),
                fn_index: scope.borrow().function_index(),
                scope_index: scope.borrow().index(),
                ast_node: expr.clone(),
                value: TaggedExprValue::Return(ret_value),
            }))
        }
        _ => todo!("{:?}", expr),
    }
}

pub fn pass1_ident_type(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    name: &str,
) -> Result<Rc<LangType>, CompilerError> {
    if let Some(local) = scope.borrow().local(name) {
        Ok(local.ty)
    } else if let Some(global) = pass1.globals.get(name) {
        Ok(global.ty.clone())
    } else if pass1.function(name).is_some() {
        Ok(pass1.pass0.void_type())
    } else {
        todo!()
    }
}
