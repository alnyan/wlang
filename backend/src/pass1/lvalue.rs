use std::{cell::RefCell, rc::Rc};

use ast::{token::BasicOperator, Node, Token};

use crate::{
    tagged::{TaggedLvalueExpr, TaggedLvalueExprValue},
    CompilerError, LangType, Pass1Program,
};

use super::{
    expr::{pass1_expr, pass1_ident_type},
    Scope,
};

pub fn pass1_lvalue(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
) -> Result<Rc<TaggedLvalueExpr>, CompilerError> {
    match expr.as_ref() {
        Node::Ident(name) => {
            let ty = pass1_ident_type(pass1, scope, name)?;
            Ok(Rc::new(TaggedLvalueExpr {
                ty,
                fn_index: scope.borrow().function_index(),
                scope_index: scope.borrow().index(),
                ast_node: expr.clone(),
                value: TaggedLvalueExprValue::Ident(name.clone()),
            }))
        }
        Node::Dereference(ptr) => {
            let ptr = pass1_expr(pass1, scope, ptr)?;
            if let LangType::Pointer(inner_ty) = ptr.ty.as_ref() {
                Ok(Rc::new(TaggedLvalueExpr {
                    ty: inner_ty.clone(),
                    fn_index: scope.borrow().function_index(),
                    scope_index: scope.borrow().index(),
                    ast_node: expr.clone(),
                    value: TaggedLvalueExprValue::Dereference(ptr),
                }))
            } else {
                todo!("Dereference of a non-pointer value")
            }
        }
        Node::ArrayElement(array, index) => {
            let array = pass1_lvalue(pass1, scope, array)?;
            let index = pass1_expr(pass1, scope, index)?;

            if !index.ty.is_compatible(&pass1.pass0.i64_type()) {
                todo!("Array index isn't an i64 value");
            }

            if let LangType::SizedArrayType(inner_ty, _) = array.ty.as_ref() {
                Ok(Rc::new(TaggedLvalueExpr {
                    ty: inner_ty.clone(),
                    fn_index: scope.borrow().function_index(),
                    scope_index: scope.borrow().index(),
                    ast_node: expr.clone(),
                    value: TaggedLvalueExprValue::ArrayElement(array, index),
                }))
            } else {
                todo!("Index of a non-array value")
            }
        }
        _ => todo!("{expr:?}"),
    }
}
