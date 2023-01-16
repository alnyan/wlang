use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    compiler::{
        CompilerError, FunctionSignature, GlobalValue, LangFunction, LocalValue, Pass0Program,
    },
    parser::{Function, GlobalDefinition, Node},
};

use super::{
    expr::{pass1_expr, pass1_type},
    FunctionScope, Pass1Program, Scope,
};

pub fn pass1_function(
    pass1: &Pass1Program,
    index: usize,
    name: String,
    args: &[(Rc<Node>, Rc<Node>)],
    ret_type: &Option<Rc<Node>>,
    body: &Rc<Node>,
) -> Result<LangFunction, CompilerError> {
    let return_type = if let Some(ty) = ret_type {
        pass1_type(pass1, ty)?
    } else {
        pass1.pass0.void_type()
    };

    let mut locals = vec![];
    for (name, ty) in args {
        let Node::Ident(name) = name.as_ref() else {
            return Err(CompilerError::UnhandledNode(name.clone()));
        };
        let ty = pass1_type(pass1, ty)?;

        locals.push((
            name.clone(),
            LocalValue {
                ty,
                is_mutable: false,
                fn_index: index,
                scope_index: None
            },
        ));
    }

    let arg_types = locals.iter().map(|(_, t)| t.ty.clone()).collect();

    let scope: Rc<RefCell<dyn Scope>> = Rc::new(RefCell::new(FunctionScope::new(
        return_type.clone(),
        HashMap::from_iter(locals),
        index,
    )));

    let body = pass1_expr(pass1, &scope, body)?;

    if body.ty != return_type {
        return Err(CompilerError::TypeMismatchUnary(
            return_type,
            body.ty.clone(),
        ));
    }

    let signature = FunctionSignature {
        return_type,
        arg_types,
    };
    Ok(LangFunction {
        name,
        signature,
        body,
        scope,
    })
}

pub fn pass1_global_definition(
    pass1: &Pass1Program,
    is_const: bool,
    ty: &Rc<Node>,
    initializer: Rc<Node>,
) -> Result<GlobalValue, CompilerError> {
    let ty = pass1_type(pass1, ty)?;

    // TODO check that it's a const value

    Ok(GlobalValue { ty, is_const, initializer })
}

pub fn pass1_program(
    pass0: Pass0Program,
    items: &[Rc<Node>],
) -> Result<Pass1Program, CompilerError> {
    let mut pass1 = Pass1Program {
        functions: vec![],
        globals: HashMap::new(),
        pass0,
    };

    for item in items {
        match item.as_ref() {
            Node::Function(Function {
                name,
                args,
                ret_type,
                body,
            }) => {
                let index = pass1.functions.len();
                pass1.functions.push(pass1_function(
                    &pass1,
                    index,
                    name.clone(),
                    args,
                    ret_type,
                    body,
                )?);
            }
            Node::GlobalDefinition(GlobalDefinition {
                is_const, name, ty, value
            }) => {
                pass1.globals.insert(
                    name.clone(),
                    pass1_global_definition(&pass1, *is_const, ty, value.clone())?,
                );
            }
            _ => todo!(),
        }
    }

    Ok(pass1)
}
