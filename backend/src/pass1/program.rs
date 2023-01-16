use std::{cell::RefCell, collections::HashMap, rc::Rc};

use ast::Node;

use crate::{
    CompilerError, FunctionImplementation, FunctionSignature, GlobalValue, LangFunction,
    LocalValue, Pass0Program,
};

use super::{
    expr::{pass1_expr, pass1_type},
    FunctionScope, Pass1Program, Scope,
};

pub fn pass1_function_signature(
    pass1: &Pass1Program,
    args: &[(Rc<Node>, Rc<Node>)],
    ret_type: &Option<Rc<Node>>,
) -> Result<FunctionSignature, CompilerError> {
    let return_type = if let Some(ty) = ret_type {
        pass1_type(pass1, ty)?
    } else {
        pass1.pass0.void_type()
    };

    let arg_types = args
        .iter()
        .map(|(name, ty)| match name.as_ref() {
            Node::Ident(name) => pass1_type(pass1, ty).map(|ty| (name.clone(), ty)),
            _ => todo!(),
        })
        .collect::<Result<Vec<_>, _>>()?;

    Ok(FunctionSignature {
        return_type,
        arg_types,
    })
}

pub fn pass1_function_impl(
    pass1: &Pass1Program,
    index: usize,
    sig: &FunctionSignature,
    body: &Rc<Node>,
) -> Result<FunctionImplementation, CompilerError> {
    let mut locals = vec![];
    for (name, ty) in sig.arg_types.iter() {
        locals.push((
            name.clone(),
            LocalValue {
                ty: ty.clone(),
                is_mutable: false,
                fn_index: index,
                scope_index: None,
            },
        ));
    }

    let scope: Rc<RefCell<dyn Scope>> = Rc::new(RefCell::new(FunctionScope::new(
        sig.return_type.clone(),
        HashMap::from_iter(locals),
        index,
    )));

    let body = pass1_expr(pass1, &scope, body)?;

    if body.ty != sig.return_type {
        return Err(CompilerError::TypeMismatchUnary(
            sig.return_type.clone(),
            body.ty.clone(),
        ));
    }

    Ok(FunctionImplementation { body, scope })
}

pub fn pass1_global_definition(
    pass1: &Pass1Program,
    is_const: bool,
    ty: &Rc<Node>,
    initializer: Rc<Node>,
) -> Result<GlobalValue, CompilerError> {
    let ty = pass1_type(pass1, ty)?;

    // TODO check that it's a const value

    Ok(GlobalValue {
        ty,
        is_const,
        initializer,
    })
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

    // Extract function signatures and globals
    for item in items {
        match item.as_ref() {
            Node::Function {
                name,
                args,
                ret_type,
                body,
            } => {
                let index = pass1.functions.len();
                let signature = pass1_function_signature(&pass1, args, ret_type)?;
                pass1.functions.push(LangFunction {
                    name: name.clone(),
                    index,
                    body_node: Some(body.clone()),
                    signature,
                    implementation: None,
                });
            }
            Node::ExternFunction {
                name,
                ret_type,
                arg_types,
            } => {
                let index = pass1.functions.len();
                let signature = pass1_function_signature(&pass1, arg_types, ret_type)?;
                pass1.functions.push(LangFunction {
                    name: name.clone(),
                    index,
                    body_node: None,
                    signature,
                    implementation: None,
                });
            }
            Node::GlobalDefinition {
                is_const,
                name,
                ty,
                value,
            } => {
                let gv = pass1_global_definition(&pass1, *is_const, ty, value.clone())?;
                pass1.globals.insert(name.clone(), gv);
            }
            _ => todo!(),
        }
    }

    // Extract function implementations
    let impls = pass1
        .functions
        .iter()
        .enumerate()
        .filter_map(|(index, f)| {
            f.body_node.as_ref().map(|body| {
                pass1_function_impl(&pass1, index, &f.signature, body).map(|r| (index, r))
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    impls.into_iter().for_each(|(index, i)| {
        pass1.functions[index].implementation = Some(i);
    });

    Ok(pass1)
}
