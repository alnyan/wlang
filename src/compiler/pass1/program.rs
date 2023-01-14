use std::{rc::Rc, cell::RefCell, collections::HashMap};

use crate::{compiler::{LangFunction, CompilerError, LocalValue, FunctionSignature, GlobalValue, Pass0Program}, parser::{Node, Function, GlobalDefinition}};

use super::{expr::{pass1_type, pass1_expr}, Pass1Program, FunctionScope, Scope};


pub fn pass1_function(
    pass1: &Pass1Program,
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
            },
        ));
    }

    let arg_types = locals.iter().map(|(_, t)| t.ty.clone()).collect();

    let scope: Rc<RefCell<dyn Scope>> = Rc::new(RefCell::new(FunctionScope {
        args: HashMap::from_iter(locals),
        return_type: return_type.clone(),
    }));

    let return_value_type = pass1_expr(pass1, &scope, body)?;

    if return_value_type != return_type {
        return Err(CompilerError::TypeMismatchUnary(
            return_type,
            return_value_type,
        ));
    }

    let signature = FunctionSignature {
        return_type,
        arg_types,
    };
    Ok(LangFunction {
        signature
    })
}

pub fn pass1_global_definition(
    pass1: &Pass1Program,
    is_const: bool,
    ty: &Rc<Node>,
) -> Result<GlobalValue, CompilerError> {
    let ty = pass1_type(pass1, ty)?;

    Ok(GlobalValue { ty, is_const })
}

pub fn pass1_program(
    pass0: Pass0Program,
    items: &[Rc<Node>],
) -> Result<Pass1Program, CompilerError> {
    let mut pass1 = Pass1Program {
        functions: HashMap::new(),
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
                pass1
                    .functions
                    .insert(name.clone(), pass1_function(&pass1, args, ret_type, body)?);
            }
            Node::GlobalDefinition(GlobalDefinition {
                is_const, name, ty, ..
            }) => {
                pass1.globals.insert(
                    name.clone(),
                    pass1_global_definition(&pass1, *is_const, ty)?,
                );
            }
            _ => todo!(),
        }
    }

    Ok(pass1)
}
