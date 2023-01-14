use std::{collections::HashMap, rc::Rc};

use crate::parser::{Function, GlobalDefinition, Node};

use super::{CompilerError, FunctionSignature, GlobalValue, Pass0Program, Type};

#[derive(Debug)]
pub struct Pass1Program {
    pub(super) functions: HashMap<String, FunctionSignature>,
    pub(super) globals: HashMap<String, GlobalValue>,
    // Pass 0 info
    pass0: Pass0Program,
}

fn pass1_type(pass0: &Pass0Program, ty: &Rc<Node>) -> Result<Rc<Type>, CompilerError> {
    let Node::Type(name) = ty.as_ref() else {
        return Err(CompilerError::UnhandledNode);
    };

    pass0
        .named_type(name)
        .ok_or(CompilerError::UndefinedType(name.to_string()))
}

fn pass1_function(
    pass0: &Pass0Program,
    args: &[(Rc<Node>, Rc<Node>)],
    ret_type: &Option<Rc<Node>>,
) -> Result<FunctionSignature, CompilerError> {
    let return_type = if let Some(ty) = ret_type {
        pass1_type(pass0, ty)?
    } else {
        pass0.void_type()
    };
    let arg_types = args
        .iter()
        .map(|(_, ty)| pass1_type(pass0, ty))
        .collect::<Result<Vec<_>, _>>()?;

    Ok(FunctionSignature {
        return_type,
        arg_types,
    })
}

fn pass1_global_definition(
    pass0: &Pass0Program,
    is_const: bool,
    ty: &Rc<Node>,
) -> Result<GlobalValue, CompilerError> {
    let ty = pass1_type(pass0, ty)?;

    Ok(GlobalValue { ty, is_const })
}

pub fn pass1_program(
    pass0: Pass0Program,
    items: &[Rc<Node>],
) -> Result<Pass1Program, CompilerError> {
    let mut functions = HashMap::new();
    let mut globals = HashMap::new();

    for item in items {
        match item.as_ref() {
            Node::Function(Function {
                name,
                args,
                ret_type,
                ..
            }) => {
                functions.insert(name.clone(), pass1_function(&pass0, args, ret_type)?);
            }
            Node::GlobalDefinition(GlobalDefinition {
                is_const, name, ty, ..
            }) => {
                globals.insert(
                    name.clone(),
                    pass1_global_definition(&pass0, *is_const, ty)?,
                );
            }
            _ => todo!(),
        }
    }

    Ok(Pass1Program {
        functions,
        globals,
        pass0,
    })
}
