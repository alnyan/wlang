use std::{collections::HashMap, rc::Rc};

use crate::parser::{Function, Node};

use super::{CompilerError, FunctionSignature, Pass0Program, Type};

#[derive(Debug)]
pub struct Pass1Program {
    pub(super) functions: HashMap<String, FunctionSignature>,
    // Pass 0 info
    pass0: Pass0Program
}

fn pass1_type(pass0: &Pass0Program, ty: &Rc<Node>) -> Result<Rc<Type>, CompilerError> {
    let Node::Type(name) = ty.as_ref() else {
        return Err(CompilerError::UnhandledNode);
    };

    pass0.named_type(name).ok_or(CompilerError::UndefinedType(name.to_string()))
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

pub fn pass1_program(
    pass0: Pass0Program,
    items: &[Rc<Node>],
) -> Result<Pass1Program, CompilerError> {
    let mut functions = HashMap::new();

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
            _ => todo!(),
        }
    }

    Ok(Pass1Program {
        functions,
        pass0
    })
}
