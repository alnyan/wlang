//! Pass 1: Extract function signatures + global value types
//!         + typecheck
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::parser::{Function, GlobalDefinition, Node};

use super::{CompilerError, FunctionSignature, GlobalValue, Pass0Program, Type};

#[derive(Debug)]
pub struct Pass1Program {
    pub(super) functions: HashMap<String, FunctionSignature>,
    pub(super) globals: HashMap<String, GlobalValue>,
    // Pass 0 info
    pass0: Pass0Program,
}

struct FunctionScope {
    return_type: Rc<Type>,
    args: HashMap<String, Rc<Type>>,
}

struct BlockScope {
    parent: Rc<RefCell<dyn Scope>>,
    locals: HashMap<String, Rc<Type>>,
}

trait Scope {
    fn local(&self, name: &str) -> Option<Rc<Type>>;
    fn add_local(&mut self, name: &str, ty: Rc<Type>);
    fn function_return_type(&self) -> Rc<Type>;
    fn is_upper(&self) -> bool;
}

impl Scope for BlockScope {
    fn local(&self, name: &str) -> Option<Rc<Type>> {
        todo!()
    }

    fn add_local(&mut self, name: &str, ty: Rc<Type>) {
        todo!()
    }

    fn function_return_type(&self) -> Rc<Type> {
        self.parent.borrow().function_return_type()
    }

    fn is_upper(&self) -> bool {
        false
    }
}

impl Scope for FunctionScope {
    fn local(&self, name: &str) -> Option<Rc<Type>> {
        if let Some(ty) = self.args.get(name) {
            Some(ty.clone())
        } else {
            None
        }
    }

    fn add_local(&mut self, name: &str, ty: Rc<Type>) {
        todo!()
    }

    fn function_return_type(&self) -> Rc<Type> {
        todo!()
    }

    fn is_upper(&self) -> bool {
        true
    }
}

fn pass1_type(pass1: &Pass1Program, ty: &Rc<Node>) -> Result<Rc<Type>, CompilerError> {
    let Node::Type(name) = ty.as_ref() else {
        return Err(CompilerError::UnhandledNode);
    };

    pass1.pass0
        .named_type(name)
        .ok_or(CompilerError::UndefinedType(name.to_string()))
}

fn pass1_expr(
    pass1: &Pass1Program,
    scope: &Rc<RefCell<dyn Scope>>,
    expr: &Rc<Node>,
) -> Result<Rc<Type>, CompilerError> {
    match expr.as_ref() {
        Node::Ident(name) => {
            if let Some(local) = scope.borrow().local(name.as_str()) {
                return Ok(local);
            }

            if let Some(global) = pass1.globals.get(name.as_str()) {
                return Ok(global.ty.clone());
            }

            todo!()
        },
        Node::Block(items) => {
            // TODO create subscope
            for (i, item) in items.iter().enumerate() {
                if i != items.len() - 1 && !matches!(item.as_ref(), Node::Statement(_)) {
                    return Err(CompilerError::UnhandledNode);
                }

                let ty = pass1_expr(pass1, scope, item)?;

                if i != items.len() - 1 {
                    assert_eq!(ty, pass1.pass0.void_type());
                } else {
                    return Ok(ty);
                }
            }
            // Empty block
            Ok(pass1.pass0.void_type())
        },
        _ => todo!()
    }
}

fn pass1_function(
    pass1: &Pass1Program,
    args: &[(Rc<Node>, Rc<Node>)],
    ret_type: &Option<Rc<Node>>,
    body: &Rc<Node>,
) -> Result<FunctionSignature, CompilerError> {
    let return_type = if let Some(ty) = ret_type {
        pass1_type(pass1, ty)?
    } else {
        pass1.pass0.void_type()
    };

    let mut locals = vec![];
    for (name, ty) in args {
        let Node::Ident(name) = name.as_ref() else {
            return Err(CompilerError::UnhandledNode);
        };
        let ty = pass1_type(pass1, ty)?;

        locals.push((name.clone(), ty));
    }

    let arg_types = locals.iter().map(|(_, t)| t.clone()).collect();

    let scope: Rc<RefCell<dyn Scope>> = Rc::new(RefCell::new(FunctionScope {
        args: HashMap::from_iter(locals),
        return_type: return_type.clone()
    }));

    let return_value_type = pass1_expr(pass1, &scope, body)?;

    if return_value_type != return_type {
        return Err(CompilerError::UnhandledNode);
    }

    Ok(FunctionSignature {
        return_type,
        arg_types,
    })
}

fn pass1_global_definition(
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
        pass0
    };

    for item in items {
        match item.as_ref() {
            Node::Function(Function {
                name,
                args,
                ret_type,
                body,
            }) => {
                pass1.functions.insert(name.clone(), pass1_function(&pass1, args, ret_type, body)?);
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
