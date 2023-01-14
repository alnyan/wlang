//! Pass 1: Extract function signatures + global value types
//!         + typecheck
use std::{
    cell::RefCell,
    collections::HashMap,
    rc::{Rc, Weak},
};

use crate::{
    lexer::token::{BasicOperator, Token},
    parser::{Function, GlobalDefinition, LocalDefinition, Node},
};

use super::{CompilerError, FunctionSignature, GlobalValue, LocalValue, Pass0Program, LangType};

#[derive(Debug)]
pub struct Pass1Program {
    pub(super) functions: HashMap<String, FunctionSignature>,
    pub(super) globals: HashMap<String, GlobalValue>,
    // Pass 0 info
    pass0: Pass0Program,
}

struct FunctionScope {
    return_type: Rc<LangType>,
    args: HashMap<String, LocalValue>,
}

struct BlockScope {
    parent: Rc<RefCell<dyn Scope>>,
    locals: HashMap<String, LocalValue>,
}

trait Scope {
    fn local(&self, name: &str) -> Option<LocalValue>;
    fn add_local(&mut self, name: &str, ty: LocalValue);
    fn function_return_type(&self) -> Rc<LangType>;
    fn is_upper(&self) -> bool;
}

impl Scope for BlockScope {
    fn local(&self, name: &str) -> Option<LocalValue> {
        if let Some(ty) = self.locals.get(name) {
            Some(ty.clone())
        } else {
            self.parent.borrow().local(name)
        }
    }

    fn add_local(&mut self, name: &str, ty: LocalValue) {
        self.locals.insert(name.to_string(), ty);
    }

    fn function_return_type(&self) -> Rc<LangType> {
        self.parent.borrow().function_return_type()
    }

    fn is_upper(&self) -> bool {
        false
    }
}

impl Scope for FunctionScope {
    fn local(&self, name: &str) -> Option<LocalValue> {
        self.args.get(name).cloned()
    }

    fn add_local(&mut self, name: &str, ty: LocalValue) {
        panic!()
    }

    fn function_return_type(&self) -> Rc<LangType> {
        todo!()
    }

    fn is_upper(&self) -> bool {
        true
    }
}

fn pass1_type(pass1: &Pass1Program, ty: &Rc<Node>) -> Result<Rc<LangType>, CompilerError> {
    let Node::Type(name) = ty.as_ref() else {
        return Err(CompilerError::UnhandledNode(ty.clone()));
    };

    pass1
        .pass0
        .named_type(name)
        .ok_or(CompilerError::UndefinedType(name.to_string()))
}

fn pass1_local_definition(
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

fn pass1_binary(
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

fn pass1_expr(
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
