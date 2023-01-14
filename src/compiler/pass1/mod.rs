//! Pass 1: Extract function signatures + global value types
//!         + typecheck
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub mod expr;
pub mod program;

pub use program::pass1_program;

use super::{GlobalValue, LangFunction, LangType, LocalValue, Pass0Program};

#[derive(Debug)]
pub struct Pass1Program {
    pub(super) functions: HashMap<String, LangFunction>,
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

pub trait Scope {
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
