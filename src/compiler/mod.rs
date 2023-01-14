use std::rc::Rc;

pub mod pass0;
pub mod pass1;

pub use pass0::{pass0_program, Pass0Program};
pub use pass1::{pass1_program, Pass1Program};

#[derive(Debug, Clone)]
pub enum CompilerError {
    UnhandledNode,
    UndefinedType(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
}

#[derive(Debug)]
pub struct FunctionSignature {
    return_type: Rc<Type>,
    arg_types: Vec<Rc<Type>>,
}

pub trait LocalScope {
    fn local(&self, name: &str) -> Option<Rc<Type>>;
    fn add_local(&mut self, name: &str, ty: Rc<Type>);
}
