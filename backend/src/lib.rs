#![feature(let_chains)]

use std::{cell::RefCell, rc::Rc};

#[macro_use]
extern crate derivative;

pub mod emit;
pub mod pass0;
pub mod pass1;
pub mod tagged;
pub mod types;

pub use tagged::{TaggedExpr, TaggedExprValue};
pub use types::LangType;

use ast::Node;
pub use emit::compile_module;
pub use pass0::{pass0_program, Pass0Program};
pub use pass1::{pass1_program, Pass1Program};

use self::pass1::Scope;

#[derive(Debug, Clone)]
pub enum CompilerError {
    TypeMismatchUnary(Rc<LangType>, Rc<LangType>),
    InvalidOperation(Rc<Node>, String),
    UnhandledNode(Rc<Node>),
    UndefinedType(String),
}

#[derive(Debug)]
pub enum CompilerOperation {
    EmitIntermediateBitcode,
    EmitIntermediateSourceCode,
}

#[derive(Debug)]
pub struct FunctionSignature {
    pub return_type: Rc<LangType>,
    pub arg_types: Vec<(String, Rc<LangType>)>,
}

#[derive(Debug)]
pub struct FunctionImplementation {
    pub body: Rc<TaggedExpr>,
    pub scope: Rc<RefCell<dyn Scope>>,
}

#[derive(Debug)]
pub struct LangFunction {
    pub name: String,
    pub index: usize,
    pub body_node: Option<Rc<Node>>,
    pub signature: FunctionSignature,
    pub implementation: Option<FunctionImplementation>,
}

#[derive(Debug)]
pub struct GlobalValue {
    pub ty: Rc<LangType>,
    pub is_const: bool,
    pub initializer: Rc<Node>,
}

#[derive(Debug, Clone)]
pub struct LocalValue {
    pub ty: Rc<LangType>,
    pub is_mutable: bool,
    pub scope_index: Option<usize>,
    pub fn_index: usize,
}
