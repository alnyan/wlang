use std::rc::Rc;

pub mod pass0;
pub mod pass1;
pub mod emit;

use inkwell::{types::Type, context::Context};
pub use pass0::{pass0_program, Pass0Program};
pub use pass1::{pass1_program, Pass1Program};
pub use emit::compile_module;

use crate::{parser::Node, lexer::token::Token};

#[derive(Debug, Clone)]
pub enum CompilerError {
    TypeMismatchUnary(Rc<LangType>, Rc<LangType>),
    InvalidOperation(Rc<Node>, String),
    UnhandledNode(Rc<Node>),
    UndefinedType(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum LangType {
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
    pub return_type: Rc<LangType>,
    pub arg_types: Vec<Rc<LangType>>,
}

#[derive(Debug)]
pub struct LangFunction {
    pub signature: FunctionSignature,
    // pub body: Rc<TaggedNode>
}

#[derive(Debug)]
pub struct GlobalValue {
    pub ty: Rc<LangType>,
    pub is_const: bool
}

#[derive(Debug, Clone)]
pub struct LocalValue {
    pub ty: Rc<LangType>,
    pub is_mutable: bool
}

#[derive(Debug, Clone)]
pub enum TaggedNodeValue {
    Binary {
        op: Token,
        lhs: Rc<TaggedNode>,
        rhs: Rc<TaggedNode>
    },
    Ident(String)
}

#[derive(Debug, Clone)]
pub struct TaggedNode {
    pub ty: Rc<LangType>,
    pub ast_node: Rc<Node>,
    pub value: TaggedNodeValue
}

impl LangType {
    pub const fn is_integer(&self) -> bool {
        match self {
            Self::Void => false,
            _ => true,
        }
    }

    pub fn to_llvm_type(&self, context: &Context) -> Option<Type> {
        match self {
            Self::Void => Some(context.void_type()),
            Self::U64 => Some(context.i64_type()),
            Self::I64 => Some(context.i64_type()),
            _ => todo!()
        }
    }
}
