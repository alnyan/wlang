#![feature(let_chains)]

use std::{cell::RefCell, rc::Rc};

#[macro_use]
extern crate derivative;

pub mod emit;
pub mod pass0;
pub mod pass1;
pub mod types;

pub use types::LangType;

use ast::{Node, Token};
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

#[derive(Debug, Clone)]
pub enum TaggedExprValue {
    Binary {
        op: Token,
        lhs: Rc<TaggedExpr>,
        rhs: Rc<TaggedExpr>,
    },
    Block(Vec<Rc<TaggedExpr>>),
    Statement(Rc<TaggedExpr>),
    LocalDefinition {
        ty: Rc<LangType>,
        name: String,
        value: Rc<TaggedExpr>,
    },
    Condition {
        condition: Rc<TaggedExpr>,
        if_true: Rc<TaggedExpr>,
        if_false: Option<Rc<TaggedExpr>>,
    },
    Loop {
        condition: Option<Rc<TaggedExpr>>,
        body: Rc<TaggedExpr>,
    },
    BreakLoop,
    Return(Option<Rc<TaggedExpr>>),
    IntegerLiteral(u64),
    Ident(String),
    Call(Rc<TaggedExpr>, Vec<Rc<TaggedExpr>>),
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct TaggedExpr {
    pub ty: Rc<LangType>,
    pub fn_index: usize,
    pub scope_index: Option<usize>,
    #[derivative(Debug = "ignore")]
    pub ast_node: Rc<Node>,
    pub value: TaggedExprValue,
}
