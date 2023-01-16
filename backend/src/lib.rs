use std::{cell::RefCell, rc::Rc};

#[macro_use]
extern crate derivative;

pub mod emit;
pub mod pass0;
pub mod pass1;

use ast::{Node, Token};
pub use emit::compile_module;
use inkwell::{
    context::ContextRef,
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, IntType},
};
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
    IntegerLiteral(u64),
    Ident(String),
    Call(Rc<TaggedExpr>, Vec<Rc<TaggedExpr>>)
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

#[allow(clippy::match_like_matches_macro)]
impl LangType {
    pub const fn is_integer(&self) -> bool {
        match self {
            Self::Void => false,
            _ => true,
        }
    }

    pub fn as_llvm_int_type<'a>(&'a self, context: ContextRef<'a>) -> Option<IntType> {
        match self {
            Self::U64 | Self::I64 => Some(context.i64_type()),
            _ => todo!(),
        }
    }

    pub fn as_basic_metadata_type<'a>(
        &self,
        context: ContextRef<'a>,
    ) -> BasicMetadataTypeEnum<'a> {
        match self {
            Self::U64 | Self::I64 => BasicMetadataTypeEnum::IntType(context.i64_type()),
            _ => todo!()
        }
    }

    pub fn make_llvm_function_type<'a>(
        &'a self,
        context: ContextRef<'a>,
        arg_types: &[BasicMetadataTypeEnum<'a>],
    ) -> FunctionType<'a> {
        match self {
            Self::U64 => self
                .as_llvm_int_type(context)
                .unwrap()
                .fn_type(arg_types, false),
            Self::Void => context.void_type().fn_type(arg_types, false),
            _ => todo!(),
        }
    }

    pub fn as_llvm_basic_type<'a>(&self, context: ContextRef<'a>) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::U64 => Some(BasicTypeEnum::IntType(context.i64_type())),
            _ => todo!(),
        }
    }

    pub fn as_llvm_any_type<'a>(&self, context: ContextRef<'a>) -> Option<AnyTypeEnum<'a>> {
        match self {
            Self::U64 => Some(AnyTypeEnum::IntType(context.i64_type())),
            _ => todo!(),
        }
    }
}
