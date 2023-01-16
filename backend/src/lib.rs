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
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType, IntType, BasicType},
    values::IntValue,
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
pub enum LangIntType {
    U64,
    I64,
    U32,
    I32,
    U16,
    I16,
    U8,
    I8,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LangType {
    Void,
    IntType(LangIntType),
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

#[allow(clippy::match_like_matches_macro)]
impl LangType {
    pub const fn is_integer(&self) -> bool {
        matches!(self, Self::IntType(_))
    }

    pub fn as_basic_metadata_type<'a>(&self, context: ContextRef<'a>) -> BasicMetadataTypeEnum<'a> {
        match self {
            Self::IntType(it) => it.as_llvm_basic_metadata_type(context),
            _ => todo!(),
        }
    }

    pub fn make_llvm_function_type<'a>(
        &'a self,
        context: ContextRef<'a>,
        arg_types: &[BasicMetadataTypeEnum<'a>],
    ) -> FunctionType<'a> {
        match self {
            Self::IntType(it) => it.as_llvm_basic_type(context).fn_type(arg_types, false),
            Self::Void => context.void_type().fn_type(arg_types, false),
        }
    }

    pub fn as_llvm_basic_type<'a>(&self, context: ContextRef<'a>) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::IntType(it) => Some(it.as_llvm_basic_type(context)),
            Self::Void => None
        }
    }
}

impl LangIntType {
    pub fn as_llvm_basic_type<'a>(&self, context: ContextRef<'a>) -> BasicTypeEnum<'a> {
        match self {
            Self::U64 | Self::I64 => context.i64_type().into(),
            Self::U32 | Self::I32 => context.i32_type().into(),
            Self::U16 | Self::I16 => context.i16_type().into(),
            Self::U8 | Self::I8 => context.i8_type().into(),
        }
    }

    pub fn as_llvm_basic_metadata_type<'a>(
        &self,
        context: ContextRef<'a>,
    ) -> BasicMetadataTypeEnum<'a> {
        match self {
            Self::U64 | Self::I64 => context.i64_type().into(),
            Self::U32 | Self::I32 => context.i32_type().into(),
            Self::U16 | Self::I16 => context.i16_type().into(),
            Self::U8 | Self::I8 => context.i8_type().into(),
        }
    }

    pub fn as_llvm_int_type<'a>(&self, context: ContextRef<'a>) -> (IntType<'a>, bool) {
        match self {
            Self::U64 => (context.i64_type(), false),
            Self::I64 => (context.i64_type(), true),
            Self::U32 => (context.i32_type(), false),
            Self::I32 => (context.i32_type(), true),
            Self::U16 => (context.i16_type(), false),
            Self::I16 => (context.i16_type(), true),
            Self::U8 => (context.i8_type(), false),
            Self::I8 => (context.i8_type(), true),
        }
    }
}
