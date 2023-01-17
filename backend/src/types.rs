use inkwell::{
    context::ContextRef,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType},
};

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

#[derive(Debug, Clone)]
pub enum LangType {
    Void,
    BoolType,
    IntType(LangIntType),
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
            Self::BoolType => context.bool_type().fn_type(arg_types, false),
            Self::Void => context.void_type().fn_type(arg_types, false),
        }
    }

    pub fn as_llvm_basic_type<'a>(&self, context: ContextRef<'a>) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::IntType(it) => Some(it.as_llvm_basic_type(context)),
            Self::BoolType => Some(context.bool_type().into()),
            Self::Void => None,
        }
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        match self {
            Self::Void => matches!(other, Self::Void),
            Self::BoolType => matches!(other, Self::BoolType),
            Self::IntType(a) => {
                if let Self::IntType(b) = other {
                    a == b
                } else {
                    false
                }
            }
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
