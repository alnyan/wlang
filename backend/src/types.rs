use std::rc::Rc;

use inkwell::{
    context::ContextRef,
    targets::TargetData,
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType},
};

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(usize)]
pub enum LangIntType {
    USIZE = 0,
    ISIZE,
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
    SizedArrayType(Rc<LangType>, usize),
    Pointer(Rc<LangType>),
}

#[allow(clippy::match_like_matches_macro)]
impl LangType {
    pub const fn is_integer(&self) -> bool {
        matches!(self, Self::IntType(_))
    }

    pub const fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }

    pub fn as_basic_metadata_type<'a>(
        &self,
        context: ContextRef<'a>,
        target_data: &TargetData,
    ) -> BasicMetadataTypeEnum<'a> {
        match self {
            Self::IntType(it) => it.as_llvm_basic_metadata_type(context, target_data),
            Self::Pointer(t) => match t.as_basic_metadata_type(context, target_data) {
                BasicMetadataTypeEnum::IntType(t) => {
                    BasicMetadataTypeEnum::PointerType(t.ptr_type(Default::default()))
                }
                BasicMetadataTypeEnum::PointerType(t) => {
                    BasicMetadataTypeEnum::PointerType(t.ptr_type(Default::default()))
                }
                _ => todo!(),
            },
            _ => todo!(),
        }
    }

    pub fn make_sized_array_type(self: Rc<Self>, size: usize) -> Rc<LangType> {
        if !self.is_integer() {
            todo!(); // Nested arrays n stuff?
        }
        Rc::new(Self::SizedArrayType(self, size))
    }

    pub fn make_pointer_type(self: Rc<Self>) -> Rc<LangType> {
        Rc::new(Self::Pointer(self))
    }

    pub fn make_llvm_function_type<'a>(
        &'a self,
        context: ContextRef<'a>,
        target_data: &TargetData,
        arg_types: &[BasicMetadataTypeEnum<'a>],
    ) -> FunctionType<'a> {
        if matches!(self, Self::Void) {
            context.void_type().fn_type(arg_types, false)
        } else {
            self.as_llvm_basic_type(context, target_data)
                .unwrap()
                .fn_type(arg_types, false)
        }
    }

    pub fn as_llvm_basic_type<'a>(
        &self,
        context: ContextRef<'a>,
        target_data: &TargetData,
    ) -> Option<BasicTypeEnum<'a>> {
        match self {
            Self::IntType(it) => Some(it.as_llvm_basic_type(context, target_data)),
            Self::BoolType => Some(context.bool_type().into()),
            Self::Void => None,
            Self::SizedArrayType(_, _) => todo!("Array"),
            Self::Pointer(inner) => match inner.as_llvm_basic_type(context, target_data) {
                Some(BasicTypeEnum::IntType(t)) => {
                    Some(t.ptr_type(Default::default()).as_basic_type_enum())
                }
                Some(BasicTypeEnum::PointerType(t)) => {
                    Some(t.ptr_type(Default::default()).as_basic_type_enum())
                }
                None => None,
                _ => todo!(),
            },
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
            Self::SizedArrayType(elem_ty, size) => {
                if let Self::SizedArrayType(b_elem_ty, b_size) = other {
                    size == b_size && elem_ty.is_compatible(b_elem_ty)
                } else {
                    false
                }
            }
            Self::Pointer(a) => {
                if let Self::Pointer(b) = other {
                    a.is_compatible(b)
                } else {
                    false
                }
            }
        }
    }
}

impl LangIntType {
    pub fn is_signed(self) -> bool {
        self as usize % 2 != 0
    }

    pub fn bit_width(self, target_data: &TargetData) -> usize {
        match self {
            Self::U64 | Self::I64 => 64,
            Self::U32 | Self::I32 => 32,
            Self::U16 | Self::I16 => 16,
            Self::U8 | Self::I8 => 8,
            Self::USIZE | Self::ISIZE => target_data.get_pointer_byte_size(None) as usize * 8,
        }
    }

    pub fn as_llvm_basic_type<'a>(
        &self,
        context: ContextRef<'a>,
        target_data: &TargetData,
    ) -> BasicTypeEnum<'a> {
        match self {
            Self::U64 | Self::I64 => context.i64_type(),
            Self::U32 | Self::I32 => context.i32_type(),
            Self::U16 | Self::I16 => context.i16_type(),
            Self::U8 | Self::I8 => context.i8_type(),
            Self::USIZE | Self::ISIZE => context.ptr_sized_int_type(target_data, None),
        }
        .into()
    }

    pub fn as_llvm_basic_metadata_type<'a>(
        &self,
        context: ContextRef<'a>,
        target_data: &TargetData,
    ) -> BasicMetadataTypeEnum<'a> {
        match self {
            Self::U64 | Self::I64 => context.i64_type(),
            Self::U32 | Self::I32 => context.i32_type(),
            Self::U16 | Self::I16 => context.i16_type(),
            Self::U8 | Self::I8 => context.i8_type(),
            Self::USIZE | Self::ISIZE => context.ptr_sized_int_type(target_data, None),
        }
        .into()
    }

    pub fn as_llvm_int_type<'a>(
        &self,
        context: ContextRef<'a>,
        target_data: &TargetData,
    ) -> (IntType<'a>, bool) {
        match self {
            Self::U64 => (context.i64_type(), false),
            Self::I64 => (context.i64_type(), true),
            Self::U32 => (context.i32_type(), false),
            Self::I32 => (context.i32_type(), true),
            Self::U16 => (context.i16_type(), false),
            Self::I16 => (context.i16_type(), true),
            Self::U8 => (context.i8_type(), false),
            Self::I8 => (context.i8_type(), true),
            Self::USIZE => (context.ptr_sized_int_type(target_data, None), false),
            Self::ISIZE => (context.ptr_sized_int_type(target_data, None), true),
        }
    }
}
