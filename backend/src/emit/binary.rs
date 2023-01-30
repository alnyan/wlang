use std::rc::Rc;

use ast::{
    token::{BasicOperator, TokenValue},
    Token,
};
use inkwell::{
    basic_block::BasicBlock,
    values::{AnyValueEnum, IntValue},
    AddressSpace, IntPredicate,
};

use crate::{CompilerError, LangType, TaggedExpr};

use super::{Codegen, LlvmFunctionScope};

trait AsIntComparisonPredicate {
    fn as_int_comparison_predicate(&self, signed: bool) -> IntPredicate;
}

impl AsIntComparisonPredicate for BasicOperator {
    fn as_int_comparison_predicate(&self, signed: bool) -> IntPredicate {
        match self {
            Self::Eq => IntPredicate::EQ,
            Self::Ne => IntPredicate::NE,
            Self::Lt if signed => IntPredicate::SLT,
            Self::Lt if !signed => IntPredicate::ULT,
            Self::Gt if signed => IntPredicate::SGT,
            Self::Gt if !signed => IntPredicate::UGT,
            Self::Le if signed => IntPredicate::SLE,
            Self::Le if !signed => IntPredicate::ULE,
            Self::Ge if signed => IntPredicate::SGE,
            Self::Ge if !signed => IntPredicate::UGE,
            _ => todo!(),
        }
    }
}

impl<'a> Codegen<'a> {
    fn compile_int_arithmetic<'b>(
        &'b self,
        signed: bool,
        op: BasicOperator,
        llvm_lhs: IntValue<'b>,
        llvm_rhs: IntValue<'b>,
    ) -> Result<IntValue<'b>, CompilerError> {
        Ok(match op {
            BasicOperator::Add => self.builder.build_int_add(llvm_lhs, llvm_rhs, "add"),
            BasicOperator::Sub => self.builder.build_int_sub(llvm_lhs, llvm_rhs, "sub"),
            BasicOperator::Mul => self.builder.build_int_mul(llvm_lhs, llvm_rhs, "mul"),
            BasicOperator::Div => {
                if signed {
                    self.builder.build_int_signed_div(llvm_lhs, llvm_rhs, "")
                } else {
                    self.builder.build_int_unsigned_div(llvm_lhs, llvm_rhs, "")
                }
            }
            BasicOperator::Mod => {
                if signed {
                    self.builder.build_int_signed_rem(llvm_lhs, llvm_rhs, "")
                } else {
                    self.builder.build_int_unsigned_rem(llvm_lhs, llvm_rhs, "")
                }
            }
            BasicOperator::BitAnd => self.builder.build_and(llvm_lhs, llvm_rhs, "band"),
            BasicOperator::BitOr => self.builder.build_or(llvm_lhs, llvm_rhs, "bor"),
            BasicOperator::Shl => self.builder.build_left_shift(llvm_lhs, llvm_rhs, "shl"),
            BasicOperator::Shr => self
                .builder
                .build_right_shift(llvm_lhs, llvm_rhs, false, "shr"),
            _ => todo!(),
        })
    }

    pub fn compile_binary(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        ty: &Rc<LangType>,
        op: &Token,
        lhs: &Rc<TaggedExpr>,
        rhs: &Rc<TaggedExpr>,
    ) -> Result<Option<AnyValueEnum>, CompilerError> {
        let llvm_lhs = self.compile_expr(llvm_func, loop_exit, lhs)?.unwrap();
        let llvm_rhs = self.compile_expr(llvm_func, loop_exit, rhs)?.unwrap();

        let value = match ty.as_ref() {
            LangType::IntType(ty) => {
                let llvm_lhs = llvm_lhs.into_int_value();
                let llvm_rhs = llvm_rhs.into_int_value();
                let (_, signed) = ty.as_llvm_int_type(self.module.get_context(), &self.target_data);

                if let TokenValue::BasicOperator(op) = op.value {
                    self.compile_int_arithmetic(signed, op, llvm_lhs, llvm_rhs)?.into()
                } else {
                    todo!()
                }
            }
            LangType::Pointer(_) => {
                if TokenValue::BasicOperator(BasicOperator::Add) == op.value {
                    // TODO better pointer arithmetic through GEP
                    let llvm_lhs = llvm_lhs.into_pointer_value();
                    let llvm_rhs = llvm_rhs.into_int_value();

                    let value = unsafe { self.builder.build_gep(self.module.get_context().i8_type().ptr_type(AddressSpace::default()), llvm_lhs, &[llvm_rhs], "") };
                    value.into()
                } else {
                    todo!()
                }
            }
            LangType::BoolType => {
                if let TokenValue::BasicOperator(op) = op.value && op.is_comparison() {
                    assert!(lhs.ty.is_compatible(&rhs.ty));
                    match lhs.ty.as_ref() {
                        LangType::IntType(ty) => {
                            let llvm_lhs = llvm_lhs.into_int_value();
                            let llvm_rhs = llvm_rhs.into_int_value();
                            let (_, signed) = ty.as_llvm_int_type(self.module.get_context(), &self.target_data);
                            self.builder.build_int_compare(op.as_int_comparison_predicate(signed), llvm_lhs, llvm_rhs, "cmp").into()
                        }
                        _ => todo!()
                    }
                } else if let TokenValue::BasicOperator(op) = op.value && op.is_logic() {
                    let llvm_lhs = llvm_lhs.into_int_value();
                    let llvm_rhs = llvm_rhs.into_int_value();
                    assert!(lhs.ty.is_compatible(&rhs.ty));
                    assert!(lhs.ty.is_compatible(&self.pass1.pass0.bool_type()));

                    match op {
                        BasicOperator::And => self.builder.build_and(llvm_lhs, llvm_rhs, "and").into(),
                        BasicOperator::Or => self.builder.build_or(llvm_lhs, llvm_rhs, "or").into(),
                        _ => todo!()
                    }
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        };

        Ok(Some(value))
    }
}
