use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    basic_block::BasicBlock,
    types::IntType,
    values::{AnyValue, AnyValueEnum, PointerValue},
    IntPredicate,
};

use crate::{types::LangIntType, CompilerError, LangType, TaggedExpr, TaggedExprValue};

use super::{Codegen, IdentValue, LlvmFunctionScope, LlvmScope};

#[derive(Debug, Clone, Copy)]
enum CastMethod {
    IntToPtr,
    PtrToInt,
    Identity,
    IntToSmallerInt,
    IntToLargerIntZeroExtend,
    IntToLargerIntSignExtend,
}

const fn cast_method(src_ty: &LangType, dst_ty: &LangType) -> Option<CastMethod> {
    match src_ty {
        LangType::IntType(LangIntType::U64) if dst_ty.is_pointer() => Some(CastMethod::IntToPtr),
        LangType::IntType(t0) => match dst_ty {
            LangType::IntType(t1) => {
                let i0 = *t0 as usize;
                let i1 = *t1 as usize;
                assert!(i0 < 8 && i1 < 8);

                let v0 = i0 / 2;
                let v1 = i1 / 2;
                let s0 = i0 % 2;

                if v0 == v1 {
                    // Identity cast
                    Some(CastMethod::Identity)
                } else if v1 > v0 {
                    // Cast to smaller
                    Some(CastMethod::IntToSmallerInt)
                } else if v0 > v1 {
                    if s0 != 0 {
                        // Cast to larger, preserve sign
                        Some(CastMethod::IntToLargerIntSignExtend)
                    } else {
                        // Cast to larger, zero-extend
                        Some(CastMethod::IntToLargerIntZeroExtend)
                    }
                } else {
                    todo!()
                }
            }
            _ => None,
        },
        LangType::Pointer(_) => match dst_ty {
            LangType::IntType(LangIntType::U64) => Some(CastMethod::PtrToInt),
            _ => None,
        },
        _ => None,
    }
}

impl<'a> Codegen<'a> {
    fn compile_loop(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        condition: &Option<Rc<TaggedExpr>>,
        body: &Rc<TaggedExpr>,
    ) -> Result<(), CompilerError> {
        let bb_loop_entry = self
            .module
            .get_context()
            .append_basic_block(llvm_func.func, "loop_entry");
        let bb_loop_body = self
            .module
            .get_context()
            .append_basic_block(llvm_func.func, "loop_body");
        let bb_loop_exit = self
            .module
            .get_context()
            .append_basic_block(llvm_func.func, "loop_exit");

        self.builder.build_unconditional_branch(bb_loop_entry);
        self.builder.position_at_end(bb_loop_entry);

        if let Some(condition) = condition {
            let condition_value = self.compile_expr(llvm_func, loop_exit, condition)?.unwrap();

            self.builder.build_conditional_branch(
                condition_value.into_int_value(),
                bb_loop_body,
                bb_loop_exit,
            );

            self.builder.position_at_end(bb_loop_body);
        } else {
            todo!()
        }

        self.compile_expr(llvm_func, Some(bb_loop_exit), body)?;

        self.builder.build_unconditional_branch(bb_loop_entry);
        self.builder.position_at_end(bb_loop_exit);

        Ok(())
    }

    fn compile_manual_array_initializer(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        ptr: PointerValue,
        ty: IntType,
        elements: &[Rc<TaggedExpr>],
    ) -> Result<(), CompilerError> {
        assert!(!elements.is_empty());

        // If value is a const-int array, use LLVM's const_array,
        //  otherwise, use per-element assignment
        if let Ok(const_ints) = elements
            .iter()
            .map(|e| match &e.value {
                TaggedExprValue::IntegerLiteral(int) => Ok(ty.const_int(*int, false)),
                _ => Err(()),
            })
            .collect::<Result<Vec<_>, _>>()
        {
            let const_array = ty.const_array(&const_ints);

            self.builder.build_store(ptr, const_array);
        } else {
            let zero_index = self.module.get_context().i64_type().const_zero();
            for (i, elem) in elements.iter().enumerate() {
                let elem = self.compile_expr(llvm_func, loop_exit, elem)?.unwrap();
                let index = self
                    .module
                    .get_context()
                    .i64_type()
                    .const_int(i as u64, false);
                let gep = unsafe {
                    self.builder
                        .build_in_bounds_gep(ptr, &[zero_index, index], "")
                };

                self.builder.build_store(gep, elem.into_int_value());
            }
        }

        Ok(())
    }

    fn compile_repeat_array_initializer(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        ptr: PointerValue,
        ty: IntType,
        element: &Rc<TaggedExpr>,
        count: usize,
    ) -> Result<(), CompilerError> {
        if let TaggedExprValue::IntegerLiteral(value) = &element.value {
            // Use LLVM's const_array
            let values = vec![ty.const_int(*value, false); count];
            let const_array = ty.const_array(&values);

            self.builder.build_store(ptr, const_array);
        } else {
            let element = self.compile_expr(llvm_func, loop_exit, element)?.unwrap();
            let index_ty = self.module.get_context().i64_type();

            if count <= 4 {
                // Just store the values directly
                todo!()
            } else {
                // NOTE this code was derived from what I saw rustc generate
                // Make a loop to copy the elements
                let start_bb = self.builder.get_insert_block().unwrap();
                let header_bb = self
                    .module
                    .get_context()
                    .append_basic_block(llvm_func.func, ".setup_array_header");
                let body_bb = self
                    .module
                    .get_context()
                    .append_basic_block(llvm_func.func, ".setup_array");
                let exit_bb = self
                    .module
                    .get_context()
                    .append_basic_block(llvm_func.func, ".end_setup_array");

                let current_ptr = unsafe {
                    self.builder.build_in_bounds_gep(ptr, &[index_ty.const_zero(), index_ty.const_zero()], "")
                };
                let end_ptr = unsafe {
                    self.builder.build_in_bounds_gep(ptr, &[index_ty.const_zero(), index_ty.const_int(count as u64, false)], "")
                };

                self.builder.build_unconditional_branch(header_bb);

                // loop header
                self.builder.position_at_end(header_bb);

                let phi = self.builder.build_phi(current_ptr.get_type(), "");
                let phi_ptr = phi.as_basic_value().into_pointer_value();
                phi.add_incoming(&[
                    (&current_ptr, start_bb),
                ]);
                // TODO rustc manages to compare pointers directly somehow
                let phi_ptr_int = self.builder.build_ptr_to_int(phi_ptr, index_ty, "");
                let end_ptr_int = self.builder.build_ptr_to_int(end_ptr, index_ty, "");
                let cmp = self.builder.build_int_compare(IntPredicate::NE, phi_ptr_int, end_ptr_int, "");
                self.builder.build_conditional_branch(cmp, body_bb, exit_bb);

                // loop body
                self.builder.position_at_end(body_bb);

                match element {
                    AnyValueEnum::IntValue(v) => self.builder.build_store(phi_ptr, v),
                    _ => todo!()
                };
                let next_ptr = unsafe {
                    self.builder.build_in_bounds_gep(phi_ptr, &[index_ty.const_int(1, false)], "")
                };
                phi.add_incoming(&[
                    (&next_ptr, body_bb),
                ]);

                self.builder.build_unconditional_branch(header_bb);

                self.builder.position_at_end(exit_bb);
            }
        }

        Ok(())
    }

    fn compile_local_definition(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        expr: &Rc<TaggedExpr>,
        name: &str,
        ty: &Rc<LangType>,
        value: &Rc<TaggedExpr>,
    ) -> Result<(), CompilerError> {
        let llvm_scope = self.llvm_scope(expr.fn_index, expr.scope_index.unwrap());

        let ptr = match ty.as_ref() {
            LangType::IntType(ty) => {
                let (ty, _) = ty.as_llvm_int_type(self.module.get_context());
                let ptr = self.builder.build_alloca(ty, name);
                let value = self.compile_expr(llvm_func, loop_exit, value)?.unwrap();

                self.builder.build_store(ptr, value.into_int_value());
                ptr
            }
            LangType::Pointer(_) => {
                let ty = ty.as_llvm_basic_type(self.module.get_context()).unwrap();
                let ptr = self.builder.build_alloca(ty, name);

                let value = self.compile_expr(llvm_func, loop_exit, value)?.unwrap();

                self.builder.build_store(ptr, value.into_pointer_value());
                ptr
            }
            LangType::SizedArrayType(ty, size) => {
                let LangType::IntType(ty) = ty.as_ref() else {
                    todo!("Array elements can only be integer types (currently)");
                };

                let (ty, _) = ty.as_llvm_int_type(self.module.get_context());
                let ptr = self
                    .builder
                    .build_alloca(ty.array_type((*size).try_into().unwrap()), name);

                match &value.value {
                    TaggedExprValue::Array(elements) => self.compile_manual_array_initializer(
                        llvm_func, loop_exit, ptr, ty, elements,
                    )?,
                    TaggedExprValue::ArrayRepeat(element, count) => self
                        .compile_repeat_array_initializer(
                            llvm_func, loop_exit, ptr, ty, element, *count,
                        )?,
                    _ => todo!(),
                }

                ptr
            }
            _ => todo!(),
        };

        llvm_scope
            .stack_values
            .borrow_mut()
            .insert(name.to_owned(), ptr);

        Ok(())
    }

    fn compile_cast(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        value: &Rc<TaggedExpr>,
        target_ty: &Rc<LangType>,
    ) -> Result<AnyValueEnum, CompilerError> {
        let src_ty = &value.ty;
        let value = self.compile_expr(llvm_func, loop_exit, value)?.unwrap();

        let Some(method) = cast_method(src_ty, target_ty) else {
            todo!()
        };

        let result = match method {
            CastMethod::PtrToInt => self
                .builder
                .build_ptr_to_int(
                    value.into_pointer_value(),
                    self.module.get_context().i64_type(),
                    "",
                )
                .as_any_value_enum(),
            CastMethod::IntToPtr => {
                let target_ty = target_ty
                    .as_llvm_basic_type(self.module.get_context())
                    .unwrap()
                    .into_pointer_type();
                self.builder
                    .build_int_to_ptr(value.into_int_value(), target_ty, "")
                    .as_any_value_enum()
            }
            CastMethod::Identity => value.as_any_value_enum(),
            CastMethod::IntToSmallerInt => {
                let LangType::IntType(target_ty) = target_ty.as_ref() else {
                    todo!()
                };
                let (target_ty, _) = target_ty.as_llvm_int_type(self.module.get_context());
                self.builder
                    .build_int_cast(value.into_int_value(), target_ty, "")
                    .as_any_value_enum()
            }
            CastMethod::IntToLargerIntZeroExtend => {
                let LangType::IntType(target_ty) = target_ty.as_ref() else {
                    todo!()
                };
                let (target_ty, _) = target_ty.as_llvm_int_type(self.module.get_context());

                self.builder
                    .build_int_z_extend(value.into_int_value(), target_ty, "")
                    .as_any_value_enum()
            }
            CastMethod::IntToLargerIntSignExtend => {
                let LangType::IntType(target_ty) = target_ty.as_ref() else {
                    todo!()
                };
                let (target_ty, _) = target_ty.as_llvm_int_type(self.module.get_context());

                self.builder
                    .build_int_s_extend(value.into_int_value(), target_ty, "")
                    .as_any_value_enum()
            }
        };

        Ok(result)
    }

    pub fn compile_expr(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        expr: &Rc<TaggedExpr>,
    ) -> Result<Option<AnyValueEnum>, CompilerError> {
        let scope = self.scope(expr.fn_index, expr.scope_index).unwrap();
        match &expr.value {
            TaggedExprValue::Block(items) => {
                let scope = Rc::new(LlvmScope {
                    _pass1_scope: scope,
                    stack_values: RefCell::new(HashMap::new()),
                });

                self.scopes
                    .borrow_mut()
                    .insert((expr.fn_index, expr.scope_index.unwrap()), scope);

                for (i, item) in items.iter().enumerate() {
                    let value = self.compile_expr(llvm_func, loop_exit, item)?;

                    if i == items.len() - 1 {
                        return Ok(value);
                    }
                }

                Ok(None)
            }
            TaggedExprValue::Statement(inner) => {
                self.compile_expr(llvm_func, loop_exit, inner)?;
                Ok(None)
            }
            TaggedExprValue::Binary { op, lhs, rhs } => {
                self.compile_binary(llvm_func, loop_exit, &expr.ty, op, lhs, rhs)
            }
            TaggedExprValue::LocalDefinition { ty, name, value } => {
                self.compile_local_definition(llvm_func, loop_exit, expr, name, ty, value)?;
                Ok(None)
            }
            TaggedExprValue::IntegerLiteral(value) => {
                if let LangType::IntType(it) = expr.ty.as_ref() {
                    let (ty, _) = it.as_llvm_int_type(self.module.get_context());
                    let value = ty.const_int(*value, false);
                    Ok(Some(value.into()))
                } else {
                    todo!()
                }
            }
            TaggedExprValue::StringLiteral(value) => {
                let s = self.builder.build_global_string_ptr(value, "");
                Ok(Some(s.as_pointer_value().as_any_value_enum()))
            }
            TaggedExprValue::Ident(name) => {
                let ptr = self.ident_ptr(llvm_func, &scope, name).unwrap();
                Ok(Some(match ptr {
                    IdentValue::Variable(ptr) => match expr.ty.as_ref() {
                        LangType::IntType(_) | LangType::BoolType | LangType::Pointer(_) => {
                            self.builder.build_load(ptr, "").into()
                        }
                        LangType::SizedArrayType(_, _) => ptr.into(),
                        LangType::Void => todo!("Reference to void-typed identifier?"),
                    },
                    IdentValue::Argument(val) => val.into(),
                }))
            }
            TaggedExprValue::Call(callee, args) => match &callee.value {
                TaggedExprValue::Ident(name) => {
                    self.compile_simple_call(llvm_func, loop_exit, name, args)
                }
                _ => todo!("Callee {:?}", callee.value),
            },
            TaggedExprValue::BreakLoop => {
                if let Some(loop_exit) = loop_exit {
                    self.builder.build_unconditional_branch(loop_exit);
                    let new_bb = self
                        .module
                        .get_context()
                        .append_basic_block(llvm_func.func, "break");
                    self.builder.position_at_end(new_bb);
                    Ok(None)
                } else {
                    todo!()
                }
            }
            TaggedExprValue::Return(return_value) => {
                if let Some(_return_value) = return_value {
                    todo!()
                } else {
                    self.builder.build_return(None);
                    let new_bb = self
                        .module
                        .get_context()
                        .append_basic_block(llvm_func.func, "ret");
                    self.builder.position_at_end(new_bb);
                    Ok(None)
                }
            }
            TaggedExprValue::Loop { condition, body } => {
                self.compile_loop(llvm_func, loop_exit, condition, body)?;
                Ok(None)
            }
            TaggedExprValue::Condition {
                condition,
                if_true,
                if_false,
            } => {
                let condition_value = self.compile_expr(llvm_func, loop_exit, condition)?.unwrap();

                let bb_true = self
                    .module
                    .get_context()
                    .append_basic_block(llvm_func.func, "if_true");
                let bb_false = self
                    .module
                    .get_context()
                    .append_basic_block(llvm_func.func, "if_false");

                self.builder.build_conditional_branch(
                    condition_value.into_int_value(),
                    bb_true,
                    bb_false,
                );

                self.builder.position_at_end(bb_true);
                // TODO pick value
                self.compile_expr(llvm_func, loop_exit, if_true)?;

                if let Some(if_false) = if_false {
                    let bb_end = self
                        .module
                        .get_context()
                        .append_basic_block(llvm_func.func, "if_end");
                    self.builder.build_unconditional_branch(bb_end);

                    self.builder.position_at_end(bb_false);

                    // TODO pick value
                    self.compile_expr(llvm_func, loop_exit, if_false)?;

                    self.builder.build_unconditional_branch(bb_end);

                    self.builder.position_at_end(bb_end);
                } else {
                    self.builder.build_unconditional_branch(bb_false);
                    self.builder.position_at_end(bb_false);
                }

                Ok(None)
            }
            TaggedExprValue::Array(_elements) => {
                todo!()
            }
            TaggedExprValue::ArrayRepeat(_element, _count) => {
                todo!()
            }
            TaggedExprValue::ArrayElement(array, index) => {
                let array = self
                    .compile_expr(llvm_func, loop_exit, array)?
                    .unwrap()
                    .into_pointer_value();
                let index = self
                    .compile_expr(llvm_func, loop_exit, index)?
                    .unwrap()
                    .into_int_value();

                // TODO emit bounds check here

                let zero_index = self.module.get_context().i64_type().const_zero();

                let gep = unsafe { self.builder.build_gep(array, &[zero_index, index], "") };

                Ok(Some(match expr.ty.as_ref() {
                    LangType::IntType(_) | LangType::BoolType => {
                        self.builder.build_load(gep, "").into()
                    }
                    LangType::SizedArrayType(_, _) => todo!("Nested array indexing"),
                    LangType::Pointer(_) => todo!("Pointer-typed array indexing"),
                    LangType::Void => todo!("Void-typed array indexing?"),
                }))
            }
            TaggedExprValue::Dereference(value) => {
                let value = self.compile_expr(llvm_func, loop_exit, value)?.unwrap();
                if !value.is_pointer_value() {
                    todo!();
                }

                let deref = self.builder.build_load(value.into_pointer_value(), "");
                Ok(Some(deref.as_any_value_enum()))
            }
            TaggedExprValue::Reference(lvalue) => {
                let lvalue = self.compile_lvalue(llvm_func, loop_exit, lvalue)?;
                Ok(Some(lvalue.as_any_value_enum()))
            }
            TaggedExprValue::Cast(value, target_ty) => {
                let value = self.compile_cast(llvm_func, loop_exit, value, target_ty)?;
                Ok(Some(value))
            }
            TaggedExprValue::Assign(lhs, rhs) => {
                let lhs = self.compile_lvalue(llvm_func, loop_exit, lhs)?;
                let rhs = self.compile_expr(llvm_func, loop_exit, rhs)?.unwrap();

                match rhs {
                    AnyValueEnum::IntValue(iv) => self.builder.build_store(lhs, iv),
                    AnyValueEnum::PointerValue(pv) => self.builder.build_store(lhs, pv),
                    _ => todo!(),
                };

                Ok(None)
            }
        }
    }
}
