use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc};

use ast::{token::BasicOperator, Node, Token};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    memory_buffer::MemoryBuffer,
    module::{Linkage, Module},
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
        GlobalValue as LlvmGlobalValue, PointerValue,
    },
    IntPredicate,
};

use crate::{CompilerOperation, LangType};

use super::{
    pass1::Scope, CompilerError, FunctionImplementation, FunctionSignature, GlobalValue,
    Pass1Program, TaggedExpr, TaggedExprValue,
};

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

pub struct Codegen<'a> {
    module: Module<'a>,
    builder: Builder<'a>,
    pass1: &'a Pass1Program,
    scopes: RefCell<HashMap<(usize, usize), Rc<LlvmScope<'a>>>>,
    globals: RefCell<HashMap<String, LlvmGlobalValue<'a>>>,
}

pub struct LlvmScope<'a> {
    _pass1_scope: Rc<RefCell<dyn Scope>>,
    stack_values: RefCell<HashMap<String, PointerValue<'a>>>,
}

pub struct LlvmFunctionScope<'a> {
    func: FunctionValue<'a>,
    param_mapping: HashMap<String, usize>,
}

pub enum IdentValue<'a> {
    Variable(PointerValue<'a>),
    Argument(BasicValueEnum<'a>),
}

impl<'a> Codegen<'a> {
    pub fn new(pass1: &'a Pass1Program, name: &str, context: &'a Context) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        Self {
            module,
            builder,
            pass1,
            scopes: RefCell::new(HashMap::new()),
            globals: RefCell::new(HashMap::new()),
        }
    }

    pub fn scope(
        &self,
        fn_index: usize,
        scope_index: Option<usize>,
    ) -> Option<Rc<RefCell<dyn Scope>>> {
        let implementation = self.pass1.functions[fn_index]
            .implementation
            .as_ref()
            .unwrap();
        let func_scope = implementation.scope.clone();
        if let Some(scope_index) = scope_index {
            func_scope.borrow().scope(scope_index)
        } else {
            Some(func_scope)
        }
    }

    pub fn compile_function_implementation(
        &self,
        implementation: &'a FunctionImplementation,
        llvm_func: &LlvmFunctionScope<'a>,
    ) -> Result<(), CompilerError> {
        let llvm_basic_block = self
            .module
            .get_context()
            .append_basic_block(llvm_func.func, "entry");
        self.builder.position_at_end(llvm_basic_block);
        let return_value = self.compile_expr(llvm_func, None, &implementation.body)?;

        if let Some(return_value) = return_value {
            match return_value {
                AnyValueEnum::IntValue(value) => {
                    self.builder.build_return(Some(&value));
                }
                _ => todo!(),
            }
        } else {
            self.builder.build_return(None);
        }

        Ok(())
    }

    pub fn compile_function_signature(
        &self,
        name: &str,
        func: &'a FunctionSignature,
    ) -> Result<FunctionValue<'a>, CompilerError> {
        let llvm_arg_types = func
            .arg_types
            .iter()
            .map(|(_, ty)| ty.as_basic_metadata_type(self.module.get_context()))
            .collect::<Vec<_>>();

        let llvm_func_ty = func
            .return_type
            .make_llvm_function_type(self.module.get_context(), &llvm_arg_types);

        Ok(self
            .module
            .add_function(name, llvm_func_ty, Some(Linkage::External)))
    }

    pub fn compile_simple_call(
        &self,
        llvm_func_scope: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        name: &str,
        args: &'a [Rc<TaggedExpr>],
    ) -> Result<Option<AnyValueEnum>, CompilerError> {
        let llvm_func = self.module.get_function(name).unwrap();
        let compiled_args = args
            .iter()
            .map(|arg| {
                self.compile_expr(llvm_func_scope, loop_exit, arg)
                    .map(Option::unwrap)
                    .map(|arg| match arg {
                        AnyValueEnum::IntValue(value) => BasicMetadataValueEnum::IntValue(value),
                        _ => todo!(),
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let result = self.builder.build_call(llvm_func, &compiled_args, "");

        Ok(Some(result.as_any_value_enum()))
    }

    pub fn compile_expr(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        expr: &'a Rc<TaggedExpr>,
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
                let llvm_rhs = self.compile_expr(llvm_func, loop_exit, rhs)?.unwrap();

                if *op == Token::BasicOperator(BasicOperator::Assign) {
                    let TaggedExprValue::Ident(name) = &lhs.value else {
                        todo!()
                    };

                    let Some(lhs_ptr) = self.local_value(llvm_func, &scope, name) else {
                        todo!()
                    };

                    match lhs_ptr {
                        IdentValue::Variable(ptr) => {
                            self.builder.build_store(
                                ptr,
                                match llvm_rhs {
                                    AnyValueEnum::IntValue(i) => BasicValueEnum::IntValue(i),
                                    _ => todo!(),
                                },
                            );

                            Ok(None)
                        }
                        _ => todo!(),
                    }
                } else {
                    let llvm_lhs = self.compile_expr(llvm_func, loop_exit, lhs)?.unwrap();

                    let value = match expr.ty.as_ref() {
                        LangType::IntType(ty) => {
                            let llvm_lhs = llvm_lhs.into_int_value();
                            let llvm_rhs = llvm_rhs.into_int_value();
                            let (_, signed) = ty.as_llvm_int_type(self.module.get_context());

                            if let Token::BasicOperator(op) = op {
                                match op {
                                    BasicOperator::Add => {
                                        self.builder.build_int_add(llvm_lhs, llvm_rhs, "add").into()
                                    }
                                    BasicOperator::Sub => {
                                        self.builder.build_int_sub(llvm_lhs, llvm_rhs, "sub").into()
                                    }
                                    BasicOperator::Mul => {
                                        self.builder.build_int_mul(llvm_lhs, llvm_rhs, "mul").into()
                                    }
                                    BasicOperator::Div => {
                                        if signed {
                                            self.builder.build_int_signed_div(llvm_lhs, llvm_rhs, "").into()
                                        } else {
                                            self.builder.build_int_unsigned_div(llvm_lhs, llvm_rhs, "").into()
                                        }
                                    }
                                    BasicOperator::Mod => {
                                        if signed {
                                            self.builder.build_int_signed_rem(llvm_lhs, llvm_rhs, "").into()
                                        } else {
                                            self.builder.build_int_unsigned_rem(llvm_lhs, llvm_rhs, "").into()
                                        }
                                    }
                                    BasicOperator::BitAnd => self.builder.build_and(llvm_lhs, llvm_rhs, "band").into(),
                                    BasicOperator::BitOr => self.builder.build_or(llvm_lhs, llvm_rhs, "bor").into(),
                                    BasicOperator::Shl => self.builder.build_left_shift(llvm_lhs, llvm_rhs, "shl").into(),
                                    BasicOperator::Shr => self.builder.build_right_shift(llvm_lhs, llvm_rhs, false, "shr").into(),
                                    _ => todo!(),
                                }
                            } else {
                                todo!()
                            }
                        }
                        LangType::BoolType => {
                            if let Token::BasicOperator(op) = op && op.is_comparison() {
                                assert!(lhs.ty.is_compatible(&rhs.ty));
                                match lhs.ty.as_ref() {
                                    LangType::IntType(ty) => {
                                        let llvm_lhs = llvm_lhs.into_int_value();
                                        let llvm_rhs = llvm_rhs.into_int_value();
                                        let (_, signed) = ty.as_llvm_int_type(self.module.get_context());
                                        self.builder.build_int_compare(op.as_int_comparison_predicate(signed), llvm_lhs, llvm_rhs, "cmp").into()
                                    }
                                    _ => todo!()
                                }
                            } else if let Token::BasicOperator(op) = op && op.is_logic() {
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
            TaggedExprValue::LocalDefinition { ty, name, value } => {
                let llvm_scope = self.llvm_scope(expr.fn_index, expr.scope_index.unwrap());

                let ptr = match ty.as_ref() {
                    LangType::IntType(ty) => {
                        let (ty, _) = ty.as_llvm_int_type(self.module.get_context());
                        let ptr = self.builder.build_alloca(ty, name);
                        let value = self.compile_expr(llvm_func, loop_exit, value)?.unwrap();

                        self.builder.build_store(ptr, value.into_int_value());
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

                        let TaggedExprValue::Array(elements) = &value.value else {
                            todo!();
                        };
                        assert!(!elements.is_empty());

                        // If value is a const-int array, use LLVM's const_array,
                        //  otherwise, use per-element assignment
                        if let Ok(const_ints) = elements
                            .iter()
                            .map(|e| match &e.value {
                                TaggedExprValue::IntegerLiteral(int) => {
                                    Ok(ty.const_int(*int, false))
                                }
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
                                    self.builder.build_in_bounds_gep(ptr, &[zero_index, index], "")
                                };

                                self.builder.build_store(gep, elem.into_int_value());
                            }
                        }

                        ptr
                    }
                    _ => todo!(),
                };

                llvm_scope
                    .stack_values
                    .borrow_mut()
                    .insert(name.clone(), ptr);

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
            TaggedExprValue::Ident(name) => {
                let ptr = if let Some(ptr) = self.local_value(llvm_func, &scope, name) {
                    ptr
                } else if let Some(gv) = self.globals.borrow().get(name) {
                    IdentValue::Variable(gv.as_pointer_value())
                } else {
                    todo!()
                };

                Ok(Some(match ptr {
                    IdentValue::Variable(ptr) => match expr.ty.as_ref() {
                        LangType::IntType(_) | LangType::BoolType => {
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
                    let condition_value =
                        self.compile_expr(llvm_func, loop_exit, condition)?.unwrap();

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

                let gep = unsafe {
                    self.builder
                        .build_gep(array, &[zero_index, index], "")
                };

                Ok(Some(match expr.ty.as_ref() {
                    LangType::IntType(_) | LangType::BoolType => {
                        self.builder.build_load(gep, "").into()
                    }
                    LangType::SizedArrayType(_, _) => todo!("Nested array indexing"),
                    LangType::Void => todo!("Void-typed array indexing?"),
                }))
            }
        }
    }

    fn compile_global_definition(
        &self,
        name: &str,
        global: &GlobalValue,
    ) -> Result<(), CompilerError> {
        let llvm_ty = global
            .ty
            .as_llvm_basic_type(self.module.get_context())
            .unwrap();

        let ptr = self.module.add_global(llvm_ty, None, name);

        match global.initializer.as_ref() {
            Node::IntegerLiteral(value, extra) => {
                let ty = self.pass1.pass0.integer_literal_extra_type(extra).unwrap();
                let LangType::IntType(ty) = ty.as_ref() else {
                    todo!();
                };
                let (ty, _) = ty.as_llvm_int_type(self.module.get_context());

                ptr.set_initializer(&ty.const_int(*value, false).as_basic_value_enum());
            }
            _ => todo!(),
        };

        self.globals.borrow_mut().insert(name.to_owned(), ptr);

        Ok(())
    }

    pub fn compile_module(&mut self) -> Result<MemoryBuffer, CompilerError> {
        for (name, global) in self.pass1.globals.iter() {
            self.compile_global_definition(name, global)?;
        }

        let func_values = self
            .pass1
            .functions
            .iter()
            .map(|f| self.compile_function_signature(&f.name, &f.signature))
            .collect::<Result<Vec<_>, _>>()?;

        for (index, func) in self.pass1.functions.iter().enumerate() {
            if let Some(implementation) = func.implementation.as_ref() {
                let llvm_func = func_values[index];

                let llvm_func_scope = LlvmFunctionScope {
                    func: llvm_func,
                    param_mapping: HashMap::from_iter(
                        func.signature
                            .arg_types
                            .iter()
                            .map(|(k, _)| k)
                            .cloned()
                            .enumerate()
                            .map(|(k, v)| (v, k)),
                    ),
                };

                self.compile_function_implementation(implementation, &llvm_func_scope)?;
            }
        }

        Ok(self.module.write_bitcode_to_memory())
    }

    fn llvm_scope(&self, fn_index: usize, scope_index: usize) -> Rc<LlvmScope<'a>> {
        self.scopes
            .borrow()
            .get(&(fn_index, scope_index))
            .cloned()
            .unwrap()
    }

    fn local_value(
        &self,
        llvm_func: &LlvmFunctionScope<'a>,
        scope: &Rc<RefCell<dyn Scope>>,
        name: &str,
    ) -> Option<IdentValue<'a>> {
        if let Some(local) = scope.borrow().local(name) {
            if let Some(scope_index) = local.scope_index {
                // Local variable
                let llvm_scope = self
                    .scopes
                    .borrow()
                    .get(&(local.fn_index, scope_index))
                    .unwrap()
                    .clone();
                let stack_values = llvm_scope.stack_values.borrow();

                stack_values.get(name).cloned().map(IdentValue::Variable)
            } else {
                // Argument
                let index = llvm_func.param_mapping.get(name).unwrap();
                let param = llvm_func.func.get_nth_param(*index as u32);

                param.map(IdentValue::Argument)
            }
        } else {
            None
        }
    }
}

pub fn compile_module<P: AsRef<Path>>(
    pass1: Pass1Program,
    operation: CompilerOperation,
    name: &str,
    dst_obj: P,
) -> Result<(), CompilerError> {
    let context = Context::create();
    let mut cg = Codegen::new(&pass1, name, &context);
    cg.compile_module()?;

    match operation {
        CompilerOperation::EmitIntermediateBitcode => {
            if !cg.module.write_bitcode_to_path(dst_obj.as_ref()) {
                todo!();
            }
        }
        CompilerOperation::EmitIntermediateSourceCode => {
            cg.module.print_to_file(dst_obj).unwrap();
        }
    }

    Ok(())
}
