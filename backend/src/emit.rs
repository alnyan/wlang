use std::{cell::RefCell, collections::HashMap, rc::Rc, path::Path, fs::File, io::Write};

use ast::{token::BasicOperator, Node, Token};
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::AnyTypeEnum,
    values::{
        AnyValue, AnyValueEnum, BasicMetadataValueEnum, BasicValue, BasicValueEnum, FunctionValue,
        GlobalValue as LlvmGlobalValue, PointerValue,
    },
    IntPredicate, memory_buffer::MemoryBuffer,
};

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
        let return_value = self.compile_expr(llvm_func, &implementation.body)?;

        if let Some(return_value) = return_value {
            match return_value {
                AnyValueEnum::IntValue(value) => {
                    self.builder.build_return(Some(&value));
                }
                _ => todo!(),
            }
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
        name: &str,
        args: &[Rc<TaggedExpr>],
    ) -> Result<Option<AnyValueEnum>, CompilerError> {
        let llvm_func = self.module.get_function(name).unwrap();
        let compiled_args = args
            .iter()
            .map(|arg| {
                self.compile_expr(llvm_func_scope, arg)
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
                    let value = self.compile_expr(llvm_func, item)?;

                    if i == items.len() - 1 {
                        return Ok(value);
                    }
                }

                Ok(None)
            }
            TaggedExprValue::Statement(inner) => {
                self.compile_expr(llvm_func, inner)?;
                Ok(None)
            }
            TaggedExprValue::Binary { op, lhs, rhs } => {
                let rhs = self.compile_expr(llvm_func, rhs)?.unwrap();

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
                                match rhs {
                                    AnyValueEnum::IntValue(i) => BasicValueEnum::IntValue(i),
                                    _ => todo!(),
                                },
                            );

                            Ok(None)
                        }
                        _ => todo!(),
                    }
                } else {
                    let lhs = self.compile_expr(llvm_func, lhs)?.unwrap();
                    let llvm_ty = expr.ty.as_llvm_any_type(self.module.get_context()).unwrap();

                    let value = match llvm_ty {
                        AnyTypeEnum::IntType(_) => {
                            let lhs = lhs.into_int_value();
                            let rhs = rhs.into_int_value();

                            if let Token::BasicOperator(op) = op {
                                if op.is_arithmetic() {
                                    match op {
                                        BasicOperator::Add => {
                                            self.builder.build_int_add(lhs, rhs, "").into()
                                        }
                                        BasicOperator::Sub => {
                                            self.builder.build_int_sub(lhs, rhs, "").into()
                                        }
                                        BasicOperator::Mul => {
                                            self.builder.build_int_mul(lhs, rhs, "").into()
                                        }
                                        BasicOperator::Div => {
                                            self.builder.build_int_unsigned_div(lhs, rhs, "").into()
                                        }
                                        BasicOperator::Mod => {
                                            self.builder.build_int_unsigned_rem(lhs, rhs, "").into()
                                        }
                                        _ => todo!(),
                                    }
                                } else if op.is_comparison() {
                                    self.builder
                                        .build_int_compare(
                                            op.as_int_comparison_predicate(false),
                                            lhs,
                                            rhs,
                                            "",
                                        )
                                        .into()
                                } else {
                                    todo!()
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
                let llvm_ty = ty.as_llvm_any_type(self.module.get_context()).unwrap();
                let ptr = match llvm_ty {
                    AnyTypeEnum::IntType(ty) => self.builder.build_alloca(ty, name),
                    _ => todo!(),
                };
                llvm_scope
                    .stack_values
                    .borrow_mut()
                    .insert(name.clone(), ptr);
                let llvm_value = self.compile_expr(llvm_func, value)?.unwrap();
                match llvm_value {
                    AnyValueEnum::IntValue(value) => self.builder.build_store(ptr, value),
                    _ => todo!(),
                };
                Ok(None)
            }
            TaggedExprValue::IntegerLiteral(value) => {
                let llvm_ty = expr
                    .ty
                    .as_llvm_any_type(self.module.get_context())
                    .unwrap()
                    .into_int_type();

                Ok(Some(llvm_ty.const_int(*value, false).into()))
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
                    IdentValue::Variable(ptr) => self.builder.build_load(ptr, "").into(),
                    IdentValue::Argument(val) => val.into(),
                }))
            }
            TaggedExprValue::Call(callee, args) => match &callee.value {
                TaggedExprValue::Ident(name) => self.compile_simple_call(llvm_func, name, args),
                _ => todo!("Callee {:?}", callee.value),
            },
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
                    let condition_value = self.compile_expr(llvm_func, condition)?.unwrap();

                    self.builder.build_conditional_branch(
                        condition_value.into_int_value(),
                        bb_loop_body,
                        bb_loop_exit,
                    );

                    self.builder.position_at_end(bb_loop_body);
                } else {
                    todo!()
                }

                self.compile_expr(llvm_func, body)?;

                self.builder.build_unconditional_branch(bb_loop_entry);
                self.builder.position_at_end(bb_loop_exit);

                Ok(None)
            }
            TaggedExprValue::Condition {
                condition,
                if_true,
                if_false,
            } => {
                let condition_value = self.compile_expr(llvm_func, condition)?.unwrap();

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
                self.compile_expr(llvm_func, if_true)?;

                if let Some(if_false) = if_false {
                    let bb_end = self
                        .module
                        .get_context()
                        .append_basic_block(llvm_func.func, "if_end");
                    self.builder.build_unconditional_branch(bb_end);

                    self.builder.position_at_end(bb_false);

                    // TODO pick value
                    self.compile_expr(llvm_func, if_false)?;

                    self.builder.build_unconditional_branch(bb_end);

                    self.builder.position_at_end(bb_end);
                } else {
                    self.builder.build_unconditional_branch(bb_false);
                    self.builder.position_at_end(bb_false);
                }

                Ok(None)
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

        let initializer = match global.initializer.as_ref() {
            Node::IntegerLiteral(value) => self
                .module
                .get_context()
                .i64_type()
                .const_int(*value, false)
                .as_basic_value_enum(),
            _ => todo!(),
        };

        let ptr = self.module.add_global(llvm_ty, None, name);
        ptr.set_initializer(&initializer);

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

pub fn compile_module<P: AsRef<Path>>(pass1: Pass1Program, name: &str, dst_obj: P) -> Result<(), CompilerError> {
    let context = Context::create();
    let mut cg = Codegen::new(&pass1, name, &context);
    let result = cg.compile_module()?;

    let Ok(mut file) = File::create(dst_obj) else {
        todo!();
    };
    file.write_all(result.as_slice()).unwrap();

    Ok(())
}
