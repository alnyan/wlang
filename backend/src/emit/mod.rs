use std::{cell::RefCell, collections::HashMap, path::Path, rc::Rc};

pub mod binary;
pub mod expr;

use ast::Node;
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
};

use crate::{
    tagged::{TaggedLvalueExpr, TaggedLvalueExprValue},
    CompilerOperation, LangType,
};

use super::{
    pass1::Scope, CompilerError, FunctionImplementation, FunctionSignature, GlobalValue,
    Pass1Program, TaggedExpr,
};

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
                AnyValueEnum::PointerValue(value) => {
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
        args: &[Rc<TaggedExpr>],
    ) -> Result<Option<AnyValueEnum>, CompilerError> {
        let llvm_func = self.module.get_function(name).unwrap();
        let compiled_args = args
            .iter()
            .map(|arg| {
                self.compile_expr(llvm_func_scope, loop_exit, arg)
                    .map(Option::unwrap)
                    .map(|arg| match arg {
                        AnyValueEnum::IntValue(value) => BasicMetadataValueEnum::IntValue(value),
                        AnyValueEnum::PointerValue(value) => {
                            BasicMetadataValueEnum::PointerValue(value)
                        }
                        _ => todo!(),
                    })
            })
            .collect::<Result<Vec<_>, _>>()?;

        let result = self.builder.build_call(llvm_func, &compiled_args, "");

        Ok(Some(result.as_any_value_enum()))
    }

    fn compile_lvalue<'b>(
        &'b self,
        llvm_func: &LlvmFunctionScope<'a>,
        loop_exit: Option<BasicBlock<'a>>,
        lvalue: &Rc<TaggedLvalueExpr>,
    ) -> Result<PointerValue<'b>, CompilerError> {
        let scope = self.scope(lvalue.fn_index, lvalue.scope_index).unwrap();
        match &lvalue.value {
            TaggedLvalueExprValue::Ident(name) => {
                let value = self.ident_ptr(llvm_func, &scope, name).unwrap();

                if let IdentValue::Variable(ptr) = value {
                    Ok(ptr)
                } else {
                    todo!()
                }
            }
            TaggedLvalueExprValue::Dereference(value) => {
                // Compile-expr will already derefence any lvalue
                let value = self
                    .compile_expr(llvm_func, loop_exit, value)?
                    .unwrap()
                    .into_pointer_value();
                // if !value.get_type().get_element_type().is_pointer_type() {
                //     todo!("LValue cannot be a non-reference type");
                // }

                Ok(value)
            }
            TaggedLvalueExprValue::ArrayElement(array, index) => {
                let array = self.compile_lvalue(llvm_func, loop_exit, array)?;
                let index = self.compile_expr(llvm_func, loop_exit, index)?.unwrap();
                let zero_index = self.module.get_context().i64_type().const_zero();

                if !array.get_type().get_element_type().is_array_type() {
                    todo!("Tried to index into a non-array type");
                }

                let gep = unsafe {
                    self.builder
                        .build_gep(array, &[zero_index, index.into_int_value()], "")
                };
                Ok(gep)
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
            Node::StringLiteral(value) => {
                let value = self
                    .module
                    .get_context()
                    .const_string(value.as_bytes(), true);
                let gstr = self.module.add_global(value.get_type(), None, "");
                gstr.set_unnamed_addr(true);
                gstr.set_linkage(Linkage::Private);
                gstr.set_alignment(1);
                gstr.set_constant(true);
                gstr.set_initializer(&value);

                let zero_index = self.module.get_context().i32_type().const_zero();
                let gep = unsafe {
                    self.builder.build_in_bounds_gep(
                        gstr.as_pointer_value(),
                        &[zero_index, zero_index],
                        "",
                    )
                };

                ptr.set_initializer(&gep);
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

    fn ident_ptr(
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
            self.globals
                .borrow()
                .get(name)
                .copied()
                .map(LlvmGlobalValue::as_pointer_value)
                .map(IdentValue::Variable)
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
