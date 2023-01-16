use std::{cell::RefCell, collections::HashMap, rc::Rc};

use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::AnyTypeEnum,
    values::{AnyValueEnum, BasicValue, GlobalValue as LlvmGlobalValue, PointerValue},
};

use crate::{
    lexer::token::{BasicOperator, Token},
    parser::Node,
};

use super::{
    pass1::Scope, CompilerError, GlobalValue, LangFunction, Pass1Program, TaggedExpr,
    TaggedExprValue,
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
        let func_scope = self.pass1.functions[fn_index].scope.clone();
        if let Some(scope_index) = scope_index {
            func_scope.borrow().scope(scope_index)
        } else {
            Some(func_scope)
        }
    }
    pub fn compile_function(&mut self, func: &'a LangFunction) -> Result<(), CompilerError> {
        assert_eq!(func.signature.arg_types, vec![]);

        let llvm_func_ty = func
            .signature
            .return_type
            .make_llvm_function_type(self.module.get_context(), &[]);
        let llvm_func = self
            .module
            .add_function(&func.name, llvm_func_ty, Some(Linkage::External));

        let llvm_basic_block = self
            .module
            .get_context()
            .append_basic_block(llvm_func, "entry");
        self.builder.position_at_end(llvm_basic_block);
        let return_value = self.compile_expr(&func.body)?;

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

    pub fn compile_expr(
        &self,
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
                    let value = self.compile_expr(item)?;

                    if i == items.len() - 1 {
                        return Ok(value);
                    }
                }

                Ok(None)
            }
            TaggedExprValue::Statement(inner) => {
                self.compile_expr(inner)?;
                Ok(None)
            }
            TaggedExprValue::Binary { op, lhs, rhs } => {
                let llvm_ty = expr.ty.as_llvm_any_type(self.module.get_context()).unwrap();
                let lhs = self.compile_expr(lhs)?.unwrap();
                let rhs = self.compile_expr(rhs)?.unwrap();

                let value = match op {
                    Token::BasicOperator(BasicOperator::Add) => match llvm_ty {
                        AnyTypeEnum::IntType(_) => {
                            assert!(lhs.is_int_value());
                            assert!(rhs.is_int_value());

                            self.builder
                                .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "")
                                .into()
                        }
                        _ => todo!(),
                    },
                    _ => todo!(),
                };

                Ok(Some(value))
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
                let llvm_value = self.compile_expr(value)?.unwrap();
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
                let ptr = if let Some(ptr) = self.local_value(&scope, name) {
                    ptr
                } else if let Some(gv) = self.globals.borrow().get(name) {
                    gv.as_pointer_value()
                } else {
                    todo!()
                };

                Ok(Some(self.builder.build_load(ptr, "").into()))
            } // _ => todo!("{:?}", expr),
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

    pub fn compile_module(&mut self) -> Result<(), CompilerError> {
        for (name, global) in self.pass1.globals.iter() {
            self.compile_global_definition(name, global)?;
        }

        for func in self.pass1.functions.iter() {
            self.compile_function(func)?;
        }

        self.module.print_to_stderr();

        let ee = self.module.create_interpreter_execution_engine().unwrap();
        let fun = self.module.get_function("main").unwrap();
        let result = unsafe { ee.run_function(fun, &[]) };
        dbg!(result.as_int(false));

        Ok(())
    }

    fn llvm_scope(&self, fn_index: usize, scope_index: usize) -> Rc<LlvmScope<'a>> {
        self.scopes
            .borrow()
            .get(&(fn_index, scope_index))
            .cloned()
            .unwrap()
    }

    fn local_value(&self, scope: &Rc<RefCell<dyn Scope>>, name: &str) -> Option<PointerValue<'a>> {
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

                stack_values.get(name).cloned()
            } else {
                // Argument
                todo!()
            }
        } else {
            None
        }
    }
}

pub fn compile_module(pass1: Pass1Program, name: &str) -> Result<(), CompilerError> {
    let context = Context::create();
    let mut cg = Codegen::new(&pass1, name, &context);
    cg.compile_module()
}
