pub struct Codegen {
    context: Context,
    module: Module,
    builder: Builder,
}

pub fn compile_basic_binary(
    cg: &mut Codegen,
    bb: &mut BasicBlock,
    op: BasicOperator,
    lhs: &Rc<Node>,
    rhs: &Rc<Node>,
) -> Result<Option<Value>, CompilerError> {
    let llvm_lhs = compile_expr(cg, bb, lhs)?.unwrap();
    let llvm_rhs = compile_expr(cg, bb, rhs)?.unwrap();

    // TODO check types lmao
    match op {
        BasicOperator::Add => {
            Ok(Some(cg.builder.build_int_add(&llvm_lhs, &llvm_rhs, "add1")))
        }
        _ => todo!()
    }
}

pub fn compile_binary(
    cg: &mut Codegen,
    bb: &mut BasicBlock,
    op: &Token,
    lhs: &Rc<Node>,
    rhs: &Rc<Node>,
) -> Result<Option<Value>, CompilerError> {
    match op {
        Token::BasicOperator(basic) => compile_basic_binary(cg, bb, *basic, lhs, rhs),
        _ => todo!(),
    }
}

pub fn compile_expr(
    cg: &mut Codegen,
    bb: &mut BasicBlock,
    expr: &Rc<Node>,
) -> Result<Option<Value>, CompilerError> {
    match expr.as_ref() {
        Node::Block(items) => {
            for (i, item) in items.iter().enumerate() {
                let value = compile_expr(cg, bb, item)?;

                if i == items.len() - 1 {
                    return Ok(value);
                }
            }

            Ok(None)
        }
        Node::IntegerLiteral(value) => Ok(Some(cg.context.i64_type().const_int(*value, false))),
        Node::Call(target, args) => {
            let llvm_args = args
                .iter()
                .map(|arg| compile_expr(cg, bb, arg))
                .collect::<Result<Vec<_>, _>>()?;
            let llvm_args = llvm_args
                .into_iter()
                .map(Option::unwrap)
                .collect::<Vec<_>>();

            // let llvm_target = compile_expr(cg, bb, target)?;
            if let Node::Ident(name) = target.as_ref() {
                let llvm_func = cg
                    .module
                    .get_function(name)
                    .unwrap_or_else(|| panic!("Undefined function: {}", name));
                Ok(Some(cg.builder.build_call(&llvm_func, &llvm_args, name)))
            } else {
                todo!()
            }
        }
        Node::Statement(inner) => {
            compile_expr(cg, bb, inner)?;
            Ok(None)
        }
        Node::Binary(op, lhs, rhs) => compile_binary(cg, bb, op, lhs, rhs),
        _ => todo!(),
    }
}

pub fn compile_function(cg: &mut Codegen, func: &Function) -> Result<(), CompilerError> {
    assert_eq!(func.ret_type, None);
    assert_eq!(func.args, vec![]);

    let void_type = cg.context.void_type();
    let mut llvm_args = vec![];
    let llvm_ret_type = void_type.fn_type(&mut llvm_args, false);
    let llvm_func = cg.module.add_function(&func.name, llvm_ret_type);

    let mut llvm_basic_block = cg.context.append_basic_block(&llvm_func, "entry");
    cg.builder.position_at_end(&mut llvm_basic_block);
    let return_value = compile_expr(cg, &mut llvm_basic_block, &func.body)?;
    cg.builder.build_return(return_value);

    Ok(())
}

pub fn compile_item(cg: &mut Codegen, item: &Rc<Node>) -> Result<(), CompilerError> {
    match item.as_ref() {
        Node::Function(func) => compile_function(cg, func),
        _ => todo!(),
    }
}

pub fn compile_module(name: &str, items: &[Rc<Node>]) -> Result<(), CompilerError> {
    let context = Context::create();
    let module = context.create_module(name);
    let builder = context.create_builder();

    let mut codegen = Codegen {
        context,
        module,
        builder,
    };

    codegen.module.add_function(
        "getchar",
        codegen.context.i8_type().fn_type(&mut vec![], false),
    );
    codegen.module.add_function(
        "putchar",
        codegen
            .context
            .i8_type()
            .fn_type(&mut vec![codegen.context.i8_type()], false),
    );

    for item in items {
        compile_item(&mut codegen, item)?;
    }

    eprintln!("Code dump:");
    eprintln!();

    let ee = codegen.module.create_execution_engine(false).unwrap();
    let llvm_main = codegen.module.get_function("main").unwrap();

    ee.run_function(llvm_main);

    Ok(())
}
