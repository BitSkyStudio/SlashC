use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::values::{AnyValue, BasicValueEnum, PointerValue};

use std::error::Error;

use crate::ast::ASTFunction;
use crate::compile::{CompiledFunction, CompiledStatement};
use crate::lexer::Operator;

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn() -> i64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_function(
        &self,
        compiled_function: CompiledFunction,
    ) -> Option<JitFunction<SumFunc>> {
        let function_name = "test";
        let i64_type = self.context.i64_type();
        let void_type = self.context.void_type();
        let fn_type = void_type.fn_type(&[], false);
        let function = self.module.add_function(function_name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let it = self.context.i64_type();
        let variables: Vec<_> = compiled_function
            .variables
            .iter()
            .map(|variable| self.builder.build_alloca(it, "var").unwrap())
            .collect();

        for statement in compiled_function.block.statements {
            self.build_statement(&statement, &variables);
        }

        self.builder.build_return(None).unwrap();

        unsafe { self.execution_engine.get_function(function_name).ok() }
    }
    fn build_statement<'a>(
        &'a self,
        statement: &CompiledStatement,
        variables: &Vec<PointerValue<'a>>,
    ) -> Option<BasicValueEnum<'a>> {
        match statement {
            CompiledStatement::IntegerLiteral { value } => Some(
                self.context
                    .i64_type()
                    .const_int(*value as u64, false)
                    .into(),
            ),
            CompiledStatement::LoadVariable { variable } => Some(
                self.builder
                    .build_load(
                        self.context.i64_type(),
                        variables[*variable as usize],
                        "loadvar",
                    )
                    .unwrap(),
            ),
            CompiledStatement::SaveVariable { variable, value } => {
                self.builder
                    .build_store(
                        variables[*variable as usize],
                        Self::build_statement(self, value, variables).unwrap(),
                    )
                    .unwrap();
                None
            }
            CompiledStatement::IntegerOp { a, b, op } => match op {
                Operator::Plus => Some(
                    self.builder
                        .build_int_add(
                            Self::build_statement(self, &a, variables)
                                .unwrap()
                                .into_int_value(),
                            Self::build_statement(self, &b, variables)
                                .unwrap()
                                .into_int_value(),
                            "add",
                        )
                        .unwrap()
                        .into(),
                ),
                _ => unimplemented!(),
            },
            CompiledStatement::FunctionCall { name, arguments } => {
                let function = self.module.get_function(&name).unwrap();

                self.builder
                    .build_call(
                        function,
                        arguments
                            .iter()
                            .map(|argument| {
                                Self::build_statement(self, argument, variables)
                                    .unwrap()
                                    .into()
                            })
                            .collect::<Vec<_>>()
                            .as_slice(),
                        "call",
                    )
                    .unwrap();
                None
            }
        }
    }
}

extern "C" fn printline(a: i64) {
    println!("dbg: {}", a);
}

pub fn testrun(function: CompiledFunction) -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::Default)?;
    let vt = context.void_type();
    let it = context.i64_type();
    let extf = module.add_function("print", vt.fn_type(&[it.into()], false), None);
    execution_engine.add_global_mapping(&extf, printline as usize);
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    let sum = codegen
        .jit_compile_function(function)
        .ok_or("Unable to JIT compile `sum`")?;

    unsafe {
        println!("{}", sum.call());
    }

    Ok(())
}
