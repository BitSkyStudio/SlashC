use inkwell::OptimizationLevel;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{AnyValue, BasicValueEnum, PointerValue};

use std::error::Error;

use crate::ast::{ASTFunction, ASTMember, DataType};
use crate::compile::{CompiledFunction, CompiledStatement, Compiler, ItemPath};
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
    fn get_type(&self, path: &DataType) -> BasicTypeEnum<'ctx> {
        let path = path.param_type.path.0.join("::");
        match path.as_str() {
            "i64" => self.context.i64_type().as_basic_type_enum(),
            "bool" => self.context.bool_type().as_basic_type_enum(),
            //todo: void
            path => self
                .context
                .get_struct_type(path)
                .unwrap()
                .as_basic_type_enum(),
        }
    }
    fn jit_compile_function(&self, function_name: String, compiled_function: CompiledFunction) {
        let function = self.module.get_function(&function_name).unwrap();
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let variables: Vec<_> = compiled_function
            .variables
            .iter()
            .map(|variable| {
                self.builder
                    .build_alloca(self.get_type(variable), "var")
                    .unwrap()
            })
            .collect();

        for i in 0..compiled_function.parameters.len() {
            self.builder
                .build_store(variables[i], function.get_nth_param(i as u32).unwrap())
                .unwrap();
        }

        let mut returned = None;
        for statement in compiled_function.block.statements {
            returned = self.build_statement(&statement, &variables);
        }
        self.builder.build_return(Some(&returned.unwrap())).unwrap();
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
            CompiledStatement::StoreVariable { variable, value } => {
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
                Operator::Minus => Some(
                    self.builder
                        .build_int_sub(
                            Self::build_statement(self, &a, variables)
                                .unwrap()
                                .into_int_value(),
                            Self::build_statement(self, &b, variables)
                                .unwrap()
                                .into_int_value(),
                            "sub",
                        )
                        .unwrap()
                        .into(),
                ),
                _ => unimplemented!(),
            },
            CompiledStatement::FunctionCall { path, arguments } => {
                let function = self.module.get_function(&path.0.join("::")).unwrap();

                let returned = self
                    .builder
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
                returned.try_as_basic_value().left()
            }
        }
    }
}

extern "C" fn printline(a: i64) {
    println!("dbg: {}", a);
}

pub fn testrun(compiler: &Compiler) -> Result<(), Box<dyn Error>> {
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

    for (path, item) in &compiler.sources {
        let name = path.0.join("::");
        match item {
            ASTMember::Function(function) => {
                let fn_type: inkwell::types::FunctionType<'_> =
                    codegen.get_type(&function.return_type).fn_type(
                        function
                            .parameters
                            .iter()
                            .map(|param| codegen.get_type(&param.data_type).into())
                            .collect::<Vec<_>>()
                            .as_slice(),
                        false,
                    );
                codegen.module.add_function(&name, fn_type, None);
            }
        }
    }
    for (path, item) in &compiler.sources {
        let name = path.0.join("::");
        match item {
            ASTMember::Function(_) => {
                codegen.jit_compile_function(name, compiler.compile_function(path.clone()));
            }
        }
    }
    let sum: JitFunction<'_, SumFunc> =
        unsafe { codegen.execution_engine.get_function("main").unwrap() };

    unsafe {
        println!("{}", sum.call());
    }

    Ok(())
}
