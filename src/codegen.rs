use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::types::{BasicType, BasicTypeEnum};
use inkwell::values::{AnyValue, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::{IntPredicate, OptimizationLevel};

use std::error::Error;

use crate::ast::{ASTFunction, ASTMember, DataType};
use crate::compile::{CompiledBlock, CompiledFunction, CompiledStatement, Compiler, ItemPath};
use crate::lexer::{Comparison, Operator, UnaryOperator};

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
    fn_value_opt: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    fn fn_value(&self) -> FunctionValue<'ctx> {
        self.fn_value_opt.unwrap()
    }
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
    fn jit_compile_function(&mut self, function_name: String, compiled_function: CompiledFunction) {
        let function = self.module.get_function(&function_name).unwrap();
        self.fn_value_opt = Some(function);
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

        let returned = self.build_block(&compiled_function.block, &variables);
        self.builder.build_return(Some(&returned.unwrap())).unwrap();
    }
    fn build_block<'a>(
        &'a self,
        block: &CompiledBlock,
        variables: &Vec<PointerValue<'a>>,
    ) -> Option<BasicValueEnum<'a>> {
        let mut returned = None;
        for statement in &block.statements {
            returned = self.build_statement(&statement, &variables);
        }
        returned
    }
    fn build_statement<'a>(
        &'a self,
        statement: &CompiledStatement,
        variables: &Vec<PointerValue<'a>>,
    ) -> Option<BasicValueEnum<'a>> {
        match statement {
            CompiledStatement::IntegerLiteral { value, data_type } => Some(
                self.get_type(&DataType::make_simple(data_type.clone()))
                    .into_int_type()
                    .const_int(*value as u64, false)
                    .into(),
            ),
            CompiledStatement::LoadVariable {
                variable,
                data_type,
            } => Some(
                self.builder
                    .build_load(
                        self.get_type(data_type),
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
            CompiledStatement::IntegerOp { a, b, op } => {
                let a = Self::build_statement(self, &a, variables)
                    .unwrap()
                    .into_int_value();
                let b = Self::build_statement(self, &b, variables)
                    .unwrap()
                    .into_int_value();
                match op {
                    Operator::Plus => Some(self.builder.build_int_add(a, b, "add").unwrap().into()),
                    Operator::Minus => {
                        Some(self.builder.build_int_sub(a, b, "sub").unwrap().into())
                    }
                    Operator::Multiply => {
                        Some(self.builder.build_int_mul(a, b, "mul").unwrap().into())
                    }
                    Operator::Divide => Some(
                        self.builder
                            .build_int_signed_div(a, b, "div")
                            .unwrap()
                            .into(),
                    ),
                    Operator::Modulo => Some(
                        self.builder
                            .build_int_signed_rem(a, b, "mod")
                            .unwrap()
                            .into(),
                    ),
                    Operator::And => Some(self.builder.build_and(a, b, "and").unwrap().into()),
                    Operator::Or => Some(self.builder.build_or(a, b, "or").unwrap().into()),
                    Operator::Xor => Some(self.builder.build_xor(a, b, "xor").unwrap().into()),
                    Operator::Comparison(comparison) => Some(
                        self.builder
                            .build_int_compare(
                                match comparison {
                                    Comparison::Equal => IntPredicate::EQ,
                                    Comparison::NotEqual => IntPredicate::NE,
                                    Comparison::Greater => IntPredicate::SGT,
                                    Comparison::GreaterEqual => IntPredicate::SGE,
                                    Comparison::Less => IntPredicate::SLT,
                                    Comparison::LessEqual => IntPredicate::SLE,
                                },
                                a,
                                b,
                                "cmp",
                            )
                            .unwrap()
                            .into(),
                    ),
                }
            }
            CompiledStatement::IntegerUnaryOp { a, op } => {
                let a = Self::build_statement(self, &a, variables)
                    .unwrap()
                    .into_int_value();
                match op {
                    UnaryOperator::Not => Some(self.builder.build_not(a, "not").unwrap().into()),
                    UnaryOperator::Negate => {
                        Some(self.builder.build_int_neg(a, "neg").unwrap().into())
                    }
                }
            }
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
            CompiledStatement::IfConditional {
                condition,
                then,
                alt,
                returned_type,
            } => {
                let parent = self.fn_value();
                // create condition by comparing without 0.0 and returning an int
                let cond = self
                    .build_statement(&condition, variables)
                    .unwrap()
                    .into_int_value();

                // build branch
                let then_bb = self.context.append_basic_block(parent, "then");
                let else_bb = self.context.append_basic_block(parent, "else");
                let cont_bb = self.context.append_basic_block(parent, "ifcont");

                self.builder
                    .build_conditional_branch(cond, then_bb, else_bb)
                    .unwrap();

                // build then block
                self.builder.position_at_end(then_bb);
                //let then_val = self.compile_expr(consequence)?;
                let then_val = self.build_block(then, variables);
                self.builder.build_unconditional_branch(cont_bb).unwrap();

                let then_bb = self.builder.get_insert_block().unwrap();

                // build else block
                self.builder.position_at_end(else_bb);
                //let else_val = self.compile_expr(alternative)?;
                let else_val = if let Some(alt) = alt {
                    self.build_block(alt, variables)
                } else {
                    None
                };

                self.builder.build_unconditional_branch(cont_bb).unwrap();

                let else_bb = self.builder.get_insert_block().unwrap();
                // emit merge block
                self.builder.position_at_end(cont_bb);

                if returned_type.param_type.path.0 == vec!["void".to_string()] {
                    None
                } else {
                    let phi = self
                        .builder
                        .build_phi(self.get_type(returned_type), "iftmp")
                        .unwrap();
                    phi.add_incoming(&[
                        (&then_val.unwrap(), then_bb),
                        (&else_val.unwrap(), else_bb),
                    ]);
                    Some(phi.as_basic_value())
                }
            }
            CompiledStatement::WhileLoop { condition, body } => {
                let parent = self.fn_value();
                // create condition by comparing without 0.0 and returning an int

                // build branch

                let cond_bb = self.context.append_basic_block(parent, "cond");
                let body_bb = self.context.append_basic_block(parent, "body");
                let end_bb = self.context.append_basic_block(parent, "whilecont");

                self.builder.build_unconditional_branch(cond_bb).unwrap();
                self.builder.position_at_end(cond_bb);
                let cond = self
                    .build_statement(&condition, variables)
                    .unwrap()
                    .into_int_value();
                self.builder
                    .build_conditional_branch(cond, body_bb, end_bb)
                    .unwrap();

                self.builder.position_at_end(body_bb);
                self.build_block(&body, variables);
                self.builder.build_unconditional_branch(cond_bb).unwrap();

                self.builder.position_at_end(end_bb);

                /*let phi = self
                    .builder
                    .build_phi(self.context.f64_type(), "iftmp")
                    .unwrap();

                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);*/

                None
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
    let mut codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
        fn_value_opt: None,
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
            ASTMember::Struct(structure) => {
                codegen.context.opaque_struct_type(&name);
            }
        }
    }
    for (path, item) in &compiler.sources {
        let name = path.0.join("::");
        match item {
            ASTMember::Function(_) => {
                codegen.jit_compile_function(name, compiler.compile_function(path.clone()));
            }
            ASTMember::Struct(structure) => {
                codegen.context.get_struct_type(&name).unwrap().set_body(
                    structure
                        .fields
                        .iter()
                        .map(|param| codegen.get_type(&param.data_type).into())
                        .collect::<Vec<_>>()
                        .as_slice(),
                    false,
                );
            }
        }
    }

    println!("{}", codegen.module.print_to_string().to_string());

    let sum: JitFunction<'_, SumFunc> =
        unsafe { codegen.execution_engine.get_function("main").unwrap() };

    unsafe {
        println!("{}", sum.call());
    }

    Ok(())
}
