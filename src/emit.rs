use std::collections::HashMap;

use wasm_encoder::{
    BlockType, CodeSection, EntityType, Export, ExportSection, Function, FunctionSection,
    GlobalType, ImportSection, Instruction, MemArg, MemoryType, Module, TypeSection, ValType,
};

use crate::ast;

pub fn emit(script: &ast::Script) -> Vec<u8> {
    let mut module = Module::new();

    let function_types = collect_function_types(script);
    {
        let mut types = TypeSection::new();
        let mut type_vec: Vec<_> = function_types.iter().map(|(k, v)| (*v, k)).collect();
        type_vec.sort();
        for (_, (params, result)) in type_vec {
            let params: Vec<_> = params.iter().cloned().map(map_type).collect();
            let result: Vec<_> = result.iter().cloned().map(map_type).collect();
            types.function(params, result);
        }
        module.section(&types);
    }

    let mut globals: HashMap<&str, u32> = HashMap::new();

    {
        let mut imports = ImportSection::new();

        for import in &script.imports {
            let (module, name) = if let Some(dot_index) = import.import.find('.') {
                (
                    &import.import[..dot_index],
                    Some(&import.import[(dot_index + 1)..]),
                )
            } else {
                (import.import, None)
            };
            let type_: EntityType = match import.type_ {
                ast::ImportType::Memory(min_size) => MemoryType {
                    minimum: min_size as u64,
                    maximum: None,
                    memory64: false,
                }
                .into(),
                ast::ImportType::Variable { type_, name } => {
                    globals.insert(name, globals.len() as u32);
                    GlobalType {
                        val_type: map_type(type_),
                        mutable: false,
                    }
                    .into()
                }
            };
            imports.import(module, name, type_);
        }

        module.section(&imports);
    }

    {
        let mut functions = FunctionSection::new();
        let mut exports = ExportSection::new();
        let mut code = CodeSection::new();

        for (index, func) in script.functions.iter().enumerate() {
            let type_ = *function_types.get(&function_type_key(func)).unwrap();
            functions.function(type_ as u32);
            if func.export {
                exports.export(func.name, Export::Function(index as u32));
            }

            code.function(&emit_function(func, &globals));
        }

        module.section(&functions);
        module.section(&exports);
        module.section(&code);
    }

    module.finish()
}

type FunctionTypeKey = (Vec<ast::Type>, Option<ast::Type>);

fn collect_function_types(script: &ast::Script) -> HashMap<FunctionTypeKey, usize> {
    let mut types: HashMap<FunctionTypeKey, usize> = HashMap::new();

    for func in &script.functions {
        let index = types.len();
        types
            .entry(function_type_key(func))
            .or_insert_with(|| index);
    }

    types
}

fn function_type_key(func: &ast::Function) -> FunctionTypeKey {
    let param_types: Vec<_> = func.params.iter().map(|(_, type_)| *type_).collect();
    (param_types, func.type_)
}

struct FunctionContext<'a> {
    function: &'a mut Function,
    globals: &'a HashMap<&'a str, u32>,
    locals: &'a HashMap<&'a str, u32>,
    labels: Vec<String>,
}

fn emit_function(func: &ast::Function, globals: &HashMap<&str, u32>) -> Function {
    let mut locals = Vec::new();
    collect_locals(&func.body, &mut locals);
    locals.sort_by_key(|(_, t)| *t);

    let mut function = Function::new_with_locals_types(locals.iter().map(|(_, t)| map_type(*t)));

    let locals: HashMap<&str, u32> = locals
        .into_iter()
        .enumerate()
        .map(|(index, (name, _))| (name, index as u32))
        .collect();

    let mut context = FunctionContext {
        function: &mut function,
        globals,
        locals: &locals,
        labels: vec![],
    };

    emit_block(&mut context, &func.body);
    if func.type_.is_none() && func.body.type_().is_some() {
        function.instruction(&Instruction::Drop);
    }
    function.instruction(&Instruction::End);

    function
}

fn collect_locals<'a>(block: &ast::Block<'a>, locals: &mut Vec<(&'a str, ast::Type)>) {
    for stmt in &block.statements {
        match stmt {
            ast::Statement::LocalVariable(v) => {
                locals.push((v.name, v.type_.unwrap()));
                if let Some(ref value) = v.value {
                    collect_locals_expr(value, locals);
                }
            }
            ast::Statement::Expression(e) => collect_locals_expr(e, locals),
            ast::Statement::Poke {
                mem_location,
                value,
                ..
            } => {
                collect_locals_expr(&mem_location.left, locals);
                collect_locals_expr(value, locals);
            }
        }
    }
    if let Some(ref expr) = block.final_expression {
        collect_locals_expr(expr, locals);
    }
}

fn collect_locals_expr<'a>(expr: &ast::Expression<'a>, locals: &mut Vec<(&'a str, ast::Type)>) {
    match &expr.expr {
        ast::Expr::Variable { .. } | ast::Expr::I32Const(_) => (),
        ast::Expr::BinOp { left, right, .. } => {
            collect_locals_expr(left, locals);
            collect_locals_expr(right, locals);
        }
        ast::Expr::BranchIf { condition, .. } => collect_locals_expr(condition, locals),
        ast::Expr::LocalTee { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::Loop { block, .. } => collect_locals(block, locals),
    }
}

fn emit_block(ctx: &mut FunctionContext, block: &ast::Block) {
    for stmt in &block.statements {
        match stmt {
            ast::Statement::Expression(e) => {
                emit_expression(ctx, e);
                if e.type_.is_some() {
                    ctx.function.instruction(&Instruction::Drop);
                }
            }
            ast::Statement::LocalVariable(v) => {
                if let Some(ref val) = v.value {
                    emit_expression(ctx, val);
                    ctx.function
                        .instruction(&Instruction::LocalSet(*ctx.locals.get(v.name).unwrap()));
                }
            }
            ast::Statement::Poke {
                mem_location,
                value,
                ..
            } => {
                emit_expression(ctx, value);
                emit_expression(ctx, &mem_location.left);
                let offset = if let ast::Expr::I32Const(v) = mem_location.right.expr {
                    v as u32 as u64
                } else {
                    unreachable!()
                };
                ctx.function.instruction(&match mem_location.size {
                    ast::MemSize::Byte => Instruction::I32Store8(MemArg {
                        align: 0,
                        memory_index: 0,
                        offset,
                    }),
                    ast::MemSize::Word => Instruction::I32Store(MemArg {
                        align: 2,
                        memory_index: 0,
                        offset,
                    }),
                });
            }
        }
    }
    if let Some(ref expr) = block.final_expression {
        emit_expression(ctx, expr);
    }
}

fn emit_expression(ctx: &mut FunctionContext, expr: &ast::Expression) {
    match &expr.expr {
        ast::Expr::BinOp {
            left, op, right, ..
        } => {
            emit_expression(ctx, left);
            emit_expression(ctx, right);
            use ast::BinOp::*;
            use ast::Type::*;
            ctx.function.instruction(&match (left.type_.unwrap(), op) {
                (I32, Add) => Instruction::I32Add,
                (I32, Sub) => Instruction::I32Sub,
                (I32, Mul) => Instruction::I32Mul,
                (I32, Div) => Instruction::I32DivS,
                (I32, Rem) => Instruction::I32RemS,
                (I32, And) => Instruction::I32And,
                (I32, Or) => Instruction::I32Or,
                (I32, Xor) => Instruction::I32Xor,
                (I32, Eq) => Instruction::I32Eq,
                (I32, Ne) => Instruction::I32Neq,
                (I32, Lt) => Instruction::I32LtS,
                (I32, Le) => Instruction::I32LeS,
                (I32, Gt) => Instruction::I32GtS,
                (I32, Ge) => Instruction::I32GeS,

                (I64, _) => todo!(),
                (F32, _) => todo!(),
                (F64, _) => todo!(),
            });
        }
        ast::Expr::BranchIf {
            condition, label, ..
        } => {
            emit_expression(ctx, condition);
            let depth = ctx
                .labels
                .iter()
                .rev()
                .enumerate()
                .find(|(_, l)| l == label)
                .unwrap()
                .0;
            ctx.function.instruction(&Instruction::BrIf(depth as u32));
        }
        ast::Expr::I32Const(v) => {
            ctx.function.instruction(&Instruction::I32Const(*v));
        }
        ast::Expr::LocalTee { name, value, .. } => {
            emit_expression(ctx, value);
            let index = ctx.locals.get(*name).unwrap();
            ctx.function.instruction(&Instruction::LocalTee(*index));
        }
        ast::Expr::Loop { label, block, .. } => {
            ctx.labels.push(label.to_string());
            ctx.function
                .instruction(&Instruction::Loop(map_block_type(block.type_())));
            emit_block(ctx, block);
            ctx.labels.pop();
            ctx.function.instruction(&Instruction::End);
        }
        ast::Expr::Variable { name, .. } => {
            if let Some(index) = ctx.locals.get(*name) {
                ctx.function.instruction(&Instruction::LocalGet(*index));
            } else if let Some(index) = ctx.globals.get(*name) {
                ctx.function.instruction(&Instruction::GlobalGet(*index));
            } else {
                unreachable!()
            }
        }
    }
}

fn map_type(t: ast::Type) -> ValType {
    match t {
        ast::Type::I32 => ValType::I32,
        ast::Type::I64 => ValType::I64,
        ast::Type::F32 => ValType::F32,
        ast::Type::F64 => ValType::F64,
    }
}

fn map_block_type(t: Option<ast::Type>) -> BlockType {
    if let Some(t) = t {
        BlockType::Result(map_type(t))
    } else {
        BlockType::Empty
    }
}
