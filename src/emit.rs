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
                (import.import.as_str(), None)
            };
            let type_: EntityType = match import.type_ {
                ast::ImportType::Memory(min_size) => MemoryType {
                    minimum: min_size as u64,
                    maximum: None,
                    memory64: false,
                }
                .into(),
                ast::ImportType::Variable {
                    type_,
                    ref name,
                    mutable,
                } => {
                    globals.insert(name, globals.len() as u32);
                    GlobalType {
                        val_type: map_type(type_),
                        mutable,
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
                exports.export(&func.name, Export::Function(index as u32));
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
    locals: &'a HashMap<String, u32>,
    labels: Vec<String>,
    deferred_inits: HashMap<&'a str, &'a ast::Expression>,
}

fn emit_function(func: &ast::Function, globals: &HashMap<&str, u32>) -> Function {
    let mut locals = Vec::new();
    collect_locals_expr(&func.body, &mut locals);
    locals.sort_by_key(|(_, t)| *t);

    let mut function = Function::new_with_locals_types(locals.iter().map(|(_, t)| map_type(*t)));

    let locals: HashMap<String, u32> = locals
        .into_iter()
        .enumerate()
        .map(|(index, (name, _))| (name, index as u32))
        .collect();

    let mut context = FunctionContext {
        function: &mut function,
        globals,
        locals: &locals,
        labels: vec![],
        deferred_inits: HashMap::new(),
    };

    emit_expression(&mut context, &func.body);
    if func.type_.is_none() && func.body.type_.is_some() {
        function.instruction(&Instruction::Drop);
    }
    function.instruction(&Instruction::End);

    function
}

fn collect_locals_expr<'a>(expr: &ast::Expression, locals: &mut Vec<(String, ast::Type)>) {
    match &expr.expr {
        ast::Expr::Block {
            statements,
            final_expression,
        } => {
            for stmt in statements {
                collect_locals_expr(stmt, locals);
            }
            if let Some(ref expr) = final_expression {
                collect_locals_expr(expr, locals);
            }
        }
        ast::Expr::Let {
            name, type_, value, ..
        } => {
            locals.push((name.clone(), type_.unwrap()));
            if let Some(ref value) = value {
                collect_locals_expr(value, locals);
            }
        }
        ast::Expr::Peek(mem_location) => collect_locals_expr(&mem_location.left, locals),
        ast::Expr::Poke {
            mem_location,
            value,
            ..
        } => {
            collect_locals_expr(&mem_location.left, locals);
            collect_locals_expr(value, locals);
        }
        ast::Expr::Variable { .. } | ast::Expr::I32Const(_) | ast::Expr::F32Const(_) => (),
        ast::Expr::UnaryOp { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::BinOp { left, right, .. } => {
            collect_locals_expr(left, locals);
            collect_locals_expr(right, locals);
        }
        ast::Expr::BranchIf { condition, .. } => collect_locals_expr(condition, locals),
        ast::Expr::LocalTee { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::Loop { block, .. } => collect_locals_expr(block, locals),
        ast::Expr::Cast { value, .. } => collect_locals_expr(value, locals),
        ast::Expr::FuncCall { params, .. } => {
            for param in params {
                collect_locals_expr(param, locals);
            }
        }
        ast::Expr::Select {
            condition,
            if_true,
            if_false,
            ..
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_expr(if_true, locals);
            collect_locals_expr(if_false, locals);
        }
        ast::Expr::If {
            condition,
            if_true,
            if_false,
        } => {
            collect_locals_expr(condition, locals);
            collect_locals_expr(if_true, locals);
            if let Some(if_false) = if_false {
                collect_locals_expr(if_false, locals);
            }
        }
        ast::Expr::Error => unreachable!(),
    }
}

fn mem_arg_for_location(mem_location: &ast::MemoryLocation) -> MemArg {
    let offset = if let ast::Expr::I32Const(v) = mem_location.right.expr {
        v as u32 as u64
    } else {
        unreachable!()
    };
    match mem_location.size {
        ast::MemSize::Byte => MemArg {
            align: 0,
            memory_index: 0,
            offset,
        },
        ast::MemSize::Word => MemArg {
            align: 2,
            memory_index: 0,
            offset,
        },
    }
}

fn emit_expression<'a>(ctx: &mut FunctionContext<'a>, expr: &'a ast::Expression) {
    match &expr.expr {
        ast::Expr::Block {
            statements,
            final_expression,
        } => {
            for stmt in statements {
                emit_expression(ctx, stmt);
                if stmt.type_.is_some() {
                    ctx.function.instruction(&Instruction::Drop);
                }
            }
            if let Some(ref expr) = final_expression {
                emit_expression(ctx, expr);
            }
        }
        ast::Expr::Let {
            value, name, defer, ..
        } => {
            if let Some(ref val) = value {
                if *defer {
                    ctx.deferred_inits.insert(name, val);
                } else {
                    emit_expression(ctx, val);
                    ctx.function
                        .instruction(&Instruction::LocalSet(*ctx.locals.get(name).unwrap()));
                }
            }
        }
        ast::Expr::Peek(mem_location) => {
            emit_expression(ctx, &mem_location.left);
            let mem_arg = mem_arg_for_location(mem_location);
            ctx.function.instruction(&match mem_location.size {
                ast::MemSize::Byte => Instruction::I32Load8_U(mem_arg),
                ast::MemSize::Word => Instruction::I32Load(mem_arg),
            });
        }
        ast::Expr::Poke {
            mem_location,
            value,
        } => {
            emit_expression(ctx, &mem_location.left);
            emit_expression(ctx, value);
            let mem_arg = mem_arg_for_location(mem_location);
            ctx.function.instruction(&match mem_location.size {
                ast::MemSize::Byte => Instruction::I32Store8(mem_arg),
                ast::MemSize::Word => Instruction::I32Store(mem_arg),
            });
        }
        ast::Expr::UnaryOp { op, value } => {
            use ast::Type::*;
            use ast::UnaryOp::*;
            match (value.type_.unwrap(), op) {
                (I32, Negate) => {
                    // TODO: try to improve this uglyness
                    ctx.function.instruction(&Instruction::I32Const(0));
                    emit_expression(ctx, value);
                    ctx.function.instruction(&Instruction::I32Sub);
                }
                _ => unreachable!(),
            };
        }
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

                (F32, Add) => Instruction::F32Add,
                (F32, Sub) => Instruction::F32Sub,
                (F32, Mul) => Instruction::F32Mul,
                (F32, Div) => Instruction::F32Div,
                (F32, Rem | And | Or | Xor) => unreachable!(),
                (F32, Eq) => Instruction::F32Eq,
                (F32, Ne) => Instruction::F32Neq,
                (F32, Lt) => Instruction::F32Lt,
                (F32, Le) => Instruction::F32Le,
                (F32, Gt) => Instruction::F32Gt,
                (F32, Ge) => Instruction::F32Ge,

                (I64, _) => todo!(),
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
                .find(|(_, l)| *l == label)
                .unwrap()
                .0;
            ctx.function.instruction(&Instruction::BrIf(depth as u32));
        }
        ast::Expr::I32Const(v) => {
            ctx.function.instruction(&Instruction::I32Const(*v));
        }
        ast::Expr::F32Const(v) => {
            ctx.function.instruction(&Instruction::F32Const(*v));
        }
        ast::Expr::LocalTee { name, value, .. } => {
            emit_expression(ctx, value);
            let index = ctx.locals.get(name).unwrap();
            ctx.function.instruction(&Instruction::LocalTee(*index));
        }
        ast::Expr::Loop { label, block, .. } => {
            ctx.labels.push(label.to_string());
            ctx.function
                .instruction(&Instruction::Loop(map_block_type(block.type_)));
            emit_expression(ctx, block);
            ctx.labels.pop();
            ctx.function.instruction(&Instruction::End);
        }
        ast::Expr::Variable(name) => {
            if let Some(index) = ctx.locals.get(name) {
                if let Some(expr) = ctx.deferred_inits.remove(name.as_str()) {
                    emit_expression(ctx, expr);
                    ctx.function.instruction(&Instruction::LocalTee(*index));
                } else {
                    ctx.function.instruction(&Instruction::LocalGet(*index));
                }
            } else if let Some(index) = ctx.globals.get(name.as_str()) {
                ctx.function.instruction(&Instruction::GlobalGet(*index));
            } else {
                unreachable!()
            }
        }
        ast::Expr::Cast { value, type_, .. } => {
            emit_expression(ctx, value);
            use ast::Type::*;
            let inst = match (value.type_.unwrap(), *type_) {
                (t1, t2) if t1 == t2 => None,
                (I32, F32) => Some(Instruction::F32ConvertI32S),
                (F32, I32) => Some(Instruction::I32TruncF32S),
                _ => todo!(),
            };
            if let Some(inst) = inst {
                ctx.function.instruction(&inst);
            }
        }
        ast::Expr::FuncCall { name, params, .. } => {
            let mut types = vec![];
            for param in params {
                types.push(param.type_.unwrap());
                emit_expression(ctx, param);
            }
            ctx.function
                .instruction(&builtin_function(name, &types).unwrap());
        }
        ast::Expr::Select {
            condition,
            if_true,
            if_false,
            ..
        } => {
            emit_expression(ctx, if_true);
            emit_expression(ctx, if_false);
            emit_expression(ctx, condition);
            ctx.function.instruction(&Instruction::Select);
        }
        ast::Expr::If {
            condition,
            if_true,
            if_false,
        } => {
            emit_expression(ctx, condition);
            ctx.function
                .instruction(&Instruction::If(map_block_type(expr.type_)));
            emit_expression(ctx, if_true);
            if if_true.type_.is_some() && if_true.type_ != expr.type_ {
                ctx.function.instruction(&Instruction::Drop);
            }
            if let Some(if_false) = if_false {
                ctx.function.instruction(&Instruction::Else);
                emit_expression(ctx, if_false);
                if if_false.type_.is_some() && if_false.type_ != expr.type_ {
                    ctx.function.instruction(&Instruction::Drop);
                }
            }
        }
        ast::Expr::Error => unreachable!(),
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

fn builtin_function(name: &str, params: &[ast::Type]) -> Option<Instruction<'static>> {
    use ast::Type::*;
    let inst = match (name, params) {
        ("sqrt", &[F32]) => Instruction::F32Sqrt,
        ("abs", &[F32]) => Instruction::F32Abs,
        _ => return None,
    };
    Some(inst)
}
