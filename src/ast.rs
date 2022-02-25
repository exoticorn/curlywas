use std::{fmt, path::PathBuf};

use crate::parser::Span;

#[derive(Debug)]
pub struct Script {
    pub imports: Vec<Import>,
    pub global_vars: Vec<GlobalVar>,
    pub functions: Vec<Function>,
    pub data: Vec<Data>,
}

#[derive(Debug)]
pub enum TopLevelItem {
    Import(Import),
    GlobalVar(GlobalVar),
    Function(Function),
    Data(Data),
}

#[derive(Debug)]
pub struct Import {
    pub span: Span,
    pub import: String,
    pub type_: ImportType,
}

#[derive(Debug)]
pub enum ImportType {
    Memory(u32),
    Variable {
        name: String,
        type_: Type,
        mutable: bool,
    },
    Function {
        name: String,
        params: Vec<Type>,
        result: Option<Type>,
    },
}

#[derive(Debug)]
pub struct GlobalVar {
    pub span: Span,
    pub name: String,
    pub value: Expression,
    pub type_: Option<Type>,
    pub mutable: bool,
}

#[derive(Debug)]
pub struct Function {
    pub span: Span,
    pub export: bool,
    pub start: bool,
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub type_: Option<Type>,
    pub body: Expression,
    pub locals: Locals,
}

#[derive(Debug, Default)]
pub struct Locals {
    pub params: Vec<Local>,
    pub locals: Vec<Local>,
}

impl Locals {
    pub fn add_param(&mut self, span: Span, name: String, type_: Type) -> u32 {
        assert!(self.locals.is_empty());
        let id = self.params.len() as u32;
        self.params.push(Local {
            span,
            name,
            type_,
            index: Some(id),
        });
        id
    }

    pub fn add_local(&mut self, span: Span, name: String, type_: Type, store: bool) -> u32 {
        let id = (self.params.len() + self.locals.len()) as u32;
        self.locals.push(Local {
            span,
            name,
            type_,
            index: store.then(|| id),
        });
        id
    }
}

impl std::ops::Index<u32> for Locals {
    type Output = Local;
    fn index(&self, id: u32) -> &Local {
        let id = id as usize;
        if id < self.params.len() {
            &self.params[id]
        } else {
            &self.locals[id - self.params.len()]
        }
    }
}

impl std::ops::IndexMut<u32> for Locals {
    fn index_mut(&mut self, id: u32) -> &mut Local {
        let id = id as usize;
        if id < self.params.len() {
            &mut self.params[id]
        } else {
            &mut self.locals[id - self.params.len()]
        }
    }
}

#[derive(Debug)]
pub struct Local {
    pub span: Span,
    pub name: String,
    pub type_: Type,
    pub index: Option<u32>,
}

#[derive(Debug)]
pub struct Data {
    pub offset: Box<Expression>,
    pub data: Vec<DataValues>,
}

#[derive(Debug)]
pub enum DataValues {
    Array {
        type_: DataType,
        values: Vec<Expression>,
    },
    String(String),
    File {
        path: PathBuf,
        data: Vec<u8>,
    },
}

#[derive(Debug, Clone)]
pub enum DataType {
    I8,
    I16,
    I32,
    I64,
    F32,
    F64,
}

#[derive(Debug)]
pub struct MemoryLocation {
    pub span: Span,
    pub size: MemSize,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug)]
pub struct Expression {
    pub type_: Option<Type>,
    pub expr: Expr,
    pub span: Span,
}

impl Expression {
    pub fn const_i32(&self) -> i32 {
        match self.expr {
            Expr::I32Const(v) => v,
            _ => panic!("Expected I32Const"),
        }
    }

    pub fn const_i64(&self) -> i64 {
        match self.expr {
            Expr::I64Const(v) => v,
            _ => panic!("Expected I64Const"),
        }
    }

    pub fn const_f32(&self) -> f32 {
        match self.expr {
            Expr::F32Const(v) => v,
            _ => panic!("Expected F32Const"),
        }
    }

    pub fn const_f64(&self) -> f64 {
        match self.expr {
            Expr::F64Const(v) => v,
            _ => panic!("Expected F64Const"),
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Block {
        statements: Vec<Expression>,
        final_expression: Option<Box<Expression>>,
    },
    I32Const(i32),
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),
    Variable {
        name: String,
        local_id: Option<u32>,
    },
    Let {
        name: String,
        type_: Option<Type>,
        value: Option<Box<Expression>>,
        let_type: LetType,
        local_id: Option<u32>,
    },
    Poke {
        mem_location: MemoryLocation,
        value: Box<Expression>,
    },
    Peek(MemoryLocation),
    Loop {
        label: String,
        block: Box<Expression>,
    },
    LabelBlock {
        label: String,
        block: Box<Expression>,
    },
    Branch(String),
    BranchIf {
        condition: Box<Expression>,
        label: String,
    },
    UnaryOp {
        op: UnaryOp,
        value: Box<Expression>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Assign {
        name: String,
        value: Box<Expression>,
        local_id: Option<u32>,
    },
    LocalTee {
        name: String,
        value: Box<Expression>,
        local_id: Option<u32>,
    },
    Cast {
        value: Box<Expression>,
        type_: Type,
    },
    FuncCall {
        name: String,
        params: Vec<Expression>,
    },
    Select {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Box<Expression>,
    },
    If {
        condition: Box<Expression>,
        if_true: Box<Expression>,
        if_false: Option<Box<Expression>>,
    },
    Return {
        value: Option<Box<Expression>>,
    },
    First {
        value: Box<Expression>,
        drop: Box<Expression>,
    },
    Error,
}

impl Expr {
    pub fn with_span(self, span: Span) -> Expression {
        Expression {
            type_: None,
            expr: self,
            span,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LetType {
    Normal,
    Lazy,
    Inline,
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
    Not,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    DivU,
    Rem,
    RemU,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Gt,
    GtU,
    Ge,
    GeU,
    Lt,
    LtU,
    Le,
    LeU,
    Shl,
    ShrU,
    ShrS,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemSize {
    Byte,
    Word,
    Float
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
        }
    }
}
