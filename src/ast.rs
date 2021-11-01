use crate::Span;

#[derive(Debug)]
pub struct Script {
    pub imports: Vec<Import>,
    pub global_vars: Vec<GlobalVar>,
    pub functions: Vec<Function>,
}

#[derive(Debug)]
pub enum TopLevelItem {
    Import(Import),
    GlobalVar(GlobalVar),
    Function(Function),
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
    // Function { name: String, params: Vec<Type>, result: Option<Type> }
}

#[derive(Debug)]
pub struct GlobalVar {
    pub span: Span,
    pub name: String,
    pub type_: Type,
}

#[derive(Debug)]
pub struct Function {
    pub span: Span,
    pub export: bool,
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub type_: Option<Type>,
    pub body: Expression,
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

#[derive(Debug)]
pub enum Expr {
    Block {
        statements: Vec<Expression>,
        final_expression: Option<Box<Expression>>,
    },
    I32Const(i32),
    F32Const(f32),
    Variable(String),
    Let {
        name: String,
        type_: Option<Type>,
        value: Option<Box<Expression>>,
        defer: bool,
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
    },
    LocalTee {
        name: String,
        value: Box<Expression>,
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
        if_false: Option<Box<Expression>>
    },
    Return {
        value: Option<Box<Expression>>
    },
    Error,
}

impl Expr {
    pub fn with_span(self, span: Span) -> Expression {
        Expression {
            type_: None,
            expr: self,
            span: span,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    Negate,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
    Lsl,
    Lsr,
    Asr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemSize {
    Byte,
    Word,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
}
