
// top level program struct for ast
#[derive(Debug)]
pub struct Program {
    pub structs: Vec<StructDef>,
    pub functions: Vec<FunctionDef>,
}

// structs for the programs
#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

// function definition
#[derive(Debug)]
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

// just supporting int, bool, and struct for now

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Struct(String),
    Void,
}

// simple statements

#[derive(Debug, Clone)]
pub enum Statement {
    // int x = 5;
    VarDec(Type, String, Option<Expr>),

    // x = 5;
    Assign(String, Expr),

    // return expr;
    Return(Expr),

    // if (expr) {body} (optional) else {body}
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),

    // while (expr) {statements}
    While(Expr, Vec<Statement>),

    // foo(x)
    ExprStatement(Expr),

    CompoundAssign(String, BinOp, Expr),

    ReturnVoid,

    For(
        Option<Box<Statement>>, // init (int i = 0;)
        Option<Expr>,           // condition (i < 3;)
        Option<Expr>,           // increment (i++;)
        Vec<Statement>          // body
    ),

    Break,

    Continue,
}

// simple expressions

#[derive(Debug, Clone)]
pub enum Expr {
    // literal int value
    IntLiteral(i64),

    // literal bool
    BoolLiteral(bool),
    
    // self explanatory
    Null,
    Identifier(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),

    // struct.val
    FieldAccess(Box<Expr>, String),

    StructInit(String, Vec<Expr>),

    // foo(x, y)
    FunctionCall(String, Vec<Expr>),
}

// binary operations

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add, 
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    Gt,
    And,
    Or,

    BitAnd,
    BitOr,
    BitXor,
    LShift,
    RShift,

    Mod,
    Le,
    Ge,
}

// unary operations

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Not,
    Neg,
    BitNot,

    PreInc,
    PreDec,
    PostInc,
    PostDec,
}