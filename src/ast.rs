
// top level program struct for ast
pub struct Program {
    pub structs: Vec<StructDef>,
    pub functions: Vec<FunctionDef>,
}

// structs for the programs
pub struct StructDef {
    pub name: String,
    pub fields: Vec<(String, Type)>,
}

// function definition
pub struct FunctionDef {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub return_type: Option<Type>,
    pub body: Vec<Statement>,
}

// just supporting int, bool, and struct for now
pub enum Type {
    Int,
    Bool,
    Struct(String),
}

// simple statements
pub enum Statement {
    // int x = 5;
    VarDec(Type, String, Expr),

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
}

// simple expressions
pub enum Expr {
    // literal int value
    IntLiteral(i64),

    // literal bool
    BoolLiteral(bool),
    
    // self explanatory
    Null,
    Identifer(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),

    // struct.val
    FieldAccess(Box<Expr>, String),

    // Struct(value: 1)
    StructInit(Stirng, Vec<(String, Expr)>),

    // foo(x, y)
    FunctionCall(String, Vec<Expr>),
}

// binary operations
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
}

// unary operations
pub enum UnaryOp {
    Not,
    Neg,
}



