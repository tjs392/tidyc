// top level program struct for ast
#[derive(Debug)]
pub struct Program {
    pub declarations: Vec<Declaration>,
}

// top level declarations
#[derive(Debug)]
pub enum Declaration {
    Function(FunctionDec),
    Variable(VarDec),
    Struct(StructDec),
    Union(UnionDec),
    Enum(EnumDec),
    Typedef(TypedefDec),
}

#[derive(Debug)]
pub struct UnionDec {
    pub name: Option<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug)]
pub struct EnumDec {
    pub name: Option<String>,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: String,
    pub value: Option<i64>,
}

#[derive(Debug)]
pub struct TypedefDec {
    pub name: String,
    pub typ: QualifiedType,
}

#[derive(Debug)]
pub struct VarDec {
    pub name: String,
    pub typ: QualifiedType,
    pub init: Option<Expr>,
    pub storage_class: StorageClass,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StorageClass {
    None,
    Static,
    Extern,
}

// structs for the programs
#[derive(Debug)]
pub struct StructDec {
    pub name: Option<String>,
    pub fields: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct StructField {
    pub name: String,
    pub typ: QualifiedType,
}

// function declaration
#[derive(Debug)]
pub struct FunctionDec {
    pub name: String,
    pub params: Vec<Param>,
    pub return_type: QualifiedType,
    pub body: Option<Vec<Statement>>,
    pub storage_class: StorageClass,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {

    // base
    Void,
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Float,
    Double,

    // signed and unsigned
    Signed(Box<Type>),
    Unsigned(Box<Type>),

    // derived types
    Pointer(Box<Type>), // int*, float*, etc.
    Array(Box<Type>, Option<usize>), // int[10] or int[],

    // type refs for easy parsing
    StructRef(String),
    UnionRef(String),
    EnumRef(String),
    TypedefRef(String),

    // user defined
    Struct {
        name: String,
        fields: Vec<(String, Type)>,
    },

    Union {
        name: String,
        fields: Vec<(String, Type)>,
    },

    Enum {
        name: String,
        variants: Vec<(String, Option<i64>)>,  // (name, optional_value)
    },

    Typedef {
        name: String,
        aliased_type: Box<Type>,
    },

    Function {
        params: Vec<Type>,
        return_type: Box<Type>,
    },
}

// derived type, just const for now
#[derive(Debug, Clone, PartialEq)]
pub struct QualifiedType {
    pub base: Type,
    pub is_const: bool,
}

// function parameter type (less ambigupus)
#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    pub name: Option<String>,
    pub typ: QualifiedType,
}

// simple statements

#[derive(Debug, Clone)]
pub enum Statement {
    // int x = 5;
    VarDec(QualifiedType, String, Option<Expr>, StorageClass),

    // x = 5;
    Assign(Expr, Expr),

    // return expr;
    Return(Expr),

    // if (expr) {body} (optional) else {body}
    If(Expr, Vec<Statement>, Option<Vec<Statement>>),

    // while (expr) {statements}
    While(Expr, Vec<Statement>),

    // foo(x)
    ExprStatement(Expr),

    CompoundAssign(CompoundOp, Box<Expr>, Box<Expr>),

    ReturnVoid,

    For(
        Option<Box<Statement>>, // init (int i = 0;)
        Option<Expr>,           // condition (i < 3;)
        Option<Expr>,           // increment (i++;)
        Vec<Statement>          // body
    ),

    Break,

    Continue,

    Switch(SwitchStmt),

    DoWhile(DoWhileStmt),

    Goto(String),

    Label(String, Box<Statement>),

    Block(Vec<Statement>), 
}

#[derive(Debug, Clone)]
pub struct SwitchStmt {
    pub expr: Expr,
    pub cases: Vec<Case>,
}

#[derive(Debug, Clone)]
pub struct Case {
    pub value: Option<Expr>,
    pub stmts: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct DoWhileStmt {
    pub body: Vec<Statement>,
    pub condition: Expr,
}

// simple expressions

#[derive(Debug, Clone)]
pub enum Expr {
    // literal int value
    IntLiteral(i64),

    // literal bool
    BoolLiteral(bool),
    FloatLiteral(f64),
    CharLiteral(char),
    StringLiteral(String),
    
    // self explanatory
    Null,
    Identifier(String),
    BinOp(Box<Expr>, BinOp, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),

    // struct.val
    FieldAccess(Box<Expr>, String),

    ArrayIndex(Box<Expr>, Box<Expr>),     // arr[i]
    Deref(Box<Expr>),                     // *ptr
    AddrOf(Box<Expr>),                     // &x
    PtrMember(Box<Expr>, String),          // ptr->field
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),  // cond ? a : b
    Cast(QualifiedType, Box<Expr>),         // (int)x
    SizeofType(QualifiedType),             // sizeof(int)
    SizeofExpr(Box<Expr>),                 // sizeof(x)

    Assign(Box<Expr>, Box<Expr>),                      // x = 5
    CompoundAssign(CompoundOp, Box<Expr>, Box<Expr>),  // x += 5
    
    Call(Box<Expr>, Vec<Expr>),  // can be (*fn_ptr)(args)
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

// compound assignments
#[derive(Debug, Clone, PartialEq)]
pub enum CompoundOp {
    AddAssign,    // +=
    SubAssign,    // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    AndAssign,    // &=
    OrAssign,     // |=
    XorAssign,    // ^=
    LShiftAssign, // <<=
    RShiftAssign, // >>=
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