use crate::{token::Token, types::Type};

#[derive(Debug, Clone)]
pub enum Stmt {
    BlockStmt(BlockStmt),
    StructStmt(StructStmt),
    ExprStmt(ExprStmt),
    FnStmt(FnStmt),
    ExternFnStmt(ExternFnStmt),
    IfStmt(IfStmt),
    ReturnStmt(ReturnStmt),
    LetStmt(LetStmt),
    WhileStmt(WhileStmt),
    ForStmt(ForStmt),
    TraitStmt(TraitStmt),
    EnumStmt(EnumStmt),
    BreakStmt(BreakStmt),
    AliasStmt(AliasStmt),
}

#[derive(Debug, Clone)]
pub enum Expr {
    AssignExpr(AssignExpr),
    BinaryExpr(BinaryExpr),
    CallExpr(CallExpr),
    GetExpr(GetExpr),
    GroupingExpr(GroupingExpr),
    LiteralExpr(LiteralExpr),
    LogicalExpr(LogicalExpr),
    SetExpr(SetExpr),
    UnaryExpr(UnaryExpr),
    VarExpr(VarExpr),
    LambdaExpr(LambdaExpr),
    TupleExpr(TupleExpr),
    StructInitializerExpr(StructInitializerExpr),
    ArrayInitializerExpr(ArrayInitializerExpr),
}

#[derive(Debug, Clone)]
pub struct Module {
    pub file_name: String,
    pub statements: Vec<Stmt>,
}

impl Module {
    pub fn new(file_name: String, statements: Vec<Stmt>) -> Self {
        Module {
            file_name,
            statements,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct StructStmt {
    pub name: Token,
    pub traits: Vec<Expr>,
    pub fields: Vec<(Token, Type)>,
    pub methods: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ExprStmt {
    pub expression: Expr,
}

#[derive(Debug, Clone)]
pub struct FnMeta {
    pub is_pub: bool,
    pub is_abstract: bool,
}

#[derive(Debug, Clone)]
pub struct FnStmt {
    pub name: Token,
    // name, type
    pub params: Vec<(Token, Type)>,
    pub return_type: Option<Type>,
    pub body: Box<Stmt>,
    pub meta: FnMeta,
}

#[derive(Debug, Clone)]
pub struct ExternFnStmt {
    pub name: Token,
    pub param_types: Vec<Type>,
    pub return_type: Type,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Box<Stmt>,
    pub else_branch: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct LetStmt {
    pub name: Token,
    pub type_: Option<Type>,
    pub initializer: Option<Expr>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub initializer: Box<Stmt>,
    pub condition: Expr,
    pub increment: Expr,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct TraitStmt {
    pub name: Token,
    pub methods: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct EnumStmt {
    pub name: Token,
    pub variants: Vec<(Token, Vec<(Token, Type)>)>,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub keyword: Token,
}

#[derive(Debug, Clone)]
pub struct AliasStmt {
    pub name: Token,
    pub aliased_type: Type,
}

#[derive(Debug, Clone)]
pub struct AssignExpr {
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct GetExpr {
    pub object: Box<Expr>,
    pub name: Token,
}

#[derive(Debug, Clone)]
pub struct GroupingExpr {
    pub expression: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub value: Token,
}

#[derive(Debug, Clone)]
pub struct LogicalExpr {
    pub left: Box<Expr>,
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct SetExpr {
    pub object: Box<Expr>,
    pub name: Token,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarExpr {
    pub name: Token,
}

#[derive(Debug, Clone)]
pub struct LambdaExpr {
    pub params: Vec<(Token, Type)>,
    pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct TupleExpr {
    pub values: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct StructInitializerExpr {
    pub name: Token,
    pub arguments: Vec<(Token, Expr)>,
}

#[derive(Debug, Clone)]
pub struct ArrayInitializerExpr {
    pub values: Vec<Expr>,
}
