use std::sync::atomic::{AtomicUsize, Ordering};

pub type Program = Vec<Stmt>;

#[derive(Clone, Debug)]
pub enum Stmt {
    Let(Binding),
    Expr(Expr),
}

#[derive(Clone, Debug)]
pub struct Binding {
    pub pattern: Pattern,
    pub arguments: Option<Vec<Pattern>>,
    pub value: Expr,
}
#[derive(Clone, Debug)]
pub struct Pattern(pub Identifier);

#[derive(Clone, Debug)]
pub enum Expr {
    Block(Block),
    Call(Call),
    If(Box<IfExpr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
    Literal(Literal),
    Identifier(Identifier),
}

#[derive(Clone, Debug)]
pub struct Block(pub Vec<Stmt>, pub Option<Box<Expr>>);

#[derive(Clone, Debug)]
pub struct Call {
    pub target: Box<Expr>,
    pub arguments: Vec<Expr>,
}

#[derive(Clone, Debug)]
pub struct IfExpr {
    pub condition: Expr,
    pub then_block: Block,
    pub else_block: Option<ElseBlock>,
}
#[derive(Clone, Debug)]
pub enum ElseBlock {
    ElseIf(Box<IfExpr>),
    Else(Block),
}

#[derive(Clone, Debug)]
pub struct BinaryExpr {
    pub lhs: Expr,
    pub op: BinaryOp,
    pub rhs: Expr,
}
#[derive(Clone, Debug)]
pub enum BinaryOp {
    Or,
    And,

    NotEq,
    Eq,

    Greater,
    GreaterEq,
    Less,
    LessEq,

    Subtract,
    Add,

    Divide,
    Multiply,
}

#[derive(Clone, Debug)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub rhs: Expr,
}
#[derive(Clone, Debug)]
pub enum UnaryOp {
    Not,
    Negate,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Bool(bool),
    Number(f64),
    Str(String),
    Nil,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Identifier {
    pub scope: Scope,
    pub name: String,
}
impl Identifier {
    pub fn new(scope: Scope, name: String) -> Self {
        Self { scope, name }
    }

    pub fn in_parent_scope(&mut self) -> Option<Self> {
        self.scope.parent.clone().map(|parent_scope| {
            *self = Identifier {
                scope: *parent_scope,
                name: self.name.clone(),
            };
            self.clone()
        })
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Scope {
    pub parent: Option<Box<Scope>>,
    id: ScopeId,
}
impl Scope {
    pub fn new() -> Self {
        let id = NEXT_SCOPE_ID.fetch_add(
            1,
            // Use Ordering::SeqCst b/c I'm not confident that anything else would work properly, and this guarentees it.
            Ordering::SeqCst,
        );
        Scope { parent: None, id }
    }

    pub fn nest(&self) -> Self {
        let mut scope = Scope::new();
        scope.parent = Some(Box::new(self.clone()));
        scope
    }
}

type ScopeId = usize;
static NEXT_SCOPE_ID: AtomicUsize = AtomicUsize::new(0);
