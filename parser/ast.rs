
use std::collections::HashMap;
use crate::lexer::Token;

#[derive(Debug)]
pub struct Program {

    pub module: Option<(String, Vec<String>)>,
    pub fixitys: Vec<HashMap<String, (usize, Assoc)>>,
    pub defs: Vec<Def>,

}

#[derive(Clone, Copy, Debug)]
pub enum Assoc { None, Left, Right }

/*
#[derive(Debug)]
pub enum TopDecl {

    Class,
    Data,
    Fixity,
    Import,
    Instance,
    Newtype,
    Type,

    Func(Func),

}
*/

#[derive(Debug)]
pub struct Block(usize, Vec<Stmt>);

#[derive(Debug)]
pub enum Stmt {
    Typedef,
    Def(Def),
}

#[derive(Debug)]
pub enum Def {
    Raw(usize, Vec<(usize, Token)>),
    Func(usize, String, Vec<Pat>, ExprExtra),
    Assign(usize, Pat, ExprExtra),
}

#[derive(Debug)]
pub struct Typesig(Vec<Typesig>);

#[derive(Debug)]
pub struct ExprExtra(usize, Expr, Option<Typesig>, Option<Block>);

#[derive(Debug)]
pub enum Expr {

    Lex(usize, Lexeme), // sub

    Ap(usize, Vec<Expr>), // mid
    Op(usize, Lexeme, Box<Expr>, Box<Expr>), // top
    PartialOpL(usize, Lexeme, Box<Expr>), // top
    PartialOpR(usize, Lexeme, Box<Expr>), // top

    IfStmt(usize, Box<Expr>, Box<Expr>, Box<Expr>), // sub
    LetStmt(usize, Block), // sub
    CaseOf(usize, Box<Expr>, Block), // sub

}

#[derive(Debug)]
pub enum Lexeme {

    Lit(Lit),
    Cons(String),
    ConsOp(String),
    Id(String),
    Op(String),

}

// #[derive(Debug)]
pub enum Pat {

    As(usize, String, Box<Pat>),
    Cons(usize, String, Vec<Pat>),
    ConsOp(usize, String, Box<Pat>, Box<Pat>),
    
    Id(usize, String),
    Op(usize, String),

    Lit(usize, Lit),

    Wildcard(usize),
    
}

impl std::fmt::Debug for Pat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Pat::*;
        match self {
            As(_, v, x) => write!(f, "(@ {} {:?})", v, *x),
            Cons(_, v, xs) => write!(f, "({} {:?})", v, xs),
            ConsOp(_, v, x, y) => write!(f, "({} {:?} {:?})", v, *x, *y),
            Id(_, v) => write!(f, "{}", v),
            Op(_, v) => write!(f, "({})", v),
            Lit(_, v) => write!(f, "{:?}", v),
            Wildcard(_) => f.write_str("_"),
        }
    }
}

#[derive(Debug)]
pub enum Lit {

    Int(String),
    Frac(String),
    Str(String),

}
