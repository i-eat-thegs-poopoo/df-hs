
use std::collections::HashMap;

use crate::lexer::Token;

#[derive(Debug)]
pub struct Program {

    pub module: Option<(String, Vec<String>)>,
    pub fixitys: Vec<HashMap<String, (usize, Assoc)>>,
    pub defs: Vec<Def>,

}

#[derive(Debug)]
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

    Lex(usize, Lexeme),

    Ap(usize, Vec<Expr>),
    Op(usize, Lexeme, Box<Expr>, Box<Expr>),
    PartialOpL(usize, Lexeme, Box<Expr>),
    PartialOpR(usize, Lexeme, Box<Expr>),

    Ternary(usize, Box<Expr>, Box<Expr>, Box<Expr>),
    LetStmt(usize, Block),
    CaseOf(usize, Box<Expr>, Block),

}

#[derive(Debug)]
pub enum Lexeme {

    Lit(usize, Lit),
    Id(usize, String),
    Op(usize, String),

}

#[derive(Debug)]
pub enum Pat {

    As(usize, String, Box<Pat>),
    Cons(usize, String, Vec<Pat>),
    ConsOp(usize, String, Box<Pat>, Box<Pat>),
    
    Id(usize, String),
    Op(usize, String),

    Lit(usize, Lit),

    Wildcard(usize),
    
}

#[derive(Debug)]
pub enum Lit {

    Int(usize, String),
    Frac(usize, String),
    Str(usize, String),

}
