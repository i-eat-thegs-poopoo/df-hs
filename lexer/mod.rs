
mod lexeme;
mod reserved;
mod braces_copy;

use braces_copy as braces;

use std::{
    fmt::{ Display, Debug },
    iter::{ Enumerate as Enum, Peekable as Peek, once },
    str::Chars,
};
use crate::{
    lexer::{ lexeme::LexIter, reserved::ResIter },
    Error,
};

pub fn lex(iter: Peek<Enum<Chars>>) -> Result<Vec<(usize, Token)>, Error> {

    braces::run(
        ResIter::new(LexIter::new(iter)).chain(once(Ok((0, Token::EoF, 0, true)))).peekable()
    )

}

#[derive(PartialEq)]
pub enum Token {

    Int(String),
    Frac(String),
    ConsId(String),
    VarId(String),
    ConsOp(String),
    VarOp(String),
    Char(char),
    String(String),

    ParenL,
    ParenR,
    Comma,
    Semicolon,
    BracketL,
    BracketR,
    BraceL,
    BraceR,

    Case,
    Class,
    Data,
    Deriving,
    Do,
    Else,
    If,
    Import,
    In,
    Infix,
    InfixL,
    InfixR,
    Instance,
    Let,
    Module,
    Newtype,
    Of,
    Then,
    Type,
    Where,
    Range,
    Typedef,
    Assign,
    Lambda,
    Pipe,
    ArrowL,
    ArrowR,
    As,
    Constraint,

    Wildcard,

    EoF,

}

impl Display for Token {

    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {

        match self {

            Token::Int(x) => write!(f, "integral number literal {}", &x),
            Token::Frac(x) => write!(f, "fractional number literal {}", &x),
            Token::ConsId(x) => write!(f, "uppercase identifier `{}`", &x),
            Token::VarId(x) => write!(f, "identifier `{}`", &x),
            Token::ConsOp(x) => write!(f, "constructor operator `{}`", &x),
            Token::VarOp(x) => write!(f, "operator `{}`", &x),
            Token::Char(x) => write!(f, "character literal {:?}", &x),
            Token::String(x) => write!(f, "string literal {:?}", &x),

            Token::ParenL => f.write_str("opening parentheses"),
            Token::ParenR => f.write_str("closing parentheses"),
            Token::Comma => f.write_str("comma"),
            Token::Semicolon => f.write_str("newline"),
            Token::BracketL => f.write_str("opening bracket"),
            Token::BracketR => f.write_str("closing bracket"),
            Token::BraceL => f.write_str("block entrance"),
            Token::BraceR => f.write_str("block exit"),
            Token::Case => f.write_str("case statement"),
            Token::Class => f.write_str("class declaration"),
            Token::Data => f.write_str("data declaration"),
            Token::Deriving => f.write_str("deriving clause"),
            Token::Do => f.write_str("do statement"),
            Token::Else => f.write_str("else clause"),
            Token::If => f.write_str("if statement"),
            Token::Import => f.write_str("import statement"),
            Token::In => f.write_str("in clause"),
            Token::Infix => f.write_str("fixity declaration"),
            Token::InfixL => f.write_str("fixity declaration"),
            Token::InfixR => f.write_str("fixity declaration"),
            Token::Instance => f.write_str("instance declaration"),
            Token::Let => f.write_str("let statement"),
            Token::Module => f.write_str("module declaration"),
            Token::Newtype => f.write_str("newtype declaration"),
            Token::Of => f.write_str("of clause"),
            Token::Then => f.write_str("then clause"),
            Token::Type => f.write_str("type declaration"),
            Token::Where => f.write_str("where statement"),
            Token::Range => f.write_str("range"),
            Token::Typedef => f.write_str("type definition"),
            Token::Assign => f.write_str("assignment"),
            Token::Lambda => f.write_str("lambda declaration"),
            Token::Pipe => f.write_str("vertical pipe"),
            Token::ArrowL => f.write_str("back arrow"),
            Token::ArrowR => f.write_str("forward arrow"),
            Token::As => f.write_str("as pattern"),
            Token::Constraint => f.write_str("constraint"),
            Token::Wildcard => f.write_str("wildcard"),
            Token::EoF => f.write_str("end of file"),

        }

    }

}

impl Debug for Token {

    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {

        match self {

            Token::Int(x) => f.write_str(&x),
            Token::Frac(x) => f.write_str(&x),
            Token::ConsId(x) => f.write_str(&x),
            Token::VarId(x) => f.write_str(&x),
            Token::ConsOp(x) => f.write_str(&x),
            Token::VarOp(x) => f.write_str(&x),
            Token::Char(x) => f.write_str(&x.to_string()),
            Token::String(x) => f.write_str(&x),

            Token::ParenL => f.write_str("("),
            Token::ParenR => f.write_str(")"),
            Token::Comma => f.write_str(","),
            Token::Semicolon => f.write_str(";\n"),
            Token::BracketL => f.write_str("["),
            Token::BracketR => f.write_str("]"),
            Token::BraceL => f.write_str("{\n"),
            Token::BraceR => f.write_str("}"),
            Token::Case => f.write_str("case"),
            Token::Class => f.write_str("class"),
            Token::Data => f.write_str("data"),
            Token::Deriving => f.write_str("deriving"),
            Token::Do => f.write_str("do"),
            Token::Else => f.write_str("else"),
            Token::If => f.write_str("if"),
            Token::Import => f.write_str("import"),
            Token::In => f.write_str("in"),
            Token::Infix => f.write_str("infix"),
            Token::InfixL => f.write_str("infixl"),
            Token::InfixR => f.write_str("infixr"),
            Token::Instance => f.write_str("instance"),
            Token::Let => f.write_str("let"),
            Token::Module => f.write_str("module"),
            Token::Newtype => f.write_str("newtype"),
            Token::Of => f.write_str("of"),
            Token::Then => f.write_str("then"),
            Token::Type => f.write_str("type"),
            Token::Where => f.write_str("where"),
            Token::Range => f.write_str(".."),
            Token::Typedef => f.write_str("::"),
            Token::Assign => f.write_str("="),
            Token::Lambda => f.write_str("\\"),
            Token::Pipe => f.write_str("|"),
            Token::ArrowL => f.write_str("<-"),
            Token::ArrowR => f.write_str("->"),
            Token::As => f.write_str("@"),
            Token::Constraint => f.write_str("=>"),
            Token::Wildcard => f.write_str("_"),
            Token::EoF => f.write_str("EOF"),

        }

    }

}
