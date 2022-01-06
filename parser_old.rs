
#![allow(dead_code)]

use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;

use crate::lexer::Token;

#[derive(Debug)]
pub enum SyntaxTree {

    NumLit(String),
    StrLit(String),
    Apply(Vec<SyntaxTree>),
    Op(String, Box<SyntaxTree>, Box<SyntaxTree>),

}

pub fn parse(iter: Peek<VecIter<(usize, Token)>>) -> Result<Vec<(usize, Token)>, (usize, String)> {

    Ok(iter)
        .map(|x| x.collect())

}

// mod parse {

//     use std::iter::Peekable as Peek;
//     use std::vec::IntoIter as VecIter;

//     use crate::lexer::Token;
    
//     pub fn run(mut iter: Peek<VecIter<(usize, Token)>>) {

//     }

// }
