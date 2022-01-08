
#![allow(dead_code)]
#![allow(unused_parens)]

pub mod ast;
mod top;
mod expr;

use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;

use crate::Error;
use crate::lexer::Token;

pub fn parse(iter: Peek<VecIter<(usize, Token)>>) -> Result<(), Error> {
// pub fn parse(iter: Peek<VecIter<(usize, Token)>>) -> Result<Vec<(usize, Token)>, Error> {

    top::run(iter)
        .and_then(expr::run)?;
    
    Ok(())

}

#[macro_export]
macro_rules! eat {

    (@string Int) => { String::from("integral number literal") };
    (@string Frac) => { String::from("fractional number literal") };
    (@string ConsId) => { String::from("uppercase identifier") };
    (@string VarId) => { String::from("identifier") };
    (@string ConsOp) => { String::from("constructor operator") };
    (@string VarOp) => { String::from("operator") };
    (@string String) => { String::from("string literal") };
    (@string $tok: ident) => { Token::$tok.to_string() };

    (@pat Int) => { Token::Int(_) };
    (@pat Frac) => { Token::Frac(_) };
    (@pat ConsId) => { Token::ConsId(_) };
    (@pat VarId) => { Token::VarId(_) };
    (@pat ConsOp) => { Token::ConsOp(_) };
    (@pat VarOp) => { Token::VarOp(_) };
    (@pat String) => { Token::String(_) };
    (@pat $tok: ident) => { Token::$tok };

    (@inner $iter: ident, $pattern: pat, $vec: expr) => {
        
        if let Some((i, token)) = $iter.next() {
            if let all @ $pattern = token {
                (i, all)
            } else {
                return Err((vec![i], format!(
                    "expected {}, got {}", 
                    $vec.join(" or "), 
                    token.to_string(),
                )))
            }
        } else { return err!(0; "unexpected end of input") }
        
    };

    ($iter: ident, $tok: ident) => {
        eat!(@inner $iter, eat!(@pat $tok), vec![eat!(@string $tok)])
    };

    ($iter: ident, $( $tok: ident ),*) => {
        eat!(@inner $iter, ($( eat!(@pat $tok) )|*), vec![$( eat!(@string $tok) ),*])
    };

}

#[macro_export]
macro_rules! eat_if {

    (@pat Int) => { Token::Int(_) };
    (@pat Frac) => { Token::Frac(_) };
    (@pat ConsId) => { Token::ConsId(_) };
    (@pat VarId) => { Token::VarId(_) };
    (@pat ConsOp) => { Token::ConsOp(_) };
    (@pat VarOp) => { Token::VarOp(_) };
    (@pat String) => { Token::String(_) };
    (@pat $tok: ident) => { Token::$tok };

    (@inner $iter: ident, $pattern: pat) => {
        $iter.next_if(|x| matches!(x, (_, $pattern)))
    };

    ($iter: ident, $tok: ident) => {
        eat_if!(@inner $iter, eat_if!(@pat $tok))
    };

    ($iter: ident, $( $tok: ident ),*) => {
        eat_if!(@inner $iter, ($( eat_if!(@pat $tok) )|*))
    };

}
