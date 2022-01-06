
#![allow(dead_code)]

use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;

use crate::Error;
use crate::err;
use crate::lexer::Token;
use crate::parser::ast;
use crate::eat;
use crate::eat_if;

// fn block(iter: &mut Peek<VecIter<(usize, Token)>>) -> Result<ast::Block, Error> {

//     let mut vec = Vec::new();

//     while eat_if!(iter, BraceR, EoF).is_none() {
//         if let Some(stmt) = stmt(iter)? {
//             vec.push(stmt);
//         }
//     }

//     Ok(Block::Raw(0, vec))

// }

fn pat_top(iter: &mut Peek<VecIter<(usize, Token)>>) -> Result<ast::Pat, Error> {

    if let Some((i, token)) = eat_if!(iter, ParenL, ConsId, VarId) {

        let out = match token {

            Token::ParenL => match eat_if!(iter, VarOp, ParenR) {

                Some((i, Token::VarOp(x))) => {
                    eat!(iter, ParenR);
                    ast::Pat::Op(i, x)
                },

                Some((_, Token::ParenR)) => {
                    ast::Pat::Cons(i, String::from("()"), Vec::new())
                }

                _ => {
                    let v = pat_top(iter)?;
                    eat!(iter, ParenR);
                    v
                },

            }

            Token::ConsId(x) => {
    
                let mut vec = Vec::new();

                while let Ok(v) = pat(iter) {
                    vec.push(v);
                }

                ast::Pat::Cons(i, x, vec)

            },

            Token::VarId(x) if eat_if!(iter, As).is_some() => ast::Pat::As(i, x, Box::new(pat(iter)?)),
            Token::VarId(x) => ast::Pat::Id(i, x),

            _ => unreachable!(),

        };

        Ok(out)

    } else { pat(iter) }

}

fn pat(iter: &mut Peek<VecIter<(usize, Token)>>) -> Result<ast::Pat, Error> {

    let (i, token) = eat!(iter, ParenL, ConsId, VarId, Wildcard, Int, Frac, String);
    let out = match token {

        Token::ParenL => if let Some((i, Token::VarOp(x))) = eat_if!(iter, VarOp) {
            eat!(iter, ParenR);
            ast::Pat::Op(i, x)
        } else {                
            let v = pat_top(iter)?;
            eat!(iter, ParenR);
            v
        },

        Token::VarId(x) if eat_if!(iter, As).is_some() => ast::Pat::As(i, x, Box::new(pat(iter)?)),
        Token::VarId(x) => ast::Pat::Id(i, x),

        Token::ConsId(x) => ast::Pat::Cons(i, x, Vec::new()),
        Token::Int(x) => ast::Pat::Lit(i, ast::Lit::Int(i, x)),
        Token::Frac(x) => ast::Pat::Lit(i, ast::Lit::Frac(i, x)),
        Token::String(x) => ast::Pat::Lit(i, ast::Lit::Str(i, x)),

        Token::Wildcard => ast::Pat::Wildcard(i),

        _ => unreachable!(),

    };

    Ok(out)

}
