
#![allow(dead_code)]

mod pat;

use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;

use crate::Error;
use crate::err;
use crate::lexer::Token;
use crate::parser::ast;
use crate::parser::ast::Program;
use crate::eat;
use crate::eat_if;

pub fn run(mut program: Program) -> Result<(), Error> {

    let def = program.defs.pop();

    if let Some(ast::Def::Raw(_, vec)) = def {
        let mut iter = vec.into_iter().peekable();
        let ast = pat::pat_op(&mut iter, &program, 0);
        match ast {
            Ok(o) => println!("{:?}", o),
            Err(e) => println!("{:?}", e),
        }
    }

    Ok(())

}

// fn block(iter: &mut Peek<VecIter<(usize, Token)>>) -> Result<ast::Block, Error> {

//     let mut vec = Vec::new();

//     while eat_if!(iter, BraceR, EoF).is_none() {
//         if let Some(stmt) = stmt(iter)? {
//             vec.push(stmt);
//         }
//     }

//     Ok(Block::Raw(0, vec))

// }

