
#![allow(dead_code)]

mod pat;
mod expr;

use crate::{
    Error,
    parser::{ ast, ast::Program },
};

pub fn run(mut program: Program) -> Result<(), Error> {

    let def = program.defs.pop();
    println!();

    if let Some(ast::Def::Raw(_, vec)) = def {
        let mut iter = vec.into_iter().peekable();
        let ast = pat::run(&mut iter, &program);
        match ast {
            Ok(o) => println!("{:?}", o),
            Err(e) => println!("{:?}", e),
        }
    }

    println!();
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
