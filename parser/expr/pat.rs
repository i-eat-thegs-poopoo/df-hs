
use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;

use crate::Error;
use crate::err;
use crate::lexer::Token;
use crate::parser::ast;
use crate::parser::ast::Program;
use crate::eat;
use crate::eat_if;

pub fn pat_op(
    iter: &mut Peek<VecIter<(usize, Token)>>, 
    program: &Program,
    fixity: usize,
) -> Result<ast::Pat, Error> {

    if fixity == 10 { 
        return pat_top(iter, program);
    } 

    let left = pat_op(iter, program, fixity + 1)?;

    if let Some((i, op, assoc)) = next(iter, program, fixity)? {
        match assoc {
            ast::Assoc::Left => {

                let mut curr = pat_op(iter, program, fixity + 1)?;
                let mut out = left;

                while let Some((i, op, assoc)) = next(iter, program, fixity)? {
                    if let ast::Assoc::Left = assoc {
                        out = ast::Pat::ConsOp(i, op, Box::new(out), Box::new(curr));
                        curr = pat_op(iter, program, fixity + 1)?;
                    } else {
                        err!(i; "cannot mix left and non-left associative operators of the same precedence")?;
                    }
                }

                return Ok(ast::Pat::ConsOp(i, op, Box::new(out), Box::new(curr)));

            },
            ast::Assoc::Right => {

                fn rec(
                    iter: &mut Peek<VecIter<(usize, Token)>>, 
                    program: &Program,
                    fixity: usize,
                ) -> Result<ast::Pat, Error> {

                    let curr = pat_op(iter, program, fixity + 1)?;

                    if let Some((i, op, assoc)) = next(iter, program, fixity)? {
                        if let ast::Assoc::Right = assoc {
                            let right = rec(iter, program, fixity)?;
                            return Ok(ast::Pat::ConsOp(i, op, Box::new(curr), Box::new(right)));
                        } else {
                            err!(i; "cannot mix right and non-right associative operators of the same precedence")?;
                        }
                    }
                    
                    Ok(curr)
                    
                }

                let right = rec(iter, program, fixity)?;

                return Ok(ast::Pat::ConsOp(i, op, Box::new(left), Box::new(right)));

            },
            ast::Assoc::None => {

                let right = pat_op(iter, program, fixity + 1)?;

                if next(iter, program, fixity)?.is_some() {
                    err!(i; "cannot mix non associative operators of the same precedence")?;
                }

                return Ok(ast::Pat::ConsOp(i, op, Box::new(left), Box::new(right)));
            },
        }
    }

    Ok(left)

}

fn next(
    iter: &mut Peek<VecIter<(usize, Token)>>, 
    program: &Program,
    fixity: usize,
) -> Result<Option<(usize, String, ast::Assoc)>, Error> {

    if let Some((i, Token::ConsOp(op))) = iter.peek() {

        let assoc = program.fixitys
            .get(fixity)
            .ok_or(err!(@t *i; "something went wrong"))?
            .get(op)
            .map(|(_, x)| x);

        if let Some(assoc) = assoc {
            return match iter.next().unwrap() {
                (i, Token::ConsOp(x)) => Ok(Some((i, x, *assoc))),
                _ => unreachable!(),
            };
        }

    }

    Ok(None)

}

macro_rules! pattern {
    ($iter: ident, $program: ident) => {
        if let Some(v) = pat($iter, $program)? { v } 
        else { eat_if!($iter, ParenL, ConsId, VarId, Wildcard, Int, Frac, String); unreachable!() }
    };
}

fn pat_top(
    iter: &mut Peek<VecIter<(usize, Token)>>,
    program: &Program,
) -> Result<ast::Pat, Error> {

    if let Some((i, token)) = eat_if!(iter, ParenL, ConsId, VarId) {

        let out = match token {

            Token::ParenL => match eat_if!(iter, VarOp, ParenR) {

                Some((i, Token::VarOp(x))) => {
                    eat!(iter, ParenR);
                    ast::Pat::Op(i, x)
                },
                Some((_, Token::ParenR)) => ast::Pat::Cons(i, String::from("()"), Vec::new()),
                _ => {
                    let v = pat_op(iter, program, 0)?;
                    eat!(iter, ParenR);
                    v
                },

            }

            Token::ConsId(x) => {
    
                let mut vec = Vec::new();

                while let Some(v) = pat(iter, program)? {
                    vec.push(v);
                }

                ast::Pat::Cons(i, x, vec)

            },

            Token::VarId(x) if eat_if!(iter, As).is_some() => ast::Pat::As(i, x, Box::new(pattern!(iter, program))),
            Token::VarId(x) => ast::Pat::Id(i, x),

            _ => unreachable!(),

        };

        Ok(out)

    } else { return Ok(pattern!(iter, program)) }

}

fn pat(
    iter: &mut Peek<VecIter<(usize, Token)>>,
    program: &Program,
) -> Result<Option<ast::Pat>, Error> {

    let (i, token) = if let Some(x) = eat_if!(iter, ParenL, ConsId, VarId, Wildcard, Int, Frac, String) { x } else { return Ok(None) };
    let out = match token {

        Token::ParenL => if let Some((i, Token::VarOp(x))) = eat_if!(iter, VarOp) {
            eat!(iter, ParenR);
            ast::Pat::Op(i, x)
        } else {                
            let v = pat_op(iter, program, 0)?;
            eat!(iter, ParenR);
            v
        },

        Token::VarId(x) if eat_if!(iter, As).is_some() => ast::Pat::As(i, x, Box::new(pattern!(iter, program))),
        Token::VarId(x) => ast::Pat::Id(i, x),

        Token::ConsId(x) => ast::Pat::Cons(i, x, Vec::new()),
        Token::Int(x) => ast::Pat::Lit(i, ast::Lit::Int(i, x)),
        Token::Frac(x) => ast::Pat::Lit(i, ast::Lit::Frac(i, x)),
        Token::String(x) => ast::Pat::Lit(i, ast::Lit::Str(i, x)),

        Token::Wildcard => ast::Pat::Wildcard(i),

        _ => unreachable!(),

    };

    Ok(Some(out))

}
