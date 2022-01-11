
use std::{
    iter::Peekable as Peek,
    vec::IntoIter as VecIter,
};
use crate::{
    err, eat, eat_if, pattern,
    Error,
    lexer::Token,
    parser::{ ast, ast::Program },
};

pub fn run(iter: &mut Peek<VecIter<(usize, Token)>>, program: &Program) -> Result<ast::Expr, Error> {

    todo!()

}

macro_rules! lex {
    ($i: ident, $var: ident, $x: expr) => {
        ast::Expr::Lex($i, ast::Lexeme::$var($x))
    };
}

macro_rules! the_very_next_thing {
    () => {
        sub_level
    };
}

fn infix(
    iter: &mut Peek<VecIter<(usize, Token)>>,
    program: &Program,
    fixity: usize,
) -> Result<ast::Expr, Error> {

    use ast::Expr;
    use ast::Lexeme as Lex;

    if fixity == 10 { 
        return top_level(iter, program);
    }

    let left = infix(iter, program, fixity + 1)?;

    if let Some((i, op, assoc)) = next(iter, program, fixity)? {
        match assoc {
            ast::Assoc::Left => {

                let mut curr = infix(iter, program, fixity + 1)?;
                let mut out = left;

                while let Some((i, op, assoc)) = next(iter, program, fixity)? {
                    if let ast::Assoc::Left = assoc {
                        out = Expr::Op(i, Lex::ConsOp(op), Box::new(out), Box::new(curr));
                        curr = infix(iter, program, fixity + 1)?;
                    } else {
                        err!(i; "cannot mix left and non-left associative operators of the same precedence")?;
                    }
                }

                return Ok(Expr::Op(i, Lex::ConsOp(op), Box::new(out), Box::new(curr)));

            },
            ast::Assoc::Right => {

                fn rec(
                    iter: &mut Peek<VecIter<(usize, Token)>>, 
                    program: &Program,
                    fixity: usize,
                ) -> Result<Expr, Error> {

                    let curr = infix(iter, program, fixity + 1)?;

                    if let Some((i, op, assoc)) = next(iter, program, fixity)? {
                        if let ast::Assoc::Right = assoc {
                            let right = rec(iter, program, fixity)?;
                            return Ok(Expr::Op(i, Lex::ConsOp(op), Box::new(curr), Box::new(right)));
                        } else {
                            err!(i; "cannot mix right and non-right associative operators of the same precedence")?;
                        }
                    }
                    
                    Ok(curr)
                    
                }

                let right = rec(iter, program, fixity)?;

                return Ok(Expr::Op(i, Lex::ConsOp(op), Box::new(left), Box::new(right)));

            },
            ast::Assoc::None => {

                let right = infix(iter, program, fixity + 1)?;

                if next(iter, program, fixity)?.is_some() {
                    err!(i; "cannot mix non associative operators of the same precedence")?;
                }

                return Ok(Expr::Op(i, Lex::ConsOp(op), Box::new(left), Box::new(right)));
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

fn top_level(
    iter: &mut Peek<VecIter<(usize, Token)>>,
    program: &Program,
) -> Result<ast::Expr, Error> {

    if let Some((i, token)) = eat_if!(iter, ConsId, VarId) {

        macro_rules! ap {
            ($x: ident, $var: ident) => {{
                let mut vec = vec![lex!(i, $var, $x)];
                while let Some(v) = sub_level(iter, program)? {
                    vec.push(v);
                }
                if vec.len() == 1 { vec.pop().unwrap() } else { ast::Expr::Ap(i, vec) }
            }};
        }

        let out = match token {
            Token::ConsId(x) => ap!(x, Cons),
            Token::VarId(x) => ap!(x, Id),
            _ => unreachable!(),
        };

        Ok(out)

    } else { return Ok(pattern!(iter, program, sub_level)) }

}

fn sub_level(
    iter: &mut Peek<VecIter<(usize, Token)>>,
    program: &Program,
) -> Result<Option<ast::Expr>, Error> {

    use ast::Lit;
    let (i, token) = if let Some(x) = eat_if!(iter, ParenL, ConsId, VarId, Int, Frac, String) { x } else { return Ok(None) };
    let out = match token {

        Token::ParenL => match eat_if!(iter, VarOp, ParenR) {
            Some((i, Token::VarOp(x))) => {
                eat!(iter, ParenR);
                lex!(i, Op, x)
            },
            Some((_, Token::ParenR)) => lex!(i, Cons, String::from("()")),
            None => {
                let v = the_very_next_thing!()(iter, program)?.unwrap();
                eat!(iter, ParenR);
                v
            },
            _ => unreachable!(),
        }

        Token::ConsId(x) => lex!(i, Cons, x),
        Token::VarId(x) => lex!(i, Id, x),
        
        Token::Int(x) => lex!(i, Lit, Lit::Int(x)),
        Token::Frac(x) => lex!(i, Lit, Lit::Frac(x)),
        Token::String(x) => lex!(i, Lit, Lit::Str(x)),

        _ => unreachable!(),

    };

    Ok(Some(out))
    
}
