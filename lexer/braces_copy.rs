
use std::iter::Peekable as Peek;
use crate::{
    bang, bang_ref,
    Error,
    TransposeRef,
    lexer::{ Token, reserved::ResIter },
};

pub fn run<'a, I: Iterator<Item=<ResIter<'a> as Iterator>::Item>>(mut iter: Peek<I>) -> Result<Vec<(usize, Token)>, Error> {

    let mut vec = Vec::new();
    let mut indents = Vec::new();

    impl_braces(0, &mut iter, &mut vec, &mut indents, false, false)?;

    Ok(vec)

}

fn parens<'a, I: Iterator<Item=<ResIter<'a> as Iterator>::Item>>(
    iter: &mut Peek<I>,
    vec: &mut Vec<(usize, Token)>,
) -> Result<(), Error> {

    let mut indents = Vec::new();
    impl_braces(0, iter, vec, &mut indents, false, false)?;

    if let Some((i, token, _, _)) = iter.next().transpose()? {
        vec.push((i, token));
    }

    Ok(())

}

fn impl_braces<'a, I: Iterator<Item=<ResIter<'a> as Iterator>::Item>>(
    base: usize,
    iter: &mut Peek<I>,
    vec: &mut Vec<(usize, Token)>,
    indents: &mut Vec<usize>,
    let_expr: bool,
    where_clause: bool,
) -> Result<Option<usize>, Error> {

    let mut guards = true;

    if let Some(val) = exit(base, iter, vec, indents, let_expr, where_clause, guards) {
        return val;
    }

    while let Some((i, token, indent, first)) = iter.next().transpose()? {

        if first && indent == base {
            guards = true;
            if !matches!(vec.last(), Some((_, Token::BraceL))) {
                vec.push((0, Token::Semicolon));
            }
        }

        let paren = matches!(token, Token::ParenL);
        let block = matches!(token, Token::Do | Token::Let | Token::Of | Token::Where);
        let is_let = token == Token::Let;
        let isnt_where = token != Token::Where;

        guards = guards && !matches!(token, Token::Assign | Token::ArrowR);
        vec.push((i, token));

        if paren {
            parens(iter, vec)?;
        }

        if block {

            vec.push((0, Token::BraceL));
            indents.push(base);

            let let_in = if let Some((_, _, indent, _)) = iter.peek().transpose()? {
                impl_braces(*indent, iter, vec, indents, is_let, isnt_where)?
            } else { None };

            vec.push((0, Token::Semicolon));
            vec.push((0, Token::BraceR));
            indents.pop();

            if let Some(i) = let_in {
                vec.push((i, Token::In));
            }
            
        }

        if let Some(val) = exit(base, iter, vec, indents, let_expr, where_clause, guards) {
            return val;
        }

    }

    Ok(None)

}

fn exit<'a, I: Iterator<Item=<ResIter<'a> as Iterator>::Item>>(
    base: usize,
    iter: &mut Peek<I>,
    vec: &mut Vec<(usize, Token)>,
    indents: &mut Vec<usize>,
    let_expr: bool,
    where_clause: bool,
    guards: bool,
) -> Option<Result<Option<usize>, Error>> {

    if let Some(x) = iter.peek() {

        let (_, token, indent, first) = bang_ref!(x);

        match token {

            Token::ParenR => return Some(Ok(None)),
            Token::In if let_expr => return Some(Ok(Some(bang!(iter.next().unwrap()).0))),
            Token::In => return Some(Ok(None)),
            Token::Where if where_clause => return Some(Ok(None)),

            Token::Pipe if guards && !indents.contains(indent) && indent != &base => {

                vec.push((0, Token::BraceL));
                indents.push(base);

                let let_in = if let Some(x) = iter.peek() {
                    
                    let (_, _, indent, _) = bang_ref!(x);

                    match impl_braces(*indent, iter, vec, indents, false, true) {
                        Ok(o) => o,
                        Err(e) => return Some(Err(e)),
                    }

                } else { None };

                vec.push((0, Token::Semicolon));
                vec.push((0, Token::BraceR));
                indents.pop();

                if let Some(i) = let_in {
                    vec.push((i, Token::In));
                }

                if let Some(val) = exit(base, iter, vec, indents, let_expr, where_clause, guards) {
                    return Some(val);
                }

            },
            
            _ if indents.contains(indent) && *first => return Some(Ok(None)),
            _ => (),

        }

    }

    None

}
