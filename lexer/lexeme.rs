
use peeking_take_while::PeekableExt;
use std::iter::Enumerate as Enum;
use std::iter::Peekable as Peek;
use std::str::Chars;

use crate::Error;
use crate::err;

pub enum Token {

    Number(String),
    Upper(String),
    Lower(String),
    Symbol(String),
    ColSymbol(String),
    Char(char),
    String(String),

    Punc(char),

}

const SYMBOL_CHARS: [char; 20] = [
    '!', '#', '$', '%', '&', '*', '+', '.', '/', '<', '=', '>', '?', '@', '^', '|', '-', '~', '\\', ':'
];

pub fn run(mut iter: Peek<Enum<Chars>>) -> Result<Vec<(usize, Token, usize, bool)>, Error> {

    let mut vec = Vec::new();

    'outer: loop {

        let line_pos = if let Some((i, _)) = iter.peek() { *i } else { break };
        let mut first = true;

        while let Some((i, x)) = iter.next() {

            if x == '\n' { continue 'outer }
            if x.is_ascii_whitespace() { continue }

            macro_rules! lex {
                ($predicate: expr) => {
                    lex_string(&mut iter, x, $predicate)
                };
            }

            let indent = i - line_pos;
            let token = match x {

                '{' | '-' if matches!(iter.peek().map(|(_, x)| x), Some('-')) => match x {

                    '{' => {

                        loop {

                            iter.by_ref().take_while(|&(_, x)| x != '-').for_each(drop);

                            match iter.next() {

                                Some((_, '}')) => break,
                                None => return err!(i; "unterminated block comment at end of file"),
                                _ => (),

                            }

                        }

                        continue;

                    }

                    '-' => {
                        iter.by_ref().take_while(|&(_, x)| x != '\n').for_each(drop);
                        continue 'outer;
                    }

                    _ => unreachable!(),

                },

                '0'..='9' => Token::Number(lex!(|x| x.is_ascii_digit())),
                'A'..='Z' => Token::Upper(lex!(|&x| x.is_ascii_alphanumeric() || x == '_')),
                'a'..='z' | '_' => {

                    let token = lex!(|&x| x.is_ascii_alphanumeric() || x == '_');

                    match token.as_str() {

                        "do" | "let" | "of" | "where" => {
                            vec.push((i, Token::Lower(token), indent, first));
                            first = true;
                            continue;
                        },

                        _ => Token::Lower(token),

                    }
                    
                },

                _ if SYMBOL_CHARS.contains(&x) => {
                    let token = lex!(|x| SYMBOL_CHARS.contains(x));
                    first = first || token.as_str() == "|";
                    if x == ':' { Token::ColSymbol(token) } else { Token::Symbol(token) }
                },

                '(' | ')' | ',' | '[' | ']' /* | ';' | '{' | '}' */ => Token::Punc(x),
                '"' => Token::String(lex_quotes(i, &mut iter)?),
                '\'' => {

                    macro_rules! next { () => {
                        iter.next().ok_or(err!(@t i; "character literal unterminated"))?.1
                    }; }

                    let first = next!();
                    let out = if first == '\\' { next!() } else { first };

                    if next!() != '\'' {
                        return err!(i; "character literals can only be one character");
                    }

                    Token::Char(out)

                }
                _ => return err!(i; "character is unexpected or invalid"),

            };

            vec.push((i, token, indent, first));
            first = false;

        }

    }

    Ok(vec)
    
}

fn lex_string<F>(iter: &mut Peek<Enum<Chars>>, head: char, predicate: F) -> String
where F: Fn(&char) -> bool {

    let mut out = head.to_string();
    let tail: String = iter
        .peeking_take_while(|(_, x)| predicate(x))
        .map(|(_, x)| x)
        .collect();

    out.push_str(&tail);
    out

}

#[inline(always)]
fn lex_quotes(i: usize, iter: &mut Peek<Enum<Chars>>) -> Result<String, Error> {

    let mut out = String::new();

    loop {

        let span: String = iter
            .peeking_take_while(|&(_, x)| x != '"' && x != '\\')
            .map(|(_, x)| x)
            .collect();

        out.push_str(&span);

        match iter.next() {

            Some((_, '"')) => return Ok(out),
            Some((_, '\\')) => {

                if let Some((_, x)) = iter.next() {
                    out.push(x);
                } else {
                    return err!(i; "unterminated string literal at end of file");
                }

                continue;

            }
            _ => return err!(i; "unterminated string literal at end of file"),

        }

    }

}
