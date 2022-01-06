
use std::iter::Peekable as Peek;
use std::vec::IntoIter as VecIter;

use crate::Error;
use crate::lexer::lexeme::Token as LexToken;
use crate::lexer::Token;

pub fn run(mut iter: Peek<VecIter<(usize, LexToken, usize, bool)>>) -> Result<Vec<(usize, Token, usize, bool)>, Error> {

    let mut vec = Vec::new();

    while let Some((i, lexeme, indent, first)) = iter.next() {

        let token = match lexeme {

            LexToken::Number(mut val) => match iter.peek() {

                Some((_, LexToken::Symbol(dot), ..)) if dot == "." => {

                    iter.next();

                    if let Some((_, LexToken::Number(frac), ..)) = iter.peek() {
                        val.push('.');
                        val.push_str(&frac);
                        iter.next();
                        Token::Frac(val)
                    } else {
                        vec.push((i, Token::Int(val), indent, first));
                        Token::VarOp(String::from("."))
                    }

                }

                _ => Token::Int(val),

            },

            LexToken::Punc(val) => match val {

                '(' => Token::ParenL,
                ')' => Token::ParenR,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '[' => Token::BracketL,
                ']' => Token::BracketR,
                '{' => Token::BraceL,
                '}' => Token::BraceR,

                _ => unreachable!(),

            },

            LexToken::Symbol(val) => match &*val {

                ".." => Token::Range,
                "=" => Token::Assign,
                "\\" => Token::Lambda,
                "|" => Token::Pipe,
                "<-" => Token::ArrowL,
                "->" => Token::ArrowR,
                "@" => Token::As,
                "=>" => Token::Constraint,

                _ => Token::VarOp(val),

            },

            LexToken::ColSymbol(val) => match &*val {

                "::" => Token::Typedef,
                _ => Token::ConsOp(val),

            }

            LexToken::Lower(val) => match &*val {

                "case" => Token::Case,
                "class" => Token::Class,
                "data" => Token::Data,
                "deriving" => Token::Deriving,
                "do" => Token::Do,
                "else" => Token::Else,
                "if" => Token::If,
                "import" => Token::Import,
                "in" => Token::In,
                "infix" => Token::Infix,
                "infixl" => Token::InfixL,
                "infixr" => Token::InfixR,
                "instance" => Token::Instance,
                "let" => Token::Let,
                "module" => Token::Module,
                "newtype" => Token::Newtype,
                "of" => Token::Of,
                "then" => Token::Then,
                "type" => Token::Type,
                "where" => Token::Where,

                "_" => Token::Wildcard,

                _ => Token::VarId(val),

            },

            LexToken::Upper(val) => Token::ConsId(val),
            LexToken::String(val) => Token::String(val),
            LexToken::Char(val) => Token::Char(val),

        };

        vec.push((i, token, indent, first));

    }

    Ok(vec)

}
