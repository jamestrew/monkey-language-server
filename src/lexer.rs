#![allow(dead_code)]

use logos::Logos;

use crate::errors::{MonkeyError, SpannedError};
use crate::types::Spanned;

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \f]")]
pub enum TokenKind {
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    ForwardSlash,
    #[token("!")]
    Bang,

    #[token("==")]
    Equal,
    #[token("!=")]
    NotEqual,
    #[token("<")]
    LT,
    #[token(">")]
    GT,

    #[token(",")]
    Comma,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,

    // keywords
    #[token("let")]
    Let,
    #[token("function")]
    Function,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("return")]
    Return,
    #[token("nil")]
    Nil,

    #[regex("[a-zA-Z][a-zA-Z0-9_]+")]
    Identifier,
    #[regex("[0-9]")]
    Int,
    #[regex("\".*\"")]
    Str,

    #[regex("\n|\r\n")]
    NewLine,
}

pub type Token = Spanned<TokenKind>;

pub struct Lexer<'source> {
    source: &'source str,
    lexer: logos::Lexer<'source, TokenKind>,
    row_num: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        let lexer = TokenKind::lexer(source);

        Self {
            source,
            lexer,
            row_num: 0,
        }
    }

    fn new_spanned_item<T>(&self, spannee: T) -> Spanned<T> {
        Spanned::new(self.row_num, self.lexer.span().clone(), spannee)
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<Token, SpannedError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token_res) = self.lexer.next() {
            match token_res {
                Ok(TokenKind::NewLine) => {
                    self.row_num += 1;
                    continue;
                }
                Ok(token) => return Some(Ok(self.new_spanned_item(token))),
                Err(_) => {
                    let err = self
                        .new_spanned_item(MonkeyError::UnexpectedToken(self.lexer.slice().into()));
                    return Some(Err(err));
                }
            }
        }
        None
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
