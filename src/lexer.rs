#![allow(dead_code)]

use std::ops::Range;

use logos::Logos;

use crate::errors::{MonkeyError, SpannedError};
use crate::types::{Position, Spanned};

#[derive(Logos, Debug, PartialEq)]
#[logos(skip r"[ \t\f]")]
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

    #[regex("[a-zA-Z][a-zA-Z0-9_]*")]
    Identifier,
    #[regex("[0-9]+")]
    Int,
    #[regex(r#""(?:[^"]|\\")*""#)]
    Str,

    #[regex("\n|\r\n")]
    NewLine,
}

pub struct _Token<'source> {
    kind: TokenKind,
    slice: &'source str,
}

impl<'source> _Token<'source> {
    pub fn new_token(lexer: &Lexer<'source>, kind: TokenKind) -> Token<'source> {
        let slice = lexer.current_slice();
        lexer.new_spanned_item(_Token { kind, slice })
    }
}

pub type Token<'source> = Spanned<_Token<'source>>;

pub struct Lexer<'source> {
    source: &'source str,
    lexer: logos::Lexer<'source, TokenKind>,
    row: usize,
    last_newline_pos: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        let lexer = TokenKind::lexer(source);

        Self {
            source,
            lexer,
            row: 0,
            last_newline_pos: 0,
        }
    }

    fn new_spanned_item<T>(&self, data: T) -> Spanned<T> {
        let span = self.lexer.span();
        let col_start = span.start - self.last_newline_pos;
        let col_end = span.end - self.last_newline_pos;

        let start = Position::new(self.row, col_start);
        let end = Position::new(self.row, col_end);

        Spanned::new(start, end, self.current_span(), data)
    }

    pub fn current_span(&self) -> Range<usize> {
        self.lexer.span().clone()
    }

    fn current_slice(&self) -> &'source str {
        self.lexer.slice()
    }
}

impl<'source> std::fmt::Debug for Token<'source> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token({:?}, {:?}, {})",
            self.kind,
            self.slice,
            self.pos_rng_str()
        )
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = Result<Token<'source>, SpannedError>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token_res) = self.lexer.next() {
            match token_res {
                Ok(TokenKind::NewLine) => {
                    self.row += 1;
                    self.last_newline_pos = self.lexer.span().end;
                    continue;
                }
                Ok(token) => return Some(Ok(_Token::new_token(self, token))),
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

    enum T<'source> {
        Token(Token<'source>),
        Error(SpannedError),
    }

    impl<'source> std::fmt::Debug for T<'source> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Self::Token(t) => write!(f, "{:?}", t),
                Self::Error(err) => write!(f, "{:?}", err),
            }
        }
    }

    fn debug_print(token_results: Vec<Result<Token<'_>, SpannedError>>) -> Vec<T> {
        let mut ret = Vec::new();
        for result in token_results {
            match result {
                Ok(token) => ret.push(T::Token(token)),
                Err(err) => ret.push(T::Error(err)),
            }
        }
        ret
    }

    macro_rules! snapshot {
        ($name:tt, $input:expr) => {
            #[test]
            fn $name() {
                let lexer = Lexer::new($input);
                let tokens: Vec<_> = lexer.collect();

                insta::with_settings!({
                    description => $input,
                }, {
                    insta::assert_debug_snapshot!(debug_print(tokens));
                })
            }
        };
    }

    snapshot!(check_span, "+\n+\n   +");
    snapshot!(simple_error, "@");
    snapshot!(simple_symbol, "+");
    snapshot!(more_symbols_1, "= + - *  / !");
    snapshot!(more_symbols_2, "==\n!=\n\t<>,;:");
    snapshot!(more_symbols_3, "(){}[]");
    snapshot!(keywords, "let\nfunction\nif\nelse\ntrue\nfalse\nreturn nil");
    snapshot!(identifiers, "x\nabc");
    snapshot!(ints, "123");
    snapshot!(str, "\"hello world 123\"");

    snapshot!(assignments1, "let five = 5;");
    snapshot!(assignments2, "let ten = 10;");
    snapshot!(
        assignments3,
        r"let add = fn(x, y) {
            x + y;
        };"
    );
    snapshot!(assignments4, "let result = add(five, ten);");

    snapshot!(operators, "!-/*5;");
    snapshot!(equality1, "5 < 10 > 5;");
    snapshot!(equality2, "10 == 10;");
    snapshot!(equality3, "10 != 9;");
    snapshot!(
        conditional,
        r"if (5 < 10) {
            return true;
        } else {
            return false;
        }"
    );

    snapshot!(arrays, "[1, 2]");
    snapshot!(dictionary, r#"{ "foo": "bar" };"#);
}
