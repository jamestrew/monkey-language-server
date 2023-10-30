use std::ops::Range;

use logos::Logos;

use crate::diagnostics::{MonkeyError, SpannedError};
use crate::parser::TokenProvider;
use crate::types::{Position, Spanned};

#[derive(Logos, Debug, PartialEq, Clone, Copy)]
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
    #[token("fn")]
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

impl AsRef<TokenKind> for TokenKind {
    fn as_ref(&self) -> &TokenKind {
        self
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Assign => write!(f, "="),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::ForwardSlash => write!(f, "/"),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::Equal => write!(f, "=="),
            TokenKind::NotEqual => write!(f, "!="),
            TokenKind::LT => write!(f, "<"),
            TokenKind::GT => write!(f, ">"),
            TokenKind::Comma => write!(f, ","),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::Colon => write!(f, ":"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::LBracket => write!(f, "["),
            TokenKind::RBracket => write!(f, "]"),
            TokenKind::Let => write!(f, "let"),
            TokenKind::Function => write!(f, "fn"),
            TokenKind::If => write!(f, "if"),
            TokenKind::Else => write!(f, "else"),
            TokenKind::True => write!(f, "true"),
            TokenKind::False => write!(f, "false"),
            TokenKind::Return => write!(f, "return"),
            TokenKind::Nil => write!(f, "nil"),
            TokenKind::Identifier => write!(f, "<identifier>"),
            TokenKind::Int => write!(f, "<int>"),
            TokenKind::Str => write!(f, "<str>"),
            TokenKind::NewLine => write!(f, r"\n"),
        }
    }
}

#[derive(PartialEq)]
pub struct _Token<'source> {
    pub kind: TokenKind,
    pub slice: &'source str,
}

impl<'source> _Token<'source> {
    #[cfg(test)]
    pub fn new(kind: TokenKind, slice: &'source str) -> _Token<'source> {
        Self { kind, slice }
    }

    pub fn from_lexer(lexer: &Lexer<'source>, kind: TokenKind) -> Token<'source> {
        let slice = lexer.current_slice();
        lexer.new_spanned_item(Self { kind, slice })
    }
}

pub type Token<'source> = Spanned<_Token<'source>>;
pub type TokenResult<'source> = Result<Token<'source>, SpannedError>;

pub fn token_result_span<T>(token_res: &TokenResult<'_>, data: T) -> Spanned<T> {
    match token_res {
        Ok(token) => token.map(data),
        Err(err) => err.map(data),
    }
}

pub struct Lexer<'source> {
    lexer: logos::Lexer<'source, TokenKind>,
    row: usize,
    last_newline_pos: usize,
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        let lexer = TokenKind::lexer(source);

        Self {
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
            "Token({:#?}, {:#?}, {})",
            self.kind,
            self.slice,
            self.pos_rng_str()
        )
    }
}

impl<'source> Iterator for Lexer<'source> {
    type Item = TokenResult<'source>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(token_res) = self.lexer.next() {
            match token_res {
                Ok(TokenKind::NewLine) => {
                    self.row += 1;
                    self.last_newline_pos = self.lexer.span().end;
                    continue;
                }
                Ok(token) => return Some(Ok(_Token::from_lexer(self, token))),
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

impl<'source> TokenProvider<'source> for Lexer<'source> {
    fn next(&mut self) -> Option<TokenResult<'source>> {
        Iterator::next(self)
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
    snapshot!(keywords, "let\nfn\nif\nelse\ntrue\nfalse\nreturn nil");
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
