use crate::stream::Stream;

pub fn lex(source: String) -> Vec<Token> {
    let mut tokens = vec![];

    let mut source = Stream::new(source.char_indices().collect());
    while let Some((pos, char)) = source.next() {
        use TokenData::*;
        let data = match char {
            '-' => Minus,
            '+' => Plus,
            '*' => Star,
            ';' => Semicolon,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '(' => OpenParen,
            ')' => CloseParen,

            '/' => {
                if source.next_if_char('/').is_some() {
                    // Comsume until end of line!
                    source.advance_while(|(_, c)| *c != '\n');
                    continue;
                } else {
                    Slash
                }
            }

            whitespace if whitespace.is_whitespace() => continue,
            unexpected_char => Error(TokenError::UnexpectedChar(unexpected_char)),
        };
    }

    tokens
}

#[derive(Clone, Debug)]
pub struct Token {
    data: TokenData,
    pos: Pos,
}

#[derive(Clone, Debug)]
pub struct Pos(usize);

#[derive(Clone, Debug)]
pub enum TokenData {
    Let,
    Semicolon,
    Equals,
    OpenBrace,
    CloseBrace,
    If,
    Else,
    Or,
    And,
    NotEquals,
    EqualsEquals,
    Less,
    LessEquals,
    Greater,
    GreaterEquals,
    Minus,
    Plus,
    Slash,
    Star,
    True,
    False,
    Number(f64),
    String(String),
    Identifier(String),
    OpenParen,
    CloseParen,

    Error(TokenError),
}

#[derive(Clone, Debug)]
pub enum TokenError {
    UnexpectedChar(char),
}

impl<T: Clone> Stream<(T, char)> {
    fn next_if_char(&mut self, expected: char) -> Option<(T, char)> {
        self.next_if(|(_, c)| *c == expected)
    }
}
