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

            '=' => source.equals_variant(Equals, EqualsEquals),
            '!' => source.equals_variant(Bang, BangEquals),
            '<' => source.equals_variant(Less, LessEquals),
            '>' => source.equals_variant(Greater, GreaterEquals),

            '/' => {
                if source.next_if_char('/').is_some() {
                    // Comsume until end of line!
                    source.advance_while(|(_, c)| *c != '\n');
                    continue;
                } else {
                    Slash
                }
            }
            '"' => {
                let mut chars = vec![];
                while let Some((_, char)) = source.next_if(|(_, c)| *c != '"') {
                    chars.push(char)
                }
                let s = String::from_iter(chars);

                if source.next_if_char('"').is_some() {
                    Str(s)
                } else {
                    Error(TokenError::UnterminatedStringLiteral)
                }
            }
            digit if digit.is_ascii_digit() => {
                let mut digits = vec![digit];
                digits.append(
                    &mut source
                        .next_while(|(_, c)| c.is_ascii_digit())
                        .into_iter()
                        .map(|(_, c)| c)
                        .collect(),
                );
                if source.next_if_char('.').is_some() {
                    digits.push('.');
                    digits.append(
                        &mut source
                            .next_while(|(_, c)| c.is_ascii_digit())
                            .into_iter()
                            .map(|(_, c)| c)
                            .collect(),
                    );
                }

                let digits = String::from_iter(digits);
                let num = digits.parse().expect("parsed string is valid f64");
                Number(num)
            }

            whitespace if whitespace.is_whitespace() => continue,
            unexpected_char => Error(TokenError::UnexpectedChar(unexpected_char)),
        };

        let token = Token {
            data,
            pos: Pos(pos),
        };
        tokens.push(token);
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
    Bang,
    BangEquals,
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
    Str(String),
    Identifier(String),
    OpenParen,
    CloseParen,

    Error(TokenError),
}

#[derive(Clone, Debug)]
pub enum TokenError {
    UnexpectedChar(char),
    UnterminatedStringLiteral,
}

impl<T: Clone> Stream<(T, char)> {
    fn next_if_char(&mut self, expected: char) -> Option<(T, char)> {
        self.next_if(|(_, c)| *c == expected)
    }

    fn equals_variant(&mut self, no_eq: TokenData, eq: TokenData) -> TokenData {
        if self.next_if_char('=').is_some() {
            eq
        } else {
            no_eq
        }
    }
}
