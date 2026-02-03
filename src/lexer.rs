

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Literals
    IntLiteral(i64),
    BoolLiteral(bool),
    Ident(String),

    // keyword identifiers
    Int,
    Bool,
    Struct,
    Return,
    If,
    Else,
    While,
    Null,

    // operations
    Plus,
    Minus,
    Star,
    Slash,
    Eq,         // ==
    NotEq,      // !=
    Lt,         // <
    Gt,         // >
    And,        // &&
    Or,         // ||
    Not,        // !
    Assign,     // =

    // bitwise ops
    Ampersand,  // &
    Pipe,       // |
    Tilde,      // ~
    Caret,      // ^
    LShift,     // <<
    RShift,     // >>

    // delimiters
    LParen,     // (
    RParen,     // )
    LBrace,     // {
    RBrace,     // }
    Semicolon,  // ;
    Comma,      // ,
    Dot,        // .
    Colon,      // :

    // self explanatory
    EOF,
}

// lexer / tokenizer
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Lexer {
            input: input.chars().collect(),
            pos: 0,
        }
    }

    fn peek(&mut self) -> Option<char> {
        self.input.get(self.pos).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek();
        self.pos += 1;
        ch
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while self.pos < self.input.len() {
            while self.pos < self.input.len() && self.input[self.pos].is_whitespace() {
                self.advance();
            }

            let ch = match self.peek() {
                Some(c) => c,
                None => break,
            };

            // check for comments
            if ch == '/' && self.input.get(self.pos + 1) == Some(&'/') {
                while self.pos < self.input.len() && self.input[self.pos] != '\n' {
                    self.advance();
                }
                continue;
            }

            // check for numbers
            if ch.is_ascii_digit() {
                let mut num = String::new();
                while let Some(c) = self.peek() {
                    if c.is_ascii_digit() {
                        num.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }
                tokens.push(Token::IntLiteral(num.parse().unwrap()));
                continue
            }

            // check identifier
            if ch.is_alphabetic() {
                let mut word = String::new();
                while let Some(c) = self.peek() {
                    if c.is_alphanumeric() || c == '_' {
                        word.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }

                let token = match word.as_str() {
                    "int"    => Token::Int,
                    "bool"   => Token::Bool,
                    "struct" => Token::Struct,
                    "return" => Token::Return,
                    "if"     => Token::If,
                    "else"   => Token::Else,
                    "while"  => Token::While,
                    "null"   => Token::Null,
                    "true"   => Token::BoolLiteral(true),
                    "false"  => Token::BoolLiteral(false),
                    _        => Token::Ident(word),
                };
                tokens.push(token);
                continue;
            }

            // check double char tokens like == << && etc.
            if self.pos + 1  <self.input.len() {
                let next = self.input[self.pos + 1];
                let double_ch = match (ch, next) {
                    ('=', '=') => Some(Token::Eq),
                    ('!', '=') => Some(Token::NotEq),
                    ('&', '&') => Some(Token::And),
                    ('|', '|') => Some(Token::Or),
                    ('<', '<') => Some(Token::LShift),
                    ('>', '>') => Some(Token::RShift),
                    _          => None,
                };

                if let Some(token) = double_ch {
                    self.advance();
                    self.advance();
                    tokens.push(token);
                    continue;
                }
            }

            // all other tokens
            self.advance();
            let token = match ch {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Star,
                '/' => Token::Slash,
                '<' => Token::Lt,
                '>' => Token::Gt,
                '!' => Token::Not,
                '=' => Token::Assign,
                '&' => Token::Ampersand,
                '|' => Token::Pipe,
                '~' => Token::Tilde,
                '^' => Token::Caret,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                ';' => Token::Semicolon,
                ',' => Token::Comma,
                '.' => Token::Dot,
                ':' => Token::Colon,
                _   => panic!("Unexpected character: {}", ch),
            };
            tokens.push(token);
        }
        tokens.push(Token::EOF);
        tokens
    }
}