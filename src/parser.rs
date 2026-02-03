use crate::ast::*;
use crate::lexer::Token;

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser {
            tokens,
            pos: 0,
        }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    fn advance(&mut self) -> Token {
        let tok = self.tokens[self.pos].clone();
        self.pos += 1;
        tok
    }

    fn expect(&mut self, expected: &Token) -> Token {
        let token = self.advance();
        if &token != expected {
            panic!("Expected {:?}, got {:?}", expected, token);
        }
        token
    }

    pub fn parse_program(&mut self) -> Program {
        let mut structs = vec![];
        let mut functions = vec![];

        while *self.peek() != Token::EOF {
            match self.peek() {
                Token::Struct => structs.push(self.parse_struct()),
                _             => functions.push(self.parse_function())
            }
        }
        Program {
            structs,
            functions
        }
    }

    fn parse_struct(&mut self) -> StructDef {
        self.expect(&Token::Struct);
        
        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected struct name, got {:?}", other),
        };

        self.expect(&Token::LBrace);

        let mut fields = vec![];
        while *self.peek() != Token::RBrace {
            let field_type = self.parse_type();
            let field_name = match self.advance() {
                Token::Ident(n) => n,
                other => panic!("Expected field name, got {:?}", other),
            };
            self.expect(&Token::Semicolon);
            fields.push((field_name, field_type));
        }

        self.expect(&Token::RBrace);
        
        StructDef {
            name,
            fields,
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.advance() {
            Token::Int  => Type::Int,
            Token::Bool => Type::Bool,
            Token::Ident(name) => Type::Struct(name),
            other => panic!("Expected type, got {:?}", other),
        }
    }

    fn parse_function(&mut self) -> FunctionDef {
        let return_type = match self.peek() {
            Token::Null => {
                self.advance();
                None
            },
            _ => Some(self.parse_type()),
        };

        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected function name, got {:?}", other),
        };

        self.expect(&Token::LParen);
        let mut params = vec![];
        while *self.peek() != Token::RParen {
            let param_type = self.parse_type();
            let param_name = match self.advance() {
                Token::Ident(n) => n,
                other => panic!("Expected parameter name, got {:?}", other),
            };
            params.push((param_name, param_type));

            if *self.peek() == Token::Comma {
                self.advance();
            }
        }
        self.expect(&Token::RParen);

        let body = self.parse_block();

        FunctionDef { 
            name, 
            params, 
            return_type, 
            body 
        }
    }

    fn parse_block(&mut self) -> Vec<Statement> {
        self.expect(&Token::LBrace);
        let mut statements = vec![];
        while *self.peek() != Token::RBrace {
            statements.push(self.parse_statement());
        }
        self.expect(&Token::RBrace);
        statements
    }
}