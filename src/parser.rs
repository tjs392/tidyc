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
        self.expect(&Token::Semicolon);
        
        StructDef {
            name,
            fields,
        }
    }

    fn parse_type(&mut self) -> Type {
        match self.advance() {
            Token::Int  => Type::Int,
            Token::Bool => Type::Bool,
            Token::Void => Type::Void,
            Token::Ident(name) => Type::Struct(name),
            other => panic!("Expected type, got {:?}", other),
        }
    }

    fn parse_function(&mut self) -> FunctionDef {
        let return_type = self.parse_type();

        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected function name, got {:?}", other),
        };

        self.expect(&Token::LParen);

        let mut params = vec![];

        if *self.peek() == Token::Void {
            self.advance();
            if *self.peek() != Token::RParen {
                panic!("Expected ')' after 'void' in parameter list");
            }
        } else if *self.peek() != Token::RParen {
            loop {
                let param_type = self.parse_type();
                let param_name = match self.advance() {
                    Token::Ident(n) => n,
                    other => panic!("Expected parameter name, got {:?}", other),
                };
                params.push((param_name, param_type));

                if *self.peek() == Token::Comma {
                    self.advance();
                } else {
                    break;
                }
            }
        }
        self.expect(&Token::RParen);

        let body = self.parse_block();

        FunctionDef { 
            name, 
            params, 
            return_type: Some(return_type), 
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

    fn parse_statement(&mut self) -> Statement {
        match self.peek() {

            Token::Int | Token::Bool | Token::Void => self.parse_var_decl(),

            Token::Ident(_) => {
                // checkpoint for tracking back
                let checkpoint = self.pos;
                let first = self.advance();
                
                match self.peek() {
                    Token::Ident(_) => {
                        // structtype varname = expr
                        let second_checkpoint = self.pos;
                        let second = self.advance();
                        
                        if matches!(self.peek(), Token::Assign | Token::Semicolon) {
                            //  structtpe varname = expr
                            self.pos = checkpoint;
                            self.parse_var_decl()
                        } else {
                            // not a declaration, backtrack and parse as expression orassignment
                            self.pos = checkpoint;
                            self.parse_assignment_or_expr()
                        }
                    },
                    Token::Assign | Token::PlusAssign | Token::MinusAssign | Token::StarAssign | Token::SlashAssign => {
                        self.pos = checkpoint;
                        self.parse_assignment_or_expr()
                    },
                    _ => {
                        self.pos = checkpoint;
                        let expr = self.parse_expression();
                        self.expect(&Token::Semicolon);
                        Statement::ExprStatement(expr)
                    }
                }
            },

            Token::Return => self.parse_return(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::For => self.parse_for(),
            
            Token::Break => {
                self.advance();
                self.expect(&Token::Semicolon);
                Statement::Break
            },
            
            Token::Continue => {
                self.advance();
                self.expect(&Token::Semicolon);
                Statement::Continue
            },

            _ => panic!("Unexpected token in statement: {:?}", self.peek()),
        }
    }

    fn parse_assignment_or_expr(&mut self) -> Statement {
        if let Token::Ident(name) = self.advance() {
            match self.peek() {
                Token::Assign => {
                    self.advance();
                    let expr = self.parse_expression();
                    self.expect(&Token::Semicolon);
                    Statement::Assign(name, expr)
                },
                Token::PlusAssign | Token::MinusAssign 
                | Token::StarAssign | Token::SlashAssign => {
                    let op_token = self.advance();
                    let expr = self.parse_expression();
                    self.expect(&Token::Semicolon);

                    let op = match op_token {
                        Token::PlusAssign => BinOp::Add,
                        Token::MinusAssign => BinOp::Sub,
                        Token::StarAssign => BinOp::Mul,
                        Token::SlashAssign => BinOp::Div,
                        _ => unreachable!(),
                    };

                    Statement::CompoundAssign(name, op, expr)
                },
                _ => {
                    self.pos -= 1;
                    let expr = self.parse_expression();
                    self.expect(&Token::Semicolon);
                    Statement::ExprStatement(expr)
                }
            }
        } else {
            panic!("Expected identifier");
        }
    }

    fn parse_var_decl(&mut self) -> Statement {
        let var_type = self.parse_type();
        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected variable name, got {:?}", other),
        };

        if *self.peek() == Token::Semicolon {
            self.advance();
            return Statement::VarDec(var_type, name, None);
        }

        self.expect(&Token::Assign);
        let expr = self.parse_expression();
        self.expect(&Token::Semicolon);

        Statement::VarDec(var_type, name, Some(expr))
    }

    fn parse_return(&mut self) -> Statement {
        self.expect(&Token::Return);

        if *self.peek() == Token::Semicolon {
            self.advance();
            return Statement::ReturnVoid;
        }

        let expr = self.parse_expression();
        self.expect(&Token::Semicolon);
        Statement::Return(expr)
    }

    fn parse_if(&mut self) -> Statement {
        self.expect(&Token::If);
        self.expect(&Token::LParen);
        let condition = self.parse_expression();
        self.expect(&Token::RParen);

        let then_block = self.parse_block();

        let else_block = if *self.peek() == Token::Else {
            self.advance();
            Some(self.parse_block())
        } else {
            None
        };

        Statement::If(condition, then_block, else_block)
    }

    fn parse_while(&mut self) -> Statement {
        self.expect(&Token::While);
        self.expect(&Token::LParen);

        let condition = self.parse_expression();

        self.expect(&Token::RParen);

        let body = self.parse_block();

        Statement::While(condition, body)
    }

    fn parse_for(&mut self) -> Statement {
        self.expect(&Token::For);
        self.expect(&Token::LParen);

        let init = if *self.peek() == Token::Semicolon {
            self.advance();
            None
        } else if matches!(self.peek(), Token::Int | Token::Bool | Token::Void) 
                || self.is_type_name() {
            Some(Box::new(self.parse_var_decl()))
        } else {
            let expr = self.parse_expression();
            self.expect(&Token::Semicolon);
            Some(Box::new(Statement::ExprStatement(expr)))
        };

        let condition = if *self.peek() == Token::Semicolon {
            None
        } else {
            Some(self.parse_expression())
        };
        self.expect(&Token::Semicolon);

        let increment = if *self.peek() == Token::RParen {
            None
        } else {
            Some(self.parse_expression())
        };
        self.expect(&Token::RParen);

        let body = self.parse_block();

        Statement::For(init, condition, increment, body)
    }

    fn is_type_name(&self) -> bool {
        matches!(self.peek(), Token::Ident(_))
    }

    // https://stackoverflow.com/questions/17369090/operator-precedence-table-for-the-c-programming-language
    // recursive decent parsing of expressions
    fn parse_expression(&mut self) -> Expr {
        self.parse_or()
    }

    fn parse_or(&mut self) -> Expr {
        let mut left = self.parse_and();
        while *self.peek() == Token::Or {
            self.advance();
            let right = self.parse_and();
            left = Expr::BinOp(Box::new(left), BinOp::Or, Box::new(right));
        }
        left
    }

    fn parse_and(&mut self) -> Expr {
        let mut left = self.parse_bitor();
        while *self.peek() == Token::And {
            self.advance();
            let right = self.parse_bitor();
            left = Expr::BinOp(Box::new(left), BinOp::And, Box::new(right));
        }
        left
    }

    fn parse_bitor(&mut self) -> Expr {
        let mut left = self.parse_bitxor();
        while *self.peek() == Token::Pipe {
            self.advance();
            let right = self.parse_bitxor();
            left = Expr::BinOp(Box::new(left), BinOp::BitOr, Box::new(right));
        }
        left
    }

    fn parse_bitxor(&mut self) -> Expr {
        let mut left = self.parse_bitand();
        while *self.peek() == Token::Caret {
            self.advance();
            let right = self.parse_bitand();
            left = Expr::BinOp(Box::new(left), BinOp::BitXor, Box::new(right));
        }
        left
    }

    fn parse_bitand(&mut self) -> Expr {
        let mut left = self.parse_equality();
        while *self.peek() == Token::Ampersand {
            self.advance();
            let right = self.parse_equality();
            left = Expr::BinOp(Box::new(left), BinOp::BitAnd, Box::new(right));
        }
        left
    }

    fn parse_equality(&mut self) -> Expr {
        let mut left = self.parse_comparison();
        loop {
            let op = match self.peek() {
                Token::Eq => BinOp::Eq,
                Token::NotEq => BinOp::NotEq,
                _ => break,
            };
            self.advance();
            let right = self.parse_comparison();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut left = self.parse_bitwise_shift();
        loop {
            let op = match self.peek() {
                Token::Lt => BinOp::Lt,
                Token::Gt => BinOp::Gt,
                Token::Le => BinOp::Le,
                Token::Ge => BinOp::Ge,
                _ => break,
            };
            self.advance();
            let right = self.parse_bitwise_shift();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_bitwise_shift(&mut self) -> Expr {
        let mut left = self.parse_additive();
        loop {
            let op = match self.peek() {
                Token::LShift => BinOp::LShift,
                Token::RShift => BinOp::RShift,
                _ => break,
            };
            self.advance();
            let right = self.parse_additive();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_additive(&mut self) -> Expr {
        let mut left = self.parse_multiplicative();
        loop {
            let op = match self.peek() {
                Token::Plus => BinOp::Add,
                Token::Minus => BinOp::Sub,
                _ => break,
            };
            self.advance();
            let right = self.parse_multiplicative();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_multiplicative(&mut self) -> Expr {
        let mut left = self.parse_unary();
        loop {
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_unary();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_unary(&mut self) -> Expr {
        match self.peek() {
            Token::Not => {
                self.advance();
                Expr::UnaryOp(UnaryOp::Not, Box::new(self.parse_unary()))
            },
            Token::Minus => {
                self.advance();
                Expr::UnaryOp(UnaryOp::Neg, Box::new(self.parse_unary()))
            },
            Token::Tilde => {
                self.advance();
                Expr::UnaryOp(UnaryOp::BitNot, Box::new(self.parse_unary()))
            },
            Token::PlusPlus => {
                self.advance();
                Expr::UnaryOp(UnaryOp::PreInc, Box::new(self.parse_unary()))
            },
            Token::MinusMinus => {
                self.advance();
                Expr::UnaryOp(UnaryOp::PreDec, Box::new(self.parse_unary()))
            },
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();
        loop {
            match self.peek() {
                Token::Dot => {
                    self.advance();
                    let field = match self.advance() {
                        Token::Ident(name) => name,
                        other => panic!("Expected field name after '.', got {:?}", other),
                    };
                    expr = Expr::FieldAccess(Box::new(expr), field);
                },
                Token::LParen => {
                    let name = match expr {
                        Expr::Identifier(name) => name,
                        _ => panic!("Can only call identifiers"),
                    };
                    self.advance();
                    
                    let mut args = vec![];
                    while *self.peek() != Token::RParen {
                        args.push(self.parse_expression());
                        if *self.peek() == Token::Comma {
                            self.advance();
                        }
                    }
                    self.expect(&Token::RParen);
                    expr = Expr::FunctionCall(name, args);
                },
                Token::PlusPlus => {
                    self.advance();
                    expr = Expr::UnaryOp(UnaryOp::PostInc, Box::new(expr));
                },
                Token::MinusMinus => {
                    self.advance();
                    expr = Expr::UnaryOp(UnaryOp::PostDec, Box::new(expr));
                },
                _ => break,
            }
        }
        expr
    }

    fn parse_primary(&mut self) -> Expr {
        match self.advance() {
            Token::IntLiteral(n) => Expr::IntLiteral(n),
            Token::BoolLiteral(b) => Expr::BoolLiteral(b),
            Token::Ident(name) => Expr::Identifier(name),

            Token::LParen => {
                let checkpoint = self.pos;

                if let Token::Ident(type_name) = self.peek() {
                    let type_name = type_name.clone();
                    self.advance();

                    if *self.peek() == Token::RParen {
                        self.advance();

                        if *self.peek() == Token::LBrace {
                            self.advance();

                            let mut values = vec![];
                            while *self.peek() != Token::RBrace {
                                values.push(self.parse_expression());
                                if *self.peek() == Token::Comma {
                                    self.advance();
                                }
                            }
                            self.expect(&Token::RBrace);

                            return Expr::StructInit(type_name, values);
                        }
                    }
                }

                self.pos = checkpoint;
                let expr = self.parse_expression();
                self.expect(&Token::RParen);
                expr
            },

            other => panic!("Unexpected token in primary expression: {:?}", other),
        }
    }
}