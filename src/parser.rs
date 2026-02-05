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
        let mut declarations = vec![];

        while *self.peek() != Token::EOF {
            declarations.push(self.parse_declaration());
        }

        Program { declarations }
    }

    fn parse_declaration(&mut self) -> Declaration {
        match self.peek() {
            Token::Struct => Declaration::Struct(self.parse_struct()),
            Token::Union => Declaration::Union(self.parse_union()),
            Token::Enum => Declaration::Enum(self.parse_enum()),
            Token::Typedef => Declaration::Typedef(self.parse_typedef()),
            _ => {
                self.parse_function_or_variable()
            }
        }
    }

    fn parse_function_or_variable(&mut self) -> Declaration {
        let storage_class = self.parse_storage_class();
        let qualified_type = self.parse_qualified_type();

        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("expected identifier, got {:?}", other),
        };

        match self.peek() {
            // function dec
            Token::LParen => {
                self.finish_parse_function(storage_class, qualified_type, name)
            }

            // either assignment or global variable dec
            Token::Semicolon | Token::Assign | Token::LBracket => {
                let mut typ = qualified_type;
                
                // int arr[10];
                while *self.peek() == Token::LBracket {
                    self.advance();
                    let size = if *self.peek() == Token::RBracket {
                        None
                    } else {
                        match self.advance() {
                            Token::IntLiteral(n) => Some(n as usize),
                            other => panic!("Expected array size, got {:?}", other),
                        }
                    };
                    self.expect(&Token::RBracket);
                    typ = QualifiedType {
                        base: Type::Array(Box::new(typ.base), size),
                        is_const: typ.is_const,
                    };
                }
                
                let init = if *self.peek() == Token::Assign {
                    self.advance();
                    Some(self.parse_expression())
                } else {
                    None
                };
                self.expect(&Token::Semicolon);

                Declaration::Variable(VarDec {
                    name,
                    typ,
                    init,
                    storage_class 
                })
            }
            other => panic!("Expected '(' or ';' after identifier, got {:?}", other),
        }
    }

    fn finish_parse_function(&mut self, storage_class: StorageClass, return_type: QualifiedType, name: String) -> Declaration {
        self.expect(&Token::LParen);

        let mut params = vec![];

        if *self.peek() == Token::Void {
            let checkpoint = self.pos;
            self.advance();
            if *self.peek() == Token::RParen {
                self.advance();
            } else {
                self.pos = checkpoint;
                params = self.parse_parameter_list();
                self.expect(&Token::RParen);
            }
        } else if *self.peek() != Token::RParen {
            params = self.parse_parameter_list();
            self.expect(&Token::RParen);
        } else {
            self.advance();
        }

        let body = if *self.peek() == Token::LBrace {
            Some(self.parse_block())
        } else {
            self.expect(&Token::Semicolon);
            None
        };

        Declaration::Function(FunctionDec {
            name,
            params,
            return_type,
            body,
            storage_class,
        })
    }

    fn parse_parameter_list(&mut self) -> Vec<Param> {
        let mut params = vec![];

        loop {
            let typ = self.parse_qualified_type();
            let name = match self.peek() {
                Token::Ident(n) => {
                    let name = n.clone();
                    self.advance();
                    Some(name)
                }
                
                // param can be unnamed in func dec
                _ => None, 
            };

            params.push(Param { name, typ });

            if *self.peek() == Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        params
    }

    fn parse_struct(&mut self) -> StructDec {
        self.expect(&Token::Struct);

        let name = match self.peek() {
            Token::Ident(n) => {
                let name = n.clone();
                self.advance();
                Some(name)
            }

            _ => None,
        };

        self.expect(&Token::LBrace);

        let mut fields = vec![];
        while *self.peek() != Token::RBrace {
            let field_type = self.parse_qualified_type();
            let field_name = match self.advance() {
                Token::Ident(n) => n,
                other => panic!("Expected field name, got {:?}", other)
            };
            self.expect(&Token::Semicolon);
            fields.push(StructField {
                name: field_name,
                typ: field_type,
            });
        }

        self.expect(&Token::RBrace);
        self.expect(&Token::Semicolon);

        StructDec { name, fields }
    }

    fn parse_enum(&mut self) -> EnumDec {
        self.expect(&Token::Enum);

        let name = match self.peek() {
            Token::Ident(n) => {
                let name = n.clone();
                self.advance();
                Some(name)
            }

            _ => None,
        };

        self.expect(&Token::LBrace);
        let mut variants = vec![];
        let mut next_value = 0i64;

        while *self.peek() != Token::RBrace {
            let variant_name = match self.advance() {
                Token::Ident(n) => n,
                other => panic!("Expected enum variant name, got {:?}", other),
            };

            let value = if *self.peek() == Token::Assign {
                self.advance();
                match self.advance() {
                    Token::IntLiteral(n) => {
                        next_value = n + 1;
                        Some(n)
                    }
                    other => panic!("Expected integer in enum, got {:?}", other),
                }
            } else {
                let v = next_value;
                next_value += 1;
                Some(v)
            };

            variants.push(EnumVariant {
                name: variant_name,
                value,
            });

            if *self.peek() == Token::Comma {
                self.advance();
            } else {
                break;
            }
        }

        self.expect(&Token::RBrace);
        self.expect(&Token::Semicolon);

        EnumDec { name, variants }
    }

    fn parse_union(&mut self) -> UnionDec {
        self.expect(&Token::Union);

        let name = match self.peek() {
            Token::Ident(n) => {
                let name = n.clone();
                self.advance();
                Some(name)
            }
            _ => None,
        };

        self.expect(&Token::LBrace);

        let mut fields = vec![];
        while *self.peek() != Token::RBrace {
            let field_type = self.parse_qualified_type();
            let field_name = match self.advance() {
                Token::Ident(n) => n,
                other => panic!("Expected field name, got {:?}", other),
            };
            self.expect(&Token::Semicolon);
            fields.push(StructField {
                name: field_name,
                typ: field_type,
            });
        }

        self.expect(&Token::RBrace);
        self.expect(&Token::Semicolon);

        UnionDec { name, fields }
    }

    fn parse_typedef(&mut self) -> TypedefDec {
        self.expect(&Token::Typedef);
        let typ = self.parse_qualified_type();
        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected typedef name got {:?}", other),
        };
        self.expect(&Token::Semicolon);

        TypedefDec { name, typ }
    }

    // just handling static and extern for now
    fn parse_storage_class(&mut self) -> StorageClass {
        match self.peek() {
            Token::Static => {
                self.advance();
                StorageClass::Static
            }
            Token::Extern => {
                self.advance();
                StorageClass::Extern
            }
            _ => StorageClass::None
        }
    }

    // just allowing const for now
    fn parse_qualified_type(&mut self) -> QualifiedType {
        let is_const = if *self.peek() == Token::Const {
            self.advance();
            true
        } else {
            false
        };

        let base = self.parse_pointer_type();

        QualifiedType { base, is_const }
    }

    // int* arr[10] array of 10 pts
    // int (*ptr)[10]; 1 pointer to beginning
    fn parse_pointer_type(&mut self) -> Type {
        let mut base_type = self.parse_base_type();

        while *self.peek() == Token::Star {
            self.advance();
            base_type = Type::Pointer(Box::new(base_type));
        }

        while *self.peek() == Token::LBracket {
            self.advance();
            let size = if *self.peek() == Token::RBracket {
                None
            } else {
                match self.advance() {
                    Token::IntLiteral(n) => Some(n as usize),
                    other => panic!("Expected array size got {:?}", other),
                }
            };

            self.expect(&Token::RBracket);
            base_type = Type::Array(Box::new(base_type), size);
        }

        base_type
    }

    fn parse_base_type(&mut self) -> Type {
        let is_signed = match self.peek() {
            Token::Signed => {
                self.advance();
                Some(true)
            }
            Token::Unsigned => {
                self.advance();
                Some(false)
            }
            _ => None,
        };

        let base = match self.advance() {
            Token::Void => Type::Void,
            Token::Char => Type::Char,
            Token::Short => Type::Short,
            Token::Int => Type::Int,
            Token::Long => {
                // checking for long long
                if *self.peek() == Token::Long {
                    self.advance();
                    Type::LongLong
                } else {
                    Type::Long
                }
            }
            Token::Float => Type::Float,
            Token::Double => Type::Double,
            Token::Bool => Type::Int, // just mapping bool to int rn, TODO: CHANGE
            Token::Struct => {
                let name = match self.advance() {
                    Token::Ident(n) => n,
                    other => panic!("expected struct name, got {:?}", other),
                };
                Type::StructRef(name)
            }
            Token::Union => {
                let name = match self.advance() {
                    Token::Ident(n) => n,
                    other => panic!("Expected union name, got {:?}", other),
                };
                Type::UnionRef(name)
            }
            Token::Enum => {
                let name = match self.advance() {
                    Token::Ident(n) => n,
                    other => panic!("Expected enum name, got {:?}", other),
                };
                Type::EnumRef(name)
            }
            // chec for typedef types
            Token::Ident(name) => Type::TypedefRef(name),
            other => panic!("Expected type, got {:?}", other),
        };

        if let Some(signed) = is_signed {
            if signed {
                Type::Signed(Box::new(base))
            } else {
                Type::Unsigned(Box::new(base))
            }
        } else {
            base
        }
    }

    // STATEMENT PARSING

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

            // checking for type keyword, this is var dec
            Token::Int | Token::Char | Token::Short | Token::Long | Token::Float | 
            Token::Double | Token::Void | Token::Bool | Token::Signed | Token::Unsigned | 
            Token::Const | Token::Static | Token::Extern 
            => self.parse_local_var_dec(),

            // struct, union, enum
            Token::Struct | Token::Union | Token::Enum => {
                let checkpoint = self.pos;
                self.advance();
                if matches!(self.peek(), Token::Ident(_)) {
                    self.advance();
                    if matches!(self.peek(), Token::Ident(_) | Token::Star) {
                        // struct structname varname = variable dec
                        self.pos = checkpoint;
                        self.parse_local_var_dec()
                    } else {
                        // not a dec
                        self.pos = checkpoint;
                        let expr = self.parse_expression();
                        self.expect(&Token::Semicolon);
                        Statement::ExprStatement(expr)
                    }
                } else {
                    self.pos = checkpoint;
                    let expr = self.parse_expression();
                    self.expect(&Token::Semicolon);
                    Statement::ExprStatement(expr)
                }
            }

            Token::Return => self.parse_return(),
            Token::If => self.parse_if(),
            Token::While => self.parse_while(),
            Token::Do => self.parse_do_while(),
            Token::For => self.parse_for_loop(),
            Token::Switch => self.parse_switch(),
            Token::Goto => self.parse_goto(),

            Token::Break => {
                self.advance();
                self.expect(&Token::Semicolon);
                Statement::Break
            }

            Token::Continue => {
                self.advance();
                self.expect(&Token::Semicolon);
                Statement::Continue
            }

            Token::LBrace => Statement::Block(self.parse_block()),

            // labels and exprs and typedefs
            Token::Ident(_) => {
                let checkpoint = self.pos;
                let name = match self.advance() {
                    Token::Ident(n) => n,
                    _ => panic!(),
                };

                if *self.peek() == Token::Colon {
                    // check for label
                    self.advance();
                    let statement = self.parse_statement();
                    Statement::Label(name, Box::new(statement))
                } else if matches!(self.peek(), Token::Ident(_) | Token::Star) {
                    // typedef'd type declaration: myint x = 5; or myint *p;
                    self.pos = checkpoint;
                    self.parse_local_var_dec()
                } else {
                    // expression
                    self.pos = checkpoint;
                    let expression = self.parse_expression();
                    self.expect(&Token::Semicolon);
                    Statement::ExprStatement(expression)
                }
            }

            _ => {
                let expression = self.parse_expression();
                self.expect(&Token::Semicolon);
                Statement::ExprStatement(expression)
            }
        }
    }

    fn parse_local_var_dec(&mut self) -> Statement {
        let storage_class = self.parse_storage_class();
        let qualified_type = self.parse_qualified_type();

        let name = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected name for var but got {:?}", other),
        };

        let mut typ = qualified_type;
        
        // handle int arr[10];
        while *self.peek() == Token::LBracket {
            self.advance();
            let size = if *self.peek() == Token::RBracket {
                None
            } else {
                match self.advance() {
                    Token::IntLiteral(n) => Some(n as usize),
                    other => panic!("Expected array size, got {:?}", other),
                }
            };
            self.expect(&Token::RBracket);
            typ = QualifiedType {
                base: Type::Array(Box::new(typ.base), size),
                is_const: typ.is_const,
            };
        }

        let init = if *self.peek() == Token::Assign {
            self.advance();
            Some(self.parse_expression())
        } else {
            None
        };

        self.expect(&Token::Semicolon);

        Statement::VarDec(typ, name, init, storage_class)
    }

    fn parse_return(&mut self) -> Statement {
        self.expect(&Token::Return);

        if *self.peek() == Token::Semicolon {
            self.advance();
            Statement::ReturnVoid
        } else {
            let expr = self.parse_expression();
            self.expect(&Token::Semicolon);
            Statement::Return(expr)
        }
    }

    fn parse_if(&mut self) -> Statement {
        self.expect(&Token::If);
        self.expect(&Token::LParen);
        let condition = self.parse_expression();
        self.expect(&Token::RParen);

        let then_block = vec![self.parse_statement()];

        let else_block = if *self.peek() == Token::Else {
            self.advance();
            Some(vec![self.parse_statement()])
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

        let body = vec![self.parse_statement()];

        Statement::While(condition, body)
    }

    fn parse_do_while(&mut self) -> Statement {
        self.expect(&Token::Do);

        let body = vec![self.parse_statement()];

        self.expect(&Token::While);
        self.expect(&Token::LParen);

        let condition = self.parse_expression();

        self.expect(&Token::RParen);
        self.expect(&Token::Semicolon);

        Statement::DoWhile(DoWhileStmt { body, condition })
    }

    fn parse_for_loop(&mut self) -> Statement {
        self.expect(&Token::For);
        self.expect(&Token::LParen);

        let init = if *self.peek() == Token::Semicolon {
            self.advance();
            None
        } else if self.is_type_keyword() {
            Some(Box::new(self.parse_local_var_dec()))
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

        let body = vec![self.parse_statement()];

        Statement::For(init, condition, increment, body)
    }

    fn parse_switch(&mut self) -> Statement {
        self.expect(&Token::Switch);
        self.expect(&Token::LParen);

        let expr = self.parse_expression();

        self.expect(&Token::RParen);
        self.expect(&Token::LBrace);

        let mut cases = vec![];

        while *self.peek() != Token::RBrace {
            match self.peek() {
                Token::Case => {
                    self.advance();
                    let value = Some(self.parse_expression());
                    self.expect(&Token::Colon);

                    let mut stmts = vec![];
                    while !matches!(self.peek(), Token::Case | Token::Default | Token::RBrace) {
                        stmts.push(self.parse_statement());
                    }

                    cases.push(Case { value, stmts });
                }
                Token::Default => {
                    self.advance();
                    self.expect(&Token::Colon);

                    let mut stmts = vec![];
                    while !matches!(self.peek(), Token::Case | Token::Default | Token::RBrace) {
                        stmts.push(self.parse_statement());
                    }

                    cases.push(Case { value: None, stmts });
                }
                _ => break,
            }
        }

        self.expect(&Token::RBrace);

        Statement::Switch(SwitchStmt { expr, cases })
    }

    fn parse_goto(&mut self) -> Statement {
        self.expect(&Token::Goto);
        let label = match self.advance() {
            Token::Ident(n) => n,
            other => panic!("Expected label after go to, got {:?}", other),
        };
        self.expect(&Token::Semicolon);
        Statement::Goto(label)
    }

    fn is_type_keyword(&self) -> bool {
        matches!(
            self.peek(),
            Token::Int | Token::Char | Token::Short | Token::Long
            | Token::Float | Token::Double | Token::Void | Token::Bool
            | Token::Signed | Token::Unsigned | Token::Const
            | Token::Static | Token::Extern
            | Token::Struct | Token::Union | Token::Enum
        )
    }

    // EXPRESSION
    // https://stackoverflow.com/questions/17369090/operator-precedence-table-for-the-c-programming-language
    // recursive decent parsing of expressions in order

    fn parse_expression(&mut self) -> Expr {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Expr {
        let mut left = self.parse_ternary();

        match self.peek() {
            Token::Assign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::Assign(Box::new(left), Box::new(right))
            }

            Token::PlusAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::AddAssign, Box::new(left), Box::new(right))
            }

            Token::MinusAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::SubAssign, Box::new(left), Box::new(right))
            }
            
            Token::StarAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::MulAssign, Box::new(left), Box::new(right))
            }
            Token::SlashAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::DivAssign, Box::new(left), Box::new(right))
            }

            Token::PercentAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::ModAssign, Box::new(left), Box::new(right))
            }

            Token::AndAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::AndAssign, Box::new(left), Box::new(right))
            }

            Token::OrAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::OrAssign, Box::new(left), Box::new(right))
            }

            Token::XorAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::XorAssign, Box::new(left), Box::new(right))
            }

            Token::LShiftAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::LShiftAssign, Box::new(left), Box::new(right))
            }

            Token::RShiftAssign => {
                self.advance();
                let right = self.parse_assignment();
                Expr::CompoundAssign(CompoundOp::RShiftAssign, Box::new(left), Box::new(right))
            }

            _ => left,
        }
    }

    fn parse_ternary(&mut self) -> Expr {
        let mut expr = self.parse_or();

        if *self.peek() == Token::Question {
            self.advance();

            let then_expr = self.parse_expression();

            self.expect(&Token::Colon);

            let else_expr = self.parse_ternary();

            expr = Expr::Ternary(Box::new(expr), Box::new(then_expr), Box::new(else_expr));
        }

        expr
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
        let mut left = self.parse_cast();
        loop {
            let op = match self.peek() {
                Token::Star => BinOp::Mul,
                Token::Slash => BinOp::Div,
                Token::Percent => BinOp::Mod,
                _ => break,
            };
            self.advance();
            let right = self.parse_cast();
            left = Expr::BinOp(Box::new(left), op, Box::new(right));
        }
        left
    }

    fn parse_cast(&mut self) -> Expr {
        // gotta check for the cast first
        // here i try parse and backtrack if not work
        if *self.peek() == Token::LParen {
            let checkpoint = self.pos;
            self.advance();

            if self.is_type_keyword() {
                let typ = self.parse_qualified_type();
                if *self.peek() == Token::RParen {
                    self.advance();
                    let expr = self.parse_cast();
                    return Expr::Cast(typ, Box::new(expr));
                }
            }

            self.pos = checkpoint;
        }

        self.parse_unary()
    }

    fn parse_unary(&mut self) -> Expr {
        match self.peek() {
            
            Token::Not => {
                self.advance();
                Expr::UnaryOp(UnaryOp::Not, Box::new(self.parse_cast()))
            }

            Token::Minus => {
                self.advance();
                Expr::UnaryOp(UnaryOp::Neg, Box::new(self.parse_cast()))
            }

            Token::Tilde => {
                self.advance();
                Expr::UnaryOp(UnaryOp::BitNot, Box::new(self.parse_cast()))
            }

            Token::PlusPlus => {
                self.advance();
                Expr::UnaryOp(UnaryOp::PreInc, Box::new(self.parse_cast()))
            }

            Token::MinusMinus => {
                self.advance();
                Expr::UnaryOp(UnaryOp::PreDec, Box::new(self.parse_cast()))
            }
            
            Token::Star => {
                self.advance();
                Expr::Deref(Box::new(self.parse_cast()))
            }

            Token::Ampersand => {
                self.advance();
                Expr::AddrOf(Box::new(self.parse_cast()))
            }

            // sizeof(int) or sizeof(expr)
            // try parse type keyword first, else just parse as an expression
            Token::Sizeof => {
                self.advance();
                if *self.peek() == Token::LParen {
                    let checkpoint = self.pos;
                    self.advance();

                    if self.is_type_keyword() {
                        let typ = self.parse_qualified_type();
                        if *self.peek() == Token::RParen {
                            self.advance();
                            return Expr::SizeofType(typ);
                        }
                    }

                    self.pos = checkpoint;
                }
                
                Expr::SizeofExpr(Box::new(self.parse_unary()))
            }

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
                        other => panic!("Expected field after '.', got {:?}", other),
                    };
                    expr = Expr::FieldAccess(Box::new(expr), field);
                }

                Token::Arrow => {
                    self.advance();
                    let field = match self.advance() {
                        Token::Ident(name) => name,
                        other => panic!("Expected field name after '->', got {:?}", other),
                    };
                    expr = Expr::PtrMember(Box::new(expr), field);
                }

                Token::LBracket => {
                    self.advance();
                    let index = self.parse_expression();
                    self.expect(&Token::RBracket);
                    expr = Expr::ArrayIndex(Box::new(expr), Box::new(index));
                }

                Token::LParen => {
                    self.advance();

                    let mut args = vec![];
                    while *self.peek() != Token::RParen {
                        args.push(self.parse_expression());
                        if *self.peek() == Token::Comma {
                            self.advance();
                        }
                    }
                    self.expect(&Token::RParen);
                    expr = Expr::Call(Box::new(expr), args);
                }

                Token::PlusPlus => {
                    self.advance();
                    expr = Expr::UnaryOp(UnaryOp::PostInc, Box::new(expr));
                }

                Token::MinusMinus => {
                    self.advance();
                    expr = Expr::UnaryOp(UnaryOp::PostDec, Box::new(expr));
                }

                _ => break,
            }
        }

        expr
    }

    fn parse_primary(&mut self) -> Expr {
        match self.advance() {
            Token::IntLiteral(n) => Expr::IntLiteral(n),
            Token::FloatLiteral(f) => Expr::FloatLiteral(f),
            Token::CharLiteral(c) => Expr::CharLiteral(c),
            Token::StringLiteral(s) => Expr::StringLiteral(s),
            Token::BoolLiteral(b) => Expr::BoolLiteral(b),
            Token::Null => Expr::Null,
            Token::Ident(name) => Expr::Identifier(name),

            Token::LParen => {
                let expr = self.parse_expression();
                self.expect(&Token::RParen);
                expr
            }

            other => panic!("Unexpected token in primary expression: {:?}", other),
        }
    }
}