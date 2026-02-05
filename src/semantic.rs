use std::collections::HashSet;

use crate::{ast::{BinOp, CompoundOp, Declaration, Expr, Program, Statement, StorageClass, Type, UnaryOp}, symbol_table::{SymbolTable}};

// TODO:
// handle constants
// goto label validations
// warnings

pub struct SemanticAnalyzer {
    sym_table: SymbolTable,
    current_function_return_type: Option<Type>,
    loop_depth: usize,
    switch_depth: usize,
    labels: HashSet<String>,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        SemanticAnalyzer { 
            sym_table: SymbolTable::new(), 
            current_function_return_type: None, 
            loop_depth: 0,
            switch_depth: 0,
            labels: HashSet::new(),
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        let mut errors = vec![];

        // collect declarations
        for decl in &program.declarations {
            if let Err(decl_errors) = self.declare_declaration(decl) {
                errors.extend(decl_errors);
            }
        }

        // validate usages
        for decl in &program.declarations {
            if let Err(e) = self.validate_declaration(decl) {
                errors.push(e);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn declare_declaration(&mut self, decl: &Declaration) -> Result<(), Vec<String>> {
        let mut errors = vec![];

        // shadowing is allowed in c, so we just check current scope for repeating symbols
        match decl {
            Declaration::Enum(enum_decl) => {
                if let Some(name) = &enum_decl.name {
                    if self.sym_table.lookup_in_current_scope(name).is_some() {
                        errors.push(format!("Redeclaration of enum '{}'", name));
                    } else {
                        let variants: Vec<(String, Option<i64>)> = enum_decl.variants
                            .iter()
                            .map(|v| (v.name.clone(), v.value))
                            .collect();

                        let enum_type = Type::Enum {
                            name: name.clone(),
                            variants,
                        };

                        if let Err(e) = self.sym_table.declare_in_scope(
                            name,
                            enum_type,
                            StorageClass::None,
                            false,
                        ) {
                            errors.push(e);
                        }
                    }
                }
            }

            Declaration::Function(func_decl) => {
                let name = &func_decl.name;

                if self.sym_table.lookup_in_current_scope(name).is_some() {
                    errors.push(format!("Redec of func {:?}", name))
                } else {
                    let param_types: Vec<Type> = func_decl.params
                        .iter()
                        .map(|p| p.typ.base.clone())
                        .collect();
                    
                    let func_type = Type::Function { 
                        params: param_types, 
                        return_type: Box::new(func_decl.return_type.base.clone()) 
                    };

                    if let Err(e) = self.sym_table.declare_in_scope(
                        name,
                        func_type,
                        func_decl.storage_class.clone(),
                        false,
                    ) {
                        errors.push(e);
                    }
                }
            }

            Declaration::Struct(struct_dec) => {
                if let Some(name) = &struct_dec.name {
                    if self.sym_table.lookup_in_current_scope(name).is_some() {
                        errors.push(format!("Redec of struct '{}'", name));
                    } else {
                        let fields: Vec<(String, Type)> = struct_dec.fields
                            .iter()
                            .map(|f| (f.name.clone(), f.typ.base.clone()))
                            .collect();

                        let struct_type = Type::Struct {
                            name: name.clone(),
                            fields,
                        };
                        if let Err(e) = self.sym_table.declare_in_scope(
                            name,
                            struct_type,
                            StorageClass::None,
                            false,
                        ) {
                            errors.push(e);
                        }
                    }
                }
            }

            Declaration::Union(union_dec) => {
                if let Some(name) = &union_dec.name {
                    if self.sym_table.lookup_in_current_scope(name).is_some() {
                        errors.push(format!("redec of union {}", name));
                    } else {
                        let fields: Vec<(String, Type)> = union_dec.fields.iter()
                            .map(|f| (f.name.clone(), f.typ.base.clone()))
                            .collect();
                        let union_type = Type::Union {
                            name: name.clone(),
                            fields,
                        };
                        if let Err(e) = self.sym_table.declare_in_scope(
                            name,
                            union_type,
                            StorageClass::None,
                            false,
                        ) {
                            errors.push(e);
                        }
                    }
                }
            }

            Declaration::Variable(var_dec) => {
                if self.sym_table.lookup_in_current_scope(&var_dec.name).is_some() {
                    errors.push(format!("Redec of var {:?}", var_dec.name));
                } else if let Err(e) = self.sym_table.declare_in_scope(
                    &var_dec.name,
                    var_dec.typ.base.clone(),
                    var_dec.storage_class.clone(),
                    var_dec.typ.is_const,
                ) {
                    errors.push(e);
                }
            }

            Declaration::Typedef(typedef_dec) => {
                if self.sym_table.lookup_in_current_scope(&typedef_dec.name).is_some() {
                    errors.push(format!("Redec of typedef {:?}", typedef_dec.name));
                } else {
                    let typedef_type = Type::Typedef {
                        name: typedef_dec.name.clone(),
                        aliased_type: Box::new(typedef_dec.typ.base.clone()),
                    };
                    if let Err(e) = self.sym_table.declare_in_scope(
                        &typedef_dec.name,
                        typedef_type,
                        StorageClass::None,
                        typedef_dec.typ.is_const,
                    ) {
                        errors.push(e);
                    }
                }
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    fn validate_declaration(&mut self, decl: &Declaration) -> Result<(), String> {
        match decl {
            Declaration::Function(func_decl) => {
                if let Some(body) = &func_decl.body {
                    self.current_function_return_type = Some(func_decl.return_type.base.clone());
                    self.labels.clear();

                    // create new scope for function body
                    self.sym_table.push_scope();

                    for param in &func_decl.params {
                        if let Some(param_name) = &param.name {
                            self.sym_table.declare_in_scope(
                                param_name,
                                param.typ.base.clone(),
                                StorageClass::None,
                                param.typ.is_const,
                            )?;
                        }
                    }

                    for stmt in body {
                        self.validate_statement(stmt)?;
                    }

                    // "recursive like" stack popping
                    self.sym_table.pop_scope();
                }

                Ok(())
            }

            // check type initialization and variable
            Declaration::Variable(var_dec) => {
                if let Some(init_expr) = &var_dec.init {
                    let init_type = self.check_expression(init_expr)?;
                    if !self.types_compatible(&var_dec.typ.base, &init_type) {
                        return Err(format!(
                            "Type mismatch: expected {:?}, got {:?}",
                            var_dec.typ.base, init_type
                        ));
                    }
                }
                Ok(())
            }

            // just validating fields have types
            Declaration::Struct(struct_dec) => {
                if let Some(_name) = &struct_dec.name {
                    for field in &struct_dec.fields {
                        self.validate_type(&field.typ.base)?;
                    }
                }
                Ok(())
            }

            // validating field types agin
            Declaration::Union(union_dec) => {
                if let Some(_name) = &union_dec.name {
                    for field in &union_dec.fields {
                        self.validate_type(&field.typ.base)?;
                    }
                }
                Ok(())
            }

            // no semantics for num fo rnow
            Declaration::Enum(_enum_dec) => {
                Ok(())
            }

            // just validate alised type exists
            Declaration::Typedef(typedef_dec) => {
                self.validate_type(&typedef_dec.typ.base)?;
                Ok(())
            }
        }
    }

    fn validate_statement(&mut self, stmt: &Statement) -> Result<(), String> {
        match stmt {
            Statement::VarDec(typ, name, init, storage_class) => {
                self.sym_table.declare_in_scope(name, typ.base.clone(), storage_class.clone(), typ.is_const)?;
                
                // checking variable initializer types are good
                if let Some(expr) = init {
                    let init_type = self.check_expression(expr)?;
                    if !self.types_compatible(&typ.base, &init_type) {
                        return Err(format!(
                            "Type mismatch: expected {:?}, got {:?}",
                            typ.base, init_type
                        ));
                    }
                }
                Ok(())
            }

            // validate left and right expressions then check type assignment
            Statement::Assign(lhs, rhs) => {
                let rhs_type = self.check_expression(rhs)?;
                let lhs_type = self.check_expression(lhs)?;

                if !self.is_lvalue(lhs) {
                    return Err("Left side of assignment must be an lvalue".to_string());
                }
                
                if !self.types_compatible(&lhs_type, &rhs_type) {
                    return Err(format!(
                        "Type mismatch: expected {:?}, got {:?}",
                        lhs_type, rhs_type
                    ));
                }
                Ok(())
            }

            // check return expr then check if the return type matches expected
            Statement::Return(expr) => {
                let expr_type = self.check_expression(expr)?;
                
                if let Some(expected_type) = &self.current_function_return_type {
                    if !self.types_compatible(expected_type, &expr_type) {
                        return Err(format!(
                            "Return type mismatch: expected {:?}, got {:?}",
                            expected_type, expr_type
                        ));
                    }
                }
                Ok(())
            }

            // self ex
            Statement::ReturnVoid => {
                if let Some(expected_type) = &self.current_function_return_type {
                    if expected_type != &Type::Void {
                        return Err(format!("Expected return value of type {:?}", expected_type));
                    }
                }
                Ok(())
            }

            // validate condition then validate stmts in body
            Statement::If(cond, then_body, else_body) => {
                self.check_expression(cond)?;
                
                for stmt in then_body {
                    self.validate_statement(stmt)?;
                }
                
                if let Some(else_stmts) = else_body {
                    for stmt in else_stmts {
                        self.validate_statement(stmt)?;
                    }
                }
                Ok(())
            }

            Statement::While(cond, body) => {
                self.check_expression(cond)?;
                
                self.loop_depth += 1;
                for stmt in body {
                    self.validate_statement(stmt)?;
                }
                self.loop_depth -= 1;
                Ok(())
            }

            Statement::For(init, cond, inc, body) => {
                self.sym_table.push_scope();
                
                if let Some(init_stmt) = init {
                    self.validate_statement(init_stmt)?;
                }
                
                if let Some(cond_expr) = cond {
                    let _cond_type = self.check_expression(cond_expr)?;
                }
                
                if let Some(inc_expr) = inc {
                    let _inc_type = self.check_expression(inc_expr)?;
                }
                
                self.loop_depth += 1;
                for stmt in body {
                    self.validate_statement(stmt)?;
                }
                self.loop_depth -= 1;
                
                self.sym_table.pop_scope();
                Ok(())
            }

            Statement::Break => {
                if self.loop_depth == 0 && self.switch_depth == 0 {
                    return Err("break statement outside of loop or switch".to_string());
                }
                Ok(())
            }

            Statement::Continue => {
                if self.loop_depth == 0 {
                    return Err("continue statement outside of loop".to_string());
                }
                Ok(())
            }

            Statement::DoWhile(do_while_stmt) => {
                self.check_expression(&do_while_stmt.condition)?;
                
                self.loop_depth += 1;
                for stmt in &do_while_stmt.body {
                    self.validate_statement(stmt)?;
                }
                self.loop_depth -= 1;
                Ok(())
            }

            Statement::Switch(switch_stmt) => {
                let expr_type = self.check_expression(&switch_stmt.expr)?;
                if !self.is_integer_type(&expr_type) {
                    return Err(format!("Switch expression must be integer type, got {:?}", expr_type));
                }
                
                let mut seen_default = false;
                
                for case in &switch_stmt.cases {
                    if let Some(case_val) = &case.value {
                        let case_type = self.check_expression(case_val)?;
                        if !self.is_integer_type(&case_type) {
                            return Err(format!("Case value must be integer type, got {:?}", case_type));
                        }
                    } else {
                        if seen_default {
                            return Err("Multiple default cases in switch".to_string());
                        }
                        seen_default = true;
                    }
                    
                    self.switch_depth += 1;
                    for stmt in &case.stmts {
                        self.validate_statement(stmt)?;
                    }
                    self.switch_depth -= 1;
                }
                
                Ok(())
            }

            Statement::ExprStatement(expr) => {
                self.check_expression(expr)?;
                Ok(())
            }

            Statement::Block(stmts) => {
                self.sym_table.push_scope();
                for stmt in stmts {
                    self.validate_statement(stmt)?;
                }
                self.sym_table.pop_scope();
                Ok(())
            }

            Statement::CompoundAssign(op, lhs, rhs) => {
                let lhs_type = self.check_expression(lhs)?;
                let rhs_type = self.check_expression(rhs)?;
                
                match op {
                    CompoundOp::AddAssign | CompoundOp::SubAssign => {
                        // pointer arith
                        if matches!(lhs_type, Type::Pointer(_)) && self.is_integer_type(&rhs_type) {
                            
                        } else if !self.is_numeric_type(&lhs_type) || !self.is_numeric_type(&rhs_type) {
                            return Err(format!(
                                "Invalid types for {:?}: {:?} and {:?}",
                                op, lhs_type, rhs_type
                            ));
                        }
                    }
                    CompoundOp::MulAssign | CompoundOp::DivAssign => {
                        if !self.is_numeric_type(&lhs_type) || !self.is_numeric_type(&rhs_type) {
                            return Err(format!(
                                "Invalid types for {:?}: {:?} and {:?}",
                                op, lhs_type, rhs_type
                            ));
                        }
                    }
                    CompoundOp::ModAssign | CompoundOp::AndAssign | CompoundOp::OrAssign |
                    CompoundOp::XorAssign | CompoundOp::LShiftAssign | CompoundOp::RShiftAssign => {
                        if !self.is_integer_type(&lhs_type) || !self.is_integer_type(&rhs_type) {
                            return Err(format!(
                                "{:?} requires integer types, got {:?} and {:?}",
                                op, lhs_type, rhs_type
                            ));
                        }
                    }
                }
                
                if !self.is_lvalue(lhs) {
                    return Err("Left side of compound assignment must be an lvalue".to_string());
                }
                
                Ok(())
            }

            // TOOD: handle goto semantics
            Statement::Goto(_) => {
                // nothing fo rnow
                Ok(())
            }

            Statement::Label(label, stmt) => {
                if self.labels.contains(label) {
                    return Err(format!("Duplicate label '{}'", label));
                }
                self.labels.insert(label.clone());
                self.validate_statement(stmt)?;
                Ok(())
            }
        }
    }

    // make sure it is left valuw (something that identifies a mem loc)
    fn is_lvalue(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Identifier(_) => true,
            Expr::Deref(_) => true,
            Expr::ArrayIndex(_, _) => true,
            Expr::FieldAccess(_, _) => true,
            Expr::PtrMember(_, _) => true,
            _ => false,
        }
    }

    fn check_expression(&mut self, expr: &Expr) -> Result<Type, String> {
        match expr {
            Expr::IntLiteral(_) => Ok(Type::Int),
            Expr::BoolLiteral(_) => Ok(Type::Int),
            Expr::FloatLiteral(_) => Ok(Type::Double),
            Expr::CharLiteral(_) => Ok(Type::Char),
            Expr::StringLiteral(_) => Ok(Type::Pointer(Box::new(Type::Char))),
            Expr::Null => Ok(Type::Pointer(Box::new(Type::Void))),

            // check if it's declared in symtabe
            Expr::Identifier(name) => {
                self.sym_table.lookup(name)
                    .map(|sym| sym.typ.clone())
                    .ok_or_else(|| format!("Undeclared identifier '{}'", name))
            }

            // lhs and rhs
            Expr::BinOp(lhs, op, rhs) => {
                let lhs_type = self.check_expression(lhs)?;
                let rhs_type = self.check_expression(rhs)?;
                self.check_binary_op(op, &lhs_type, &rhs_type)
            }

            // just expr
            Expr::UnaryOp(op, operand) => {
                if matches!(op, UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec) {
                    if !self.is_lvalue(operand) {
                        return Err("Increment/decrement requires an lvalue".to_string());
                    }
                }

                let operand_type = self.check_expression(operand)?;
                self.check_unary_op(op, &operand_type)
            }

            Expr::Call(callee, args) => {
                let callee_type = self.check_expression(callee)?;

                // handling .method() and ->method()
                let (params, return_type) = match &callee_type {
                    
                    Type::Function { params, return_type } => (params.clone(), *return_type.clone()),
                    
                    Type::Pointer(inner) => {
                        if let Type::Function { params, return_type } = inner.as_ref() {
                            (params.clone(), *return_type.clone())
                        } else {
                            return Err(format!("Call on non func type {:?}", callee_type));
                        }
                    }
                    _ => return Err(format!("Call on non function type {:?}", callee_type)),
                };

                if args.len() != params.len() {
                    return Err(format!(
                        "Expected {} arguments, got {}",
                        params.len(), args.len()
                    ));
                }

                // check arguments against parameters
                for (arg, param_type) in args.iter().zip(params.iter()) {
                    let arg_type = self.check_expression(arg)?;
                    if !self.types_compatible(param_type, &arg_type) {
                        return Err(format!(
                            "Argument type mismatch: expected {:?}, got {:?}",
                            param_type, arg_type
                        ));
                    }
                }

                Ok(return_type)
            }

            // expr.field
            Expr::FieldAccess(obj, field) => {
                let obj_type = self.check_expression(obj)?;
                self.get_field_type(&obj_type, field)
            }

            // expr->feild
            Expr::PtrMember(ptr, field) => {
                let ptr_type = self.check_expression(ptr)?;
                match ptr_type {
                    Type::Pointer(inner) => self.get_field_type(&inner, field),
                    _ => Err(format!("Cannot use -> on non-pointer type {:?}", ptr_type)),
                }
            }

            // arr[idx]
            Expr::ArrayIndex(arr, idx) => {
                let arr_type = self.check_expression(arr)?;
                let idx_type = self.check_expression(idx)?;

                if !self.is_integer_type(&idx_type) {
                    return Err("Array index must be an integer type".to_string());
                }

                match arr_type {
                    Type::Array(elem_type, _) => Ok(*elem_type),
                    Type::Pointer(elem_type) => Ok(*elem_type),
                    _ => Err(format!("Cannot index into {:?}", arr_type)),
                }
            }

            // *var
            Expr::Deref(expr) => {
                let expr_type = self.check_expression(expr)?;
                match expr_type {
                    Type::Pointer(inner) => Ok(*inner),
                    Type::Array(inner, _) => Ok(*inner),
                    _ => Err(format!("Cannot dereference non pointer type {:?}", expr_type)),
                }
            }

            // &var just checking expressoin for now
            Expr::AddrOf(expr) => {
                if !self.is_lvalue(expr) {
                    return Err("Cannot take address of non-lvalue".to_string());
                }
                let expr_type = self.check_expression(expr)?;
                Ok(Type::Pointer(Box::new(expr_type)))
            }

            // cond ? then : else
            Expr::Ternary(cond, then_expr, else_expr) => {
                self.check_expression(cond)?;
                let then_type = self.check_expression(then_expr)?;
                let else_type = self.check_expression(else_expr)?;

                if self.types_compatible(&then_type, &else_type) {
                    Ok(self.common_type(&then_type, &else_type))
                } else {
                    Err(format!(
                        "Ternary branches have incompatible types: {:?} and {:?}",
                        then_type, else_type
                    ))
                }
            }

            // (int)var
            Expr::Cast(target_type, expr) => {
                let _expr_type = self.check_expression(expr)?;
                Ok(target_type.base.clone())
            }

            Expr::SizeofType(_) => Ok(Type::Unsigned(Box::new(Type::Long))),
            Expr::SizeofExpr(expr) => {
                let _expr_type = self.check_expression(expr)?;
                Ok(Type::Unsigned(Box::new(Type::Long)))
            }

            Expr::Assign(lhs, rhs) => {
                let lhs_type = self.check_expression(lhs)?;
                let rhs_type = self.check_expression(rhs)?;

                if !self.is_lvalue(lhs) {
                    return Err("Left side of assignment must be an lvalue".to_string());
                }

                if !self.types_compatible(&lhs_type, &rhs_type) {
                    return Err(format!(
                        "Assignment type mismatch: {:?} = {:?}",
                        lhs_type, rhs_type
                    ));
                }

                Ok(lhs_type)
            }

            Expr::CompoundAssign(op, lhs, rhs) => {
                let lhs_type = self.check_expression(lhs)?;
                let rhs_type = self.check_expression(rhs)?;

                if !self.is_lvalue(lhs) {
                    return Err("Compound assignment requires an lvalue".to_string());
                }

                match op {
                    CompoundOp::AddAssign | CompoundOp::SubAssign => {
                        if matches!(lhs_type, Type::Pointer(_)) && self.is_integer_type(&rhs_type) {
                        } else if !self.is_numeric_type(&lhs_type) || !self.is_numeric_type(&rhs_type) {
                            return Err(format!(
                                "Invalid types for {:?}: {:?} and {:?}",
                                op, lhs_type, rhs_type
                            ));
                        }
                    }
                    CompoundOp::MulAssign | CompoundOp::DivAssign => {
                        if !self.is_numeric_type(&lhs_type) || !self.is_numeric_type(&rhs_type) {
                            return Err(format!(
                                "Invalid types for {:?}: {:?} and {:?}",
                                op, lhs_type, rhs_type
                            ));
                        }
                    }
                    CompoundOp::ModAssign | CompoundOp::AndAssign | CompoundOp::OrAssign |
                    CompoundOp::XorAssign | CompoundOp::LShiftAssign | CompoundOp::RShiftAssign => {
                        if !self.is_integer_type(&lhs_type) || !self.is_integer_type(&rhs_type) {
                            return Err(format!(
                                "{:?} requires integer types, got {:?} and {:?}",
                                op, lhs_type, rhs_type
                            ));
                        }
                    }
                }

                Ok(lhs_type)
            }
        }
    }

    fn check_binary_op(&self, op: &BinOp, lhs: &Type, rhs: &Type) -> Result<Type, String> {
        match op {

            BinOp::Add | BinOp::Sub | BinOp::Mul | BinOp::Div => {
                // pointer arithmetics
                if let Type::Pointer(_) = lhs {
                    if self.is_integer_type(rhs) {
                        return Ok(lhs.clone());
                    }
                }
                if let Type::Pointer(_) = rhs {
                    if self.is_integer_type(lhs) && matches!(op, BinOp::Add) {
                        return Ok(rhs.clone());
                    }
                }

                if let (Type::Pointer(_), Type::Pointer(_)) = (lhs, rhs) {
                    if matches!(op, BinOp::Sub) {
                        return Ok(Type::Long);
                    }
                }

                // non pointer ariths
                if self.is_numeric_type(lhs) && self.is_numeric_type(rhs) {
                    Ok(self.common_type(lhs, rhs))
                } else {
                    Err(format!("Invalid operands to {:?}: {:?} and {:?}", op, lhs, rhs))
                }
            }

            BinOp::Mod => {
                if self.is_integer_type(lhs) && self.is_integer_type(rhs) {
                    Ok(self.common_type(lhs, rhs))
                } else {
                    Err(format!("Modulo requires integer types, got {:?} and {:?}", lhs, rhs))
                }
            }

            // comparison result is 0 or 1 (bool rep as int)
            BinOp::Eq | BinOp::NotEq | BinOp::Lt | BinOp::Gt | BinOp::Le | BinOp::Ge => {
                if self.types_compatible(lhs, rhs) {
                    Ok(Type::Int)
                } else {
                    Err(format!("Cannot compare {:?} and {:?}", lhs, rhs))
                }
            }

            // logic ops
            BinOp::And | BinOp::Or => {
                // scalar types can be logopped on
                if self.is_scalar_type(lhs) && self.is_scalar_type(rhs) {
                    Ok(Type::Int)
                } else {
                    Err(format!("Wrong type for logical ops, got {:?} and {:?}", lhs, rhs))
                }
            }

            // bitwise
            BinOp::BitAnd | BinOp::BitOr | BinOp::BitXor => {
                if self.is_integer_type(lhs) && self.is_integer_type(rhs) {
                    Ok(self.common_type(lhs, rhs))
                } else {
                    Err(format!("Bitwise operators require integer types, got {:?} and {:?}", lhs, rhs))
                }
            }

            // shifts
            BinOp::LShift | BinOp::RShift => {
                if self.is_integer_type(lhs) && self.is_integer_type(rhs) {
                    Ok(lhs.clone())
                } else {
                    Err(format!("Shift operators require integer types, got {:?} and {:?}", lhs, rhs))
                }
            }
        }
    }

    fn check_unary_op(&self, op: &UnaryOp, operand: &Type) -> Result<Type, String> {
        match op {
            UnaryOp::Neg => {
                if self.is_numeric_type(operand) {
                    Ok(operand.clone())
                } else {
                    Err(format!("Cannot negate non-numeric type {:?}", operand))
                }
            }

            UnaryOp::Not => {
                if self.is_scalar_type(operand) {
                    Ok(Type::Int)
                } else {
                    Err(format!("Logical not requires scalar type, got {:?}", operand))
                }
            }

            UnaryOp::BitNot => {
                if self.is_integer_type(operand) {
                    Ok(operand.clone())
                } else {
                    Err(format!("Bitwise not requires integer type, got {:?}", operand))
                }
            }

            UnaryOp::PreInc | UnaryOp::PreDec | UnaryOp::PostInc | UnaryOp::PostDec => {
                if self.is_numeric_type(operand) || matches!(operand, Type::Pointer(_)) {
                    Ok(operand.clone())
                } else {
                    Err(format!("Increment/decrement requires numeric or pointer type, got {:?}", operand))
                }
            }
        }
    }

    fn get_field_type(&self, struct_type: &Type, field_name: &str) -> Result<Type, String> {
        // get actual types from typedefs
        let resolved = self.resolve_type(struct_type);

        match &resolved {
            Type::Struct { name, fields } | Type::Union { name, fields } => {
                for (fname, ftype) in fields {
                    if fname == field_name {
                        return Ok(ftype.clone());
                    }
                }
                Err(format!("No field {} in {}", field_name, name))
            }
            Type::StructRef(name) | Type::UnionRef(name) => {
                Err(format!("Undefined struct/union {}", name))
            }
            _ => Err(format!("Cannot access field on type {:?}", struct_type)),
        }
    }

    // unwraps all aliases from user defined typdefs
    // because ast makes type refs
    fn resolve_type(&self, typ: &Type) -> Type {
        match typ {
            // resolve references by looking up symbol table
            Type::StructRef(name) => {
                if let Some(sym) = self.sym_table.lookup(name) {
                    if let Type::Struct { .. } = &sym.typ {
                        return sym.typ.clone();
                    }
                }
                typ.clone()
            }
            Type::UnionRef(name) => {
                if let Some(sym) = self.sym_table.lookup(name) {
                    if let Type::Union { .. } = &sym.typ {
                        return sym.typ.clone();
                    }
                }
                typ.clone()
            }
            Type::EnumRef(name) => {
                if let Some(sym) = self.sym_table.lookup(name) {
                    if let Type::Enum { .. } = &sym.typ {
                        return sym.typ.clone();
                    }
                }
                typ.clone()
            }
            Type::TypedefRef(name) => {
                if let Some(sym) = self.sym_table.lookup(name) {
                    if let Type::Typedef { aliased_type, .. } = &sym.typ {
                        return self.resolve_type(aliased_type);
                    }
                }
                typ.clone()
            }
            Type::Typedef { aliased_type, .. } => self.resolve_type(aliased_type),
            _ => typ.clone(),
        }
    }

    // checking compatible types
    // https://pebble.gitbooks.io/learning-c-with-pebble/content/appendixa.html
    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        let expected = self.resolve_type(expected);
        let actual = self.resolve_type(actual);

        if expected == actual {
            return true;
        }

        if let Type::Pointer(ref inner) = expected {
            if **inner == Type::Void && matches!(actual, Type::Pointer(_)) {
                return true;
            }
        }
        if let Type::Pointer(ref inner) = actual {
            if **inner == Type::Void && matches!(expected, Type::Pointer(_)) {
                return true;
            }
        }

        if matches!(actual, Type::Pointer(_)) && matches!(expected, Type::Pointer(_)) {
            return true;
        }

        if self.is_integer_type(&expected) && self.is_integer_type(&actual) {
            return true;
        }

        if self.is_numeric_type(&expected) && self.is_numeric_type(&actual) {
            return true;
        }

        // array type decays down to pointer
        if let Type::Pointer(ref ptr_inner) = expected {
            if let Type::Array(ref arr_inner, _) = actual {
                return self.types_compatible(ptr_inner.as_ref(), arr_inner.as_ref());
            }
        }

        false
    }

    fn is_integer_type(&self, typ: &Type) -> bool {
        let typ = self.resolve_type(typ);
        matches!(typ,
            Type::Char
                | Type::Short
                | Type::Int
                | Type::Long
                | Type::LongLong
                | Type::Signed(_)
                | Type::Unsigned(_)
        )
    }

    fn is_numeric_type(&self, typ: &Type) -> bool {
        let typ = self.resolve_type(typ);
        self.is_integer_type(&typ) || matches!(typ, Type::Float | Type::Double)
    }

    // arithmetic types, integers, floats, pointer types
    fn is_scalar_type(&self, typ: &Type) -> bool {
        let typ = self.resolve_type(typ);
        self.is_numeric_type(&typ) || matches!(typ, Type::Pointer(_))
    }

    fn common_type(&self, a: &Type, b: &Type) -> Type {
        if matches!(a, Type::Double) || matches!(b, Type::Double) {
            return Type::Double;
        }
        if matches!(a, Type::Float) || matches!(b, Type::Float) {
            return Type::Float;
        }
        if matches!(a, Type::LongLong) || matches!(b, Type::LongLong) {
            return Type::LongLong;
        }
        if matches!(a, Type::Long) || matches!(b, Type::Long) {
            return Type::Long;
        }
        Type::Int
    }

    fn validate_type(&self, typ: &Type) -> Result<(), String> {
        match typ {
            Type::StructRef(name) => {
                if self.sym_table.lookup(name).is_none() {
                    return Err(format!("Unknown struct {}", name));
                }
                Ok(())
            }
            Type::UnionRef(name) => {
                if self.sym_table.lookup(name).is_none() {
                    return Err(format!("Unknown union {}", name));
                }
                Ok(())
            }
            Type::EnumRef(name) => {
                if self.sym_table.lookup(name).is_none() {
                    return Err(format!("Unknown enum {}", name));
                }
                Ok(())
            }
            Type::TypedefRef(name) => {
                if self.sym_table.lookup(name).is_none() {
                    return Err(format!("Unknown type {}", name));
                }
                Ok(())
            }
            Type::Struct { .. } | Type::Union { .. } | Type::Enum { .. } => Ok(()),
            Type::Typedef { .. } => Ok(()),
            Type::Pointer(inner) => self.validate_type(inner),
            Type::Array(inner, _) => self.validate_type(inner),
            Type::Function { params, return_type } => {
                for param in params {
                    self.validate_type(param)?;
                }
                self.validate_type(return_type)
            }
            _ => Ok(()),
        }
    }
}