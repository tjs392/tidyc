// see design.txt for isa specs

// I will be taking using Lua's bytecode format closely
// https://mcours.net/cours/pdf/info1/Introduction_toLua_jkrfufd.pdf
// for iABC instruction:
// [opcode][dest][reg][reg]
// [  6   ][ 8  ][ 9 ][ 9 ]
// for the two registers at the end,
// use the first 8 bits for the reg number
// and then a flag bit for the constant table index

// for function:
// each function will get their function chunk
// with their own registers and constants
// codegen will generator code per function

use std::{collections::{HashMap, HashSet}, u8::MAX};

use bitvec::vec::BitVec;

use crate::ast::{BinOp, Declaration, EnumDec, Expr, FunctionDec, Program, Statement};

// 6 bit opcode
pub enum OpCode {
    // iABC
    ADD, SUB, MUL, DIV, MOD, MOV,
    EQ, LT, LE,

    // iABx
    LOADK, 
    TEST,

    // iAsBx
    JMP, // unconditional jump
}


pub enum Instruction {
    // iABC: three operand instructions (arithmetic, etc)
    ABC { 
        // enforce 9 bit register for b and c
        opcode: OpCode, //6 bit opcode
        a: u8,          // 8 bit destination register
        b: u16,         // 9 bit source register
        c: u16,         // another 9 it source register
    },

     // iABx: one register + large immediate/index (load constant, etc)
    ABx {
        opcode: OpCode, // 6 bit op
        a: u8,          // 8 bit dest reg
        bx: u32         // 18 bit immediate (262,143 MAX)
    },

    iAsBx {
        opcode: OpCode,
        offset: i32, // enforce signed 18 bit offset <<
    },
}

pub struct FunctionChunk {
    pub name: String,
    pub instructions: Vec<Instruction>,
    pub constants: Vec<i64>,
    pub max_registers: u8,
}

// builder for compiling single func
struct FunctionBuilder {
    /// func name
    name: String,

    /// returned instructs
    instructions: Vec<Instruction>,

    /// static 256 (8bit) registers, so using a bitvec to 
    /// track state. 0 = not in use, 1 = in use
    register_state: BitVec,

    /// var name -> register id
    sym_table: HashMap<String, u8>,

    /// max register allocated so can 
    /// allocate registers at compile time
    max_reg: u8,

    /// array of constants to check during compile time
    constants: Vec<i64>,

    /// set of permanent variable registers
    permanent_regs: HashSet<u8>,
}

impl FunctionBuilder {
    fn new(name: String) -> Self {
        FunctionBuilder {
            name,
            instructions: vec![],
            constants: vec![],
            register_state: BitVec::repeat(false, 256),
            sym_table: HashMap::new(),
            max_reg: 0,
            permanent_regs: HashSet::new(),
        }
    }

    // allocate reg and return its id
    fn allocate_register(&mut self) -> u8 {
        let first = self.register_state.first_zero().unwrap();
        self.register_state.set(first, true);
        self.max_reg = self.max_reg.max(first as u8);
        first as u8
    }

    // free register
    fn free_register(&mut self,reg_id: u8) {
        self.register_state.set(reg_id as usize, false)
    }

    /// emit instruction to instr vec
    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    fn emit_jump_placeholder(&mut self) -> usize {
        self.emit(Instruction::iAsBx { opcode: OpCode::JMP, offset: 0 });
        self.instructions.len() - 1
    }

    fn finish_jump(&mut self, jump_idx: usize) {
        let offset = (self.instructions.len() - jump_idx - 1) as i32;
        self.instructions[jump_idx] = Instruction::iAsBx { 
            opcode: OpCode::JMP, 
            offset 
        };
    }

    fn add_constant(&mut self, value: i64) -> usize {
        // see if we can reuse constant to save space. this is just compile time cost so not too worried
        if let Some(idx) = self.constants.iter().position(|&c| c == value) {
            return idx;
        }
        self.constants.push(value);
        self.constants.len() - 1
    }

    fn finalize(self) -> FunctionChunk {
        FunctionChunk {
            name: self.name,
            instructions: self.instructions,
            constants: self.constants,
            max_registers: self.max_reg,
        }
    }

    // == compilation :D

    pub fn gen_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::ExprStatement(expr) => {
                self.gen_expr(expr, None);
            }

            // variable declaration just allocates a permanent register and 
            // stores the right hand side expression in that reg
            Statement::VarDec(typ, name, expr, storage_class) => {
                // just reuse the expression register for the var reg
                if let Some(init_expr) = expr {
                    if matches!(init_expr, Expr::Identifier(_)) {
                        // allocate new register and use target (generates MOV)
                        // this is for something like y = x
                        let var_reg = self.allocate_register();
                        self.permanent_regs.insert(var_reg);
                        self.sym_table.insert(name.clone(), var_reg);
                        self.gen_expr(init_expr, Some(var_reg));
                    } else {
                        // claim the expression's result register (optimal)
                        // this will help reuse the result register
                        let expr_reg = self.gen_expr(init_expr, None);
                        self.permanent_regs.insert(expr_reg);
                        self.sym_table.insert(name.clone(), expr_reg);
                    }
                } else {
                    let var_reg = self.allocate_register();
                    self.permanent_regs.insert(var_reg);

                    self.sym_table.insert(name.clone(), var_reg);
                }
            }

            // if statement are pretty straight forward
            Statement::If(cond, then_body, else_body) => {
                let cond_reg = self.gen_expr(cond, None);
                self.emit(Instruction::ABC { opcode: OpCode::TEST, a: cond_reg, b: 0, c: 0 });

                let jmp_to_else = self.emit_jump_placeholder();

                for stmt in then_body {
                    self.gen_statement(stmt);
                }

                if let Some(else_block) = else_body {
                    let jmp_skip_else = self.emit_jump_placeholder();
                    self.finish_jump(jmp_to_else);

                    for stmt in else_block { 
                        self.gen_statement(stmt);
                    }

                    self.finish_jump(jmp_skip_else);
                } else {
                    self.finish_jump(jmp_to_else);
                }

                if !self.permanent_regs.contains(&cond_reg) {
                    self.free_register(cond_reg);
                }
            }

            Statement::Block(stmts) => {
                for s in stmts {
                    self.gen_statement(s);
                }
            }

            other => {
                eprintln!("Unimplemented stateme: {:?}", other);
                todo!()
            }
        }
    }

    // returns the register, takes optional target register as well
    pub fn gen_expr(&mut self, expr: &Expr, target: Option<u8>) -> u8 {
        match expr {
            Expr::IntLiteral(val) => {
                let result_reg = target.unwrap_or_else(|| self.allocate_register());
                let const_idx = self.add_constant(*val);
                self.emit(
                    // loadk into dest register, the constant idx
                    Instruction::ABx { 
                        opcode: OpCode::LOADK, 
                        a: result_reg, 
                        bx: const_idx as u32
                    }
                );
                result_reg
            }

            Expr::BinOp(lhs, op, rhs) => {
                let left_reg = self.gen_expr(lhs, None);
                let right_reg = self.gen_expr(rhs, None);

                // seeing if we can save an extra register allocation
                let result_reg = if let Some(t) = target {
                    // use target if specified
                    t
                } else if !self.permanent_regs.contains(&right_reg) {
                    // if the right expr is not variable,
                    // reuse the right reg
                    right_reg 
                } else {
                    // else just make a new register cause we need
                    self.allocate_register()
                };

                let opcode = match op {
                    BinOp::Add => OpCode::ADD,
                    BinOp::Sub => OpCode::SUB,
                    BinOp::Mul => OpCode::MUL,
                    BinOp::Div => OpCode::DIV,
                    BinOp::Mod => OpCode::MOD,

                    BinOp::Eq => OpCode::EQ,
                    BinOp::Lt => OpCode::LT,
                    BinOp::Le => OpCode::LE,

                    other => {
                        eprintln!("Unimplemented expression: {:?}", other);
                        todo!()
                    }
                };

                self.emit(
                    Instruction::ABC { opcode, a: result_reg, b: left_reg as u16, c: right_reg as u16}
                );

                // dont forget to free registers! :D >:D >_>
                // but need to check if the return register is going into a variable!!
                // so make sure youre not clearing permanent var regs
                if !self.permanent_regs.contains(&left_reg) {
                    self.free_register(left_reg);
                }
                if !self.permanent_regs.contains(&right_reg) && right_reg != result_reg {
                    self.free_register(right_reg);
                }

                result_reg
            }

            // if we see an identifier, we need the value
            // so get the register of where that value lives
            // and move it into the target and return the register
            Expr::Identifier(name) => {
                let var_reg = *self.sym_table.get(name).unwrap();

                if let Some(target) = target {
                    if var_reg != target {
                        self.emit(Instruction::ABC { 
                            opcode: OpCode::MOV, 
                            a: target, 
                            b: var_reg as u16, 
                            c: 0 
                        });
                    }
                    target
                } else {
                    var_reg
                }
            }

            other => {
                eprintln!("Unimplemented expression: {:?}", other);
                todo!()
            }
        }
    }
}



pub struct CodeGenerator {
    // all function chunks
    pub functions: Vec<FunctionChunk>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator { functions: vec![] }
    }

    pub fn print_instructions(&self) {
        for func in &self.functions {
            println!("\n=== Function: {} ===", func.name);
            println!("Registers: {} (r0-r{})", func.max_registers + 1, func.max_registers);
            
            for (i, instr) in func.instructions.iter().enumerate() {
                match instr {
                    Instruction::ABC { opcode, a, b, c } => {
                        let op_name = match opcode {
                            OpCode::ADD => "ADD",
                            OpCode::SUB => "SUB",
                            OpCode::MUL => "MUL",
                            OpCode::DIV => "DIV",
                            OpCode::MOD => "MOD",
                            OpCode::MOV => "MOV",
                            OpCode::EQ => "EQ",
                            OpCode::LT => "LT",
                            OpCode::LE => "LE",
                            OpCode::TEST => "TEST",
                            _ => "Unknown",
                        };
                        println!("{:04}: {} r{}, r{}, r{}", i, op_name, a, b, c);
                    }

                    Instruction::ABx { opcode, a, bx } => {
                        let op_name = match opcode {
                            OpCode::LOADK => "LOADK",
                            _ => "Unknown",
                        };
                        println!("{:04}: {} r{}, K{}", i, op_name, a, bx);
                    }

                    Instruction::iAsBx { opcode, offset } => {
                        let op_name = match opcode {
                            OpCode::JMP => "JMP",
                            _ => "Unknown",
                        };
                        println!("{:04}: {} {},", i, op_name, offset);
                    }

                }
            }
            
            println!("\nConstants:");
            for (i, val) in func.constants.iter().enumerate() {
                println!("  K{}: {}", i, val);
            }
        }
    }

    pub fn gen_program(&mut self, program: &Program) {
        for decl in &program.declarations {
            match decl {
                Declaration::Function(func) => self.gen_function(func),
                _ => {}
            }
        }
    }

    fn gen_function(&mut self, func: &FunctionDec) {
        let mut builder = FunctionBuilder::new(func.name.clone());
        
        if let Some(body) = &func.body {
            for stmt in body {
                builder.gen_statement(stmt);
            }
        }
        
        self.functions.push(builder.finalize());
    }
}