// see design.txt for isa specs

// I will be taking using Lua's bytecode format closely
// for iABC instruction:
// [opcode][dest][reg][reg]
// [  6   ][ 8  ][ 9 ][ 9 ]
// for the two registers at the end,
// use the first 8 bits for the reg number
// and then a flag bit for the constant table index

use std::{collections::HashMap, u8::MAX};

use bitvec::vec::BitVec;

use crate::ast::{BinOp, Declaration, EnumDec, Expr, FunctionDec, Program, Statement};

// 6 bit opcode
pub enum OpCode {
    // iABC
    ADD, SUB, MUL, DIV, MOD,

    // iABx
    LOADK,
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
}

pub struct CodeGenerator {
    // returned instructs
    instructions: Vec<Instruction>,

    // static 256 (8bit) registers, so using a bitvec to 
    // track state. 0 = not in use, 1 = in use
    register_state: BitVec,

    // var name -> register id
    sym_table: HashMap<String, u8>,

    // max register allocated so can 
    // allocate registers at compile time
    max_reg: u8,

    // array of constants to check during compile time
    constants: Vec<i64>,
}

impl CodeGenerator {
    pub fn new() -> Self {
        CodeGenerator {
            instructions: vec![],
            register_state: BitVec::repeat(false, 256),
            sym_table: HashMap::new(),
            max_reg: 0,
            constants: vec![],
        }
    }

    // allocate mem to reg and return its id
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

    // emit instruction to instr vec
    fn emit(&mut self, instr: Instruction) {
        self.instructions.push(instr);
    }

    fn add_constant(&mut self, value: i64) -> usize {
        // see if we can reuse constant to save space. this is just compile time cost so not too worried
        if let Some(idx) = self.constants.iter().position(|&c| c == value) {
            return idx;
        }
        self.constants.push(value);
        self.constants.len() - 1
    }

    pub fn gen_program(&mut self, program: &Program) {
        for decl in &program.declarations {
            match decl {
                Declaration::Function(func_decl) => {
                    self.gen_function(func_decl);
                }

                _ => {}
            }
        }
    }

    pub fn gen_function(&mut self, func: &FunctionDec) {
        if let Some(body) = &func.body {
            for stmt in body {
                self.gen_statement(stmt);
            }
        }
    }

    pub fn gen_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::ExprStatement(expr) => {
                self.gen_expr(expr);
            }
            _ => todo!()
        }
    }

    // returns the register
    pub fn gen_expr(&mut self, expr: &Expr) -> u8 {
        match expr {
            Expr::IntLiteral(val) => {
                let reg = self.allocate_register();
                let const_idx = self.add_constant(*val);
                self.emit(
                    // loadk into dest register, the constant idx
                    Instruction::ABx { 
                        opcode: OpCode::LOADK, 
                        a: reg, 
                        bx: const_idx as u32
                    }
                );
                reg
            }

            Expr::BinOp(lhs, op, rhs) => {
                let left_reg = self.gen_expr(lhs);
                let right_reg = self.gen_expr(rhs);
                let result_reg = self.allocate_register();

                let opcode = match op {
                    BinOp::Add => OpCode::ADD,
                    BinOp::Sub => OpCode::SUB,
                    BinOp::Mul => OpCode::MUL,
                    BinOp::Div => OpCode::DIV,
                    BinOp::Mod => OpCode::MOD,
                    other => panic!("Not implemented {:?}", other),
                };

                self.emit(
                    Instruction::ABC { opcode, a: result_reg, b: left_reg as u16, c: right_reg as u16}
                );

                // dont forget to free registers! :D >:D >_>
                self.free_register(left_reg);
                self.free_register(right_reg);

                result_reg
            }

            _ => todo!()
        }
    }
}