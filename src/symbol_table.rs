/*
table structure:

------------------------------------------------------------------------
| name_id | typ | scope_level | storage_class | is_const | line_number |
------------------------------------------------------------------------


stack of scopes uses 1 map per scope:
  scope 0 (global): {name_id -> Symbol, ..}
  scope 1 (block 1): {..}
*/
use std::collections::HashMap;
use crate::ast::{Type, StorageClass};

// https://www.reddit.com/r/Compilers/comments/1dy9722/symbol_table_design/
// going to just do stack of hash tables as my DS and then string interneing frot he lookups

pub struct StringInterner {
    strings: Vec<String>,
    lookup: HashMap<String, usize>, // string -> id
}

impl StringInterner {
    pub fn new() -> Self {
        StringInterner {
            strings: vec![],
            lookup: HashMap::new(),
        }
    }

    pub fn intern(&mut self, s: &str) -> usize {
        if let Some(&id) = self.lookup.get(s) {
            return id;
        }
        let id = self.strings.len();
        self.strings.push(s.to_string());
        self.lookup.insert(s.to_string(), id);
        id
    }

    pub fn get_str(&self, id: usize) -> &str {
        &self.strings[id]
    }

    pub fn get_id(&self, s: &str) -> Option<usize> {
        self.lookup.get(s).copied()
    }
}

pub struct Symbol {
    pub name_id: usize,
    pub typ: Type,
    pub scope_level: usize,
    pub storage_class: StorageClass,
    pub is_const: bool,
}

pub struct Scope {
    symbols: HashMap<usize, Symbol>, // name_id -> symbol
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            symbols: HashMap::new(),
        }
    }

    pub fn declare(&mut self, name_id: usize, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(&name_id) {
            return Err("Duplicate dec".to_string());
        }

        self.symbols.insert(name_id, symbol);
        Ok(())
    }

    pub fn lookup(&self, name_id: usize) -> Option<&Symbol> {
        self.symbols.get(&name_id)
    }
}

pub struct SymbolTable {
    interner: StringInterner,
    scopes: Vec<Scope>,
    scope_level: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        // starting scope is the global scope, inc scope each time and pop scopes as they go out
        SymbolTable {
            interner: StringInterner::new(),
            scopes: vec![Scope::new()],
            scope_level: 0,
        }
    }

    pub fn intern(&mut self, s: &str) -> usize {
        self.interner.intern(s)
    }

    // declaring symbol in curr scope
    pub fn declare_in_scope(&mut self, name: &str, typ: Type, storage: StorageClass, is_const: bool) -> Result<(), String> {
        let name_id = self.intern(name);
        let symbol = Symbol {
            name_id,
            typ,
            scope_level: self.scope_level,
            storage_class: storage,
            is_const,
        };
        self.scopes[self.scope_level].declare(name_id, symbol)
    }

    // here we look up the symbol
    // looking from current scope up to global since symbols propagate
    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        let name_id = self.interner.get_id(name)?;

        for i in (0..=self.scope_level).rev() {
            if let Some(sym) = self.scopes[i].lookup(name_id) {
                return Some(sym);
            }
        }
        None
    }

    pub fn lookup_in_current_scope(&self, name: &str) -> Option<&Symbol> {
        let name_id = self.interner.get_id(name)?;
        self.scopes[self.scope_level].lookup(name_id)
    }

    // cf to new scope
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.scope_level += 1;
    }

    // pop scope
    pub fn pop_scope(&mut self) -> Option<Scope> {
        if self.scope_level > 0 {
            self.scope_level -= 1;
            return self.scopes.pop();
        }
        None
    }

    // stirng id -> name
    pub fn get_name(&self, name_id: usize) -> &str {
        self.interner.get_str(name_id)
    }
}