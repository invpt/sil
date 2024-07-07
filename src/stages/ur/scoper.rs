use rustc_hash::FxHashMap;

use crate::{builtin::Builtin, strings::Intern};

use super::Symbol;

#[derive(Debug)]
pub struct Scoper<'s> {
    stack: Vec<FxHashMap<Intern<'s>, Symbol<'s>>>,
    sym_counter: usize,
}

impl<'s> Scoper<'s> {
    pub fn new() -> Scoper<'s> {
        Scoper {
            stack: vec![FxHashMap::default()],
            sym_counter: Builtin::COUNT,
        }
    }
    pub fn lookup(&mut self, name: Intern<'s>) -> Option<Symbol<'s>> {
        for scope in self.stack.iter().rev() {
            if let Some(symb) = scope.get(&name) {
                return Some(*symb);
            }
        }

        if let Some(builtin) = Builtin::VALUES.iter().find(|b| b.name() == name) {
            Some(Symbol::builtin(*builtin))
        } else {
            None
        }
    }

    pub fn new_symbol(&mut self, name: Intern<'s>) -> Symbol<'s> {
        let index = self.sym_counter;
        self.sym_counter = self
            .sym_counter
            .checked_add(1)
            .expect("Symbol counter overflowed!");
        let sym = Symbol { name, index };
        self.stack.last_mut().unwrap().insert(name, sym);
        sym
    }

    pub fn push(&mut self) {
        self.stack.push(FxHashMap::default())
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }
}
