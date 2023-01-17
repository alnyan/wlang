//! Pass 1: Extract function signatures + global value types
//!         + typecheck
use std::collections::HashMap;

pub mod expr;
pub mod program;
pub mod scope;

pub use program::pass1_program;
pub use scope::{BlockScope, FunctionScope, Scope};

use super::{GlobalValue, LangFunction, Pass0Program};

#[derive(Debug)]
pub struct Pass1Program {
    pub(super) functions: Vec<LangFunction>,
    pub(super) globals: HashMap<String, GlobalValue>,
    // Pass 0 info
    pub(super) pass0: Pass0Program,
}

impl Pass1Program {
    pub fn function(&self, name: &str) -> Option<&LangFunction> {
        self.functions.iter().find(|f| f.name == name)
    }
}
