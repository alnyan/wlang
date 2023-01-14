//! Pass 0: Extract all available types (custom + prelude)
use std::{collections::HashMap, rc::Rc};

use crate::parser::Node;

use super::{CompilerError, LangType};

#[derive(Debug)]
pub struct Pass0Program {
    types: HashMap<String, Rc<LangType>>,
    void_type: Rc<LangType>,
}

impl Pass0Program {
    pub fn named_type(&self, name: &str) -> Option<Rc<LangType>> {
        self.types.get(name).cloned()
    }

    pub fn void_type(&self) -> Rc<LangType> {
        self.void_type.clone()
    }
}

pub fn pass0_program(_items: &[Rc<Node>]) -> Result<Pass0Program, CompilerError> {
    // TODO custom types

    Ok(Pass0Program {
        types: HashMap::from_iter([
            ("i64".to_owned(), Rc::new(LangType::I64)),
            ("i32".to_owned(), Rc::new(LangType::I32)),
            ("i16".to_owned(), Rc::new(LangType::I16)),
            ("i8".to_owned(), Rc::new(LangType::I8)),
            ("u64".to_owned(), Rc::new(LangType::U64)),
            ("u32".to_owned(), Rc::new(LangType::U32)),
            ("u16".to_owned(), Rc::new(LangType::U16)),
            ("u8".to_owned(), Rc::new(LangType::U8)),
        ]),
        void_type: Rc::new(LangType::Void),
    })
}
