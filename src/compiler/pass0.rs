/// Pass 0: Extract all available types (custom + prelude)
use std::{collections::HashMap, rc::Rc};

use crate::parser::Node;

use super::{CompilerError, Type};

#[derive(Debug)]
pub struct Pass0Program {
    types: HashMap<String, Rc<Type>>,
    void_type: Rc<Type>,
}

impl Pass0Program {
    pub fn named_type(&self, name: &str) -> Option<Rc<Type>> {
        self.types.get(name).cloned()
    }

    pub fn void_type(&self) -> Rc<Type> {
        self.void_type.clone()
    }
}

pub fn pass0_program(_items: &[Rc<Node>]) -> Result<Pass0Program, CompilerError> {
    // TODO custom types

    Ok(Pass0Program {
        types: HashMap::from_iter([
            ("i64".to_owned(), Rc::new(Type::I64)),
            ("i32".to_owned(), Rc::new(Type::I32)),
            ("i16".to_owned(), Rc::new(Type::I16)),
            ("i8".to_owned(), Rc::new(Type::I8)),
            ("u64".to_owned(), Rc::new(Type::U64)),
            ("u32".to_owned(), Rc::new(Type::U32)),
            ("u16".to_owned(), Rc::new(Type::U16)),
            ("u8".to_owned(), Rc::new(Type::U8)),
        ]),
        void_type: Rc::new(Type::Void),
    })
}
