//! Pass 0: Extract all available types (custom + prelude)
use std::{collections::HashMap, rc::Rc};

use ast::Node;

use crate::LangIntType;

use super::{CompilerError, LangType};

#[derive(Debug)]
pub struct Pass0Program {
    types: HashMap<String, Rc<LangType>>,
    bool_type: Rc<LangType>,
    i64_type: Rc<LangType>,
    void_type: Rc<LangType>,
}

impl Pass0Program {
    pub fn named_type(&self, name: &str) -> Option<Rc<LangType>> {
        self.types.get(name).cloned()
    }

    pub fn void_type(&self) -> Rc<LangType> {
        self.void_type.clone()
    }

    pub fn bool_type(&self) -> Rc<LangType> {
        self.bool_type.clone()
    }

    pub fn i64_type(&self) -> Rc<LangType> {
        self.i64_type.clone()
    }

    pub fn integer_literal_extra_type(&self, name: &str) -> Option<Rc<LangType>> {
        if name.is_empty() {
            Some(self.i64_type())
        } else {
            self.named_type(name)
        }
    }
}

pub fn pass0_program(_items: &[Rc<Node>]) -> Result<Pass0Program, CompilerError> {
    // TODO custom types
    let i64_type = Rc::new(LangType::IntType(LangIntType::I64));
    let bool_type = Rc::new(LangType::BoolType);

    Ok(Pass0Program {
        types: HashMap::from_iter([
            ("i64".to_owned(), i64_type.clone()),
            (
                "i32".to_owned(),
                Rc::new(LangType::IntType(LangIntType::I32)),
            ),
            (
                "i16".to_owned(),
                Rc::new(LangType::IntType(LangIntType::I16)),
            ),
            ("i8".to_owned(), Rc::new(LangType::IntType(LangIntType::I8))),
            (
                "u64".to_owned(),
                Rc::new(LangType::IntType(LangIntType::U64)),
            ),
            (
                "u32".to_owned(),
                Rc::new(LangType::IntType(LangIntType::U32)),
            ),
            (
                "u16".to_owned(),
                Rc::new(LangType::IntType(LangIntType::U16)),
            ),
            ("u8".to_owned(), Rc::new(LangType::IntType(LangIntType::U8))),
        ]),
        i64_type,
        bool_type,
        void_type: Rc::new(LangType::Void),
    })
}
