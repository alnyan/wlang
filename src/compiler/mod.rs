use std::{rc::Rc, collections::HashMap};

// pub mod pass2;
pub mod pass1;

#[derive(Debug, Clone)]
pub enum CompileError {
    UndefinedVariable(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Location {
    Variable(String),
    IntegerLiteral(u64),
    Register(char),
    Temporary,
}

#[derive(Debug, Clone)]
pub enum TypeInfo {
    U64
}

pub struct Context {
    stack_size: usize,
    // TODO TypeId?
    types: HashMap<String, Rc<TypeInfo>>,
    locals: HashMap<String, (Rc<TypeInfo>, Option<Location>)>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            stack_size: 0,
            types: HashMap::from_iter([
                ("u64".to_owned(), Rc::new(TypeInfo::U64))
            ]),
            locals: HashMap::new(),
        }
    }

    pub fn get_type(&self, name: &str) -> Option<Rc<TypeInfo>> {
        self.types.get(name).cloned()
    }

    pub fn add_local(&mut self, ty: Rc<TypeInfo>) -> Result<Location, CompileError> {
        todo!()
    }

    // pub fn alloc_temp(&mut self) -> usize {
    //     let n = self.stack_size;
    //     self.stack_size += 1;
    //     n
    // }
}

pub fn type_eq(a: &Rc<TypeInfo>, b: &Rc<TypeInfo>) -> bool {
    Rc::ptr_eq(a, b)
}

impl TypeInfo {
    pub fn is_primitive_numeric(&self) -> bool {
        match self {
            TypeInfo::U64 => true,
            _ => false
        }
    }
}
