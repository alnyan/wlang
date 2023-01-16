use std::{cell::RefCell, collections::HashMap, fmt::Debug, rc::Rc};

use crate::{LangType, LocalValue};

#[derive(Debug)]
pub struct FunctionScope {
    return_type: Rc<LangType>,
    args: HashMap<String, LocalValue>,
    children: Vec<Rc<RefCell<dyn Scope>>>,
    index: usize,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct BlockScope {
    #[derivative(Debug = "ignore")]
    parent: Rc<RefCell<dyn Scope>>,
    locals: HashMap<String, LocalValue>,
    index: usize,
}

pub trait Scope: Debug {
    // Variables
    fn local(&self, name: &str) -> Option<LocalValue>;
    fn add_local(&mut self, name: &str, ty: LocalValue);

    // Function signature
    fn function_return_type(&self) -> Rc<LangType>;
    fn is_upper(&self) -> bool;

    // Parent-child relations
    fn add_scope(&mut self, parent: Rc<RefCell<dyn Scope>>) -> Rc<RefCell<dyn Scope>>;
    fn scope(&self, index: usize) -> Option<Rc<RefCell<dyn Scope>>>;

    // Stored in tagged nodes to reenter the scopes later
    fn index(&self) -> Option<usize>;
    fn function_index(&self) -> usize;
}

impl BlockScope {
    pub fn new(parent: Rc<RefCell<dyn Scope>>, index: usize) -> Self {
        Self {
            parent,
            index,
            locals: HashMap::new(),
        }
    }
}

impl FunctionScope {
    pub fn new(return_type: Rc<LangType>, args: HashMap<String, LocalValue>, index: usize) -> Self {
        Self {
            return_type,
            args,
            index,
            children: vec![],
        }
    }
}

impl Scope for BlockScope {
    fn index(&self) -> Option<usize> {
        Some(self.index)
    }

    fn local(&self, name: &str) -> Option<LocalValue> {
        if let Some(ty) = self.locals.get(name) {
            Some(ty.clone())
        } else {
            self.parent.borrow().local(name)
        }
    }

    fn add_local(&mut self, name: &str, ty: LocalValue) {
        self.locals.insert(name.to_string(), ty);
    }

    fn function_return_type(&self) -> Rc<LangType> {
        self.parent.borrow().function_return_type()
    }

    fn add_scope(&mut self, parent: Rc<RefCell<dyn Scope>>) -> Rc<RefCell<dyn Scope>> {
        self.parent.borrow_mut().add_scope(parent)
    }

    fn scope(&self, index: usize) -> Option<Rc<RefCell<dyn Scope>>> {
        self.parent.borrow().scope(index)
    }

    fn function_index(&self) -> usize {
        self.parent.borrow().function_index()
    }

    fn is_upper(&self) -> bool {
        false
    }
}

impl Scope for FunctionScope {
    fn index(&self) -> Option<usize> {
        None
    }

    fn function_index(&self) -> usize {
        self.index
    }

    fn local(&self, name: &str) -> Option<LocalValue> {
        self.args.get(name).cloned()
    }

    fn add_local(&mut self, _name: &str, _ty: LocalValue) {
        panic!("Tried to add a local to function")
    }

    fn add_scope(&mut self, parent: Rc<RefCell<dyn Scope>>) -> Rc<RefCell<dyn Scope>> {
        let index = self.children.len();
        let scope = Rc::new(RefCell::new(BlockScope::new(parent, index)));
        self.children.push(scope.clone());
        scope
    }

    fn scope(&self, index: usize) -> Option<Rc<RefCell<dyn Scope>>> {
        self.children.get(index).cloned()
    }

    fn function_return_type(&self) -> Rc<LangType> {
        self.return_type.clone()
    }

    fn is_upper(&self) -> bool {
        true
    }
}
