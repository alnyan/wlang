use std::rc::Rc;

use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum TypeNode {
    Simple(String),
    Pointer(Rc<Node>),
    SizedArray(Rc<Node>, usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemNode {
    Function {
        name: String,
        ret_type: Option<Rc<Node>>,
        args: Vec<(Rc<Node>, Rc<Node>)>,
        body: Rc<Node>,
    },
    GlobalDefinition {
        name: String,
        is_const: bool,
        ty: Rc<Node>,
        value: Rc<Node>,
    },
    ExternFunction {
        name: String,
        ret_type: Option<Rc<Node>>,
        arg_types: Vec<(Rc<Node>, Rc<Node>)>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Ident(String),
    Reference(Rc<Node>),
    Dereference(Rc<Node>),
    IntegerLiteral(u64, String),
    StringLiteral(String),
    Binary(Token, Rc<Node>, Rc<Node>),
    Type(TypeNode),
    Array(Vec<Rc<Node>>),
    ArrayRepeat(Rc<Node>, Rc<Node>),
    ArrayElement(Rc<Node>, Rc<Node>),
    Call(Rc<Node>, Vec<Rc<Node>>),
    Block(Vec<Rc<Node>>),
    Statement(Rc<Node>),
    BreakLoop,
    Return(Option<Rc<Node>>),
    Condition {
        condition: Rc<Node>,
        if_true: Rc<Node>,
        if_false: Option<Rc<Node>>,
    },
    Loop {
        condition: Option<Rc<Node>>,
        body: Rc<Node>,
    },
    LocalDefinition {
        name: String,
        is_mutable: bool,
        ty: Rc<Node>,
        value: Rc<Node>,
    },
    Item(Rc<ItemNode>)
}
