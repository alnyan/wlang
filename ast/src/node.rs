use std::rc::Rc;

use crate::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Node {
    Ident(String),
    IntegerLiteral(u64, String),
    Binary(Token, Rc<Node>, Rc<Node>),
    Type(String),
    Call(Rc<Node>, Vec<Rc<Node>>),
    Function {
        name: String,
        ret_type: Option<Rc<Node>>,
        args: Vec<(Rc<Node>, Rc<Node>)>,
        body: Rc<Node>,
    },
    Block(Vec<Rc<Node>>),
    Statement(Rc<Node>),
    BreakLoop,
    Condition {
        condition: Rc<Node>,
        if_true: Rc<Node>,
        if_false: Option<Rc<Node>>
    },
    Loop {
        condition: Option<Rc<Node>>,
        body: Rc<Node>
    },
    GlobalDefinition {
        name: String,
        is_const: bool,
        ty: Rc<Node>,
        value: Rc<Node>,
    },
    LocalDefinition {
        name: String,
        is_mutable: bool,
        ty: Rc<Node>,
        value: Rc<Node>,
    },
    ExternFunction {
        name: String,
        ret_type: Option<Rc<Node>>,
        arg_types: Vec<(Rc<Node>, Rc<Node>)>,
    },
}
