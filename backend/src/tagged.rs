use std::rc::Rc;

use ast::{Token, Node};

use crate::LangType;


#[derive(Debug, Clone)]
pub enum TaggedExprValue {
    Binary {
        op: Token,
        lhs: Rc<TaggedExpr>,
        rhs: Rc<TaggedExpr>,
    },
    Block(Vec<Rc<TaggedExpr>>),
    Statement(Rc<TaggedExpr>),
    Cast(Rc<TaggedExpr>, Rc<LangType>),
    Dereference(Rc<TaggedExpr>),
    Reference(Rc<TaggedLvalueExpr>),
    Assign(Rc<TaggedLvalueExpr>, Rc<TaggedExpr>),
    LocalDefinition {
        ty: Rc<LangType>,
        name: String,
        value: Rc<TaggedExpr>,
    },
    Condition {
        condition: Rc<TaggedExpr>,
        if_true: Rc<TaggedExpr>,
        if_false: Option<Rc<TaggedExpr>>,
    },
    Loop {
        condition: Option<Rc<TaggedExpr>>,
        body: Rc<TaggedExpr>,
    },
    Array(Vec<Rc<TaggedExpr>>),
    ArrayElement(Rc<TaggedExpr>, Rc<TaggedExpr>),
    BreakLoop,
    Return(Option<Rc<TaggedExpr>>),
    IntegerLiteral(u64),
    StringLiteral(String),
    Ident(String),
    Call(Rc<TaggedExpr>, Vec<Rc<TaggedExpr>>),
}

#[derive(Debug, Clone)]
pub enum TaggedLvalueExprValue {
    Ident(String),
    Dereference(Rc<TaggedLvalueExpr>),
    ArrayElement(Rc<TaggedLvalueExpr>, Rc<TaggedExpr>)
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct TaggedExpr {
    pub ty: Rc<LangType>,
    pub fn_index: usize,
    pub scope_index: Option<usize>,
    #[derivative(Debug = "ignore")]
    pub ast_node: Rc<Node>,
    pub value: TaggedExprValue,
}

#[derive(Derivative)]
#[derivative(Debug)]
pub struct TaggedLvalueExpr {
    pub ty: Rc<LangType>,
    pub fn_index: usize,
    pub scope_index: Option<usize>,
    #[derivative(Debug = "ignore")]
    pub ast_node: Rc<Node>,
    pub value: TaggedLvalueExprValue
}
