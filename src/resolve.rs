use crate::{ASTNode, ASTValue};
use rayon::iter::Either;

#[derive(Debug, Clone)]
pub enum PathName {
    ConstIdent {
        // const ident are parameters from ocaml
        name: String
    },
    Ident {
        name: String
    },
    Ref {
        base: String,
        ptr: Box<PathName>,
    },
}

#[derive(Debug, Clone)]
pub struct RefVar {
    path: PathName,

}

#[derive(Debug, Clone)]
pub enum TypeUnresolved {
    Bit,
    Bits {
        width: Box<Value>
    },
    Name(String),
}

#[derive(Debug, Clone)]
pub enum Value {
    ConstInt(i64),
    ConstBit(bool),
    Operated {
        op: String,
        lhs: Box<Value>,
        rhs: Box<Value>,
    },
    OpExtract {
        hi: Box<Value>,
        lo: Box<Value>,
        val: Box<Value>,
    },
    OpCast {
        to: TypeUnresolved,
        val: Box<Value>,
    },
    Ite {
        cond: Box<Value>,
        lhs: Box<Value>,
        rhs: Box<Value>,
    },
    Var(RefVar),
    Tuple(Vec<Value>),
    Undefined, // in case of error
}

type MultipleOperation = Vec<Operation>;

#[derive(Debug, Clone)]
pub enum Operation {
    Multiple(MultipleOperation),
    MuxOperation(Value, MultipleOperation, MultipleOperation),
    Assign(RefVar, Value),
    Jmp(Value),
    Nope,
}

impl Operation {
    fn into_multiple(self) -> Vec<Operation> {
        match self {
            Self::Multiple(ops) => ops,
            op => vec![op]
        }
    }
}

fn create_ident(ident: String) -> Value {
    Value::Var(RefVar {
        path: PathName::Ident {
            name: ident
        }
    })
}

pub fn resolve_value(value: ASTValue) -> Value {
    match value {
        ASTValue::Constant(const_val) =>
            match const_val {
                Either::Left(i) => Value::ConstInt(i),
                Either::Right(_bits) => Value::ConstInt(0)
                // FIXME convert to bit vector
            }
        ASTValue::Tuple(value) =>
            Value::Tuple(value.into_iter().map(resolve_value).collect()),
        ASTValue::BinOp { lhs, rhs, op } =>
            Value::Operated {
                op,
                lhs: resolve_value(*lhs).into(),
                rhs: resolve_value(*rhs).into(),
            },
        ASTValue::BitsAccess(ident, range) => {
            match range {
                Either::Left(index) => {
                    let index : Box<_> = resolve_value(*index).into();
                    Value::OpCast {
                        to: TypeUnresolved::Bit,
                        val: Value::OpExtract {
                            val: resolve_value(*ident).into(),
                            lo: index.clone(),
                            hi: index,
                        }.into(),
                    }
                }
                Either::Right((from, to)) => {
                    Value::OpExtract {
                        val: resolve_value(*ident).into(),
                        lo: resolve_value(*from).into(),
                        hi: resolve_value(*to).into(),
                    }
                }
            }
        }
        ASTValue::Ident(ident) => create_ident(ident),
        _ => Value::Undefined
    }
}

pub fn member_resolute() {}

pub fn lift_ast(node: ASTNode) -> Operation {
    match node {
        ASTNode::Multiple(ops) =>
            Operation::Multiple(ops.into_iter().map(lift_ast).collect()),
        ASTNode::IfElse { cond, hold, otherwise } =>
            Operation::MuxOperation(resolve_value(*cond),
                                    lift_ast(*hold).into_multiple(),
                                    lift_ast(*otherwise).into_multiple()),
        ASTNode::CaseWhen { val, cases } =>
            {
                let cond_val = resolve_value(*val);
                cases.into_iter().fold(Operation::Nope
                                       , |eff, v| {
                        eff
                    })
            }
        _ => panic!("undefined ast node type")
    }
}
