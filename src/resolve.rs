use crate::{ASTNode, ASTValue, LeftPattern, TyName};
use rayon::iter::Either;
use std::collections::VecDeque;

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
    pub(crate) path: PathName,

}

#[derive(Debug, Clone)]
pub enum TypeUnresolved {
    Bit,
    Bits {
        width: Box<Value>
    },
    Name(String),
}

impl From<TyName> for TypeUnresolved {
    fn from(ty: TyName) -> Self {
        match ty {
            TyName::Bit => TypeUnresolved::Bit,
            TyName::Bits { width } =>
                TypeUnresolved::Bits {
                    width: resolve_value(width).into()
                },
            TyName::NameBind(name) => TypeUnresolved::Name(name),
            TyName::Int | TyName::String =>
                TypeUnresolved::Bits {
                    width: Value::ConstInt(64).into()
                    // FIXME this might have to be extended
                },
            TyName::Bot => TypeUnresolved::Name("Bot".to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    ConstInt(i64),
    ConstBit(bool),
    CallSiteRet {
        name: String,
        params: Vec<Value>,
    },
    CallSiteIndex {
        name: String,
        params: Vec<Value>,
    },
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
    LetVar(String, Either<TypeUnresolved, Value>, Box<Operation>),
    Multiple(MultipleOperation),
    MuxOperation(Value, MultipleOperation, MultipleOperation),
    Assign(RefVar, Value),
    Jmp(Value),
    CallSite {
        func: String,
        params: Vec<Value>,
    },
    CallSiteAssign {
        func: String,
        params: Vec<Value>,
        value: Value,
    },
    Nope,
}

impl Default for Operation {
    fn default() -> Self {
        Self::Nope
    }
}

impl Operation {
    fn into_multiple(self) -> Vec<Operation> {
        match self {
            Self::Multiple(ops) => ops,
            op => vec![op]
        }
    }

    fn from_multiple(ops: Vec<Operation>) -> Self {
        match ops.len() {
            0 => Operation::Nope,
            1 => ops.into_iter().last().unwrap(),
            _ => Operation::Multiple(ops)
        }
    }
}

impl From<Vec<Operation>> for Operation {
    fn from(ops: Vec<Operation>) -> Self {
        Operation::from_multiple(ops)
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
                    let index: Box<_> = resolve_value(*index).into();
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
        ASTValue::FuncCall(name, params) =>
            Value::CallSiteRet {
                name,
                params: params.into_iter().map(resolve_value).collect(),
            },
        ASTValue::IfElse { cond, hold, otherwise } => {
            Value::Ite {
                cond: resolve_value(*cond).into(),
                lhs: resolve_value(*hold).into(),
                rhs: resolve_value(*otherwise).into(),
            }
        }
        ASTValue::IndexAccess(ident, params) =>
            Value::CallSiteIndex {
                name: ident,
                params: params.into_iter().map(resolve_value).collect(),
            },
        ASTValue::MemberAccess(_, _) => {
            // member access are dropped for now
            Value::Undefined
        }
        ASTValue::Unit => {
            Value::Undefined
        }
    }
}

pub fn member_resolute() {}

// pub fn split_into<T>(vec: Vec<T>, pos: usize) -> (Vec<T>, Vec<T>) {
//     unsafe {
//         let (first, len, cap) = vec.into_raw_parts();
//         let v1 = Vec::from_raw_parts(first, pos, pos);
//         let v2 = Vec::from_raw_parts(first + pos, len - pos, cap - pos);
//         (v1, v2)
//     }
// }

pub fn lift_ast_ctx(mut nodes: Vec<ASTNode>) -> Operation {
    let first_split = nodes.iter().position(|node| {
        match node {
            ASTNode::VarDecl(_, _) => true,
            _ => false
        }
    });
    match first_split {
        Some(idx) => {
            let (base, rest) = nodes.split_at(idx);
            let let_var = match rest.split_first() {
                Some((first, tail)) => {
                    match first {
                        ASTNode::VarDecl(ty, name) => {
                            Operation::LetVar(name.clone()
                                              , Either::Left(TypeUnresolved::from(ty.clone()))
                                              , Box::new(lift_ast_ctx(tail.to_vec())))
                        }
                        _ => Operation::Nope
                    }
                }
                None => Operation::Nope
            };
            if base.len() == 0 {
                let_var
            } else {
                let mut base: Vec<_> = base.iter().cloned().map(lift_ast).collect();
                base.push(let_var);
                Operation::Multiple(base)
            }
        }
        None => Operation::Multiple(nodes.into_iter().map(lift_ast).collect())
    }
}

pub fn create_assign(assignee: LeftPattern, assign_val: Value) -> Operation {
    match assignee {
        LeftPattern::Ident(ident) =>
            Operation::Assign(RefVar {
                path: PathName::Ident {
                    name: ident
                }
            }, assign_val),
        LeftPattern::MemberBitsAccess(name, accessor) => {
            let access_len = accessor.len() - 1;
            Operation::from_multiple(accessor.into_iter().zip(0..).map(|(acc, idx)| {
                let bit_var = Value::OpCast {
                    to: TypeUnresolved::Bit,
                    val: Value::OpExtract {
                        hi: Value::ConstInt((access_len - idx) as i64).into(),
                        lo: Value::ConstInt((access_len - idx) as i64).into(),
                        val: assign_val.clone().into(),
                    }.into(),
                };
                Operation::Assign(RefVar {
                    path: PathName::Ident {
                        name: format!("{}.{}", name, acc)
                    }
                }, bit_var)
            }).collect())
        }
        LeftPattern::BitsAccess(name, method) => {
            let (from, to, val) = match method {
                Either::Left(idx) => {
                    let idx = resolve_value(*idx);
                    (idx.clone(), idx,
                     Value::OpCast {
                         to: TypeUnresolved::Bits { width: Value::ConstInt(1).into() },
                         val: assign_val.into(),
                     })
                }
                Either::Right((from, to)) => {
                    (resolve_value(*from), resolve_value(*to), assign_val)
                }
            };
            // This should be later defined in Env or OCaml native
            Operation::CallSite {
                func: "replace_bits".to_string(),
                params: vec![
                    create_ident(name),
                    from,
                    to,
                    val
                ],
            }
        }
// Indices access are actually function calls
        LeftPattern::IndexAccess(name, params) => {
            Operation::CallSiteAssign {
                func: name,
                params: params.into_iter().map(resolve_value).collect(),
                value: assign_val,
            }
        }
        LeftPattern::Tuple(assignees) => {
            match assign_val {
                Value::Tuple(values) => {
                    assert_eq!(assignees.len(), values.len());
                    let ops: Vec<_> = assignees.into_iter().zip(values.into_iter())
                        .filter_map(|(lhs, rhs)| {
                            if let Either::Left(val) = lhs {
                                Some(create_assign(val, rhs))
                            } else {
                                None
                            }
                        }).collect();
                    Operation::from(ops)
                }
                v => {
                    panic!("Provided value {:?} cannot be deconstructed into a tuple", v)
                }
            }
        }
        _ => Default::default()
    }
}

pub fn lift_ast(node: ASTNode) -> Operation {
    match node {
        ASTNode::Multiple(ops) =>
            lift_ast_ctx(ops),
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
        ASTNode::LocalFuncCall(name, params) =>
            Operation::CallSite {
                func: name,
                params: params.into_iter().map(resolve_value).collect(),
            },
        ASTNode::VarAssign(pattern, value) => {
            create_assign(pattern, resolve_value(*value))
        }
        _ => Operation::Nope
    }
}
