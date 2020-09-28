use crate::env::DefEnv;
use crate::resolve::{Operation, RefVar, PathName};
use crate::gen::Code::*;
use crate::resolve::Value;
use crate::resolve::TypeUnresolved;
use std::str::FromStr;

#[derive(Debug)]
pub enum Code {
    Line(String),
    Empty,
    Indent {
        leading: String,
        line: Box<Code>,
    },
    Multiple {
        lines: Vec<Code>
    },
}

impl From<&str> for Code {
    fn from(line: &str) -> Self {
        Code::Line(line.to_string())
    }
}

#[derive(Clone)]
pub struct FuncCall {
    ident: String,
    params: Vec<Value>,
    ret_to: RefVar,
}

#[derive(Default)]
pub struct CallRets {
    calls: Vec<FuncCall>,
    rid: usize,
}

pub fn extract_value_funcs(value: Value, calls: &mut CallRets) -> Value {
    match value {
        Value::Tuple(values) =>
            Value::Tuple(
                values.into_iter()
                    .map(|v| extract_value_funcs(v, calls)).collect()
            ),
        Value::CallSiteRet { name, params } => {
            let rid = format!("ret{}", calls.rid);
            let var = RefVar {
                path: PathName::Ident { name: rid }
            };
            calls.rid += 1;
            calls.calls.push(FuncCall {
                ident: name,
                params,
                ret_to: var.clone(),
            });
            Value::Var(var)
        }
        Value::CallSiteIndex { name, params } => {
            let rid = format!("index_ret{}", calls.rid);
            let var = RefVar {
                path: PathName::Ident { name: rid }
            };
            calls.rid += 1;
            calls.calls.push(FuncCall {
                ident: name,
                params,
                ret_to: var.clone(),
            });
            Value::Var(var)
        }
        Value::Operated { op, lhs, rhs } => {
            let lhs = extract_value_funcs(*lhs, calls).into();
            let rhs = extract_value_funcs(*rhs, calls).into();
            Value::Operated { op, lhs, rhs }
        }
        Value::OpExtract { hi, lo, val } => {
            let hi = extract_value_funcs(*hi, calls).into();
            let lo = extract_value_funcs(*lo, calls).into();
            let val = extract_value_funcs(*val, calls).into();
            Value::OpExtract { hi, lo, val }
        }
        Value::OpCast { to, val } => {
            let val = extract_value_funcs(*val, calls).into();
            Value::OpCast { to, val }
        }
        Value::Ite { cond, lhs, rhs } => {
            let cond = extract_value_funcs(*cond, calls).into();
            let lhs = extract_value_funcs(*lhs, calls).into();
            let rhs = extract_value_funcs(*rhs, calls).into();
            Value::Ite { cond, lhs, rhs }
        }
        v => v
    }
}

fn path_name(path: PathName) -> String {
    match path {
        PathName::Ident { name } => name,
        PathName::ConstIdent { name } => name,
        PathName::Ref { base, ptr } =>
            format!("{}.{}", base, path_name(*ptr))
    }
}

pub fn ref_var_name(var: RefVar) -> String {
    path_name(var.path)
}

pub fn attach_calls(def: &DefEnv, lets: CallRets, tail: Code) -> Code {
    let multiple = Code::Multiple {
        lines: {
            let mut fcalls : Vec<_> = lets.calls.iter().cloned().map(|call| {
                let mut vec = call.params;
                vec.push(Value::Var(call.ret_to));
                generate_fn(def, call.ident, vec)
            }).collect();
            fcalls.push(tail);
            fcalls
        }
    };
    lets.calls.iter().fold(multiple, |prev, call| {
        let line = format!("local_var >>= fun {} ->", ref_var_name(call.ret_to.clone()));
        match prev {
            Code::Empty => Code::Line(line),
            prev => Code::Indent { leading: line, line: Box::new(prev) }
        }
    })
}

pub fn generate_value(value: Value) -> String {
    match value {
        Value::Var(var) => ref_var_name(var),
        Value::ConstInt(i) => { format!("!${}", i) }
        Value::ConstBit(bit) => {
            if bit {
                "b1".to_string()
            } else {
                "b0".to_string()
            }
        }
        Value::CallSiteRet { name: _, params: _ } => {
            panic!("unexpected value type, calls should be resolved earlier")
        }
        Value::CallSiteIndex { name: _, params: _ } => {
            panic!("unexpected value type, calls should be resolved earlier")
        }
        Value::Operated { op, lhs, rhs } => {
            let lhs = generate_value(*lhs);
            let rhs = generate_value(*rhs);
            format!("{} {} {}", lhs, op, rhs)
        }
        Value::OpExtract { hi, lo, val } => {
            format!("extract {} {} {}", generate_value(*hi), generate_value(*lo), generate_value(*val))
        }
        Value::OpCast { to, val } => {
            let val = generate_value(*val);
            match to {
                TypeUnresolved::Bit => format!("msb {}", val),
                TypeUnresolved::Bits { width } => format!("ite {} one zero", val),
                _ => val
            }
        }
        Value::Ite { cond, lhs, rhs } => {
            format!("ite {} {} {}", generate_value(*cond), generate_value(*lhs), generate_value(*rhs))
        }
        Value::Tuple(values) => {
            let values: Vec<_> = values.into_iter().map(generate_value).collect();
            format!("({})", values.join(", "))
        }
        Value::Undefined => "bot".to_string()
    }
}

pub fn generate_insn(def: &DefEnv, op: Operation) -> Code {
    let mut calls = CallRets::default();
    match op {
        Operation::Assign(lhs, rhs) => {
            let rhs = extract_value_funcs(rhs, &mut calls);
            let rhs = generate_value(rhs);
            attach_calls(def, calls,
                         Code::Line(format!("data @@ {} := {}", ref_var_name(lhs), rhs)))
        }
        Operation::Jmp(to) => {
            let to = extract_value_funcs(to, &mut calls);
            let to = generate_value(to);
            attach_calls(def, calls,
                         Code::Line(format!("ctrl @@ {}", to)))
        }
        Operation::Multiple(ops) => {
            Code::Multiple {
                lines: ops.into_iter().map(|line| generate_insn(def, line)).collect(),
            }
        }
        Operation::MuxOperation(cond, lhs, rhs) => {
            let cond = extract_value_funcs(cond, &mut calls);
            let cond = generate_value(cond);
            let lhs = Code::Multiple { lines: lhs.into_iter().map(|line| generate_insn(def, line)).collect() };
            let rhs = Code::Multiple { lines: rhs.into_iter().map(|line| generate_insn(def, line)).collect() };
            attach_calls(def, calls, Code::Multiple {
                lines: vec![
                    Code::Indent {
                        leading: format!("when_else_ {}", cond),
                        line: Box::new(lhs),
                    },
                    rhs
                ]
            })
        }
        Operation::Nope => Code::from("bot"),

        Operation::LetVar(name, value, trail) => {
            Code::Indent {
                leading: format!("local_var >>= fun {} ->", name),
                line: Box::new(generate_insn(def, *trail)),
            }
        }
        Operation::CallSite { func, params } => {
            generate_fn(def, func, params)
        }
        Operation::CallSiteAssign { func, mut params, value } => {
            params.push(value);
            generate_fn(def, func, params)
        }
    }
}

fn generate_fn(def: &DefEnv, func: String, params: Vec<Value>) -> Code {
    let mut calls = CallRets::default();
    let params: Vec<_> = params.into_iter().map(|val| extract_value_funcs(val, &mut calls)).collect();
    let params: Vec<_> = params.into_iter().map(generate_value).collect();
    Code::Line(format!("{} {}", func, params.join(" ")))
}
