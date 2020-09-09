#[derive(Parser)]
#[grammar = "arm_spec.pest"]
pub struct SpecParser;

use pest::Parser;
use pest::iterators::Pair;
use std::borrow::Borrow;
use rayon::iter::Either;
use std::collections::HashMap;
use crate::ASTValue::{Unit, BinOp, IfElse};
use crate::ASTNode::{Multiple, VarDecl, VarAssign, Nope, LocalFuncCall, CaseWhen};
use crate::TyName::{NameBind, Bits, Bit, Int};
use crate::LeftPattern::Ident;

pub(crate) type RuleList<'a> = Vec<Pair<'a, Rule>>;

macro_rules! get_inner {
    ($pair : expr) => {$pair.into_inner().collect::<RuleList>()}
}

macro_rules! inner_last {
    ($pair : expr) => {$pair.into_inner().last().unwrap()};
}

macro_rules! get_rule {
    ($list : expr, $index : expr) => {
        $list[$index].clone().into_inner().collect()
    };
}

#[derive(Debug, Clone)]
pub enum TyName {
    NameBind(String),
    Bits {
        width: ASTValue // the bit width is constrained by constants waiting for resolution
    },
    Bit,
    Int,
    // integer
    String,
    // literal
    Bot, // no type
}

#[derive(Debug, Clone)]
pub enum LeftPattern {
    Ident(String),
    Tuple(Vec<Either<LeftPattern, ()>>),
    IndexAccess(String, Value),
    BitsAccess(String, Either<Value, (Value, Value)>),
    MemberAccess(String, Box<LeftPattern>),
}

type Value = Box<ASTValue>;
type ConstValue = Either<i64, String>;

#[derive(Debug, Clone)]
pub enum ASTValue {
    BinOp {
        lhs: Value,
        rhs: Value,
        op: String,
    },
    IfElse {
        cond: Value,
        hold: Value,
        otherwise: Value,
    },
    IndexAccess(String, Value),
    BitsAccess(String, Either<Value, (Value, Value)>),
    MemberAccess(String, Value),
    FuncCall(String, Vec<ASTValue>),
    Ident(String),
    Constant(ConstValue),
    Tuple(Vec<ASTValue>),
    Unit, // Equiv to unit tuple
}

impl ASTValue {
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Multiple(Vec<ASTNode>),
    VarDecl(TyName, String),
    VarAssign(LeftPattern, Value),
    LocalFuncCall(String, Vec<ASTValue>),
    // Just like FuncCall
    IfElse {
        cond: Value,
        hold: Box<ASTNode>,
        otherwise: Box<ASTNode>,
    },
    CaseWhen {
        val: Value,
        cases: HashMap<Either<ConstValue, String>, ASTNode>,
    },
    Nope, // do nothing
}

pub fn parse_operation(sema: &str) {
    let tree = SpecParser::parse(
        Rule::arm_spec,
        sema,
    ).unwrap_or_else(|e| panic!("{}", e)).collect::<RuleList>();
    walk_pairs(tree[0].clone().into_inner());
}

pub fn walk_pairs(pairs: pest::iterators::Pairs<Rule>) -> Vec<ASTNode> {
    pairs.into_iter().map(|it| walk_node(it)).collect()
}

fn load_value_list(body: RuleList) -> Vec<ASTValue> {
    body.into_iter().map(|it| walk_value_expr(get_inner!(it))).collect()
}

fn walk_bit_accessor(comp: RuleList) -> Either<Value, (Value, Value)> {
    if comp.len() == 2 {
        Either::Left(walk_value_expr(get_rule!(comp, 1)).boxed())
    } else {
        Either::Right((
            walk_value_expr(get_rule!(comp, 1)).boxed(),
            walk_value_expr(get_rule!(comp, 2)).boxed()
        ))
    }
}

pub fn walk_bin_op_left(body: RuleList) -> ASTValue {
    let left_op = body[0].clone();
    match left_op.as_rule() {
        Rule::right_tuple => {
            let comp = get_inner!(left_op);
            ASTValue::Tuple(load_value_list(get_rule!(comp, 0)))
        }
        Rule::indices_access => {
            let comp = get_inner!(left_op);
            ASTValue::IndexAccess(comp[0].as_str().to_string()
                                  , Box::new(walk_value_expr(get_rule!(comp, 1))))
        }
        Rule::bits_access => {
            let comp = get_inner!(left_op);
            let id = comp[0].as_str().to_string();
            let index = walk_bit_accessor(comp);
            ASTValue::BitsAccess(
                id,
                index,
            )
        }
        Rule::member_access_right => {
            let comp = get_inner!(left_op);
            let id = comp[0].as_str().to_string();
            ASTValue::MemberAccess(id, walk_value_expr(get_rule!(comp, 1)).boxed())
        }
        Rule::function_call => {
            let comp = get_inner!(left_op);
            ASTValue::FuncCall(comp[0].as_str().to_string(),
                               load_value_list(get_rule!(comp, 1)))
        }
        Rule::constant_val => {
            let comp = inner_last!(left_op);
            if comp.as_rule() == Rule::number {
                ASTValue::Constant(Either::Left(comp.as_str().parse::<i64>().unwrap()))
            } else {
                let str = comp.as_str();
                ASTValue::Constant(Either::Right(str[1..str.len() - 1].to_string()))
            }
        }
        Rule::ident => ASTValue::Ident(left_op.as_str().to_string()),
        _ => Unit
    }
}

fn walk_const_or_ident(body: RuleList) -> Either<Either<i64, String>, String> {
    let left_op = body[0].clone();
    match left_op.as_rule() {
        Rule::constant_val => {
            let comp = inner_last!(left_op);
            if comp.as_rule() == Rule::number {
                Either::Left(Either::Left(comp.as_str().parse::<i64>().unwrap()))
            } else {
                let str = comp.as_str();
                Either::Left(Either::Right(str[1..str.len() - 1].to_string()))
            }
        }
        Rule::ident => Either::Right(left_op.as_str().to_string()),
        _ => panic!("Unexpected input")
    }
}

fn walk_value_expr(body: RuleList) -> ASTValue {
    let mut priority = HashMap::<&str, (i32, bool)>::new();
    priority.insert("<", (5, true));
    priority.insert("<=", (5, true));
    priority.insert("==", (5, true));
    priority.insert(">=", (5, true));
    priority.insert(">", (5, true));
    priority.insert("!=", (5, true));
    priority.insert(":", (6, true));
    priority.insert("+", (10, true));
    priority.insert("-", (10, true));
    priority.insert("*", (20, true));
    priority.insert("/", (20, true));
    walk_ast_value(body, 0, &priority).0
}

// with prec climber
pub fn walk_ast_value<'a>(body: RuleList<'a>, prec_val: i32, priority: &HashMap<&str, (i32, bool)>) -> (ASTValue, Option<RuleList<'a>>) {
    let primary = body[0].clone();
    let rule = primary.as_rule();
    let composition = get_inner!(primary);
    match rule {
        Rule::if_else_value => {
            (IfElse {
                cond: Box::new(walk_value_expr(get_rule!(composition, 0))),
                hold: Box::new(walk_value_expr(get_rule!(composition, 1))),
                otherwise: Box::new(walk_value_expr(get_rule!(composition, 2))),
            }, None)
        }
        Rule::bin_op_left => {
            (walk_bin_op_left(composition), None)
        }
        Rule::right_bin_op => { // prec climber
            let lhs = walk_bin_op_left(get_rule!(composition, 0));
            let mut last_op = composition[1].clone();
            let mut op = last_op.as_str().to_string();
            let mut prior = priority.get(&op[..]).expect("Operator not found !");
            let mut result = lhs;
            let mut remnants: RuleList = get_rule!(composition, 2);
            while prior.0 >= prec_val {
                //println!("{} {} {:?}", prec_val, op, prior);
                let next_prior = if prior.1 { prior.0 + 1 } else { prior.0 };
                let rhs = walk_ast_value(remnants.clone(), next_prior, priority);
                let suc = rhs.1;
                result = BinOp {
                    lhs: Box::new(result),
                    rhs: Box::new(rhs.0),
                    op,
                };
                match suc {
                    None => {
                        return (result, None);
                    }
                    Some(expr) => {
                        remnants = expr;
                        last_op = remnants[0].clone();
                        op = last_op.as_str().to_string();
                        prior = priority.get(&op[..]).expect("Operator not found !");
                        remnants = remnants[1..].to_vec();
                    }
                }
            }
            if remnants.len() > 0 {
                let mut ret_vec = vec![last_op];
                ret_vec.extend(remnants.into_iter());
                (result, Some(ret_vec))
            } else {
                (result, None)
            }
        }
        _ => (Unit, None)
    }
}

pub fn walk_left_pattern(node: Pair<Rule>) -> LeftPattern {
    let inner = inner_last!(node);
    match inner.as_rule() {
        Rule::member_access => {
            let comp = get_inner!(inner);
            LeftPattern::MemberAccess(comp[0].as_str().to_string()
                                      , Box::new(walk_left_pattern(comp[1].clone())))
        }
        Rule::indices_access => {
            let comp = get_inner!(inner);
            LeftPattern::IndexAccess(comp[0].as_str().to_string()
                                     , Box::new(walk_value_expr(get_rule!(comp, 1))))
        }
        Rule::bits_access => {
            let comp = get_inner!(inner);
            let id = comp[0].as_str().to_string();
            let index = walk_bit_accessor(comp);
            LeftPattern::BitsAccess(
                id,
                index,
            )
        }
        Rule::left_tuple => {
            let comp = get_inner!(inner);
            LeftPattern::Tuple(comp.into_iter().map(|it| {
                match it.as_rule() {
                    Rule::ref_value_drop => Either::Right(()),
                    Rule::ref_value => Either::Left(walk_left_pattern(it)),
                    _ => panic!("unexpected pattern composition")
                }
            }).collect())
        }
        Rule::ident => LeftPattern::Ident(inner.as_str().to_string()),
        _ => LeftPattern::Tuple(vec![])
    }
}

pub fn walk_ty_name(node: Pair<Rule>) -> TyName {
    let inner = inner_last!(node);
    match inner.as_rule() {
        Rule::ident => {
            match inner.as_str() {
                "bit" => Bit,
                "integer" => Int,
                other => NameBind(other.to_string())
            }
        }
        Rule::right_value => {
            Bits {
                width: walk_value_expr(inner.into_inner().collect())
            }
        }
        _ => {
            panic!("unexpected typename token")
        }
    }
}

pub fn walk_single_expr(node: Pair<Rule>) -> ASTNode {
    let inner = inner_last!(node);
    match inner.as_rule() {
        Rule::var_decl_assign => {
            let composition = get_inner!(inner);
            Multiple(vec![
                VarDecl(walk_ty_name(composition[0].clone())
                        , composition[1].as_str().to_string()),
                VarAssign(Ident(composition[1].as_str().to_string()),
                          Box::new(walk_value_expr(get_rule!(composition, 2))))
            ])
        }
        Rule::var_decl => {
            let composition = get_inner!(inner);
            VarDecl(walk_ty_name(composition[0].clone())
                    , composition[1].as_str().to_string())
        }
        Rule::var_assign => {
            let composition = get_inner!(inner);
            VarAssign(walk_left_pattern(composition[0].clone()),
                      walk_value_expr(get_rule!(composition, 1)).boxed())
        }
        Rule::function_call => {
            let composition = get_inner!(inner);
            LocalFuncCall(composition[0].as_str().to_string(),
                          load_value_list(get_rule!(composition, 1)))
        }
        _ => Nope,
    }
}

pub fn walk_single_or_block(node: Pair<Rule>) -> ASTNode {
    println!("{:?}", node);
    match node.as_rule() {
        Rule::single_expr => walk_single_expr(node),
        Rule::indented_block => ASTNode::Multiple(
            node.into_inner().map(|it| walk_node(it)).collect()
        ),
        _ => Nope
    }
}

pub fn walk_node(node: Pair<Rule>) -> ASTNode {
    println!("{:?}", node);
    if node.as_rule() == Rule::base_expr {
        let inner = inner_last!(node);
        match inner.as_rule() {
            Rule::single_expr => dbg!(walk_single_expr(inner)),
            Rule::if_else_expr => {
                let comp = get_inner!(inner);
                let cond = walk_value_expr(get_rule!(comp, 0));
                let else_block = if comp.len() > 2 {
                    let else_ = get_inner!(comp[2].clone());
                    walk_single_or_block(else_[0].clone())
                } else { Nope };
                let holds_block = {
                    let holds = get_inner!(comp[1].clone());
                    walk_single_or_block(holds[0].clone())
                };
                dbg!(ASTNode::IfElse {
                    cond: Box::new(cond),
                    hold: Box::new(holds_block),
                    otherwise: Box::new(else_block),
                })
            }
            Rule::case_expr => {
                let comp = get_inner!(inner);
                let cond = walk_value_expr(get_rule!(comp, 0));
                let trail = comp[1].clone().into_inner().last().unwrap().into_inner();
                let mut calls = HashMap::new();
                trail.for_each(|pair| {
                    let inner = get_inner!(pair);
                    let value = walk_const_or_ident(get_rule!(inner, 0));
                    let sema = walk_single_or_block(inner[1].clone());
                    calls.insert(value, sema);
                });
                CaseWhen {
                    val: cond.boxed(),
                    cases: calls,
                }
            }
            _ => Nope
        }
    } else { Nope }
}
