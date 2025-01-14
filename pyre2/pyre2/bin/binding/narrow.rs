/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_python_ast::BoolOp;
use ruff_python_ast::CmpOp;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBoolOp;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprCompare;
use ruff_python_ast::ExprName;
use ruff_python_ast::ExprNamed;
use ruff_python_ast::ExprUnaryOp;
use ruff_python_ast::UnaryOp;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::smallmap;

use crate::types::types::Type;
use crate::util::prelude::SliceExt;

#[derive(Clone, Debug)]
pub enum NarrowVal {
    Expr(Box<Expr>),
    #[expect(dead_code)]
    Type(Box<Type>, TextRange),
}

impl NarrowVal {
    pub fn range(&self) -> TextRange {
        match self {
            Self::Expr(e) => e.range(),
            Self::Type(_, r) => *r,
        }
    }
}

#[derive(Clone, Debug)]
pub enum NarrowOp {
    Is(NarrowVal),
    IsNot(NarrowVal),
    Truthy,
    Falsy,
    Eq(NarrowVal),
    NotEq(NarrowVal),
    And(Vec<NarrowOp>),
    Or(Vec<NarrowOp>),
    IsInstance(NarrowVal),
    IsNotInstance(NarrowVal),
    /// (func, args) for a function call that may narrow the type of its first argument.
    Call(NarrowVal, Arguments),
    NotCall(NarrowVal, Arguments),
}

impl NarrowOp {
    pub fn negate(&self) -> Self {
        match self {
            Self::Is(e) => Self::IsNot(e.clone()),
            Self::IsNot(e) => Self::Is(e.clone()),
            Self::IsInstance(e) => Self::IsNotInstance(e.clone()),
            Self::IsNotInstance(e) => Self::IsInstance(e.clone()),
            Self::Eq(e) => Self::NotEq(e.clone()),
            Self::NotEq(e) => Self::Eq(e.clone()),
            Self::Truthy => Self::Falsy,
            Self::Falsy => Self::Truthy,
            Self::And(ops) => Self::Or(ops.map(|op| op.negate())),
            Self::Or(ops) => Self::And(ops.map(|op| op.negate())),
            Self::Call(f, args) => Self::NotCall(f.clone(), args.clone()),
            Self::NotCall(f, args) => Self::Call(f.clone(), args.clone()),
        }
    }

    fn and(&mut self, other: Self) {
        match self {
            Self::And(ops) => ops.push(other),
            _ => *self = Self::And(vec![self.clone(), other]),
        }
    }

    fn or(&mut self, other: Self) {
        match self {
            Self::Or(ops) => ops.push(other),
            _ => *self = Self::Or(vec![self.clone(), other]),
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct NarrowOps(pub SmallMap<Name, (NarrowOp, TextRange)>);

impl NarrowOps {
    pub fn new() -> Self {
        Self(SmallMap::new())
    }

    pub fn negate(&self) -> Self {
        if self.0.len() == 1 {
            let (name, (op, range)) = self.0.first().unwrap();
            Self(smallmap! {
                name.clone() => (op.negate(), *range)
            })
        } else {
            // We don't have a way to model an `or` condition involving multiple variables (e.g., `x is None or not y`).
            Self::new()
        }
    }

    fn and(&mut self, name: Name, op: NarrowOp, range: TextRange) {
        if let Some((existing_op, _)) = self.0.get_mut(&name) {
            existing_op.and(op)
        } else {
            self.0.insert(name, (op, range));
        }
    }

    pub fn and_all(&mut self, other: Self) {
        for (name, (op, range)) in other.0.into_iter() {
            self.and(name, op, range);
        }
    }

    pub fn or_all(&mut self, other: Self) {
        // We can only model an `or` condition involving a single variable.
        if self.0.len() != 1 || other.0.len() != 1 {
            *self = Self::new();
            return;
        }
        let (self_name, (self_op, _)) = self.0.iter_mut().next().unwrap();
        let (other_name, (other_op, _)) = other.0.into_iter_hashed().next().unwrap();
        if *self_name != *other_name {
            *self = Self::new();
            return;
        }
        self_op.or(other_op);
    }

    pub fn from_expr(test: Option<Expr>) -> Self {
        match test {
            Some(Expr::Compare(ExprCompare {
                range: _,
                left,
                ops: cmp_ops,
                comparators,
            })) => {
                let mut narrow_ops = Self::new();
                let names = expr_to_names(&left);
                let ops = cmp_ops
                    .iter()
                    .zip(comparators)
                    .filter_map(|(cmp_op, right)| {
                        let range = right.range();
                        let op = match cmp_op {
                            CmpOp::Is => NarrowOp::Is(NarrowVal::Expr(Box::new(right))),
                            CmpOp::IsNot => NarrowOp::IsNot(NarrowVal::Expr(Box::new(right))),
                            CmpOp::Eq => NarrowOp::Eq(NarrowVal::Expr(Box::new(right))),
                            CmpOp::NotEq => NarrowOp::NotEq(NarrowVal::Expr(Box::new(right))),
                            _ => {
                                return None;
                            }
                        };
                        Some((op, range))
                    });

                for (op, range) in ops {
                    for name in names.iter() {
                        narrow_ops.and(name.id.clone(), op.clone(), range);
                    }
                }
                narrow_ops
            }
            Some(Expr::BoolOp(ExprBoolOp {
                range: _,
                op: BoolOp::And,
                values,
            })) => {
                let mut narrow_ops = Self::new();
                for e in values {
                    narrow_ops.and_all(Self::from_expr(Some(e)))
                }
                narrow_ops
            }
            Some(Expr::BoolOp(ExprBoolOp {
                range: _,
                op: BoolOp::Or,
                values,
            })) => {
                let mut exprs = values.into_iter();
                if let Some(first_val) = exprs.next() {
                    let mut narrow_ops = Self::from_expr(Some(first_val));
                    for next_val in exprs {
                        narrow_ops.or_all(Self::from_expr(Some(next_val)));
                    }
                    narrow_ops
                } else {
                    Self::new()
                }
            }
            Some(Expr::UnaryOp(ExprUnaryOp {
                range: _,
                op: UnaryOp::Not,
                operand: box e,
            })) => Self::from_expr(Some(e)).negate(),
            Some(Expr::Call(ExprCall {
                range,
                func,
                arguments:
                    ref args @ Arguments {
                        range: _,
                        args: ref posargs,
                        keywords: _,
                    },
            })) if !posargs.is_empty() => {
                // This may be a function call that narrows the type of its first argument. Record
                // it as a possible narrowing operation that we'll resolve in the answers phase.
                let mut narrow_ops = Self::new();
                for name in expr_to_names(&posargs[0]) {
                    narrow_ops.and(
                        name.id,
                        NarrowOp::Call(NarrowVal::Expr(func.clone()), args.clone()),
                        range,
                    );
                }
                narrow_ops
            }
            Some(e) => {
                let mut narrow_ops = Self::new();
                for name in expr_to_names(&e) {
                    narrow_ops.and(name.id, NarrowOp::Truthy, e.range());
                }
                narrow_ops
            }
            None => Self::new(),
        }
    }
}

fn expr_to_names(expr: &Expr) -> Vec<ExprName> {
    match expr {
        Expr::Name(name) => vec![name.clone()],
        Expr::Named(ExprNamed {
            range: _,
            target,
            value,
        }) => expr_to_names(target)
            .into_iter()
            .chain(expr_to_names(value))
            .collect(),
        _ => Vec::new(),
    }
}
