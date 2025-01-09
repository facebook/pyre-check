/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::smallmap;

use crate::util::prelude::SliceExt;

#[derive(Clone, Debug)]
pub enum NarrowOp {
    Is(Box<Expr>),
    IsNot(Box<Expr>),
    Truthy,
    Falsy,
    Eq(Box<Expr>),
    NotEq(Box<Expr>),
    And(Vec<NarrowOp>),
    Or(Vec<NarrowOp>),
}

impl NarrowOp {
    pub fn negate(&self) -> Self {
        match self {
            Self::Is(e) => Self::IsNot(e.clone()),
            Self::IsNot(e) => Self::Is(e.clone()),
            Self::Eq(e) => Self::NotEq(e.clone()),
            Self::NotEq(e) => Self::Eq(e.clone()),
            Self::Truthy => Self::Falsy,
            Self::Falsy => Self::Truthy,
            Self::And(ops) => Self::Or(ops.map(|op| op.negate())),
            Self::Or(ops) => Self::And(ops.map(|op| op.negate())),
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

#[derive(Clone, Debug)]
pub struct NarrowOps(pub SmallMap<Name, (NarrowOp, TextRange)>);

impl NarrowOps {
    pub fn new() -> Self {
        Self(SmallMap::new())
    }

    pub fn negate(&self) -> Self {
        if self.0.len() == 1 {
            let (name, (op, range)) = self.0.first().unwrap();
            NarrowOps(smallmap! {
                name.clone() => (op.negate(), *range)
            })
        } else {
            // We don't have a way to model an `or` condition involving multiple variables (e.g., `x is None or not y`).
            NarrowOps::new()
        }
    }

    pub fn and(&mut self, name: Name, op: NarrowOp, range: TextRange) {
        if let Some((existing_op, _)) = self.0.get_mut(&name) {
            existing_op.and(op)
        } else {
            self.0.insert(name, (op, range));
        }
    }

    pub fn and_all(&mut self, other: NarrowOps) {
        for (name, (op, range)) in other.0.into_iter() {
            self.and(name, op, range);
        }
    }

    pub fn or_all(&mut self, other: NarrowOps) {
        // We can only model an `or` condition involving a single variable.
        if self.0.len() != 1 || other.0.len() != 1 {
            *self = NarrowOps::new();
            return;
        }
        let (self_name, (self_op, _)) = self.0.iter_mut().next().unwrap();
        let (other_name, (other_op, _)) = other.0.into_iter_hashed().next().unwrap();
        if *self_name != *other_name {
            *self = NarrowOps::new();
            return;
        }
        self_op.or(other_op);
    }
}
