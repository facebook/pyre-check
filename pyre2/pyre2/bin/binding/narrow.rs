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

#[derive(Clone, Debug)]
pub enum NarrowOp {
    Is(Box<Expr>),
    IsNot(Box<Expr>),
    Truthy,
    Falsy,
    Eq(Box<Expr>),
    NotEq(Box<Expr>),
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
        }
    }
}

#[derive(Clone, Debug)]
pub struct NarrowOps(pub SmallMap<Name, Vec<(NarrowOp, TextRange)>>);

impl NarrowOps {
    pub fn new() -> Self {
        Self(SmallMap::new())
    }

    pub fn negate(&self) -> Self {
        if self.0.len() == 1 {
            let (name, ops) = self.0.first().unwrap();
            if ops.len() == 1 {
                let (op, range) = &ops[0];
                return NarrowOps(smallmap! {
                    name.clone() => vec![(op.negate(), *range)]
                });
            }
        }
        // Negating multiple operations requires 'or' support, which we don't have.
        NarrowOps::new()
    }

    pub fn and(&mut self, name: Name, op: NarrowOp, range: TextRange) {
        if let Some(ops) = self.0.get_mut(&name) {
            ops.push((op, range));
        } else {
            self.0.insert(name, vec![(op, range)]);
        }
    }

    pub fn and_all(&mut self, other: NarrowOps) {
        for (name, ops) in other.0.into_iter() {
            if let Some(existing_ops) = self.0.get_mut(&name) {
                existing_ops.extend(ops);
            } else {
                self.0.insert(name, ops);
            }
        }
    }
}
