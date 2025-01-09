/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::Expr;

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
    pub fn negate(self) -> Self {
        match self {
            Self::Is(e) => Self::IsNot(e),
            Self::IsNot(e) => Self::Is(e),
            Self::Eq(e) => Self::NotEq(e),
            Self::NotEq(e) => Self::Eq(e),
            Self::Truthy => Self::Falsy,
            Self::Falsy => Self::Truthy,
        }
    }
}
