/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Something suitable as a base class.

use std::fmt;
use std::fmt::Display;

use ruff_python_ast::Expr;

use crate::types::types::Type;
use crate::util::display::commas_iter;

#[derive(Debug, Clone)]
pub enum BaseClass {
    #[expect(dead_code)] // Will be used in the future
    NamedTuple,
    #[expect(dead_code)] // Will be used in the future
    TypedDict,
    Generic(Vec<Type>),
    Protocol(Vec<Type>),
    Expr(Expr),
}

impl Display for BaseClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BaseClass::NamedTuple => write!(f, "NamedTuple"),
            BaseClass::TypedDict => write!(f, "TypedDict"),
            BaseClass::Generic(xs) => write!(f, "Generic[{}]", commas_iter(|| xs.iter())),
            BaseClass::Protocol(xs) => write!(f, "Protocol[{}]", commas_iter(|| xs.iter())),
            // TODO(stroxler): Do not use Debug here. Putting this off for now because I'm expecting
            // to refactor in upcoming commits until this is an implementation detail of `classes.rs`,
            // at which point we won't need Display at all anymore.
            BaseClass::Expr(s) => write!(f, "Expr({s:?})"),
        }
    }
}

impl BaseClass {
    pub fn visit_mut<'a>(&'a mut self, f: impl FnMut(&'a mut Type)) {
        match self {
            BaseClass::Generic(xs) | BaseClass::Protocol(xs) => xs.iter_mut().for_each(f),
            BaseClass::Expr(_) => {}
            BaseClass::NamedTuple | BaseClass::TypedDict => {}
        }
    }

    pub fn can_apply(&self) -> bool {
        matches!(self, BaseClass::Generic(_) | BaseClass::Protocol(_))
    }

    pub fn apply(&mut self, args: Vec<Type>) {
        match self {
            BaseClass::Generic(xs) | BaseClass::Protocol(xs) => {
                xs.extend(args);
            }
            _ => panic!("cannot apply base class"),
        }
    }
}
