/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use pyrefly_derive::TypeEq;
use pyrefly_derive::VisitMut;

use crate::types::types::Type;

#[derive(Clone, Debug, VisitMut, TypeEq, Eq, PartialEq)]
pub struct YieldResult {
    pub yield_ty: Type,
    pub send_ty: Type,
}

#[derive(Clone, Debug, VisitMut, TypeEq, Eq, PartialEq)]
pub struct YieldFromResult {
    pub yield_ty: Type,
    pub send_ty: Type,
    pub return_ty: Type,
}

impl YieldResult {
    pub fn recursive() -> Self {
        YieldResult {
            yield_ty: Type::any_implicit(),
            send_ty: Type::any_implicit(),
        }
    }

    pub fn any_error() -> Self {
        YieldResult {
            yield_ty: Type::any_error(),
            send_ty: Type::any_error(),
        }
    }
}

impl YieldFromResult {
    pub fn recursive() -> Self {
        YieldFromResult {
            yield_ty: Type::any_implicit(),
            send_ty: Type::any_implicit(),
            return_ty: Type::any_implicit(),
        }
    }

    pub fn any_error() -> Self {
        YieldFromResult {
            yield_ty: Type::any_error(),
            send_ty: Type::any_error(),
            return_ty: Type::any_error(),
        }
    }

    pub fn from_iterable(yield_ty: Type) -> Self {
        YieldFromResult {
            yield_ty,
            send_ty: Type::any_implicit(),
            return_ty: Type::any_implicit(),
        }
    }

    pub fn from_generator(generator: (Type, Type, Type)) -> Self {
        let (yield_ty, send_ty, return_ty) = generator;
        YieldFromResult {
            yield_ty,
            send_ty,
            return_ty,
        }
    }
}

impl Display for YieldResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "yield: {}, send: {}", self.yield_ty, self.send_ty)
    }
}

impl Display for YieldFromResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "yield: {}, send: {}, return: {}",
            self.yield_ty, self.send_ty, self.return_ty
        )
    }
}
