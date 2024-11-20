/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::alt::answers::AnswersSolver;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;

/// `TypeOrder` provides a minimal API allowing `Subset` to request additional
/// information about types that may be required for solving bindings
///
/// This is needed for cases like the nominal type order and structural types where
/// the `Type` object itself does not contain enough information to determine
/// subset relations.
#[derive(Clone, Copy)]
pub struct TypeOrder<'a>(&'a AnswersSolver<'a>);

impl<'a> TypeOrder<'a> {
    pub fn new(solver: &'a AnswersSolver<'a>) -> Self {
        Self(solver)
    }

    pub fn stdlib(&self) -> &Stdlib {
        self.0.stdlib
    }

    pub fn has_superclass(&self, got: &Class, want: &Class) -> bool {
        self.0.has_superclass(got, want)
    }

    pub fn as_superclass(&self, class: &ClassType, want: &Class) -> Option<ClassType> {
        self.0.as_superclass(class, want)
    }
}
