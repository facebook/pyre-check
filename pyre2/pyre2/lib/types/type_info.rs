/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;

use crate::types::types::Type;

/// The `TypeInfo` datatype represents type information associated with a
/// name or expression in a control flow context.
///
/// This is distinct from `Type` because expressions and bound names can have
/// additional type information in them - in particular, knowledge of type
/// narrowing - that is not part of the composable type system because it *only*
/// applies to top-level expressions and names.
#[derive(
    Debug, Clone, PartialEq, Eq, Visit, VisitMut, TypeEq, PartialOrd, Ord, Hash
)]
pub struct TypeInfo {
    pub ty: Type,
}

impl TypeInfo {
    pub fn of_ty(ty: Type) -> Self {
        Self { ty }
    }

    pub fn with_ty(self, ty: Type) -> Self {
        Self { ty }
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn into_ty(self) -> Type {
        self.ty
    }

    pub fn arc_clone(self: Arc<Self>) -> Self {
        Arc::unwrap_or_clone(self)
    }

    pub fn arc_clone_ty(self: Arc<Self>) -> Type {
        self.arc_clone().into_ty()
    }
}
