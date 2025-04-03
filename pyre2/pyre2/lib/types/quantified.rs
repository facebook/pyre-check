/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use parse_display::Display;
use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;

use crate::types::callable::ParamList;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::type_var::Restriction;
use crate::types::types::Type;
use crate::util::uniques::Unique;
use crate::util::uniques::UniqueFactory;

#[derive(
    Debug, Clone, PartialEq, Eq, TypeEq, Hash, Visit, Ord, PartialOrd, VisitMut
)]
pub struct QuantifiedInfo {
    pub kind: QuantifiedKind,
    pub default: Option<Type>,
    pub restriction: Restriction,
}

#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, Ord, PartialOrd, Hash
)]
pub struct Quantified {
    /// Unique identifier
    unique: Unique,
    info: Box<QuantifiedInfo>,
}

#[derive(
    Debug, Clone, Copy, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash, Display
)]
pub enum QuantifiedKind {
    TypeVar,
    ParamSpec,
    TypeVarTuple,
}

impl QuantifiedKind {
    pub fn empty_value(self) -> Type {
        match self {
            QuantifiedKind::TypeVar => Type::any_implicit(),
            QuantifiedKind::ParamSpec => Type::ParamSpecValue(ParamList::everything()),
            QuantifiedKind::TypeVarTuple => Type::any_tuple(),
        }
    }
}

impl Display for Quantified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.info.kind {
            QuantifiedKind::TypeVar => write!(f, "?_TypeVar"),
            QuantifiedKind::ParamSpec => write!(f, "?_ParamSpec"),
            QuantifiedKind::TypeVarTuple => write!(f, "?_TypeVarTuple"),
        }
    }
}

impl Quantified {
    pub fn new(unique: Unique, info: QuantifiedInfo) -> Self {
        Quantified {
            unique,
            info: Box::new(info),
        }
    }

    pub fn type_var(
        uniques: &UniqueFactory,
        default: Option<Type>,
        restriction: Restriction,
    ) -> Self {
        Self::new(
            uniques.fresh(),
            QuantifiedInfo {
                kind: QuantifiedKind::TypeVar,
                restriction,
                default,
            },
        )
    }

    pub fn param_spec(uniques: &UniqueFactory) -> Self {
        Self::new(
            uniques.fresh(),
            QuantifiedInfo {
                kind: QuantifiedKind::ParamSpec,
                restriction: Restriction::Unrestricted,
                default: None,
            },
        )
    }

    pub fn type_var_tuple(uniques: &UniqueFactory) -> Self {
        Self::new(
            uniques.fresh(),
            QuantifiedInfo {
                kind: QuantifiedKind::TypeVarTuple,
                restriction: Restriction::Unrestricted,
                default: None,
            },
        )
    }

    pub fn to_type(self) -> Type {
        Type::Quantified(self)
    }

    pub fn as_value(&self, stdlib: &Stdlib) -> ClassType {
        match self.info.kind {
            QuantifiedKind::TypeVar => stdlib.type_var(),
            QuantifiedKind::ParamSpec => stdlib.param_spec(),
            QuantifiedKind::TypeVarTuple => stdlib.type_var_tuple(),
        }
    }

    pub fn kind(&self) -> QuantifiedKind {
        self.info.kind
    }

    pub fn is_param_spec(&self) -> bool {
        matches!(self.info.kind, QuantifiedKind::ParamSpec)
    }

    pub fn is_type_var_tuple(&self) -> bool {
        matches!(self.info.kind, QuantifiedKind::TypeVarTuple)
    }

    pub fn as_gradual_type(&self) -> Type {
        // TODO(stroxler): Look into what it would take to do better when there's an upper-bound on a TypeVar.
        self.info.kind.empty_value()
    }
}
