/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use parse_display::Display;

use crate::types::callable::ParamList;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::util::uniques::Unique;
use crate::util::uniques::UniqueFactory;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Quantified {
    /// Unique identifier
    unique: Unique,
    kind: QuantifiedKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Display)]
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
            QuantifiedKind::TypeVarTuple => Type::any_implicit(), // TODO
        }
    }
}

impl Display for Quantified {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "?_")
    }
}

impl Quantified {
    pub fn new(uniques: &UniqueFactory, kind: QuantifiedKind) -> Self {
        Quantified {
            unique: uniques.fresh(),
            kind,
        }
    }

    pub fn type_var(uniques: &UniqueFactory) -> Self {
        Quantified::new(uniques, QuantifiedKind::TypeVar)
    }

    pub fn param_spec(uniques: &UniqueFactory) -> Self {
        Quantified::new(uniques, QuantifiedKind::ParamSpec)
    }

    pub fn type_var_tuple(uniques: &UniqueFactory) -> Self {
        Quantified::new(uniques, QuantifiedKind::TypeVarTuple)
    }

    pub fn to_type(self) -> Type {
        Type::Quantified(self)
    }

    pub fn as_value(&self, stdlib: &Stdlib) -> ClassType {
        match self.kind {
            QuantifiedKind::TypeVar => stdlib.type_var(),
            QuantifiedKind::ParamSpec => stdlib.param_spec(),
            QuantifiedKind::TypeVarTuple => stdlib.type_var_tuple(),
        }
    }

    pub fn kind(&self) -> QuantifiedKind {
        self.kind
    }

    pub fn is_param_spec(&self) -> bool {
        matches!(self.kind, QuantifiedKind::ParamSpec)
    }
}
