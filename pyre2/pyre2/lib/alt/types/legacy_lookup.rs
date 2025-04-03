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

use crate::types::types::TParamInfo;
use crate::types::types::Type;

/// Python's legacy (pre-PEP 695) type variable syntax is not syntactic at all, it requires
/// name resolution of global variables plus multiple sets of rules for when a global that
/// is a type variable placeholder is allowed to be used as a type parameter.
///
/// This type represents the result of such a lookup: given a name appearing in a function or
/// a class, we either determine that the name is *not* a type variable and return the type
/// for the name, or we determine that it is one and create a `Quantified` that
/// represents that variable as a type parameter.
#[derive(Debug, Clone, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum LegacyTypeParameterLookup {
    Parameter(TParamInfo),
    NotParameter(Type),
}

impl Display for LegacyTypeParameterLookup {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Parameter(p) => write!(f, "{}", p.quantified.name()),
            Self::NotParameter(ty) => write!(f, "{ty}"),
        }
    }
}

impl LegacyTypeParameterLookup {
    pub fn parameter(&self) -> Option<&TParamInfo> {
        match self {
            Self::Parameter(p) => Some(p),
            Self::NotParameter(_) => None,
        }
    }
}
