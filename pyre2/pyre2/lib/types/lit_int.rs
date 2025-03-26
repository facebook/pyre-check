/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;
use std::fmt;
use std::fmt::Display;
use std::ops::Neg;

use num_bigint::BigInt;
use num_traits::cast::ToPrimitive;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;

use crate::types::equality::TypeEq;
use crate::types::types::Type;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LitInt(LitIntInner);

impl Display for LitInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            LitIntInner::Small(x) => write!(f, "{}", x),
            LitIntInner::Big(x) => write!(f, "{}", x),
        }
    }
}

// No types contained inside.
impl Visit<Type> for LitInt {
    fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a Type)) {}
}
impl VisitMut<Type> for LitInt {
    fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut Type)) {}
}

impl TypeEq for LitInt {}

impl Ord for LitIntInner {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Small(x), Self::Small(y)) => x.cmp(y),
            (Self::Small(x), Self::Big(y)) => BigInt::from(*x).cmp(y),
            (Self::Big(x), Self::Small(y)) => (**x).cmp(&BigInt::from(*y)),
            (Self::Big(x), Self::Big(y)) => x.cmp(y),
        }
    }
}

impl PartialOrd for LitIntInner {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug, Clone, Visit, VisitMut, PartialEq, Eq, Hash)]
enum LitIntInner {
    /// Small values, without a heap allocation.
    Small(i64),
    /// Guaranteed not to be within the range of i64.
    Big(Box<BigInt>),
}

impl LitInt {
    pub fn new(x: i64) -> Self {
        Self(LitIntInner::Small(x))
    }

    fn new_big(x: BigInt) -> Self {
        match x.to_i64() {
            Some(x) => Self(LitIntInner::Small(x)),
            None => Self(LitIntInner::Big(Box::new(x))),
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Self(LitIntInner::Small(x)) => Some(*x),
            Self(LitIntInner::Big(_)) => None,
        }
    }

    pub fn negate(&self) -> Self {
        match &self.0 {
            LitIntInner::Small(x) => match x.checked_neg() {
                Some(x) => Self::new(x),
                None => Self(LitIntInner::Big(Box::new(BigInt::from(*x).neg()))),
            },
            LitIntInner::Big(x) => Self::new_big(x.clone().neg()),
        }
    }

    pub fn invert(&self) -> LitInt {
        match &self.0 {
            LitIntInner::Small(x) => Self(LitIntInner::Small(!*x)),
            LitIntInner::Big(x) => Self::new_big(!(**x).clone()),
        }
    }

    pub fn as_bool(&self) -> bool {
        match &self.0 {
            LitIntInner::Small(x) => *x != 0,
            LitIntInner::Big(_) => true,
        }
    }
}
