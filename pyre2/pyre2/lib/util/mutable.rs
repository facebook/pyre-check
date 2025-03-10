/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! A trait for values that have a mutable and immutable part.

use std::hash::Hash;
use std::hash::Hasher;

use dupe::Dupe;

/// A trait for values that have a mutable and immutable part.
pub trait Mutable {
    /// Equality for the immutable part of the value.
    /// Values which return `true` must also produce the same immutable hash.
    fn immutable_eq(&self, other: &Self) -> bool;

    /// Hash for the immutable part of the value.
    fn immutable_hash<H: Hasher>(&self, state: &mut H);

    /// Equality for the mutable part of the value.
    /// Values which return `true` must also produce the same mutable hash.
    fn mutable_eq(&self, other: &Self) -> bool;

    /// Hash for the mutable part of the value.
    fn mutable_hash<H: Hasher>(&self, state: &mut H);

    /// Mutate the mutable part of the value.
    /// If `immutable_eq` returns `true` for both values,
    /// then after `mutate` the values should be fully equal.
    fn mutate(&self, x: &Self);
}

/// A wrapper around a `Mutable` value such that the `Hash`/`Eq`
/// implementations are based on the immutable part of the value.
#[derive(Clone, Dupe, Copy, Debug, Default)]
pub struct ImmutableKey<T: Mutable>(pub T);

impl<T: Mutable> PartialEq for ImmutableKey<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.immutable_eq(&other.0)
    }
}

impl<T: Mutable> Eq for ImmutableKey<T> {}

impl<T: Mutable> Hash for ImmutableKey<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.immutable_hash(state);
    }
}

/// A wrapper around a `Mutable` value such that the `Hash`/`Eq`
/// implementations are based on the mutable and immutable part of the value.
#[derive(Clone, Dupe, Copy, Debug, Default)]
pub struct FullKey<T: Mutable>(pub T);

impl<T: Mutable> PartialEq for FullKey<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0.immutable_eq(&other.0) && self.0.mutable_eq(&other.0)
    }
}

impl<T: Mutable> Eq for FullKey<T> {}

impl<T: Mutable> Hash for FullKey<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.immutable_hash(state);
        self.0.mutable_hash(state);
    }
}
