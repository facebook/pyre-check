/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/// Visitors based on <https://ndmitchell.com/#uniplate_30_sep_2007>.
///
/// Should call the function on all immediate `To` children of `Self`.
/// As a special case, if `Self == To` then it should descend one layer.
pub trait Visit<To: ?Sized = Self> {
    /// Note the guarantee that every element will be contained in the original structure.
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To));
}

/// Like `Visit`, but mutably.
pub trait VisitMut<To: ?Sized = Self> {
    /// In contrast to `visit`, we don't have a guarantee that the results will be in
    /// the original structure. This decision is pragmatic - it's rare to mutate _and_
    /// store the values (since mutating probably means you can't capture them).
    /// Lacking the lifetimes means we can have an `Arc` implement `VisitMut` by doing
    /// a `clone()` first.
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To));
}

// While it is possible to implement the more general `impl<To, T: Visit<To>> Visit<To> for Vec<T>`,
// we can't tell if To == T or not (no stable type equality in Rust), and thus might miss T.
impl<T> Visit<T> for Vec<T> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a T)) {
        for item in self {
            f(item)
        }
    }
}

impl<T> Visit<T> for [T] {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a T)) {
        for item in self {
            f(item)
        }
    }
}

impl<T> Visit<T> for Box<[T]> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a T)) {
        for item in self {
            f(item)
        }
    }
}

impl<T> Visit<T> for Option<T> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a T)) {
        if let Some(item) = self {
            f(item)
        }
    }
}

impl<T> Visit<T> for Option<Box<T>> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a T)) {
        if let Some(item) = self {
            f(item)
        }
    }
}

impl<T> VisitMut<T> for Vec<T> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut T)) {
        for item in self {
            f(item)
        }
    }
}

impl<T> VisitMut<T> for Option<T> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut T)) {
        if let Some(item) = self {
            f(item)
        }
    }
}

impl<T> VisitMut<T> for Box<[T]> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut T)) {
        for item in self {
            f(item)
        }
    }
}

impl<T> VisitMut<T> for Option<Box<T>> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut T)) {
        if let Some(item) = self {
            f(item)
        }
    }
}
