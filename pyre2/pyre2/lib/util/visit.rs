/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::any::Any;

/// Visitors based on <https://ndmitchell.com/#uniplate_30_sep_2007>.
pub trait Visit<To: 'static = Self>: 'static + Sized {
    /// Should call the function on all the `To` children of `Self`.
    ///
    /// Note the lifetime guarantee that every element will be contained in the original structure.
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To));

    /// Like `visit`, but if `To == Self` then calls the function directly.
    fn visit0<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        if let Some(to) = (self as &dyn Any).downcast_ref::<To>() {
            f(to);
        } else {
            self.visit(f)
        }
    }
}

/// Like `Visit`, but mutably.
pub trait VisitMut<To: 'static = Self>: 'static + Sized {
    /// In contrast to `visit`, we don't have a guarantee that the results will be in
    /// the original structure. This decision is pragmatic - it's rare to mutate _and_
    /// store the values (since mutating probably means you can't capture them).
    /// Lacking the lifetimes means we can have an `Arc` implement `VisitMut` by doing
    /// a `clone()` first.
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To));

    fn visit0_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        if let Some(to) = (self as &mut dyn Any).downcast_mut::<To>() {
            f(to);
        } else {
            self.visit_mut(f)
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Vec<T> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        for item in self {
            item.visit0(f);
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Vec<T> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        for item in self {
            item.visit0_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Box<[T]> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        for item in self {
            item.visit0(f);
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Box<[T]> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        for item in self {
            item.visit0_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Option<T> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        if let Some(item) = self {
            item.visit0(f)
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Option<T> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        if let Some(item) = self {
            item.visit0_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Box<T> {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        (**self).visit0(f)
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Box<T> {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        (**self).visit0_mut(f)
    }
}
