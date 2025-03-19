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

macro_rules! no_children {
    ($t:ty) => {
        impl<To: 'static> Visit<To> for $t {
            fn visit<'a>(&'a self, _: &mut dyn FnMut(&'a To)) {}
        }

        impl<To: 'static> VisitMut<To> for $t {
            fn visit_mut(&mut self, _: &mut dyn FnMut(&mut To)) {}
        }
    };
}

no_children!(bool);
no_children!(u8);
no_children!(u16);
no_children!(u32);
no_children!(u64);
no_children!(u128);
no_children!(usize);
no_children!(i8);
no_children!(i16);
no_children!(i32);
no_children!(i64);
no_children!(i128);
no_children!(isize);
no_children!(());

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

impl<To: 'static, T0: Visit<To>, T1: Visit<To>> Visit<To> for (T0, T1) {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        self.0.visit0(f);
        self.1.visit0(f);
    }
}

impl<To: 'static, T0: VisitMut<To>, T1: VisitMut<To>> VisitMut<To> for (T0, T1) {
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        self.0.visit0_mut(f);
        self.1.visit0_mut(f);
    }
}

impl<To: 'static, T0: Visit<To>, T1: Visit<To>, T2: Visit<To>> Visit<To> for (T0, T1, T2) {
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        self.0.visit0(f);
        self.1.visit0(f);
        self.2.visit0(f);
    }
}

impl<To: 'static, T0: VisitMut<To>, T1: VisitMut<To>, T2: VisitMut<To>> VisitMut<To>
    for (T0, T1, T2)
{
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        self.0.visit0_mut(f);
        self.1.visit0_mut(f);
        self.2.visit0_mut(f);
    }
}

impl<To: 'static, T0: Visit<To>, T1: Visit<To>, T2: Visit<To>, T3: Visit<To>> Visit<To>
    for (T0, T1, T2, T3)
{
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        self.0.visit0(f);
        self.1.visit0(f);
        self.2.visit0(f);
        self.3.visit0(f);
    }
}

impl<To: 'static, T0: VisitMut<To>, T1: VisitMut<To>, T2: VisitMut<To>, T3: VisitMut<To>>
    VisitMut<To> for (T0, T1, T2, T3)
{
    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        self.0.visit0_mut(f);
        self.1.visit0_mut(f);
        self.2.visit0_mut(f);
        self.3.visit0_mut(f);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_visit() {
        let mut info = (vec![1, 2, 3], Some(4i32), vec![Some(5i32)]);
        let mut collect = Vec::new();
        info.visit(&mut |x: &i32| collect.push(*x));
        assert_eq!(&collect, &[1i32, 2, 3, 4, 5]);

        info.visit_mut(&mut |x: &mut i32| *x *= 2);
        collect.clear();
        info.visit(&mut |x: &i32| collect.push(*x));
        assert_eq!(&collect, &[2i32, 4, 6, 8, 10]);
    }
}
