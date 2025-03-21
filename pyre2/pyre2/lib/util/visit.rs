/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Traits for visiting contained data types. See [`Visit`] (the immutable version)
//! for full documentation, where [`VisitMut`] works similarly.

use std::any;
use std::any::Any;

use const_str;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use vec1::Vec1;

use crate::module::module_name::ModuleName;
use crate::util::uniques::Unique;

/// Visitors based on <https://ndmitchell.com/#uniplate_30_sep_2007>.
///
/// Given a from type (`Self`) you want to visit all the `To` values contained within it.
/// It does not visit any of the `To` values contained within those `To` values
/// (i.e. it is not recursive), but you can build a recursive traversal using `recurse`.
///
/// The `recurse` and `visit` methods do the same thing, unless `To == Self`, in which case
/// `recurse` will ignore the outer element, while `visit` will call the argument function
/// with `self`.
///
/// The `RECURSE_CONTAINS` and `VISIT_CONTAINS` are `false` if there is a guarantee that when
/// called they will _never_ call the argument function E.g. they are a no-op.
/// Can be used for optimisation.
///
/// When making the choice of using either `recurse` or `visit`:
///
/// * If you are defining an instance, define `recurse` (and potentially `RECURSE_CONTAINS`)
///   and `visit` will be inferred automatically.
/// * If you are starting at a type and want to operate on values within it, use `visit`.
/// * If you are in a recursive traversal, e.g. a argument to `visit`, use `recurse`.
///
/// For example, given a `enum Expr {Constant(i32), Negate(Box<Expr>) ...}` you could write
/// a traversal to collect all the constants with:
///
/// ```ignore
/// fn f(x: &Expr, res: &mut Vec<i32>) {
///     if let Expr::Constant(x) = x {
///         res.push(*x)
///     }
///     x.recurse(|x| f(x, res));
/// }
/// let mut res = Vec::new();
/// value.visit(|x| f(x, &mut res));
/// ```
///
/// Here we make the first call using `visit`, which will work regardless of whether
/// we are at an `Expr`, or a `Vec<Expr>`. But within the recursive call, we use `recurse`.
///
/// * If you use `recurse` instead of `visit`, you will miss the outer element if
///   `value = Expr::Constant(1)`.
/// * If you use `visit` instead of `recurse`, you will infinite loop and stack overflow.
pub trait Visit<To: 'static = Self>: 'static + Sized {
    /// Is `recurse` a no-op.
    const RECURSE_CONTAINS: bool = true;

    /// Is `visit` a no-op.
    const VISIT_CONTAINS: bool = Self::RECURSE_CONTAINS || type_eq::<To, Self>();

    /// Should call the function on all the `To` children of `Self`.
    ///
    /// Note the lifetime guarantee that every element will be contained in the original structure.
    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To));

    /// Like `visit`, but if `To == Self` then calls the function directly.
    fn visit<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        if Self::VISIT_CONTAINS
            && let Some(to) = (self as &dyn Any).downcast_ref::<To>()
        {
            f(to);
        } else if Self::RECURSE_CONTAINS {
            self.recurse(f)
        }
    }
}

/// Like `Visit`, but mutably.
pub trait VisitMut<To: 'static = Self>: 'static + Sized {
    const RECURSE_CONTAINS: bool = true;
    const VISIT_CONTAINS: bool = Self::RECURSE_CONTAINS || type_eq::<To, Self>();

    /// In contrast to `visit`, we don't have a guarantee that the results will be in
    /// the original structure. This decision is pragmatic - it's rare to mutate _and_
    /// store the values (since mutating probably means you can't capture them).
    /// Lacking the lifetimes means we can have an `Arc` implement `VisitMut` by doing
    /// a `clone()` first.
    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To));

    fn visit_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        if let Some(to) = (self as &mut dyn Any).downcast_mut::<To>() {
            f(to);
        } else {
            self.recurse_mut(f)
        }
    }
}

/// Type-level equality by types.
/// Seems to work, as we test it, but this is not ideal.
/// We'd really like to compare the `TypeId`, but that isn't `const` yet.
const fn type_eq<T1, T2>() -> bool {
    const_str::equal!(any::type_name::<T1>(), any::type_name::<T2>())
}

macro_rules! visit_nothing {
    ($t:ty) => {
        impl<To: 'static> Visit<To> for $t {
            const RECURSE_CONTAINS: bool = false;
            fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a To)) {}
        }

        impl<To: 'static> VisitMut<To> for $t {
            const RECURSE_CONTAINS: bool = false;
            fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut To)) {}
        }
    };
}

visit_nothing!(bool);
visit_nothing!(u8);
visit_nothing!(u16);
visit_nothing!(u32);
visit_nothing!(u64);
visit_nothing!(u128);
visit_nothing!(usize);
visit_nothing!(i8);
visit_nothing!(i16);
visit_nothing!(i32);
visit_nothing!(i64);
visit_nothing!(i128);
visit_nothing!(isize);
visit_nothing!(());

// We can't visit `str` on its own, so this is atomic.
visit_nothing!(Box<str>);

// Pyrefly types that have nothing inside
visit_nothing!(Name);
visit_nothing!(Unique);
visit_nothing!(ModuleName);
visit_nothing!(TextRange);

impl<To: 'static, T: Visit<To>> Visit<To> for Vec<T> {
    const RECURSE_CONTAINS: bool = <T as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        for item in self {
            item.visit(f);
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Vec<T> {
    const RECURSE_CONTAINS: bool = <T as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        for item in self {
            item.visit_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Vec1<T> {
    const RECURSE_CONTAINS: bool = <T as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        for item in self {
            item.visit(f);
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Vec1<T> {
    const RECURSE_CONTAINS: bool = <T as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        for item in self {
            item.visit_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Box<[T]> {
    const RECURSE_CONTAINS: bool = <T as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        for item in self {
            item.visit(f);
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Box<[T]> {
    const RECURSE_CONTAINS: bool = <T as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        for item in self {
            item.visit_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Option<T> {
    const RECURSE_CONTAINS: bool = <T as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        if let Some(item) = self {
            item.visit(f)
        }
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Option<T> {
    const RECURSE_CONTAINS: bool = <T as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        if let Some(item) = self {
            item.visit_mut(f);
        }
    }
}

impl<To: 'static, T: Visit<To>> Visit<To> for Box<T> {
    const RECURSE_CONTAINS: bool = <T as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        (**self).visit(f)
    }
}

impl<To: 'static, T: VisitMut<To>> VisitMut<To> for Box<T> {
    const RECURSE_CONTAINS: bool = <T as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        (**self).visit_mut(f)
    }
}

impl<To: 'static, T0: Visit<To>, T1: Visit<To>> Visit<To> for (T0, T1) {
    const RECURSE_CONTAINS: bool =
        <T0 as Visit<To>>::VISIT_CONTAINS || <T1 as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        self.0.visit(f);
        self.1.visit(f);
    }
}

impl<To: 'static, T0: VisitMut<To>, T1: VisitMut<To>> VisitMut<To> for (T0, T1) {
    const RECURSE_CONTAINS: bool =
        <T0 as VisitMut<To>>::VISIT_CONTAINS || <T1 as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        self.0.visit_mut(f);
        self.1.visit_mut(f);
    }
}

impl<To: 'static, T0: Visit<To>, T1: Visit<To>, T2: Visit<To>> Visit<To> for (T0, T1, T2) {
    const RECURSE_CONTAINS: bool = <T0 as Visit<To>>::VISIT_CONTAINS
        || <T1 as Visit<To>>::VISIT_CONTAINS
        || <T2 as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        self.0.visit(f);
        self.1.visit(f);
        self.2.visit(f);
    }
}

impl<To: 'static, T0: VisitMut<To>, T1: VisitMut<To>, T2: VisitMut<To>> VisitMut<To>
    for (T0, T1, T2)
{
    const RECURSE_CONTAINS: bool = <T0 as VisitMut<To>>::VISIT_CONTAINS
        || <T1 as VisitMut<To>>::VISIT_CONTAINS
        || <T2 as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        self.0.visit_mut(f);
        self.1.visit_mut(f);
        self.2.visit_mut(f);
    }
}

impl<To: 'static, T0: Visit<To>, T1: Visit<To>, T2: Visit<To>, T3: Visit<To>> Visit<To>
    for (T0, T1, T2, T3)
{
    const RECURSE_CONTAINS: bool = <T0 as Visit<To>>::VISIT_CONTAINS
        || <T1 as Visit<To>>::VISIT_CONTAINS
        || <T2 as Visit<To>>::VISIT_CONTAINS
        || <T3 as Visit<To>>::VISIT_CONTAINS;

    fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a To)) {
        self.0.visit(f);
        self.1.visit(f);
        self.2.visit(f);
        self.3.visit(f);
    }
}

impl<To: 'static, T0: VisitMut<To>, T1: VisitMut<To>, T2: VisitMut<To>, T3: VisitMut<To>>
    VisitMut<To> for (T0, T1, T2, T3)
{
    const RECURSE_CONTAINS: bool = <T0 as VisitMut<To>>::VISIT_CONTAINS
        || <T1 as VisitMut<To>>::VISIT_CONTAINS
        || <T2 as VisitMut<To>>::VISIT_CONTAINS
        || <T3 as VisitMut<To>>::VISIT_CONTAINS;

    fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut To)) {
        self.0.visit_mut(f);
        self.1.visit_mut(f);
        self.2.visit_mut(f);
        self.3.visit_mut(f);
    }
}

#[cfg(test)]
mod tests {
    use pyrefly_derive::Visit;
    use pyrefly_derive::VisitMut;
    use static_assertions::const_assert;

    use super::*;

    #[test]
    fn test_visit() {
        let mut info = (vec![1, 2, 3], Some(4i32), vec![Some(5i32)]);
        let mut collect = Vec::new();
        info.recurse(&mut |x: &i32| collect.push(*x));
        assert_eq!(&collect, &[1i32, 2, 3, 4, 5]);

        info.recurse_mut(&mut |x: &mut i32| *x *= 2);
        collect.clear();
        info.recurse(&mut |x: &i32| collect.push(*x));
        assert_eq!(&collect, &[2i32, 4, 6, 8, 10]);
    }

    #[test]
    fn test_visit_contains() {
        let xs = vec![1i32, 2, 3];
        let mut count = 0;
        xs.recurse(&mut |_: &i32| count += 1);
        assert_eq!(count, 3);
        count = 0;
        xs.recurse(&mut |_: &bool| count += 1);
        assert_eq!(count, 0);

        struct Foo;
        impl Visit<i32> for Foo {
            const RECURSE_CONTAINS: bool = false;
            fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a i32)) {
                unreachable!("Should not be reaching here")
            }
            fn visit<'a>(&'a self, _: &mut dyn FnMut(&'a i32)) {
                // Deliberately implement visit0 so the optimisation on this doesn't kick in,
                // only the optimisation on Vec itself.
                unreachable!("Should not be reaching here")
            }
        }

        const_assert!(!<Foo as Visit<i32>>::VISIT_CONTAINS);
        const_assert!(!type_eq::<Foo, i32>());
        const_assert!(!<Foo as Visit<i32>>::RECURSE_CONTAINS);
        const_assert!(!<Vec<Foo> as Visit<i32>>::RECURSE_CONTAINS);
        const_assert!(!<Vec<Foo> as Visit<i32>>::VISIT_CONTAINS);
        vec![Foo].visit(&mut |_: &i32| ());
    }

    #[derive(Visit, VisitMut, PartialEq, Eq, Debug)]
    struct Foo {
        x: i32,
        f: (Bar, Baz),
    }

    #[derive(Visit, VisitMut, PartialEq, Eq, Debug)]
    struct Bar(i32, i32);

    #[derive(Visit, VisitMut, PartialEq, Eq, Debug)]
    enum Baz {
        A,
        B(bool, bool),
        C { x: i32, y: i32 },
    }

    #[derive(Visit, VisitMut, PartialEq, Eq, Debug)]
    struct Generic<T>(T);

    #[test]
    fn test_visit_derive() {
        let mut info = (
            Foo {
                x: 1,
                f: (Bar(2, 3), Baz::B(true, false)),
            },
            Generic(Baz::A),
            Baz::C { x: 4, y: 5 },
        );
        let mut collect = Vec::new();
        info.visit(&mut |x: &i32| collect.push(*x));
        assert_eq!(&collect, &[1i32, 2, 3, 4, 5]);
        let mut collect = Vec::new();
        info.visit_mut(&mut |x: &mut bool| collect.push(*x));
        assert_eq!(&collect, &[true, false]);
        let mut collect = Vec::new();
        info.visit(&mut |x: &Bar| collect.push(x));
        assert_eq!(&collect, &[&Bar(2, 3)]);

        const_assert!(<Foo as Visit<i32>>::VISIT_CONTAINS);
        const_assert!(!<Foo as Visit<u8>>::VISIT_CONTAINS);
        const_assert!(<Generic<i32> as Visit<i32>>::VISIT_CONTAINS);
        const_assert!(!<Generic<i32> as Visit<u8>>::VISIT_CONTAINS);
    }

    #[test]
    fn test_visit_subset() {
        #[derive(PartialEq, Eq, Debug)]
        struct Foo(i32);

        impl Visit<i32> for Foo {
            fn recurse<'a>(&'a self, f: &mut dyn FnMut(&'a i32)) {
                f(&self.0);
            }
        }

        impl VisitMut<i32> for Foo {
            fn recurse_mut(&mut self, f: &mut dyn FnMut(&mut i32)) {
                f(&mut self.0);
            }
        }

        /// We derive Visit/VisitMut for `Foo`, but know it will only work for i32
        #[derive(Visit, VisitMut, PartialEq, Eq, Debug)]
        struct Bar(Foo);

        let mut info = Bar(Foo(1));
        info.visit_mut(&mut |x: &mut i32| *x += 2);
        let mut collect = Vec::new();
        info.visit(&mut |x: &i32| collect.push(*x));
        assert_eq!(&collect, &[3i32]);
    }
}
