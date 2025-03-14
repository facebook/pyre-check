/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::ordered_set::OrderedSet;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use vec1::Vec1;

use crate::module::module_name::ModuleName;
use crate::util::arc_id::ArcId;
use crate::util::uniques::Unique;

#[derive(Debug, Default)]
pub struct TypeEqCtx {}

pub trait TypeEq: Eq {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        let _ = ctx;
        self == other
    }
}

impl TypeEq for () {}
impl TypeEq for bool {}
impl TypeEq for char {}
impl TypeEq for i8 {}
impl TypeEq for i16 {}
impl TypeEq for i32 {}
impl TypeEq for i64 {}
impl TypeEq for i128 {}
impl TypeEq for isize {}
impl TypeEq for u8 {}
impl TypeEq for u16 {}
impl TypeEq for u32 {}
impl TypeEq for u64 {}
impl TypeEq for u128 {}
impl TypeEq for usize {}
impl TypeEq for String {}
impl TypeEq for str {}

impl TypeEq for Name {}
impl TypeEq for ModuleName {}
impl TypeEq for TextRange {}

impl TypeEq for Unique {}

// We don't need to recursively call type_eq since we are doing
// pointer equality. So don't see whatever is inside.
impl<T> TypeEq for ArcId<T> {}

impl<T1: TypeEq, T2: TypeEq> TypeEq for (T1, T2) {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.0.type_eq(&other.0, ctx) && self.1.type_eq(&other.1, ctx)
    }
}

impl<T1: TypeEq, T2: TypeEq, T3: TypeEq> TypeEq for (T1, T2, T3) {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.0.type_eq(&other.0, ctx)
            && self.1.type_eq(&other.1, ctx)
            && self.2.type_eq(&other.2, ctx)
    }
}

impl<T: TypeEq> TypeEq for Vec<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.as_slice().type_eq(other.as_slice(), ctx)
    }
}

impl<T: TypeEq> TypeEq for Vec1<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.as_slice().type_eq(other.as_slice(), ctx)
    }
}

impl<T: TypeEq> TypeEq for [T] {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.type_eq(b, ctx))
    }
}

impl<T: TypeEq + ?Sized> TypeEq for &T {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        (*self).type_eq(*other, ctx)
    }
}

impl<T: TypeEq + ?Sized> TypeEq for Box<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.as_ref().type_eq(other.as_ref(), ctx)
    }
}

impl<T: TypeEq + ?Sized> TypeEq for Arc<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.as_ref().type_eq(other.as_ref(), ctx)
    }
}

impl<T: TypeEq> TypeEq for Option<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        match (self, other) {
            (Some(a), Some(b)) => a.type_eq(b, ctx),
            (None, None) => true,
            _ => false,
        }
    }
}

impl<K: TypeEq, V: TypeEq> TypeEq for OrderedMap<K, V> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.type_eq(&b, ctx))
    }
}

impl<T: TypeEq> TypeEq for OrderedSet<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.type_eq(b, ctx))
    }
}

impl<K: TypeEq, V: TypeEq> TypeEq for SmallMap<K, V> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.type_eq(&b, ctx))
    }
}

impl<T: TypeEq> TypeEq for SmallSet<T> {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.len() == other.len()
            && self
                .iter()
                .zip(other.iter())
                .all(|(a, b)| a.type_eq(b, ctx))
    }
}

#[cfg(test)]
mod tests {
    use pyrefly_derive::TypeEq;

    use super::*;

    #[derive(TypeEq, PartialEq, Eq, Debug)]
    struct Foo {
        x: i32,
        f: (Bar, Baz),
    }

    #[derive(TypeEq, PartialEq, Eq, Debug)]
    struct Bar(i32, i32);

    #[derive(TypeEq, PartialEq, Eq, Debug)]
    enum Baz {
        A,
        B(bool, bool),
        C { x: i32, y: i32 },
    }

    #[derive(TypeEq, PartialEq, Eq, Debug)]
    struct Generic<T>(T);

    #[test]
    fn test_type_eq() {
        let mut ctx = TypeEqCtx::default();
        assert!(
            Foo {
                x: 1,
                f: (Bar(1, 2), Baz::A)
            }
            .type_eq(
                &Foo {
                    x: 1,
                    f: (Bar(1, 2), Baz::A)
                },
                &mut ctx
            )
        );
        assert!(
            !Foo {
                x: 1,
                f: (Bar(1, 2), Baz::C { x: 1, y: 2 })
            }
            .type_eq(
                &Foo {
                    x: 1,
                    f: (Bar(1, 2), Baz::B(true, false))
                },
                &mut ctx
            )
        );
        assert!(
            !Foo {
                x: 1,
                f: (Bar(1, 2), Baz::A)
            }
            .type_eq(
                &Foo {
                    x: 1,
                    f: (Bar(1, 3), Baz::A)
                },
                &mut ctx
            )
        );
        assert!(Generic(1).type_eq(&Generic(1), &mut ctx));
        assert!(!Generic(1).type_eq(&Generic(2), &mut ctx));
    }
}
