/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#![allow(dead_code)] // Working on it

#[allow(unused_imports)]
use pyrefly_derive::TypeEq;

pub trait TypeEq: Eq {
    fn type_eq(&self, other: &Self) -> bool {
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

impl<T1: TypeEq, T2: TypeEq> TypeEq for (T1, T2) {
    fn type_eq(&self, other: &Self) -> bool {
        self.0.type_eq(&other.0) && self.1.type_eq(&other.1)
    }
}

#[cfg(test)]
mod tests {
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
        assert!(
            Foo {
                x: 1,
                f: (Bar(1, 2), Baz::A)
            }
            .type_eq(&Foo {
                x: 1,
                f: (Bar(1, 2), Baz::A)
            })
        );
        assert!(
            !Foo {
                x: 1,
                f: (Bar(1, 2), Baz::C { x: 1, y: 2 })
            }
            .type_eq(&Foo {
                x: 1,
                f: (Bar(1, 2), Baz::B(true, false))
            })
        );
        assert!(
            !Foo {
                x: 1,
                f: (Bar(1, 2), Baz::A)
            }
            .type_eq(&Foo {
                x: 1,
                f: (Bar(1, 3), Baz::A)
            })
        );
        assert!(Generic(1).type_eq(&Generic(1)));
        assert!(!Generic(1).type_eq(&Generic(2)));
    }
}
