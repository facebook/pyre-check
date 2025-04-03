/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::hash::Hash;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::ordered_set::OrderedSet;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::Hashed;
use vec1::Vec1;

use crate::module::module_name::ModuleName;
use crate::types::param_spec::ParamSpec;
use crate::types::type_var::TypeVar;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::util::uniques::Unique;

/// Compare a set of types using the same context.
/// This will enable unique's to match, so important to use a single context
/// for all type comparisons.
#[derive(Debug, Default)]
pub struct TypeEqCtx {
    /// These Var's on the LHS are equal to those on the RHS
    unique: SmallMap<Unique, Unique>,
    /// These are Arc values we have previously declared to be equal.
    /// Important we cache them as otherwise on an enum with N field,
    /// we have N class equalities, and the cost of a class equality includes
    /// comparing the field names stored inside the class. That makes it O(N^2).
    arcs: SmallSet<(*const (), *const ())>,
    // Things that have identity
    param_spec: SmallMap<ParamSpec, ParamSpec>,
    type_var: SmallMap<TypeVar, TypeVar>,
    type_var_tuple: SmallMap<TypeVarTuple, TypeVarTuple>,
}

impl TypeEq for Unique {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        match ctx.unique.entry(*self) {
            Entry::Occupied(e) => e.get() == other,
            Entry::Vacant(e) => {
                e.insert(*other);
                true
            }
        }
    }
}

fn type_eq_identity<T>(
    x: &T,
    y: &T,
    ctx: &mut TypeEqCtx,
    map: impl Fn(&mut TypeEqCtx) -> &mut SmallMap<T, T>,
    eq: impl FnOnce(&mut TypeEqCtx) -> bool,
) -> bool
where
    T: Dupe + Eq + Hash,
{
    if let Some(res) = map(ctx).get(x) {
        return res == y;
    }
    if !eq(ctx) {
        return false;
    }
    map(ctx).insert(x.dupe(), y.dupe());
    true
}

impl TypeEq for ParamSpec {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        type_eq_identity(
            self,
            other,
            ctx,
            |ctx| &mut ctx.param_spec,
            |ctx| self.type_eq_inner(other, ctx),
        )
    }
}

impl TypeEq for TypeVar {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        type_eq_identity(
            self,
            other,
            ctx,
            |ctx| &mut ctx.type_var,
            |ctx| self.type_eq_inner(other, ctx),
        )
    }
}

impl TypeEq for TypeVarTuple {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        type_eq_identity(
            self,
            other,
            ctx,
            |ctx| &mut ctx.type_var_tuple,
            |ctx| self.type_eq_inner(other, ctx),
        )
    }
}

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

impl<T0: TypeEq, T1: TypeEq> TypeEq for (T0, T1) {
    fn type_eq(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.0.type_eq(&other.0, ctx) && self.1.type_eq(&other.1, ctx)
    }
}

impl<T0: TypeEq, T1: TypeEq, T2: TypeEq> TypeEq for (T0, T1, T2) {
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
        if Arc::ptr_eq(self, other) {
            return true;
        }
        let key = Hashed::new((
            Arc::as_ptr(self) as *const (),
            Arc::as_ptr(other) as *const (),
        ));
        if ctx.arcs.contains_hashed(key.as_ref()) {
            return true;
        }
        let res = self.as_ref().type_eq(other.as_ref(), ctx);
        if res {
            ctx.arcs.insert_hashed(key);
        }
        res
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
    use crate::types::callable::Callable;
    use crate::types::callable::FuncFlags;
    use crate::types::callable::FuncMetadata;
    use crate::types::callable::Function;
    use crate::types::callable::FunctionKind;
    use crate::types::callable::ParamList;
    use crate::types::quantified::Quantified;
    use crate::types::quantified::QuantifiedInfo;
    use crate::types::quantified::QuantifiedKind;
    use crate::types::type_var::Restriction;
    use crate::types::types::Forallable;
    use crate::types::types::TParamInfo;
    use crate::types::types::TParams;
    use crate::types::types::Type;
    use crate::util::uniques::UniqueFactory;

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

    #[test]
    fn test_equal_forall() {
        let uniques = UniqueFactory::new();

        fn mk_function(uniques: &UniqueFactory) -> Type {
            let q = Quantified::new(
                uniques.fresh(),
                QuantifiedInfo {
                    name: Name::new_static("test"),
                    kind: QuantifiedKind::TypeVar,
                    restriction: Restriction::Unrestricted,
                    default: None,
                },
            );
            Forallable::Function(Function {
                signature: Callable::list(ParamList::everything(), q.clone().to_type()),
                metadata: FuncMetadata {
                    kind: FunctionKind::Overload,
                    flags: FuncFlags::default(),
                },
            })
            .forall(
                TParams::new(vec![TParamInfo {
                    quantified: q,
                    variance: None,
                }])
                .unwrap(),
            )
        }

        let a = mk_function(&uniques);
        let b = mk_function(&uniques);
        assert_eq!(a, a);
        assert_ne!(a, b);

        assert!(a.type_eq(&a, &mut TypeEqCtx::default()));
        assert!(a.type_eq(&b, &mut TypeEqCtx::default()));
    }
}
