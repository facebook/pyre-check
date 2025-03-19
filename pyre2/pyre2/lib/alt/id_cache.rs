/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hasher;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use starlark_map::small_map::SmallMap;

use crate::module::module_info::ModuleInfo;
use crate::types::class::Class;
use crate::types::class::ClassFieldProperties;
use crate::types::class::ClassIndex;
use crate::types::param_spec::ParamSpec;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::lock::Mutex;
use crate::util::mutable::FullKey;
use crate::util::mutable::ImmutableKey;
use crate::util::mutable::Mutable;

/// An identifiable thing held behind an ArcId.
/// For Eq/Hash, we only hash the immutable pieces.
#[derive(Debug, Dupe, Clone)]
enum Identifiable {
    #[expect(dead_code)]
    Class(Class),
    ParamSpec(ParamSpec),
    TypeVar(TypeVar),
    TypeVarTuple(TypeVarTuple),
}

impl Mutable for Identifiable {
    fn immutable_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Class(a), Self::Class(b)) => a.immutable_eq(b),
            (Self::ParamSpec(a), Self::ParamSpec(b)) => a.immutable_eq(b),
            (Self::TypeVar(a), Self::TypeVar(b)) => a.immutable_eq(b),
            (Self::TypeVarTuple(a), Self::TypeVarTuple(b)) => a.immutable_eq(b),
            _ => false,
        }
    }

    fn immutable_hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Class(a) => a.immutable_hash(state),
            Self::ParamSpec(a) => a.immutable_hash(state),
            Self::TypeVar(a) => a.immutable_hash(state),
            Self::TypeVarTuple(a) => a.immutable_hash(state),
        }
    }

    fn mutable_eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Class(a), Self::Class(b)) => a.mutable_eq(b),
            (Self::ParamSpec(a), Self::ParamSpec(b)) => a.mutable_eq(b),
            (Self::TypeVar(a), Self::TypeVar(b)) => a.mutable_eq(b),
            (Self::TypeVarTuple(a), Self::TypeVarTuple(b)) => a.mutable_eq(b),
            _ => false,
        }
    }

    fn mutable_hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Class(a) => a.mutable_hash(state),
            Self::ParamSpec(a) => a.mutable_hash(state),
            Self::TypeVar(a) => a.mutable_hash(state),
            Self::TypeVarTuple(a) => a.mutable_hash(state),
        }
    }

    fn mutate(&self, x: &Self) {
        match (self, x) {
            (Self::Class(a), Self::Class(b)) => a.mutate(b),
            (Self::ParamSpec(a), Self::ParamSpec(b)) => a.mutate(b),
            (Self::TypeVar(a), Self::TypeVar(b)) => a.mutate(b),
            (Self::TypeVarTuple(a), Self::TypeVarTuple(b)) => a.mutate(b),
            _ => panic!("Expected same variant"),
        }
    }
}

impl Identifiable {
    #[expect(dead_code)]
    fn unwrap_class(self) -> Class {
        match self {
            Self::Class(x) => x,
            _ => panic!("Expected Class"),
        }
    }

    fn unwrap_param_spec(self) -> ParamSpec {
        match self {
            Self::ParamSpec(x) => x,
            _ => panic!("Expected ParamSpec"),
        }
    }

    fn unwrap_type_var(self) -> TypeVar {
        match self {
            Self::TypeVar(x) => x,
            _ => panic!("Expected TypeVar"),
        }
    }

    fn unwrap_type_var_tuple(self) -> TypeVarTuple {
        match self {
            Self::TypeVarTuple(x) => x,
            _ => panic!("Expected TypeVarTuple"),
        }
    }
}

/// Caching wrapper to create identifiers that are indexed by `ArcId`.
///
/// The idea is we have a list of previously used values, and we can reuse them.
/// To reuse a thing it must match on all the fields that are immutable, and
/// then we mutate all the fields that are mutable to perfectly match our desired
/// value.
///
/// We use `immutable_hash`/`immutable_eq` to determine if we can reuse a thing.
/// We use `mutate` to mutate the mutable fields to match our desired value.
#[derive(Debug)]
pub struct IdCache(Mutex<IdCacheInner>);

#[derive(Debug)]
struct IdCacheInner {
    /// Things that were created last time and we can reuse.
    reusable: HashMap<ImmutableKey<Identifiable>, Vec<Identifiable>>,
    /// Things we have created afresh this time, or taken from the reusable list.
    /// Note that due to races on Calculation, we might end up trying to create
    /// the same `Identifiable` twice, and in order to avoid invalidating the
    /// interface, we need to reuse perfectly identical things we have created.
    created: HashSet<FullKey<Identifiable>>,
}

/// A history of the identifiers that were created, that can be reused.
#[derive(Debug, Clone, Default)]
pub struct IdCacheHistory(Vec<Identifiable>);

impl IdCache {
    #[allow(clippy::mutable_key_type)] // Our Eq/Hash deliberately excludes the mutable bits.
    pub fn new(history: IdCacheHistory) -> Self {
        let mut reusable: HashMap<ImmutableKey<Identifiable>, Vec<Identifiable>> = HashMap::new();
        for x in history.0 {
            reusable.entry(ImmutableKey(x.dupe())).or_default().push(x);
        }
        let created = HashSet::with_capacity(reusable.len());
        Self(Mutex::new(IdCacheInner { reusable, created }))
    }

    pub fn history(&self) -> IdCacheHistory {
        let lock = self.0.lock();
        IdCacheHistory(lock.created.iter().map(|x| x.0.dupe()).collect())
    }

    fn get(&self, mut x: Identifiable) -> Identifiable {
        if true {
            return x;
        }

        let mut lock = self.0.lock();

        // First check if we have already created this thing this time around.
        if let Some(res) = lock.created.get(&FullKey(x.dupe())) {
            return res.0.dupe();
        }

        match lock.reusable.entry(ImmutableKey(x.dupe())) {
            Entry::Occupied(mut e) => {
                if let Some(existing) = e.get_mut().pop() {
                    if e.get().is_empty() {
                        e.remove();
                    }
                    existing.mutate(&x);
                    x = existing;
                }
            }
            Entry::Vacant(_) => {}
        };
        lock.created.insert(FullKey(x.dupe()));
        x
    }

    pub fn class(
        &self,
        index: ClassIndex,
        name: Identifier,
        module_info: ModuleInfo,
        tparams: TParams,
        fields: SmallMap<Name, ClassFieldProperties>,
    ) -> Class {
        Class::new_identity(index, name, module_info, tparams, fields)
    }

    pub fn param_spec(&self, name: Identifier, module: ModuleInfo) -> ParamSpec {
        self.get(Identifiable::ParamSpec(ParamSpec::new_identity(
            name, module,
        )))
        .unwrap_param_spec()
    }

    pub fn type_var_tuple(&self, name: Identifier, module: ModuleInfo) -> TypeVarTuple {
        self.get(Identifiable::TypeVarTuple(TypeVarTuple::new_identity(
            name, module,
        )))
        .unwrap_type_var_tuple()
    }

    pub fn type_var(
        &self,
        name: Identifier,
        module: ModuleInfo,
        restriction: Restriction,
        default: Option<Type>,
        variance: Option<Variance>,
    ) -> TypeVar {
        self.get(Identifiable::TypeVar(TypeVar::new_identity(
            name,
            module,
            restriction,
            default,
            variance,
        )))
        .unwrap_type_var()
    }
}
