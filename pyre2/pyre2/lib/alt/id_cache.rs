/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::mem;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use starlark_map::small_map::SmallMap;

use crate::module::module_info::ModuleInfo;
use crate::types::class::Class;
use crate::types::class::ClassFieldProperties;
use crate::types::param_spec::ParamSpec;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::lock::Mutex;

/// An identifiable thing held behind an ArcId.
/// For Eq/Hash, we only hash the immutable pieces.
#[derive(Debug, Dupe, Clone)]
enum Identifiable {
    Class(Class),
    ParamSpec(ParamSpec),
    TypeVar(TypeVar),
    TypeVarTuple(TypeVarTuple),
}

impl Identifiable {
    fn mutate(&self, x: &Self) {
        match (self, x) {
            (Self::Class(a), Self::Class(b)) => a.mutate(b),
            (Self::ParamSpec(a), Self::ParamSpec(b)) => a.mutate(b),
            (Self::TypeVar(a), Self::TypeVar(b)) => a.mutate(b),
            (Self::TypeVarTuple(a), Self::TypeVarTuple(b)) => a.mutate(b),
            _ => panic!("Expected same variant"),
        }
    }

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

impl PartialEq for Identifiable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Class(a), Self::Class(b)) => a.immutable_eq(b),
            (Self::ParamSpec(a), Self::ParamSpec(b)) => a.immutable_eq(b),
            (Self::TypeVar(a), Self::TypeVar(b)) => a.immutable_eq(b),
            (Self::TypeVarTuple(a), Self::TypeVarTuple(b)) => a.immutable_eq(b),
            _ => false,
        }
    }
}

impl Eq for Identifiable {}

impl Hash for Identifiable {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Class(a) => a.immutable_hash(state),
            Self::ParamSpec(a) => a.immutable_hash(state),
            Self::TypeVar(a) => a.immutable_hash(state),
            Self::TypeVarTuple(a) => a.immutable_hash(state),
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
pub struct IdCache {
    /// Things that were created last time and we can reuse.
    reusable: Mutex<HashMap<Identifiable, Vec<Identifiable>>>,
    /// Things we are creating.
    recorded: Mutex<Vec<Identifiable>>,
}

/// A history of the identifiers that were created, that can be reused.
#[derive(Debug, Clone, Default)]
pub struct IdCacheHistory(Vec<Identifiable>);

impl IdCache {
    pub fn new(history: IdCacheHistory) -> Self {
        #[allow(clippy::mutable_key_type)] // Our Eq/Hash deliberately excludes the mutable bits.
        let mut reusable: HashMap<Identifiable, Vec<Identifiable>> = HashMap::new();
        for x in history.0 {
            reusable.entry(x.dupe()).or_default().push(x);
        }
        Self {
            reusable: Mutex::new(reusable),
            recorded: Mutex::new(Vec::new()),
        }
    }

    pub fn history(&self) -> IdCacheHistory {
        IdCacheHistory(mem::take(&mut *self.recorded.lock()))
    }

    fn get(&self, mut x: Identifiable) -> Identifiable {
        let mut reusable = self.reusable.lock();
        match reusable.entry(x.dupe()) {
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
        self.recorded.lock().push(x.dupe());
        x
    }

    pub fn class(
        &self,
        name: Identifier,
        module_info: ModuleInfo,
        tparams: TParams,
        fields: SmallMap<Name, ClassFieldProperties>,
    ) -> Class {
        self.get(Identifiable::Class(Class::new_identity(
            name,
            module_info,
            tparams,
            fields,
        )))
        .unwrap_class()
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
