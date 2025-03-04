/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

#[expect(dead_code)]
#[derive(Debug, Dupe, Clone)]
enum Identifiable {
    Class(Class),
    ParamSpec(ParamSpec),
    TypeVar(TypeVar),
    TypeVarTuple(TypeVarTuple),
}

/// Caching wrapper to create identifiers that are indexed by `ArcId`.
#[derive(Debug)]
pub struct IdCache {
    /// Things we are creating.
    recorded: Mutex<Vec<Identifiable>>,
}

/// A history of the identifiers that were created, that can be reused.
#[derive(Debug, Clone, Default)]
#[expect(dead_code)]
pub struct IdCacheHistory(Vec<Identifiable>);

impl IdCache {
    pub fn new(history: IdCacheHistory) -> Self {
        drop(history);
        Self {
            recorded: Mutex::new(Vec::new()),
        }
    }

    pub fn history(&self) -> IdCacheHistory {
        IdCacheHistory(mem::take(&mut *self.recorded.lock()))
    }

    pub fn class(
        &self,
        name: Identifier,
        module_info: ModuleInfo,
        tparams: TParams,
        fields: SmallMap<Name, ClassFieldProperties>,
    ) -> Class {
        let res = Class::new_identity(name, module_info, tparams, fields);
        self.recorded.lock().push(Identifiable::Class(res.dupe()));
        res
    }

    pub fn param_spec(&self, name: Identifier, module: ModuleInfo) -> ParamSpec {
        let res = ParamSpec::new_identity(name, module);
        self.recorded
            .lock()
            .push(Identifiable::ParamSpec(res.dupe()));
        res
    }

    pub fn type_var_tuple(&self, name: Identifier, module: ModuleInfo) -> TypeVarTuple {
        let res = TypeVarTuple::new_identity(name, module);
        self.recorded
            .lock()
            .push(Identifiable::TypeVarTuple(res.dupe()));
        res
    }

    pub fn type_var(
        &self,
        name: Identifier,
        module: ModuleInfo,
        restriction: Restriction,
        default: Option<Type>,
        variance: Option<Variance>,
    ) -> TypeVar {
        let res = TypeVar::new_identity(name, module, restriction, default, variance);
        self.recorded.lock().push(Identifiable::TypeVar(res.dupe()));
        res
    }
}
