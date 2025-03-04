/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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

/// Caching wrapper to create identifiers that are indexed by `ArcId`.
pub struct IdCache {}

impl IdCache {
    pub fn class(
        name: Identifier,
        module_info: ModuleInfo,
        tparams: TParams,
        fields: SmallMap<Name, ClassFieldProperties>,
    ) -> Class {
        Class::new_identity(name, module_info, tparams, fields)
    }

    pub fn param_spec(name: Identifier, module: ModuleInfo) -> ParamSpec {
        ParamSpec::new_identity(name, module)
    }

    pub fn type_var_tuple(name: Identifier, module: ModuleInfo) -> TypeVarTuple {
        TypeVarTuple::new_identity(name, module)
    }

    pub fn type_var(
        name: Identifier,
        module: ModuleInfo,
        restriction: Restriction,
        default: Option<Type>,
        variance: Option<Variance>,
    ) -> TypeVar {
        TypeVar::new_identity(name, module, restriction, default, variance)
    }
}
