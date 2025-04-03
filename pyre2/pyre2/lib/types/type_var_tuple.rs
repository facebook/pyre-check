/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use dupe::Dupe;
use pyrefly_derive::TypeEq;
use ruff_python_ast::Identifier;

use crate::module::module_info::ModuleInfo;
use crate::types::equality::TypeEq;
use crate::types::equality::TypeEqCtx;
use crate::types::qname::QName;
use crate::types::types::Type;
use crate::util::arc_id::ArcId;
use crate::util::visit::Visit;
use crate::util::visit::VisitMut;

/// Used to represent TypeVarTuple calls. Each TypeVarTuple is unique, so use the ArcId to separate them.
#[derive(Clone, Dupe, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TypeVarTuple(ArcId<TypeVarTupleInner>);

impl Visit<Type> for TypeVarTuple {
    const RECURSE_CONTAINS: bool = false;
    fn recurse<'a>(&'a self, _: &mut dyn FnMut(&'a Type)) {}
}

impl VisitMut<Type> for TypeVarTuple {
    const RECURSE_CONTAINS: bool = false;
    fn recurse_mut(&mut self, _: &mut dyn FnMut(&mut Type)) {}
}

impl Display for TypeVarTuple {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.qname.id())
    }
}

#[derive(Debug, PartialEq, TypeEq, Eq, Ord, PartialOrd)]
struct TypeVarTupleInner {
    qname: QName,
    default: Option<Type>,
}

impl TypeVarTuple {
    pub fn new(name: Identifier, module: ModuleInfo, default: Option<Type>) -> Self {
        Self(ArcId::new(TypeVarTupleInner {
            qname: QName::new(name, module),
            default,
        }))
    }

    pub fn qname(&self) -> &QName {
        &self.0.qname
    }

    pub fn default(&self) -> Option<&Type> {
        self.0.default.as_ref()
    }

    pub fn to_type(&self) -> Type {
        Type::TypeVarTuple(self.dupe())
    }

    pub fn type_eq_inner(&self, other: &Self, ctx: &mut TypeEqCtx) -> bool {
        self.0.type_eq(&other.0, ctx)
    }
}
