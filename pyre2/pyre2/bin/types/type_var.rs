/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt::Display;
use std::fmt::{self};

use dupe::Dupe;
use ruff_python_ast::Identifier;

use crate::module::module_info::ModuleInfo;
use crate::types::qname::QName;
use crate::types::types::Type;
use crate::util::arc_id::ArcId;

/// Used to represent TypeVar calls. Each TypeVar is unique, so use the ArcId to separate them.
#[derive(Clone, Dupe, Debug, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct TypeVar(ArcId<TypeVarInner>);

impl Display for TypeVar {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.qname.name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum Restriction {
    Constraints(Vec<Type>),
    Bound(Type),
    Unrestricted,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub enum Variance {
    Covariant,
    Contravariant,
    Invariant,
    Inferred,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
struct TypeVarInner {
    qname: QName,
    restriction: Restriction,
    default: Option<Type>,
    variance: Variance,
}

impl TypeVar {
    pub fn new(
        name: Identifier,
        module: ModuleInfo,
        restriction: Restriction,
        default: Option<Type>,
        variance: Variance,
    ) -> Self {
        Self(ArcId::new(TypeVarInner {
            qname: QName::new(name, module),
            restriction,
            default,
            variance,
        }))
    }

    pub fn qname(&self) -> &QName {
        &self.0.qname
    }

    pub fn to_type(&self) -> Type {
        Type::TypeVar(self.dupe())
    }

    pub fn is_ctor(x: &Type) -> bool {
        matches!(
            x, Type::ClassDef(cls)
            if cls.name() == "TypeVar" && matches!(cls.module_info().name().as_str(), "typing" | "typing_extensions"))
    }
}
