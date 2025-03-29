/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;
use ruff_python_ast::name::Name;

use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::Substitution;
use crate::types::class::TArgs;
use crate::types::qname::QName;
use crate::types::types::Type;

#[derive(Clone, Debug, TypeEq, PartialEq, Eq, Hash)]
pub struct TypedDictField {
    pub ty: Type,
    pub required: bool,
    pub read_only: bool,
}

impl TypedDictField {
    pub fn substitute(self, substitution: &Substitution) -> Self {
        Self {
            ty: substitution.substitute(self.ty),
            required: self.required,
            read_only: self.read_only,
        }
    }
}

#[derive(
    Debug, Visit, VisitMut, PartialOrd, Ord, Clone, TypeEq, Eq, PartialEq, Hash
)]
pub struct TypedDict {
    class: Class,
    args: TArgs,
}

impl TypedDict {
    pub fn new(class: Class, args: TArgs) -> Self {
        Self { class, args }
    }

    pub fn qname(&self) -> &QName {
        self.class.qname()
    }

    pub fn name(&self) -> &Name {
        self.class.name()
    }

    pub fn class_object(&self) -> &Class {
        &self.class
    }

    pub fn targs(&self) -> &TArgs {
        &self.args
    }

    pub fn as_class_type(&self) -> ClassType {
        // TypedDict instances behave very differently from instances of other classes, so we don't
        // represent TypedDicts as ClassType in normal typechecking logic. However, the two do
        // share a bit of behavior, so we occasionally convert a TypedDict to a ClassType in order
        // to reuse code.
        ClassType::new(self.class.dupe(), self.args.clone())
    }
}
