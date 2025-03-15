/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::cmp::Ordering;

use dupe::Dupe;
use pyrefly_derive::TypeEq;
use ruff_python_ast::name::Name;
use starlark_map::ordered_map::OrderedMap;

use crate::types::callable::Required;
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

#[derive(Debug, Clone, TypeEq, Eq, PartialEq, Hash)]
pub struct TypedDict {
    class: Class,
    args: TArgs,
    fields: OrderedMap<Name, TypedDictField>,
}

impl PartialOrd for TypedDict {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TypedDict {
    fn cmp(&self, other: &Self) -> Ordering {
        // The class uniquely defines the `TypeDict`, everything else is just supporting.
        self.class.cmp(&other.class)
    }
}

impl TypedDict {
    pub fn new(class: Class, args: TArgs, fields: OrderedMap<Name, TypedDictField>) -> Self {
        Self {
            class,
            args,
            fields,
        }
    }

    pub fn qname(&self) -> &QName {
        self.class.qname()
    }

    pub fn name(&self) -> &Name {
        self.class.name()
    }

    pub fn fields(&self) -> &OrderedMap<Name, TypedDictField> {
        &self.fields
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

    pub fn visit<'a>(&'a self, mut f: &mut dyn FnMut(&'a Type)) {
        let Self {
            class: _,
            args,
            fields,
        } = self;
        args.visit(&mut f);
        fields.values().for_each(|x| f(&x.ty));
    }

    pub fn visit_mut(&mut self, mut f: &mut dyn FnMut(&mut Type)) {
        let Self {
            class: _,
            args,
            fields,
        } = self;
        args.visit_mut(&mut f);
        fields.values_mut().for_each(|x| f(&mut x.ty));
    }

    pub fn kw_param_info(&self) -> Vec<(Name, Type, Required)> {
        self.fields()
            .iter()
            .map(|(name, field)| {
                (
                    name.clone(),
                    field.ty.clone(),
                    if field.required {
                        Required::Required
                    } else {
                        Required::Optional
                    },
                )
            })
            .collect()
    }
}
