/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Clone_;
use dupe::Copy_;
use dupe::Dupe_;
use ruff_python_ast::name::Name;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::attr::Attribute;
use crate::error::collector::ErrorCollector;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;

/// `TypeOrder` provides a minimal API allowing `Subset` to request additional
/// information about types that may be required for solving bindings
///
/// This is needed for cases like the nominal type order and structural types where
/// the `Type` object itself does not contain enough information to determine
/// subset relations.
#[derive(Clone_, Copy_, Dupe_)]
pub struct TypeOrder<'a, Ans: LookupAnswer>(&'a AnswersSolver<'a, Ans>);

impl<'a, Ans: LookupAnswer> TypeOrder<'a, Ans> {
    pub fn new(solver: &'a AnswersSolver<'a, Ans>) -> Self {
        Self(solver)
    }

    pub fn stdlib(self) -> &'a Stdlib {
        self.0.stdlib
    }

    pub fn has_superclass(self, got: &Class, want: &Class, errors: &ErrorCollector) -> bool {
        self.0.has_superclass(got, want, errors)
    }

    pub fn as_superclass(
        self,
        class: &ClassType,
        want: &Class,
        errors: &ErrorCollector,
    ) -> Option<ClassType> {
        self.0.as_superclass(class, want, errors)
    }

    pub fn has_metaclass(
        self,
        cls: &Class,
        metaclass: &ClassType,
        errors: &ErrorCollector,
    ) -> bool {
        let metadata = self.0.get_metadata_for_class(cls, errors);
        match metadata.metaclass() {
            Some(m) => *m == *metaclass,
            None => *metaclass == self.stdlib().builtins_type(),
        }
    }

    pub fn is_protocol(self, cls: &Class, errors: &ErrorCollector) -> bool {
        self.0.get_metadata_for_class(cls, errors).is_protocol()
    }

    pub fn get_all_member_names(self, cls: &Class, errors: &ErrorCollector) -> SmallSet<Name> {
        self.0.get_all_member_names(cls, errors)
    }

    pub fn try_lookup_attr(
        self,
        base: Type,
        attr_name: &Name,
        errors: &ErrorCollector,
    ) -> Option<Attribute> {
        self.0.try_lookup_attr(base, attr_name, errors)
    }

    pub fn resolve_as_instance_method(self, attr: Attribute) -> Option<Type> {
        self.0.resolve_as_instance_method(attr)
    }

    pub fn is_attr_subset(
        self,
        got: &Attribute,
        want: &Attribute,
        is_subset: &mut dyn FnMut(&Type, &Type) -> bool,
    ) -> bool {
        self.0.is_attr_subset(got, want, is_subset)
    }
}
