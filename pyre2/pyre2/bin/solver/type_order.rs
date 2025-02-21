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

    pub fn has_superclass(self, got: &Class, want: &Class) -> bool {
        self.0.has_superclass(got, want)
    }

    pub fn as_superclass(self, class: &ClassType, want: &Class) -> Option<ClassType> {
        self.0.as_superclass(class, want)
    }

    pub fn has_metaclass(self, cls: &Class, metaclass: &ClassType) -> bool {
        let metadata = self.0.get_metadata_for_class(cls);
        match metadata.metaclass() {
            Some(m) => *m == *metaclass,
            None => *metaclass == self.stdlib().builtins_type(),
        }
    }

    pub fn is_protocol(self, cls: &Class) -> bool {
        self.0.get_metadata_for_class(cls).is_protocol()
    }

    pub fn get_protocol_member_names(self, cls: &Class) -> SmallSet<Name> {
        let meta = self.0.get_metadata_for_class(cls);
        if let Some(proto) = meta.protocol_metadata() {
            proto.members.clone()
        } else {
            SmallSet::new()
        }
    }

    pub fn try_lookup_attr(self, base: Type, attr_name: &Name) -> Option<Attribute> {
        self.0.try_lookup_attr(base, attr_name)
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

    pub fn named_tuple_element_types(self, cls: &ClassType) -> Option<Vec<Type>> {
        self.0.named_tuple_element_types(cls)
    }

    pub fn promote_silently(self, cls: &Class) -> Type {
        self.0.promote_silently(cls)
    }
}
