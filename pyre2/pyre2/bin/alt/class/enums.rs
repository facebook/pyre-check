/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::types::class_metadata::EnumMetadata;
use crate::binding::binding::ClassFieldInitialization;
use crate::types::literal::Lit;
use crate::types::types::Decoration;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn get_enum_member(&self, enum_: &EnumMetadata, name: &Name) -> Option<Lit> {
        if let Some(field) = self.get_class_member(enum_.cls.class_object(), name)
            && field.value.is_enum_member()
        {
            Some(Lit::Enum(Box::new((enum_.cls.clone(), name.clone()))))
        } else {
            None
        }
    }

    pub fn get_enum_members(&self, enum_: &EnumMetadata) -> SmallSet<Lit> {
        enum_
            .cls
            .class_object()
            .fields()
            .filter_map(|f| self.get_enum_member(enum_, f))
            .collect()
    }

    pub fn is_valid_enum_member(
        &self,
        name: &Name,
        ty: &Type,
        initialization: ClassFieldInitialization,
    ) -> bool {
        // Names starting but not ending with __ are private
        // Names starting and ending with _ are reserved by the enum
        if name.starts_with("__") && !name.ends_with("__")
            || name.starts_with("_") && name.ends_with("_")
        {
            return false;
        }
        // Enum members must be initialized on the class
        if initialization == ClassFieldInitialization::Instance {
            return false;
        }
        match ty {
            // Methods decorated with @member are members
            Type::Decoration(Decoration::EnumMember(_)) => true,
            // Callables are not valid enum members
            Type::BoundMethod(_, _) | Type::Callable(_, _) | Type::Decoration(_) => false,
            // Values initialized with nonmember() are not members
            Type::ClassType(cls)
                if cls.class_object().has_qname("enum", "nonmember")
                    || cls.class_object().has_qname("builtins", "staticmethod")
                    || cls.class_object().has_qname("builtins", "classmethod")
                    || cls.class_object().has_qname("enum", "property") =>
            {
                false
            }
            _ => true,
        }
    }
}
