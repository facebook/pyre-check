/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInner;
use crate::alt::types::class_metadata::ClassSynthesizedField;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::binding::binding::ClassFieldInitialization;
use crate::types::class::Class;
use crate::types::literal::Lit;
use crate::types::types::Decoration;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn get_enum_member(&self, cls: &Class, name: &Name) -> Option<Lit> {
        if let Some(ClassField(ClassFieldInner::Simple {
            ty: Type::Literal(lit),
            ..
        })) = self.get_class_field(cls, name)
            && matches!(&lit, Lit::Enum(box (lit_cls, ..)) if lit_cls.class_object() == cls)
        {
            Some(lit)
        } else {
            None
        }
    }

    pub fn get_enum_members(&self, cls: &Class) -> SmallSet<Lit> {
        cls.fields()
            .filter_map(|f| self.get_enum_member(cls, f))
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
            Type::BoundMethod(_) | Type::Callable(_, _) | Type::Decoration(_) => false,
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

    pub fn get_enum_synthesized_fields(&self, cls: &Class) -> Option<ClassSynthesizedFields> {
        let metadata = self.get_metadata_for_class(cls);
        let enum_metadata = metadata.enum_metadata()?;
        let mut fields = SmallMap::new();
        // Enum classes cannot inherit members.
        for name in cls.fields() {
            if let Some(ClassField(ClassFieldInner::Simple {
                ty, initialization, ..
            })) = self.get_class_field(cls, name)
            {
                if self.is_valid_enum_member(name, &ty, initialization) {
                    let lit = Type::Literal(Lit::Enum(Box::new((
                        enum_metadata.cls.clone(),
                        name.clone(),
                        ty,
                    ))));
                    fields.insert(name.clone(), ClassSynthesizedField::new(lit, true));
                }
            }
        }
        Some(ClassSynthesizedFields::new(fields))
    }
}
