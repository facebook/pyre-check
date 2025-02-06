/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use ruff_python_ast::name::Name;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInner;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::binding::binding::ClassFieldInitialization;
use crate::dunder;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::tuple::Tuple;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    /// Gets dataclass fields for an `@dataclass`-decorated class.
    pub fn get_dataclass_fields(
        &self,
        cls: &Class,
        bases_with_metadata: &[(ClassType, Arc<ClassMetadata>)],
    ) -> SmallSet<Name> {
        let mut all_fields = SmallSet::new();
        for (_, metadata) in bases_with_metadata.iter().rev() {
            if let Some(dataclass) = metadata.dataclass_metadata() {
                all_fields.extend(dataclass.fields.clone());
            }
        }
        for name in cls.fields() {
            if cls.is_field_annotated(name) {
                all_fields.insert(name.clone());
            }
        }
        all_fields
    }

    pub fn get_dataclass_synthesized_field(&self, cls: &Class, name: &Name) -> Option<ClassField> {
        let metadata = self.get_metadata_for_class(cls);
        let dataclass = metadata.dataclass_metadata()?;
        if *name == dunder::INIT && dataclass.kws.init {
            Some(self.get_dataclass_init(cls, &dataclass.fields, dataclass.kws.kw_only))
        } else if *name == dunder::MATCH_ARGS && dataclass.kws.match_args {
            Some(self.get_dataclass_match_args(cls, &dataclass.fields, dataclass.kws.kw_only))
        } else {
            None
        }
    }

    fn iter_fields(&self, cls: &Class, fields: &SmallSet<Name>) -> Vec<(Name, ClassField, bool)> {
        let mut kw_only = false;
        fields.iter().filter_map(|name| {
            let field @ ClassField(ClassFieldInner::Simple { ty, .. }) = &self.get_class_member(cls, name).unwrap().value;
            // A field with type KW_ONLY is a sentinel value that indicates that the remaining
            // fields should be keyword-only params in the generated `__init__`.
            if matches!(ty, Type::ClassType(cls) if cls.class_object().has_qname("dataclasses", "KW_ONLY")) {
                kw_only = true;
                None
            } else {
                Some((name.clone(), field.clone(), kw_only))
            }
        }).collect()
    }

    /// Gets a dataclass field as a function param.
    fn get_dataclass_param(&self, name: &Name, field: ClassField, kw_only: bool) -> Param {
        let ClassField(ClassFieldInner::Simple {
            ty, initialization, ..
        }) = field;
        let required = match initialization {
            ClassFieldInitialization::Class => Required::Required,
            ClassFieldInitialization::Instance => Required::Optional,
        };
        if kw_only {
            Param::KwOnly(name.clone(), ty, required)
        } else {
            Param::Pos(name.clone(), ty, required)
        }
    }

    /// Gets __init__ method for an `@dataclass`-decorated class.
    fn get_dataclass_init(
        &self,
        cls: &Class,
        fields: &SmallSet<Name>,
        kw_only: bool,
    ) -> ClassField {
        let mut params = vec![Param::Pos(
            Name::new("self"),
            cls.self_type(),
            Required::Required,
        )];
        for (name, field, field_kw_only) in self.iter_fields(cls, fields) {
            params.push(self.get_dataclass_param(&name, field, kw_only || field_kw_only));
        }
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), Type::None)),
            CallableKind::Def,
        );
        ClassField(ClassFieldInner::Simple {
            ty,
            annotation: None,
            initialization: ClassFieldInitialization::Class,
            readonly: false,
            is_enum_member: false,
        })
    }

    fn get_dataclass_match_args(
        &self,
        cls: &Class,
        fields: &SmallSet<Name>,
        kw_only: bool,
    ) -> ClassField {
        // Keyword-only fields do not appear in __match_args__.
        let ts = if kw_only {
            Vec::new()
        } else {
            let filtered_fields = self.iter_fields(cls, fields);
            filtered_fields
                .iter()
                .filter_map(|(name, _, field_kw_only)| {
                    if *field_kw_only {
                        None
                    } else {
                        Some(Type::Literal(Lit::String(name.as_str().into())))
                    }
                })
                .collect()
        };
        let ty = Type::Tuple(Tuple::Concrete(ts));
        ClassField(ClassFieldInner::Simple {
            ty,
            annotation: None,
            initialization: ClassFieldInitialization::Class,
            readonly: false,
            is_enum_member: false,
        })
    }
}
