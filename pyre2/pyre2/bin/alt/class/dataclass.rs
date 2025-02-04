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
    pub(super) fn get_dataclass_fields(
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

    pub(super) fn get_dataclass_synthesized_field(
        &self,
        cls: &Class,
        name: &Name,
    ) -> Option<ClassField> {
        let metadata = self.get_metadata_for_class(cls);
        let dataclass = metadata.dataclass_metadata()?;
        if *name == dunder::INIT && dataclass.synthesized_fields.init {
            Some(self.get_dataclass_init(cls, &dataclass.fields, dataclass.kw_only))
        } else if *name == dunder::MATCH_ARGS && dataclass.synthesized_fields.match_args {
            Some(self.get_dataclass_match_args(&dataclass.fields))
        } else {
            None
        }
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
        for name in fields {
            let field = self.get_class_member(cls, name).unwrap().value;
            params.push(self.get_dataclass_param(name, field, kw_only));
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

    fn get_dataclass_match_args(&self, fields: &SmallSet<Name>) -> ClassField {
        let ts = fields
            .iter()
            .map(|name| Type::Literal(Lit::String(name.as_str().into())));
        let ty = Type::Tuple(Tuple::Concrete(ts.collect()));
        ClassField(ClassFieldInner::Simple {
            ty,
            annotation: None,
            initialization: ClassFieldInitialization::Class,
            readonly: false,
            is_enum_member: false,
        })
    }
}
