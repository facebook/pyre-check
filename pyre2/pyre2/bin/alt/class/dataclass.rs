/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use ruff_python_ast::name::Name;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInner;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedField;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
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

    pub fn get_dataclass_synthesized_fields(&self, cls: &Class) -> Option<ClassSynthesizedFields> {
        let metadata = self.get_metadata_for_class(cls);
        let dataclass = metadata.dataclass_metadata()?;
        let mut fields = SmallMap::new();
        if dataclass.kws.init {
            fields.insert(
                dunder::INIT,
                self.get_dataclass_init(cls, &dataclass.fields, dataclass.kws.kw_only),
            );
        }
        if dataclass.kws.order {
            fields.extend(self.get_dataclass_rich_comparison_methods(cls));
        }
        if dataclass.kws.match_args {
            fields.insert(
                dunder::MATCH_ARGS,
                self.get_dataclass_match_args(cls, &dataclass.fields, dataclass.kws.kw_only),
            );
        }
        Some(ClassSynthesizedFields::new(fields))
    }

    fn iter_fields(&self, cls: &Class, fields: &SmallSet<Name>) -> Vec<(Name, ClassField, bool)> {
        let mut kw_only = false;
        fields.iter().filter_map(|name| {
            let field @ ClassField(ClassFieldInner::Simple { ty, .. }) = &*self.get_class_member(cls, name).unwrap().value;
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

    /// Gets __init__ method for an `@dataclass`-decorated class.
    fn get_dataclass_init(
        &self,
        cls: &Class,
        fields: &SmallSet<Name>,
        kw_only: bool,
    ) -> ClassSynthesizedField {
        let mut params = vec![Param::Pos(
            Name::new("self"),
            cls.self_type(),
            Required::Required,
        )];
        for (name, field, field_kw_only) in self.iter_fields(cls, fields) {
            params.push(field.as_param(&name, kw_only || field_kw_only));
        }
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), Type::None)),
            CallableKind::Def,
        );
        ClassSynthesizedField::new(ty, false)
    }

    fn get_dataclass_match_args(
        &self,
        cls: &Class,
        fields: &SmallSet<Name>,
        kw_only: bool,
    ) -> ClassSynthesizedField {
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
        ClassSynthesizedField::new(ty, false)
    }

    fn get_dataclass_rich_comparison_methods(
        &self,
        cls: &Class,
    ) -> SmallMap<Name, ClassSynthesizedField> {
        let self_ty = cls.self_type();
        let self_ = Param::Pos(Name::new("self"), self_ty.clone(), Required::Required);
        let other = Param::Pos(Name::new("other"), self_ty, Required::Required);
        let ret = Type::ClassType(self.stdlib.bool());
        let field = ClassSynthesizedField::new(
            Type::Callable(
                Box::new(Callable::list(ParamList::new(vec![self_, other]), ret)),
                CallableKind::Def,
            ),
            false,
        );
        dunder::RICH_CMPS
            .iter()
            .map(|name| (name.clone(), field.clone()))
            .collect()
    }
}
