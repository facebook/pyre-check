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
use crate::alt::class::class_field::ClassField;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedField;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::dunder;
use crate::types::callable::BoolKeywords;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::DataclassKeywords;
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
        if dataclass.kws.is_set(&DataclassKeywords::INIT) {
            fields.insert(
                dunder::INIT,
                self.get_dataclass_init(
                    cls,
                    &dataclass.fields,
                    dataclass.kws.is_set(&DataclassKeywords::KW_ONLY),
                ),
            );
        }
        if dataclass.kws.is_set(&DataclassKeywords::ORDER) {
            fields.extend(self.get_dataclass_rich_comparison_methods(cls));
        }
        if dataclass.kws.is_set(&DataclassKeywords::MATCH_ARGS) {
            fields.insert(
                dunder::MATCH_ARGS,
                self.get_dataclass_match_args(
                    cls,
                    &dataclass.fields,
                    dataclass.kws.is_set(&DataclassKeywords::KW_ONLY),
                ),
            );
        }
        // See rules for `__hash__` creation under "unsafe_hash":
        // https://docs.python.org/3/library/dataclasses.html#module-contents
        if dataclass.kws.is_set(&DataclassKeywords::UNSAFE_HASH)
            || (dataclass.kws.is_set(&DataclassKeywords::EQ)
                && dataclass.kws.is_set(&DataclassKeywords::FROZEN))
        {
            fields.insert(dunder::HASH, self.get_dataclass_hash(cls));
        } else if dataclass.kws.is_set(&DataclassKeywords::EQ) {
            fields.insert(dunder::HASH, ClassSynthesizedField::new(Type::None));
        }
        Some(ClassSynthesizedFields::new(fields))
    }

    fn iter_fields(
        &self,
        cls: &Class,
        fields: &SmallSet<Name>,
    ) -> Vec<(Name, ClassField, BoolKeywords)> {
        let mut kw_only = false;
        fields
            .iter()
            .filter_map(|name| {
                let field = &*self.get_class_member(cls, name).unwrap().value;
                // A field with type KW_ONLY is a sentinel value that indicates that the remaining
                // fields should be keyword-only params in the generated `__init__`.
                if field.is_dataclass_kwonly_marker() {
                    kw_only = true;
                    None
                } else {
                    field
                        .dataclass_flags_of(kw_only)
                        .map(|flags| (name.clone(), field.clone(), flags))
                }
            })
            .collect()
    }

    /// Gets __init__ method for an `@dataclass`-decorated class.
    fn get_dataclass_init(
        &self,
        cls: &Class,
        fields: &SmallSet<Name>,
        kw_only: bool,
    ) -> ClassSynthesizedField {
        let mut params = vec![cls.self_param()];
        for (name, field, field_flags) in self.iter_fields(cls, fields) {
            if field_flags.is_set(&DataclassKeywords::INIT) {
                params.push(field.as_param(
                    &name,
                    field_flags.is_set(&DataclassKeywords::DEFAULT),
                    kw_only || field_flags.is_set(&DataclassKeywords::KW_ONLY),
                ));
            }
        }
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), Type::None)),
            CallableKind::Def,
        );
        ClassSynthesizedField::new(ty)
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
                .filter_map(|(name, _, field_flags)| {
                    if field_flags.is_set(&DataclassKeywords::KW_ONLY) {
                        None
                    } else {
                        Some(Type::Literal(Lit::String(name.as_str().into())))
                    }
                })
                .collect()
        };
        let ty = Type::Tuple(Tuple::Concrete(ts));
        ClassSynthesizedField::new(ty)
    }

    fn get_dataclass_rich_comparison_methods(
        &self,
        cls: &Class,
    ) -> SmallMap<Name, ClassSynthesizedField> {
        let self_ = cls.self_param();
        let other = Param::Pos(Name::new("other"), cls.self_type(), Required::Required);
        let ret = Type::ClassType(self.stdlib.bool());
        let field = ClassSynthesizedField::new(Type::Callable(
            Box::new(Callable::list(ParamList::new(vec![self_, other]), ret)),
            CallableKind::Def,
        ));
        dunder::RICH_CMPS
            .iter()
            .map(|name| (name.clone(), field.clone()))
            .collect()
    }

    fn get_dataclass_hash(&self, cls: &Class) -> ClassSynthesizedField {
        let params = vec![cls.self_param()];
        let ret = self.stdlib.int().to_type();
        ClassSynthesizedField::new(Type::Callable(
            Box::new(Callable::list(ParamList::new(params), ret)),
            CallableKind::Def,
        ))
    }
}
