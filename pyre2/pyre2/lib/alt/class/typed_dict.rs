/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use ruff_python_ast::name::Name;
use ruff_python_ast::DictItem;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;
use starlark_map::smallmap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedField;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::Substitution;
use crate::types::class::TArgs;
use crate::types::literal::Lit;
use crate::types::typed_dict::TypedDict;
use crate::types::typed_dict::TypedDictField;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn check_dict_items_against_typed_dict(
        &self,
        dict_items: Vec<&DictItem>,
        typed_dict: &TypedDict,
        range: TextRange,
        errors: &ErrorCollector,
    ) {
        let fields = typed_dict.fields();
        let mut has_expansion = false;
        let mut keys: SmallSet<Name> = SmallSet::new();
        dict_items.iter().for_each(|x| match &x.key {
            Some(key) => {
                let key_type = self.expr_infer(key, errors);
                if let Type::Literal(Lit::String(name)) = key_type {
                    let key_name = Name::new(name.clone());
                    if let Some(field) = fields.get(&key_name) {
                        self.expr(&x.value, Some(&field.ty), errors);
                    } else {
                        self.error(
                            errors,
                            key.range(),
                            ErrorKind::Unknown,
                            format!(
                                "Key `{}` is not defined in TypedDict `{}`",
                                name,
                                typed_dict.name()
                            ),
                        );
                    }
                    keys.insert(key_name);
                } else {
                    self.error(
                        errors,
                        key.range(),
                        ErrorKind::Unknown,
                        format!("Expected string literal key, got `{}`", key_type),
                    );
                }
            }
            None => {
                has_expansion = true;
                self.expr(
                    &x.value,
                    Some(&Type::TypedDict(Box::new(typed_dict.clone()))),
                    errors,
                );
            }
        });
        if !has_expansion {
            fields.iter().for_each(|(key, field)| {
                if field.required && !keys.contains(key) {
                    self.error(
                        errors,
                        range,
                        ErrorKind::Unknown,
                        format!(
                            "Missing required key `{}` for TypedDict `{}`",
                            key,
                            typed_dict.name()
                        ),
                    );
                }
            });
        }
    }

    pub fn get_typed_dict_fields(
        &self,
        cls: &Class,
        bases_with_metadata: &[(ClassType, Arc<ClassMetadata>)],
        is_total: bool,
    ) -> SmallMap<Name, bool> {
        let mut all_fields = SmallMap::new();
        for (_, metadata) in bases_with_metadata.iter().rev() {
            if let Some(td) = metadata.typed_dict_metadata() {
                all_fields.extend(td.fields.clone());
            }
        }
        for name in cls.fields() {
            if cls.is_field_annotated(name) {
                all_fields.insert(name.clone(), is_total);
            }
        }
        all_fields
    }

    pub fn sub_typed_dict_fields(
        &self,
        cls: &Class,
        targs: &TArgs,
    ) -> OrderedMap<Name, TypedDictField> {
        let tparams = cls.tparams();
        let substitution = Substitution::new(
            tparams
                .quantified()
                .zip(targs.as_slice().iter().cloned())
                .collect(),
        );
        let metadata = self.get_metadata_for_class(cls);
        metadata
            .typed_dict_metadata()
            .unwrap()
            .fields
            .iter()
            .filter_map(|(name, is_total)| {
                self.get_class_member(cls, name)
                    .and_then(|member| {
                        Arc::unwrap_or_clone(member.value).as_typed_dict_field_info(*is_total)
                    })
                    .map(|field| (name.clone(), field.substitute(&substitution)))
            })
            .collect()
    }

    fn get_typed_dict_init(
        &self,
        cls: &Class,
        fields: &SmallMap<Name, bool>,
    ) -> ClassSynthesizedField {
        let mut params = vec![cls.self_param()];
        for (name, is_total) in fields {
            // TODO(stroxler): Look into whether we can re-wire the code so that it is not possible to
            // have the typed dict think a field exists that cannot be converted to a `TypedDictField`
            // (this can happen for any unannotated field - e.g. a classmethod or staticmethod).
            if let Some(field) = self.get_class_member(cls, name).and_then(|member| {
                Arc::unwrap_or_clone(member.value).as_typed_dict_field_info(*is_total)
            }) {
                params.push(Param::Pos(
                    name.clone(),
                    field.ty,
                    if field.required {
                        Required::Required
                    } else {
                        Required::Optional
                    },
                ));
            }
        }
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), Type::None)),
            CallableKind::Def,
        );
        ClassSynthesizedField::new(ty)
    }

    pub fn get_typed_dict_synthesized_fields(&self, cls: &Class) -> Option<ClassSynthesizedFields> {
        let metadata = self.get_metadata_for_class(cls);
        let td = metadata.typed_dict_metadata()?;
        Some(ClassSynthesizedFields::new(
            smallmap! { dunder::INIT => self.get_typed_dict_init(cls, &td.fields) },
        ))
    }
}
