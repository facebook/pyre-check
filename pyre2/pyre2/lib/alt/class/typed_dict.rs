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
use crate::error::context::TypeCheckContext;
use crate::error::context::TypeCheckKind;
use crate::error::kind::ErrorKind;
use crate::types::callable::Callable;
use crate::types::callable::FuncMetadata;
use crate::types::callable::Function;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::Substitution;
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
        let fields = self.typed_dict_fields(typed_dict);
        let mut has_expansion = false;
        let mut keys: SmallSet<Name> = SmallSet::new();
        dict_items.iter().for_each(|x| match &x.key {
            Some(key) => {
                let key_type = self.expr_infer(key, errors);
                if let Type::Literal(Lit::String(name)) = key_type {
                    let key_name = Name::new(name);
                    if let Some(field) = fields.get(&key_name) {
                        self.expr(
                            &x.value,
                            Some((&field.ty, &|| {
                                TypeCheckContext::of_kind(TypeCheckKind::TypedDictKey(
                                    key_name.clone(),
                                ))
                            })),
                            errors,
                        );
                    } else {
                        self.error(
                            errors,
                            key.range(),
                            ErrorKind::TypedDictKeyError,
                            None,
                            format!(
                                "Key `{}` is not defined in TypedDict `{}`",
                                key_name,
                                typed_dict.name()
                            ),
                        );
                    }
                    keys.insert(key_name);
                } else {
                    self.error(
                        errors,
                        key.range(),
                        ErrorKind::TypedDictKeyError,
                        None,
                        format!("Expected string literal key, got `{}`", key_type),
                    );
                }
            }
            None => {
                has_expansion = true;
                self.expr(
                    &x.value,
                    Some((&Type::TypedDict(Box::new(typed_dict.clone())), &|| {
                        TypeCheckContext::of_kind(TypeCheckKind::TypedDictUnpacking)
                    })),
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
                        ErrorKind::TypedDictKeyError,
                        None,
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

    fn substitution(&self, typed_dict: &TypedDict, class: &Class) -> Substitution {
        let targs = typed_dict.targs();
        let tparams = class.tparams();
        Substitution::new(
            tparams
                .quantified()
                .zip(targs.as_slice().iter().cloned())
                .collect(),
        )
    }

    // Get the field names + requiredness, given the ClassMetadata of a typed dict.
    // Callers must be certain the class is a typed dict, we will panic if it is not.
    fn fields_from_metadata<'m>(metadata: &'m ClassMetadata) -> &'m SmallMap<Name, bool> {
        &metadata.typed_dict_metadata().unwrap().fields
    }

    fn class_field_to_typed_dict_field(
        &self,
        class: &Class,
        substitution: &Substitution,
        name: &Name,
        is_total: bool,
    ) -> Option<TypedDictField> {
        self.get_class_member(class, name).and_then(|member| {
            Arc::unwrap_or_clone(member.value)
                .as_typed_dict_field_info(is_total)
                .map(|field| field.substitute(substitution))
        })
    }

    pub fn typed_dict_fields(&self, typed_dict: &TypedDict) -> SmallMap<Name, TypedDictField> {
        let class = typed_dict.class_object();
        let metadata = self.get_metadata_for_class(class);
        let substitution = self.substitution(typed_dict, class);
        Self::fields_from_metadata(&metadata)
            .iter()
            .filter_map(|(name, is_total)| {
                self.class_field_to_typed_dict_field(class, &substitution, name, *is_total)
                    .map(|field| (name.clone(), field))
            })
            .collect()
    }

    pub fn typed_dict_field(&self, typed_dict: &TypedDict, name: &Name) -> Option<TypedDictField> {
        let class = typed_dict.class_object();
        let metadata = self.get_metadata_for_class(class);
        let substitution = self.substitution(typed_dict, class);
        Self::fields_from_metadata(&metadata)
            .get(name)
            .and_then(|is_total| {
                self.class_field_to_typed_dict_field(class, &substitution, name, *is_total)
            })
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
        let ty = Type::Function(Box::new(Function {
            signature: Callable::list(ParamList::new(params), Type::None),
            metadata: FuncMetadata::def(
                self.module_info().name(),
                cls.name().clone(),
                dunder::INIT,
            ),
        }));
        ClassSynthesizedField::new(ty)
    }

    pub fn get_typed_dict_synthesized_fields(&self, cls: &Class) -> Option<ClassSynthesizedFields> {
        let metadata = self.get_metadata_for_class(cls);
        let td = metadata.typed_dict_metadata()?;
        Some(ClassSynthesizedFields::new(
            smallmap! { dunder::INIT => self.get_typed_dict_init(cls, &td.fields) },
        ))
    }

    pub fn typed_dict_kw_param_info(&self, typed_dict: &TypedDict) -> Vec<(Name, Type, Required)> {
        self.typed_dict_fields(typed_dict)
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
