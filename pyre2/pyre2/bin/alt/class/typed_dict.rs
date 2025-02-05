/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::DictItem;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::ordered_map::OrderedMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInner;
use crate::types::annotation::Annotation;
use crate::types::annotation::Qualifier;
use crate::types::class::Class;
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
    ) {
        let fields = typed_dict.fields();
        let mut has_expansion = false;
        let mut keys: SmallSet<Name> = SmallSet::new();
        dict_items.iter().for_each(|x| match &x.key {
            Some(key) => {
                let key_type = self.expr(key, None);
                if let Type::Literal(Lit::String(name)) = key_type {
                    let key_name = Name::new(name.clone());
                    if let Some(field) = fields.get(&key_name) {
                        self.expr(&x.value, Some(&field.ty));
                    } else {
                        self.error(
                            key.range(),
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
                        key.range(),
                        format!("Expected string literal key, got `{}`", key_type),
                    );
                }
            }
            None => {
                has_expansion = true;
                self.expr(
                    &x.value,
                    Some(&Type::TypedDict(Box::new(typed_dict.clone()))),
                );
            }
        });
        if !has_expansion {
            fields.iter().for_each(|(key, field)| {
                if field.required && !keys.contains(key) {
                    self.error(
                        range,
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

    pub(super) fn get_typed_dict_fields(
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
        self.get_all_members(cls)
            .iter()
            .filter_map(|(name, (field, cls))| {
                let metadata = self.get_metadata_for_class(cls);
                if !metadata.is_typed_dict() {
                    return None;
                }
                if let ClassField(ClassFieldInner::Simple {
                    annotation:
                        Some(Annotation {
                            ty: Some(ty),
                            qualifiers,
                        }),
                    ..
                }) = field
                {
                    let is_total = metadata
                        .get_keyword(&Name::new("total"))
                        .map_or(true, |ty| match ty {
                            Type::Literal(Lit::Bool(b)) => b,
                            _ => true,
                        });
                    Some((
                        name.clone(),
                        TypedDictField {
                            ty: substitution.substitute(ty.clone()),
                            required: if qualifiers.contains(&Qualifier::Required) {
                                true
                            } else if qualifiers.contains(&Qualifier::NotRequired) {
                                false
                            } else {
                                is_total
                            },
                            read_only: qualifiers.contains(&Qualifier::ReadOnly),
                        },
                    ))
                } else {
                    None
                }
            })
            .collect()
    }
}
