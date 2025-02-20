/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use starlark_map::smallmap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInitialization;
use crate::alt::class::classdef::ClassFieldInner;
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
    pub fn get_named_tuple_elements(&self, cls: &Class) -> Vec<Name> {
        let mut elements = Vec::new();
        for name in cls.fields() {
            if let Some(range) = cls.field_decl_range(name) {
                elements.push((name.clone(), range));
            }
        }
        elements.sort_by_key(|e| e.1.start());
        elements.iter().map(|e| e.0.clone()).collect()
    }

    pub fn named_tuple_element_types(&self, cls: &ClassType) -> Option<Vec<Type>> {
        let class_metadata = self.get_metadata_for_class(cls.class_object());
        let named_tuple_metadata = class_metadata.named_tuple_metadata()?;
        Some(
            named_tuple_metadata
                .elements
                .iter()
                .filter_map(|name| {
                    let attr = self.try_lookup_attr(Type::ClassType(cls.clone()), name)?;
                    self.resolve_as_instance_method(attr)
                })
                .collect(),
        )
    }

    fn get_named_tuple_new(&self, cls: &Class, elements: &Vec<Name>) -> ClassSynthesizedField {
        let mut params = vec![Param::Pos(
            Name::new("cls"),
            Type::Type(Box::new(cls.self_type())),
            Required::Required,
        )];
        for name in elements {
            let ClassField(ClassFieldInner::Simple {
                ty, initialization, ..
            }) = &*self.get_class_member(cls, name).unwrap().value;
            let required = match initialization {
                ClassFieldInitialization::Class(_) => Required::Optional,
                ClassFieldInitialization::Instance => Required::Required,
            };
            params.push(Param::Pos(name.clone(), ty.clone(), required));
        }
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), cls.self_type())),
            CallableKind::Def,
        );
        ClassSynthesizedField::new(ty)
    }

    fn get_named_tuple_iter(&self, cls: &Class, elements: &[Name]) -> ClassSynthesizedField {
        let params = vec![cls.self_param()];
        let element_types: Vec<Type> = elements
            .iter()
            .map(|name| {
                let ClassField(ClassFieldInner::Simple { ty, .. }) =
                    &*self.get_class_member(cls, name).unwrap().value;
                ty.clone()
            })
            .collect();
        let ty = Type::Callable(
            Box::new(Callable::list(
                ParamList::new(params),
                Type::ClassType(self.stdlib.iterable(self.unions(element_types))),
            )),
            CallableKind::Def,
        );
        ClassSynthesizedField::new(ty)
    }

    fn get_named_tuple_match_args(&self, elements: &[Name]) -> ClassSynthesizedField {
        let ty = Type::Tuple(Tuple::Concrete(
            elements
                .iter()
                .map(|e| Type::Literal(Lit::String(e.as_str().into())))
                .collect(),
        ));
        ClassSynthesizedField::new(ty)
    }

    pub fn get_named_tuple_synthesized_fields(
        &self,
        cls: &Class,
    ) -> Option<ClassSynthesizedFields> {
        let metadata = self.get_metadata_for_class(cls);
        let named_tuple = metadata.named_tuple_metadata()?;
        Some(ClassSynthesizedFields::new(
            smallmap! { dunder::NEW => self.get_named_tuple_new(cls, &named_tuple.elements),
                dunder::MATCH_ARGS => self.get_named_tuple_match_args(&named_tuple.elements),
                dunder::ITER => self.get_named_tuple_iter(cls, &named_tuple.elements)
            },
        ))
    }
}
