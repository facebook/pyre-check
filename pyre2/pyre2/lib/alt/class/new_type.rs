/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Identifier;
use starlark_map::smallmap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
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
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn get_new_type_field_params(&self, cls: &Class, base_class: ClassType) -> Param {
        let required = Required::Required;
        Param::Pos(
            Name::new(Identifier::new("_x", cls.range())),
            base_class.to_type(),
            required,
        )
    }

    fn get_new_type_init(&self, cls: &Class, base: ClassType) -> ClassSynthesizedField {
        let mut params = vec![cls.self_param()];
        params.push(self.get_new_type_field_params(cls, base));
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), cls.self_type())),
            CallableKind::Def,
        );
        ClassSynthesizedField::new(ty)
    }

    pub fn get_new_type_synthesized_fields(&self, cls: &Class) -> Option<ClassSynthesizedFields> {
        let metadata = self.get_metadata_for_class(cls);

        let base_type = metadata.bases_with_metadata();
        let is_new_type = metadata.is_new_type();

        if is_new_type && base_type.len() == 1 {
            let (base_class, _) = base_type[0].clone();
            Some(ClassSynthesizedFields::new(smallmap! {
                dunder::INIT => self.get_new_type_init(cls, base_class),
            }))
        } else {
            None
        }
    }
}
