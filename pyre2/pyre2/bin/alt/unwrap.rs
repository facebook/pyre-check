/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;
use crate::types::types::Var;

pub struct UnwrappedDict {
    pub key: Type,
    pub value: Type,
}

impl UnwrappedDict {
    pub fn to_class_type(self, stdlib: &Stdlib) -> ClassType {
        stdlib.dict(self.key, self.value)
    }

    pub fn to_type(self, stdlib: &Stdlib) -> Type {
        self.to_class_type(stdlib).to_type()
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn fresh_var(&self) -> Var {
        self.solver().fresh_unwrap(self.uniques)
    }

    fn force_var(&self, var: Var) -> Type {
        self.solver().force_var(var)
    }

    fn is_subset_eq(&self, got: &Type, want: &Type) -> bool {
        self.solver().is_subset_eq(got, want, self.type_order())
    }

    // If `error_range` is None, do not report errors
    pub fn unwrap_awaitable(&self, ty: &Type, error_range: Option<TextRange>) -> Type {
        let var = self.fresh_var();
        let awaitable_ty = self.stdlib.awaitable(var.to_type()).to_type();
        let is_awaitable = self.is_subset_eq(ty, &awaitable_ty);
        if !is_awaitable && let Some(range) = error_range {
            self.error(range, "Expression is not awaitable".to_owned())
        } else {
            self.force_var(var)
        }
    }

    pub fn unwrap_dict(&self, ty: &Type) -> Option<UnwrappedDict> {
        let key = self.fresh_var();
        let value = self.fresh_var();
        let dict_type = self.stdlib.dict(key.to_type(), value.to_type()).to_type();
        if self.is_subset_eq(&dict_type, ty) {
            Some(UnwrappedDict {
                key: self.force_var(key),
                value: self.force_var(value),
            })
        } else {
            None
        }
    }

    pub fn unwrap_set(&self, ty: &Type) -> Option<Type> {
        let elem = self.fresh_var();
        let set_type = self.stdlib.set(elem.to_type()).to_type();
        if self.is_subset_eq(&set_type, ty) {
            Some(self.force_var(elem))
        } else {
            None
        }
    }

    pub fn unwrap_list(&self, ty: &Type) -> Option<Type> {
        let elem = self.fresh_var();
        let list_type = self.stdlib.list(elem.to_type()).to_type();
        if self.is_subset_eq(&list_type, ty) {
            Some(self.force_var(elem))
        } else {
            None
        }
    }
}
