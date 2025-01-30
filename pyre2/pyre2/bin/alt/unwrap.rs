/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

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
    pub fn fresh_var(&self) -> Var {
        self.solver().fresh_unwrap(self.uniques)
    }

    fn expand_var_opt(&self, var: Var) -> Option<Type> {
        // FIXME: Really want to check if the Var is constrained in any way.
        // No way to do that currently, but this is close.
        let res = self.expand_var(var);
        if res == var.to_type() {
            None
        } else {
            Some(res)
        }
    }

    fn expand_var_pair_opt(&self, var1: Var, var2: Var) -> Option<(Type, Type)> {
        let res1 = self.expand_var(var1);
        let res2 = self.expand_var(var2);
        if res1 == var1.to_type() && res2 == var2.to_type() {
            None
        } else {
            Some((res1, res2))
        }
    }

    fn expand_var(&self, var: Var) -> Type {
        self.solver().expand(var.to_type())
    }

    fn is_subset_eq(&self, got: &Type, want: &Type) -> bool {
        self.solver().is_subset_eq(got, want, self.type_order())
    }

    pub fn is_async_generator(&self, ty: &Type) -> bool {
        let yield_ty = self.fresh_var();
        let send_ty = self.fresh_var();

        let async_generator_ty = self
            .stdlib
            .async_generator(yield_ty.to_type(), send_ty.to_type())
            .to_type();
        self.solver()
            .is_subset_eq(&async_generator_ty, ty, self.type_order())
    }

    pub fn unwrap_awaitable(&self, ty: &Type) -> Option<Type> {
        let var = self.fresh_var();
        let awaitable_ty = self.stdlib.awaitable(var.to_type()).to_type();
        if self.is_subset_eq(ty, &awaitable_ty) {
            Some(self.expand_var(var))
        } else {
            None
        }
    }

    pub fn decompose_dict(&self, ty: &Type) -> Option<UnwrappedDict> {
        let key = self.fresh_var();
        let value = self.fresh_var();
        let dict_type = self.stdlib.dict(key.to_type(), value.to_type()).to_type();
        if self.is_subset_eq(&dict_type, ty) {
            let (key, value) = self.expand_var_pair_opt(key, value)?;
            Some(UnwrappedDict { key, value })
        } else {
            None
        }
    }

    pub fn decompose_set(&self, ty: &Type) -> Option<Type> {
        let elem = self.fresh_var();
        let set_type = self.stdlib.set(elem.to_type()).to_type();
        if self.is_subset_eq(&set_type, ty) {
            self.expand_var_opt(elem)
        } else {
            None
        }
    }

    pub fn decompose_list(&self, ty: &Type) -> Option<Type> {
        let elem = self.fresh_var();
        let list_type = self.stdlib.list(elem.to_type()).to_type();
        if self.is_subset_eq(&list_type, ty) {
            self.expand_var_opt(elem)
        } else {
            None
        }
    }

    pub fn decompose_generator(&self, ty: &Type) -> Option<(Type, Type, Type)> {
        let yield_ty = self.fresh_var();
        let send_ty = self.fresh_var();
        let return_ty = self.fresh_var();
        let generator_ty = self
            .stdlib
            .generator(yield_ty.to_type(), send_ty.to_type(), return_ty.to_type())
            .to_type();
        if self.is_subset_eq(&generator_ty, ty) {
            let yield_ty: Type = self.expand_var_opt(yield_ty)?;
            let send_ty = self.expand_var_opt(send_ty).unwrap_or(Type::None);
            let return_ty = self.expand_var_opt(return_ty).unwrap_or(Type::None);
            Some((yield_ty, send_ty, return_ty))
        } else if ty.is_any() {
            Some((
                Type::any_explicit(),
                Type::any_explicit(),
                Type::any_explicit(),
            ))
        } else {
            None
        }
    }

    pub fn decompose_tuple(&self, ty: &Type) -> Option<Type> {
        let elem = self.fresh_var();
        let tuple_type = self.stdlib.tuple(elem.to_type()).to_type();
        if self.is_subset_eq(&tuple_type, ty) {
            self.expand_var_opt(elem)
        } else {
            None
        }
    }
}
