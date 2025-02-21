/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::types::callable::Param;
use crate::types::callable::Required;
use crate::types::types::Type;
use crate::types::types::Var;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn fresh_var(&self) -> Var {
        self.solver().fresh_unwrap(self.uniques)
    }

    fn expand_var_opt(&self, var: Var) -> Option<Type> {
        // FIXME: Really want to check if the Var is constrained in any way.
        // No way to do that currently, but this is close.
        let res = self.expand_var(var);
        if matches!(res, Type::Var(..)) {
            None
        } else {
            Some(res)
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

    pub fn unwrap_mapping(&self, ty: &Type) -> Option<(Type, Type)> {
        let key = self.fresh_var();
        let value = self.fresh_var();
        let dict_type = self
            .stdlib
            .mapping(key.to_type(), value.to_type())
            .to_type();
        if self.is_subset_eq(ty, &dict_type) {
            Some((self.expand_var(key), self.expand_var(value)))
        } else {
            None
        }
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

    pub fn unwrap_generator(&self, ty: &Type) -> Option<(Type, Type, Type)> {
        let yield_ty = self.fresh_var();
        let send_ty = self.fresh_var();
        let return_ty = self.fresh_var();
        let generator_ty = self
            .stdlib
            .generator(yield_ty.to_type(), send_ty.to_type(), return_ty.to_type())
            .to_type();
        if self.is_subset_eq(ty, &generator_ty) {
            let yield_ty: Type = self.expand_var(yield_ty);
            let send_ty = self.expand_var(send_ty);
            let return_ty = self.expand_var(return_ty);
            Some((yield_ty, send_ty, return_ty))
        } else {
            None
        }
    }

    pub fn unwrap_iterable(&self, ty: &Type) -> Option<Type> {
        let iter_ty = self.fresh_var();
        let iterable_ty = self.stdlib.iterable(iter_ty.to_type()).to_type();
        if self.is_subset_eq(ty, &iterable_ty) {
            Some(self.expand_var(iter_ty))
        } else {
            None
        }
    }

    pub fn decompose_dict(&self, ty: &Type) -> (Option<Type>, Option<Type>) {
        let key = self.fresh_var();
        let value = self.fresh_var();
        let dict_type = self.stdlib.dict(key.to_type(), value.to_type()).to_type();
        if self.is_subset_eq(&dict_type, ty) {
            let key = self.expand_var_opt(key);
            let value = self.expand_var_opt(value);
            (key, value)
        } else {
            (None, None)
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

    pub fn decompose_lambda(&self, ty: &Type, param_vars: &[Var]) -> Option<Type> {
        let return_ty = self.fresh_var();
        // Note that parameters are optional and we add a `*args: Any`. This is to ensure that
        // our callable_ty is compatible with _any_ callable type hint. This is because we want
        // contextual typing for lambda params even if the callable signature doesn't match the
        // hint.
        //
        // For example, `f: Callable[[int], None] = lambda x y: ...` should give a contextual
        // hint for `x`, even though the number of params doesn't line up.
        //
        // Why? Well, this behavior gives more stability for transiently-invalid code. For example,
        // imagine you have a contextually typed lambda, and you add a parameter to it, which
        // causes an error. Without this permissiveness, the checking of the body of the lambda
        // will be affected, because we lose contextual type information for the existing params.
        let params = param_vars
            .iter()
            .map(|var| Param::PosOnly(var.to_type(), Required::Optional))
            .chain(std::iter::once(Param::VarArg(Type::any_implicit())))
            .collect::<Vec<_>>();
        let callable_ty = Type::callable(params, return_ty.to_type());

        if self.is_subset_eq(&callable_ty, ty) {
            self.expand_var_opt(return_ty)
        } else {
            None
        }
    }

    pub fn decompose_generator_yield(&self, ty: &Type) -> Option<Type> {
        let yield_ty = self.fresh_var();
        let generator_ty = self
            .stdlib
            .generator(
                yield_ty.to_type(),
                self.fresh_var().to_type(),
                self.fresh_var().to_type(),
            )
            .to_type();
        if self.is_subset_eq(&generator_ty, ty) {
            self.expand_var_opt(yield_ty)
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

    pub fn decompose_async_generator(&self, ty: &Type) -> Option<(Type, Type)> {
        let yield_ty = self.fresh_var();
        let send_ty = self.fresh_var();
        let async_generator_ty = self
            .stdlib
            .async_generator(yield_ty.to_type(), send_ty.to_type())
            .to_type();
        if self.is_subset_eq(&async_generator_ty, ty) {
            let yield_ty: Type = self.expand_var_opt(yield_ty)?;
            let send_ty = self.expand_var_opt(send_ty).unwrap_or(Type::None);
            Some((yield_ty, send_ty))
        } else if ty.is_any() {
            Some((Type::any_explicit(), Type::any_explicit()))
        } else {
            None
        }
    }
}
