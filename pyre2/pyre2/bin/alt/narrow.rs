/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::binding::narrow::NarrowOp;
use crate::binding::narrow::NarrowVal;
use crate::error::collector::ErrorCollector;
use crate::types::callable::CallableKind;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::tuple::Tuple;
use crate::types::types::CalleeKind;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    // Get the union of all members of an enum, minus the specified member
    fn subtract_enum_member(&self, cls: &ClassType, name: &Name, errors: &ErrorCollector) -> Type {
        let e = self.get_enum_from_class_type(cls).unwrap();
        // Enums derived from enum.Flag cannot be treated as a union of their members
        if e.is_flag {
            return Type::ClassType(cls.clone());
        }
        self.unions(
            self.get_enum_members(cls.class_object())
                .into_iter()
                .filter_map(|f| {
                    if let Lit::Enum(box (_, member_name, _)) = &f
                        && *member_name == *name
                    {
                        None
                    } else {
                        Some(Type::Literal(f))
                    }
                })
                .collect::<Vec<_>>(),
            errors,
        )
    }

    fn intersect(&self, left: &Type, right: &Type, errors: &ErrorCollector) -> Type {
        // Get our best approximation of ty & right.
        self.distribute_over_union(left, errors, |t| {
            if self
                .solver()
                .is_subset_eq(right, t, self.type_order(), errors)
            {
                right.clone()
            } else if self
                .solver()
                .is_subset_eq(t, right, self.type_order(), errors)
            {
                t.clone()
            } else {
                Type::never()
            }
        })
    }

    fn subtract(&self, left: &Type, right: &Type, errors: &ErrorCollector) -> Type {
        self.distribute_over_union(left, errors, |left| {
            if self
                .solver()
                .is_subset_eq(left, right, self.type_order(), errors)
            {
                Type::never()
            } else {
                left.clone()
            }
        })
    }

    fn resolve_narrowing_call(
        &self,
        func: &NarrowVal,
        args: &Arguments,
        errors: &ErrorCollector,
    ) -> Option<NarrowOp> {
        let func_ty = self.narrow_val_infer(func, errors);
        if args.args.len() > 1 {
            let second_arg = &args.args[1];
            let op = match func_ty.callee_kind() {
                Some(CalleeKind::Callable(CallableKind::IsInstance)) => Some(NarrowOp::IsInstance(
                    NarrowVal::Expr(Box::new(second_arg.clone())),
                )),
                Some(CalleeKind::Callable(CallableKind::IsSubclass)) => Some(NarrowOp::IsSubclass(
                    NarrowVal::Expr(Box::new(second_arg.clone())),
                )),
                _ => None,
            };
            if op.is_some() {
                return op;
            }
        }
        func_ty
            .as_typeguard()
            .map(|t| NarrowOp::TypeGuard(t.clone()))
    }

    fn narrow_val_infer(&self, val: &NarrowVal, errors: &ErrorCollector) -> Type {
        match val {
            NarrowVal::Expr(e) => self.expr(e, None, errors),
            NarrowVal::Type(t, _) => (**t).clone(),
        }
    }

    fn distribute_narrow_op_over_tuple(
        &self,
        build_op: &dyn Fn(NarrowVal) -> NarrowOp,
        ty: &Type,
        range: TextRange,
    ) -> Option<NarrowOp> {
        if let Type::Tuple(Tuple::Concrete(ts)) = ty {
            Some(NarrowOp::Or(
                ts.iter()
                    .map(|t| build_op(NarrowVal::Type(Box::new(t.clone()), range)))
                    .collect(),
            ))
        } else {
            None
        }
    }

    fn unwrap_class_object_or_error(
        &self,
        ty: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Option<Type> {
        let unwrapped = self.unwrap_class_object_silently(ty);
        if unwrapped.is_none() {
            self.error(errors, range, format!("Expected class object, got {}", ty));
        }
        unwrapped
    }

    pub fn narrow(&self, ty: &Type, op: &NarrowOp, errors: &ErrorCollector) -> Type {
        match op {
            NarrowOp::Is(v) => {
                let right = self.narrow_val_infer(v, errors);
                // Get our best approximation of ty & right.
                self.intersect(ty, &right, errors)
            }
            NarrowOp::IsNot(v) => {
                let right = self.narrow_val_infer(v, errors);
                // Get our best approximation of ty - right.
                self.distribute_over_union(ty, errors, |t| {
                    // Only certain literal types can be compared by identity.
                    match (t, &right) {
                        (
                            _,
                            Type::None | Type::Literal(Lit::Bool(_)) | Type::Literal(Lit::Enum(_)),
                        ) if *t == right => Type::never(),
                        (Type::ClassType(cls), Type::Literal(Lit::Bool(b)))
                            if *cls == self.stdlib.bool() =>
                        {
                            Type::Literal(Lit::Bool(!b))
                        }
                        (
                            Type::ClassType(left_cls),
                            Type::Literal(Lit::Enum(box (right_cls, name, _))),
                        ) if *left_cls == *right_cls => {
                            self.subtract_enum_member(left_cls, name, errors)
                        }
                        _ => t.clone(),
                    }
                })
            }
            NarrowOp::IsInstance(v) => {
                let right = self.narrow_val_infer(v, errors);
                if let Some(distributed_op) =
                    self.distribute_narrow_op_over_tuple(&NarrowOp::IsInstance, &right, v.range())
                {
                    self.narrow(ty, &distributed_op, errors)
                } else if let Some(right) =
                    self.unwrap_class_object_or_error(&right, v.range(), errors)
                {
                    self.intersect(ty, &right, errors)
                } else {
                    ty.clone()
                }
            }
            NarrowOp::IsNotInstance(v) => {
                let right = self.narrow_val_infer(v, errors);
                if let Some(distributed_op) =
                    self.distribute_narrow_op_over_tuple(&NarrowOp::IsInstance, &right, v.range())
                {
                    self.narrow(ty, &distributed_op.negate(), errors)
                } else if let Some(right) =
                    self.unwrap_class_object_or_error(&right, v.range(), errors)
                {
                    self.subtract(ty, &right, errors)
                } else {
                    ty.clone()
                }
            }
            NarrowOp::IsSubclass(v) => {
                let right = self.narrow_val_infer(v, errors);
                if let Some(distributed_op) =
                    self.distribute_narrow_op_over_tuple(&NarrowOp::IsSubclass, &right, v.range())
                {
                    self.narrow(ty, &distributed_op, errors)
                } else if let Some(left) = self.untype_opt(ty.clone(), v.range(), errors)
                    && let Some(right) =
                        self.unwrap_class_object_or_error(&right, v.range(), errors)
                {
                    Type::type_form(self.intersect(&left, &right, errors))
                } else {
                    ty.clone()
                }
            }
            NarrowOp::IsNotSubclass(v) => {
                let right = self.narrow_val_infer(v, errors);
                if let Some(distributed_op) =
                    self.distribute_narrow_op_over_tuple(&NarrowOp::IsSubclass, &right, v.range())
                {
                    self.narrow(ty, &distributed_op.negate(), errors)
                } else if let Some(left) = self.untype_opt(ty.clone(), v.range(), errors)
                    && let Some(right) =
                        self.unwrap_class_object_or_error(&right, v.range(), errors)
                {
                    Type::type_form(self.subtract(&left, &right, errors))
                } else {
                    ty.clone()
                }
            }
            NarrowOp::TypeGuard(t) => t.clone(),
            NarrowOp::NotTypeGuard(_) => ty.clone(),
            NarrowOp::Truthy | NarrowOp::Falsy => self.distribute_over_union(ty, errors, |t| {
                let boolval = matches!(op, NarrowOp::Truthy);
                if t.as_bool() == Some(!boolval) {
                    Type::never()
                } else if matches!(t, Type::ClassType(cls) if *cls == self.stdlib.bool()) {
                    Type::Literal(Lit::Bool(boolval))
                } else {
                    t.clone()
                }
            }),
            NarrowOp::Eq(v) => {
                let right = self.narrow_val_infer(v, errors);
                if matches!(right, Type::Literal(_) | Type::None) {
                    self.intersect(ty, &right, errors)
                } else {
                    ty.clone()
                }
            }
            NarrowOp::NotEq(v) => {
                let right = self.narrow_val_infer(v, errors);
                if matches!(right, Type::Literal(_) | Type::None) {
                    self.distribute_over_union(ty, errors, |t| match (t, &right) {
                        (_, _) if *t == right => Type::never(),
                        (Type::ClassType(cls), Type::Literal(Lit::Bool(b)))
                            if *cls == self.stdlib.bool() =>
                        {
                            Type::Literal(Lit::Bool(!b))
                        }
                        (
                            Type::ClassType(left_cls),
                            Type::Literal(Lit::Enum(box (right_cls, name, _))),
                        ) if *left_cls == *right_cls => {
                            self.subtract_enum_member(left_cls, name, errors)
                        }
                        _ => t.clone(),
                    })
                } else {
                    ty.clone()
                }
            }
            NarrowOp::And(ops) => {
                let mut ops_iter = ops.iter();
                if let Some(first_op) = ops_iter.next() {
                    let mut ret = self.narrow(ty, first_op, errors);
                    for next_op in ops_iter {
                        ret = self.narrow(&ret, next_op, errors);
                    }
                    ret
                } else {
                    ty.clone()
                }
            }
            NarrowOp::Or(ops) => self.unions(ops.map(|op| self.narrow(ty, op, errors)), errors),
            NarrowOp::Call(func, args) | NarrowOp::NotCall(func, args) => {
                if let Some(resolved_op) = self.resolve_narrowing_call(func, args, errors) {
                    if matches!(op, NarrowOp::Call(..)) {
                        self.narrow(ty, &resolved_op, errors)
                    } else {
                        self.narrow(ty, &resolved_op.negate(), errors)
                    }
                } else {
                    ty.clone()
                }
            }
        }
    }
}
