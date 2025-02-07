/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::izip;

use crate::alt::answers::LookupAnswer;
use crate::dunder;
use crate::solver::solver::Subset;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Params;
use crate::types::callable::Required;
use crate::types::class::ClassType;
use crate::types::class::TArgs;
use crate::types::simplify::unions;
use crate::types::tuple::Tuple;
use crate::types::type_var::Variance;
use crate::types::types::TParams;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> Subset<'a, Ans> {
    pub fn is_subset_param_list(&mut self, l_args: &[Param], u_args: &[Param]) -> bool {
        let mut l_args_iter = l_args.iter();
        let mut u_args_iter = u_args.iter();
        let mut l_arg = l_args_iter.next();
        let mut u_arg = u_args_iter.next();
        loop {
            match (l_arg, u_arg) {
                (None, None) => return true,
                (
                    Some(Param::PosOnly(l, _) | Param::Pos(_, l, _)),
                    Some(Param::PosOnly(u, Required::Required)),
                ) => {
                    if self.is_subset_eq(u, l) {
                        l_arg = l_args_iter.next();
                        u_arg = u_args_iter.next();
                    } else {
                        return false;
                    }
                }
                (
                    Some(Param::Pos(l_name, l, Required::Required)),
                    Some(Param::Pos(u_name, u, Required::Required)),
                ) if l_name == u_name => {
                    if self.is_subset_eq(u, l) {
                        l_arg = l_args_iter.next();
                        u_arg = u_args_iter.next();
                    } else {
                        return false;
                    }
                }
                (
                    Some(
                        Param::PosOnly(_, Required::Optional)
                        | Param::Pos(_, _, Required::Optional)
                        | Param::KwOnly(_, _, Required::Optional),
                    ),
                    None,
                ) => return true,
                (Some(Param::VarArg(_)), None) => return true,
                (Some(Param::VarArg(l)), Some(Param::PosOnly(u, Required::Required))) => {
                    if self.is_subset_eq(u, l) {
                        u_arg = u_args_iter.next();
                    } else {
                        return false;
                    }
                }
                (Some(Param::Kwargs(_)), None) => return true,
                (Some(Param::VarArg(l)), Some(Param::VarArg(u)))
                | (Some(Param::Kwargs(l)), Some(Param::Kwargs(u))) => {
                    if self.is_subset_eq(u, l) {
                        l_arg = l_args_iter.next();
                        u_arg = u_args_iter.next();
                    } else {
                        return false;
                    }
                }
                _ => return false,
            }
        }
    }

    fn get_call_attr(&mut self, protocol: &ClassType) -> Option<Type> {
        self.type_order
            .try_lookup_attr(protocol.clone().to_type(), &dunder::CALL)
            .and_then(|attr| self.type_order.resolve_as_instance_method(attr))
    }

    pub fn is_subset_protocol(&mut self, got: Type, protocol: ClassType) -> bool {
        let recursive_check = (got.clone(), Type::ClassType(protocol.clone()));
        if !self.recursive_assumptions.insert(recursive_check) {
            // Assume recursive checks are true
            return true;
        }
        let to = self.type_order;
        let protocol_members = to.get_all_member_names(protocol.class_object());
        for name in protocol_members {
            if name == dunder::INIT || name == dunder::NEW {
                // Protocols can't be instantiated
                continue;
            }
            if let Some(got) = to.try_lookup_attr(got.clone(), &name)
                && let Some(want) = to.try_lookup_attr(protocol.clone().to_type(), &name)
                && to.is_attr_subset(&got, &want, &mut |got, want| self.is_subset_eq(got, want))
            {
                continue;
            } else if matches!(got, Type::Callable(_, _))
                && name == dunder::CALL
                && let Some(want) = self.get_call_attr(&protocol)
            {
                if let Type::BoundMethod(box method) = want
                    && let Some(want_no_self) = method.as_callable()
                    && !self.is_subset_eq(&got, &want_no_self)
                {
                    return false;
                }
            } else {
                return false;
            }
        }
        true
    }

    /// Implementation of subset equality for Type, other than Var.
    pub fn is_subset_eq_impl(&mut self, got: &Type, want: &Type) -> bool {
        match (got, want) {
            (_, Type::Any(_)) | (Type::Any(_), _) => true,
            (Type::Never(_), _) => true,
            (_, Type::ClassType(want))
                if *want == *self.type_order.stdlib().object_class_type() =>
            {
                true // everything is an instance of `object`
            }
            (Type::Union(ls), u) => ls.iter().all(|l| self.is_subset_eq(l, u)),
            (l, Type::Intersect(us)) => us.iter().all(|u| self.is_subset_eq(l, u)),
            (l, Type::Union(us)) => us.iter().any(|u| self.is_subset_eq(l, u)),
            (Type::Intersect(ls), u) => ls.iter().any(|l| self.is_subset_eq(l, u)),
            (Type::BoundMethod(box method), Type::Callable(_, _))
                if let Some(l_no_self) = method.as_callable() =>
            {
                self.is_subset_eq_impl(&l_no_self, want)
            }
            (Type::BoundMethod(box l), Type::BoundMethod(box u))
                if let Some(l_no_self) = l.as_callable()
                    && let Some(u_no_self) = u.as_callable() =>
            {
                self.is_subset_eq_impl(&l_no_self, &u_no_self)
            }
            (Type::Callable(l, _), Type::Callable(u, _)) => {
                self.is_subset_eq(&l.ret, &u.ret)
                    && match (&l.params, &u.params) {
                        (Params::Ellipsis, _) | (_, Params::Ellipsis) => true,
                        (Params::List(l_args), Params::List(u_args)) => {
                            self.is_subset_param_list(l_args.items(), u_args.items())
                        }
                        (Params::List(ls), Params::ParamSpec(args, Type::Var(v))) => {
                            match self.lookup_param_spec_var(*v) {
                                Some(v) => match v {
                                    Type::ParamSpecValue(value) => self.is_subset_param_list(
                                        ls.items(),
                                        value.prepend_types(args).items(),
                                    ),
                                    _ => false,
                                },
                                None => {
                                    let args = ParamList::new_types(args);
                                    if ls.len() < args.len() {
                                        return false;
                                    }
                                    let (pre, post) = ls.items().split_at(args.len());
                                    if !self.is_subset_param_list(pre, args.items()) {
                                        return false;
                                    }
                                    self.set_param_spec_var(*v, ParamList::new(post.to_vec()));
                                    true
                                }
                            }
                        }
                        (Params::List(ls), Params::ParamSpec(args, Type::Quantified(q))) => {
                            // Must be: ls = args + [Args[q], KWargs[q]]
                            let args = ParamList::new_types(args);
                            let args = args
                                .items()
                                .iter()
                                .cloned()
                                .chain(vec![
                                    Param::VarArg(Type::Args(*q)),
                                    Param::Kwargs(Type::Kwargs(*q)),
                                ])
                                .collect::<Vec<_>>();
                            self.is_subset_param_list(ls.items(), &args)
                        }
                        (Params::ParamSpec(ls, p1), Params::ParamSpec(us, p2)) if p1 == p2 => {
                            if ls.len() != us.len() {
                                return false;
                            }
                            for (l, u) in ls.iter().zip(us.iter()) {
                                if !self.is_subset_eq(u, l) {
                                    return false;
                                }
                            }
                            true
                        }
                        (Params::ParamSpec(_, _), _) | (_, Params::ParamSpec(_, _)) => {
                            // TODO: need instantiation for param spec
                            false
                        }
                    }
            }
            (Type::TypedDict(got), Type::TypedDict(want)) => {
                // For each key in `want`, `got` has the corresponding key
                // and the corresponding value type in `got` is consistent with the value type in `want`.
                // For each required key in `got`, the corresponding key is required in `want`.
                // For each non-required key in `got`, the corresponding key is not required in `want`.
                want.fields().iter().all(|(k, want_v)| {
                    got.fields()
                        .get(k)
                        .map_or(false, |got_v| self.is_subset_eq(&got_v.ty, &want_v.ty))
                }) && got.fields().iter().all(|(k, got_v)| {
                    want.fields()
                        .get(k)
                        .map_or(true, |want_v| got_v.required == want_v.required)
                })
            }
            (Type::TypedDict(_), _) => {
                let stdlib = self.type_order.stdlib();
                self.is_subset_eq(
                    &stdlib
                        .mapping(
                            stdlib.str().to_type(),
                            stdlib.object_class_type().clone().to_type(),
                        )
                        .to_type(),
                    want,
                )
            }
            (Type::ClassType(got), Type::ClassType(want))
                if *want == self.type_order.stdlib().float()
                    && *got == self.type_order.stdlib().int() =>
            {
                true
            }
            (Type::ClassType(got), Type::ClassType(want))
                if *want == self.type_order.stdlib().complex()
                    && (*got == self.type_order.stdlib().int()
                        || *got == self.type_order.stdlib().float()) =>
            {
                true
            }
            (Type::ClassType(got), Type::ClassType(want)) => {
                let got_is_protocol = self.type_order.is_protocol(got.class_object());
                let want_is_protocol = self.type_order.is_protocol(want.class_object());
                if got_is_protocol && !want_is_protocol {
                    // Protocols are never assignable to concrete types
                    return false;
                }
                match self.type_order.as_superclass(got, want.class_object()) {
                    Some(got) => self.check_targs(got.targs(), want.targs(), want.tparams()),
                    // Structural checking for assigning to protocols
                    None if want_is_protocol => {
                        self.is_subset_protocol(got.clone().to_type(), want.clone())
                    }
                    _ => false,
                }
            }
            (_, Type::ClassType(want)) if self.type_order.is_protocol(want.class_object()) => {
                self.is_subset_protocol(got.clone(), want.clone())
            }
            (Type::ClassType(got), Type::BoundMethod(_) | Type::Callable(_, _))
                if self.type_order.is_protocol(got.class_object())
                    && let Some(call_ty) = self.get_call_attr(got) =>
            {
                self.is_subset_eq(&call_ty, want)
            }
            (Type::ClassDef(got), Type::ClassDef(want)) => {
                self.type_order.has_superclass(got, want)
            }
            (Type::ClassDef(got), Type::Type(box Type::ClassType(want))) => {
                self.type_order.has_superclass(got, want.class_object())
            }
            (Type::Type(box Type::ClassType(got)), Type::ClassDef(want)) => {
                self.type_order.has_superclass(got.class_object(), want)
            }
            (Type::ClassDef(got), Type::ClassType(want)) => {
                self.type_order.has_metaclass(got, want)
            }
            (Type::Type(box Type::ClassType(got)), Type::ClassType(want)) => {
                self.type_order.has_metaclass(got.class_object(), want)
            }
            (Type::ClassDef(_), Type::Type(box Type::Any(_)))
            | (Type::Type(box Type::Any(_)), Type::ClassDef(_)) => true,
            (Type::Tuple(Tuple::Concrete(lelts)), Type::Tuple(Tuple::Concrete(uelts))) => {
                if lelts.len() == uelts.len() {
                    lelts
                        .iter()
                        .zip(uelts)
                        .all(|(l, u)| self.is_subset_eq(l, u))
                } else {
                    false
                }
            }
            (Type::Tuple(Tuple::Unbounded(box Type::Any(_))), Type::Tuple(_)) => true,
            (Type::Tuple(Tuple::Concrete(lelts)), Type::Tuple(Tuple::Unbounded(box u))) => {
                lelts.iter().all(|l| self.is_subset_eq(l, u))
            }
            (Type::Tuple(Tuple::Unbounded(box l)), Type::Tuple(Tuple::Unbounded(box u))) => {
                self.is_subset_eq(l, u)
            }
            (Type::Tuple(Tuple::Concrete(left_elts)), _) => {
                let tuple_type = self
                    .type_order
                    .stdlib()
                    .tuple(unions(left_elts.clone()))
                    .to_type();
                self.is_subset_eq(&tuple_type, want)
            }
            (Type::Tuple(Tuple::Unbounded(box left_elt)), _) => {
                let tuple_type = self.type_order.stdlib().tuple(left_elt.clone()).to_type();
                self.is_subset_eq(&tuple_type, want)
            }
            (Type::Literal(lit), Type::LiteralString) => lit.is_string(),
            (Type::Literal(lit), t @ Type::ClassType(_)) => self.is_subset_eq(
                &lit.general_class_type(self.type_order.stdlib()).to_type(),
                t,
            ),
            (Type::Literal(l_lit), Type::Literal(u_lit)) => l_lit == u_lit,
            (Type::LiteralString, _) => {
                self.is_subset_eq(&self.type_order.stdlib().str().to_type(), want)
            }
            (Type::Type(l), Type::Type(u)) => self.is_subset_eq(l, u),
            (Type::Type(_), _) => {
                self.is_subset_eq(&self.type_order.stdlib().builtins_type().to_type(), want)
            }
            (Type::TypeGuard(l), Type::TypeGuard(u)) => {
                // TypeGuard is covariant
                self.is_subset_eq(l, u)
            }
            (Type::TypeGuard(_) | Type::TypeIs(_), _) => {
                self.is_subset_eq(&self.type_order.stdlib().bool().to_type(), want)
            }
            (Type::Ellipsis, _) => {
                // Bit of a weird case - pretty sure we should be modelling these slightly differently
                // - probably not as a dedicated Type alternative.
                self.is_subset_eq(&self.type_order.stdlib().ellipsis_type().to_type(), want)
            }
            (Type::None, _) => {
                self.is_subset_eq(&self.type_order.stdlib().none_type().to_type(), want)
            }
            (Type::Forall(_, _), _) => {
                // FIXME: Probably need to do some kind of substitution here
                false
            }
            (Type::TypeAlias(ta), _) => {
                self.is_subset_eq_impl(&ta.as_value(self.type_order.stdlib()), want)
            }
            _ => false,
        }
    }

    fn check_targs(&mut self, got: &TArgs, want: &TArgs, params: &TParams) -> bool {
        let got = got.as_slice();
        let want = want.as_slice();
        assert_eq!(got.len(), want.len());
        assert_eq!(want.len(), params.len());
        for (got_arg, want_arg, param) in izip!(got, want, params.iter()) {
            let result = match param.variance {
                Variance::Covariant => self.is_subset_eq(got_arg, want_arg),
                Variance::Contravariant => self.is_subset_eq(want_arg, got_arg),
                Variance::Invariant => self.is_equal(got_arg, want_arg),
            };
            if !result {
                return false;
            }
        }
        true
    }
}
