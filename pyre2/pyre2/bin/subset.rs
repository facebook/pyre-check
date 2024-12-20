/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use itertools::izip;

use crate::alt::answers::LookupAnswer;
use crate::solver::Subset;
use crate::types::callable::Param;
use crate::types::callable::Params;
use crate::types::callable::Required;
use crate::types::class::TArgs;
use crate::types::simplify::unions;
use crate::types::tuple::Tuple;
use crate::types::type_var::Variance;
use crate::types::types::TParams;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> Subset<'a, Ans> {
    /// Implementation of subset equality for Type, other than Var.
    pub fn is_subset_eq_impl(&mut self, got: &Type, want: &Type) -> bool {
        match (got, want) {
            (Type::Union(ls), u) => ls.iter().all(|l| self.is_subset_eq(l, u)),
            (l, Type::Intersect(us)) => us.iter().all(|u| self.is_subset_eq(l, u)),
            (l, Type::Union(us)) => us.iter().any(|u| self.is_subset_eq(l, u)),
            (Type::Intersect(ls), u) => ls.iter().any(|l| self.is_subset_eq(l, u)),
            (Type::Callable(l), Type::Callable(u)) => {
                self.is_subset_eq(&l.ret, &u.ret)
                    && match (&l.params, &u.params) {
                        (Params::Ellipsis, _) | (_, Params::Ellipsis) => true,
                        (Params::List(l_args), Params::List(u_args)) => {
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
                                        Some(
                                            Param::PosOnly(_, Required::Optional)
                                            | Param::Pos(_, _, Required::Optional)
                                            | Param::KwOnly(_, _, Required::Optional),
                                        ),
                                        None,
                                    ) => return true,
                                    (Some(Param::VarArg(_)), None) => return true,
                                    (
                                        Some(Param::VarArg(l)),
                                        Some(Param::PosOnly(u, Required::Required)),
                                    ) => {
                                        if self.is_subset_eq(u, l) {
                                            u_arg = u_args_iter.next();
                                        } else {
                                            return false;
                                        }
                                    }
                                    (Some(Param::Kwargs(_)), None) => return true,
                                    _ => return false,
                                }
                            }
                        }
                        (Params::ParamSpec(_), _) | (_, Params::ParamSpec(_)) => {
                            // TODO: need instantiation for param spec
                            false
                        }
                    }
            }
            (Type::ClassType(got), Type::ClassType(want)) => {
                match self.type_order.as_superclass(got, want.class_object()) {
                    Some(got) => self.check_targs(got.targs(), want.targs(), want.tparams()),
                    None => false,
                }
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
            (_, Type::Any(_)) => true,
            (Type::Any(_), _) => true,
            (Type::Never(_), _) => true,
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
