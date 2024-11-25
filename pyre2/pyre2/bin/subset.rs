/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::solver::Subset;
use crate::types::callable::Arg;
use crate::types::callable::Args;
use crate::types::callable::Required;
use crate::types::class::TArgs;
use crate::types::simplify::unions;
use crate::types::tuple::Tuple;
use crate::types::types::Type;

impl<'a> Subset<'a> {
    /// Implementation of subset equality for Type, other than Var.
    pub fn is_subset_eq_impl(&mut self, got: &Type, want: &Type) -> bool {
        match (got, want) {
            (Type::Union(ls), u) => ls.iter().all(|l| self.is_subset_eq(l, u)),
            (l, Type::Intersect(us)) => us.iter().all(|u| self.is_subset_eq(l, u)),
            (l, Type::Union(us)) => us.iter().any(|u| self.is_subset_eq(l, u)),
            (Type::Intersect(ls), u) => ls.iter().any(|l| self.is_subset_eq(l, u)),
            (Type::Callable(l), Type::Callable(u)) => {
                self.is_subset_eq(&l.ret, &u.ret)
                    && match (&l.args, &u.args) {
                        (Args::Ellipsis, _) | (_, Args::Ellipsis) => true,
                        (Args::List(l_args), Args::List(u_args)) => {
                            let mut l_args_iter = l_args.iter();
                            let mut u_args_iter = u_args.iter();
                            let mut l_arg = l_args_iter.next();
                            let mut u_arg = u_args_iter.next();
                            loop {
                                match (l_arg, u_arg) {
                                    (None, None) => return true,
                                    (
                                        Some(Arg::PosOnly(l, _) | Arg::Pos(_, l, _)),
                                        Some(Arg::PosOnly(u, Required::Required)),
                                    ) => {
                                        if self.is_subset_eq(u, l) {
                                            l_arg = l_args_iter.next();
                                            u_arg = u_args_iter.next();
                                        } else {
                                            return false;
                                        }
                                    }
                                    (Some(Arg::VarArg(_)), None) => return true,
                                    (
                                        Some(Arg::VarArg(l)),
                                        Some(Arg::PosOnly(u, Required::Required)),
                                    ) => {
                                        if self.is_subset_eq(u, l) {
                                            u_arg = u_args_iter.next();
                                        } else {
                                            return false;
                                        }
                                    }
                                    _ => return false,
                                }
                            }
                        }
                        (Args::ParamSpec(_), _) | (_, Args::ParamSpec(_)) => {
                            // TODO: need instantiation for param spec
                            false
                        }
                    }
            }
            (Type::ClassType(got), Type::ClassType(want)) => {
                match self.type_order.as_superclass(got, want.class_object()) {
                    Some(got) => self.is_eq_targs(got.targs(), want.targs()),
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
                let tuple_type = self.type_order.stdlib().tuple(unions(left_elts.clone()));
                self.is_subset_eq(&tuple_type, want)
            }
            (Type::Literal(lit), Type::LiteralString) => lit.is_string(),
            (Type::Literal(lit), t @ Type::ClassType(_)) => {
                self.is_subset_eq(&lit.general_type(self.type_order.stdlib()), t)
            }
            (Type::Literal(l_lit), Type::Literal(u_lit)) => l_lit == u_lit,
            (Type::LiteralString, _) => self.is_subset_eq(&self.type_order.stdlib().str(), want),
            (Type::Type(l), Type::Type(u)) => self.is_subset_eq(l, u),
            (Type::Ellipsis, Type::ClassType(c)) if c.name().id() == "EllipsisType" => {
                // Bit of a weird case - pretty sure we should be modelling these slightly differently
                // - probably not as a dedicated Type alternative.
                true
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

    pub fn is_eq_targs(&mut self, got: &TArgs, want: &TArgs) -> bool {
        for (got_arg, want_arg) in got.as_slice().iter().zip(want.as_slice()) {
            if !self.is_equal(got_arg, want_arg) {
                return false;
            }
        }
        true
    }
}
