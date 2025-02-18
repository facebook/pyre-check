/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_python_ast::BoolOp;
use ruff_python_ast::CmpOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBinOp;
use ruff_python_ast::ExprSlice;
use ruff_python_ast::ExprStarred;
use ruff_python_ast::Identifier;
use ruff_python_ast::Number;
use ruff_python_ast::Operator;
use ruff_python_ast::UnaryOp;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::UNKNOWN;
use crate::alt::call::CallStyle;
use crate::alt::call::CallTarget;
use crate::alt::callable::CallArg;
use crate::ast::Ast;
use crate::binding::binding::Key;
use crate::binding::binding::KeyYield;
use crate::binding::binding::KeyYieldFrom;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Params;
use crate::types::callable::Required;
use crate::types::class::ClassKind;
use crate::types::literal::Lit;
use crate::types::param_spec::ParamSpec;
use crate::types::special_form::SpecialForm;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::TypeVarArgs;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::types::CalleeKind;
use crate::types::types::Decoration;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;
use crate::visitors::Visitors;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    // Helper method for inferring the type of a boolean operation over a sequence of values.
    fn boolop(&self, values: &[Expr], op: BoolOp, errors: &ErrorCollector) -> Type {
        let target = match op {
            BoolOp::And => false,
            BoolOp::Or => true,
        };
        let should_shortcircuit = |t: &Type| t.as_bool() == Some(target);
        let should_discard = |t: &Type| t.as_bool() == Some(!target);

        let mut types = Vec::new();
        let last_index = values.len() - 1;
        for (i, value) in values.iter().enumerate() {
            let t = self.expr_infer(value, errors);
            if should_shortcircuit(&t) {
                types.push(t);
                break;
            }
            // If we reach the last value, we should always keep it.
            if i != last_index && should_discard(&t) {
                continue;
            }
            match t {
                Type::Union(options) => {
                    for option in options {
                        if !should_discard(&option) {
                            types.push(option);
                        }
                    }
                }
                _ => types.push(t),
            }
        }
        self.unions(types)
    }

    pub fn expr(&self, x: &Expr, check: Option<&Type>, errors: &ErrorCollector) -> Type {
        match check {
            Some(want) if !want.is_any() => {
                let got = self.expr_infer_with_hint(x, Some(want), errors);
                self.check_type(want, &got, x.range(), errors)
            }
            _ => self.expr_infer(x, errors),
        }
    }

    /// Infers types for `if` clauses in the given comprehensions.
    /// This is for error detection only; the types are not used.
    fn ifs_infer(&self, comps: &[Comprehension], errors: &ErrorCollector) {
        for comp in comps.iter() {
            for if_clause in comp.ifs.iter() {
                self.expr_infer(if_clause, errors);
            }
        }
    }

    pub fn attr_infer(
        &self,
        obj: &Type,
        attr_name: &Name,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        self.distribute_over_union(obj, |obj| {
            self.type_of_attr_get(obj.clone(), attr_name, range, errors, "Expr::attr_infer")
        })
    }

    fn binop_infer(&self, x: &ExprBinOp, errors: &ErrorCollector) -> Type {
        let binop_call = |op: Operator, lhs: &Type, rhs: Type, range: TextRange| -> Type {
            // TODO(yangdanny): handle reflected dunder methods
            let method_type = self.attr_infer(lhs, &Name::new(op.dunder()), range, errors);
            let callable =
                self.as_call_target_or_error(method_type, CallStyle::BinaryOp(op), range, errors);
            self.call_infer(callable, &[CallArg::Type(&rhs, range)], &[], range, errors)
        };
        let lhs = self.expr_infer(&x.left, errors);
        let rhs = self.expr_infer(&x.right, errors);
        if let Type::Any(style) = &lhs {
            return style.propagate();
        } else if x.op == Operator::BitOr
            && let Some(l) = self.untype_opt(lhs.clone(), x.left.range())
            && let Some(r) = self.untype_opt(rhs.clone(), x.right.range())
        {
            return Type::type_form(self.union(l, r));
        }
        self.distribute_over_union(&lhs, |lhs| binop_call(x.op, lhs, rhs.clone(), x.range))
    }

    /// When interpreted as static types (as opposed to when accounting for runtime
    /// behavior when used as values), `Type::ClassDef(cls)` is equivalent to
    /// `Type::Type(box Type::ClassType(cls, default_targs(cls)))` where `default_targs(cls)`
    /// is the result of looking up the class `tparams` and synthesizing default `targs` that
    /// are gradual if needed (e.g. `list` is treated as `list[Any]` when used as an annotation).
    ///
    /// This function canonicalizes to `Type::ClassType` or `Type::TypedDict`
    pub fn canonicalize_all_class_types(&self, ty: Type, range: TextRange) -> Type {
        ty.transform(|ty| match ty {
            Type::ClassDef(cls) => {
                *ty = Type::type_form(self.promote(cls, range));
            }
            _ => {}
        })
    }

    fn literal_bool_infer(&self, x: &Expr, errors: &ErrorCollector) -> bool {
        let ty = self.expr_infer(x, errors);
        match ty {
            Type::Literal(Lit::Bool(b)) => b,
            _ => {
                self.error(
                    errors,
                    x.range(),
                    format!("Expected literal True or False, got {ty}"),
                );
                false
            }
        }
    }

    fn tyvar_from_arguments(&self, arguments: &Arguments, errors: &ErrorCollector) -> TypeVar {
        let args = TypeVarArgs::from_arguments(arguments);

        let name = match args.name {
            Some(Expr::StringLiteral(x)) => Identifier::new(Name::new(x.value.to_str()), x.range),
            _ => {
                let msg = if args.name.is_none() {
                    "Missing `name` argument to TypeVar"
                } else {
                    "Expected first argument of TypeVar to be a string literal"
                };
                self.error(errors, arguments.range, msg.to_owned());
                // FIXME: This isn't ideal - we are creating a fake Identifier, which is not good.
                Identifier::new(UNKNOWN, arguments.range)
            }
        };
        let constraints = args.constraints.map(|x| self.expr_untype(x, errors));
        let bound = args.bound.map(|x| self.expr_untype(x, errors));
        let default = args.default.map(|x| self.expr_untype(x, errors));
        let covariant = args
            .covariant
            .is_some_and(|x| self.literal_bool_infer(x, errors));
        let contravariant = args
            .contravariant
            .is_some_and(|x| self.literal_bool_infer(x, errors));
        let infer_variance = args
            .infer_variance
            .is_some_and(|x| self.literal_bool_infer(x, errors));

        for kw in args.unknown {
            self.error(
                errors,
                kw.range,
                match &kw.arg {
                    Some(id) => format!("Unexpected keyword argument `{}` to TypeVar", id.id),
                    None => "Cannot pass unpacked keyword arguments to TypeVar".to_owned(),
                },
            );
        }
        let restriction = if let Some(bound) = bound {
            if !constraints.is_empty() {
                self.error(
                    errors,
                    arguments.range,
                    "TypeVar cannot have both constraints and bound".to_owned(),
                );
            }
            Restriction::Bound(bound)
        } else if !constraints.is_empty() {
            Restriction::Constraints(constraints)
        } else {
            Restriction::Unrestricted
        };
        if [covariant, contravariant, infer_variance]
            .iter()
            .filter(|x| **x)
            .count()
            > 1
        {
            self.error(
                errors,
                arguments.range,
                "Contradictory variance specifications".to_owned(),
            );
        }
        let variance = if covariant {
            Some(Variance::Covariant)
        } else if contravariant {
            Some(Variance::Contravariant)
        } else if infer_variance {
            None
        } else {
            Some(Variance::Invariant)
        };
        TypeVar::new(
            name,
            self.module_info().dupe(),
            restriction,
            default,
            variance,
        )
    }

    pub fn expr_infer(&self, x: &Expr, errors: &ErrorCollector) -> Type {
        self.expr_infer_with_hint(x, None, errors)
    }

    /// Apply a decorator. This effectively synthesizes a function call.
    pub fn apply_decorator(
        &self,
        decorator: Idx<Key>,
        decoratee: Type,
        overload: &mut bool,
        errors: &ErrorCollector,
    ) -> Type {
        if matches!(&decoratee, Type::ClassDef(cls) if cls.has_qname("typing", "TypeVar")) {
            // Avoid recursion in TypeVar, which is decorated with `@final`, whose type signature
            // itself depends on a TypeVar.
            return decoratee;
        }
        let ty_decorator = self.get_idx(decorator);
        match ty_decorator.callee_kind() {
            Some(CalleeKind::Class(ClassKind::StaticMethod)) => {
                return Type::Decoration(Decoration::StaticMethod(Box::new(decoratee)));
            }
            Some(CalleeKind::Class(ClassKind::ClassMethod)) => {
                return Type::Decoration(Decoration::ClassMethod(Box::new(decoratee)));
            }
            Some(CalleeKind::Class(ClassKind::Property)) => {
                return Type::Decoration(Decoration::Property(Box::new((decoratee, None))));
            }
            Some(CalleeKind::Class(ClassKind::EnumMember)) => {
                return Type::Decoration(Decoration::EnumMember(Box::new(decoratee)));
            }
            Some(CalleeKind::Callable(CallableKind::Overload)) => {
                *overload = true;
            }
            _ => {}
        }
        match &*ty_decorator {
            Type::Decoration(Decoration::PropertySetterDecorator(getter)) => {
                return Type::Decoration(Decoration::Property(Box::new((
                    (**getter).clone(),
                    Some(decoratee),
                ))));
            }
            _ => {}
        }
        if matches!(&decoratee, Type::ClassDef(_)) {
            // TODO: don't blanket ignore class decorators.
            return decoratee;
        }
        let range = self.bindings().idx_to_key(decorator).range();
        let call_target = self.as_call_target_or_error(
            ty_decorator.arc_clone(),
            CallStyle::FreeForm,
            range,
            errors,
        );
        let arg = CallArg::Type(&decoratee, range);
        self.call_infer(call_target, &[arg], &[], range, errors)
    }

    fn expr_infer_with_hint(&self, x: &Expr, hint: Option<&Type>, errors: &ErrorCollector) -> Type {
        match x {
            Expr::BoolOp(x) => self.boolop(&x.values, x.op, errors),
            Expr::Named(x) => self.expr_infer_with_hint(&x.value, hint, errors),
            Expr::BinOp(x) => self.binop_infer(x, errors),
            Expr::UnaryOp(x) => {
                let t = self.expr_infer(&x.operand, errors);
                let unop = |t: &Type, f: &dyn Fn(&Lit) -> Type, method: &Name| match t {
                    Type::Literal(lit) => f(lit),
                    Type::ClassType(_) => {
                        self.call_method_or_error(t, method, x.range, &[], &[], errors)
                    }
                    _ => self.error(
                        errors,
                        x.range,
                        format!("Unary {} is not supported on {}", x.op.as_str(), t),
                    ),
                };
                self.distribute_over_union(&t, |t| match x.op {
                    UnaryOp::USub => {
                        let f = |lit: &Lit| {
                            lit.negate(self.stdlib, self.module_info(), x.range, errors)
                        };
                        unop(t, &f, &dunder::NEG)
                    }
                    UnaryOp::UAdd => {
                        let f = |lit: &Lit| lit.clone().to_type();
                        unop(t, &f, &dunder::POS)
                    }
                    UnaryOp::Not => match t.as_bool() {
                        None => self.stdlib.bool().to_type(),
                        Some(b) => Type::Literal(Lit::Bool(!b)),
                    },
                    UnaryOp::Invert => {
                        let f = |lit: &Lit| lit.invert(self.module_info(), x.range, errors);
                        unop(t, &f, &dunder::INVERT)
                    }
                })
            }
            Expr::Lambda(lambda) => {
                let hint_callable = hint.and_then(|ty| {
                    if let Some((_, CallTarget::Callable(c))) = self.as_call_target(ty.clone()) {
                        Some(c)
                    } else {
                        None
                    }
                });
                let parameters: Vec<Param> = if let Some(parameters) = &lambda.parameters {
                    // TODO: use callable hint parameters to check body
                    (**parameters)
                        .iter()
                        .map(|p| {
                            let ty = self
                                .get(&Key::Definition(ShortIdentifier::new(p.name())))
                                .arc_clone();
                            Param::Pos(p.name().clone().id, ty, Required::Required)
                        })
                        .collect()
                } else {
                    vec![]
                };
                if let Some(callable) = hint_callable {
                    let inferred_callable = Type::Callable(
                        Box::new(Callable {
                            params: Params::List(ParamList::new(parameters)),
                            ret: self.expr(&lambda.body, Some(&callable.ret), errors),
                        }),
                        CallableKind::Anon,
                    );
                    let wanted_callable = Type::Callable(Box::new(callable), CallableKind::Anon);
                    self.check_type(&wanted_callable, &inferred_callable, x.range(), errors);
                    wanted_callable
                } else {
                    Type::Callable(
                        Box::new(Callable {
                            params: Params::List(ParamList::new(parameters)),
                            ret: self.expr_infer(&lambda.body, errors),
                        }),
                        CallableKind::Anon,
                    )
                }
            }
            Expr::If(x) => {
                // TODO: Support type refinement
                let condition_type = self.expr_infer(&x.test, errors);
                if let Some(hint_ty) = hint {
                    let mut body_hint = hint;
                    let mut orelse_hint = hint;
                    match condition_type.as_bool() {
                        Some(true) => orelse_hint = None,
                        Some(false) => body_hint = None,
                        None => {}
                    }
                    self.expr(&x.body, body_hint, errors);
                    self.expr(&x.orelse, orelse_hint, errors);
                    hint_ty.clone()
                } else {
                    let body_type = self.expr_infer(&x.body, errors);
                    let orelse_type = self.expr_infer(&x.orelse, errors);
                    match condition_type.as_bool() {
                        Some(true) => body_type,
                        Some(false) => orelse_type,
                        None => self.union(body_type, orelse_type),
                    }
                }
            }
            Expr::Tuple(x) => {
                let ts = match hint {
                    Some(Type::Tuple(Tuple::Concrete(elts))) if elts.len() == x.elts.len() => elts,
                    Some(ty) => match self.decompose_tuple(ty) {
                        Some(elem_ty) => &vec![elem_ty; x.elts.len()],
                        None => &Vec::new(),
                    },
                    None => &Vec::new(),
                };
                Type::tuple(
                    x.elts
                        .iter()
                        .enumerate()
                        .map(|(i, x)| self.expr(x, ts.get(i), errors))
                        .collect(),
                )
            }
            Expr::List(x) => {
                let hint = hint.and_then(|ty| self.decompose_list(ty));
                if let Some(hint) = hint {
                    x.elts.iter().for_each(|x| {
                        self.expr(x, Some(&hint), errors);
                    });
                    self.stdlib.list(hint).to_type()
                } else if x.is_empty() {
                    let elem_ty = self.solver().fresh_contained(self.uniques).to_type();
                    self.stdlib.list(elem_ty).to_type()
                } else {
                    let tys = x
                        .elts
                        .map(|x| self.expr_infer(x, errors).promote_literals(self.stdlib));
                    self.stdlib.list(self.unions(tys)).to_type()
                }
            }
            Expr::Dict(x) => {
                let dict_hint = hint.and_then(|ty| self.decompose_dict(ty));
                let flattened_items = Ast::flatten_dict_items(&x.items);
                if let Some(hint @ Type::TypedDict(box typed_dict)) = hint {
                    self.check_dict_items_against_typed_dict(
                        flattened_items,
                        typed_dict,
                        x.range,
                        errors,
                    );
                    hint.clone()
                } else if let Some(dict_hint) = dict_hint {
                    flattened_items.iter().for_each(|x| match &x.key {
                        Some(key) => {
                            self.expr(key, Some(&dict_hint.key), errors);
                            self.expr(&x.value, Some(&dict_hint.value), errors);
                        }
                        None => {
                            let ty = self.expr_infer_with_hint(&x.value, hint, errors);
                            if let Some((key_ty, value_ty)) = self.unwrap_mapping(&ty) {
                                self.check_type(&key_ty, &dict_hint.key, x.range(), errors);
                                self.check_type(&value_ty, &dict_hint.value, x.range(), errors);
                            } else {
                                self.error(
                                    errors,
                                    x.value.range(),
                                    format!("Expected a mapping, got {}", ty),
                                );
                            }
                        }
                    });
                    dict_hint.to_type(self.stdlib)
                } else if x.is_empty() {
                    let key_ty = self.solver().fresh_contained(self.uniques).to_type();
                    let value_ty = self.solver().fresh_contained(self.uniques).to_type();
                    self.stdlib.dict(key_ty, value_ty).to_type()
                } else {
                    let mut key_tys = Vec::new();
                    let mut value_tys = Vec::new();
                    flattened_items.iter().for_each(|x| match &x.key {
                        Some(key) => {
                            let key_t = self.expr_infer(key, errors).promote_literals(self.stdlib);
                            let value_t = self
                                .expr_infer(&x.value, errors)
                                .promote_literals(self.stdlib);
                            key_tys.push(key_t);
                            value_tys.push(value_t);
                        }
                        None => {
                            let ty = self.expr(&x.value, None, errors);
                            if let Some((key_ty, value_ty)) = self.unwrap_mapping(&ty) {
                                key_tys.push(key_ty);
                                value_tys.push(value_ty);
                            } else {
                                self.error(
                                    errors,
                                    x.value.range(),
                                    format!("Expected a mapping, got {}", ty),
                                );
                            }
                        }
                    });
                    let key_ty = self.unions(key_tys);
                    let value_ty = self.unions(value_tys);
                    self.stdlib.dict(key_ty, value_ty).to_type()
                }
            }
            Expr::Set(x) => {
                let hint = hint.and_then(|ty| self.decompose_set(ty));
                if let Some(hint) = hint {
                    x.elts.iter().for_each(|x| {
                        self.expr(x, Some(&hint), errors);
                    });
                    self.stdlib.set(hint).to_type()
                } else if x.is_empty() {
                    let elem_ty = self.solver().fresh_contained(self.uniques).to_type();
                    self.stdlib.set(elem_ty).to_type()
                } else {
                    let tys = x
                        .elts
                        .map(|x| self.expr_infer(x, errors).promote_literals(self.stdlib));
                    self.stdlib.set(self.unions(tys)).to_type()
                }
            }
            Expr::ListComp(x) => {
                let hint = hint.and_then(|ty| self.decompose_list(ty));
                self.ifs_infer(&x.generators, errors);
                if let Some(hint) = hint {
                    self.expr(&x.elt, Some(&hint), errors);
                    self.stdlib.list(hint).to_type()
                } else {
                    let elem_ty = self
                        .expr_infer(&x.elt, errors)
                        .promote_literals(self.stdlib);
                    self.stdlib.list(elem_ty).to_type()
                }
            }
            Expr::SetComp(x) => {
                let hint = hint.and_then(|ty| self.decompose_set(ty));
                self.ifs_infer(&x.generators, errors);
                if let Some(hint) = hint {
                    self.expr(&x.elt, Some(&hint), errors);
                    self.stdlib.set(hint).to_type()
                } else {
                    let elem_ty = self
                        .expr_infer(&x.elt, errors)
                        .promote_literals(self.stdlib);
                    self.stdlib.set(elem_ty).to_type()
                }
            }
            Expr::DictComp(x) => {
                let hint = hint.and_then(|ty| self.decompose_dict(ty));
                self.ifs_infer(&x.generators, errors);
                if let Some(hint) = hint {
                    self.expr(&x.key, Some(&hint.key), errors);
                    self.expr(&x.value, Some(&hint.value), errors);
                    hint.to_type(self.stdlib)
                } else {
                    let key_ty = self
                        .expr_infer(&x.key, errors)
                        .promote_literals(self.stdlib);
                    let value_ty = self
                        .expr_infer(&x.value, errors)
                        .promote_literals(self.stdlib);
                    self.stdlib.dict(key_ty, value_ty).to_type()
                }
            }
            Expr::Generator(x) => {
                let hint = hint.and_then(|ty| self.decompose_generator(ty));
                self.ifs_infer(&x.generators, errors);
                if let Some((yield_ty, send_ty, return_ty)) = hint {
                    self.expr(&x.elt, Some(&yield_ty), errors);
                    self.check_type(&send_ty, &Type::None, x.range, errors);
                    self.check_type(&Type::None, &return_ty, x.range, errors);
                    self.stdlib
                        .generator(yield_ty, send_ty, return_ty)
                        .to_type()
                } else {
                    let yield_ty = self.expr_infer(&x.elt, errors);
                    self.stdlib
                        .generator(yield_ty, Type::None, Type::None)
                        .to_type()
                }
            }
            Expr::Await(x) => {
                let awaiting_ty = self.expr_infer(&x.value, errors);
                match self.unwrap_awaitable(&awaiting_ty) {
                    Some(ty) => ty,
                    None => self.error(errors, x.range, "Expression is not awaitable".to_owned()),
                }
            }
            Expr::Yield(x) => self.get(&KeyYield(x.range)).send_ty.clone(),
            Expr::YieldFrom(x) => self.get(&KeyYieldFrom(x.range)).return_ty.clone(),
            Expr::Compare(x) => {
                let left = self.expr_infer(&x.left, errors);
                let comparisons = x.ops.iter().zip(x.comparators.iter());
                for (op, comparator) in comparisons {
                    let right = self.expr_infer(comparator, errors);
                    let right_range = comparator.range();
                    let compare_by_method = |ty, method, arg| {
                        self.call_method(ty, &method, x.range, &[arg], &[], errors)
                    };
                    let comparison_error = || {
                        self.error(
                            errors,
                            x.range,
                            format!(
                                "`{}` not supported between `{}` and `{}`",
                                op.as_str(),
                                left,
                                right
                            ),
                        );
                    };
                    match op {
                        CmpOp::Eq | CmpOp::NotEq | CmpOp::Is | CmpOp::IsNot => {
                            // We assume these comparisons never error. Technically, `__eq__` and
                            // `__ne__` can be overridden, but we generally bake in the assumption
                            // that `==` and `!=` check equality as typically defined in Python.
                        }
                        CmpOp::In | CmpOp::NotIn => {
                            // `x in y` desugars to `y.__contains__(x)`
                            if compare_by_method(
                                &right,
                                dunder::CONTAINS,
                                CallArg::Type(&left, x.left.range()),
                            )
                            .is_some()
                            {
                                // Comparison method called. We ignore the return type and assume `bool`.
                            } else {
                                comparison_error();
                            }
                        }
                        _ => {
                            if let Some(magic_method) = dunder::rich_comparison_dunder(*op)
                                && compare_by_method(
                                    &left,
                                    magic_method,
                                    CallArg::Type(&right, right_range),
                                )
                                .is_some()
                            {
                                // Comparison method called. We ignore the return type and assume `bool`.
                            } else {
                                comparison_error();
                            }
                        }
                    }
                }
                self.stdlib.bool().to_type()
            }
            Expr::Call(x) if is_special_name(&x.func, "assert_type") => {
                if x.arguments.args.len() == 2 {
                    let expr_a = &x.arguments.args[0];
                    let expr_b = &x.arguments.args[1];
                    let a = self.expr_infer(expr_a, errors);
                    let b = self.expr_untype(expr_b, errors);
                    let a = self.canonicalize_all_class_types(
                        self.solver().deep_force(a).explicit_any().anon_callables(),
                        expr_a.range(),
                    );
                    let b = self.canonicalize_all_class_types(
                        self.solver().deep_force(b).explicit_any().anon_callables(),
                        expr_b.range(),
                    );
                    if a != b {
                        self.error(
                            errors,
                            x.range,
                            format!(
                                "assert_type({}, {}) failed",
                                a.deterministic_printing(),
                                b.deterministic_printing()
                            ),
                        );
                    }
                } else {
                    self.error(
                        errors,
                        x.range,
                        format!(
                            "assert_type needs 2 arguments, got {:#?}",
                            x.arguments.args.len()
                        ),
                    );
                }
                Type::None
            }
            Expr::Call(x) if is_special_name(&x.func, "reveal_type") => {
                if x.arguments.args.len() == 1 {
                    let t = self
                        .solver()
                        .deep_force(self.expr_infer(&x.arguments.args[0], errors));
                    self.error(
                        errors,
                        x.range,
                        format!("revealed type: {}", t.deterministic_printing()),
                    );
                } else {
                    self.error(
                        errors,
                        x.range,
                        format!(
                            "reveal_type needs 1 argument, got {}",
                            x.arguments.args.len()
                        ),
                    );
                }
                Type::None
            }
            Expr::Call(x) => {
                let ty_fun = self.expr_infer(&x.func, errors);
                if TypeVar::is_ctor(&ty_fun) {
                    Type::type_form(self.tyvar_from_arguments(&x.arguments, errors).to_type())
                } else if TypeVarTuple::is_ctor(&ty_fun)
                    && let Some(name) = arguments_one_string(&x.arguments)
                {
                    Type::type_form(TypeVarTuple::new(name, self.module_info().dupe()).to_type())
                } else if ParamSpec::is_ctor(&ty_fun)
                    && let Some(name) = arguments_one_string(&x.arguments)
                {
                    Type::type_form(ParamSpec::new(name, self.module_info().dupe()).to_type())
                } else {
                    let func_range = x.func.range();
                    let args = x.arguments.args.map(|arg| match arg {
                        Expr::Starred(x) => CallArg::Star(&x.value, x.range),
                        _ => CallArg::Expr(arg),
                    });
                    self.distribute_over_union(&ty_fun, |ty| {
                        let callable = self.as_call_target_or_error(
                            ty.clone(),
                            CallStyle::FreeForm,
                            func_range,
                            errors,
                        );
                        self.call_infer(callable, &args, &x.arguments.keywords, func_range, errors)
                    })
                }
            }
            Expr::FString(x) => {
                // Ensure we detect type errors in f-string expressions.
                Visitors::visit_fstring_expr(x, |x| {
                    self.expr_infer(x, errors);
                });
                match Lit::from_fstring(x) {
                    Some(lit) => lit.to_type(),
                    _ => self.stdlib.str().to_type(),
                }
            }
            Expr::StringLiteral(x) => Lit::from_string_literal(x).to_type(),
            Expr::BytesLiteral(x) => Lit::from_bytes_literal(x).to_type(),
            Expr::NumberLiteral(x) => match &x.value {
                Number::Int(x) => match Lit::from_int(x) {
                    Some(lit) => lit.to_type(),
                    None => self.stdlib.int().to_type(),
                },
                Number::Float(_) => self.stdlib.float().to_type(),
                Number::Complex { .. } => self.stdlib.complex().to_type(),
            },
            Expr::BooleanLiteral(x) => Lit::from_boolean_literal(x).to_type(),
            Expr::NoneLiteral(_) => Type::None,
            Expr::EllipsisLiteral(_) => Type::Ellipsis,
            Expr::Attribute(x) => {
                let obj = self.expr_infer(&x.value, errors);
                match (&obj, x.attr.id.as_str()) {
                    (Type::Literal(Lit::Enum(box (_, member, _))), "_name_" | "name") => {
                        Type::Literal(Lit::String(member.as_str().into()))
                    }
                    (Type::Literal(Lit::Enum(box (_, _, raw_type))), "_value_" | "value") => {
                        raw_type.clone()
                    }
                    _ => self.attr_infer(&obj, &x.attr.id, x.range, errors),
                }
            }
            Expr::Subscript(x) => {
                let xs = Ast::unpack_slice(&x.slice);
                // FIXME: We don't deal properly with hint here, we should.
                let mut fun = self.expr_infer(&x.value, errors);
                if let Type::Var(v) = fun {
                    fun = self.solver().force_var(v);
                }
                if matches!(&fun, Type::ClassDef(t) if t.name() == "tuple") {
                    fun = Type::type_form(Type::SpecialForm(SpecialForm::Tuple));
                }
                match fun {
                    Type::Forall(params, ty) => {
                        let param_map = params
                            .quantified()
                            .zip(xs.map(|x| self.expr_untype(x, errors)))
                            .collect::<SmallMap<_, _>>();
                        ty.subst(&param_map)
                    }
                    // Note that we have to check for `builtins.type` by name here because this code runs
                    // when we're bootstrapping the stdlib and don't have access to class objects yet.
                    Type::ClassDef(cls) if cls.has_qname("builtins", "type") => {
                        let targ = match xs.len() {
                            // This causes us to treat `type[list]` as equivalent to `type[list[Any]]`,
                            // which may or may not be what we want.
                            1 => self.expr_untype(&xs[0], errors),
                            _ => {
                                self.error(
                                    errors,
                                    x.range,
                                    format!(
                                        "Expected 1 type argument for class `type`, got {}",
                                        xs.len()
                                    ),
                                );
                                Type::any_error()
                            }
                        };
                        // TODO: Validate that `targ` refers to a "valid in-scope class or TypeVar"
                        // (https://typing.readthedocs.io/en/latest/spec/annotations.html#type-and-annotation-expressions)
                        Type::type_form(Type::type_form(targ))
                    }
                    Type::ClassDef(cls) => Type::type_form(self.specialize(
                        &cls,
                        xs.map(|x| self.expr_untype(x, errors)),
                        x.range,
                        errors,
                    )),
                    Type::Type(box Type::SpecialForm(special)) => {
                        self.apply_special_form(special, xs, x.range, errors)
                    }
                    Type::Tuple(Tuple::Concrete(elts)) if xs.len() == 1 => {
                        self.infer_tuple_index(elts, &x.slice, x.range, errors)
                    }
                    Type::Tuple(Tuple::Unbounded(elt)) if xs.len() == 1 => self
                        .call_method_or_error(
                            &Type::Tuple(Tuple::Unbounded(elt)),
                            &dunder::GETITEM,
                            x.range,
                            &[CallArg::Expr(&x.slice)],
                            &[],
                            errors,
                        ),
                    Type::Any(style) => style.propagate(),
                    Type::ClassType(cls)
                        if let Some(elts) = self.named_tuple_element_types(&cls) =>
                    {
                        self.infer_tuple_index(elts, &x.slice, x.range, errors)
                    }
                    Type::ClassType(_) => self.call_method_or_error(
                        &fun,
                        &dunder::GETITEM,
                        x.range,
                        &[CallArg::Expr(&x.slice)],
                        &[],
                        errors,
                    ),
                    Type::TypedDict(typed_dict) => match self.expr_infer(&x.slice, errors) {
                        Type::Literal(Lit::String(field_name)) => {
                            if let Some(field) =
                                typed_dict.fields().get(&Name::new(field_name.clone()))
                            {
                                field.ty.clone()
                            } else {
                                self.error(
                                    errors,
                                    x.slice.range(),
                                    format!(
                                        "TypedDict `{}` does not have key `{}`",
                                        typed_dict.name(),
                                        field_name
                                    ),
                                )
                            }
                        }
                        t => self.error(
                            errors,
                            x.slice.range(),
                            format!(
                                "Invalid key for TypedDict `{}`, got `{}`",
                                typed_dict.name(),
                                t.deterministic_printing()
                            ),
                        ),
                    },
                    t => self.error(
                        errors,
                        x.range,
                        format!(
                            "Can't apply arguments to non-class, got {}",
                            t.deterministic_printing()
                        ),
                    ),
                }
            }
            Expr::Starred(ExprStarred { value: box x, .. }) => {
                let ty = self.expr_untype(x, errors);
                Type::Unpack(Box::new(ty))
            }
            Expr::Name(x) => match x.id.as_str() {
                "" => Type::any_error(), // Must already have a parse error
                "Any" => Type::type_form(Type::any_explicit()),
                _ => self
                    .get(&Key::Usage(ShortIdentifier::expr_name(x)))
                    .arc_clone(),
            },
            Expr::Slice(_) => {
                // TODO(stroxler, yangdanny): slices are generic, we should not hard code to int.
                let int = self.stdlib.int().to_type();
                self.stdlib.slice(int.clone(), int.clone(), int).to_type()
            }
            Expr::IpyEscapeCommand(x) => self.error(
                errors,
                x.range,
                "IPython escapes are not supported".to_owned(),
            ),
        }
    }

    /// When indexing/slicing concrete tuples with literals, try to infer a more precise type
    fn infer_tuple_index(
        &self,
        elts: Vec<Type>,
        slice: &Expr,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let xs = Ast::unpack_slice(slice);
        match &xs[0] {
            Expr::Slice(ExprSlice {
                lower: lower_expr,
                upper: upper_expr,
                step: None,
                ..
            }) => {
                let lower_literal = match lower_expr {
                    Some(box expr) => {
                        let lower_type = self.expr_infer(expr, errors);
                        match &lower_type {
                            Type::Literal(Lit::Int(idx)) => Some(*idx),
                            _ => None,
                        }
                    }
                    None => Some(0),
                };
                let upper_literal = match upper_expr {
                    Some(box expr) => {
                        let upper_type = self.expr_infer(expr, errors);
                        match &upper_type {
                            Type::Literal(Lit::Int(idx)) => Some(*idx),
                            _ => None,
                        }
                    }
                    None => Some(elts.len() as i64),
                };
                match (lower_literal, upper_literal) {
                    (Some(lower), Some(upper))
                        if lower <= upper
                            && lower >= 0
                            && upper >= 0
                            && upper <= elts.len() as i64 =>
                    {
                        Type::Tuple(Tuple::concrete(
                            elts[lower as usize..upper as usize].to_vec(),
                        ))
                    }
                    _ => self.todo(errors, "tuple slice", range),
                }
            }
            _ => {
                let idx_type = self.expr_infer(&xs[0], errors);
                match &idx_type {
                    Type::Literal(Lit::Int(idx)) => {
                        let elt_idx = if *idx >= 0 {
                            *idx
                        } else {
                            elts.len() as i64 + *idx
                        } as usize;
                        if let Some(elt) = elts.get(elt_idx) {
                            elt.clone()
                        } else {
                            self.error(
                                errors,
                                range,
                                format!(
                                    "Index {idx} out of range for tuple with {} elements",
                                    elts.len()
                                ),
                            )
                        }
                    }
                    _ => self.call_method_or_error(
                        &Type::Tuple(Tuple::Concrete(elts)),
                        &dunder::GETITEM,
                        range,
                        &[CallArg::Expr(slice)],
                        &[],
                        errors,
                    ),
                }
            }
        }
    }
}

fn is_special_name(x: &Expr, name: &str) -> bool {
    match x {
        Expr::Name(x) => x.id.as_str() == name,
        _ => false,
    }
}

fn arguments_one_string(x: &Arguments) -> Option<Identifier> {
    if x.args.len() == 1 && x.keywords.is_empty() {
        match &x.args[0] {
            Expr::StringLiteral(x) => Some(Identifier {
                id: Name::new(x.value.to_str()),
                range: x.range,
            }),
            _ => None,
        }
    } else {
        None
    }
}
