/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::BoolOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprSlice;
use ruff_python_ast::ExprStarred;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;
use ruff_python_ast::Number;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::call::CallStyle;
use crate::alt::callable::CallArg;
use crate::alt::solve::TypeFormContext;
use crate::binding::binding::Key;
use crate::binding::binding::KeyYield;
use crate::binding::binding::KeyYieldFrom;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::error::context::ErrorContext;
use crate::error::context::TypeCheckContext;
use crate::error::kind::ErrorKind;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::ruff::ast::Ast;
use crate::types::callable::Callable;
use crate::types::callable::FunctionKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Params;
use crate::types::callable::Required;
use crate::types::lit_int::LitInt;
use crate::types::literal::Lit;
use crate::types::param_spec::ParamSpec;
use crate::types::quantified::QuantifiedKind;
use crate::types::special_form::SpecialForm;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::types::AnyStyle;
use crate::types::types::CalleeKind;
use crate::types::types::Type;
use crate::types::types::TypeInfo;
use crate::util::prelude::SliceExt;
use crate::util::prelude::VecExt;
use crate::util::visit::Visit;

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
            for t in t.into_unions() {
                // If we reach the last value, we should always keep it.
                if i == last_index || !should_discard(&t) {
                    if i != last_index && t == self.stdlib.bool().to_type() {
                        types.push(Lit::Bool(target).to_type());
                    } else if i != last_index && t == self.stdlib.int().to_type() && !target {
                        types.push(Lit::Int(LitInt::new(0)).to_type());
                    } else if i != last_index && t == self.stdlib.str().to_type() && !target {
                        types.push(Lit::String(String::new().into_boxed_str()).to_type());
                    } else {
                        types.push(t);
                    }
                }
            }
        }
        self.unions(types)
    }

    pub fn expr(
        &self,
        x: &Expr,
        check: Option<(&Type, &dyn Fn() -> TypeCheckContext)>,
        errors: &ErrorCollector,
    ) -> Type {
        self.expr_type_info(x, check, errors).into_ty()
    }

    pub fn expr_type_info(
        &self,
        x: &Expr,
        check: Option<(&Type, &dyn Fn() -> TypeCheckContext)>,
        errors: &ErrorCollector,
    ) -> TypeInfo {
        self.expr_type_info_with_separate_check_errors(
            x,
            check.map(|(ty, tcc)| (ty, tcc, errors)),
            errors,
        )
    }

    pub fn expr_with_separate_check_errors(
        &self,
        x: &Expr,
        check: Option<(&Type, &dyn Fn() -> TypeCheckContext, &ErrorCollector)>,
        errors: &ErrorCollector,
    ) -> Type {
        self.expr_type_info_with_separate_check_errors(x, check, errors)
            .into_ty()
    }

    fn expr_type_info_with_separate_check_errors(
        &self,
        x: &Expr,
        check: Option<(&Type, &dyn Fn() -> TypeCheckContext, &ErrorCollector)>,
        errors: &ErrorCollector,
    ) -> TypeInfo {
        match check {
            Some((want, tcc, check_errors)) if !want.is_any() => {
                let got = self.expr_infer_type_info_with_hint(x, Some(want), errors);
                self.check_and_return_type_info(want, got, x.range(), check_errors, tcc)
            }
            _ => self.expr_infer_type_info(x, errors),
        }
    }

    /// Infers types for `if` clauses in the given comprehensions.
    /// This is for error detection only; the types are not used.
    fn ifs_infer(&self, comps: &[Comprehension], errors: &ErrorCollector) {
        for comp in comps {
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
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        self.distribute_over_union(obj, |obj| {
            self.type_of_attr_get(obj, attr_name, range, errors, context, "Expr::attr_infer")
        })
    }

    /// When interpreted as static types (as opposed to when accounting for runtime
    /// behavior when used as values), `Type::ClassDef(cls)` is equivalent to
    /// `Type::Type(box Type::ClassType(cls, default_targs(cls)))` where `default_targs(cls)`
    /// is the result of looking up the class `tparams` and synthesizing default `targs` that
    /// are gradual if needed (e.g. `list` is treated as `list[Any]` when used as an annotation).
    ///
    /// This function canonicalizes to `Type::ClassType` or `Type::TypedDict`
    pub fn canonicalize_all_class_types(&self, ty: Type, range: TextRange) -> Type {
        ty.transform(&mut |ty| match ty {
            Type::SpecialForm(SpecialForm::Tuple) => {
                *ty = Type::Tuple(Tuple::unbounded(Type::Any(AnyStyle::Implicit)));
            }
            Type::SpecialForm(SpecialForm::Callable) => {
                *ty = Type::callable_ellipsis(Type::Any(AnyStyle::Implicit))
            }
            Type::ClassDef(cls) => {
                if cls.has_qname("builtins", "tuple") {
                    *ty = Type::type_form(Type::Tuple(Tuple::unbounded(Type::Any(
                        AnyStyle::Implicit,
                    ))));
                } else {
                    *ty = Type::type_form(self.promote(cls, range));
                }
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
                    ErrorKind::InvalidLiteral,
                    None,
                    format!("Expected literal True or False, got {ty}"),
                );
                false
            }
        }
    }

    pub fn typevar_from_call(
        &self,
        name: Identifier,
        x: &ExprCall,
        errors: &ErrorCollector,
    ) -> TypeVar {
        let mut arg_name = false;
        let mut restriction = None;
        let mut default = None;
        let mut variance = None;

        let check_name_arg = |arg: &Expr| {
            if let Expr::StringLiteral(lit) = arg {
                if lit.value.to_str() != name.id.as_str() {
                    self.error(
                        errors,
                        x.range,
                        ErrorKind::InvalidTypeVar,
                        None,
                        format!(
                            "TypeVar must be assigned to a variable named `{}`",
                            lit.value.to_str()
                        ),
                    );
                }
            } else {
                self.error(
                    errors,
                    arg.range(),
                    ErrorKind::InvalidTypeVar,
                    None,
                    "Expected first argument of TypeVar to be a string literal".to_owned(),
                );
            }
        };

        let mut try_set_variance = |kw: &Keyword, v: Option<Variance>| {
            if self.literal_bool_infer(&kw.value, errors) {
                if variance.is_some() {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidTypeVar,
                        None,
                        "Contradictory variance specifications".to_owned(),
                    );
                } else {
                    variance = Some(v);
                }
            }
        };

        let mut iargs = x.arguments.args.iter();
        if let Some(arg) = iargs.next() {
            check_name_arg(arg);
            arg_name = true;
        }

        let constraints = iargs
            .map(|arg| self.expr_untype(arg, TypeFormContext::TypeVarConstraint, errors))
            .collect::<Vec<_>>();
        if !constraints.is_empty() {
            restriction = Some(Restriction::Constraints(constraints));
        }

        for kw in &x.arguments.keywords {
            match &kw.arg {
                Some(id) => match id.id.as_str() {
                    "bound" => {
                        let bound =
                            self.expr_untype(&kw.value, TypeFormContext::TypeVarConstraint, errors);
                        if restriction.is_some() {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidTypeVar,
                                None,
                                "TypeVar cannot have both constraints and bound".to_owned(),
                            );
                        } else {
                            restriction = Some(Restriction::Bound(bound));
                        }
                    }
                    "default" => {
                        default = Some((
                            self.expr_untype(&kw.value, TypeFormContext::TypeVarDefault, errors),
                            kw.value.range(),
                        ))
                    }
                    "covariant" => try_set_variance(kw, Some(Variance::Covariant)),
                    "contravariant" => try_set_variance(kw, Some(Variance::Contravariant)),
                    "infer_variance" => try_set_variance(kw, None),
                    "name" => {
                        if arg_name {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidTypeVar,
                                None,
                                "Multiple values for argument `name`".to_owned(),
                            );
                        } else {
                            check_name_arg(&kw.value);
                            arg_name = true;
                        }
                    }
                    _ => {
                        self.error(
                            errors,
                            kw.range,
                            ErrorKind::InvalidTypeVar,
                            None,
                            format!("Unexpected keyword argument `{}` to TypeVar", id.id),
                        );
                    }
                },
                _ => {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidTypeVar,
                        None,
                        "Cannot pass unpacked keyword arguments to TypeVar".to_owned(),
                    );
                }
            }
        }

        if !arg_name {
            self.error(
                errors,
                x.range,
                ErrorKind::InvalidTypeVar,
                None,
                "Missing `name` argument".to_owned(),
            );
        }
        let restriction = restriction.unwrap_or(Restriction::Unrestricted);
        let mut default_value = None;
        if let Some((default_ty, default_range)) = default {
            default_value = Some(self.validate_type_var_default(
                &name.id,
                QuantifiedKind::TypeVar,
                &default_ty,
                default_range,
                &restriction,
                errors,
            ));
        }
        TypeVar::new(
            name,
            self.module_info().dupe(),
            restriction,
            default_value,
            variance.unwrap_or(Some(Variance::Invariant)),
        )
    }

    pub fn paramspec_from_call(
        &self,
        name: Identifier,
        x: &ExprCall,
        errors: &ErrorCollector,
    ) -> ParamSpec {
        // TODO: check and complain on extra args, keywords
        let mut arg_name = false;

        let check_name_arg = |arg: &Expr| {
            if let Expr::StringLiteral(lit) = arg {
                if lit.value.to_str() != name.id.as_str() {
                    self.error(
                        errors,
                        x.range,
                        ErrorKind::InvalidParamSpec,
                        None,
                        format!(
                            "ParamSpec must be assigned to a variable named `{}`",
                            lit.value.to_str()
                        ),
                    );
                }
            } else {
                self.error(
                    errors,
                    arg.range(),
                    ErrorKind::InvalidParamSpec,
                    None,
                    "Expected first argument of ParamSpec to be a string literal".to_owned(),
                );
            }
        };

        if let Some(arg) = x.arguments.args.first() {
            check_name_arg(arg);
            arg_name = true;
        }
        let mut default = None;
        for kw in &x.arguments.keywords {
            match &kw.arg {
                Some(id) => match id.id.as_str() {
                    "name" => {
                        if arg_name {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidParamSpec,
                                None,
                                "Multiple values for argument `name`".to_owned(),
                            );
                        } else {
                            check_name_arg(&kw.value);
                            arg_name = true;
                        }
                    }
                    "default" => {
                        default = Some((
                            self.expr_untype(&kw.value, TypeFormContext::ParamSpecDefault, errors),
                            kw.range(),
                        ));
                    }
                    _ => {
                        self.error(
                            errors,
                            kw.range,
                            ErrorKind::InvalidParamSpec,
                            None,
                            format!("Unexpected keyword argument `{}` to ParamSpec", id.id),
                        );
                    }
                },
                _ => {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidParamSpec,
                        None,
                        "Cannot pass unpacked keyword arguments to ParamSpec".to_owned(),
                    );
                }
            }
        }

        if !arg_name {
            self.error(
                errors,
                x.range,
                ErrorKind::InvalidParamSpec,
                None,
                "Missing `name` argument".to_owned(),
            );
        }
        let mut default_value = None;
        if let Some((default_ty, default_range)) = default {
            default_value = Some(self.validate_type_var_default(
                &name.id,
                QuantifiedKind::ParamSpec,
                &default_ty,
                default_range,
                &Restriction::Unrestricted,
                errors,
            ));
        }
        ParamSpec::new(name, self.module_info().dupe(), default_value)
    }

    pub fn typevartuple_from_call(
        &self,
        name: Identifier,
        x: &ExprCall,
        errors: &ErrorCollector,
    ) -> TypeVarTuple {
        let mut arg_name = false;
        let check_name_arg = |arg: &Expr| {
            if let Expr::StringLiteral(lit) = arg {
                if lit.value.to_str() != name.id.as_str() {
                    self.error(
                        errors,
                        x.range,
                        ErrorKind::InvalidTypeVarTuple,
                        None,
                        format!(
                            "TypeVarTuple must be assigned to a variable named `{}`",
                            lit.value.to_str()
                        ),
                    );
                }
            } else {
                self.error(
                    errors,
                    arg.range(),
                    ErrorKind::InvalidTypeVarTuple,
                    None,
                    "Expected first argument of TypeVarTuple to be a string literal".to_owned(),
                );
            }
        };
        if let Some(arg) = x.arguments.args.first() {
            check_name_arg(arg);
            arg_name = true;
        }
        if let Some(arg) = x.arguments.args.get(1) {
            self.error(
                errors,
                arg.range(),
                ErrorKind::InvalidTypeVarTuple,
                None,
                "Unexpected positional argument to TypeVarTuple".to_owned(),
            );
        }
        let mut default = None;
        for kw in &x.arguments.keywords {
            match &kw.arg {
                Some(id) => match id.id.as_str() {
                    "name" => {
                        if arg_name {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidTypeVarTuple,
                                None,
                                "Multiple values for argument `name`".to_owned(),
                            );
                        } else {
                            check_name_arg(&kw.value);
                            arg_name = true;
                        }
                    }
                    "default" => {
                        default = Some((
                            self.expr_untype(
                                &kw.value,
                                TypeFormContext::TypeVarTupleDefault,
                                errors,
                            ),
                            kw.range(),
                        ));
                    }
                    _ => {
                        self.error(
                            errors,
                            kw.range,
                            ErrorKind::InvalidTypeVarTuple,
                            None,
                            format!("Unexpected keyword argument `{}` to TypeVarTuple", id.id),
                        );
                    }
                },
                _ => {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidTypeVarTuple,
                        None,
                        "Cannot pass unpacked keyword arguments to TypeVarTuple".to_owned(),
                    );
                }
            }
        }
        if !arg_name {
            self.error(
                errors,
                x.range,
                ErrorKind::InvalidTypeVarTuple,
                None,
                "Missing `name` argument".to_owned(),
            );
        }
        let mut default_value = None;
        if let Some((default_ty, default_range)) = default {
            default_value = Some(self.validate_type_var_default(
                &name.id,
                QuantifiedKind::TypeVarTuple,
                &default_ty,
                default_range,
                &Restriction::Unrestricted,
                errors,
            ));
        }
        TypeVarTuple::new(name, self.module_info().dupe(), default_value)
    }

    pub fn expr_infer(&self, x: &Expr, errors: &ErrorCollector) -> Type {
        self.expr_infer_type_info(x, errors).into_ty()
    }

    fn expr_infer_type_info(&self, x: &Expr, errors: &ErrorCollector) -> TypeInfo {
        self.expr_infer_type_info_with_hint(x, None, errors)
    }

    pub fn check_isinstance(&self, ty_fun: &Type, x: &ExprCall, errors: &ErrorCollector) {
        if let Some(CalleeKind::Function(FunctionKind::IsInstance)) = ty_fun.callee_kind() {
            if x.arguments.args.len() == 2 {
                let is_instance_class_type = self.expr_infer(&x.arguments.args[1], errors);
                if let Type::ClassDef(cls) = is_instance_class_type {
                    let metadata = self.get_metadata_for_class(&cls);
                    if metadata.is_new_type() {
                        self.error(
                            errors,
                            x.range,
                            ErrorKind::InvalidArgument,
                            None,
                            format!("NewType `{}` not allowed in isinstance", cls.name()),
                        );
                    }
                }
            }
        }
    }

    /// Apply a decorator. This effectively synthesizes a function call.
    pub fn apply_decorator(
        &self,
        decorator: Idx<Key>,
        decoratee: Type,
        errors: &ErrorCollector,
    ) -> Type {
        if matches!(&decoratee, Type::ClassDef(cls) if cls.has_qname("typing", "TypeVar")) {
            // Avoid recursion in TypeVar, which is decorated with `@final`, whose type signature
            // itself depends on a TypeVar.
            return decoratee;
        }
        let ty_decorator = self.get_idx(decorator).arc_clone_ty();
        if matches!(&decoratee, Type::ClassDef(_)) {
            // TODO: don't blanket ignore class decorators.
            return decoratee;
        }
        let range = self.bindings().idx_to_key(decorator).range();
        let call_target =
            self.as_call_target_or_error(ty_decorator, CallStyle::FreeForm, range, errors, None);
        let arg = CallArg::Type(&decoratee, range);
        self.call_infer(call_target, &[arg], &[], range, errors, None)
    }

    /// Helper to infer element types for a list or set.
    fn elts_infer(
        &self,
        elts: &[Expr],
        elt_hint: Option<Type>,
        errors: &ErrorCollector,
    ) -> Vec<Type> {
        elts.map(|x| match x {
            Expr::Starred(ExprStarred { box value, .. }) => {
                let hint = elt_hint
                    .as_ref()
                    .map(|ty| self.stdlib.iterable(ty.clone()).to_type());
                let unpacked_ty = self.expr_infer_with_hint_promote(value, hint.as_ref(), errors);
                if let Some(iterable_ty) = self.unwrap_iterable(&unpacked_ty) {
                    iterable_ty
                } else {
                    self.error(
                        errors,
                        x.range(),
                        ErrorKind::NotIterable,
                        None,
                        format!("Expected an iterable, got {}", unpacked_ty),
                    )
                }
            }
            _ => self.expr_infer_with_hint_promote(x, elt_hint.as_ref(), errors),
        })
    }

    fn expr_infer_type_info_with_hint(
        &self,
        x: &Expr,
        hint: Option<&Type>,
        errors: &ErrorCollector,
    ) -> TypeInfo {
        let res = match x {
            Expr::Name(x) => {
                let ty = match x.id.as_str() {
                    "" => Type::any_error(), // Must already have a parse error
                    _ => self
                        .get(&Key::Usage(ShortIdentifier::expr_name(x)))
                        .arc_clone_ty(),
                };
                TypeInfo::of_ty(ty)
            }
            Expr::Attribute(x) => {
                let obj = self.expr_infer(&x.value, errors);
                let ty = match (&obj, x.attr.id.as_str()) {
                    (Type::Literal(Lit::Enum(box (_, member, _))), "_name_" | "name") => {
                        Type::Literal(Lit::String(member.as_str().into()))
                    }
                    (Type::Literal(Lit::Enum(box (_, _, raw_type))), "_value_" | "value") => {
                        raw_type.clone()
                    }
                    _ => self.attr_infer(&obj, &x.attr.id, x.range, errors, None),
                };
                TypeInfo::of_ty(ty)
            }
            Expr::Named(x) => self.expr_infer_type_info_with_hint(&x.value, hint, errors),
            // All other expressions operate at the `Type` level only, so we avoid the overhead of
            // wrapping and unwrapping `TypeInfo` by computing the result as a `Type` and only wrapping
            // at the end.
            _ => TypeInfo::of_ty(self.expr_infer_type_no_trace(x, hint, errors)),
        };
        self.record_type_trace(x.range(), res.ty());
        res
    }

    /// This function should not be used directly: we want every expression to record a type trace,
    /// and that is handled in expr_infer_type_info_with_hint. This function should *only* be called
    /// via expr_infer_type_info_with_hint.
    fn expr_infer_type_no_trace(
        &self,
        x: &Expr,
        hint: Option<&Type>,
        errors: &ErrorCollector,
    ) -> Type {
        match x {
            Expr::Name(..) | Expr::Attribute(..) | Expr::Named(..) => {
                // These cases are required to preserve attribute narrowing information. But anyone calling
                // this function only needs the Type, so we can just pull it out.
                self.expr_infer_type_info_with_hint(x, hint, errors)
                    .into_ty()
            }
            Expr::If(x) => {
                // TODO: Support type narrowing
                let condition_type = self.expr_infer(&x.test, errors);
                let body_type = self.expr_infer_type_no_trace(&x.body, hint, errors);
                let orelse_type = self.expr_infer_type_no_trace(&x.orelse, hint, errors);
                match condition_type.as_bool() {
                    Some(true) => body_type,
                    Some(false) => orelse_type,
                    None => self.union(body_type, orelse_type),
                }
            }
            Expr::BoolOp(x) => self.boolop(&x.values, x.op, errors),
            Expr::BinOp(x) => self.binop_infer(x, errors),
            Expr::UnaryOp(x) => self.unop_infer(x, errors),
            Expr::Lambda(lambda) => {
                let mut param_vars = Vec::new();
                if let Some(parameters) = &lambda.parameters {
                    param_vars.reserve(parameters.len());
                    for x in parameters {
                        param_vars.push((&x.name().id, self.bindings().get_lambda_param(x.name())));
                    }
                }
                let return_hint = hint.and_then(|ty| self.decompose_lambda(ty, &param_vars));
                let params = param_vars.into_map(|(name, var)| {
                    Param::Pos(
                        name.clone(),
                        self.solver().force_var(var),
                        Required::Required,
                    )
                });
                let params = Params::List(ParamList::new(params));
                let ret = self.expr_infer_type_no_trace(&lambda.body, return_hint.as_ref(), errors);
                Type::Callable(Box::new(Callable { params, ret }))
            }
            Expr::Tuple(x) => {
                // These hints could be more precise
                let hint_ts = match hint {
                    Some(Type::Tuple(Tuple::Concrete(elts))) => elts,
                    Some(Type::Tuple(Tuple::Unpacked(box (prefix, _, _)))) => prefix,
                    _ => &Vec::new(),
                };
                let default_hint = match hint {
                    Some(Type::Tuple(Tuple::Unbounded(box elt))) => Some(elt),
                    _ => None,
                };
                let mut prefix = Vec::new();
                let mut unbounded = Vec::new();
                let mut suffix = Vec::new();
                let mut hint_ts_idx: usize = 0;
                let mut encountered_invalid_star = false;
                for elt in x.elts.iter() {
                    match elt {
                        Expr::Starred(ExprStarred { box value, .. }) => {
                            let ty = self.expr_infer(value, errors);
                            match ty {
                                Type::Tuple(Tuple::Concrete(elts)) => {
                                    if unbounded.is_empty() {
                                        hint_ts_idx = hint_ts_idx.saturating_add(elts.len());
                                        prefix.extend(elts);
                                    } else {
                                        suffix.extend(elts)
                                    }
                                }
                                Type::Tuple(Tuple::Unpacked(box (pre, middle, suff)))
                                    if unbounded.is_empty() =>
                                {
                                    prefix.extend(pre);
                                    suffix.extend(suff);
                                    unbounded.push(middle);
                                    hint_ts_idx = usize::MAX;
                                }
                                _ => {
                                    if let Some(iterable_ty) = self.unwrap_iterable(&ty) {
                                        if !unbounded.is_empty() {
                                            unbounded.push(Type::Tuple(Tuple::unbounded(
                                                self.unions(suffix),
                                            )));
                                            suffix = Vec::new();
                                        }
                                        unbounded.push(Type::Tuple(Tuple::unbounded(iterable_ty)));
                                        hint_ts_idx = usize::MAX;
                                    } else {
                                        self.error(
                                            errors,
                                            x.range(),
                                            ErrorKind::NotIterable,
                                            None,
                                            format!("Expected an iterable, got {}", ty),
                                        );
                                        encountered_invalid_star = true;
                                    }
                                }
                            }
                        }
                        _ => {
                            let ty = self.expr_infer_type_no_trace(
                                elt,
                                if unbounded.is_empty() {
                                    hint_ts.get(hint_ts_idx).or(default_hint)
                                } else {
                                    None
                                },
                                errors,
                            );
                            hint_ts_idx = hint_ts_idx.saturating_add(1);
                            if unbounded.is_empty() {
                                prefix.push(ty)
                            } else {
                                suffix.push(ty)
                            }
                        }
                    }
                }
                if encountered_invalid_star {
                    // We already produced the type error, and we can't really roll up a suitable outermost type here.
                    // TODO(stroxler): should we really be producing a `tuple[Any]` here? We do at least know *something* about the type!
                    Type::any_error()
                } else {
                    match unbounded.as_slice() {
                        [] => Type::tuple(prefix),
                        [middle] => Type::Tuple(Tuple::unpacked(prefix, middle.clone(), suffix)),
                        // We can't precisely model unpacking two unbounded iterables, so we'll keep any
                        // concrete prefix and suffix elements and merge everything in between into an unbounded tuple
                        _ => {
                            let middle_types: Vec<Type> = unbounded
                                .iter()
                                .map(|t| {
                                    self.unwrap_iterable(t)
                                        .unwrap_or(Type::Any(AnyStyle::Implicit))
                                })
                                .collect();
                            Type::Tuple(Tuple::unpacked(
                                prefix,
                                Type::Tuple(Tuple::Unbounded(Box::new(self.unions(middle_types)))),
                                suffix,
                            ))
                        }
                    }
                }
            }
            Expr::List(x) => {
                let elt_hint = hint.and_then(|ty| self.decompose_list(ty));
                if x.is_empty() {
                    let elem_ty = self.solver().fresh_contained(self.uniques).to_type();
                    self.stdlib.list(elem_ty).to_type()
                } else {
                    let elem_tys = self.elts_infer(&x.elts, elt_hint, errors);
                    self.stdlib.list(self.unions(elem_tys)).to_type()
                }
            }
            Expr::Dict(x) => {
                let flattened_items = Ast::flatten_dict_items(&x.items);
                if let Some(hint @ Type::TypedDict(box typed_dict)) = hint {
                    self.check_dict_items_against_typed_dict(
                        flattened_items,
                        typed_dict,
                        x.range,
                        errors,
                    );
                    hint.clone()
                } else {
                    let (key_hint, value_hint) =
                        hint.map_or((None, None), |ty| self.decompose_dict(ty));
                    if x.is_empty() {
                        let key_ty = key_hint.unwrap_or_else(|| {
                            self.solver().fresh_contained(self.uniques).to_type()
                        });
                        let value_ty = value_hint.unwrap_or_else(|| {
                            self.solver().fresh_contained(self.uniques).to_type()
                        });
                        self.stdlib.dict(key_ty, value_ty).to_type()
                    } else {
                        let mut key_tys = Vec::new();
                        let mut value_tys = Vec::new();
                        flattened_items.iter().for_each(|x| match &x.key {
                            Some(key) => {
                                let key_t = self.expr_infer_with_hint_promote(
                                    key,
                                    key_hint.as_ref(),
                                    errors,
                                );
                                let value_t = self.expr_infer_with_hint_promote(
                                    &x.value,
                                    value_hint.as_ref(),
                                    errors,
                                );
                                key_tys.push(key_t);
                                value_tys.push(value_t);
                            }
                            None => {
                                let ty = self.expr_infer(&x.value, errors);
                                if let Some((key_ty, value_ty)) = self.unwrap_mapping(&ty) {
                                    key_tys.push(key_ty);
                                    value_tys.push(value_ty);
                                } else {
                                    self.error(
                                        errors,
                                        x.value.range(),
                                        ErrorKind::InvalidArgument,
                                        None,
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
            }
            Expr::Set(x) => {
                let elem_hint = hint.and_then(|ty| self.decompose_set(ty));
                if x.is_empty() {
                    let elem_ty = elem_hint
                        .unwrap_or_else(|| self.solver().fresh_contained(self.uniques).to_type());
                    self.stdlib.set(elem_ty).to_type()
                } else {
                    let elem_tys = self.elts_infer(&x.elts, elem_hint, errors);
                    self.stdlib.set(self.unions(elem_tys)).to_type()
                }
            }
            Expr::ListComp(x) => {
                let elem_hint = hint.and_then(|ty| self.decompose_list(ty));
                self.ifs_infer(&x.generators, errors);
                let elem_ty = self.expr_infer_with_hint_promote(&x.elt, elem_hint.as_ref(), errors);
                self.stdlib.list(elem_ty).to_type()
            }
            Expr::SetComp(x) => {
                let elem_hint = hint.and_then(|ty| self.decompose_set(ty));
                self.ifs_infer(&x.generators, errors);
                self.ifs_infer(&x.generators, errors);
                let elem_ty = self.expr_infer_with_hint_promote(&x.elt, elem_hint.as_ref(), errors);
                self.stdlib.set(elem_ty).to_type()
            }
            Expr::DictComp(x) => {
                let (key_hint, value_hint) =
                    hint.map_or((None, None), |ty| self.decompose_dict(ty));
                self.ifs_infer(&x.generators, errors);
                let key_ty = self.expr_infer_with_hint_promote(&x.key, key_hint.as_ref(), errors);
                let value_ty =
                    self.expr_infer_with_hint_promote(&x.value, value_hint.as_ref(), errors);
                self.stdlib.dict(key_ty, value_ty).to_type()
            }
            Expr::Generator(x) => {
                let yield_hint = hint.and_then(|ty| self.decompose_generator_yield(ty));
                self.ifs_infer(&x.generators, errors);
                let yield_ty = self
                    .expr_infer_type_info_with_hint(&x.elt, yield_hint.as_ref(), errors)
                    .into_ty();
                self.stdlib
                    .generator(yield_ty, Type::None, Type::None)
                    .to_type()
            }
            Expr::Await(x) => {
                let awaiting_ty = self.expr_infer(&x.value, errors);
                match self.unwrap_awaitable(&awaiting_ty) {
                    Some(ty) => ty,
                    None => self.error(
                        errors,
                        x.range,
                        ErrorKind::AsyncError,
                        None,
                        ErrorContext::Await(awaiting_ty).format(),
                    ),
                }
            }
            Expr::Yield(x) => self.get(&KeyYield(x.range)).send_ty.clone(),
            Expr::YieldFrom(x) => self.get(&KeyYieldFrom(x.range)).return_ty.clone(),
            Expr::Compare(x) => self.compare_infer(x, errors),
            Expr::Call(x) => {
                let ty_fun = self.expr_infer(&x.func, errors);
                if matches!(&ty_fun, Type::ClassDef(cls) if cls.has_qname("builtins", "super")) {
                    if is_special_name(&x.func, "super") {
                        self.get(&Key::SuperInstance(x.range)).arc_clone_ty()
                    } else {
                        // Because we have to construct a binding for super in order to fill in
                        // implicit arguments, we can't handle things like local aliases to super.
                        Type::any_implicit()
                    }
                } else {
                    let func_range = x.func.range();
                    self.distribute_over_union(&ty_fun, |ty| match ty.callee_kind() {
                        Some(CalleeKind::Function(FunctionKind::AssertType)) => self
                            .call_assert_type(
                                &x.arguments.args,
                                &x.arguments.keywords,
                                x.range,
                                errors,
                            ),
                        Some(CalleeKind::Function(FunctionKind::RevealType)) => self
                            .call_reveal_type(
                                &x.arguments.args,
                                &x.arguments.keywords,
                                x.range,
                                errors,
                            ),
                        Some(CalleeKind::Function(FunctionKind::Cast)) => {
                            // For typing.cast, we have to hard-code a check for whether the first argument
                            // is a type, so it's simplest to special-case the entire call.
                            self.call_typing_cast(
                                &x.arguments.args,
                                &x.arguments.keywords,
                                func_range,
                                errors,
                            )
                        }
                        // Treat assert_type and reveal_type like pseudo-builtins for convenience. Note that we still
                        // log a name-not-found error, but we also assert/reveal the type as requested.
                        None if matches!(ty, Type::Any(AnyStyle::Error))
                            && is_special_name(&x.func, "assert_type") =>
                        {
                            self.call_assert_type(
                                &x.arguments.args,
                                &x.arguments.keywords,
                                x.range,
                                errors,
                            )
                        }
                        None if matches!(ty, Type::Any(AnyStyle::Error))
                            && is_special_name(&x.func, "reveal_type") =>
                        {
                            self.call_reveal_type(
                                &x.arguments.args,
                                &x.arguments.keywords,
                                x.range,
                                errors,
                            )
                        }
                        _ => {
                            self.check_isinstance(&ty_fun, x, errors);
                            let args = x.arguments.args.map(|arg| match arg {
                                Expr::Starred(x) => CallArg::Star(&x.value, x.range),
                                _ => CallArg::Expr(arg),
                            });
                            let callable = self.as_call_target_or_error(
                                ty.clone(),
                                CallStyle::FreeForm,
                                func_range,
                                errors,
                                None,
                            );
                            self.call_infer(
                                callable,
                                &args,
                                &x.arguments.keywords,
                                func_range,
                                errors,
                                None,
                            )
                        }
                    })
                }
            }
            Expr::FString(x) => {
                // Ensure we detect type errors in f-string expressions.
                let mut all_literal_strings = true;
                x.visit(&mut |x| {
                    let fstring_expr_ty = self.expr_infer(x, errors);
                    if !fstring_expr_ty.is_literal_string() {
                        all_literal_strings = false;
                    }
                });
                match Lit::from_fstring(x) {
                    Some(lit) => lit.to_type(),
                    _ if all_literal_strings => Type::LiteralString,
                    _ => self.stdlib.str().to_type(),
                }
            }
            Expr::StringLiteral(x) => Lit::from_string_literal(x).to_type(),
            Expr::BytesLiteral(x) => Lit::from_bytes_literal(x).to_type(),
            Expr::NumberLiteral(x) => match &x.value {
                Number::Int(x) => Lit::from_int(x).to_type(),
                Number::Float(_) => self.stdlib.float().to_type(),
                Number::Complex { .. } => self.stdlib.complex().to_type(),
            },
            Expr::BooleanLiteral(x) => Lit::from_boolean_literal(x).to_type(),
            Expr::NoneLiteral(_) => Type::None,
            Expr::EllipsisLiteral(_) => Type::Ellipsis,
            Expr::Subscript(x) => {
                let xs = Ast::unpack_slice(&x.slice);
                // TODO: We don't deal properly with hint here, we should.
                let fun = self.expr_infer(&x.value, errors);
                self.distribute_over_union(&fun, |fun| {
                    let mut fun = fun.clone();
                    if let Type::Var(v) = fun {
                        fun = self.solver().force_var(v);
                    }
                    if matches!(&fun, Type::ClassDef(t) if t.name() == "tuple") {
                        fun = Type::type_form(Type::SpecialForm(SpecialForm::Tuple));
                    }
                    match fun {
                        Type::Forall(forall) => {
                            let tys = xs.map(|x| {
                                self.expr_untype(x, TypeFormContext::TypeArgument, errors)
                            });
                            let targs = self.check_and_create_targs(
                                &forall.body.name(),
                                &forall.tparams,
                                tys,
                                x.range,
                                errors,
                            );
                            let param_map = forall
                                .tparams
                                .quantified()
                                .zip(targs.as_slice().iter().cloned())
                                .collect::<SmallMap<_, _>>();
                            forall.body.as_type().subst(&param_map)
                        }
                        // Note that we have to check for `builtins.type` by name here because this code runs
                        // when we're bootstrapping the stdlib and don't have access to class objects yet.
                        Type::ClassDef(cls) if cls.has_qname("builtins", "type") => {
                            let targ = match xs.len() {
                                // This causes us to treat `type[list]` as equivalent to `type[list[Any]]`,
                                // which may or may not be what we want.
                                1 => {
                                    self.expr_untype(&xs[0], TypeFormContext::TypeArgument, errors)
                                }
                                _ => self.error(
                                    errors,
                                    x.range,
                                    ErrorKind::BadSpecialization,
                                    None,
                                    format!(
                                        "Expected 1 type argument for `type`, got {}",
                                        xs.len()
                                    ),
                                ),
                            };
                            // TODO: Validate that `targ` refers to a "valid in-scope class or TypeVar"
                            // (https://typing.readthedocs.io/en/latest/spec/annotations.html#type-and-annotation-expressions)
                            Type::type_form(Type::type_form(targ))
                        }
                        // TODO: pyre_extensions.PyreReadOnly is a non-standard type system extension that marks read-only
                        // objects. We don't support it yet.
                        Type::ClassDef(cls)
                            if cls.has_qname("pyre_extensions", "PyreReadOnly")
                                || cls.has_qname("pyre_extensions", "ReadOnly") =>
                        {
                            match xs.len() {
                                1 => self.expr_infer(&xs[0], errors),
                                _ => self.error(
                                    errors,
                                    x.range,
                                    ErrorKind::BadSpecialization,
                                    None,
                                    format!(
                                        "Expected 1 type argument for `PyreReadOnly`, got {}",
                                        xs.len()
                                    ),
                                ),
                            }
                        }
                        Type::ClassDef(cls) => Type::type_form(self.specialize(
                            &cls,
                            xs.map(|x| self.expr_untype(x, TypeFormContext::TypeArgument, errors)),
                            x.range,
                            errors,
                        )),
                        Type::Type(box Type::SpecialForm(special)) => {
                            self.apply_special_form(special, &x.slice, x.range, errors)
                        }
                        Type::Tuple(Tuple::Concrete(ref elts)) if xs.len() == 1 => self
                            .infer_tuple_index(
                                elts.to_owned(),
                                &xs[0],
                                x.range,
                                errors,
                                Some(&|| ErrorContext::Index(fun.clone())),
                            ),
                        Type::Tuple(_) if xs.len() == 1 => self.call_method_or_error(
                            &fun,
                            &dunder::GETITEM,
                            x.range,
                            &[CallArg::Expr(&x.slice)],
                            &[],
                            errors,
                            Some(&|| ErrorContext::Index(fun.clone())),
                        ),
                        Type::Any(style) => style.propagate(),
                        Type::LiteralString | Type::Literal(Lit::String(_)) if xs.len() <= 3 => {
                            // We could have a more precise type here, but this matches Pyright.
                            self.stdlib.str().to_type()
                        }
                        Type::ClassType(ref cls)
                            if let Some(elts) = self.named_tuple_element_types(cls) =>
                        {
                            self.infer_tuple_index(
                                elts,
                                &x.slice,
                                x.range,
                                errors,
                                Some(&|| ErrorContext::Index(fun.clone())),
                            )
                        }
                        Type::ClassType(_) => self.call_method_or_error(
                            &fun,
                            &dunder::GETITEM,
                            x.range,
                            &[CallArg::Expr(&x.slice)],
                            &[],
                            errors,
                            Some(&|| ErrorContext::Index(fun.clone())),
                        ),
                        Type::TypedDict(typed_dict) => {
                            let key_ty = self.expr_infer(&x.slice, errors);
                            self.distribute_over_union(&key_ty, |ty| match ty {
                                Type::Literal(Lit::String(field_name)) => {
                                    if let Some(field) =
                                        self.typed_dict_field(&typed_dict, &Name::new(field_name))
                                    {
                                        field.ty.clone()
                                    } else {
                                        self.error(
                                            errors,
                                            x.slice.range(),
                                            ErrorKind::TypedDictKeyError,
                                            None,
                                            format!(
                                                "TypedDict `{}` does not have key `{}`",
                                                typed_dict.name(),
                                                field_name
                                            ),
                                        )
                                    }
                                }
                                _ => self.error(
                                    errors,
                                    x.slice.range(),
                                    ErrorKind::TypedDictKeyError,
                                    None,
                                    format!(
                                        "Invalid key for TypedDict `{}`, got `{}`",
                                        typed_dict.name(),
                                        self.for_display(ty.clone())
                                    ),
                                ),
                            })
                        }
                        t => self.error(
                            errors,
                            x.range,
                            ErrorKind::BadSpecialization,
                            None,
                            format!(
                                "Can't apply arguments to non-class, got {}",
                                self.for_display(t)
                            ),
                        ),
                    }
                })
            }
            Expr::Starred(ExprStarred { value: box x, .. }) => {
                let ty = self.expr_untype(x, TypeFormContext::TypeArgument, errors);
                Type::Unpack(Box::new(ty))
            }
            Expr::Slice(_) => {
                // TODO(stroxler, yangdanny): slices are generic, we should not hard code to int.
                let int = self.stdlib.int().to_type();
                self.stdlib.slice(int.clone(), int.clone(), int).to_type()
            }
            Expr::IpyEscapeCommand(x) => self.error(
                errors,
                x.range,
                ErrorKind::Unsupported,
                None,
                "IPython escapes are not supported".to_owned(),
            ),
        }
    }

    fn expr_infer_with_hint_promote(
        &self,
        x: &Expr,
        hint: Option<&Type>,
        errors: &ErrorCollector,
    ) -> Type {
        let ty = self
            .expr_infer_type_info_with_hint(x, hint, errors)
            .into_ty();
        if let Some(want) = hint
            && self.solver().is_subset_eq(&ty, want, self.type_order())
        {
            want.clone()
        } else {
            ty.promote_literals(self.stdlib)
        }
    }

    /// When indexing/slicing concrete tuples with literals, try to infer a more precise type
    fn infer_tuple_index(
        &self,
        elts: Vec<Type>,
        index: &Expr,
        range: TextRange,
        errors: &ErrorCollector,
        context: Option<&dyn Fn() -> ErrorContext>,
    ) -> Type {
        match index {
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
                            Type::Literal(Lit::Int(idx)) => idx.as_i64(),
                            _ => None,
                        }
                    }
                    None => Some(0),
                };
                let upper_literal = match upper_expr {
                    Some(box expr) => {
                        let upper_type = self.expr_infer(expr, errors);
                        match &upper_type {
                            Type::Literal(Lit::Int(idx)) => idx.as_i64(),
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
                    _ => self.call_method_or_error(
                        &Type::Tuple(Tuple::Concrete(elts)),
                        &dunder::GETITEM,
                        range,
                        &[CallArg::Expr(index)],
                        &[],
                        errors,
                        context,
                    ),
                }
            }
            _ => {
                let idx_type = self.expr_infer(index, errors);
                match &idx_type {
                    Type::Literal(Lit::Int(idx)) if let Some(idx) = idx.as_i64() => {
                        let elt_idx = if idx >= 0 {
                            idx
                        } else {
                            elts.len() as i64 + idx
                        } as usize;
                        if let Some(elt) = elts.get(elt_idx) {
                            elt.clone()
                        } else {
                            self.error(
                                errors,
                                range,
                                ErrorKind::IndexError,
                                None,
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
                        &[CallArg::Expr(index)],
                        &[],
                        errors,
                        context,
                    ),
                }
            }
        }
    }
}

/// Match on an expression by name. Should be used only for special names that we essentially treat like keywords,
/// like reveal_type.
fn is_special_name(x: &Expr, name: &str) -> bool {
    match x {
        // Note that this matches on a bare name regardless of whether it's been imported.
        // It's convenient to be able to call functions like reveal_type in the course of
        // debugging without scrolling to the top of the file to add an import.
        Expr::Name(x) => x.id.as_str() == name,
        _ => false,
    }
}
