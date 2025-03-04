/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::BoolOp;
use ruff_python_ast::CmpOp;
use ruff_python_ast::Comprehension;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprBinOp;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprSlice;
use ruff_python_ast::ExprStarred;
use ruff_python_ast::Identifier;
use ruff_python_ast::Keyword;
use ruff_python_ast::Number;
use ruff_python_ast::Operator;
use ruff_python_ast::UnaryOp;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::call::CallStyle;
use crate::alt::callable::CallArg;
use crate::ast::Ast;
use crate::binding::binding::Key;
use crate::binding::binding::KeyYield;
use crate::binding::binding::KeyYieldFrom;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::error::context::TypeCheckContext;
use crate::error::kind::ErrorKind;
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
use crate::types::type_var::Variance;
use crate::types::type_var_tuple::TypeVarTuple;
use crate::types::types::AnyStyle;
use crate::types::types::CalleeKind;
use crate::types::types::Decoration;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;
use crate::util::prelude::VecExt;
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
        self.expr_with_separate_check_errors(x, check.map(|ty| (ty, errors)), errors)
    }

    pub fn expr_with_separate_check_errors(
        &self,
        x: &Expr,
        check: Option<(&Type, &ErrorCollector)>,
        errors: &ErrorCollector,
    ) -> Type {
        match &check {
            Some((want, check_errors)) if !want.is_any() => {
                let got = self.expr_infer_with_hint(x, Some(want), errors);
                self.check_type(
                    want,
                    &got,
                    x.range(),
                    check_errors,
                    &TypeCheckContext::unknown(),
                )
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
            .map(|arg| self.expr_untype(arg, errors))
            .collect::<Vec<_>>();
        if !constraints.is_empty() {
            restriction = Some(Restriction::Constraints(constraints));
        }

        for kw in &x.arguments.keywords {
            match &kw.arg {
                Some(id) => match id.id.as_str() {
                    "bound" => {
                        let bound = self.expr_untype(&kw.value, errors);
                        if restriction.is_some() {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidTypeVar,
                                "TypeVar cannot have both constraints and bound".to_owned(),
                            );
                        } else {
                            restriction = Some(Restriction::Bound(bound));
                        }
                    }
                    "default" => default = Some(self.expr_untype(&kw.value, errors)),
                    "covariant" => try_set_variance(kw, Some(Variance::Covariant)),
                    "contravariant" => try_set_variance(kw, Some(Variance::Contravariant)),
                    "infer_variance" => try_set_variance(kw, None),
                    "name" => {
                        if arg_name {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidTypeVar,
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
                            format!("Unexpected keyword argument `{}` to TypeVar", id.id),
                        );
                    }
                },
                _ => {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidTypeVar,
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
                "Missing `name` argument".to_owned(),
            );
        }

        self.id_cache().type_var(
            name,
            self.module_info().dupe(),
            restriction.unwrap_or(Restriction::Unrestricted),
            default,
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
                    "Expected first argument of ParamSpec to be a string literal".to_owned(),
                );
            }
        };

        if let Some(arg) = x.arguments.args.first() {
            check_name_arg(arg);
            arg_name = true;
        }

        for kw in &x.arguments.keywords {
            match &kw.arg {
                Some(id) => match id.id.as_str() {
                    "name" => {
                        if arg_name {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidParamSpec,
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
                            ErrorKind::InvalidParamSpec,
                            format!("Unexpected keyword argument `{}` to ParamSpec", id.id),
                        );
                    }
                },
                _ => {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidParamSpec,
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
                "Missing `name` argument".to_owned(),
            );
        }

        self.id_cache().param_spec(name, self.module_info().dupe())
    }

    pub fn typevartuple_from_call(
        &self,
        name: Identifier,
        x: &ExprCall,
        errors: &ErrorCollector,
    ) -> TypeVarTuple {
        // TODO: check and complain on extra args, keywords
        let mut arg_name = false;

        let check_name_arg = |arg: &Expr| {
            if let Expr::StringLiteral(lit) = arg {
                if lit.value.to_str() != name.id.as_str() {
                    self.error(
                        errors,
                        x.range,
                        ErrorKind::InvalidTypeVarTuple,
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
                    "Expected first argument of TypeVarTuple to be a string literal".to_owned(),
                );
            }
        };

        if let Some(arg) = x.arguments.args.first() {
            check_name_arg(arg);
            arg_name = true;
        }

        for kw in &x.arguments.keywords {
            match &kw.arg {
                Some(id) => match id.id.as_str() {
                    "name" => {
                        if arg_name {
                            self.error(
                                errors,
                                kw.range,
                                ErrorKind::InvalidTypeVarTuple,
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
                            ErrorKind::InvalidTypeVarTuple,
                            format!("Unexpected keyword argument `{}` to TypeVarTuple", id.id),
                        );
                    }
                },
                _ => {
                    self.error(
                        errors,
                        kw.range,
                        ErrorKind::InvalidTypeVarTuple,
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
                "Missing `name` argument".to_owned(),
            );
        }

        self.id_cache()
            .type_var_tuple(name, self.module_info().dupe())
    }

    pub fn expr_infer(&self, x: &Expr, errors: &ErrorCollector) -> Type {
        self.expr_infer_with_hint(x, None, errors)
    }

    pub fn check_isinstance(&self, ty_fun: &Type, x: &ExprCall, errors: &ErrorCollector) {
        if let Some(CalleeKind::Callable(CallableKind::IsInstance)) = ty_fun.callee_kind() {
            if x.arguments.args.len() == 2 {
                let is_instance_class_type = self.expr_infer(&x.arguments.args[1], errors);
                if let Type::ClassDef(cls) = is_instance_class_type {
                    let metadata = self.get_metadata_for_class(&cls);
                    if metadata.is_new_type() {
                        self.error(
                            errors,
                            x.range,
                            ErrorKind::InvalidArgument,
                            format!("NewType `{}` not allowed in isinstance.", cls.name()),
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
            Some(CalleeKind::Callable(CallableKind::Override)) => {
                // if an override decorator exists, then update the callable kind
                return Type::Decoration(Decoration::Override(Box::new(decoratee)));
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
        let ty = match x {
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
                        ErrorKind::UnsupportedOperand,
                        format!("Unary {} is not supported on {}", x.op.as_str(), t),
                    ),
                };
                self.distribute_over_union(&t, |t| match x.op {
                    UnaryOp::USub => {
                        let f = |lit: &Lit| lit.negate(self.stdlib, x.range, errors);
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
                        let f = |lit: &Lit| lit.invert(x.range, errors);
                        unop(t, &f, &dunder::INVERT)
                    }
                })
            }
            Expr::Lambda(lambda) => {
                let mut param_vars = Vec::new();
                if let Some(parameters) = &lambda.parameters {
                    param_vars.reserve(parameters.len());
                    for x in parameters.iter() {
                        param_vars.push((&x.name().id, self.bindings().get_lambda_param(x.name())));
                    }
                }
                let return_hint = hint
                    .iter()
                    .flat_map(|ty| self.decompose_lambda(ty, &param_vars))
                    .next();
                let params = param_vars.into_map(|(name, var)| {
                    Param::Pos(
                        name.clone(),
                        self.solver().force_var(var),
                        Required::Required,
                    )
                });
                let params = Params::List(ParamList::new(params));
                let ret = self.expr_infer_with_hint(&lambda.body, return_hint.as_ref(), errors);
                Type::Callable(Box::new(Callable { params, ret }), CallableKind::Anon)
            }
            Expr::If(x) => {
                // TODO: Support type refinement
                let condition_type = self.expr_infer(&x.test, errors);
                let body_type = self.expr_infer_with_hint(&x.body, hint, errors);
                let orelse_type = self.expr_infer_with_hint(&x.orelse, hint, errors);
                match condition_type.as_bool() {
                    Some(true) => body_type,
                    Some(false) => orelse_type,
                    None => self.union(body_type, orelse_type),
                }
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
                for elt in x.elts.iter() {
                    match elt {
                        Expr::Starred(ExprStarred { box value, .. }) => {
                            let ty = self.expr_infer_with_hint(value, None, errors);
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
                                        return self.error(
                                            errors,
                                            x.range(),
                                            ErrorKind::NotIterable,
                                            format!("Expected an iterable, got {}", ty),
                                        );
                                    }
                                }
                            }
                        }
                        _ => {
                            let ty = self.expr_infer_with_hint(
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
            Expr::List(x) => {
                let elt_hint = hint.and_then(|ty| self.decompose_list(ty));
                if x.is_empty() {
                    let elem_ty = self.solver().fresh_contained(self.uniques).to_type();
                    self.stdlib.list(elem_ty).to_type()
                } else {
                    let elem_tys = x.elts.map(|x| match x {
                        Expr::Starred(ExprStarred { box value, .. }) => {
                            let unpacked_ty =
                                self.expr_infer_with_hint_promote(value, hint, errors);
                            if let Some(iterable_ty) = self.unwrap_iterable(&unpacked_ty) {
                                iterable_ty
                            } else {
                                self.error(
                                    errors,
                                    x.range(),
                                    ErrorKind::NotIterable,
                                    format!("Expected an iterable, got {}", unpacked_ty),
                                )
                            }
                        }
                        _ => self.expr_infer_with_hint_promote(x, elt_hint.as_ref(), errors),
                    });
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
                let hint = hint.and_then(|ty| self.decompose_set(ty));
                if x.is_empty() {
                    let elem_ty = hint
                        .unwrap_or_else(|| self.solver().fresh_contained(self.uniques).to_type());
                    self.stdlib.set(elem_ty).to_type()
                } else {
                    let elem_tys = x
                        .elts
                        .map(|x| self.expr_infer_with_hint_promote(x, hint.as_ref(), errors));
                    self.stdlib.set(self.unions(elem_tys)).to_type()
                }
            }
            Expr::ListComp(x) => {
                let hint = hint.and_then(|ty| self.decompose_list(ty));
                self.ifs_infer(&x.generators, errors);
                let elem_ty = self.expr_infer_with_hint_promote(&x.elt, hint.as_ref(), errors);
                self.stdlib.list(elem_ty).to_type()
            }
            Expr::SetComp(x) => {
                let hint = hint.and_then(|ty| self.decompose_set(ty));
                self.ifs_infer(&x.generators, errors);
                self.ifs_infer(&x.generators, errors);
                let elem_ty = self.expr_infer_with_hint_promote(&x.elt, hint.as_ref(), errors);
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
                let yield_ty = self.expr_infer_with_hint(&x.elt, yield_hint.as_ref(), errors);
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
                        "Expression is not awaitable".to_owned(),
                    ),
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
                            ErrorKind::UnsupportedOperand,
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
            Expr::Call(x) => {
                let ty_fun = self.expr_infer(&x.func, errors);
                if matches!(&ty_fun, Type::ClassDef(cls) if cls.has_qname("builtins", "super")) {
                    if is_special_name(&x.func, "super") {
                        return self.get(&Key::SuperInstance(x.range)).arc_clone();
                    } else {
                        // Because we have to construct a binding for super in order to fill in
                        // implicit arguments, we can't handle things like local aliases to super.
                        return Type::any_implicit();
                    }
                }
                let func_range = x.func.range();
                self.distribute_over_union(&ty_fun, |ty| match ty.callee_kind() {
                    Some(CalleeKind::Callable(CallableKind::AssertType)) => self.call_assert_type(
                        &x.arguments.args,
                        &x.arguments.keywords,
                        x.range,
                        errors,
                    ),
                    Some(CalleeKind::Callable(CallableKind::RevealType)) => self.call_reveal_type(
                        &x.arguments.args,
                        &x.arguments.keywords,
                        x.range,
                        errors,
                    ),
                    Some(CalleeKind::Callable(CallableKind::Cast)) => {
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
                        );
                        self.call_infer(callable, &args, &x.arguments.keywords, func_range, errors)
                    }
                })
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
                            _ => self.error(
                                errors,
                                x.range,
                                ErrorKind::BadSpecialization,
                                format!(
                                    "Expected 1 type argument for class `type`, got {}",
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
                                format!(
                                    "Expected 1 type argument for class `PyreReadOnly`, got {}",
                                    xs.len()
                                ),
                            ),
                        }
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
                    Type::Tuple(_) if xs.len() == 1 => self.call_method_or_error(
                        &fun,
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
                    Type::TypedDict(typed_dict) => {
                        let key_ty = self.expr_infer(&x.slice, errors);
                        self.distribute_over_union(&key_ty, |ty| match ty {
                            Type::Literal(Lit::String(field_name)) => {
                                if let Some(field) =
                                    typed_dict.fields().get(&Name::new(field_name.clone()))
                                {
                                    field.ty.clone()
                                } else {
                                    self.error(
                                        errors,
                                        x.slice.range(),
                                        ErrorKind::TypedDictKeyError,
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
                                format!(
                                    "Invalid key for TypedDict `{}`, got `{}`",
                                    typed_dict.name(),
                                    ty.clone().deterministic_printing()
                                ),
                            ),
                        })
                    }
                    t => self.error(
                        errors,
                        x.range,
                        ErrorKind::BadSpecialization,
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
                // TODO(stroxler): The handling of `typing.Any` should use proper name resolution.
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
                ErrorKind::Unsupported,
                "IPython escapes are not supported".to_owned(),
            ),
        };
        self.record_type_trace(x.range(), &ty);
        ty
    }

    fn expr_infer_with_hint_promote(
        &self,
        x: &Expr,
        hint: Option<&Type>,
        errors: &ErrorCollector,
    ) -> Type {
        let ty = self.expr_infer_with_hint(x, hint, errors);
        if let Some(hint) = hint
            && self.solver().is_subset_eq(&ty, hint, self.type_order())
        {
            hint.clone()
        } else {
            ty.promote_literals(self.stdlib)
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
                                ErrorKind::IndexError,
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
