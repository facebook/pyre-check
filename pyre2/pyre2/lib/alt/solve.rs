/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use itertools::Either;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::TypeParam;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::ordered_set::OrderedSet;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;
use vec1::Vec1;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::callable::CallArg;
use crate::alt::class::class_field::ClassField;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::alt::types::decorated_function::DecoratedFunction;
use crate::alt::types::legacy_lookup::LegacyTypeParameterLookup;
use crate::alt::types::yields::YieldFromResult;
use crate::alt::types::yields::YieldResult;
use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClass;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::BindingYield;
use crate::binding::binding::BindingYieldFrom;
use crate::binding::binding::ContextManagerKind;
use crate::binding::binding::EmptyAnswer;
use crate::binding::binding::FunctionBinding;
use crate::binding::binding::FunctionKind;
use crate::binding::binding::Key;
use crate::binding::binding::KeyExport;
use crate::binding::binding::NoneIfRecursive;
use crate::binding::binding::RaisedException;
use crate::binding::binding::SizeExpectation;
use crate::binding::binding::SuperStyle;
use crate::binding::binding::UnpackedPosition;
use crate::dunder;
use crate::dunder::inplace_dunder;
use crate::error::collector::ErrorCollector;
use crate::graph::index::Idx;
use crate::module::module_path::ModuleStyle;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Annotation;
use crate::types::annotation::Qualifier;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassKind;
use crate::types::literal::Lit;
use crate::types::module::Module;
use crate::types::quantified::Quantified;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::types::AnyStyle;
use crate::types::types::CalleeKind;
use crate::types::types::TParamInfo;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::types::types::TypeAlias;
use crate::types::types::TypeAliasStyle;
use crate::util::prelude::SliceExt;

pub enum Iterable {
    OfType(Type),
    FixedLen(Vec<Type>),
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn solve_legacy_tparam(
        &self,
        binding: &BindingLegacyTypeParam,
    ) -> Arc<LegacyTypeParameterLookup> {
        match &*self.get_idx(binding.0) {
            Type::Type(box Type::TypeVar(x)) => {
                let q = Quantified::type_var(self.uniques);
                Arc::new(LegacyTypeParameterLookup::Parameter(TParamInfo {
                    name: x.qname().id().clone(),
                    quantified: q,
                    restriction: x.restriction().clone(),
                    default: x.default().cloned(),
                    variance: x.variance(),
                }))
            }
            Type::Type(box Type::TypeVarTuple(x)) => {
                let q = Quantified::type_var_tuple(self.uniques);
                Arc::new(LegacyTypeParameterLookup::Parameter(TParamInfo {
                    name: x.qname().id().clone(),
                    quantified: q,
                    restriction: Restriction::Unrestricted,
                    default: None,
                    variance: Some(Variance::Invariant),
                }))
            }
            Type::Type(box Type::ParamSpec(x)) => {
                let q = Quantified::param_spec(self.uniques);
                Arc::new(LegacyTypeParameterLookup::Parameter(TParamInfo {
                    name: x.qname().id().clone(),
                    quantified: q,
                    restriction: Restriction::Unrestricted,
                    default: None,
                    variance: Some(Variance::Invariant),
                }))
            }
            ty => Arc::new(LegacyTypeParameterLookup::NotParameter(ty.clone())),
        }
    }

    pub fn solve_mro(
        &self,
        binding: &BindingClassMetadata,
        errors: &ErrorCollector,
    ) -> Arc<ClassMetadata> {
        let BindingClassMetadata {
            def: k,
            bases,
            keywords,
            decorators,
        } = binding;
        let metadata = match &self.get_idx(*k).0 {
            None => ClassMetadata::recursive(),
            Some(cls) => self.class_metadata_of(cls, bases, keywords, decorators, errors),
        };
        Arc::new(metadata)
    }

    pub fn solve_annotation(
        &self,
        binding: &BindingAnnotation,
        errors: &ErrorCollector,
    ) -> Arc<Annotation> {
        match binding {
            BindingAnnotation::AnnotateExpr(x, self_type) => {
                let mut ann = self.expr_annotation(x, errors);
                if let Some(self_type) = self_type
                    && let Some(ty) = &mut ann.ty
                {
                    let self_type = &*self.get_idx(*self_type);
                    ty.subst_self_type_mut(self_type);
                }
                Arc::new(ann)
            }
            BindingAnnotation::Type(x) => Arc::new(Annotation::new_type(x.clone())),
        }
    }

    fn expr_qualifier(&self, x: &Expr, errors: &ErrorCollector) -> Option<Qualifier> {
        let ty = match x {
            Expr::Name(_) | Expr::Attribute(_) => Some(self.expr(x, None, errors)),
            _ => None,
        };
        if let Some(Type::Type(box Type::SpecialForm(special))) = ty {
            special.to_qualifier()
        } else {
            None
        }
    }

    fn expr_annotation(&self, x: &Expr, errors: &ErrorCollector) -> Annotation {
        match x {
            _ if let Some(qualifier) = self.expr_qualifier(x, errors) => Annotation {
                qualifiers: vec![qualifier],
                ty: None,
            },
            Expr::Subscript(x)
                if Ast::unpack_slice(&x.slice).len() == 1
                    && let Some(qualifier) = self.expr_qualifier(&x.value, errors) =>
            {
                let mut ann = self.expr_annotation(&x.slice, errors);
                ann.qualifiers.insert(0, qualifier);
                ann
            }
            _ => Annotation::new_type(self.expr_untype(x, errors)),
        }
    }

    /// Given an `iterable` type, determine the iteration type; this is the type
    /// of `x` if we were to loop using `for x in iterable`.
    pub fn iterate(
        &self,
        iterable: &Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Vec<Iterable> {
        // Use the iterable protocol interfaces to determine the iterable type.
        // Special cases like Tuple should be intercepted first.
        match iterable {
            Type::ClassType(cls) if let Some(elts) = self.named_tuple_element_types(cls) => {
                vec![Iterable::FixedLen(elts.clone())]
            }
            Type::Tuple(Tuple::Concrete(elts)) => vec![Iterable::FixedLen(elts.clone())],
            Type::Var(v) if let Some(_guard) = self.recurser.recurse(*v) => {
                self.iterate(&self.solver().force_var(*v), range, errors)
            }
            Type::Union(ts) => ts
                .iter()
                .flat_map(|t| self.iterate(t, range, errors))
                .collect(),
            _ => {
                let ty = self
                    .unwrap_iterable(iterable)
                    .or_else(|| {
                        let int_ty = self.stdlib.int().to_type();
                        let arg = CallArg::Type(&int_ty, range);
                        self.call_method(iterable, &dunder::GETITEM, range, &[arg], &[], errors)
                    })
                    .unwrap_or_else(|| {
                        self.error(
                            errors,
                            range,
                            format!(
                                "Type `{}` is not iterable",
                                iterable.clone().deterministic_printing()
                            ),
                        )
                    });
                vec![Iterable::OfType(ty)]
            }
        }
    }

    fn check_is_exception(
        &self,
        x: &Expr,
        range: TextRange,
        allow_none: bool,
        errors: &ErrorCollector,
    ) {
        let actual_type = self.expr(x, None, errors);
        if allow_none && actual_type.is_none() {
            return;
        }
        let base_exception_class = self.stdlib.base_exception();
        let base_exception_class_type = Type::ClassDef(base_exception_class.class_object().dupe());
        let base_exception_type = base_exception_class.to_type();
        let expected_types = vec![base_exception_type, base_exception_class_type];
        if !self.solver().is_subset_eq(
            &actual_type,
            &Type::Union(expected_types),
            self.type_order(),
        ) {
            self.error(errors,
                 range,
                 format!(
                     "Expression `{}` has type `{actual_type}` which does not derive from BaseException",
                     self.module_info().display(x)
                 ),
             );
        }
    }

    fn tvars_to_tparams_for_type_alias(
        &self,
        ty: &mut Type,
        seen: &mut SmallMap<TypeVar, Quantified>,
        tparams: &mut Vec<TParamInfo>,
    ) {
        match ty {
            Type::Union(ts) => {
                for t in ts.iter_mut() {
                    self.tvars_to_tparams_for_type_alias(t, seen, tparams);
                }
            }
            Type::ClassType(cls) => {
                for t in cls.targs_mut().as_mut() {
                    self.tvars_to_tparams_for_type_alias(t, seen, tparams);
                }
            }
            Type::Callable(callable, _) => {
                let visit = |t: &mut Type| self.tvars_to_tparams_for_type_alias(t, seen, tparams);
                callable.visit_mut(visit);
            }
            Type::TypeVar(ty_var) => {
                let q = match seen.entry(ty_var.dupe()) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let q = Quantified::type_var(self.uniques);
                        e.insert(q);
                        tparams.push(TParamInfo {
                            name: ty_var.qname().id().clone(),
                            quantified: q,
                            restriction: Restriction::Unrestricted,
                            default: ty_var.default().cloned(),
                            variance: ty_var.variance(),
                        });
                        q
                    }
                };
                *ty = Type::Quantified(q);
            }
            _ => {}
        }
    }

    fn as_type_alias(
        &self,
        name: &Name,
        style: TypeAliasStyle,
        ty: Type,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if matches!(
            style,
            TypeAliasStyle::Scoped | TypeAliasStyle::LegacyExplicit
        ) && self.untype_opt(ty.clone(), range).is_none()
        {
            self.error(
                errors,
                range,
                format!("Expected `{name}` to be a type alias, got {ty}"),
            );
            return Type::any_error();
        }
        let mut ty = match &ty {
            Type::ClassDef(cls) => Type::type_form(self.promote(cls, range)),
            t => t.clone(),
        };
        let mut seen = SmallMap::new();
        let mut tparams = Vec::new();
        match ty {
            Type::Type(ref mut t) => {
                self.tvars_to_tparams_for_type_alias(t, &mut seen, &mut tparams)
            }
            _ => {}
        }
        let ta = Type::TypeAlias(TypeAlias::new(name.clone(), ty, style));
        ta.forall(self.type_params(range, tparams, errors))
    }

    fn context_value_enter(
        &self,
        context_manager_type: &Type,
        kind: ContextManagerKind,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match kind {
            ContextManagerKind::Sync => self.call_method_or_error(
                context_manager_type,
                &dunder::ENTER,
                range,
                &[],
                &[],
                errors,
            ),
            ContextManagerKind::Async => match self.unwrap_awaitable(&self.call_method_or_error(
                context_manager_type,
                &dunder::AENTER,
                range,
                &[],
                &[],
                errors,
            )) {
                Some(ty) => ty,
                None => self.error(
                    errors,
                    range,
                    format!("Expected `{}` to be async", dunder::AENTER),
                ),
            },
        }
    }

    fn context_value_exit(
        &self,
        context_manager_type: &Type,
        kind: ContextManagerKind,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let base_exception_class_type = Type::type_form(self.stdlib.base_exception().to_type());
        let arg1 = Type::Union(vec![base_exception_class_type, Type::None]);
        let arg2 = Type::Union(vec![self.stdlib.base_exception().to_type(), Type::None]);
        let arg3 = Type::Union(vec![self.stdlib.traceback_type().to_type(), Type::None]);
        let exit_arg_types = [
            CallArg::Type(&arg1, range),
            CallArg::Type(&arg2, range),
            CallArg::Type(&arg3, range),
        ];
        match kind {
            ContextManagerKind::Sync => self.call_method_or_error(
                context_manager_type,
                &dunder::EXIT,
                range,
                &exit_arg_types,
                &[],
                errors,
            ),
            ContextManagerKind::Async => match self.unwrap_awaitable(&self.call_method_or_error(
                context_manager_type,
                &dunder::AEXIT,
                range,
                &exit_arg_types,
                &[],
                errors,
            )) {
                Some(ty) => ty,
                None => self.error(
                    errors,
                    range,
                    format!("Expected `{}` to be async", dunder::AEXIT),
                ),
            },
        }
    }

    fn context_value(
        &self,
        context_manager_type: Type,
        kind: ContextManagerKind,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let enter_type = self.context_value_enter(&context_manager_type, kind, range, errors);
        let exit_type = self.context_value_exit(&context_manager_type, kind, range, errors);
        self.check_type(
            &Type::Union(vec![self.stdlib.bool().to_type(), Type::None]),
            &exit_type,
            range,
            errors,
        );
        // TODO: `exit_type` may also affect exceptional control flow, which is yet to be supported:
        // https://typing.readthedocs.io/en/latest/spec/exceptions.html#context-managers
        enter_type
    }

    pub fn scoped_type_params(
        &self,
        x: Option<&TypeParams>,
        errors: &ErrorCollector,
    ) -> Vec<TParamInfo> {
        match x {
            Some(x) => {
                fn get_quantified(t: &Type) -> Quantified {
                    match t {
                        Type::Type(box Type::Quantified(q)) => *q,
                        _ => unreachable!(),
                    }
                }
                let mut params = Vec::new();
                for raw_param in x.type_params.iter() {
                    let name = raw_param.name();
                    let restriction = match raw_param {
                        TypeParam::TypeVar(tv) => match &tv.bound {
                            Some(box Expr::Tuple(tup)) => Restriction::Constraints(
                                tup.elts.map(|e| self.expr_untype(e, errors)),
                            ),
                            Some(e) => Restriction::Bound(self.expr_untype(e, errors)),
                            None => Restriction::Unrestricted,
                        },
                        _ => Restriction::Unrestricted,
                    };
                    let default = raw_param.default().map(|e| self.expr_untype(e, errors));
                    params.push(TParamInfo {
                        name: name.id.clone(),
                        quantified: get_quantified(
                            &self.get(&Key::Definition(ShortIdentifier::new(name))),
                        ),
                        restriction,
                        default,
                        variance: None,
                    });
                }
                params
            }
            None => Vec::new(),
        }
    }

    pub fn type_params(
        &self,
        range: TextRange,
        info: Vec<TParamInfo>,
        errors: &ErrorCollector,
    ) -> TParams {
        match TParams::new(info) {
            Ok(validated_tparams) => validated_tparams,
            Err(fixed_tparams) => {
                self.error(
                    errors,
                    range,
                    "A type parameter without a default cannot follow one with a default"
                        .to_owned(),
                );
                fixed_tparams
            }
        }
    }

    pub fn solve_binding(&self, binding: &Binding, errors: &ErrorCollector) -> Arc<Type> {
        // Replace any solved recursive variables with their answers.
        // We call self.unions() to simplify cases like
        // v = @1 | int, @1 = int.
        Arc::new(
            match self
                .solver()
                .expand(self.solve_binding_inner(binding, errors))
            {
                Type::Union(ts) => self.unions(ts),
                t => t,
            },
        )
    }

    pub fn solve_expectation(
        &self,
        binding: &BindingExpect,
        errors: &ErrorCollector,
    ) -> Arc<EmptyAnswer> {
        match binding {
            BindingExpect::UnpackedLength(b, range, expect) => {
                let iterable_ty = self.solve_binding_inner(b, errors);
                let iterables = self.iterate(&iterable_ty, *range, errors);
                for iterable in iterables {
                    match iterable {
                        Iterable::OfType(_) => {}
                        Iterable::FixedLen(ts) => {
                            let error = match expect {
                                SizeExpectation::Eq(n) => {
                                    if ts.len() == *n {
                                        None
                                    } else {
                                        match n {
                                            1 => Some(format!("{n} value")),
                                            _ => Some(format!("{n} values")),
                                        }
                                    }
                                }
                                SizeExpectation::Ge(n) => {
                                    if ts.len() >= *n {
                                        None
                                    } else {
                                        Some(format!("{n}+ values"))
                                    }
                                }
                            };
                            match error {
                                Some(expectation) => {
                                    self.error(
                                        errors,
                                        *range,
                                        format!(
                                            "Cannot unpack {} (of size {}) into {}",
                                            iterable_ty,
                                            ts.len(),
                                            expectation,
                                        ),
                                    );
                                }
                                None => {}
                            }
                        }
                    }
                }
            }
            BindingExpect::CheckRaisedException(RaisedException::WithoutCause(exc)) => {
                self.check_is_exception(exc, exc.range(), false, errors);
            }
            BindingExpect::CheckRaisedException(RaisedException::WithCause(box (exc, cause))) => {
                self.check_is_exception(exc, exc.range(), false, errors);
                self.check_is_exception(cause, cause.range(), true, errors);
            }
            BindingExpect::Eq(k1, k2, name) => {
                let ann1 = self.get_idx(*k1);
                let ann2 = self.get_idx(*k2);
                if let Some(t1) = &ann1.ty
                    && let Some(t2) = &ann2.ty
                    && *t1 != *t2
                {
                    self.error(
                        errors,
                        self.bindings().idx_to_key(*k1).range(),
                        format!(
                            "Inconsistent type annotations for {}: {}, {}",
                            name,
                            t1.clone().deterministic_printing(),
                            t2.clone().deterministic_printing(),
                        ),
                    );
                }
            }
            BindingExpect::CheckAssignTypeToAttribute(box (attr, got)) => {
                let base = self.expr(&attr.value, None, errors);
                let got = self.solve_binding(got, errors);
                self.check_attr_set_with_type(
                    base,
                    &attr.attr.id,
                    &got,
                    attr.range,
                    errors,
                    "Answers::solve_binding_inner::CheckAssignTypeToAttribute",
                );
            }
            BindingExpect::CheckAssignExprToAttribute(box (attr, value)) => {
                let base = self.expr(&attr.value, None, errors);
                self.check_attr_set_with_expr(
                    base,
                    &attr.attr.id,
                    value,
                    attr.range,
                    errors,
                    "Answers::solve_binding_inner::CheckAssignExprToAttribute",
                );
            }
        }
        Arc::new(EmptyAnswer)
    }

    pub fn solve_class(
        &self,
        cls: &BindingClass,
        errors: &ErrorCollector,
    ) -> Arc<NoneIfRecursive<Class>> {
        let cls = match cls {
            BindingClass::ClassDef(x) => self.class_definition(
                &x.def,
                x.fields.clone(),
                &x.bases,
                &x.legacy_tparams,
                errors,
            ),
            BindingClass::FunctionalClassDef(x, fields) => {
                self.functional_class_definition(x, fields)
            }
        };
        Arc::new(NoneIfRecursive(Some(cls)))
    }

    pub fn solve_class_field(
        &self,
        field: &BindingClassField,
        errors: &ErrorCollector,
    ) -> Arc<ClassField> {
        let field = match &self.get_idx(field.class).0 {
            None => ClassField::recursive(),
            Some(class) => {
                let value_ty = self.solve_binding(&field.value, errors);
                let annotation = field.annotation.map(|a| self.get_idx(a));
                self.calculate_class_field(
                    &field.name,
                    value_ty.as_ref(),
                    annotation.as_deref(),
                    &field.initial_value,
                    class,
                    field.range,
                    errors,
                )
            }
        };
        Arc::new(field)
    }

    pub fn solve_class_synthesized_fields(
        &self,
        fields: &BindingClassSynthesizedFields,
    ) -> Arc<ClassSynthesizedFields> {
        let fields = match &self.get_idx(fields.0).0 {
            None => ClassSynthesizedFields::default(),
            Some(cls) => self
                .get_typed_dict_synthesized_fields(cls)
                .or_else(|| self.get_dataclass_synthesized_fields(cls))
                .or_else(|| self.get_named_tuple_synthesized_fields(cls))
                .unwrap_or_default(),
        };
        Arc::new(fields)
    }

    fn solve_binding_inner(&self, binding: &Binding, errors: &ErrorCollector) -> Type {
        match binding {
            Binding::Expr(ann, e) => {
                let ty = ann.map(|k| self.get_idx(k));
                self.expr(e, ty.as_ref().and_then(|x| x.ty.as_ref()), errors)
            }
            Binding::TypeVar(ann, name, x) => {
                let ty = Type::type_form(self.typevar_from_call(name.clone(), x, errors).to_type());
                if let Some(k) = ann
                    && let Some(want) = &self.get_idx(*k).ty
                {
                    self.check_type(want, &ty, x.range, errors)
                } else {
                    ty
                }
            }
            Binding::ParamSpec(ann, name, x) => {
                let ty =
                    Type::type_form(self.paramspec_from_call(name.clone(), x, errors).to_type());
                if let Some(k) = ann
                    && let Some(want) = &self.get_idx(*k).ty
                {
                    self.check_type(want, &ty, x.range, errors)
                } else {
                    ty
                }
            }
            Binding::TypeVarTuple(ann, name, x) => {
                let ty = Type::type_form(
                    self.typevartuple_from_call(name.clone(), x, errors)
                        .to_type(),
                );
                if let Some(k) = ann
                    && let Some(want) = &self.get_idx(*k).ty
                {
                    self.check_type(want, &ty, x.range, errors)
                } else {
                    ty
                }
            }
            Binding::ReturnType(x) => {
                let is_generator = !x.yields.is_empty();
                let implicit_return = self.get_idx(x.implicit_return);
                if let Some((range, annot)) = &x.annot {
                    // TODO: A return type annotation like `Final` is invalid in this context.
                    // It will result in an implicit Any type, which is reasonable, but we should
                    // at least error here.
                    let ty = self.get_idx(*annot).get_type().clone();
                    if x.is_async && is_generator {
                        if self.decompose_async_generator(&ty).is_none() {
                            self.error(
                                errors,
                                *range,
                                "Async generator function should return `AsyncGenerator`"
                                    .to_owned(),
                            );
                        }
                    } else if is_generator {
                        if let Some((_, _, return_ty)) = self.decompose_generator(&ty) {
                            self.check_type(&return_ty, &implicit_return, *range, errors);
                        } else {
                            self.error(
                                errors,
                                *range,
                                "Generator function should return `Generator`".to_owned(),
                            );
                        }
                    } else {
                        self.check_type(&ty, &implicit_return, *range, errors);
                    }
                    ty
                } else {
                    let returns = x.returns.iter().map(|k| self.get_idx(*k).arc_clone());
                    // TODO: It should always be a no-op to include a `Type::Never` in unions, but
                    // `simple::test_solver_variables` fails if we do, because `solver::unions` does
                    // `is_subset_eq` to force free variables, causing them to be equated to
                    // `Type::Never` instead of becoming `Type::Any`.
                    let return_ty = if implicit_return.is_never() {
                        self.unions(returns.collect())
                    } else {
                        self.unions(
                            returns
                                .chain(std::iter::once(implicit_return.arc_clone()))
                                .collect(),
                        )
                    };
                    if is_generator {
                        let yield_ty = self.unions(
                            x.yields
                                .iter()
                                .map(|x| match x {
                                    Either::Left(k) => self.get_idx(*k).yield_ty.clone(),
                                    Either::Right(k) => self.get_idx(*k).yield_ty.clone(),
                                })
                                .collect(),
                        );
                        if x.is_async {
                            self.stdlib
                                .async_generator(yield_ty, Type::any_implicit())
                                .to_type()
                        } else {
                            self.stdlib
                                .generator(yield_ty, Type::any_implicit(), return_ty)
                                .to_type()
                        }
                    } else if x.is_async {
                        // TODO: Use the more precise `types.CoroutineType` instead.
                        self.stdlib
                            .coroutine(Type::any_implicit(), Type::any_implicit(), return_ty)
                            .to_type()
                    } else {
                        return_ty
                    }
                }
            }
            Binding::ReturnExplicit(x) => {
                let annot = x.annot.map(|k| self.get_idx(k));
                let hint = annot.as_ref().and_then(|ann| ann.ty.as_ref());

                if let Some(expr) = &x.expr {
                    if x.is_async && x.is_generator {
                        self.expr(expr, None, errors);
                        self.error(
                            errors,
                            expr.range(),
                            "Return statement with value is not allowed in async generator"
                                .to_owned(),
                        )
                    } else if x.is_generator {
                        let hint =
                            hint.and_then(|ty| self.decompose_generator(ty).map(|(_, _, r)| r));
                        self.expr(expr, hint.as_ref(), errors)
                    } else if matches!(hint, Some(Type::TypeGuard(_))) {
                        let hint = Some(Type::ClassType(self.stdlib.bool()));
                        self.expr(expr, hint.as_ref(), errors)
                    } else {
                        self.expr(expr, hint, errors)
                    }
                } else {
                    Type::None
                }
            }
            Binding::ReturnImplicit(x) => {
                // TODO: This is a bit of a hack. We want to implement Pyright's behavior where
                // stub functions allow any annotation, but we also infer a `Never` return type
                // instead of `None`. Instead, we should just ignore the implicit return for stub
                // functions when solving Binding::ReturnType. Unfortunately, this leads to
                // another issue (see comment on Binding::ReturnType).
                if x.function_kind == FunctionKind::Stub
                    || x.last_exprs
                        .as_ref()
                        .is_some_and(|xs| xs.iter().all(|k| self.get_idx(*k).is_never()))
                {
                    Type::never()
                } else {
                    Type::None
                }
            }
            Binding::ExceptionHandler(box ann, is_star) => {
                let base_exception_type = self.stdlib.base_exception().to_type();
                let base_exception_group_any_type = if *is_star {
                    // Only query for `BaseExceptionGroup` if we see an `except*` handler (which
                    // was introduced in Python3.11).
                    // We can't unconditionally query for `BaseExceptionGroup` until Python3.10
                    // is out of its EOL period.
                    Some(
                        self.stdlib
                            .base_exception_group(Type::Any(AnyStyle::Implicit))
                            .to_type(),
                    )
                } else {
                    None
                };
                let check_exception_type = |exception_type: Type, range| {
                    let exception = self.untype(exception_type, range, errors);
                    self.check_type(&base_exception_type, &exception, range, errors);
                    if let Some(base_exception_group_any_type) =
                        base_exception_group_any_type.as_ref()
                        && !exception.is_any()
                        && self.solver().is_subset_eq(
                            &exception,
                            base_exception_group_any_type,
                            self.type_order(),
                        )
                    {
                        self.error(errors, range, "Exception handler annotation in `except*` clause may not extend `BaseExceptionGroup`".to_owned());
                    }
                    exception
                };
                let exceptions = match ann {
                    // if the exception classes are written as a tuple literal, use each annotation's position for error reporting
                    Expr::Tuple(tup) => tup
                        .elts
                        .iter()
                        .map(|e| check_exception_type(self.expr(e, None, errors), e.range()))
                        .collect(),
                    _ => {
                        let exception_types = self.expr(ann, None, errors);
                        match exception_types {
                            Type::Tuple(Tuple::Concrete(ts)) => ts
                                .into_iter()
                                .map(|t| check_exception_type(t, ann.range()))
                                .collect(),
                            Type::Tuple(Tuple::Unbounded(box t)) => {
                                vec![check_exception_type(t, ann.range())]
                            }
                            _ => vec![check_exception_type(exception_types, ann.range())],
                        }
                    }
                };
                if *is_star {
                    self.stdlib
                        .exception_group(self.unions(exceptions))
                        .to_type()
                } else {
                    self.unions(exceptions)
                }
            }
            Binding::AugAssign(x) => {
                let base = self.expr(&x.target, None, errors);
                self.call_method_or_error(
                    &base,
                    &inplace_dunder(x.op),
                    x.range,
                    &[CallArg::Expr(&x.value)],
                    &[],
                    errors,
                )
            }
            Binding::IterableValue(ann, e) => {
                let ty = ann.map(|k| self.get_idx(k));
                let hint =
                    ty.and_then(|x| x.ty.clone().map(|ty| self.stdlib.iterable(ty).to_type()));
                let iterables =
                    self.iterate(&self.expr(e, hint.as_ref(), errors), e.range(), errors);
                let mut values = Vec::new();
                for iterable in iterables {
                    match iterable {
                        Iterable::OfType(ty) => values.push(ty),
                        Iterable::FixedLen(ts) => values.extend(ts),
                    }
                }
                self.unions(values)
            }
            Binding::ContextValue(ann, e, kind) => {
                let context_manager = self.expr(e, None, errors);
                let context_value = self.context_value(context_manager, *kind, e.range(), errors);
                let ty = ann.map(|k| self.get_idx(k));
                match ty.as_ref().and_then(|x| x.ty.as_ref()) {
                    Some(ty) => self.check_type(ty, &context_value, e.range(), errors),
                    None => context_value,
                }
            }
            Binding::SubscriptValue(box b, x) => {
                let base = self.expr(&x.value, None, errors);
                let slice_ty = self.expr(&x.slice, None, errors);
                let value_ty = self.solve_binding_inner(b, errors);
                match (&base, &slice_ty) {
                    (Type::TypedDict(typed_dict), Type::Literal(Lit::String(field_name))) => {
                        if let Some(field) = typed_dict.fields().get(&Name::new(field_name.clone()))
                        {
                            if field.read_only {
                                self.error(
                                    errors,
                                    x.slice.range(),
                                    format!(
                                        "Key `{}` in TypedDict `{}` is read-only",
                                        field_name,
                                        typed_dict.name(),
                                    ),
                                )
                            } else if !self.solver().is_subset_eq(
                                &value_ty,
                                &field.ty,
                                self.type_order(),
                            ) {
                                self.error(
                                    errors,
                                    x.range(),
                                    format!("Expected {}, got {}", field.ty, value_ty),
                                )
                            } else {
                                Type::None
                            }
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
                    (_, _) => self.call_method_or_error(
                        &base,
                        &dunder::SETITEM,
                        x.range,
                        &[
                            CallArg::Type(&slice_ty, x.slice.range()),
                            // use the subscript's location
                            CallArg::Type(&value_ty, x.range),
                        ],
                        &[],
                        errors,
                    ),
                }
            }
            Binding::UnpackedValue(b, range, pos) => {
                let iterables = self.iterate(&self.solve_binding_inner(b, errors), *range, errors);
                let mut values = Vec::new();
                for iterable in iterables {
                    values.push(match iterable {
                        Iterable::OfType(ty) => match pos {
                            UnpackedPosition::Index(_) | UnpackedPosition::ReverseIndex(_) => ty,
                            UnpackedPosition::Slice(_, _) => self.stdlib.list(ty).to_type(),
                        },
                        Iterable::FixedLen(ts) => {
                            match pos {
                                UnpackedPosition::Index(i) | UnpackedPosition::ReverseIndex(i) => {
                                    let idx = if matches!(pos, UnpackedPosition::Index(_)) {
                                        *i
                                    } else {
                                        ts.len() - *i
                                    };
                                    if let Some(element) = ts.get(idx) {
                                        element.clone()
                                    } else {
                                        // We'll report this error when solving for Binding::UnpackedLength.
                                        Type::any_error()
                                    }
                                }
                                UnpackedPosition::Slice(i, j) => {
                                    let start = *i;
                                    let end = ts.len() - *j;
                                    if start <= ts.len() && end >= start {
                                        let elem_ty = self.unions(ts[start..end].to_vec());
                                        self.stdlib.list(elem_ty).to_type()
                                    } else {
                                        // We'll report this error when solving for Binding::UnpackedLength.
                                        Type::any_error()
                                    }
                                }
                            }
                        }
                    })
                }
                self.unions(values)
            }
            Binding::Function(idx, mut pred, class_meta) => {
                // Overloads in .pyi should not have an implementation.
                let skip_implementation = self.module_info().path().style()
                    == ModuleStyle::Interface
                    || class_meta.is_some_and(|idx| self.get_idx(idx).is_protocol());
                let def = self.get_idx(*idx);
                if def.is_overload {
                    // This function is decorated with @overload. We should warn if this function is actually called anywhere.
                    let successor = self.bindings().get(*idx).successor;
                    let ty = def.ty.clone();
                    if successor.is_none() {
                        // This is the last definition in the chain. We should produce an overload type.
                        let mut acc = Vec1::new(ty);
                        let mut first = def;
                        while let Some(def) = self.step_overload_pred(&mut pred) {
                            acc.push(def.ty.clone());
                            first = def;
                        }
                        if !skip_implementation {
                            self.error(
                                errors,
                                first.id_range,
                                "Overloaded function must have an implementation".to_owned(),
                            );
                        }
                        if acc.len() == 1 {
                            self.error(
                                errors,
                                first.id_range,
                                "Overloaded function needs at least two signatures".to_owned(),
                            );
                            acc.split_off_first().0
                        } else {
                            acc.reverse();
                            Type::Overload(acc)
                        }
                    } else {
                        ty
                    }
                } else {
                    let mut acc = Vec::new();
                    let mut first = def;
                    while let Some(def) = self.step_overload_pred(&mut pred) {
                        acc.push(def.ty.clone());
                        first = def;
                    }
                    acc.reverse();
                    if let Ok(defs) = Vec1::try_from_vec(acc) {
                        if defs.len() == 1 {
                            self.error(
                                errors,
                                first.id_range,
                                "Overloaded function needs at least two signatures".to_owned(),
                            );
                            defs.split_off_first().0
                        } else {
                            Type::Overload(defs)
                        }
                    } else {
                        first.ty.clone()
                    }
                }
            }
            Binding::Import(m, name) => self
                .get_from_module(*m, &KeyExport(name.clone()))
                .arc_clone(),
            Binding::ClassDef(x, decorators) => match &self.get_idx(*x).0 {
                None => Type::any_implicit(),
                Some(cls) => {
                    let mut ty = Type::ClassDef(cls.clone());
                    for x in decorators.iter().rev() {
                        ty = self.apply_decorator(*x, ty, &mut false, errors)
                    }
                    ty
                }
            },
            Binding::SelfType(k) => match &self.get_idx(*k).0 {
                None => Type::any_implicit(),
                Some(cls) => cls.self_type(),
            },
            Binding::Forward(k) => self.get_idx(*k).arc_clone(),
            Binding::Phi(ks) => {
                if ks.len() == 1 {
                    self.get_idx(*ks.first().unwrap()).arc_clone()
                } else {
                    self.unions(
                        ks.iter()
                            .map(|k| self.get_idx(*k).arc_clone())
                            .collect::<Vec<_>>(),
                    )
                }
            }
            Binding::Narrow(k, op) => self.narrow(&self.get_idx(*k), op, errors),
            Binding::AnnotatedType(ann, val) => match &self.get_idx(*ann).ty {
                Some(ty) => ty.clone(),
                None => self.solve_binding_inner(val, errors),
            },
            Binding::AnyType(x) => Type::Any(*x),
            Binding::StrType => self.stdlib.str().to_type(),
            Binding::TypeParameter(q) => Type::type_form(q.to_type()),
            Binding::Module(m, path, prev) => {
                let prev = prev
                    .as_ref()
                    .and_then(|x| self.get_idx(*x).as_module().cloned());
                match prev {
                    Some(prev) if prev.path() == path => prev.add_module(*m).to_type(),
                    _ => {
                        if path.len() == 1 {
                            Type::Module(Module::new(
                                path[0].clone(),
                                OrderedSet::from_iter([(*m)]),
                            ))
                        } else {
                            assert_eq!(&m.components(), path);
                            Type::Module(Module::new_as(*m))
                        }
                    }
                }
            }
            Binding::CheckLegacyTypeParam(key, range_if_scoped_params_exist) => {
                match &*self.get_idx(*key) {
                    LegacyTypeParameterLookup::Parameter(p) => {
                        // This class or function has scoped (PEP 695) type parameters. Mixing legacy-style parameters is an error.
                        if let Some(r) = range_if_scoped_params_exist {
                            self.error(
                                errors,
                                *r,
                                format!(
                                    "Type parameter {} is not included in the type parameter list",
                                    self.module_info()
                                        .display(&self.bindings().idx_to_key(*key).0)
                                ),
                            );
                        }
                        Type::type_form(p.quantified.to_type())
                    }
                    LegacyTypeParameterLookup::NotParameter(ty) => ty.clone(),
                }
            }
            Binding::NameAssign(name, annot_key, expr) => {
                let annot = annot_key.map(|k| self.get_idx(k));
                let ty = self.expr(expr, annot.as_ref().and_then(|x| x.ty.as_ref()), errors);
                let expr_range = expr.range();
                match (annot, &ty) {
                    (Some(annot), _) if annot.qualifiers.contains(&Qualifier::TypeAlias) => self
                        .as_type_alias(
                            name,
                            TypeAliasStyle::LegacyExplicit,
                            ty,
                            expr_range,
                            errors,
                        ),
                    // TODO(stroxler, rechen): Do we want to include Type::ClassDef(_)
                    // when there is no annotation, so that `mylist = list` is treated
                    // like a value assignment rather than a type alias?
                    (None, Type::Type(_)) => self.as_type_alias(
                        name,
                        TypeAliasStyle::LegacyImplicit,
                        ty,
                        expr_range,
                        errors,
                    ),
                    _ => ty,
                }
            }
            Binding::ScopedTypeAlias(name, params, expr) => {
                let ty = self.expr(expr, None, errors);
                let expr_range = expr.range();
                let ta = self.as_type_alias(name, TypeAliasStyle::Scoped, ty, expr_range, errors);
                match ta {
                    Type::Forall(..) => self.error(
                        errors,
                        expr.range(),
                        format!("Type parameters used in `{name}` but not declared"),
                    ),
                    Type::TypeAlias(_) => {
                        let params_range = params.as_ref().map_or(expr_range, |x| x.range);
                        ta.forall(self.type_params(
                            params_range,
                            self.scoped_type_params(params.as_ref(), errors),
                            errors,
                        ))
                    }
                    _ => ta,
                }
            }
            Binding::PatternMatchMapping(mapping_key, binding_key) => {
                // TODO: check that value is a mapping
                // TODO: check against duplicate keys (optional)
                let key_ty = self.expr(mapping_key, None, errors);
                let binding_ty = self.get_idx(*binding_key).arc_clone();
                let arg = CallArg::Type(&key_ty, mapping_key.range());
                self.call_method_or_error(
                    &binding_ty,
                    &dunder::GETITEM,
                    mapping_key.range(),
                    &[arg],
                    &[],
                    errors,
                )
            }
            Binding::PatternMatchClassPositional(_, idx, key, range) => {
                // TODO: check that value matches class
                // TODO: check against duplicate keys (optional)
                let binding_ty = self.get_idx(*key).arc_clone();
                let match_args = self.attr_infer(&binding_ty, &dunder::MATCH_ARGS, *range, errors);
                match match_args {
                    Type::Tuple(Tuple::Concrete(ts)) => {
                        if *idx < ts.len() {
                            if let Some(Type::Literal(Lit::String(box attr_name))) = ts.get(*idx) {
                                self.attr_infer(&binding_ty, &Name::new(attr_name), *range, errors)
                            } else {
                                self.error(
                                    errors,
                                    *range,
                                    format!(
                                        "Expected literal string in `__match_args__`, got {}",
                                        ts[*idx]
                                    ),
                                )
                            }
                        } else {
                            self.error(
                                errors,
                                *range,
                                format!("Index {idx} out of range for `__match_args__`"),
                            )
                        }
                    }
                    Type::Any(AnyStyle::Error) => match_args,
                    _ => self.error(
                        errors,
                        *range,
                        format!(
                            "Expected concrete tuple for __match_args__, got {}",
                            match_args
                        ),
                    ),
                }
            }
            Binding::PatternMatchClassKeyword(_, attr, key) => {
                // TODO: check that value matches class
                // TODO: check against duplicate keys (optional)
                let binding_ty = self.get_idx(*key).arc_clone();
                self.attr_infer(&binding_ty, &attr.id, attr.range, errors)
            }
            Binding::Decorator(expr) => self.expr_infer(expr, errors),
            Binding::LambdaParameter(var) => var.to_type(),
            Binding::FunctionParameter(param) => {
                match param {
                    Either::Left(key) => self.get_idx(*key).ty.clone().unwrap_or_else(|| {
                        // This annotation isn't valid. It's something like `: Final` that doesn't
                        // have enough information to create a real type.
                        Type::any_implicit()
                    }),
                    Either::Right((var, function_idx)) => {
                        // Force the function binding to be evaluated, if it hasn't already.
                        // Solving the function will also force the Var type to some concrete type,
                        // and this must happen first so the Var can not interact with other types.
                        self.get_idx(*function_idx);
                        var.to_type()
                    }
                }
            }
            Binding::SuperInstance(style, range) => {
                match style {
                    SuperStyle::ExplicitArgs(cls_binding, obj_binding) => {
                        match &*self.get_idx(*cls_binding) {
                            Type::Any(style) => style.propagate(),
                            cls_type @ Type::ClassDef(cls) => {
                                match &*self.get_idx(*obj_binding) {
                                    Type::Any(style) => style.propagate(),
                                    Type::ClassType(obj_cls) => {
                                        // Get the class that attribute lookup on the super instance should be done on.
                                        // This is the class above `cls` in `obj_cls`'s MRO.
                                        let mut lookup_cls = None;
                                        let metadata = self.get_metadata_for_class(obj_cls.class_object());
                                        let mut found = false;
                                        for ancestor in
                                            [obj_cls].into_iter().chain(metadata.ancestors(self.stdlib))
                                        {
                                            if ancestor.class_object() == cls {
                                                found = true;
                                                // Handle the corner case of `ancestor` being `object` (and
                                                // therefore having no ancestor of its own).
                                                lookup_cls = Some(ancestor);
                                            } else if found {
                                                lookup_cls = Some(ancestor);
                                                break;
                                            }
                                        }
                                        lookup_cls.map_or_else(|| {
                                            self.error(
                                                errors,
                                                *range,
                                                format!("Illegal `super({}, {})` call: `{}` is not an instance or subclass of `{}`", cls_type, obj_cls, obj_cls, cls_type)
                                            )
                                        }, |lookup_cls| Type::SuperInstance(Box::new(lookup_cls.clone()), Box::new(obj_cls.clone())))
                                    }
                                    t => {
                                        // TODO: handle the case when the second argument is a class
                                        self.error(errors, *range, format!("Expected second argument to `super` to be a class instance, got `{}`", t.clone().deterministic_printing()))
                                    }
                                }
                            }
                            t => self.error(
                                errors,
                                *range,
                                format!(
                                    "Expected first argument to `super` to be a class object, got `{}`",
                                    t.clone().deterministic_printing()
                                ),
                            ),
                        }
                    }
                }
            }
        }
    }

    pub fn solve_function(
        &self,
        x: &FunctionBinding,
        errors: &ErrorCollector,
    ) -> Arc<DecoratedFunction> {
        let check_default = |default: &Option<Box<Expr>>, ty: &Type| {
            let mut required = Required::Required;
            if let Some(default) = default {
                required = Required::Optional;
                if x.kind != FunctionKind::Stub
                    || !matches!(default.as_ref(), Expr::EllipsisLiteral(_))
                {
                    self.expr(default, Some(ty), errors);
                }
            }
            required
        };

        let mut self_type = if x.def.name.id == dunder::NEW {
            // __new__ is a staticmethod, and does not take a self parameter.
            None
        } else {
            x.self_type.map(|idx| self.get_idx(idx).arc_clone())
        };

        // Look for a @classmethod or @staticmethod decorator and change the "self" type
        // accordingly. This is not totally correct, since it doesn't account for chaining
        // decorators, or weird cases like both decorators existing at the same time.
        for x in &x.decorators {
            match self.get_idx(*x).callee_kind() {
                Some(CalleeKind::Class(ClassKind::StaticMethod)) => {
                    self_type = None;
                    break;
                }
                Some(CalleeKind::Class(ClassKind::ClassMethod)) => {
                    self_type = self_type.map(|ty| Type::Type(Box::new(ty)));
                    break;
                }
                _ => {}
            }
        }

        let mut get_param_ty = |name: &Identifier| {
            let ty = match self.bindings().get_function_param(name) {
                Either::Left(idx) => self.get_idx(idx).get_type().clone(),
                Either::Right(var) => {
                    // If this is the first parameter and there is a self type, solve to `Self`.
                    // We only try to solve the first param for now. Other unannotated params
                    // are also Var, but will always be forced to Any. In the future, we might
                    // consider contextual information to infer parameter types, like decorator
                    // applications.
                    if let Some(ty) = &self_type {
                        self.solver()
                            .is_subset_eq(&var.to_type(), ty, self.type_order());
                    }
                    self.solver().force_var(var)
                }
            };
            self_type = None; // Stop using `self` type solve Var params after the first param.
            ty
        };
        let mut params = Vec::with_capacity(x.def.parameters.len());
        params.extend(x.def.parameters.posonlyargs.iter().map(|x| {
            let ty = get_param_ty(&x.parameter.name);
            let required = check_default(&x.default, &ty);
            Param::PosOnly(ty, required)
        }));
        params.extend(x.def.parameters.args.iter().map(|x| {
            let ty = get_param_ty(&x.parameter.name);
            let required = check_default(&x.default, &ty);
            Param::Pos(x.parameter.name.id.clone(), ty, required)
        }));
        params.extend(x.def.parameters.vararg.iter().map(|x| {
            let ty = get_param_ty(&x.name);
            Param::VarArg(ty)
        }));
        params.extend(x.def.parameters.kwonlyargs.iter().map(|x| {
            let ty = get_param_ty(&x.parameter.name);
            let required = check_default(&x.default, &ty);
            Param::KwOnly(x.parameter.name.id.clone(), ty, required)
        }));
        params.extend(x.def.parameters.kwarg.iter().map(|x| {
            let ty = match self.bindings().get_function_param(&x.name) {
                Either::Left(idx) => {
                    let annot = self.get_idx(idx);
                    let ty = annot.get_type().clone();
                    if annot.qualifiers.contains(&Qualifier::Unpack) {
                        Type::Unpack(Box::new(ty))
                    } else {
                        ty
                    }
                }
                Either::Right(var) => self.solver().force_var(var),
            };
            Param::Kwargs(ty)
        }));
        let ret = self
            .get(&Key::ReturnType(ShortIdentifier::new(&x.def.name)))
            .arc_clone();

        let ret = if x.def.is_async && !self.is_async_generator(&ret) {
            self.stdlib
                .coroutine(Type::any_implicit(), Type::any_implicit(), ret)
                .to_type()
        } else {
            ret
        };

        let mut tparams = self.scoped_type_params(x.def.type_params.as_deref(), errors);
        let legacy_tparams = x
            .legacy_tparams
            .iter()
            .filter_map(|key| self.get_idx(*key).deref().parameter().cloned());
        tparams.extend(legacy_tparams);
        let callable = Type::Callable(
            Box::new(Callable::make(params, ret)),
            CallableKind::from_name(self.module_info().name(), &x.def.name.id),
        );
        let mut ty = callable.forall(self.type_params(x.def.range, tparams, errors));
        let mut is_overload = false;
        for x in x.decorators.iter().rev() {
            ty = self.apply_decorator(*x, ty, &mut is_overload, errors)
        }
        Arc::new(DecoratedFunction {
            id_range: x.def.name.range,
            ty,
            is_overload,
        })
    }

    // Given the index to a function binding, return the previous function binding, if any.
    fn step_overload_pred(&self, pred: &mut Option<Idx<Key>>) -> Option<Arc<DecoratedFunction>> {
        let pred_idx = (*pred)?;
        let mut b = self.bindings().get(pred_idx);
        while let Binding::Forward(k) = b {
            b = self.bindings().get(*k);
        }
        if let Binding::Function(idx, pred_, _) = b {
            let def = self.get_idx(*idx);
            if def.is_overload {
                *pred = *pred_;
                Some(def)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn solve_yield(&self, x: &BindingYield, errors: &ErrorCollector) -> Arc<YieldResult> {
        match x {
            BindingYield::Yield(annot, x) => {
                // TODO: Keep track of whether the function is async in the binding, decompose hint
                // appropriately instead of just trying both.
                let annot = annot.map(|k| self.get_idx(k));
                let hint = annot.as_ref().and_then(|x| x.ty.as_ref()).and_then(|ty| {
                    if let Some((yield_ty, send_ty, _)) = self.decompose_generator(ty) {
                        Some((yield_ty, send_ty))
                    } else {
                        self.decompose_async_generator(ty)
                    }
                });
                if let Some((yield_hint, send_ty)) = hint {
                    let yield_ty = if let Some(expr) = x.value.as_ref() {
                        self.expr(expr, Some(&yield_hint), errors)
                    } else {
                        self.check_type(&yield_hint, &Type::None, x.range, errors)
                    };
                    Arc::new(YieldResult { yield_ty, send_ty })
                } else {
                    let yield_ty = if let Some(expr) = x.value.as_ref() {
                        self.expr(expr, None, errors)
                    } else {
                        Type::None
                    };
                    let send_ty = Type::any_implicit();
                    Arc::new(YieldResult { yield_ty, send_ty })
                }
            }
            BindingYield::Invalid(x) => {
                if let Some(expr) = x.value.as_ref() {
                    self.expr(expr, None, errors);
                }
                self.error(
                    errors,
                    x.range,
                    "Invalid `yield` outside of a function".to_owned(),
                );
                Arc::new(YieldResult::any_error())
            }
        }
    }

    pub fn solve_yield_from(
        &self,
        x: &BindingYieldFrom,
        errors: &ErrorCollector,
    ) -> Arc<YieldFromResult> {
        match x {
            BindingYieldFrom::YieldFrom(annot, x) => {
                // TODO: Error if the function is async
                let annot = annot.map(|k| self.get_idx(k));
                let want = annot.as_ref().and_then(|x| x.ty.as_ref());

                let mut ty = self.expr(&x.value, None, errors);
                let res = if let Some(generator) = self.unwrap_generator(&ty) {
                    YieldFromResult::from_generator(generator)
                } else if let Some(yield_ty) = self.unwrap_iterable(&ty) {
                    // Promote the type to a generator for the check below to succeed.
                    // Per PEP-380, if None is sent to the delegating generator, the
                    // iterator's __next__() method is called, so promote to a generator
                    // with a `None` send type.
                    // TODO: This might cause confusing type errors.
                    ty = self
                        .stdlib
                        .generator(yield_ty.clone(), Type::None, Type::None)
                        .to_type();
                    YieldFromResult::from_iterable(yield_ty)
                } else {
                    ty = self.error(
                        errors,
                        x.range,
                        format!("yield from value must be iterable, got `{ty}`"),
                    );
                    YieldFromResult::any_error()
                };
                if let Some(want) = want {
                    self.check_type(want, &ty, x.range, errors);
                }
                Arc::new(res)
            }
            BindingYieldFrom::Invalid(x) => {
                self.expr(&x.value, None, errors);
                self.error(
                    errors,
                    x.range,
                    "Invalid `yield from` outside of a function".to_owned(),
                );
                Arc::new(YieldFromResult::any_error())
            }
        }
    }

    /// Unwraps a type, originally evaluated as a value, so that it can be used as a type annotation.
    /// For example, in `def f(x: int): ...`, we evaluate `int` as a value, gettings its type as
    /// `type[int]`, then call `untype(type[int])` to get the `int` annotation.
    fn untype(&self, ty: Type, range: TextRange, errors: &ErrorCollector) -> Type {
        if let Some(t) = self.untype_opt(ty.clone(), range) {
            t
        } else {
            self.error(
                errors,
                range,
                format!(
                    "Expected a type form, got instance of `{}`",
                    ty.deterministic_printing()
                ),
            )
        }
    }

    pub fn untype_opt(&self, ty: Type, range: TextRange) -> Option<Type> {
        match self.canonicalize_all_class_types(ty, range) {
            Type::Union(xs) if !xs.is_empty() => {
                let mut ts = Vec::new();
                for x in xs.into_iter() {
                    let t = self.untype_opt(x, range)?;
                    ts.push(t);
                }
                Some(self.unions(ts))
            }
            Type::Var(v) if let Some(_guard) = self.recurser.recurse(v) => {
                self.untype_opt(self.solver().force_var(v), range)
            }
            Type::Type(box t) => Some(t),
            Type::None => Some(Type::None), // Both a value and a type
            Type::Ellipsis => Some(Type::Ellipsis), // A bit weird because of tuples, so just promote it
            Type::Any(style) => Some(style.propagate()),
            Type::TypeAlias(ta) => self.untype_opt(ta.as_type(), range),
            t @ Type::Unpack(
                box Type::Tuple(_) | box Type::TypeVarTuple(_) | box Type::Quantified(_),
            ) => Some(t),
            Type::Unpack(box Type::Var(v)) if let Some(_guard) = self.recurser.recurse(v) => {
                self.untype_opt(Type::Unpack(Box::new(self.solver().force_var(v))), range)
            }
            Type::Unpack(box t) => Some(t),
            _ => None,
        }
    }

    pub fn expr_untype(&self, x: &Expr, errors: &ErrorCollector) -> Type {
        match x {
            // TODO: this is only valid in a type argument list for generics or Callable, not as a standalone annotation
            Expr::List(x) => {
                let elts: Vec<Param> = x
                    .elts
                    .iter()
                    .map(|x| Param::PosOnly(self.expr_untype(x, errors), Required::Required))
                    .collect();
                Type::ParamSpecValue(ParamList::new(elts))
            }
            _ => self.untype(self.expr(x, None, errors), x.range(), errors),
        }
    }
}
