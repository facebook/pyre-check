/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::TypeParam;
use ruff_python_ast::TypeParams;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::ordered_set::OrderedSet;
use starlark_map::small_map::Entry;
use starlark_map::small_map::SmallMap;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::answers::UNKNOWN;
use crate::alt::callable::CallArg;
use crate::alt::class::classdef::ClassField;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::alt::types::function_answer::FunctionAnswer;
use crate::alt::types::legacy_lookup::LegacyTypeParameterLookup;
use crate::ast::Ast;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::ContextManagerKind;
use crate::binding::binding::EmptyAnswer;
use crate::binding::binding::FunctionBinding;
use crate::binding::binding::FunctionKind;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyExport;
use crate::binding::binding::RaisedException;
use crate::binding::binding::SizeExpectation;
use crate::binding::binding::UnpackedPosition;
use crate::dunder;
use crate::dunder::inplace_dunder;
use crate::error::collector::ErrorCollector;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Annotation;
use crate::types::annotation::Qualifier;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::Param;
use crate::types::callable::Required;
use crate::types::literal::Lit;
use crate::types::module::Module;
use crate::types::quantified::Quantified;
use crate::types::tuple::Tuple;
use crate::types::type_var::Restriction;
use crate::types::type_var::TypeVar;
use crate::types::type_var::Variance;
use crate::types::types::AnyStyle;
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
        let cls = self.get_idx_class_def(*k).unwrap();
        Arc::new(self.class_metadata_of(&cls, bases, keywords, decorators, errors))
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
            BindingAnnotation::Forward(k) => {
                Arc::new(Annotation::new_type(self.get_idx(*k).arc_clone()))
            }
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
        let iterate_by_interface = || {
            let iteration_ty = if let Some(iterator_ty) =
                self.call_method(iterable, &dunder::ITER, range, &[], &[], errors)
            {
                Some(self.call_method_or_error(
                    &iterator_ty,
                    &dunder::NEXT,
                    range,
                    &[],
                    &[],
                    errors,
                ))
            } else {
                let int_ty = self.stdlib.int().to_type();
                let arg = CallArg::Type(&int_ty, range);
                self.call_method(iterable, &dunder::GETITEM, range, &[arg], &[], errors)
            };
            Iterable::OfType(iteration_ty.unwrap_or_else(|| {
                self.error(
                    errors,
                    range,
                    format!(
                        "Type `{}` is not iterable",
                        iterable.clone().deterministic_printing()
                    ),
                )
            }))
        };
        match iterable {
            Type::ClassType(_) | Type::Type(box Type::ClassType(_)) => {
                vec![iterate_by_interface()]
            }
            Type::ClassDef(cls) => {
                if self.get_metadata_for_class(cls).is_typed_dict() {
                    vec![Iterable::OfType(self.error(
                        errors,
                        range,
                        format!(
                            "Type `{}` (a `TypedDict` class object) is not iterable",
                            iterable.clone().deterministic_printing()
                        ),
                    ))]
                } else {
                    vec![iterate_by_interface()]
                }
            }
            Type::TypedDict(_) => self.iterate(
                &self
                    .stdlib
                    .mapping(
                        self.stdlib.str().to_type(),
                        self.stdlib.object_class_type().clone().to_type(),
                    )
                    .to_type(),
                range,
                errors,
            ),
            Type::Tuple(Tuple::Concrete(elts)) => vec![Iterable::FixedLen(elts.clone())],
            Type::Tuple(Tuple::Unbounded(box elt)) => vec![Iterable::OfType(elt.clone())],
            Type::LiteralString => vec![Iterable::OfType(self.stdlib.str().to_type())],
            Type::Literal(lit) if lit.is_string() => {
                vec![Iterable::OfType(self.stdlib.str().to_type())]
            }
            Type::Any(_) => vec![Iterable::OfType(Type::any_implicit())],
            Type::Var(v) if let Some(_guard) = self.recurser.recurse(*v) => {
                self.iterate(&self.solver().force_var(*v), range, errors)
            }
            Type::Union(ts) => ts
                .iter()
                .flat_map(|t| self.iterate(t, range, errors))
                .collect(),
            _ => vec![Iterable::OfType(self.error(
                errors,
                range,
                format!(
                    "`{}` is not iterable",
                    iterable.clone().deterministic_printing()
                ),
            ))],
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
        let expected_types = if let base_exception_type @ Type::ClassType(c) =
            &self.stdlib.base_exception().to_type()
        {
            let base_exception_class_type = Type::ClassDef(c.class_object().dupe());
            vec![base_exception_type.clone(), base_exception_class_type]
        } else {
            unreachable!("The stdlib base exception type should be a ClassInstance")
        };
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

    pub fn solve_class_field(
        &self,
        field: &BindingClassField,
        errors: &ErrorCollector,
    ) -> Arc<ClassField> {
        let value_ty = self.solve_binding(&field.value, errors);
        let annotation = field.annotation.map(|a| self.get_idx(a));
        Arc::new(self.calculate_class_field(
            &field.name,
            value_ty.as_ref(),
            annotation.as_deref(),
            field.initialization,
            &self.get_idx_class_def(field.class).unwrap(),
            field.range,
            errors,
        ))
    }

    pub fn solve_class_synthesized_fields(
        &self,
        fields: &BindingClassSynthesizedFields,
    ) -> Arc<ClassSynthesizedFields> {
        let cls = self.get_idx_class_def(fields.0).unwrap();
        if let Some(fields) = self.get_typed_dict_synthesized_fields(&cls) {
            Arc::new(fields)
        } else if let Some(fields) = self.get_enum_synthesized_fields(&cls) {
            Arc::new(fields)
        } else if let Some(fields) = self.get_dataclass_synthesized_fields(&cls) {
            Arc::new(fields)
        } else {
            Arc::new(ClassSynthesizedFields::default())
        }
    }

    fn solve_binding_inner(&self, binding: &Binding, errors: &ErrorCollector) -> Type {
        match binding {
            Binding::Expr(ann, e) => {
                let ty = ann.map(|k| self.get_idx(k));
                self.expr(e, ty.as_ref().and_then(|x| x.ty.as_ref()), errors)
            }
            Binding::Generator(yield_type, return_type) => {
                let yield_type = self.solve_binding_inner(yield_type, errors);
                let return_type = self.solve_binding_inner(return_type, errors);
                self.stdlib
                    .generator(yield_type, Type::any_implicit(), return_type)
                    .to_type()
            }
            Binding::AsyncGenerator(yield_type) => {
                let yield_type = self.solve_binding_inner(yield_type, errors);
                self.stdlib
                    .async_generator(yield_type, Type::any_implicit())
                    .to_type()
            }
            Binding::SendTypeOfYieldAnnotation(ann, range) => {
                let gen_ann: Option<Arc<Annotation>> = ann.map(|k| self.get_idx(k));
                match gen_ann {
                    Some(gen_ann) => {
                        let gen_type = gen_ann.get_type();
                        // try to interpret the annotation as a generator
                        if let Some((_, send_type, _)) = self.decompose_generator(gen_type) {
                            send_type
                        }
                        // try to interpret the annotation as an async generator
                        else if let Some((_, send_type)) =
                            self.decompose_async_generator(gen_type)
                        {
                            send_type
                        } else {
                            self.error(errors, *range, format!("Yield expression found but the function has an incompatible annotation `{gen_type}`"))
                        }
                    }

                    None => Type::any_explicit(),
                }
            }
            Binding::YieldTypeOfYieldAnnotation(ann, range, is_async) => {
                let gen_ann: Option<Arc<Annotation>> = ann.map(|k| self.get_idx(k));
                match gen_ann {
                    Some(gen_ann) => {
                        let gen_type = gen_ann.get_type();
                        if let Some((yield_type, _, _)) = self.decompose_generator(gen_type) {
                            // check type annotation against async flag
                            if *is_async {
                                self.error(errors, *range, format!("Return type of generator must be compatible with `{gen_type}`"))
                            } else {
                                yield_type
                            }
                        } else if let Some((yield_type, _)) =
                            self.decompose_async_generator(gen_type)
                        {
                            // check type annotation against async flag
                            if !*is_async {
                                self.error(errors, *range, format!("Return type of generator must be compatible with `{gen_type}`"))
                            } else {
                                yield_type
                            }
                        } else {
                            self.error(errors, *range, format!("Yield expression found but the function has an incompatible annotation `{gen_type}`"))
                        }
                    }

                    None => Type::any_explicit(),
                }
            }
            Binding::ReturnTypeOfYieldAnnotation(ann, range) => {
                let gen_ann: Option<Arc<Annotation>> = ann.map(|k| self.get_idx(k));
                match gen_ann {
                    Some(gen_ann) => {
                        let gen_type = gen_ann.get_type();

                        if let Some((_, _, return_type)) = self.decompose_generator(gen_type) {
                            return_type
                        } else {
                            self.error(errors, *range, format!("YieldFrom expression found but the function has an incompatible annotation `{gen_type}`"))
                        }
                    }

                    None => Type::any_explicit(),
                }
            }
            Binding::YieldTypeOfYield(x) => match &x.value {
                Some(x) => self.expr(x, None, errors),
                None => Type::None,
            },
            Binding::YieldTypeOfYieldFrom(x) => {
                let gen_type = self.expr(&x.value, None, errors);
                self.decompose_generator(&gen_type)
                    .map_or(Type::any_implicit(), |(y, _, _)| y)
            }
            Binding::AsyncReturnType(x) => {
                let expr_type = self.expr(&x.0, None, errors);
                let return_type = self.solve_binding_inner(&x.1, errors);

                if self.is_async_generator(&return_type) && !expr_type.is_none() {
                    self.error(errors,
                        x.0.range(),
                        format!("Return statement with type `{expr_type}` is not allowed in async generator "),
                    )
                } else {
                    return_type
                }
            }
            Binding::ReturnExpr(ann, e, has_yields) => {
                let ann: Option<Arc<Annotation>> = ann.map(|k| self.get_idx(k));
                let hint = ann.as_ref().and_then(|x| x.ty.as_ref());

                if *has_yields {
                    let hint = match hint {
                        Some(t) => {
                            let decomposed = self.decompose_generator(t);
                            decomposed.map(|(_, _, r)| r)
                        }
                        None => None,
                    };
                    self.expr(e, hint.as_ref(), errors)
                } else if matches!(hint, Some(Type::TypeGuard(_))) {
                    self.expr(e, Some(&Type::ClassType(self.stdlib.bool())), errors)
                } else {
                    self.expr(e, hint, errors)
                }
            }
            Binding::ReturnExprWithReturn(_ann, _e, _has_yields, _last_statement_types) => {
                // todo zeina: solve this binding
                Type::any_error()
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
                                Type::ClassType(self.stdlib.none_type())
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
            Binding::Function(x) => self.get_idx(*x).ty.clone(),
            Binding::Import(m, name) => self
                .get_from_module(*m, &KeyExport(name.clone()))
                .arc_clone(),
            Binding::ClassDef(x) => {
                let mut ty = Type::ClassDef(self.class_definition(
                    &x.def,
                    x.fields.clone(),
                    &x.bases,
                    &x.legacy_tparams,
                    errors,
                ));
                for x in x.decorators.iter().rev() {
                    ty = self.apply_decorator(*x, ty, errors)
                }
                ty
            }
            Binding::FunctionalClassDef(x, fields) => {
                Type::ClassDef(self.functional_class_definition(x, fields))
            }
            Binding::SelfType(k) => match &*self.get_idx(*k) {
                Type::ClassDef(c) => c.self_type(),
                _ => unreachable!(),
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
                    (None, Type::Type(box t))
                        if matches!(&**expr, Expr::Call(_))
                            && let Some(tvar) = t.as_tvar_declaration() =>
                    {
                        let tvar_name = &tvar.name.id;
                        if *name != *tvar_name && *tvar_name != UNKNOWN {
                            self.error(
                                errors,
                                expr_range,
                                format!("TypeVar must be assigned to a variable named {tvar_name}"),
                            );
                        }
                        ty
                    }
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
        }
    }

    pub fn solve_function(
        &self,
        x: &FunctionBinding,
        errors: &ErrorCollector,
    ) -> Arc<FunctionAnswer> {
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
        let mut params = Vec::with_capacity(x.def.parameters.len());
        params.extend(x.def.parameters.posonlyargs.iter().map(|x| {
            let annot = self.get(&KeyAnnotation::Annotation(ShortIdentifier::new(
                &x.parameter.name,
            )));
            let ty = annot.get_type();
            let required = check_default(&x.default, ty);
            Param::PosOnly(ty.clone(), required)
        }));
        params.extend(x.def.parameters.args.iter().map(|x| {
            let annot = self.get(&KeyAnnotation::Annotation(ShortIdentifier::new(
                &x.parameter.name,
            )));
            let ty = annot.get_type();
            let required = check_default(&x.default, ty);
            Param::Pos(x.parameter.name.id.clone(), ty.clone(), required)
        }));
        params.extend(x.def.parameters.vararg.iter().map(|x| {
            let annot = self.get(&KeyAnnotation::Annotation(ShortIdentifier::new(&x.name)));
            let ty = annot.get_type();
            Param::VarArg(ty.clone())
        }));
        params.extend(x.def.parameters.kwonlyargs.iter().map(|x| {
            let annot = self.get(&KeyAnnotation::Annotation(ShortIdentifier::new(
                &x.parameter.name,
            )));
            let ty = annot.get_type();
            let required = check_default(&x.default, ty);
            Param::KwOnly(x.parameter.name.id.clone(), ty.clone(), required)
        }));
        params.extend(x.def.parameters.kwarg.iter().map(|x| {
            let annot = self.get(&KeyAnnotation::Annotation(ShortIdentifier::new(&x.name)));
            let ty = annot.get_type();
            let is_unpack = annot.qualifiers.contains(&Qualifier::Unpack);
            Param::Kwargs(if is_unpack {
                Type::Unpack(Box::new(ty.clone()))
            } else {
                ty.clone()
            })
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
        for x in x.decorators.iter().rev() {
            ty = self.apply_decorator(*x, ty, errors)
        }
        Arc::new(FunctionAnswer {
            ty,
            is_overload: false, // TODO: detect overload when applying decorators
        })
    }

    /// Unwraps a type, originally evaluated as a value, so that it can be used as a type annotation.
    /// For example, in `def f(x: int): ...`, we evaluate `int` as a value, gettings its type as
    /// `type[int]`, then call `untype(type[int])` to get the `int` annotation.
    pub fn untype(&self, ty: Type, range: TextRange, errors: &ErrorCollector) -> Type {
        if let Some(t) = self.untype_opt(ty.clone(), range) {
            t
        } else {
            self.error(
                errors,
                range,
                format!("untype, got {}", ty.deterministic_printing()),
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
            _ => None,
        }
    }

    pub fn expr_untype(&self, x: &Expr, errors: &ErrorCollector) -> Type {
        self.untype(self.expr(x, None, errors), x.range(), errors)
    }
}
