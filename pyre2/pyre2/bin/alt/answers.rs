/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
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

use crate::alt::binding::Binding;
use crate::alt::binding::BindingAnnotation;
use crate::alt::binding::BindingBaseClass;
use crate::alt::binding::BindingLegacyTypeParam;
use crate::alt::binding::BindingMro;
use crate::alt::binding::BindingTypeParams;
use crate::alt::binding::ContextManagerKind;
use crate::alt::binding::FunctionKind;
use crate::alt::binding::Key;
use crate::alt::binding::KeyAnnotation;
use crate::alt::binding::KeyBaseClass;
use crate::alt::binding::KeyLegacyTypeParam;
use crate::alt::binding::KeyMro;
use crate::alt::binding::KeyTypeParams;
use crate::alt::binding::RaisedException;
use crate::alt::binding::SizeExpectation;
use crate::alt::binding::UnpackedPosition;
use crate::alt::bindings::BindingEntry;
use crate::alt::bindings::BindingTable;
use crate::alt::bindings::Bindings;
use crate::alt::exports::Exports;
use crate::alt::expr::TypeCallArg;
use crate::alt::table::Keyed;
use crate::alt::table::TableKeyed;
use crate::ast::Ast;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::graph::calculation::Calculation;
use crate::graph::index::Idx;
use crate::graph::index_map::IndexMap;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::solver::Solver;
use crate::table;
use crate::table_for_each;
use crate::table_mut_for_each;
use crate::table_try_for_each;
use crate::type_order::TypeOrder;
use crate::types::annotation::Annotation;
use crate::types::annotation::Qualifier;
use crate::types::base_class::BaseClass;
use crate::types::callable::Arg;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::module::Module;
use crate::types::mro::Mro;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::type_var::TypeVar;
use crate::types::types::AnyStyle;
use crate::types::types::LegacyTypeParameterLookup;
use crate::types::types::Quantified;
use crate::types::types::QuantifiedVec;
use crate::types::types::Type;
use crate::types::types::TypeAlias;
use crate::types::types::TypeAliasStyle;
use crate::types::types::Var;
use crate::uniques::UniqueFactory;
use crate::util::display::DisplayWith;
use crate::util::recurser::Recurser;

/// Invariants:
///
/// * Every module name referenced anywhere MUST be present
///   in the `exports` and `bindings` map.
/// * Every key referenced in `bindings`/`answers` MUST be present.
///
/// We never issue contains queries on these maps.
#[derive(Debug)]
pub struct Answers<'a> {
    exports: &'a SmallMap<ModuleName, Exports>,
    bindings: &'a Bindings,
    errors: &'a ErrorCollector,
    table: AnswerTable,
}

type AnswerEntry<K> = IndexMap<K, Calculation<Arc<<K as Keyed>::Answer>, <K as Solve>::Recursive>>;

table!(
    #[derive(Debug, Default)]
    pub struct AnswerTable(AnswerEntry)
);

impl<'a> Display for Answers<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn go<'a, K: Solve>(
            answers: &Answers<'a>,
            entry: &AnswerEntry<K>,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result
        where
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            for (idx, answer) in entry.iter() {
                let key = answers.bindings.idx_to_key(idx);
                let value = answers.bindings.get(idx);
                writeln!(
                    f,
                    "{} = {} = {}",
                    key,
                    value.display_with(answers.bindings),
                    match answer.get() {
                        Some(v) => v.to_string(),
                        None => "(unsolved)".to_owned(),
                    },
                )?;
            }
            Ok(())
        }

        table_try_for_each!(self.table, |x| go(self, x, f));
        Ok(())
    }
}

pub type SolutionsEntry<K> = IndexMap<K, <K as Keyed>::Answer>;

table!(
    #[derive(Default, Debug, Clone)]
    pub struct Solutions(pub SolutionsEntry)
);

#[derive(Debug, Clone)]
pub struct AnswersSolver<'a> {
    answers: &'a SmallMap<ModuleName, Answers<'a>>,
    current: &'a Answers<'a>,
    uniques: &'a UniqueFactory,
    pub recurser: &'a Recurser<Var>,
    pub solver: &'a Solver<'a>,
    pub stdlib: &'a Stdlib,
}

pub trait Solve: Keyed {
    type Recursive: Dupe = ();

    fn solve(answers: &AnswersSolver, binding: &Self::Value) -> Arc<Self::Answer>;

    fn recursive(answers: &AnswersSolver) -> Self::Recursive;

    fn record_recursive(
        _answers: &AnswersSolver,
        _key: &Self,
        _answer: Arc<Self::Answer>,
        _recursive: Self::Recursive,
    ) {
    }

    fn promote_recursive(x: Self::Recursive) -> Self::Answer;

    fn visit_type_mut(v: &mut Self::Answer, f: &mut dyn FnMut(&mut Type));
}

impl Solve for Key {
    type Recursive = Var;

    fn solve(answers: &AnswersSolver, binding: &Binding) -> Arc<Type> {
        answers.solve_binding(binding)
    }

    fn recursive(answers: &AnswersSolver) -> Self::Recursive {
        answers.solver.fresh_recursive()
    }

    fn record_recursive(answers: &AnswersSolver, key: &Key, answer: Arc<Type>, recursive: Var) {
        answers.record_recursive(key, answer, recursive);
    }

    fn promote_recursive(x: Self::Recursive) -> Self::Answer {
        Type::Var(x)
    }

    fn visit_type_mut(v: &mut Type, f: &mut dyn FnMut(&mut Type)) {
        f(v);
    }
}

impl Solve for KeyAnnotation {
    fn solve(answers: &AnswersSolver, binding: &BindingAnnotation) -> Arc<Annotation> {
        answers.solve_annotation(binding)
    }

    fn recursive(_answers: &AnswersSolver) -> Self::Recursive {}

    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        Annotation::default()
    }

    fn visit_type_mut(v: &mut Annotation, f: &mut dyn FnMut(&mut Type)) {
        v.ty.iter_mut().for_each(f);
    }
}

impl Solve for KeyBaseClass {
    fn solve(answers: &AnswersSolver, binding: &BindingBaseClass) -> Arc<BaseClass> {
        answers.solve_base_class(binding)
    }

    fn recursive(_answers: &AnswersSolver) -> Self::Recursive {}

    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        BaseClass::Type(Type::any_implicit())
    }

    fn visit_type_mut(v: &mut BaseClass, f: &mut dyn FnMut(&mut Type)) {
        v.visit_mut(f);
    }
}

impl Solve for KeyMro {
    fn solve(answers: &AnswersSolver, binding: &BindingMro) -> Arc<Mro> {
        answers.solve_mro(binding)
    }

    fn recursive(_answers: &AnswersSolver) -> Self::Recursive {}

    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        Mro::cyclic()
    }

    fn visit_type_mut(v: &mut Mro, f: &mut dyn FnMut(&mut Type)) {
        v.visit_mut(f);
    }
}

impl Solve for KeyLegacyTypeParam {
    fn solve(
        answers: &AnswersSolver,
        binding: &BindingLegacyTypeParam,
    ) -> Arc<LegacyTypeParameterLookup> {
        answers.solve_legacy_tparam(binding)
    }

    fn recursive(_answers: &AnswersSolver) -> Self::Recursive {}

    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        LegacyTypeParameterLookup::NotParameter(Type::any_implicit())
    }

    fn visit_type_mut(v: &mut LegacyTypeParameterLookup, f: &mut dyn FnMut(&mut Type)) {
        v.not_parameter_mut().into_iter().for_each(f);
    }
}

impl Solve for KeyTypeParams {
    fn solve(answers: &AnswersSolver, binding: &BindingTypeParams) -> Arc<QuantifiedVec> {
        answers.solve_tparams(binding)
    }

    fn recursive(_answers: &AnswersSolver) -> Self::Recursive {}

    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        QuantifiedVec(Vec::new())
    }

    fn visit_type_mut(_: &mut Self::Answer, _: &mut dyn FnMut(&mut Type)) {
        // There are no types in the answer
    }
}

impl<'a> Answers<'a> {
    pub fn new(
        exports: &'a SmallMap<ModuleName, Exports>,
        bindings: &'a Bindings,
        errors: &'a ErrorCollector,
    ) -> Self {
        fn presize<K: Solve>(items: &mut AnswerEntry<K>, bindings: &Bindings)
        where
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            let ks = bindings.keys::<K>();
            items.reserve(ks.len());
            for k in ks {
                items.insert_once(k, Calculation::new());
            }
        }
        let mut table = AnswerTable::default();
        table_mut_for_each!(&mut table, |items| presize(items, bindings));

        Self {
            exports,
            bindings,
            errors,
            table,
        }
    }

    pub fn len(&self) -> usize {
        let mut res = 0;
        table_for_each!(&self.table, |x: &AnswerEntry<_>| res += x.len());
        res
    }

    pub fn solve(
        &self,
        answers: &SmallMap<ModuleName, Answers>,
        stdlib: &Stdlib,
        uniques: &'a UniqueFactory,
        solver: &'a Solver<'a>,
    ) -> Solutions {
        let mut res = Solutions::default();

        fn pre_solve<K: Solve>(items: &mut SolutionsEntry<K>, answers: &AnswersSolver)
        where
            AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            items.reserve(answers.current.bindings.keys::<K>().len());
            for idx in answers.current.bindings.keys::<K>() {
                let k = answers.current.bindings.idx_to_key(idx);
                let v = Arc::unwrap_or_clone(answers.get(k));
                items.insert_once(idx, v);
            }
        }
        let answers_solver = AnswersSolver {
            stdlib,
            answers,
            uniques,
            solver,
            recurser: &Recurser::new(),
            current: self,
        };
        table_mut_for_each!(&mut res, |items| pre_solve(items, &answers_solver));

        // Now force all types to be fully resolved.
        fn post_solve<K: Solve>(items: &mut SolutionsEntry<K>, solver: &Solver) {
            for v in items.values_mut() {
                K::visit_type_mut(v, &mut |x| solver.deep_force_mut(x));
            }
        }
        table_mut_for_each!(&mut res, |items| post_solve(items, solver));
        res
    }

    /// Resolve the type of global `name` in module `module`, assuming the
    /// resolution does not depend directly on the behavior of any stdlib types.
    /// This is used exclusively to bootstrap stdlib support.
    pub fn lookup_class_without_stdlib(
        &self,
        module: ModuleName,
        name: &Name,
        answers: &SmallMap<ModuleName, Answers<'_>>,
        uniques: &'a UniqueFactory,
        solver: &'a Solver<'a>,
    ) -> Option<Class> {
        let solver = AnswersSolver {
            stdlib: &Stdlib::for_bootstrapping(),
            uniques,
            solver,
            answers,
            recurser: &Recurser::new(),
            current: self,
        };
        match solver.get_import(name, module, TextRange::default()) {
            Type::ClassDef(cls) => Some(cls),
            ty => {
                tracing::error!(
                    "Did not expect non-class type `{ty}` for stdlib import `{module}.{name}`"
                );
                None
            }
        }
    }
}

enum Iterable {
    OfType(Type),
    FixedLen(Vec<Type>),
}

impl<'a> AnswersSolver<'a> {
    pub fn bindings(&self) -> &Bindings {
        self.current.bindings
    }

    pub fn errors(&self) -> &ErrorCollector {
        self.current.errors
    }

    pub fn module_info(&self) -> &ModuleInfo {
        self.current.bindings.module_info()
    }

    pub fn with_module(&self, name: ModuleName) -> Self {
        let current = self.answers.get(&name).unwrap();
        AnswersSolver {
            current,
            ..self.clone()
        }
    }

    pub fn type_order(&self) -> TypeOrder {
        TypeOrder::new(self)
    }

    fn try_get_idx<K: Solve>(&self, idx: Idx<K>) -> Result<Arc<K::Answer>, K::Recursive>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        let calculation = self.current.table.get::<K>().get(idx).unwrap_or_else(|| {
            // Do not fix a panic by removing this error.
            // We should always be sure before calling `get`.
            panic!(
                "Internal error: Answer not found: {}, {}",
                self.module_info().name(),
                self.bindings().idx_to_key(idx),
            )
        });
        let result = calculation.calculate_with_recursive(
            || {
                let binding = self.bindings().get(idx);
                K::solve(self, binding)
            },
            || K::recursive(self),
        );
        if let Ok((ref v, Some(ref r))) = result {
            let k = self.bindings().idx_to_key(idx);
            K::record_recursive(self, k, v.clone(), r.clone());
        }
        result.map(|x| x.0)
    }

    fn get<K: Solve>(&self, k: &K) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.get_idx(self.bindings().key_to_idx(k))
    }

    fn get_idx<K: Solve>(&self, k: Idx<K>) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        match self.try_get_idx(k) {
            Ok(v) => v,
            Err(r) => Arc::new(K::promote_recursive(r)),
        }
    }

    fn get_annotation(&self, key: &KeyAnnotation) -> Arc<Annotation> {
        self.get(key)
    }

    fn get_annotation_idx(&self, key: Idx<KeyAnnotation>) -> Arc<Annotation> {
        self.get_idx(key)
    }

    fn record_recursive(&self, key: &Key, answer: Arc<Type>, recursive: Var) {
        self.solver.record_recursive(
            recursive,
            answer.arc_clone(),
            self.type_order(),
            self.errors(),
            self.module_info(),
            key.range(),
        );
    }

    pub fn get_type(&self, key: &Key) -> Arc<Type> {
        self.get(key)
    }

    pub fn get_type_idx(&self, key: Idx<Key>) -> Arc<Type> {
        self.get_idx(key)
    }

    pub fn get_base_class(&self, key: &KeyBaseClass) -> Arc<BaseClass> {
        self.get(key)
    }

    pub fn get_mro(&self, key: &KeyMro) -> Arc<Mro> {
        self.get(key)
    }

    pub fn get_tparams(&self, key: &KeyTypeParams) -> Arc<QuantifiedVec> {
        self.get(key)
    }

    pub fn get_legacy_tparam_idx(
        &self,
        key: Idx<KeyLegacyTypeParam>,
    ) -> Arc<LegacyTypeParameterLookup> {
        self.get_idx(key)
    }

    fn solve_legacy_tparam(
        &self,
        binding: &BindingLegacyTypeParam,
    ) -> Arc<LegacyTypeParameterLookup> {
        match &*self.get_type_idx(binding.0) {
            Type::Type(box Type::TypeVar(_)) => {
                let q = Quantified::type_var(self.uniques);
                Arc::new(LegacyTypeParameterLookup::Parameter(q))
            }
            Type::Type(box Type::TypeVarTuple(_)) => {
                let q = Quantified::type_var_tuple(self.uniques);
                Arc::new(LegacyTypeParameterLookup::Parameter(q))
            }
            Type::Type(box Type::ParamSpec(_)) => {
                let q = Quantified::param_spec(self.uniques);
                Arc::new(LegacyTypeParameterLookup::Parameter(q))
            }
            ty => Arc::new(LegacyTypeParameterLookup::NotParameter(ty.clone())),
        }
    }

    fn solve_tparams(&self, binding: &BindingTypeParams) -> Arc<QuantifiedVec> {
        let res = match binding {
            BindingTypeParams::Function(scoped, legacy) => {
                let legacy_tparams: Vec<_> = legacy
                    .iter()
                    .filter_map(|key| {
                        self.get_legacy_tparam_idx(*key)
                            .deref()
                            .parameter()
                            .copied()
                    })
                    .collect();
                let mut tparams = scoped.clone();
                tparams.extend(legacy_tparams);
                QuantifiedVec(tparams)
            }
            BindingTypeParams::Class(class_def, legacy_params) => {
                match &*self.get_type(class_def) {
                    Type::ClassDef(cls) => self.tparams_of(cls, legacy_params),
                    _ => {
                        unreachable!(
                            "The key inside a ClassTypeParams binding must be a class type"
                        )
                    }
                }
            }
        };
        Arc::new(res)
    }

    fn solve_mro(&self, binding: &BindingMro) -> Arc<Mro> {
        match binding {
            BindingMro(k) => {
                let self_ty = self.get_type_idx(*k);
                match &*self_ty {
                    Type::ClassType(cls) => Arc::new(self.mro_of(cls.class_object())),
                    _ => {
                        unreachable!("The key inside an Mro binding must be a class type")
                    }
                }
            }
        }
    }

    fn solve_base_class(&self, binding: &BindingBaseClass) -> Arc<BaseClass> {
        match binding {
            BindingBaseClass(x, self_type) => Arc::new(self.base_class_of(x, *self_type)),
        }
    }

    fn solve_annotation(&self, binding: &BindingAnnotation) -> Arc<Annotation> {
        match binding {
            BindingAnnotation::AnnotateExpr(x, self_type) => {
                let mut ann = self.expr_annotation(x);
                if let Some(self_type) = self_type
                    && let Some(ty) = &mut ann.ty
                {
                    let self_type = &*self.get_type_idx(*self_type);
                    ty.subst_self_type_mut(self_type);
                }
                Arc::new(ann)
            }
            BindingAnnotation::Type(x) => Arc::new(Annotation::new_type(x.clone())),
            BindingAnnotation::AttrType(attr) => {
                let e = Expr::Attribute(attr.clone());
                let t = self.expr(&e, None);
                Arc::new(Annotation::new_type(t))
            }
            BindingAnnotation::Forward(k) => {
                Arc::new(Annotation::new_type(self.get_type_idx(*k).arc_clone()))
            }
        }
    }

    fn expr_qualifier(&self, x: &Expr) -> Option<Qualifier> {
        let ty = match x {
            Expr::Name(_) | Expr::Attribute(_) => Some(self.expr(x, None)),
            _ => None,
        };
        if let Some(Type::Type(box Type::SpecialForm(special))) = ty {
            special.to_qualifier()
        } else {
            None
        }
    }

    fn expr_annotation(&self, x: &Expr) -> Annotation {
        match x {
            _ if let Some(qualifier) = self.expr_qualifier(x) => Annotation {
                qualifiers: vec![qualifier],
                ty: None,
            },
            Expr::Subscript(x)
                if Ast::unpack_slice(&x.slice).len() == 1
                    && let Some(qualifier) = self.expr_qualifier(&x.value) =>
            {
                let mut ann = self.expr_annotation(&x.slice);
                ann.qualifiers.insert(0, qualifier);
                ann
            }
            _ => Annotation::new_type(self.expr_untype(x)),
        }
    }

    fn iterate(&self, iterable: &Type, range: TextRange) -> Iterable {
        match iterable {
            Type::ClassType(cls) => {
                let ty = if self.has_attribute(cls.class_object(), &dunder::ITER) {
                    let iterator_ty =
                        self.call_method_with_types(iterable, &dunder::ITER, range, &[]);
                    self.call_method_with_types(&iterator_ty, &dunder::NEXT, range, &[])
                } else if self.has_attribute(cls.class_object(), &dunder::GETITEM) {
                    let arg = TypeCallArg::new(self.stdlib.int(), range);
                    self.call_method_with_types(iterable, &dunder::GETITEM, range, &[arg])
                } else {
                    self.error(range, format!("Class `{}` is not iterable", cls.name()))
                };
                Iterable::OfType(ty)
            }
            Type::Tuple(Tuple::Concrete(elts)) => Iterable::FixedLen(elts.clone()),
            Type::Tuple(Tuple::Unbounded(box elt)) => Iterable::OfType(elt.clone()),
            Type::LiteralString => Iterable::OfType(self.stdlib.str()),
            Type::Literal(lit) if lit.is_string() => Iterable::OfType(self.stdlib.str()),
            Type::Any(_) => Iterable::OfType(Type::any_implicit()),
            _ => Iterable::OfType(
                self.error_todo("Answers::solve_binding - Binding::IterableValue", range),
            ),
        }
    }

    fn check_is_exception(&self, x: &Expr, range: TextRange, allow_none: bool) {
        let actual_type = self.expr(x, None);
        if allow_none && actual_type.is_none() {
            return;
        }
        let expected_types =
            if let base_exception_type @ Type::ClassType(c) = &self.stdlib.base_exception() {
                let base_exception_class_type = Type::ClassDef(c.class_object().dupe());
                vec![base_exception_type.clone(), base_exception_class_type]
            } else {
                unreachable!("The stdlib base exception type should be a ClassInstance")
            };
        if !self.solver.is_subset_eq(
            &actual_type,
            &Type::Union(expected_types),
            self.type_order(),
        ) {
            self.error(
                range,
                format!(
                    "Expression `{}` has type `{actual_type}` which does not derive from BaseException",
                    self.module_info().display(x)
                ),
            );
        }
    }

    fn tvars_to_quantifieds_for_type_alias(
        &self,
        ty: &mut Type,
        seen: &mut SmallMap<TypeVar, Quantified>,
        quantifieds: &mut Vec<Quantified>,
    ) {
        match ty {
            Type::Union(ts) => {
                for t in ts.iter_mut() {
                    self.tvars_to_quantifieds_for_type_alias(t, seen, quantifieds);
                }
            }
            Type::ClassType(cls) => {
                for t in cls.targs_mut().as_mut() {
                    self.tvars_to_quantifieds_for_type_alias(t, seen, quantifieds);
                }
            }
            Type::Callable(callable) => {
                let visit =
                    |t: &mut Type| self.tvars_to_quantifieds_for_type_alias(t, seen, quantifieds);
                callable.visit_mut(visit);
            }
            Type::TypeVar(ty_var) => {
                let q = match seen.entry(ty_var.dupe()) {
                    Entry::Occupied(e) => *e.get(),
                    Entry::Vacant(e) => {
                        let q = Quantified::type_var(self.uniques);
                        e.insert(q);
                        quantifieds.push(q);
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
    ) -> Type {
        if matches!(
            style,
            TypeAliasStyle::Scoped | TypeAliasStyle::LegacyExplicit
        ) && self.untype_opt(ty.clone(), range).is_none()
        {
            self.error(
                range,
                format!("Expected `{name}` to be a type alias, got {ty}"),
            );
            return Type::any_error();
        }
        let mut ty = match &ty {
            Type::ClassDef(cls) => {
                Type::type_form(Type::ClassType(self.promote_to_class_type(cls, range)))
            }
            t => t.clone(),
        };
        let mut seen = SmallMap::new();
        let mut quantifieds = Vec::new();
        match ty {
            Type::Type(ref mut t) => {
                self.tvars_to_quantifieds_for_type_alias(t, &mut seen, &mut quantifieds)
            }
            _ => {}
        }
        let ta = Type::TypeAlias(TypeAlias::new(name.clone(), ty, style));
        if quantifieds.is_empty() {
            ta
        } else {
            Type::Forall(quantifieds, Box::new(ta))
        }
    }

    fn context_value_enter(
        &self,
        context_manager_type: &Type,
        kind: ContextManagerKind,
        range: TextRange,
    ) -> Type {
        match kind {
            ContextManagerKind::Sync => {
                self.call_method_with_types(context_manager_type, &dunder::ENTER, range, &[])
            }
            ContextManagerKind::Async => self.unwrap_awaitable(
                self.call_method_with_types(context_manager_type, &dunder::AENTER, range, &[]),
                None,
            ),
        }
    }

    fn context_value_exit(
        &self,
        context_manager_type: &Type,
        kind: ContextManagerKind,
        range: TextRange,
    ) -> Type {
        let base_exception_class_type = Type::type_form(self.stdlib.base_exception());
        let exit_arg_types = [
            TypeCallArg::new(
                Type::Union(vec![base_exception_class_type, Type::None]),
                range,
            ),
            TypeCallArg::new(
                Type::Union(vec![self.stdlib.base_exception(), Type::None]),
                range,
            ),
            TypeCallArg::new(
                Type::Union(vec![self.stdlib.traceback_type(), Type::None]),
                range,
            ),
        ];
        match kind {
            ContextManagerKind::Sync => self.call_method_with_types(
                context_manager_type,
                &dunder::EXIT,
                range,
                &exit_arg_types,
            ),
            ContextManagerKind::Async => self.unwrap_awaitable(
                self.call_method_with_types(
                    context_manager_type,
                    &dunder::AEXIT,
                    range,
                    &exit_arg_types,
                ),
                None,
            ),
        }
    }

    fn context_value(
        &self,
        context_manager_type: Type,
        kind: ContextManagerKind,
        range: TextRange,
    ) -> Type {
        let enter_type = self.context_value_enter(&context_manager_type, kind, range);
        let exit_type = self.context_value_exit(&context_manager_type, kind, range);
        self.check_type(
            &Type::Union(vec![self.stdlib.bool(), Type::None]),
            &exit_type,
            range,
        );
        // TODO: `exit_type` may also affect exceptional control flow, which is yet to be supported:
        // https://typing.readthedocs.io/en/latest/spec/exceptions.html#context-managers
        enter_type
    }

    fn solve_binding(&self, binding: &Binding) -> Arc<Type> {
        // Replace any solved recursive variables with their answers.
        // We call self.unions() to simplify cases like
        // v = @1 | int, @1 = int.
        Arc::new(
            match self.solver.expand(self.solve_binding_inner(binding)) {
                Type::Union(ts) => self.unions(&ts),
                t => t,
            },
        )
    }

    fn solve_binding_inner(&self, binding: &Binding) -> Type {
        match binding {
            Binding::Expr(ann, e) => {
                let ty = ann.map(|k| self.get_annotation_idx(k));
                self.expr(e, ty.as_ref().and_then(|x| x.ty.as_ref()))
            }
            Binding::IterableValue(ann, e) => {
                let ty = ann.map(|k| self.get_annotation_idx(k));
                let hint = ty.and_then(|x| x.ty.clone().map(|ty| self.stdlib.iterable(ty)));
                let iterable = self.iterate(&self.expr(e, hint.as_ref()), e.range());
                match iterable {
                    Iterable::OfType(ty) => ty,
                    Iterable::FixedLen(ts) => self.unions(&ts),
                }
            }
            Binding::ContextValue(ann, e, kind) => {
                let context_manager = self.expr(e, None);
                let context_value = self.context_value(context_manager, *kind, e.range());
                let ty = ann.map(|k| self.get_annotation_idx(k));
                match ty.as_ref().and_then(|x| x.ty.as_ref()) {
                    Some(ty) => self.check_type(ty, &context_value, e.range()),
                    None => context_value,
                }
            }
            Binding::SubscriptValue(box b, x) => {
                let base = self.expr(&x.value, None);
                let slice_ty = self.expr(&x.slice, None);
                let value_ty = self.solve_binding_inner(b);
                self.call_method_with_types(
                    &base,
                    &dunder::SETITEM,
                    x.range,
                    &[
                        TypeCallArg::new(slice_ty, x.slice.range()),
                        // use the subscript's location
                        TypeCallArg::new(value_ty, x.range),
                    ],
                )
            }
            Binding::UnpackedValue(b, range, pos) => {
                let iterable = self.iterate(&self.solve_binding_inner(b), *range);
                match iterable {
                    Iterable::OfType(ty) => match pos {
                        UnpackedPosition::Index(_) | UnpackedPosition::ReverseIndex(_) => ty,
                        UnpackedPosition::Slice(_, _) => self.stdlib.list(ty),
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
                                    let elem_ty = self.unions(&ts[start..end]);
                                    self.stdlib.list(elem_ty)
                                } else {
                                    // We'll report this error when solving for Binding::UnpackedLength.
                                    Type::any_error()
                                }
                            }
                        }
                    }
                }
            }
            Binding::UnpackedLength(b, range, expect) => {
                let iterable_ty = self.solve_binding_inner(b);
                let iterable = self.iterate(&iterable_ty, *range);
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
                Type::None // Unused
            }
            Binding::Function(x, kind) => {
                let check_default = |default: &Option<Box<Expr>>, ty: &Type| {
                    let mut required = Required::Required;
                    if let Some(default) = default {
                        required = Required::Optional;
                        if *kind != FunctionKind::Stub
                            || !matches!(default.as_ref(), Expr::EllipsisLiteral(_))
                        {
                            self.expr(default, Some(ty));
                        }
                    }
                    required
                };
                let mut args = Vec::with_capacity(x.parameters.len());
                args.extend(x.parameters.posonlyargs.iter().map(|x| {
                    let annot =
                        self.get_annotation(&KeyAnnotation::Annotation(x.parameter.name.clone()));
                    let ty = annot.get_type();
                    let required = check_default(&x.default, ty);
                    Arg::PosOnly(ty.clone(), required)
                }));
                args.extend(x.parameters.args.iter().map(|x| {
                    let annot =
                        self.get_annotation(&KeyAnnotation::Annotation(x.parameter.name.clone()));
                    let ty = annot.get_type();
                    let required = check_default(&x.default, ty);
                    Arg::Pos(x.parameter.name.id.clone(), ty.clone(), required)
                }));
                args.extend(x.parameters.vararg.iter().map(|x| {
                    let annot = self.get_annotation(&KeyAnnotation::Annotation(x.name.clone()));
                    let ty = annot.get_type();
                    Arg::VarArg(ty.clone())
                }));
                args.extend(x.parameters.kwonlyargs.iter().map(|x| {
                    let annot =
                        self.get_annotation(&KeyAnnotation::Annotation(x.parameter.name.clone()));
                    let ty = annot.get_type();
                    let required = check_default(&x.default, ty);
                    Arg::KwOnly(x.parameter.name.id.clone(), ty.clone(), required)
                }));
                args.extend(x.parameters.kwarg.iter().map(|x| {
                    let annot = self.get_annotation(&KeyAnnotation::Annotation(x.name.clone()));
                    let ty = annot.get_type();
                    Arg::Kwargs(ty.clone())
                }));
                let ret = self.get_type(&Key::ReturnType(x.name.clone())).arc_clone();
                let ret = if x.is_async {
                    self.stdlib
                        .coroutine(Type::any_implicit(), Type::any_implicit(), ret)
                } else {
                    ret
                };
                let qs = self.get_tparams(&KeyTypeParams(x.name.clone()));
                Type::forall(qs.0.clone(), Type::callable(args, ret))
            }
            Binding::Import(m, name) => self
                .with_module(*m)
                .get_type(&Key::Export(name.clone()))
                .arc_clone(),
            Binding::Class(x, fields, n_bases) => {
                let tparams = self.type_params(&x.type_params);
                let c = Class::new(
                    x.name.clone(),
                    self.module_info().dupe(),
                    tparams,
                    fields.clone(),
                    *n_bases,
                );
                Type::ClassDef(c)
            }
            Binding::SelfType(k) => match &*self.get_type_idx(*k) {
                Type::ClassDef(c) => c.self_type(self.get_tparams_for_class(c).deref()),
                _ => unreachable!(),
            },
            Binding::Forward(k) => self.get_type_idx(*k).arc_clone(),
            Binding::Phi(ks) => {
                if ks.len() == 1 {
                    self.get_type_idx(*ks.first().unwrap()).arc_clone()
                } else {
                    self.unions(
                        &ks.iter()
                            .map(|k| self.get_type_idx(*k).arc_clone())
                            .collect::<Vec<_>>(),
                    )
                }
            }
            Binding::CheckRaisedException(RaisedException::WithoutCause(exc)) => {
                self.check_is_exception(exc, exc.range(), false);
                Type::None // Unused
            }
            Binding::CheckRaisedException(RaisedException::WithCause(exc, cause)) => {
                self.check_is_exception(exc, exc.range(), false);
                self.check_is_exception(cause, cause.range(), true);
                Type::None // Unused
            }
            Binding::AnnotatedType(ann, val) => match &self.get_annotation_idx(*ann).ty {
                Some(ty) => ty.clone(),
                None => self.solve_binding_inner(val),
            },
            Binding::AnyType(x) => Type::Any(*x),
            Binding::StrType => self.stdlib.str(),
            Binding::TypeParameter(q) => Type::type_form(q.to_type()),
            Binding::Module(m, path, prev) => {
                let prev = prev
                    .as_ref()
                    .and_then(|x| self.get_type_idx(*x).as_module().cloned());
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
                match &*self.get_legacy_tparam_idx(*key) {
                    LegacyTypeParameterLookup::Parameter(q) => {
                        // This class or function has scoped (PEP 695) type parameters. Mixing legacy-style parameters is an error.
                        if let Some(r) = range_if_scoped_params_exist {
                            self.error(
                                *r,
                                format!(
                                    "Type parameter {} is not included in the type parameter list",
                                    self.bindings().idx_to_key(*key).0
                                ),
                            );
                        }
                        Type::type_form(q.to_type())
                    }
                    LegacyTypeParameterLookup::NotParameter(ty) => ty.clone(),
                }
            }
            Binding::Eq(k1, k2, name) => {
                let ann1 = self.get_annotation_idx(*k1);
                let ann2 = self.get_annotation_idx(*k2);
                if let Some(t1) = &ann1.ty
                    && let Some(t2) = &ann2.ty
                    && *t1 != *t2
                {
                    self.error(
                        self.bindings().idx_to_key(*k1).range(),
                        format!(
                            "Inconsistent type annotations for {}: {}, {}",
                            name,
                            t1.clone().deterministic_printing(),
                            t2.clone().deterministic_printing(),
                        ),
                    );
                }
                Type::None // Unused
            }
            Binding::NameAssign(name, annot_key, binding, range) => {
                let annot = annot_key.map(|k| self.get_annotation_idx(k));
                let ty = self.solve_binding_inner(binding);
                match (annot, &ty) {
                    (Some(annot), _) if annot.qualifiers.contains(&Qualifier::TypeAlias) => {
                        self.as_type_alias(name, TypeAliasStyle::LegacyExplicit, ty, *range)
                    }
                    // TODO(stroxler, rechen): Do we want to include Type::ClassDef(_)
                    // when there is no annotation, so that `mylist = list` is treated
                    // like a value assignment rather than a type alias?
                    (None, Type::Type(box t)) if !t.is_tvar_declaration(name) => {
                        self.as_type_alias(name, TypeAliasStyle::LegacyImplicit, ty, *range)
                    }
                    _ => ty,
                }
            }
            Binding::ScopedTypeAlias(name, qs, binding, range) => {
                let ty = self.solve_binding_inner(binding);
                let ta = self.as_type_alias(name, TypeAliasStyle::Scoped, ty, *range);
                match ta {
                    Type::Forall(other_qs, inner_ta) => {
                        self.error(
                            *range,
                            format!("Type parameters used in `{name}` but not declared"),
                        );
                        let mut all_qs = qs.clone();
                        all_qs.extend(other_qs);
                        Type::Forall(all_qs, inner_ta)
                    }
                    Type::TypeAlias(_) if !qs.is_empty() => Type::Forall(qs.clone(), Box::new(ta)),
                    _ => ta,
                }
            }
        }
    }

    pub fn check_type(&self, want: &Type, got: &Type, loc: TextRange) -> Type {
        if matches!(got, Type::Any(AnyStyle::Error)) {
            // Don't propagate errors
            got.clone()
        } else if self.solver.is_subset_eq(got, want, self.type_order()) {
            got.clone()
        } else {
            self.solver
                .error(want, got, self.errors(), self.module_info(), loc);
            want.clone()
        }
    }

    pub fn unions(&self, xs: &[Type]) -> Type {
        self.solver.unions(xs.to_owned(), self.type_order())
    }

    pub fn union(&self, x: &Type, y: &Type) -> Type {
        self.unions(&[x.clone(), y.clone()])
    }

    pub fn error_todo(&self, msg: &str, x: impl Ranged + Debug) -> Type {
        self.errors().todo(self.module_info(), msg, x);
        Type::any_error()
    }

    pub fn error(&self, range: TextRange, msg: String) -> Type {
        self.errors().add(self.module_info(), range, msg);
        Type::any_error()
    }

    fn type_params(&self, x: &Option<Box<TypeParams>>) -> SmallMap<Name, Quantified> {
        let mut names = Vec::new();
        match x {
            Some(box x) => {
                for x in &x.type_params {
                    match x {
                        TypeParam::TypeVar(x) => names.push(&x.name),
                        TypeParam::ParamSpec(_) => {
                            self.error_todo("Answers::type_params", x);
                        }
                        TypeParam::TypeVarTuple(_) => {
                            self.error_todo("Answers::type_params", x);
                        }
                    }
                }
            }
            None => {}
        }

        fn get_quantified(t: &Type) -> Quantified {
            match t {
                Type::Type(box Type::Quantified(q)) => *q,
                _ => unreachable!(),
            }
        }

        names
            .into_iter()
            .map(|x| {
                (
                    x.id.clone(),
                    get_quantified(&self.get_type(&Key::Definition(x.clone()))),
                )
            })
            .collect()
    }

    /// Unwraps a type, originally evaluated as a value, so that it can be used as a type annotation.
    /// For example, in `def f(x: int): ...`, we evaluate `int` as a value, gettings its type as
    /// `type[int]`, then call `untype(type[int])` to get the `int` annotation.
    pub fn untype(&self, ty: Type, range: TextRange) -> Type {
        if let Some(t) = self.untype_opt(ty.clone(), range) {
            t
        } else {
            self.error(
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
                Some(self.unions(&ts))
            }
            Type::Var(v) if let Some(_guard) = self.recurser.recurse(v) => {
                self.untype_opt(self.solver.force_var(v), range)
            }
            Type::Type(box t) => Some(t),
            Type::None => Some(Type::None), // Both a value and a type
            Type::Ellipsis => Some(Type::Ellipsis), // A bit weird because of tuples, so just promote it
            Type::Any(style) => Some(style.propagate()),
            Type::TypeAlias(ta) => self.untype_opt(ta.as_type(), range),
            _ => None,
        }
    }

    pub fn expr_untype(&self, x: &Expr) -> Type {
        self.untype(self.expr(x, None), x.range())
    }

    pub fn get_import(&self, name: &Name, from: ModuleName, range: TextRange) -> Type {
        let exports = self.current.exports.get(&from).unwrap();
        if !exports.contains(name, self.current.exports) {
            self.error(range, format!("No attribute `{name}` in module `{from}`",))
        } else {
            self.with_module(from)
                .get_type(&Key::Export(name.clone()))
                .arc_clone()
        }
    }

    pub fn promote(&self, ty: Type, hint: Option<&Type>) -> Type {
        if let Some(t) = hint {
            t.clone()
        } else {
            ty.promote_literals(self.stdlib)
        }
    }
}
