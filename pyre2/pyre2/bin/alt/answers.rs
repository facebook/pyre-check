/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;

use crate::alt::class::classdef::ClassField;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::alt::types::legacy_lookup::LegacyTypeParameterLookup;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::EmptyAnswer;
use crate::binding::binding::FunctionBinding;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyClassSynthesizedFields;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyFunction;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::Keyed;
use crate::binding::bindings::BindingEntry;
use crate::binding::bindings::BindingTable;
use crate::binding::bindings::Bindings;
use crate::binding::table::TableKeyed;
use crate::error::collector::ErrorCollector;
use crate::export::exports::LookupExport;
use crate::graph::calculation::Calculation;
use crate::graph::index::Idx;
use crate::graph::index_map::IndexMap;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::solver::solver::Solver;
use crate::solver::type_order::TypeOrder;
use crate::table;
use crate::table_for_each;
use crate::table_mut_for_each;
use crate::table_try_for_each;
use crate::types::annotation::Annotation;
use crate::types::class::Class;
use crate::types::stdlib::Stdlib;
use crate::types::types::AnyStyle;
use crate::types::types::Type;
use crate::types::types::Var;
use crate::util::display::DisplayWith;
use crate::util::prelude::SliceExt;
use crate::util::recurser::Recurser;
use crate::util::uniques::UniqueFactory;

pub const UNKNOWN: Name = Name::new_static("~unknown");

/// Invariants:
///
/// * Every module name referenced anywhere MUST be present
///   in the `exports` and `bindings` map.
/// * Every key referenced in `bindings`/`answers` MUST be present.
///
/// We never issue contains queries on these maps.
#[derive(Debug)]
pub struct Answers {
    solver: Solver,
    table: AnswerTable,
}

pub type AnswerEntry<K> =
    IndexMap<K, Calculation<Arc<<K as Keyed>::Answer>, <K as SolveRecursive>::Recursive>>;

table!(
    #[derive(Debug, Default)]
    pub struct AnswerTable(AnswerEntry)
);

impl DisplayWith<Bindings> for Answers {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, bindings: &Bindings) -> fmt::Result {
        fn go<K: SolveRecursive>(
            bindings: &Bindings,
            entry: &AnswerEntry<K>,
            f: &mut fmt::Formatter<'_>,
        ) -> fmt::Result
        where
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            for (idx, answer) in entry.iter() {
                let key = bindings.idx_to_key(idx);
                let value = bindings.get(idx);
                writeln!(
                    f,
                    "{} = {} = {}",
                    bindings.module_info().display(key),
                    value.display_with(bindings),
                    match answer.get() {
                        Some(v) => v.to_string(),
                        None => "(unsolved)".to_owned(),
                    },
                )?;
            }
            Ok(())
        }

        table_try_for_each!(self.table, |x| go(bindings, x, f));
        Ok(())
    }
}

pub type SolutionsEntry<K> = SmallMap<K, <K as Keyed>::Answer>;

table!(
    #[derive(Default, Debug, Clone)]
    pub struct Solutions(pub SolutionsEntry)
);

impl DisplayWith<ModuleInfo> for Solutions {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        fn go<K: Keyed>(
            entry: &SolutionsEntry<K>,
            f: &mut fmt::Formatter<'_>,
            ctx: &ModuleInfo,
        ) -> fmt::Result
        where
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            for (key, answer) in entry.iter() {
                writeln!(f, "{} = {}", ctx.display(key), answer)?;
            }
            Ok(())
        }

        table_try_for_each!(self, |x| go(x, f, ctx));
        Ok(())
    }
}

#[derive(Clone)]
pub struct AnswersSolver<'a, Ans: LookupAnswer> {
    exports: &'a dyn LookupExport,
    answers: &'a Ans,
    current: &'a Answers,
    // The base solver is only used to reset the error collector at binding
    // boundaries. Answers code should generally use the error collector passed
    // along the call stack instead.
    base_errors: &'a ErrorCollector,
    bindings: &'a Bindings,
    pub uniques: &'a UniqueFactory,
    pub recurser: &'a Recurser<Var>,
    pub stdlib: &'a Stdlib,
}

pub trait LookupAnswer: Sized {
    fn get<K: Solve<Self> + Keyed<EXPORTED = true>>(
        &self,
        module: ModuleName,
        k: &K,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>;
}

pub trait SolveRecursive: Keyed {
    type Recursive: Dupe = ();

    fn promote_recursive(x: Self::Recursive) -> Self::Answer;

    fn visit_type_mut(v: &mut Self::Answer, f: &mut dyn FnMut(&mut Type));
}

impl SolveRecursive for Key {
    type Recursive = Var;
    fn promote_recursive(x: Self::Recursive) -> Self::Answer {
        Type::Var(x)
    }
    fn visit_type_mut(v: &mut Type, f: &mut dyn FnMut(&mut Type)) {
        f(v);
    }
}
impl SolveRecursive for KeyExpect {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        EmptyAnswer
    }
    fn visit_type_mut(_: &mut Self::Answer, _: &mut dyn FnMut(&mut Type)) {}
}
impl SolveRecursive for KeyExport {
    type Recursive = Var;
    fn promote_recursive(x: Self::Recursive) -> Self::Answer {
        Type::Var(x)
    }
    fn visit_type_mut(v: &mut Type, f: &mut dyn FnMut(&mut Type)) {
        f(v);
    }
}
impl SolveRecursive for KeyFunction {
    type Recursive = Var;
    fn promote_recursive(x: Self::Recursive) -> Self::Answer {
        Type::Var(x)
    }
    fn visit_type_mut(v: &mut Type, f: &mut dyn FnMut(&mut Type)) {
        f(v);
    }
}
impl SolveRecursive for KeyClassField {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        // TODO(stroxler) Revisit the recursive handling, which needs changes in the plumbing
        // to work correctly; what we have here is a fallback to permissive gradual typing.
        ClassField::recursive()
    }
    fn visit_type_mut(v: &mut ClassField, f: &mut dyn FnMut(&mut Type)) {
        v.visit_type_mut(f);
    }
}
impl SolveRecursive for KeyClassSynthesizedFields {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        ClassSynthesizedFields::default()
    }
    fn visit_type_mut(v: &mut ClassSynthesizedFields, f: &mut dyn FnMut(&mut Type)) {
        v.visit_type_mut(f)
    }
}
impl SolveRecursive for KeyAnnotation {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        Annotation::default()
    }
    fn visit_type_mut(v: &mut Annotation, f: &mut dyn FnMut(&mut Type)) {
        v.ty.iter_mut().for_each(f);
    }
}
impl SolveRecursive for KeyClassMetadata {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        ClassMetadata::recursive()
    }
    fn visit_type_mut(v: &mut ClassMetadata, f: &mut dyn FnMut(&mut Type)) {
        v.visit_mut(f);
    }
}
impl SolveRecursive for KeyLegacyTypeParam {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        LegacyTypeParameterLookup::NotParameter(Type::any_implicit())
    }
    fn visit_type_mut(v: &mut LegacyTypeParameterLookup, f: &mut dyn FnMut(&mut Type)) {
        v.not_parameter_mut().into_iter().for_each(f);
    }
}

pub trait Solve<Ans: LookupAnswer>: SolveRecursive {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &Self::Value,
        _errors: &ErrorCollector,
    ) -> Arc<Self::Answer>;

    fn recursive(answers: &AnswersSolver<Ans>) -> Self::Recursive;

    fn record_recursive(
        _answers: &AnswersSolver<Ans>,
        _key: &Self,
        _answer: Arc<Self::Answer>,
        _recursive: Self::Recursive,
        _errors: &ErrorCollector,
    ) {
    }
}

impl<Ans: LookupAnswer> Solve<Ans> for Key {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &Binding,
        errors: &ErrorCollector,
    ) -> Arc<Type> {
        answers.solve_binding(binding, errors)
    }

    fn recursive(answers: &AnswersSolver<Ans>) -> Self::Recursive {
        answers.solver().fresh_recursive(answers.uniques)
    }

    fn record_recursive(
        answers: &AnswersSolver<Ans>,
        key: &Key,
        answer: Arc<Type>,
        recursive: Var,
        errors: &ErrorCollector,
    ) {
        answers.record_recursive(key.range(), answer, recursive, errors);
    }
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyExpect {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingExpect,
        errors: &ErrorCollector,
    ) -> Arc<EmptyAnswer> {
        answers.solve_expectation(binding, errors)
    }

    fn recursive(_: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyExport {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &Binding,
        errors: &ErrorCollector,
    ) -> Arc<Type> {
        answers.solve_binding(binding, errors)
    }

    fn recursive(answers: &AnswersSolver<Ans>) -> Self::Recursive {
        answers.solver().fresh_recursive(answers.uniques)
    }

    fn record_recursive(
        answers: &AnswersSolver<Ans>,
        key: &KeyExport,
        answer: Arc<Type>,
        recursive: Var,
        errors: &ErrorCollector,
    ) {
        answers.record_recursive(key.range(), answer, recursive, errors);
    }
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyFunction {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &FunctionBinding,
        errors: &ErrorCollector,
    ) -> Arc<Type> {
        answers.solve_function(binding, errors)
    }

    fn recursive(answers: &AnswersSolver<Ans>) -> Self::Recursive {
        answers.solver().fresh_recursive(answers.uniques)
    }

    fn record_recursive(
        answers: &AnswersSolver<Ans>,
        key: &KeyFunction,
        answer: Arc<Type>,
        recursive: Var,
        errors: &ErrorCollector,
    ) {
        answers.record_recursive(key.range(), answer, recursive, errors);
    }
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyClassField {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingClassField,
        errors: &ErrorCollector,
    ) -> Arc<ClassField> {
        answers.solve_class_field(binding, errors)
    }

    fn recursive(_: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyClassSynthesizedFields {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingClassSynthesizedFields,
        _errors: &ErrorCollector,
    ) -> Arc<ClassSynthesizedFields> {
        answers.solve_class_synthesized_fields(binding)
    }

    fn recursive(_: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyAnnotation {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingAnnotation,
        errors: &ErrorCollector,
    ) -> Arc<Annotation> {
        answers.solve_annotation(binding, errors)
    }

    fn recursive(_answers: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyClassMetadata {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingClassMetadata,
        errors: &ErrorCollector,
    ) -> Arc<ClassMetadata> {
        answers.solve_mro(binding, errors)
    }

    fn recursive(_answers: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyLegacyTypeParam {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingLegacyTypeParam,
        _errors: &ErrorCollector,
    ) -> Arc<LegacyTypeParameterLookup> {
        answers.solve_legacy_tparam(binding)
    }

    fn recursive(_answers: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl Answers {
    pub fn new(bindings: &Bindings) -> Self {
        fn presize<K: SolveRecursive>(items: &mut AnswerEntry<K>, bindings: &Bindings)
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
            solver: Solver::new(),
            table,
        }
    }

    #[expect(dead_code)]
    fn len(&self) -> usize {
        let mut res = 0;
        table_for_each!(&self.table, |x: &AnswerEntry<_>| res += x.len());
        res
    }

    pub fn solve<Ans: LookupAnswer>(
        &self,
        exports: &dyn LookupExport,
        answers: &Ans,
        bindings: &Bindings,
        errors: &ErrorCollector,
        stdlib: &Stdlib,
        uniques: &UniqueFactory,
        exported_only: bool,
    ) -> Solutions {
        let mut res = Solutions::default();

        fn pre_solve<Ans: LookupAnswer, K: Solve<Ans>>(
            items: &mut SolutionsEntry<K>,
            answers: &AnswersSolver<Ans>,
            exported_only: bool,
        ) where
            AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
            BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        {
            let retain = K::EXPORTED || !exported_only;
            if retain {
                items.reserve(answers.bindings.keys::<K>().len());
            }
            for idx in answers.bindings.keys::<K>() {
                let k = answers.bindings.idx_to_key(idx);
                let v = answers.get(k);
                if retain {
                    items.insert(k.clone(), Arc::unwrap_or_clone(v));
                }
            }
        }
        let answers_solver = AnswersSolver {
            stdlib,
            answers,
            bindings,
            base_errors: errors,
            exports,
            uniques,
            recurser: &Recurser::new(),
            current: self,
        };
        table_mut_for_each!(&mut res, |items| pre_solve(
            items,
            &answers_solver,
            exported_only
        ));

        // Now force all types to be fully resolved.
        fn post_solve<K: SolveRecursive>(items: &mut SolutionsEntry<K>, solver: &Solver) {
            for v in items.values_mut() {
                K::visit_type_mut(v, &mut |x| solver.deep_force_mut(x));
            }
        }
        table_mut_for_each!(&mut res, |items| post_solve(items, &self.solver));
        res
    }

    pub fn solve_key<Ans: LookupAnswer, K: Solve<Ans>>(
        &self,
        exports: &dyn LookupExport,
        answers: &Ans,
        bindings: &Bindings,
        errors: &ErrorCollector,
        stdlib: &Stdlib,
        uniques: &UniqueFactory,
        key: &K,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        let solver = AnswersSolver {
            stdlib,
            uniques,
            answers,
            bindings,
            base_errors: errors,
            exports,
            recurser: &Recurser::new(),
            current: self,
        };
        solver.get(key)
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn bindings(&self) -> &Bindings {
        self.bindings
    }

    pub fn module_info(&self) -> &ModuleInfo {
        self.bindings.module_info()
    }

    pub fn solver(&self) -> &Solver {
        &self.current.solver
    }

    pub fn get_from_module<K: Solve<Ans> + Keyed<EXPORTED = true>>(
        &self,
        module: ModuleName,
        k: &K,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        if module == self.module_info().name() {
            self.get(k)
        } else {
            self.answers.get(module, k)
        }
    }

    pub fn get_from_class<K: Solve<Ans> + Keyed<EXPORTED = true>>(
        &self,
        cls: &Class,
        k: &K,
    ) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
        Solutions: TableKeyed<K, Value = SolutionsEntry<K>>,
    {
        self.get_from_module(cls.module_info().name(), k)
    }

    pub fn type_order(&self) -> TypeOrder<Ans> {
        TypeOrder::new(self)
    }

    pub fn get_idx<K: Solve<Ans>>(&self, idx: Idx<K>) -> Arc<K::Answer>
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
                self.module_info().display(self.bindings().idx_to_key(idx)),
            )
        });
        let result = calculation.calculate_with_recursive(
            || {
                let binding = self.bindings().get(idx);
                K::solve(self, binding, self.base_errors)
            },
            || K::recursive(self),
        );
        if let Ok((v, Some(r))) = &result {
            let k = self.bindings().idx_to_key(idx);
            K::record_recursive(self, k, v.dupe(), r.clone(), self.base_errors);
        }
        match result {
            Ok((v, _)) => v,
            Err(r) => Arc::new(K::promote_recursive(r)),
        }
    }

    pub fn get<K: Solve<Ans>>(&self, k: &K) -> Arc<K::Answer>
    where
        AnswerTable: TableKeyed<K, Value = AnswerEntry<K>>,
        BindingTable: TableKeyed<K, Value = BindingEntry<K>>,
    {
        self.get_idx(self.bindings().key_to_idx(k))
    }

    fn record_recursive(
        &self,
        loc: TextRange,
        answer: Arc<Type>,
        recursive: Var,
        errors: &ErrorCollector,
    ) {
        self.solver().record_recursive(
            recursive,
            answer.arc_clone(),
            self.type_order(),
            errors,
            self.module_info(),
            loc,
        );
    }

    pub fn check_type(
        &self,
        want: &Type,
        got: &Type,
        loc: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if matches!(got, Type::Any(AnyStyle::Error)) {
            // Don't propagate errors
            got.clone()
        } else if self.solver().is_subset_eq(got, want, self.type_order()) {
            got.clone()
        } else {
            self.solver()
                .error(want, got, errors, self.module_info(), loc);
            want.clone()
        }
    }

    pub fn distribute_over_union(&self, ty: &Type, mut f: impl FnMut(&Type) -> Type) -> Type {
        match ty {
            Type::Union(tys) => self.unions(tys.map(f)),
            _ => f(ty),
        }
    }

    pub fn unions(&self, xs: Vec<Type>) -> Type {
        self.solver().unions(xs, self.type_order())
    }

    pub fn union(&self, x: Type, y: Type) -> Type {
        self.unions(vec![x, y])
    }

    pub fn todo(&self, errors: &ErrorCollector, msg: &str, x: impl Ranged + Debug) -> Type {
        errors.todo(self.module_info(), msg, x);
        Type::any_error()
    }

    pub fn error(&self, errors: &ErrorCollector, range: TextRange, msg: String) -> Type {
        errors.add(self.module_info(), range, msg);
        Type::any_error()
    }

    pub fn get_import(&self, name: &Name, from: ModuleName) -> Option<Type> {
        if let Ok(exports) = self.exports.get(from) {
            if exports.contains(name, self.exports) {
                Some(
                    self.get_from_module(from, &KeyExport(name.clone()))
                        .arc_clone(),
                )
            } else {
                None
            }
        } else {
            // We have already errored on `m` when loading the module. No need to emit error again.
            Some(Type::any_error())
        }
    }
}
