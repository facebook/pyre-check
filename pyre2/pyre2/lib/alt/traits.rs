/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::sync::Arc;

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_text_size::Ranged;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::class_field::ClassField;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::ClassSynthesizedFields;
use crate::alt::types::decorated_function::DecoratedFunction;
use crate::alt::types::legacy_lookup::LegacyTypeParameterLookup;
use crate::alt::types::yields::YieldFromResult;
use crate::alt::types::yields::YieldResult;
use crate::binding::binding::AnnotationTarget;
use crate::binding::binding::AnnotationWithTarget;
use crate::binding::binding::Binding;
use crate::binding::binding::BindingAnnotation;
use crate::binding::binding::BindingClass;
use crate::binding::binding::BindingClassField;
use crate::binding::binding::BindingClassMetadata;
use crate::binding::binding::BindingClassSynthesizedFields;
use crate::binding::binding::BindingExpect;
use crate::binding::binding::BindingFunction;
use crate::binding::binding::BindingLegacyTypeParam;
use crate::binding::binding::BindingYield;
use crate::binding::binding::BindingYieldFrom;
use crate::binding::binding::EmptyAnswer;
use crate::binding::binding::Key;
use crate::binding::binding::KeyAnnotation;
use crate::binding::binding::KeyClass;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyClassSynthesizedFields;
use crate::binding::binding::KeyExpect;
use crate::binding::binding::KeyExport;
use crate::binding::binding::KeyFunction;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::binding::binding::KeyYield;
use crate::binding::binding::KeyYieldFrom;
use crate::binding::binding::Keyed;
use crate::binding::binding::NoneIfRecursive;
use crate::error::collector::ErrorCollector;
use crate::types::annotation::Annotation;
use crate::types::class::Class;
use crate::types::types::Type;
use crate::types::types::Var;

pub trait SolveRecursive: Keyed {
    type Recursive: Dupe = ();

    fn promote_recursive(x: Self::Recursive) -> Self::Answer;
}

impl SolveRecursive for Key {
    type Recursive = Var;
    fn promote_recursive(x: Self::Recursive) -> Self::Answer {
        Type::Var(x)
    }
}
impl SolveRecursive for KeyExpect {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        EmptyAnswer
    }
}
impl SolveRecursive for KeyExport {
    type Recursive = Var;
    fn promote_recursive(x: Self::Recursive) -> Self::Answer {
        Type::Var(x)
    }
}
impl SolveRecursive for KeyFunction {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        // TODO(samgoldman) I'm not sure this really makes sense. These bindings should never
        // be recursive, but this definition is required.
        DecoratedFunction::recursive()
    }
}
impl SolveRecursive for KeyClass {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        NoneIfRecursive(None)
    }
}
impl SolveRecursive for KeyClassField {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        // TODO(stroxler) Revisit the recursive handling, which needs changes in the plumbing
        // to work correctly; what we have here is a fallback to permissive gradual typing.
        ClassField::recursive()
    }
}
impl SolveRecursive for KeyClassSynthesizedFields {
    type Recursive = ();
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        ClassSynthesizedFields::default()
    }
}
impl SolveRecursive for KeyAnnotation {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        AnnotationWithTarget {
            target: AnnotationTarget::Assign(Name::default()),
            annotation: Annotation::default(),
        }
    }
}
impl SolveRecursive for KeyClassMetadata {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        ClassMetadata::recursive()
    }
}
impl SolveRecursive for KeyLegacyTypeParam {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        LegacyTypeParameterLookup::NotParameter(Type::any_implicit())
    }
}
impl SolveRecursive for KeyYield {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        // In practice, we should never have recursive bindings with yield.
        YieldResult::recursive()
    }
}
impl SolveRecursive for KeyYieldFrom {
    fn promote_recursive(_: Self::Recursive) -> Self::Answer {
        // In practice, we should never have recursive bindings with yield from.
        YieldFromResult::recursive()
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
        binding: &BindingFunction,
        errors: &ErrorCollector,
    ) -> Arc<DecoratedFunction> {
        answers.solve_function(binding, errors)
    }

    fn recursive(_: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyClass {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingClass,
        errors: &ErrorCollector,
    ) -> Arc<NoneIfRecursive<Class>> {
        answers.solve_class(binding, errors)
    }

    fn recursive(_: &AnswersSolver<Ans>) -> Self::Recursive {}
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
    ) -> Arc<AnnotationWithTarget> {
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

impl<Ans: LookupAnswer> Solve<Ans> for KeyYield {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingYield,
        errors: &ErrorCollector,
    ) -> Arc<YieldResult> {
        answers.solve_yield(binding, errors)
    }

    fn recursive(_answers: &AnswersSolver<Ans>) -> Self::Recursive {}
}

impl<Ans: LookupAnswer> Solve<Ans> for KeyYieldFrom {
    fn solve(
        answers: &AnswersSolver<Ans>,
        binding: &BindingYieldFrom,
        errors: &ErrorCollector,
    ) -> Arc<YieldFromResult> {
        answers.solve_yield_from(binding, errors)
    }

    fn recursive(_answers: &AnswersSolver<Ans>) -> Self::Recursive {}
}
