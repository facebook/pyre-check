/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;
use std::sync::Arc;

use itertools::Either;
use itertools::Itertools;
use ruff_python_ast::name::Name;
use ruff_python_ast::Decorator;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::class::classdef::ClassField;
use crate::alt::class::classdef::ClassFieldInner;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::DataclassMetadata;
use crate::alt::types::class_metadata::EnumMetadata;
use crate::ast::Ast;
use crate::binding::binding::ClassFieldInitialization;
use crate::binding::binding::Key;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::dunder;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::callable::Callable;
use crate::types::callable::CallableKind;
use crate::types::callable::Param;
use crate::types::callable::ParamList;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::special_form::SpecialForm;
use crate::types::tuple::Tuple;
use crate::types::type_var::Variance;
use crate::types::types::CalleeKind;
use crate::types::types::TParamInfo;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

/// Private helper type used to share part of the logic needed for the
/// binding-level work of finding legacy type parameters versus the type-level
/// work of computing inherticance information and the MRO.
#[derive(Debug, Clone)]
pub(super) enum BaseClass {
    TypedDict,
    Generic(Vec<Type>),
    Protocol(Vec<Type>),
    Expr(Expr),
}

impl BaseClass {
    pub fn can_apply(&self) -> bool {
        matches!(self, BaseClass::Generic(_) | BaseClass::Protocol(_))
    }

    pub fn apply(&mut self, args: Vec<Type>) {
        match self {
            BaseClass::Generic(xs) | BaseClass::Protocol(xs) => {
                xs.extend(args);
            }
            _ => panic!("cannot apply base class"),
        }
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn class_metadata_of(
        &self,
        cls: &Class,
        bases: &[Expr],
        keywords: &[(Name, Expr)],
        decorators: &[Decorator],
    ) -> ClassMetadata {
        let mut is_typed_dict = false;
        let mut is_named_tuple = false;
        let mut enum_metadata = None;
        let mut dataclass_metadata = None;
        let bases: Vec<BaseClass> = bases.map(|x| self.base_class_of(x));
        let is_protocol = bases.iter().any(|x| matches!(x, BaseClass::Protocol(_)));
        let bases_with_metadata = bases
            .iter()
            .filter_map(|x| match x {
                BaseClass::Expr(x) => match self.expr_untype(x) {
                    Type::ClassType(c) => {
                        let cls = c.class_object();
                        let class_metadata = self.get_metadata_for_class(cls);
                        if class_metadata.is_typed_dict() {
                            is_typed_dict = true;
                        }
                        if class_metadata.is_named_tuple()
                        || cls.has_qname("typing", "NamedTuple")
                        {
                            is_named_tuple = true;
                        }
                        if is_protocol && !class_metadata.is_protocol() {
                            self.error(
                                x.range(),
                                "If `Protocol` is included as a base class, all other bases must be protocols.".to_owned(),
                            );
                        }
                        if dataclass_metadata.is_none() && let Some(base_dataclass) = class_metadata.dataclass_metadata() {
                            // If we inherit from a dataclass, copy its fields. Note that if this class is
                            // itself decorated with @dataclass, we'll recompute the fields and overwrite this.
                            dataclass_metadata = Some(DataclassMetadata {
                                fields: base_dataclass.fields.clone(),
                                synthesized_fields: SmallSet::new(),
                                frozen: base_dataclass.frozen,
                            });
                        }
                        Some((c, class_metadata))
                    }
                    Type::TypedDict(typed_dict) => {
                        is_typed_dict = true;
                        let class_object = typed_dict.class_object();
                        let class_metadata = self.get_metadata_for_class(class_object);
                        // In normal typechecking logic, TypedDicts should never be represented as ClassType.
                        // However, we convert it to a ClassType here so that MRO works properly and we can look up
                        // the types of the declared items.
                        Some((
                            ClassType::new(class_object.clone(), typed_dict.targs().clone()),
                            class_metadata,
                        ))
                    }
                    _ => None,
                },
                BaseClass::TypedDict => {
                    is_typed_dict = true;
                    None
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        if is_named_tuple && bases_with_metadata.len() > 1 {
            self.error(
                cls.name().range,
                "Named tuples do not support multiple inheritance".to_owned(),
            );
        }
        let (metaclasses, keywords): (Vec<_>, Vec<(_, _)>) =
            keywords.iter().partition_map(|(n, x)| match n.as_str() {
                "metaclass" => Either::Left(x),
                _ => Either::Right((n.clone(), self.expr(x, None))),
            });

        let base_metaclasses = bases_with_metadata
            .iter()
            .filter_map(|(b, metadata)| metadata.metaclass().map(|m| (&b.name().id, m)))
            .collect::<Vec<_>>();
        let metaclass =
            self.calculate_metaclass(cls, metaclasses.into_iter().next(), &base_metaclasses);
        if let Some(metaclass) = &metaclass {
            self.check_base_class_metaclasses(cls, metaclass, &base_metaclasses);
            if self.solver().is_subset_eq(
                &Type::ClassType(metaclass.clone()),
                &Type::ClassType(self.stdlib.enum_meta()),
                self.type_order(),
            ) {
                if !cls.tparams().is_empty() {
                    self.error(cls.name().range, "Enums may not be generic.".to_owned());
                }
                enum_metadata = Some(EnumMetadata {
                    // A generic enum is an error, but we create Any type args anyway to handle it gracefully.
                    cls: ClassType::new(cls.clone(), self.create_default_targs(cls, None)),
                    is_flag: bases_with_metadata.iter().any(|(base, _)| {
                        self.solver().is_subset_eq(
                            &Type::ClassType(base.clone()),
                            &Type::ClassType(self.stdlib.enum_flag()),
                            self.type_order(),
                        )
                    }),
                })
            }
            if is_typed_dict {
                self.error(
                    cls.name().range,
                    "Typed dictionary definitions may not specify a metaclass.".to_owned(),
                );
            }
        }
        for decorator in decorators {
            let ty_decorator = self.expr(&decorator.expression, None);
            if let Some(CalleeKind::Callable(CallableKind::Dataclass(kws))) =
                ty_decorator.callee_kind()
            {
                let dataclass_fields = self.get_dataclass_fields(cls, &bases_with_metadata);
                let mut synthesized_fields = SmallSet::new();
                if kws.init && !cls.contains(&dunder::INIT) {
                    // If a class already defines `__init__`, @dataclass doesn't overwrite it.
                    synthesized_fields.insert(dunder::INIT);
                }
                if kws.match_args {
                    synthesized_fields.insert(dunder::MATCH_ARGS);
                }
                dataclass_metadata = Some(DataclassMetadata {
                    fields: dataclass_fields,
                    synthesized_fields,
                    frozen: kws.frozen,
                });
            }
        }
        if is_typed_dict
            && let Some(bad) = bases_with_metadata.iter().find(|x| !x.1.is_typed_dict())
        {
            self.error(
                cls.name().range,
                format!("`{}` is not a typed dictionary. Typed dictionary definitions may only extend other typed dictionaries.", bad.0),
            );
        }
        ClassMetadata::new(
            cls,
            bases_with_metadata,
            metaclass,
            keywords,
            is_typed_dict,
            is_named_tuple,
            enum_metadata,
            is_protocol,
            dataclass_metadata,
            self.errors(),
        )
    }

    pub fn get_synthesized_field(&self, cls: &Class, name: &Name) -> Option<ClassField> {
        self.get_dataclass_synthesized_field(cls, name)
    }

    /// This helper deals with special cases where we want to intercept an `Expr`
    /// manually and create a special variant of `BaseClass` instead of calling
    /// `expr_untype` and creating a `BaseClass::Type`.
    ///
    /// TODO(stroxler): See if there's a way to express this more clearly in the types.
    fn special_base_class(&self, base_expr: &Expr) -> Option<BaseClass> {
        if let Expr::Name(name) = base_expr {
            match &*self.get(&Key::Usage(ShortIdentifier::expr_name(name))) {
                Type::Type(box Type::SpecialForm(special)) => match special {
                    SpecialForm::Protocol => Some(BaseClass::Protocol(Vec::new())),
                    SpecialForm::Generic => Some(BaseClass::Generic(Vec::new())),
                    SpecialForm::TypedDict => Some(BaseClass::TypedDict),
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
        }
    }

    pub(super) fn base_class_of(&self, base_expr: &Expr) -> BaseClass {
        if let Some(special_base_class) = self.special_base_class(base_expr) {
            // This branch handles cases like `Protocol`
            special_base_class
        } else if let Expr::Subscript(subscript) = base_expr
            && let Some(mut special_base_class) = self.special_base_class(&subscript.value)
            && special_base_class.can_apply()
        {
            // This branch handles `Generic[...]` and `Protocol[...]`
            let args = Ast::unpack_slice(&subscript.slice).map(|x| self.expr_untype(x));
            special_base_class.apply(args);
            special_base_class
        } else {
            // This branch handles all other base classes.
            BaseClass::Expr(base_expr.clone())
        }
    }

    pub(super) fn class_tparams(
        &self,
        name: &Identifier,
        scoped_tparams: Vec<TParamInfo>,
        bases: Vec<BaseClass>,
        legacy: &[Idx<KeyLegacyTypeParam>],
    ) -> TParams {
        let legacy_tparams = legacy
            .iter()
            .filter_map(|key| self.get_idx(*key).deref().parameter().cloned())
            .collect::<SmallSet<_>>();
        let legacy_map = legacy_tparams
            .iter()
            .map(|p| (p.quantified, p))
            .collect::<SmallMap<_, _>>();

        let lookup_tparam = |t: &Type| {
            let q = t.as_quantified()?;
            let p = legacy_map.get(&q);
            if p.is_none() {
                self.error(
                    name.range,
                    "Redundant type parameter declaration".to_owned(),
                );
            }
            p.map(|x| (*x).clone())
        };

        // TODO(stroxler): There are a lot of checks, such as that `Generic` only appears once
        // and no non-type-vars are used, that we can more easily detect in a dedictated class
        // validation step that validates all the bases. We are deferring these for now.
        let mut generic_tparams = SmallSet::new();
        let mut protocol_tparams = SmallSet::new();
        for base in bases.iter() {
            match base {
                BaseClass::Generic(ts) => {
                    for t in ts.iter() {
                        if let Some(p) = lookup_tparam(t) {
                            generic_tparams.insert(p);
                        }
                    }
                }
                BaseClass::Protocol(ts) if !ts.is_empty() => {
                    for t in ts.iter() {
                        if let Some(p) = lookup_tparam(t) {
                            protocol_tparams.insert(p);
                        }
                    }
                }
                _ => {}
            }
        }
        if !generic_tparams.is_empty() && !protocol_tparams.is_empty() {
            self.error(
                name.range,
                format!(
                    "Class `{}` specifies type parameters in both `Generic` and `Protocol` bases",
                    name.id,
                ),
            );
        }
        // Initialized the tparams: combine scoped and explicit type parameters
        let mut tparams = SmallSet::new();
        tparams.extend(scoped_tparams);
        tparams.extend(generic_tparams);
        tparams.extend(protocol_tparams);
        // Handle implicit tparams: if a Quantified was bound at this scope and is not yet
        // in tparams, we add it. These will be added in left-to-right order.
        let implicit_tparams_okay = tparams.is_empty();
        for p in legacy_tparams.iter() {
            if !tparams.contains(p) {
                if !implicit_tparams_okay {
                    self.error(
                        name.range,
                        format!(
                            "Class `{}` uses type variables not specified in `Generic` or `Protocol` base",
                            name.id,
                        ),
                    );
                }
                tparams.insert(p.clone());
            }
        }
        // TODO: This is a very bad variance inference algorithm.
        for tparam in tparams.iter_mut_unchecked() {
            if tparam.variance.is_none() {
                tparam.variance = Some(Variance::Invariant);
            }
        }
        self.type_params(name.range, tparams.into_iter().collect())
    }

    fn calculate_metaclass(
        &self,
        cls: &Class,
        raw_metaclass: Option<&Expr>,
        base_metaclasses: &[(&Name, &ClassType)],
    ) -> Option<ClassType> {
        let direct_meta = raw_metaclass.and_then(|x| self.direct_metaclass(cls, x));

        if let Some(metaclass) = direct_meta {
            Some(metaclass)
        } else {
            let mut inherited_meta: Option<ClassType> = None;
            for (_, m) in base_metaclasses.iter() {
                let m = (*m).clone();
                let accept_m = match &inherited_meta {
                    None => true,
                    Some(inherited) => self.solver().is_subset_eq(
                        &Type::ClassType(m.clone()),
                        &Type::ClassType(inherited.clone()),
                        self.type_order(),
                    ),
                };
                if accept_m {
                    inherited_meta = Some(m);
                }
            }
            inherited_meta
        }
    }

    fn check_base_class_metaclasses(
        &self,
        cls: &Class,
        metaclass: &ClassType,
        base_metaclasses: &[(&Name, &ClassType)],
    ) {
        // It is a runtime error to define a class whose metaclass (whether
        // specified directly or through inheritance) is not a subtype of all
        // base class metaclasses.
        let metaclass_type = Type::ClassType(metaclass.clone());
        for (base_name, m) in base_metaclasses.iter() {
            let base_metaclass_type = Type::ClassType((*m).clone());
            if !self
                .solver()
                .is_subset_eq(&metaclass_type, &base_metaclass_type, self.type_order())
            {
                self.error(
                    cls.name().range,
                    format!(
                        "Class `{}` has metaclass `{}` which is not a subclass of metaclass `{}` from base class `{}`",
                        cls.name().id,
                        metaclass_type,
                        base_metaclass_type,
                        base_name,
                    )
                );
            }
        }
    }

    fn direct_metaclass(&self, cls: &Class, raw_metaclass: &Expr) -> Option<ClassType> {
        match self.expr_untype(raw_metaclass) {
            Type::ClassType(meta) => {
                if self.solver().is_subset_eq(
                    &Type::ClassType(meta.clone()),
                    &Type::ClassType(self.stdlib.builtins_type()),
                    self.type_order(),
                ) {
                    Some(meta)
                } else {
                    self.error(
                        raw_metaclass.range(),
                        format!(
                            "Metaclass of `{}` has type `{}` which is not a subclass of `type`",
                            cls.name().id,
                            Type::ClassType(meta),
                        ),
                    );
                    None
                }
            }
            ty => {
                self.error(
                    cls.name().range,
                    format!(
                        "Metaclass of `{}` has type `{}` is not a simple class type.",
                        cls.name().id,
                        ty,
                    ),
                );
                None
            }
        }
    }

    /// Gets dataclass fields for an `@dataclass`-decorated class.
    fn get_dataclass_fields(
        &self,
        cls: &Class,
        bases_with_metadata: &[(ClassType, Arc<ClassMetadata>)],
    ) -> SmallSet<Name> {
        let mut all_fields = SmallSet::new();
        for (base, metadata) in bases_with_metadata.iter().rev() {
            if let Some(dataclass) = metadata.dataclass_metadata() {
                for name in &dataclass.fields {
                    if self.get_class_member(base.class_object(), name).is_some() {
                        all_fields.insert(name.clone());
                    }
                }
            }
        }
        for name in cls.fields() {
            if cls.is_field_annotated(name) {
                all_fields.insert(name.clone());
            }
        }
        all_fields
    }

    /// Gets a dataclass field as a function param.
    fn get_dataclass_param(&self, name: &Name, field: ClassField) -> Param {
        let ClassField(ClassFieldInner::Simple {
            ty,
            annotation: _,
            initialization,
            readonly: _,
        }) = field;
        let required = match initialization {
            ClassFieldInitialization::Class => Required::Required,
            ClassFieldInitialization::Instance => Required::Optional,
        };
        Param::Pos(name.clone(), ty, required)
    }

    fn get_dataclass_synthesized_field(&self, cls: &Class, name: &Name) -> Option<ClassField> {
        let metadata = self.get_metadata_for_class(cls);
        let dataclass = metadata.dataclass_metadata()?;
        // TODO(rechen): use a series of boolean flags to get rid of the unreachable!(...).
        if !dataclass.synthesized_fields.contains(name) {
            return None;
        }
        if *name == dunder::INIT {
            Some(self.get_dataclass_init(cls, &dataclass.fields))
        } else if *name == dunder::MATCH_ARGS {
            Some(self.get_dataclass_match_args(&dataclass.fields))
        } else {
            unreachable!("No implementation found for dataclass-synthesized method: {name}");
        }
    }

    /// Gets __init__ method for an `@dataclass`-decorated class.
    fn get_dataclass_init(&self, cls: &Class, fields: &SmallSet<Name>) -> ClassField {
        let mut params = vec![Param::Pos(
            Name::new("self"),
            cls.self_type(),
            Required::Required,
        )];
        for name in fields {
            let field = self.get_class_member(cls, name).unwrap().value;
            params.push(self.get_dataclass_param(name, field));
        }
        let ty = Type::Callable(
            Box::new(Callable::list(ParamList::new(params), Type::None)),
            CallableKind::Def,
        );
        ClassField(ClassFieldInner::Simple {
            ty,
            annotation: None,
            initialization: ClassFieldInitialization::Class,
            readonly: false,
        })
    }

    fn get_dataclass_match_args(&self, fields: &SmallSet<Name>) -> ClassField {
        let ts = fields
            .iter()
            .map(|name| Type::Literal(Lit::String(name.as_str().into())));
        let ty = Type::Tuple(Tuple::Concrete(ts.collect()));
        ClassField(ClassFieldInner::Simple {
            ty,
            annotation: None,
            initialization: ClassFieldInitialization::Class,
            readonly: false,
        })
    }
}
