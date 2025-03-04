/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ops::Deref;

use dupe::Dupe;
use itertools::Either;
use itertools::Itertools;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::DataclassMetadata;
use crate::alt::types::class_metadata::EnumMetadata;
use crate::alt::types::class_metadata::NamedTupleMetadata;
use crate::alt::types::class_metadata::ProtocolMetadata;
use crate::alt::types::class_metadata::TypedDictMetadata;
use crate::ast::Ast;
use crate::binding::binding::Key;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::graph::index::Idx;
use crate::types::callable::CallableKind;
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
pub enum BaseClass {
    TypedDict,
    Generic(Vec<Type>),
    Protocol(Vec<Type>),
    Expr(Expr),
    CollectionsNamedTuple(TextRange),
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
    pub fn check_new_type_base(
        &self,
        base_type_and_range: &Option<(Type, TextRange)>,
        cls: &Class,
        errors: &ErrorCollector,
        is_new_type: bool,
    ) {
        match (base_type_and_range, is_new_type) {
            (Some((Type::ClassType(c), _)), false) => {
                let base_cls = c.class_object();
                let base_class_metadata = self.get_metadata_for_class(base_cls);
                if base_class_metadata.is_new_type() {
                    self.error(
                        errors,
                        cls.range(),
                        ErrorKind::Unknown,
                        "Subclassing a NewType not allowed".to_owned(),
                    );
                }
            }
            // TODO: raise an error for generic classes and other forbidden types such as hashable
            (Some((Type::ClassType(c), _)), true) => {
                let base_cls = c.class_object();
                let base_class_metadata = self.get_metadata_for_class(base_cls);
                if base_class_metadata.is_protocol() {
                    self.error(
                        errors,
                        cls.range(),
                        ErrorKind::Unknown,
                        "Second argument to NewType cannot be a protocol".to_owned(),
                    );
                }
            }
            (_, true) => {
                self.error(
                    errors,
                    cls.range(),
                    ErrorKind::Unknown,
                    "Second argument to NewType is incorrect".to_owned(),
                );
            }
            (_, _) => {}
        }
    }

    pub fn class_metadata_of(
        &self,
        cls: &Class,
        bases: &[Expr],
        keywords: &[(Name, Expr)],
        decorators: &[Idx<Key>],
        is_new_type: bool,
        special_base: &Option<Box<BaseClass>>,
        errors: &ErrorCollector,
    ) -> ClassMetadata {
        let mut is_typed_dict = false;
        let mut named_tuple_metadata = None;
        let mut enum_metadata = None;
        let mut dataclass_metadata = None;
        let mut bases: Vec<BaseClass> = bases.map(|x| self.base_class_of(x, errors));
        if let Some(box special_base) = special_base {
            bases.push(special_base.clone());
        }
        let mut protocol_metadata = if bases.iter().any(|x| matches!(x, BaseClass::Protocol(_))) {
            Some(ProtocolMetadata {
                members: cls.fields().cloned().collect(),
            })
        } else {
            None
        };
        let mut has_base_any = false;
        let bases_with_metadata = bases
            .iter()
            .filter_map(|x| {
                let base_type_and_range = match x {
                    BaseClass::Expr(x) => Some((self.expr_untype(x, errors), x.range())),
                    BaseClass::TypedDict => {
                        is_typed_dict = true;
                        None
                    }
                    BaseClass::CollectionsNamedTuple(range) => {
                        Some((self.stdlib.named_tuple().to_type(), *range))
                    }
                    _ => None,
                };
                self.check_new_type_base(&base_type_and_range, cls, errors, is_new_type);
                match base_type_and_range {
                    Some((Type::ClassType(c), range)) => {
                        let base_cls = c.class_object();
                        let base_class_metadata = self.get_metadata_for_class(base_cls);
                        if base_class_metadata.is_typed_dict() {
                            is_typed_dict = true;
                        }
                        if base_cls.has_qname("typing", "NamedTuple")
                        {
                            if named_tuple_metadata.is_none() {
                                named_tuple_metadata = Some(NamedTupleMetadata {
                                    elements: self.get_named_tuple_elements(cls)
                                })
                            }
                        } else if let Some(base_named_tuple) = base_class_metadata.named_tuple_metadata() {
                            if named_tuple_metadata.is_none() {
                                named_tuple_metadata = Some(base_named_tuple.clone());
                            }
                        }
                        if let Some(proto) = &mut protocol_metadata {
                            if let Some(base_proto) = base_class_metadata.protocol_metadata() {
                                proto.members.extend(base_proto.members.iter().cloned());
                            } else {
                                self.error(errors,
                                    range,
                                    ErrorKind::InvalidInheritance,
                                    "If `Protocol` is included as a base class, all other bases must be protocols.".to_owned(),
                                );
                            }
                        }
                        if dataclass_metadata.is_none() && let Some(base_dataclass) = base_class_metadata.dataclass_metadata() {
                            // If we inherit from a dataclass, inherit its metadata. Note that if this class is
                            // itself decorated with @dataclass, we'll compute new metadata and overwrite this.
                            dataclass_metadata = Some(base_dataclass.inherit());
                        }
                        Some((c, base_class_metadata))
                    }
                    Some((Type::Tuple(Tuple::Concrete(ts)), _)) => {
                        let class_ty = self.stdlib.tuple(self.unions(ts));
                        let metadata = self.get_metadata_for_class(class_ty.class_object());
                        Some((class_ty, metadata))
                    }
                    Some((Type::Tuple(Tuple::Unbounded(t)), _)) => {
                        let class_ty = self.stdlib.tuple(*t);
                        let metadata = self.get_metadata_for_class(class_ty.class_object());
                        Some((class_ty, metadata))
                    }
                    Some((Type::TypedDict(typed_dict), _)) => {
                        is_typed_dict = true;
                        let class_object = typed_dict.class_object();
                        let class_metadata = self.get_metadata_for_class(class_object);
                        Some((
                            typed_dict.as_class_type(),
                            class_metadata,
                        ))
                    }
                    // todo zeina: Ideally, we can directly add this class to the list of base classes. Revist this when fixing the "Any" representation.  
                    Some((Type::Any(_), _)) =>  {has_base_any = true; None}
                    _ => None,
                }
            })
            .collect::<Vec<_>>();
        if named_tuple_metadata.is_some() && bases_with_metadata.len() > 1 {
            self.error(
                errors,
                cls.range(),
                ErrorKind::InvalidInheritance,
                "Named tuples do not support multiple inheritance".to_owned(),
            );
        }
        let (metaclasses, keywords): (Vec<_>, Vec<(_, _)>) =
            keywords.iter().partition_map(|(n, x)| match n.as_str() {
                "metaclass" => Either::Left(x),
                _ => Either::Right((n.clone(), self.expr(x, None, errors))),
            });
        let typed_dict_metadata = if is_typed_dict {
            let is_total = !keywords.iter().any(|(n, t)| {
                n.as_str() == "total" && matches!(t, Type::Literal(Lit::Bool(false)))
            });
            let fields = self.get_typed_dict_fields(cls, &bases_with_metadata, is_total);
            Some(TypedDictMetadata { fields })
        } else {
            None
        };
        let base_metaclasses = bases_with_metadata
            .iter()
            .filter_map(|(b, metadata)| metadata.metaclass().map(|m| (b.name(), m)))
            .collect::<Vec<_>>();
        let metaclass = self.calculate_metaclass(
            cls,
            metaclasses.into_iter().next(),
            &base_metaclasses,
            errors,
        );
        if let Some(metaclass) = &metaclass {
            self.check_base_class_metaclasses(cls, metaclass, &base_metaclasses, errors);
            if self.solver().is_subset_eq(
                &Type::ClassType(metaclass.clone()),
                &Type::ClassType(self.stdlib.enum_meta()),
                self.type_order(),
            ) {
                if !cls.tparams().is_empty() {
                    self.error(
                        errors,
                        cls.range(),
                        ErrorKind::Unknown,
                        "Enums may not be generic.".to_owned(),
                    );
                }
                enum_metadata = Some(EnumMetadata {
                    // A generic enum is an error, but we create Any type args anyway to handle it gracefully.
                    cls: ClassType::new(cls.dupe(), self.create_default_targs(cls, None)),
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
                    errors,
                    cls.range(),
                    ErrorKind::InvalidInheritance,
                    "Typed dictionary definitions may not specify a metaclass.".to_owned(),
                );
            }
        }
        for decorator in decorators {
            let ty_decorator = self.get_idx(*decorator);
            if let Some(CalleeKind::Callable(CallableKind::Dataclass(kws))) =
                ty_decorator.callee_kind()
            {
                let dataclass_fields = self.get_dataclass_fields(cls, &bases_with_metadata);
                dataclass_metadata = Some(DataclassMetadata {
                    fields: dataclass_fields,
                    kws: *kws,
                });
            }
        }
        if is_typed_dict
            && let Some(bad) = bases_with_metadata.iter().find(|x| !x.1.is_typed_dict())
        {
            self.error(errors,
                cls.range(),
                ErrorKind::Unknown,
                format!("`{}` is not a typed dictionary. Typed dictionary definitions may only extend other typed dictionaries.", bad.0),
            );
        }
        ClassMetadata::new(
            cls,
            bases_with_metadata,
            metaclass,
            keywords,
            typed_dict_metadata,
            named_tuple_metadata,
            enum_metadata,
            protocol_metadata,
            dataclass_metadata,
            has_base_any,
            is_new_type,
            errors,
        )
    }

    /// This helper deals with special cases where we want to intercept an `Expr`
    /// manually and create a special variant of `BaseClass` instead of calling
    /// `expr_untype` and creating a `BaseClass::Type`.
    ///
    /// TODO(stroxler): See if there's a way to express this more clearly in the types.
    fn special_base_class(&self, base_expr: &Expr, errors: &ErrorCollector) -> Option<BaseClass> {
        if matches!(base_expr, Expr::Name(_) | Expr::Attribute(_)) {
            match self.expr_infer(base_expr, errors) {
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

    pub fn base_class_of(&self, base_expr: &Expr, errors: &ErrorCollector) -> BaseClass {
        if let Some(special_base_class) = self.special_base_class(base_expr, errors) {
            // This branch handles cases like `Protocol`
            special_base_class
        } else if let Expr::Subscript(subscript) = base_expr
            && let Some(mut special_base_class) = self.special_base_class(&subscript.value, errors)
            && special_base_class.can_apply()
        {
            // This branch handles `Generic[...]` and `Protocol[...]`
            let args = Ast::unpack_slice(&subscript.slice).map(|x| self.expr_untype(x, errors));
            special_base_class.apply(args);
            special_base_class
        } else {
            // This branch handles all other base classes.
            BaseClass::Expr(base_expr.clone())
        }
    }

    pub fn class_tparams(
        &self,
        name: &Identifier,
        scoped_tparams: Vec<TParamInfo>,
        bases: Vec<BaseClass>,
        legacy: &[Idx<KeyLegacyTypeParam>],
        errors: &ErrorCollector,
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
                    errors,
                    name.range,
                    ErrorKind::Unknown,
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
                errors,
                name.range,
                ErrorKind::Unknown,
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
                    self.error(errors,
                        name.range,
                        ErrorKind::Unknown,
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
        self.type_params(name.range, tparams.into_iter().collect(), errors)
    }

    fn calculate_metaclass(
        &self,
        cls: &Class,
        raw_metaclass: Option<&Expr>,
        base_metaclasses: &[(&Name, &ClassType)],
        errors: &ErrorCollector,
    ) -> Option<ClassType> {
        let direct_meta = raw_metaclass.and_then(|x| self.direct_metaclass(cls, x, errors));

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
        errors: &ErrorCollector,
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
                self.error(errors,
                    cls.range(),
                    ErrorKind::InvalidInheritance,
                    format!(
                        "Class `{}` has metaclass `{}` which is not a subclass of metaclass `{}` from base class `{}`",
                        cls.name(),
                        metaclass_type,
                        base_metaclass_type,
                        base_name,
                    ),
                );
            }
        }
    }

    fn direct_metaclass(
        &self,
        cls: &Class,
        raw_metaclass: &Expr,
        errors: &ErrorCollector,
    ) -> Option<ClassType> {
        match self.expr_untype(raw_metaclass, errors) {
            Type::ClassType(meta) => {
                if self.solver().is_subset_eq(
                    &Type::ClassType(meta.clone()),
                    &Type::ClassType(self.stdlib.builtins_type()),
                    self.type_order(),
                ) {
                    Some(meta)
                } else {
                    self.error(
                        errors,
                        raw_metaclass.range(),
                        ErrorKind::InvalidInheritance,
                        format!(
                            "Metaclass of `{}` has type `{}` which is not a subclass of `type`",
                            cls.name(),
                            Type::ClassType(meta),
                        ),
                    );
                    None
                }
            }
            ty => {
                self.error(
                    errors,
                    cls.range(),
                    ErrorKind::InvalidInheritance,
                    format!(
                        "Metaclass of `{}` has type `{}` is not a simple class type.",
                        cls.name(),
                        ty,
                    ),
                );
                None
            }
        }
    }
}
