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
use itertools::EitherOrBoth;
use itertools::Itertools;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtClassDef;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::ast::Ast;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::TArgs;
use crate::types::class_metadata::ClassMetadata;
use crate::types::literal::Lit;
use crate::types::special_form::SpecialForm;
use crate::types::type_var::Variance;
use crate::types::types::TParamInfo;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::display::count;
use crate::util::prelude::SliceExt;

/// Class members can fail to be
pub enum NoClassAttribute {
    NoClassMember,
    IsGenericMember,
}

pub struct Attribute {
    pub value: Type,
    defining_class: Class,
}

impl Attribute {
    pub fn defined_on(&self, cls: &Class) -> bool {
        self.defining_class == *cls
    }
}

/// Private helper type used to share part of the logic needed for the
/// binding-level work of finding legacy type parameters versus the type-level
/// work of computing inherticance information and the MRO.
#[derive(Debug, Clone)]
enum BaseClass {
    #[expect(dead_code)] // Will be used in the future
    NamedTuple,
    #[expect(dead_code)] // Will be used in the future
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

fn is_unbound_function(ty: &Type) -> bool {
    match ty {
        Type::Forall(_, t) => is_unbound_function(t),
        Type::Callable(_) => true,
        _ => false,
    }
}

fn bind_attribute(obj: Type, attr: Type) -> Type {
    if is_unbound_function(&attr) {
        Type::BoundMethod(Box::new(obj), Box::new(attr))
    } else {
        attr
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn class_definition(
        &self,
        x: &StmtClassDef,
        fields: SmallSet<Name>,
        bases: &[Expr],
        legacy_tparams: &[Idx<KeyLegacyTypeParam>],
    ) -> Class {
        let scoped_tparams = self.scoped_type_params(&x.type_params);
        let bases = bases.map(|x| self.base_class_of(x));
        let tparams = self.class_tparams(&x.name, scoped_tparams, bases, legacy_tparams);
        Class::new(
            x.name.clone(),
            self.module_info().dupe(),
            tparams,
            fields.clone(),
        )
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
                    _ => None,
                },
                _ => None,
            }
        } else {
            None
        }
    }

    fn base_class_of(&self, base_expr: &Expr) -> BaseClass {
        if let Some(special_base_class) = self.special_base_class(base_expr) {
            // This branch handles cases like `NamedTuple` or `Protocol`
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

    fn class_tparams(
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

    pub fn class_metadata_of(
        &self,
        cls: &Class,
        bases: &[Expr],
        keywords: &SmallMap<Name, Expr>,
    ) -> ClassMetadata {
        let bases_with_metadata: Vec<_> = bases
            .iter()
            .filter_map(|x| match self.base_class_of(x) {
                BaseClass::Expr(x) => match self.expr_untype(&x) {
                    Type::ClassType(c) => {
                        let class_metadata = self.get_metadata_for_class(c.class_object());
                        Some((c, class_metadata))
                    }
                    _ => None,
                },
                _ => None,
            })
            .collect();
        let (metaclasses, keywords): (Vec<_>, SmallMap<_, _>) =
            keywords.iter().partition_map(|(n, x)| match n.as_str() {
                "metaclass" => Either::Left(x),
                _ => Either::Right((n.clone(), self.expr(x, None))),
            });
        let metaclass =
            self.calculate_metaclass(cls, metaclasses.into_iter().next(), &bases_with_metadata);
        ClassMetadata::new(cls, bases_with_metadata, metaclass, keywords, self.errors())
    }

    fn calculate_metaclass(
        &self,
        cls: &Class,
        raw_metaclass: Option<&Expr>,
        bases_with_metadata: &[(ClassType, Arc<ClassMetadata>)],
    ) -> Option<ClassType> {
        let direct_meta = raw_metaclass.and_then(|x| self.direct_metaclass(cls, x));
        let base_metaclasses: Vec<_> = bases_with_metadata
            .iter()
            .filter_map(|(b, metadata)| metadata.metaclass().map(|m| (&b.name().id, m)))
            .collect();
        let metaclass = if let Some(metaclass) = direct_meta {
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
        };
        // It is a runtime error to define a class whose metaclass (whether
        // specified directly or through inheritance) is not a subtype of all
        // base class metaclasses.
        if let Some(metaclass) = &metaclass {
            let metaclass_type = Type::ClassType(metaclass.clone());
            for (base_name, m) in base_metaclasses.iter() {
                let base_metaclass_type = Type::ClassType((*m).clone());
                if !self.solver().is_subset_eq(
                    &metaclass_type,
                    &base_metaclass_type,
                    self.type_order(),
                ) {
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
        metaclass
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

    pub fn get_metadata_for_class(&self, cls: &Class) -> Arc<ClassMetadata> {
        self.get_from_class(cls, &KeyClassMetadata(ShortIdentifier::new(cls.name())))
    }

    fn check_and_create_targs(&self, cls: &Class, targs: Vec<Type>, range: TextRange) -> TArgs {
        let tparams = cls.tparams();
        let nargs = targs.len();
        let mut checked_targs = Vec::new();
        for pair in tparams.iter().zip_longest(targs) {
            match pair {
                EitherOrBoth::Both(_, arg) => {
                    checked_targs.push(arg);
                }
                EitherOrBoth::Left(param) if let Some(default) = &param.default => {
                    checked_targs.push(default.clone());
                }
                _ => {
                    self.error(
                        range,
                        format!(
                            "Expected {} for class `{}`, got {}.",
                            count(tparams.len(), "type argument"),
                            cls.name(),
                            nargs
                        ),
                    );
                    // We have either too few or too many targs. If too few, pad out with Any.
                    // If there are too many, the extra are ignored.
                    checked_targs
                        .extend(vec![Type::any_error(); tparams.len().saturating_sub(nargs)]);
                    break;
                }
            }
        }
        TArgs::new(checked_targs)
    }

    fn create_default_targs(
        &self,
        cls: &Class,
        // Placeholder for strict mode: we want to force callers to pass a range so
        // that we don't refactor in a way where none is available, but this is unused
        // because we do not have a strict mode yet.
        _range: Option<TextRange>,
    ) -> TArgs {
        let tparams = cls.tparams();
        if tparams.is_empty() {
            TArgs::default()
        } else {
            // TODO(stroxler): We should error here, but the error needs to be
            // configurable in the long run, and also suppressed in dependencies
            // no matter what the configuration is.
            //
            // Our plumbing isn't ready for that yet, so for now we are silently
            // using gradual type arguments.
            TArgs::new(vec![Type::any_error(); tparams.len()])
        }
    }

    /// Given a class and some (explicit) type arguments, construct a `Type`
    /// that represents the type of an instance of the class with those `targs`.
    pub fn specialize_as_class_type(
        &self,
        cls: &Class,
        targs: Vec<Type>,
        range: TextRange,
    ) -> ClassType {
        let targs = self.check_and_create_targs(cls, targs, range);
        ClassType::new(cls.dupe(), targs)
    }

    /// Given a class, create a `Type` that represents to an instance annotated
    /// with that class name. This will either have empty type arguments if the
    /// class is not generic, or type arguments populated with gradual types if
    /// it is (e.g. applying an annotation of `list` to a variable means
    /// `list[Any]`).
    ///
    /// We require a range because depending on the configuration we may raise
    /// a type error when a generic class is promoted using gradual types.
    pub fn promote_to_class_type(&self, cls: &Class, range: TextRange) -> ClassType {
        let targs = self.create_default_targs(cls, Some(range));
        ClassType::new(cls.dupe(), targs)
    }

    /// Private version of `promote_to_class_type` that does not potentially
    /// raise strict mode errors. Should only be used for unusual scenarios.
    fn promote_to_class_type_silently(&self, cls: &Class) -> ClassType {
        let targs = self.create_default_targs(cls, None);
        ClassType::new(cls.dupe(), targs)
    }

    /// Creates a type from the class with fresh variables for its type parameters.
    pub fn instantiate_fresh(&self, cls: &Class) -> Type {
        let qs = cls.tparams().quantified().collect::<Vec<_>>();
        let promoted_cls = Type::Type(Box::new(Type::ClassType(ClassType::new(
            cls.dupe(),
            TArgs::new(qs.map(|q| Type::Quantified(*q))),
        ))));
        self.solver()
            .fresh_quantified(qs.as_slice(), promoted_cls, self.uniques)
            .1
    }

    /// Get an ancestor `ClassType`, in terms of the type parameters of `class`.
    fn get_ancestor(&self, class: &Class, want: &Class) -> Option<ClassType> {
        self.get_metadata_for_class(class)
            .ancestors(self.stdlib)
            .find(|ancestor| ancestor.class_object() == want)
            .cloned()
    }

    /// Is `want` a superclass of `class` in the class hierarchy? Will return `false` if
    /// `want` is a protocol, unless it is explicitly marked as a base class in the MRO.
    pub fn has_superclass(&self, class: &Class, want: &Class) -> bool {
        class == want || self.get_ancestor(class, want).is_some()
    }

    /// Return the type representing `class` upcast to `want`, if `want` is a
    /// supertype of `class` in the class hierarchy. Will return `None` if
    /// `want` is not a superclass, including if `want` is a protocol (unless it
    /// explicitly appears in the MRO).
    pub fn as_superclass(&self, class: &ClassType, want: &Class) -> Option<ClassType> {
        if class.class_object() == want {
            Some(class.clone())
        } else {
            self.get_ancestor(class.class_object(), want)
                .map(|ancestor| ancestor.substitute(&class.substitution()))
        }
    }

    fn get_class_field(&self, cls: &Class, name: &Name) -> Option<Arc<Type>> {
        if cls.contains(name) {
            let ty = self.get_from_class(
                cls,
                &KeyClassField(ShortIdentifier::new(cls.name()), name.clone()),
            );
            Some(ty)
        } else {
            None
        }
    }

    fn get_class_member(&self, cls: &Class, name: &Name) -> Option<(Arc<Type>, Class)> {
        if let Some(member) = self.get_class_field(cls, name) {
            Some((member, cls.dupe()))
        } else {
            self.get_metadata_for_class(cls)
                .ancestors(self.stdlib)
                .filter_map(|ancestor| {
                    self.get_class_field(ancestor.class_object(), name)
                        .as_deref()
                        .map(|ty| {
                            let raw_member = ancestor.instantiate_member(ty.clone());
                            (Arc::new(raw_member), ancestor.class_object().dupe())
                        })
                })
                .next()
        }
    }

    pub fn has_attribute(&self, cls: &Class, name: &Name) -> bool {
        self.get_class_member(cls, name).is_some()
    }

    pub fn get_instance_attribute(&self, cls: &ClassType, name: &Name) -> Option<Attribute> {
        self.get_class_member(cls.class_object(), name)
            .map(|(member_ty, defining_class)| {
                let instantiated_ty = cls.instantiate_member((*member_ty).clone());
                Attribute {
                    value: bind_attribute(cls.self_type(), instantiated_ty),
                    defining_class,
                }
            })
    }

    fn depends_on_class_type_parameter(&self, cls: &Class, ty: &Type) -> bool {
        let tparams = cls.tparams();
        let mut qs = SmallSet::new();
        ty.collect_quantifieds(&mut qs);
        tparams.quantified().any(|q| qs.contains(&q))
    }

    pub fn get_class_attribute(
        &self,
        cls: &Class,
        name: &Name,
    ) -> Result<Attribute, NoClassAttribute> {
        match self.get_class_member(cls, name) {
            None => Err(NoClassAttribute::NoClassMember),
            Some((ty, defining_class)) => {
                if self.depends_on_class_type_parameter(cls, ty.as_ref()) {
                    Err(NoClassAttribute::IsGenericMember)
                } else if cls.is_enum(&|c| self.get_metadata_for_class(c)) {
                    // TODO(stroxler, yangdanny) Enums can contain attributes that are not
                    // members, we eventually need to implement enough checks to know the
                    // difference.
                    Ok(Attribute {
                        value: Type::Literal(Lit::Enum(Box::new((
                            self.promote_to_class_type_silently(cls),
                            name.to_owned(),
                        )))),
                        defining_class,
                    })
                } else {
                    Ok(Attribute {
                        value: ty.as_ref().clone(),
                        defining_class,
                    })
                }
            }
        }
    }

    /// Given an identifier, see whether it is bound to an enum class. If so,
    /// return a `ClassType` for the enum class, otherwise return `None`.
    pub fn get_enum_class_type(&self, name: Identifier) -> Option<ClassType> {
        match self.get(&Key::Usage(ShortIdentifier::new(&name))).deref() {
            Type::ClassDef(class) if class.is_enum(&|c| self.get_metadata_for_class(c)) => {
                // TODO(stroxler): Eventually, we should raise type errors on generic Enum because
                // this doesn't make semantic sense. But in the meantime we need to be robust against
                // this possibility.
                Some(self.promote_to_class_type_silently(class))
            }
            _ => None,
        }
    }
}
