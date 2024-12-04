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
use itertools::Itertools;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtClassDef;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use super::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::binding::Key;
use crate::alt::binding::KeyClassMetadata;
use crate::alt::binding::KeyExported;
use crate::alt::binding::KeyLegacyTypeParam;
use crate::ast::Ast;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::TArgs;
use crate::types::class_metadata::ClassMetadata;
use crate::types::literal::Lit;
use crate::types::special_form::SpecialForm;
use crate::types::types::Quantified;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

/// Class members can fail to be
pub enum NoClassAttribute {
    NoClassMember,
    IsGenericMember,
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

fn strip_first_argument(ty: &Type) -> Type {
    let (gs, ty) = ty.as_forall();
    let ty = match ty {
        Type::Callable(c) if c.args_len() >= Some(1) => {
            Type::callable(c.args.as_list().unwrap()[1..].to_owned(), c.ret.clone())
        }
        _ => ty.clone(),
    };
    Type::forall(
        if let Some(gs) = gs {
            gs.to_owned()
        } else {
            TParams::new(Vec::new())
        },
        ty,
    )
}

fn replace_return_type(ty: Type, ret: Type) -> Type {
    let (gs, ty) = ty.as_forall();
    let ty = match ty {
        Type::Callable(c) => Type::callable(c.args.as_list().unwrap().to_owned(), ret),
        _ => ty.clone(),
    };
    Type::forall(
        if let Some(gs) = gs {
            gs.to_owned()
        } else {
            TParams::new(Vec::new())
        },
        ty,
    )
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
        scoped_tparams: Vec<Quantified>,
        bases: Vec<BaseClass>,
        legacy: &[Idx<KeyLegacyTypeParam>],
    ) -> TParams {
        let legacy_quantifieds: SmallSet<_> = legacy
            .iter()
            .filter_map(|key| self.get_idx(*key).deref().parameter().cloned())
            .collect();
        // TODO(stroxler): There are a lot of checks, such as that `Generic` only appears once
        // and no non-type-vars are used, that we can more easily detect in a dedictated class
        // validation step that validates all the bases. We are deferring these for now.
        let mut generic_tparams = SmallSet::new();
        let mut protocol_tparams = SmallSet::new();
        for base in bases.iter() {
            match base {
                BaseClass::Generic(ts) => {
                    generic_tparams.extend(ts.iter().filter_map(|t| t.as_quantified().cloned()))
                }
                BaseClass::Protocol(ts) if !ts.is_empty() => {
                    protocol_tparams.extend(ts.iter().filter_map(|t| t.as_quantified().cloned()))
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
        for q in legacy_quantifieds.into_iter() {
            if !tparams.contains(&q) {
                if !implicit_tparams_okay {
                    self.error(
                        name.range,
                        format!(
                            "Class `{}` uses type variables not specified in `Generic` or `Protocol` base",
                            name.id,
                        ),
                    );
                }
                tparams.insert(q);
            }
        }
        TParams::new(tparams.into_iter().collect())
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

    // TODO(stroxler): Make sure inherited metaclasses are compatible; we are currently producing
    // the right metaclass when the code is valid, but failing to catch cases that crash at runtime.
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
        if targs.len() == tparams.len() {
            TArgs::new(targs)
        } else {
            self.error(
                range,
                format!(
                    "Expected {} type argument{} for class `{}`, got {}.",
                    tparams.len(),
                    if tparams.len() == 1 { "" } else { "s" },
                    cls.name(),
                    targs.len()
                ),
            );
            TArgs::new(vec![Type::any_error(); tparams.len()])
        }
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
        if tparams.0.is_empty() {
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
                &KeyExported::ClassField(ShortIdentifier::new(cls.name()), name.clone()),
            );
            Some(ty)
        } else {
            None
        }
    }

    fn get_class_member(&self, cls: &Class, name: &Name) -> Option<Arc<Type>> {
        if let Some(member) = self.get_class_field(cls, name) {
            Some(member)
        } else {
            self.get_metadata_for_class(cls)
                .ancestors(self.stdlib)
                .filter_map(|ancestor| {
                    self.get_class_field(ancestor.class_object(), name)
                        .as_deref()
                        .map(|ty| {
                            let raw_member = ancestor.instantiate_member(ty.clone());
                            Arc::new(raw_member)
                        })
                })
                .next()
        }
    }

    pub fn has_attribute(&self, cls: &Class, name: &Name) -> bool {
        self.get_class_member(cls, name).is_some()
    }

    pub fn get_instance_attribute(&self, cls: &ClassType, name: &Name) -> Option<Type> {
        self.get_class_member(cls.class_object(), name)
            .map(|member_ty| {
                let instantiated_ty = cls.instantiate_member((*member_ty).clone());
                strip_first_argument(&instantiated_ty)
            })
    }

    pub fn get_instance_attribute_or_error(
        &self,
        cls: &ClassType,
        name: &Name,
        range: TextRange,
    ) -> Type {
        self.get_instance_attribute(cls, name).unwrap_or_else(|| {
            self.error(
                range,
                format!(
                    "Object of class `{}` has no attribute `{}`",
                    cls.name(),
                    name
                ),
            )
        })
    }

    fn depends_on_class_type_parameter(&self, cls: &Class, ty: &Type) -> bool {
        let tparams = cls.tparams();
        let mut qs = SmallSet::new();
        ty.collect_quantifieds(&mut qs);
        tparams.quantified().any(|q| qs.contains(q))
    }

    pub fn get_class_attribute(&self, cls: &Class, name: &Name) -> Result<Type, NoClassAttribute> {
        match self.get_class_member(cls, name) {
            None => Err(NoClassAttribute::NoClassMember),
            Some(ty) => {
                if self.depends_on_class_type_parameter(cls, ty.as_ref()) {
                    Err(NoClassAttribute::IsGenericMember)
                } else if cls.is_enum(&|c| self.get_metadata_for_class(c)) {
                    // TODO(stroxler, yangdanny) Enums can contain attributes that are not
                    // members, we eventually need to implement enough checks to know the
                    // difference.
                    Ok(Type::Literal(Lit::Enum(Box::new((
                        self.promote_to_class_type_silently(cls),
                        name.to_owned(),
                    )))))
                } else {
                    Ok(ty.as_ref().clone())
                }
            }
        }
    }

    pub fn get_class_attribute_or_error(&self, cls: &Class, name: &Name, range: TextRange) -> Type {
        match self.get_class_attribute(cls, name) {
            Ok(ty) => ty,
            Err(NoClassAttribute::NoClassMember) => self.error(
                range,
                format!("Class `{}` has no class attribute `{}`", cls.name(), name),
            ),
            Err(NoClassAttribute::IsGenericMember) => self.error(
                range,
                format!(
                    "Generic attribute `{}` of class `{}` is not visible on the class",
                    name,
                    cls.name()
                ),
            ),
        }
    }

    pub fn get_init_method(&self, cls: &Class) -> Type {
        let init = Name::new("__init__");
        let init_ty = self.get_class_field(cls, &init);
        let ret = cls.self_type();
        match init_ty.as_deref() {
            Some(ty) => replace_return_type(strip_first_argument(ty), ret),
            None => Type::callable(Vec::new(), ret),
        }
    }

    pub fn get_constructor_for_class_type(&self, cls: &ClassType) -> Type {
        let init_ty = self.get_init_method(cls.class_object());
        cls.instantiate_member(init_ty)
    }

    pub fn get_constructor_for_class_object(&self, cls: &Class) -> Type {
        let init_ty = self.get_init_method(cls);
        let tparams = cls.tparams();
        Type::forall(tparams.clone(), init_ty)
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
