/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
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
use crate::alt::attr::AccessNotAllowed;
use crate::alt::attr::AttributeAccess;
use crate::ast::Ast;
use crate::binding::binding::ClassFieldInitialization;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::dunder;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Annotation;
use crate::types::annotation::Qualifier;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::Substitution;
use crate::types::class::TArgs;
use crate::types::class_metadata::ClassMetadata;
use crate::types::literal::Enum;
use crate::types::literal::Lit;
use crate::types::special_form::SpecialForm;
use crate::types::type_var::Variance;
use crate::types::types::TParamInfo;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::types::types::TypedDict;
use crate::types::types::TypedDictField;
use crate::util::display::count;
use crate::util::prelude::SliceExt;

/// Raw information about an attribute declared somewhere in a class. We need to
/// know whether it is initialized in the class body in order to determine
/// both visibility rules and whether method binding should be performed.
#[derive(Debug, Clone)]
pub struct ClassField {
    ty: Type,
    annotation: Option<Annotation>,
    initialization: ClassFieldInitialization,
}

impl ClassField {
    pub fn new(
        ty: Type,
        annotation: Option<Annotation>,
        initialization: ClassFieldInitialization,
    ) -> Self {
        Self {
            ty,
            annotation,
            initialization,
        }
    }

    pub fn recursive() -> Self {
        Self {
            ty: Type::any_implicit(),
            annotation: None,
            initialization: ClassFieldInitialization::Class,
        }
    }

    pub fn visit_type_mut(&mut self, mut f: &mut dyn FnMut(&mut Type)) {
        f(&mut self.ty);
        for a in self.annotation.iter_mut() {
            a.visit_type_mut(&mut f);
        }
    }
}

impl Display for ClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let initialized = match self.initialization {
            ClassFieldInitialization::Class => "initialized in body",
            ClassFieldInitialization::Instance => "not initialized in body",
        };
        write!(f, "@{} ({})", self.ty, initialized)
    }
}

/// Result of looking up a member of a class in the MRO, including a handle to the defining
/// class which may be some ancestor.
///
/// For example, given `class A: x: int; class B(A): pass`, the defining class
/// for attribute `x` is `A` even when `x` is looked up on `B`.
struct WithDefiningClass<T> {
    value: T,
    defining_class: Class,
}

impl<T> WithDefiningClass<T> {
    fn defined_on(&self, cls: &Class) -> bool {
        self.defining_class == *cls
    }
}

/// Private helper type used to share part of the logic needed for the
/// binding-level work of finding legacy type parameters versus the type-level
/// work of computing inherticance information and the MRO.
#[derive(Debug, Clone)]
enum BaseClass {
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
        Type::Callable(_, _) => true,
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
        let scoped_tparams = self.scoped_type_params(x.type_params.as_deref());
        let bases = bases.map(|x| self.base_class_of(x));
        let tparams = self.class_tparams(&x.name, scoped_tparams, bases, legacy_tparams);
        Class::new(
            x.name.clone(),
            self.module_info().dupe(),
            tparams,
            fields.clone(),
        )
    }

    pub fn functional_class_definition(&self, name: &Identifier, fields: &SmallSet<Name>) -> Class {
        Class::new(
            name.clone(),
            self.module_info().dupe(),
            TParams::default(),
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
                    SpecialForm::TypedDict => Some(BaseClass::TypedDict),
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
        keywords: &[(Name, Expr)],
    ) -> ClassMetadata {
        let mut is_typed_dict = false;
        let mut is_named_tuple = false;
        let bases: Vec<BaseClass> = bases.iter().map(|x| self.base_class_of(x)).collect();
        let is_protocol = bases.iter().any(|x| matches!(x, BaseClass::Protocol(_)));
        let bases_with_metadata: Vec<_> = bases
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
            .collect();
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
        let metaclass =
            self.calculate_metaclass(cls, metaclasses.into_iter().next(), &bases_with_metadata);
        if is_typed_dict && metaclass.is_some() {
            self.error(
                cls.name().range,
                "Typed dictionary definitions may not specify a metaclass.".to_owned(),
            );
        }
        ClassMetadata::new(
            cls,
            bases_with_metadata,
            metaclass,
            keywords,
            is_typed_dict,
            is_named_tuple,
            is_protocol,
            self.errors(),
        )
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

    pub fn get_enum(&self, ty: &Type) -> Option<Enum> {
        if let Type::ClassType(cls) = ty {
            self.get_enum_from_class_type(cls)
        } else {
            None
        }
    }

    pub fn get_enum_from_class_type(&self, cls: &ClassType) -> Option<Enum> {
        let metadata = self.get_metadata_for_class(cls.class_object());
        metadata.metaclass().and_then(|m| {
            if self.solver().is_subset_eq(
                &Type::ClassType(m.clone()),
                &Type::ClassType(self.stdlib.enum_meta()),
                self.type_order(),
            ) {
                Some(Enum { cls: cls.clone() })
            } else {
                None
            }
        })
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

    /// Given a class or typed dictionary and some (explicit) type arguments, construct a `Type`
    /// that represents the type of an instance of the class or typed dictionary with those `targs`.
    pub fn specialize(&self, cls: &Class, targs: Vec<Type>, range: TextRange) -> Type {
        let targs = self.check_and_create_targs(cls, targs, range);
        let metadata = self.get_metadata_for_class(cls);
        if metadata.is_typed_dict() {
            Type::TypedDict(TypedDict::new(
                cls.dupe(),
                targs.clone(),
                self.get_typed_dict_fields(cls, &targs),
            ))
        } else {
            Type::ClassType(ClassType::new(cls.dupe(), targs))
        }
    }

    /// Given a class or typed dictionary, create a `Type` that represents to an instance annotated
    /// with the class or typed dictionary's bare name. This will either have empty type arguments if the
    /// class or typed dictionary is not generic, or type arguments populated with gradual types if
    /// it is (e.g. applying an annotation of `list` to a variable means
    /// `list[Any]`).
    ///
    /// We require a range because depending on the configuration we may raise
    /// a type error when a generic class or typed dictionary is promoted using gradual types.
    pub fn promote(&self, cls: &Class, range: TextRange) -> Type {
        let metadata = self.get_metadata_for_class(cls);
        let targs = self.create_default_targs(cls, Some(range));
        if metadata.is_typed_dict() {
            Type::TypedDict(TypedDict::new(
                cls.dupe(),
                targs.clone(),
                self.get_typed_dict_fields(cls, &targs),
            ))
        } else {
            Type::ClassType(ClassType::new(cls.dupe(), targs))
        }
    }

    /// Private version of `promote` that does not potentially
    /// raise strict mode errors. Should only be used for unusual scenarios.
    fn promote_silently(&self, cls: &Class) -> Type {
        let metadata = self.get_metadata_for_class(cls);
        let targs = self.create_default_targs(cls, None);
        if metadata.is_typed_dict() {
            Type::TypedDict(TypedDict::new(
                cls.dupe(),
                targs.clone(),
                self.get_typed_dict_fields(cls, &targs),
            ))
        } else {
            Type::ClassType(ClassType::new(cls.dupe(), targs))
        }
    }

    pub fn unwrap_class_object_silently(&self, ty: &Type) -> Option<Type> {
        match ty {
            Type::ClassDef(c) => Some(self.promote_silently(c)),
            Type::TypeAlias(ta) => self.unwrap_class_object_silently(&ta.as_value(self.stdlib)),
            _ => None,
        }
    }

    /// Creates a type from the class with fresh variables for its type parameters.
    pub fn instantiate_fresh(&self, cls: &Class) -> Type {
        let metadata = self.get_metadata_for_class(cls);
        let qs = cls.tparams().quantified().collect::<Vec<_>>();
        let targs = TArgs::new(qs.map(|q| Type::Quantified(*q)));
        let promoted_cls = if metadata.is_typed_dict() {
            Type::type_form(Type::TypedDict(TypedDict::new(
                cls.dupe(),
                targs.clone(),
                self.get_typed_dict_fields(cls, &targs),
            )))
        } else {
            Type::type_form(Type::ClassType(ClassType::new(cls.dupe(), targs)))
        };
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

    fn get_class_field(&self, cls: &Class, name: &Name) -> Option<ClassField> {
        if cls.contains(name) {
            let field = self.get_from_class(
                cls,
                &KeyClassField(ShortIdentifier::new(cls.name()), name.clone()),
            );
            Some((*field).clone())
        } else {
            None
        }
    }

    fn get_class_member(&self, cls: &Class, name: &Name) -> Option<(ClassField, Class)> {
        if let Some(field) = self.get_class_field(cls, name) {
            Some((field, cls.dupe()))
        } else {
            self.get_metadata_for_class(cls)
                .ancestors(self.stdlib)
                .filter_map(|ancestor| {
                    self.get_class_field(ancestor.class_object(), name)
                        .map(|mut field| {
                            field.ty = ancestor.instantiate_member(field.ty);
                            (field, ancestor.class_object().dupe())
                        })
                })
                .next()
        }
    }

    // Get every member of a class, including those declared in parent classes.
    pub fn get_all_members(&self, cls: &Class) -> SmallMap<Name, (ClassField, Class)> {
        let mut members = SmallMap::new();
        for name in cls.fields() {
            if let Some(field) = self.get_class_field(cls, name) {
                members.insert(name.clone(), (field, cls.dupe()));
            }
        }
        for ancestor in self.get_metadata_for_class(cls).ancestors(self.stdlib) {
            for name in ancestor.class_object().fields() {
                if !members.contains_key(name) {
                    if let Some(mut field) = self.get_class_field(ancestor.class_object(), name) {
                        field.ty = ancestor.instantiate_member(field.ty);
                        members.insert(name.clone(), (field, ancestor.class_object().dupe()));
                    }
                }
            }
        }
        members
    }

    fn get_instance_attribute_with_defining_class(
        &self,
        cls: &ClassType,
        name: &Name,
    ) -> Option<WithDefiningClass<Type>> {
        self.get_class_member(cls.class_object(), name)
            .map(|(member, defining_class)| {
                let instantiated_ty = cls.instantiate_member(member.ty);
                let ty = match member.initialization {
                    ClassFieldInitialization::Class => {
                        bind_attribute(cls.self_type(), instantiated_ty)
                    }
                    ClassFieldInitialization::Instance => instantiated_ty,
                };
                WithDefiningClass {
                    value: ty,
                    defining_class,
                }
            })
    }

    pub fn get_instance_attribute(&self, cls: &ClassType, name: &Name) -> Option<Type> {
        self.get_instance_attribute_with_defining_class(cls, name)
            .map(|attr| attr.value)
    }

    fn depends_on_class_type_parameter(&self, cls: &Class, ty: &Type) -> bool {
        let tparams = cls.tparams();
        let mut qs = SmallSet::new();
        ty.collect_quantifieds(&mut qs);
        tparams.quantified().any(|q| qs.contains(&q))
    }

    /// Gets an attribute from a class definition.
    ///
    /// Returns `None` if there is no such attribute, otherwise an `Attribute` object
    /// that describes whether access is allowed and the type if so.
    ///
    /// Access is disallowed for instance-only attributes and for attributes whose
    /// type contains a class-scoped type parameter - e.g., `class A[T]: x: T`.
    pub fn get_class_attribute(&self, cls: &Class, name: &Name) -> Option<AttributeAccess> {
        let (member, _) = self.get_class_member(cls, name)?;
        if self.depends_on_class_type_parameter(cls, &member.ty) {
            Some(AttributeAccess::not_allowed(
                AccessNotAllowed::ClassAttributeIsGeneric(cls.clone()),
            ))
        } else {
            match member.initialization {
                ClassFieldInitialization::Instance => Some(AttributeAccess::not_allowed(
                    AccessNotAllowed::ClassUseOfInstanceAttribute(cls.clone()),
                )),
                ClassFieldInitialization::Class => {
                    if let Some(e) = self.get_enum(&self.promote_silently(cls))
                        && let Some(member) = e.get_member(name)
                    {
                        Some(AttributeAccess::allowed(Type::Literal(member)))
                    } else {
                        Some(AttributeAccess::allowed(member.ty))
                    }
                }
            }
        }
    }

    /// Get the class's `__new__` method.
    pub fn get_dunder_new(&self, cls: &ClassType) -> Option<Type> {
        let new_attr = self
            .get_class_member(cls.class_object(), &dunder::NEW)
            .map(|(member, defining_class)| WithDefiningClass {
                value: cls.instantiate_member(member.ty),
                defining_class,
            })?;
        if new_attr.defined_on(self.stdlib.object_class_type().class_object()) {
            // The default behavior of `object.__new__` is already baked into our implementation of
            // class construction; we only care about `__new__` if it is overridden.
            return None;
        }
        Some(new_attr.value)
    }

    /// Get the class's `__init__` method, if we should analyze it
    /// We skip analyzing the call to `__init__` if:
    /// (1) it isn't defined (possible if we've been passed a custom typeshed), or
    /// (2) the class overrides `object.__new__` but not `object.__init__`, in wich case the
    ///     `__init__` call always succeeds at runtime.
    pub fn get_dunder_init(&self, cls: &ClassType, overrides_new: bool) -> Option<Type> {
        let init_method = self.get_instance_attribute_with_defining_class(cls, &dunder::INIT)?;
        if !(overrides_new
            && init_method.defined_on(self.stdlib.object_class_type().class_object()))
        {
            Some(init_method.value)
        } else {
            None
        }
    }

    /// Get the metaclass `__call__` method.
    pub fn get_metaclass_dunder_call(&self, cls: &ClassType) -> Option<Type> {
        let metadata = self.get_metadata_for_class(cls.class_object());
        let metaclass = metadata.metaclass()?;
        let attr = self.get_instance_attribute_with_defining_class(metaclass, &dunder::CALL)?;
        if attr.defined_on(self.stdlib.builtins_type().class_object()) {
            // The behavior of `type.__call__` is already baked into our implementation of constructors,
            // so we can skip analyzing it at the type level.
            None
        } else {
            Some(attr.value)
        }
    }

    /// Given an identifier, see whether it is bound to an enum class. If so,
    /// return the enum, otherwise return `None`.
    pub fn get_enum_from_name(&self, name: Identifier) -> Option<Enum> {
        self.get_enum_from_key(
            self.bindings()
                .key_to_idx(&Key::Usage(ShortIdentifier::new(&name))),
        )
    }

    pub fn get_enum_from_key(&self, key: Idx<Key>) -> Option<Enum> {
        // TODO(stroxler): Eventually, we should raise type errors on generic Enum because
        // this doesn't make semantic sense. But in the meantime we need to be robust against
        // this possibility.
        match self.get_idx(key).deref() {
            Type::ClassDef(class) => self.get_enum(&self.promote_silently(class)),
            _ => None,
        }
    }

    pub fn get_typed_dict_from_key(&self, key: Idx<Key>) -> Option<TypedDict> {
        match self.get_idx(key).deref() {
            Type::ClassDef(cls) => match self.promote_silently(cls) {
                Type::TypedDict(typed_dict) => Some(typed_dict),
                _ => None,
            },
            _ => None,
        }
    }

    fn get_typed_dict_fields(&self, cls: &Class, targs: &TArgs) -> SmallMap<Name, TypedDictField> {
        let tparams = cls.tparams();
        let substitution = Substitution::new(
            tparams
                .quantified()
                .zip(targs.as_slice().iter().cloned())
                .collect(),
        );
        self.get_all_members(cls)
            .iter()
            .filter_map(|(name, (field, cls))| {
                if let ClassField {
                    annotation:
                        Some(Annotation {
                            ty: Some(ty),
                            qualifiers,
                        }),
                    ..
                } = field
                {
                    let is_total = self
                        .get_metadata_for_class(cls)
                        .get_keyword(&Name::new("total"))
                        .map_or(true, |ty| match ty {
                            Type::Literal(Lit::Bool(b)) => b,
                            _ => true,
                        });
                    Some((
                        name.clone(),
                        TypedDictField {
                            ty: substitution.substitute(ty.clone()),
                            required: if qualifiers.contains(&Qualifier::Required) {
                                true
                            } else if qualifiers.contains(&Qualifier::NotRequired) {
                                false
                            } else {
                                is_total
                            },
                            read_only: qualifiers.contains(&Qualifier::ReadOnly),
                        },
                    ))
                } else {
                    None
                }
            })
            .collect()
    }
}
