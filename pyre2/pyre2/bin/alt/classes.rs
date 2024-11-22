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
use ruff_python_ast::Identifier;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use super::answers::AnswersSolver;
use crate::alt::binding::Key;
use crate::alt::binding::KeyBaseClass;
use crate::alt::binding::KeyLegacyTypeParam;
use crate::alt::binding::KeyMro;
use crate::alt::binding::KeyTypeParams;
use crate::ast::Ast;
use crate::graph::index::Idx;
use crate::types::base_class::BaseClass;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::Substitution;
use crate::types::class::TArgs;
use crate::types::literal::Lit;
use crate::types::mro::Mro;
use crate::types::types::QuantifiedVec;
use crate::types::types::Type;
use crate::util::prelude::SliceExt;

/// Class members can fail to be
pub enum NoClassAttribute {
    NoClassMember,
    IsGenericMember,
}

fn strip_first_argument(ty: &Type) -> Type {
    let (gs, ty) = ty.as_forall();
    let ty = match ty {
        Type::Callable(c) if c.args_len() >= Some(1) => {
            Type::callable(c.args.as_list().unwrap()[1..].to_owned(), c.ret.clone())
        }
        _ => ty.clone(),
    };
    Type::forall(gs.to_owned(), ty)
}

fn replace_return_type(ty: Type, ret: Type) -> Type {
    let (gs, ty) = ty.as_forall();
    let ty = match ty {
        Type::Callable(c) => Type::callable(c.args.as_list().unwrap().to_owned(), ret),
        _ => ty.clone(),
    };
    Type::forall(gs.to_owned(), ty)
}

impl<'a> AnswersSolver<'a> {
    pub fn base_class_of(&self, base_expr: &Expr, self_type: Idx<Key>) -> BaseClass {
        let mut base = self.expr_base_class(base_expr);
        let self_type = &*self.get_idx(self_type);
        base.subst_self_type_mut(self_type);
        base
    }

    /// This helper deals with special cases where we want to intercept an `Expr`
    /// manually and create a special variant of `BaseClass` instead of calling
    /// `expr_untype` and creating a `BaseClass::Type`.
    ///
    /// TODO(stroxler): See if there's a way to express this more clearly in the types.
    fn special_base_class(&self, base_expr: &Expr) -> Option<BaseClass> {
        if let Expr::Name(name) = base_expr {
            match &*self.get(&Key::Usage(Ast::expr_name_identifier(name.clone()))) {
                Type::Type(box Type::SpecialForm(special)) => special.to_base_class(),
                _ => None,
            }
        } else {
            None
        }
    }

    fn expr_base_class(&self, base_expr: &Expr) -> BaseClass {
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
            BaseClass::Type(self.expr_untype(base_expr))
        }
    }

    fn get_substitution(&self, class: &ClassType) -> Substitution {
        class.substitution(&self.get_tparams_for_class(class.class_object()))
    }

    pub fn tparams_of(&self, cls: &Class, legacy: &[Idx<KeyLegacyTypeParam>]) -> QuantifiedVec {
        let legacy_tparams: SmallMap<_, _> = legacy
            .iter()
            .filter_map(|key| {
                self.get_idx(*key)
                    .deref()
                    .parameter()
                    .map(|q| (self.bindings().idx_to_key(*key).0.clone(), *q))
            })
            .collect();
        let scoped_tparams = cls.scoped_tparams();
        // TODO(stroxler): Add checks for:
        // - Use of both Generic and Protocol with parameters as arguments.
        // - Use of both a Generic/Protocol base and implicit generic parameters.
        //
        // Note that we should *not* validate the absence of legacy parameters when there are
        // scoped parameters, that is handled by CheckLegacyTypeParam.
        let mut tparams = scoped_tparams.clone();
        for (identifier, q) in legacy_tparams.iter() {
            let entry = tparams.entry(identifier.id.clone());
            entry.or_insert_with(|| *q);
        }
        QuantifiedVec(tparams.values().copied().collect())
    }

    pub fn mro_of(&self, cls: &Class) -> Mro {
        Mro::new(
            cls,
            &|cls| self.get_mro_for_class(cls),
            &|cls| self.bases_of_class(cls),
            &|base| self.get_substitution(base),
            self.errors(),
        )
    }

    pub fn get_tparams_for_class(&self, cls: &Class) -> Arc<QuantifiedVec> {
        self.get_from_class(cls, &KeyTypeParams(cls.name().clone()))
    }

    pub fn get_mro_for_class(&self, cls: &Class) -> Arc<Mro> {
        self.get_from_class(cls, &KeyMro(cls.name().clone()))
    }

    fn get_base_class_index(&self, cls: &Class, base_idx: usize) -> Arc<BaseClass> {
        self.get_from_class(cls, &KeyBaseClass(cls.name().clone(), base_idx))
    }

    pub fn bases_of_class(&self, cls: &Class) -> Vec<Arc<BaseClass>> {
        (0..cls.n_bases())
            .map(|base_idx| self.get_base_class_index(cls, base_idx))
            .collect()
    }

    fn check_and_create_targs(&self, cls: &Class, targs: Vec<Type>, range: TextRange) -> TArgs {
        let tparams = self.get_tparams_for_class(cls);
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
        let tparams = self.get_tparams_for_class(cls);
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
        ClassType::create_with_validated_targs(cls.dupe(), targs)
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
        ClassType::create_with_validated_targs(cls.dupe(), targs)
    }

    /// Private version of `promote_to_class_type` that does not potentially
    /// raise strict mode errors. Should only be used for unusual scenarios.
    fn promote_to_class_type_silently(&self, cls: &Class) -> ClassType {
        let targs = self.create_default_targs(cls, None);
        ClassType::create_with_validated_targs(cls.dupe(), targs)
    }

    fn instantiate_class_member(&self, cls: &ClassType, ty: Type) -> Type {
        cls.substitution(&self.get_tparams_for_class(cls.class_object()))
            .substitute(ty)
    }

    /// Get an ancestor `ClassType`, in terms of the type parameters of `class`.
    fn get_ancestor(&self, class: &Class, want: &Class) -> Option<ClassType> {
        self.get_mro_for_class(class)
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
                .map(|ancestor| ancestor.substitute(&self.get_substitution(class)))
        }
    }

    fn get_class_field(&self, cls: &Class, name: &Name) -> Option<Arc<Type>> {
        if cls.contains(name) {
            let ty = self.get_from_class(cls, &Key::ClassField(cls.name().clone(), name.clone()));
            Some(ty)
        } else {
            None
        }
    }

    fn get_class_member(&self, cls: &Class, name: &Name) -> Option<Arc<Type>> {
        if let Some(member) = self.get_class_field(cls, name) {
            Some(member)
        } else {
            self.get_mro_for_class(cls)
                .ancestors(self.stdlib)
                .filter_map(|ancestor| {
                    self.get_class_field(ancestor.class_object(), name)
                        .as_deref()
                        .map(|ty| {
                            let raw_member = self.instantiate_class_member(ancestor, ty.clone());
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
                let instantiated_ty = self.instantiate_class_member(cls, (*member_ty).clone());
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
        let tparams = self.get_tparams_for_class(cls);
        let mut qs = SmallSet::new();
        ty.collect_quantifieds(&mut qs);
        tparams.0.iter().any(|q| qs.contains(q))
    }

    pub fn get_class_attribute(&self, cls: &Class, name: &Name) -> Result<Type, NoClassAttribute> {
        match self.get_class_member(cls, name) {
            None => Err(NoClassAttribute::NoClassMember),
            Some(ty) => {
                if self.depends_on_class_type_parameter(cls, ty.as_ref()) {
                    Err(NoClassAttribute::IsGenericMember)
                } else if cls.is_enum(&|c| self.get_mro_for_class(c)) {
                    // TODO(stroxler, yangdanny) Enums can contain attributes that are not
                    // members, we eventually need to implement enough checks to know the
                    // difference.
                    Ok(Type::Literal(Lit::Enum(
                        self.promote_to_class_type_silently(cls),
                        name.to_owned(),
                    )))
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
        let tparams = self.get_tparams_for_class(cls);
        let ret = cls.self_type(tparams.deref());
        match init_ty.as_deref() {
            Some(ty) => replace_return_type(strip_first_argument(ty), ret),
            None => Type::callable(Vec::new(), ret),
        }
    }

    pub fn get_constructor_for_class_type(&self, cls: &ClassType) -> Type {
        let init_ty = self.get_init_method(cls.class_object());
        self.instantiate_class_member(cls, init_ty)
    }

    pub fn get_constructor_for_class_object(&self, cls: &Class) -> Type {
        let init_ty = self.get_init_method(cls);
        let tparams = self.get_tparams_for_class(cls);
        Type::forall(tparams.0.clone(), init_ty)
    }

    /// Given an identifier, see whether it is bound to an enum class. If so,
    /// return a `ClassType` for the enum class, otherwise return `None`.
    pub fn get_enum_class_type(&self, name: Identifier) -> Option<ClassType> {
        match self.get(&Key::Usage(name.clone())).deref() {
            Type::ClassDef(class) if class.is_enum(&|c| self.get_mro_for_class(c)) => {
                // TODO(stroxler): Eventually, we should raise type errors on generic Enum because
                // this doesn't make semantic sense. But in the meantime we need to be robust against
                // this possibility.
                Some(self.promote_to_class_type_silently(class))
            }
            _ => None,
        }
    }
}
