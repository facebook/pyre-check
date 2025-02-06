/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;
use std::sync::Arc;

use dupe::Dupe;
use itertools::EitherOrBoth;
use itertools::Itertools;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::Identifier;
use ruff_python_ast::StmtClassDef;
use ruff_text_size::TextRange;
use starlark_map::small_map::SmallMap;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::attr::Attribute;
use crate::alt::attr::NoAccessReason;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::alt::types::class_metadata::EnumMetadata;
use crate::binding::binding::ClassFieldInitialization;
use crate::binding::binding::Key;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassMetadata;
use crate::binding::binding::KeyClassSynthesizedFields;
use crate::binding::binding::KeyLegacyTypeParam;
use crate::dunder;
use crate::graph::index::Idx;
use crate::module::short_identifier::ShortIdentifier;
use crate::types::annotation::Annotation;
use crate::types::callable::Param;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassFieldProperties;
use crate::types::class::ClassType;
use crate::types::class::TArgs;
use crate::types::typed_dict::TypedDict;
use crate::types::types::Decoration;
use crate::types::types::TParams;
use crate::types::types::Type;
use crate::util::display::count;
use crate::util::prelude::SliceExt;

/// Raw information about an attribute declared somewhere in a class. We need to
/// know whether it is initialized in the class body in order to determine
/// both visibility rules and whether method binding should be performed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassField(pub ClassFieldInner);

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClassFieldInner {
    Simple {
        ty: Type,
        annotation: Option<Annotation>,
        initialization: ClassFieldInitialization,
        readonly: bool,
        is_enum_member: bool,
    },
}

impl ClassField {
    fn new(
        ty: Type,
        annotation: Option<Annotation>,
        initialization: ClassFieldInitialization,
        readonly: bool,
        is_enum_member: bool,
    ) -> Self {
        Self(ClassFieldInner::Simple {
            ty,
            annotation,
            initialization,
            readonly,
            is_enum_member,
        })
    }

    pub fn recursive() -> Self {
        Self(ClassFieldInner::Simple {
            ty: Type::any_implicit(),
            annotation: None,
            initialization: ClassFieldInitialization::Class,
            readonly: false,
            is_enum_member: false,
        })
    }

    pub fn visit_type_mut(&mut self, mut f: &mut dyn FnMut(&mut Type)) {
        match &mut self.0 {
            ClassFieldInner::Simple { ty, annotation, .. } => {
                f(ty);
                for a in annotation.iter_mut() {
                    a.visit_type_mut(&mut f);
                }
            }
        }
    }

    fn initialization(&self) -> ClassFieldInitialization {
        match &self.0 {
            ClassFieldInner::Simple { initialization, .. } => *initialization,
        }
    }

    pub fn is_enum_member(&self) -> bool {
        match &self.0 {
            ClassFieldInner::Simple { is_enum_member, .. } => *is_enum_member,
        }
    }

    fn instantiate_for(&self, cls: &ClassType) -> Self {
        match &self.0 {
            ClassFieldInner::Simple {
                ty,
                annotation,
                initialization,
                readonly,
                is_enum_member,
            } => Self(ClassFieldInner::Simple {
                ty: cls.instantiate_member(ty.clone()),
                annotation: annotation.clone(),
                initialization: *initialization,
                readonly: *readonly,
                is_enum_member: *is_enum_member,
            }),
        }
    }

    pub fn as_param(self, name: &Name, kw_only: bool) -> Param {
        let ClassField(ClassFieldInner::Simple {
            ty, initialization, ..
        }) = self;
        let required = match initialization {
            ClassFieldInitialization::Class => Required::Required,
            ClassFieldInitialization::Instance => Required::Optional,
        };
        if kw_only {
            Param::KwOnly(name.clone(), ty, required)
        } else {
            Param::Pos(name.clone(), ty, required)
        }
    }

    fn depends_on_class_type_parameter(&self, cls: &Class) -> bool {
        let tparams = cls.tparams();
        let mut qs = SmallSet::new();
        match &self.0 {
            ClassFieldInner::Simple { ty, .. } => ty.collect_quantifieds(&mut qs),
        };
        tparams.quantified().any(|q| qs.contains(&q))
    }

    fn as_raw_special_method_type(self, cls: &ClassType) -> Option<Type> {
        match self.instantiate_for(cls).0 {
            ClassFieldInner::Simple { ty, .. } => match self.initialization() {
                ClassFieldInitialization::Class => Some(ty),
                ClassFieldInitialization::Instance => None,
            },
        }
    }

    fn as_special_method_type(self, cls: &ClassType) -> Option<Type> {
        self.as_raw_special_method_type(cls).and_then(|ty| {
            if is_unbound_function(&ty) {
                Some(make_bound_method(cls.self_type(), ty))
            } else {
                None
            }
        })
    }

    fn as_instance_attribute(self, cls: &ClassType) -> Attribute {
        match self.instantiate_for(cls).0 {
            ClassFieldInner::Simple { ty, readonly, .. } => match self.initialization() {
                ClassFieldInitialization::Class => bind_instance_attribute(cls, ty),
                ClassFieldInitialization::Instance if readonly => Attribute::read_only(ty),
                ClassFieldInitialization::Instance => Attribute::read_write(ty),
            },
        }
    }

    fn as_class_attribute(self, cls: &Class) -> Attribute {
        match &self.0 {
            ClassFieldInner::Simple {
                initialization: ClassFieldInitialization::Instance,
                ..
            } => Attribute::no_access(NoAccessReason::ClassUseOfInstanceAttribute(cls.clone())),
            ClassFieldInner::Simple {
                initialization: ClassFieldInitialization::Class,
                ty,
                ..
            } => {
                if self.depends_on_class_type_parameter(cls) {
                    Attribute::no_access(NoAccessReason::ClassAttributeIsGeneric(cls.clone()))
                } else {
                    bind_class_attribute(cls, ty.clone())
                }
            }
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

fn bind_class_attribute(cls: &Class, attr: Type) -> Attribute {
    match attr {
        Type::Decoration(Decoration::StaticMethod(box attr)) => Attribute::read_write(attr),
        Type::Decoration(Decoration::ClassMethod(box attr)) => {
            Attribute::read_write(make_bound_method(Type::ClassDef(cls.dupe()), attr))
        }
        // Accessing a property descriptor on the class gives the property itself,
        // with no magic access rules at runtime.
        p @ Type::Decoration(Decoration::Property(_)) => Attribute::read_write(p),
        attr => Attribute::read_write(attr),
    }
}

fn bind_instance_attribute(cls: &ClassType, attr: Type) -> Attribute {
    match attr {
        Type::Decoration(Decoration::StaticMethod(box attr)) => Attribute::read_write(attr),
        Type::Decoration(Decoration::ClassMethod(box attr)) => Attribute::read_write(
            make_bound_method(Type::ClassDef(cls.class_object().dupe()), attr),
        ),
        Type::Decoration(Decoration::Property(box (getter, setter))) => Attribute::property(
            make_bound_method(Type::ClassType(cls.clone()), getter),
            setter.map(|setter| make_bound_method(Type::ClassType(cls.clone()), setter)),
            cls.class_object().dupe(),
        ),
        attr => Attribute::read_write(if is_unbound_function(&attr) {
            make_bound_method(cls.self_type(), attr)
        } else {
            attr
        }),
    }
}

fn make_bound_method(obj: Type, attr: Type) -> Type {
    // TODO(stroxler): Think about what happens if `attr` is not callable. This
    // can happen with the current logic if a decorator spits out a non-callable
    // type that gets wrapped in `@classmethod`.
    Type::BoundMethod(Box::new(obj), Box::new(attr))
}

impl Display for ClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            ClassFieldInner::Simple {
                ty, initialization, ..
            } => {
                let initialized = match initialization {
                    ClassFieldInitialization::Class => "initialized in body",
                    ClassFieldInitialization::Instance => "not initialized in body",
                };
                write!(f, "@{} ({})", ty, initialized)
            }
        }
    }
}

/// Result of looking up a member of a class in the MRO, including a handle to the defining
/// class which may be some ancestor.
///
/// For example, given `class A: x: int; class B(A): pass`, the defining class
/// for attribute `x` is `A` even when `x` is looked up on `B`.
pub struct WithDefiningClass<T> {
    pub value: T,
    defining_class: Class,
}

impl<T> WithDefiningClass<T> {
    fn defined_on(&self, cls: &Class) -> bool {
        self.defining_class == *cls
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn class_definition(
        &self,
        x: &StmtClassDef,
        fields: SmallMap<Name, ClassFieldProperties>,
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

    pub fn get_idx_class_def(&self, idx: Idx<Key>) -> Option<Class> {
        let ty = self.get_idx(idx);
        match &*ty {
            Type::ClassDef(cls) => Some(cls.dupe()),
            _ => None,
        }
    }

    pub fn functional_class_definition(
        &self,
        name: &Identifier,
        fields: &SmallMap<Name, ClassFieldProperties>,
    ) -> Class {
        Class::new(
            name.clone(),
            self.module_info().dupe(),
            TParams::default(),
            fields.clone(),
        )
    }

    pub fn get_metadata_for_class(&self, cls: &Class) -> Arc<ClassMetadata> {
        self.get_from_class(cls, &KeyClassMetadata(ShortIdentifier::new(cls.name())))
    }

    fn get_enum_from_class(&self, cls: &Class) -> Option<EnumMetadata> {
        self.get_metadata_for_class(cls).enum_metadata().cloned()
    }

    pub fn get_enum_from_class_type(&self, class_type: &ClassType) -> Option<EnumMetadata> {
        self.get_enum_from_class(class_type.class_object())
    }

    /// Given an identifier, see whether it is bound to an enum class. If so,
    /// return the enum, otherwise return `None`.
    pub fn get_enum_from_name(&self, name: Identifier) -> Option<EnumMetadata> {
        let key = self
            .bindings()
            .key_to_idx(&Key::Usage(ShortIdentifier::new(&name)));
        self.get_idx_class_def(key)
            .and_then(|cls| self.get_enum_from_class(&cls))
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

    pub fn create_default_targs(
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

    fn type_of_instance(&self, cls: &Class, targs: TArgs) -> Type {
        let metadata = self.get_metadata_for_class(cls);
        if metadata.is_typed_dict() {
            let fields = self.sub_typed_dict_fields(cls, &targs);
            Type::TypedDict(Box::new(TypedDict::new(cls.dupe(), targs, fields)))
        } else {
            Type::ClassType(ClassType::new(cls.dupe(), targs))
        }
    }

    /// Given a class or typed dictionary and some (explicit) type arguments, construct a `Type`
    /// that represents the type of an instance of the class or typed dictionary with those `targs`.
    pub fn specialize(&self, cls: &Class, targs: Vec<Type>, range: TextRange) -> Type {
        let targs = self.check_and_create_targs(cls, targs, range);
        self.type_of_instance(cls, targs)
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
        let targs = self.create_default_targs(cls, Some(range));
        self.type_of_instance(cls, targs)
    }

    /// Private version of `promote` that does not potentially
    /// raise strict mode errors. Should only be used for unusual scenarios.
    fn promote_silently(&self, cls: &Class) -> Type {
        let targs = self.create_default_targs(cls, None);
        self.type_of_instance(cls, targs)
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
        let qs = cls.tparams().quantified().collect::<Vec<_>>();
        let targs = TArgs::new(qs.map(|q| Type::Quantified(*q)));
        let promoted_cls = Type::type_form(self.type_of_instance(cls, targs));
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

    pub fn calculate_class_field(
        &self,
        name: &Name,
        value_ty: &Type,
        annotation: Option<&Annotation>,
        initialization: ClassFieldInitialization,
        class: &Class,
        range: TextRange,
    ) -> ClassField {
        let metadata = self.get_metadata_for_class(class);
        let mut is_enum_member = false;
        if let Some(enum_) = self.get_enum_from_class(class)
            && self.is_valid_enum_member(name, value_ty, initialization)
        {
            is_enum_member = true;
            if annotation.is_some() {
                self.error(range, format!("Enum member `{}` may not be annotated directly. Instead, annotate the _value_ attribute.", name));
            }

            if let Some(enum_value_ty) = self.type_of_enum_value(enum_) {
                if !matches!(value_ty, Type::Tuple(_))
                    && !self
                        .solver()
                        .is_subset_eq(value_ty, &enum_value_ty, self.type_order())
                {
                    self.error(range, format!("The value for enum member `{}` must match the annotation of the _value_ attribute.", name));
                }
            }
        }
        if metadata.is_typed_dict() && matches!(initialization, ClassFieldInitialization::Class) {
            self.error(
                range,
                format!("TypedDict item `{}` may not be initialized.", name),
            );
        }
        let (ty, ann) = if let Some(ann) = annotation {
            match &ann.ty {
                Some(ty) => (ty, Some(ann)),
                None => (value_ty, Some(ann)),
            }
        } else {
            (value_ty, None)
        };
        let readonly = metadata
            .dataclass_metadata()
            .map_or(false, |dataclass| dataclass.kws.frozen);
        ClassField::new(
            ty.clone(),
            ann.cloned(),
            initialization,
            readonly,
            is_enum_member,
        )
    }

    fn get_class_field(&self, cls: &Class, name: &Name) -> Option<ClassField> {
        if cls.contains(name) {
            let field = self.get_from_class(
                cls,
                &KeyClassField(ShortIdentifier::new(cls.name()), name.clone()),
            );
            Some((*field).clone())
        } else {
            let synthesized_fields = self.get_from_class(
                cls,
                &KeyClassSynthesizedFields(ShortIdentifier::new(cls.name())),
            );
            synthesized_fields.get(name).cloned()
        }
    }

    pub fn get_class_member(
        &self,
        cls: &Class,
        name: &Name,
    ) -> Option<WithDefiningClass<ClassField>> {
        if let Some(field) = self.get_class_field(cls, name) {
            Some(WithDefiningClass {
                value: field,
                defining_class: cls.dupe(),
            })
        } else {
            self.get_metadata_for_class(cls)
                .ancestors(self.stdlib)
                .filter_map(|ancestor| {
                    self.get_class_field(ancestor.class_object(), name)
                        .map(|field| WithDefiningClass {
                            value: field.instantiate_for(ancestor),
                            defining_class: ancestor.class_object().dupe(),
                        })
                })
                .next()
        }
    }

    // Get every member of a class, including those declared in parent classes.
    pub(super) fn get_all_members(&self, cls: &Class) -> SmallMap<Name, (ClassField, Class)> {
        let mut members = SmallMap::new();
        for name in cls.fields() {
            if let Some(field) = self.get_class_field(cls, name) {
                members.insert(name.clone(), (field, cls.dupe()));
            }
        }
        for ancestor in self.get_metadata_for_class(cls).ancestors(self.stdlib) {
            for name in ancestor.class_object().fields() {
                if !members.contains_key(name) {
                    if let Some(field) = self.get_class_field(ancestor.class_object(), name) {
                        members.insert(
                            name.clone(),
                            (
                                field.instantiate_for(ancestor),
                                ancestor.class_object().dupe(),
                            ),
                        );
                    }
                }
            }
        }
        members
    }

    pub fn get_all_member_names(&self, cls: &Class) -> SmallSet<Name> {
        self.get_all_members(cls)
            .keys()
            .cloned()
            .collect::<SmallSet<_>>()
    }

    pub fn get_instance_attribute(&self, cls: &ClassType, name: &Name) -> Option<Attribute> {
        self.get_class_member(cls.class_object(), name)
            .map(|member| member.value.as_instance_attribute(cls))
    }

    /// Gets an attribute from a class definition.
    ///
    /// Returns `None` if there is no such attribute, otherwise an `Attribute` object
    /// that describes whether access is allowed and the type if so.
    ///
    /// Access is disallowed for instance-only attributes and for attributes whose
    /// type contains a class-scoped type parameter - e.g., `class A[T]: x: T`.
    pub fn get_class_attribute(&self, cls: &Class, name: &Name) -> Option<Attribute> {
        if let Some(e) = self.get_enum_from_class(cls)
            && let Some(enum_member) = self.get_enum_member(&e, name)
        {
            Some(Attribute::read_write(Type::Literal(enum_member)))
        } else {
            let member = self.get_class_member(cls, name)?.value;
            Some(member.as_class_attribute(cls))
        }
    }

    /// Get the class's `__new__` method.
    ///
    /// This lookup skips normal method binding logic (it behaves like a cross
    /// between a classmethod and a constructor; downstream code handles this
    /// using the raw callable type).
    pub fn get_dunder_new(&self, cls: &ClassType) -> Option<Type> {
        let new_member = self.get_class_member(cls.class_object(), &dunder::NEW)?;
        if new_member.defined_on(self.stdlib.object_class_type().class_object()) {
            // The default behavior of `object.__new__` is already baked into our implementation of
            // class construction; we only care about `__new__` if it is overridden.
            None
        } else {
            new_member.value.as_raw_special_method_type(cls)
        }
    }

    /// Get the class's `__init__` method, if we should analyze it
    /// We skip analyzing the call to `__init__` if:
    /// (1) it isn't defined (possible if we've been passed a custom typeshed), or
    /// (2) the class overrides `object.__new__` but not `object.__init__`, in wich case the
    ///     `__init__` call always succeeds at runtime.
    pub fn get_dunder_init(&self, cls: &ClassType, overrides_new: bool) -> Option<Type> {
        let init_method = self.get_class_member(cls.class_object(), &dunder::INIT)?;
        if !(overrides_new
            && init_method.defined_on(self.stdlib.object_class_type().class_object()))
        {
            init_method.value.as_special_method_type(cls)
        } else {
            None
        }
    }

    /// Get the metaclass `__call__` method.
    pub fn get_metaclass_dunder_call(&self, cls: &ClassType) -> Option<Type> {
        let metadata = self.get_metadata_for_class(cls.class_object());
        let metaclass = metadata.metaclass()?;
        let attr = self.get_class_member(metaclass.class_object(), &dunder::CALL)?;
        if attr.defined_on(self.stdlib.builtins_type().class_object()) {
            // The behavior of `type.__call__` is already baked into our implementation of constructors,
            // so we can skip analyzing it at the type level.
            None
        } else {
            attr.value.as_special_method_type(metaclass)
        }
    }
}
