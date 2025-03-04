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
use ruff_python_ast::name::Name;
use ruff_python_ast::Arguments;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_text_size::TextRange;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::attr::Attribute;
use crate::alt::attr::DescriptorBase;
use crate::alt::attr::NoAccessReason;
use crate::alt::types::class_metadata::ClassMetadata;
use crate::binding::binding::ClassFieldInitialValue;
use crate::binding::binding::KeyClassField;
use crate::binding::binding::KeyClassSynthesizedFields;
use crate::dunder;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::error::style::ErrorStyle;
use crate::types::annotation::Annotation;
use crate::types::annotation::Qualifier;
use crate::types::callable::BoolKeywords;
use crate::types::callable::CallableKind;
use crate::types::callable::DataclassKeywords;
use crate::types::callable::Param;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::literal::Lit;
use crate::types::typed_dict::TypedDictField;
use crate::types::types::BoundMethod;
use crate::types::types::CalleeKind;
use crate::types::types::Decoration;
use crate::types::types::Type;

/// Correctly analyzing which attributes are visible on class objects, as well
/// as handling method binding correctly, requires distinguishing which fields
/// are assigned values in the class body.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ClassFieldInitialization {
    /// If this is a dataclass field, BoolKeywords stores the field's dataclass
    /// flags (which are boolean options that control how fields behave).
    Class(Option<BoolKeywords>),
    Instance,
}

impl Display for ClassFieldInitialization {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Class(_) => write!(f, "initialized in body"),
            Self::Instance => write!(f, "not initialized in body"),
        }
    }
}

impl ClassFieldInitialization {
    pub fn recursive() -> Self {
        ClassFieldInitialization::Class(None)
    }
}

/// Raw information about an attribute declared somewhere in a class. We need to
/// know whether it is initialized in the class body in order to determine
/// both visibility rules and whether method binding should be performed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassField(ClassFieldInner);

#[derive(Debug, Clone, PartialEq, Eq)]
enum ClassFieldInner {
    // TODO(stroxler): We should refactor `ClassFieldInner` into enum cases; currently
    // the semantics are encoded ad-hoc into the fields of a large product which
    // has made hacking features relatively easy, but makes the code hard to read.
    Simple {
        ty: Type,
        annotation: Option<Annotation>,
        initialization: ClassFieldInitialization,
        readonly: bool,
        // Descriptor getter method, if there is one. `None` indicates no getter.
        descriptor_getter: Option<Type>,
        // Descriptor setter method, if there is one. `None` indicates no setter.
        descriptor_setter: Option<Type>,
    },
}

impl Display for ClassField {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.0 {
            ClassFieldInner::Simple {
                ty, initialization, ..
            } => write!(f, "@{ty} ({initialization})"),
        }
    }
}

impl ClassField {
    fn new(
        ty: Type,
        annotation: Option<Annotation>,
        initialization: ClassFieldInitialization,
        readonly: bool,
        descriptor_getter: Option<Type>,
        descriptor_setter: Option<Type>,
    ) -> Self {
        Self(ClassFieldInner::Simple {
            ty,
            annotation,
            initialization,
            readonly,
            descriptor_getter,
            descriptor_setter,
        })
    }

    pub fn new_synthesized(ty: Type) -> Self {
        ClassField(ClassFieldInner::Simple {
            ty,
            annotation: None,
            initialization: ClassFieldInitialization::Class(None),
            readonly: false,
            descriptor_getter: None,
            descriptor_setter: None,
        })
    }

    pub fn recursive() -> Self {
        Self(ClassFieldInner::Simple {
            ty: Type::any_implicit(),
            annotation: None,
            initialization: ClassFieldInitialization::recursive(),
            readonly: false,
            descriptor_getter: None,
            descriptor_setter: None,
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
            ClassFieldInner::Simple { initialization, .. } => initialization.clone(),
        }
    }

    fn instantiate_for(&self, cls: &ClassType) -> Self {
        match &self.0 {
            ClassFieldInner::Simple {
                ty,
                annotation,
                initialization,
                readonly,
                descriptor_getter,
                descriptor_setter,
            } => Self(ClassFieldInner::Simple {
                ty: cls.instantiate_member(ty.clone()),
                annotation: annotation.clone(),
                initialization: initialization.clone(),
                readonly: *readonly,
                descriptor_getter: descriptor_getter
                    .as_ref()
                    .map(|ty| cls.instantiate_member(ty.clone())),
                descriptor_setter: descriptor_setter
                    .as_ref()
                    .map(|ty| cls.instantiate_member(ty.clone())),
            }),
        }
    }

    pub fn as_param(self, name: &Name, default: bool, kw_only: bool) -> Param {
        let ClassField(ClassFieldInner::Simple { ty, .. }) = self;
        let required = match default {
            true => Required::Optional,
            false => Required::Required,
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
                ClassFieldInitialization::Class(_) => Some(ty),
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

    pub fn as_named_tuple_type(&self) -> Type {
        match &self.0 {
            ClassFieldInner::Simple { ty, .. } => ty.clone(),
        }
    }

    pub fn as_named_tuple_requiredness(&self) -> Required {
        match &self.0 {
            ClassFieldInner::Simple {
                initialization: ClassFieldInitialization::Class(_),
                ..
            } => Required::Optional,
            ClassFieldInner::Simple {
                initialization: ClassFieldInitialization::Instance,
                ..
            } => Required::Required,
        }
    }

    pub fn as_typed_dict_field_info(self, required_by_default: bool) -> Option<TypedDictField> {
        match &self.0 {
            ClassFieldInner::Simple {
                annotation:
                    Some(Annotation {
                        ty: Some(ty),
                        qualifiers,
                    }),
                ..
            } => Some(TypedDictField {
                ty: ty.clone(),
                read_only: qualifiers.contains(&Qualifier::ReadOnly),
                required: if qualifiers.contains(&Qualifier::Required) {
                    true
                } else if qualifiers.contains(&Qualifier::NotRequired) {
                    false
                } else {
                    required_by_default
                },
            }),
            _ => None,
        }
    }

    pub fn as_enum_member(self, enum_cls: &Class) -> Option<Lit> {
        match self.0 {
            ClassFieldInner::Simple {
                ty: Type::Literal(lit),
                ..
            } if matches!(&lit, Lit::Enum(box (lit_cls, ..)) if lit_cls.class_object() == enum_cls) => {
                Some(lit)
            }
            _ => None,
        }
    }

    pub fn is_dataclass_kwonly_marker(&self) -> bool {
        match &self.0 {
            ClassFieldInner::Simple { ty, .. } => {
                matches!(ty, Type::ClassType(cls) if cls.class_object().has_qname("dataclasses", "KW_ONLY"))
            }
        }
    }

    pub fn dataclass_flags_of(&self, kw_only: bool) -> Option<BoolKeywords> {
        match &self.0 {
            ClassFieldInner::Simple {
                initialization,
                annotation,
                ..
            } => {
                if let Some(annot) = annotation
                    && annot.qualifiers.contains(&Qualifier::ClassVar)
                {
                    return None; // Class variables are not dataclass fields
                }
                let mut flags = match initialization {
                    ClassFieldInitialization::Class(Some(field_flags)) => field_flags.clone(),
                    ClassFieldInitialization::Class(None) => {
                        let mut kws = BoolKeywords::new();
                        kws.set(DataclassKeywords::DEFAULT.0, true);
                        kws
                    }
                    ClassFieldInitialization::Instance => BoolKeywords::new(),
                };
                if kw_only {
                    flags.set(DataclassKeywords::KW_ONLY.0, true);
                }
                Some(flags)
            }
        }
    }
}

pub fn is_unbound_function(ty: &Type) -> bool {
    match ty {
        Type::Forall(_, t) => is_unbound_function(t),
        Type::Callable(_, _) => true,
        Type::Overload(_) => true,
        _ => false,
    }
}

pub fn bind_class_attribute(cls: &Class, attr: Type) -> Attribute {
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

pub fn make_bound_method(obj: Type, attr: Type) -> Type {
    // TODO(stroxler): Think about what happens if `attr` is not callable. This
    // can happen with the current logic if a decorator spits out a non-callable
    // type that gets wrapped in `@classmethod`.
    Type::BoundMethod(Box::new(BoundMethod { obj, func: attr }))
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

/// Result of looking up a member of a class in the MRO, including a handle to the defining
/// class which may be some ancestor.
///
/// For example, given `class A: x: int; class B(A): pass`, the defining class
/// for attribute `x` is `A` even when `x` is looked up on `B`.
#[derive(Debug)]
pub(in crate::alt::class) struct WithDefiningClass<T> {
    pub value: T,
    pub defining_class: Class,
}

impl<T> WithDefiningClass<T> {
    pub(in crate::alt::class) fn defined_on(&self, cls: &Class) -> bool {
        self.defining_class == *cls
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn calculate_class_field(
        &self,
        name: &Name,
        value_ty: &Type,
        annotation: Option<&Annotation>,
        initial_value: &ClassFieldInitialValue,
        class: &Class,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> ClassField {
        let metadata = self.get_metadata_for_class(class);
        let initialization = self.get_class_field_initialization(&metadata, initial_value);

        // Ban typed dict from containing values; fields should be annotation-only.
        // TODO(stroxler): we ought to look into this more: class-level attributes make sense on a `TypedDict` class;
        // the typing spec does not explicitly define whether this is permitted.
        if metadata.is_typed_dict() && matches!(initialization, ClassFieldInitialization::Class(_))
        {
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                format!("TypedDict item `{}` may not be initialized.", name),
            );
        }

        // Determine whether this is an explicit `@override` and remove the decoration from the type if so.
        let (value_ty, is_override) = value_ty.clone().extract_override();

        // Promote literals. The check on `annotation` is an optimization, it does not (currently) affect semantics.
        // TODO(stroxler): if we see a read-only `Qualifier` like `Final`, it is sound to preserve literals.
        let value_ty = if annotation.map_or(true, |a| a.ty.is_none()) && value_ty.is_literal() {
            value_ty.promote_literals(self.stdlib)
        } else {
            value_ty
        };

        // Types provided in annotations shadow inferred types
        let ty = if let Some(ann) = annotation {
            match &ann.ty {
                Some(ty) => ty,
                None => &value_ty,
            }
        } else {
            &value_ty
        };

        // Enum handling:
        // - Check whether the field is a member (which depends only on its type and name)
        // - Validate that a member should not have an annotation, and should respect any explicit annotatin on `_value_`
        //
        // TODO(stroxler, yangdanny): We currently operate on promoted types, which means we do not infer `Literal[...]`
        // types for the `.value` / `._value_` attributes of literals. This is permitted in the spec although not optimal
        // for most cases; we are handling it this way in part because generic enum behavior is not yet well-specified.
        let ty = if let Some(enum_) = metadata.enum_metadata()
            && self.is_valid_enum_member(name, ty, &initialization)
        {
            if annotation.is_some() {
                self.error(errors, range,
                    ErrorKind::Unknown,
                     format!("Enum member `{}` may not be annotated directly. Instead, annotate the _value_ attribute.", name),
                    );
            }
            if let Some(enum_value_ty) = self.type_of_enum_value(enum_) {
                if !matches!(ty, Type::Tuple(_))
                    && !self
                        .solver()
                        .is_subset_eq(ty, &enum_value_ty, self.type_order())
                {
                    self.error(errors,
                         range,
                         ErrorKind::Unknown,
                         format!("The value for enum member `{}` must match the annotation of the _value_ attribute.", name), 
                        );
                }
            }
            &Type::Literal(Lit::Enum(Box::new((
                enum_.cls.clone(),
                name.clone(),
                ty.clone(),
            ))))
        } else {
            ty
        };

        // Dataclass read-onlyness (does not currently handle other kinds of readonlyness)
        let readonly = metadata
            .dataclass_metadata()
            .is_some_and(|dataclass| dataclass.kws.is_set(&DataclassKeywords::FROZEN));

        // Identify whether this is a descriptor
        let (mut descriptor_getter, mut descriptor_setter) = (None, None);
        match ty {
            // TODO(stroxler): This works for simple descriptors. There three known gaps, there may be others:
            // - If the field is instance-only, descriptor dispatching won't occur, an instance-only attribute
            //   that happens to be a descriptor just behaves like a normal instance-only attribute.
            // - Gracefully handle instance-only `__get__`/`__set__`. Descriptors only seem to be detected
            //   when the descriptor attribute is initialized on the class body of the descriptor.
            // - Do we care about distributing descriptor behavior over unions? If so, what about the case when
            //   the raw class field is a union of a descriptor and a non-descriptor? Do we want to allow this?
            Type::ClassType(c) => {
                if c.class_object().contains(&dunder::GET) {
                    descriptor_getter = Some(self.attr_infer(ty, &dunder::GET, range, errors));
                }
                if c.class_object().contains(&dunder::SET) {
                    descriptor_setter = Some(self.attr_infer(ty, &dunder::SET, range, errors));
                }
            }
            _ => {}
        };

        // Create the resulting field and check for override inconsistencies before returning
        let class_field = ClassField::new(
            ty.clone(),
            annotation.cloned(),
            initialization,
            readonly,
            descriptor_getter,
            descriptor_setter,
        );
        self.check_class_field_for_override_mismatch(
            name,
            &class_field,
            class,
            is_override,
            range,
            errors,
        );
        class_field
    }

    fn get_class_field_initialization(
        &self,
        metadata: &ClassMetadata,
        initial_value: &ClassFieldInitialValue,
    ) -> ClassFieldInitialization {
        match initial_value {
            ClassFieldInitialValue::Instance => ClassFieldInitialization::Instance,
            ClassFieldInitialValue::Class(None) => ClassFieldInitialization::Class(None),
            ClassFieldInitialValue::Class(Some(e)) => {
                // If this field was created via a call to a dataclass field specifier, extract field flags from the call.
                if metadata.dataclass_metadata().is_some()
                    && let Expr::Call(ExprCall {
                        range: _,
                        func,
                        arguments: Arguments { keywords, .. },
                    }) = e
                {
                    let mut flags = BoolKeywords::new();
                    // We already type-checked this expression as part of computing the type for the ClassField,
                    // so we can ignore any errors encountered here.
                    let ignore_errors =
                        ErrorCollector::new(self.module_info().dupe(), ErrorStyle::Never);
                    let func_ty = self.expr_infer(func, &ignore_errors);
                    if matches!(
                        func_ty.callee_kind(),
                        Some(CalleeKind::Callable(CallableKind::DataclassField))
                    ) {
                        for kw in keywords {
                            if let Some(id) = &kw.arg
                                && (id.id == DataclassKeywords::DEFAULT.0
                                    || id.id == "default_factory")
                            {
                                flags.set(DataclassKeywords::DEFAULT.0, true);
                            } else {
                                let val = self.expr_infer(&kw.value, &ignore_errors);
                                flags.set_keyword(kw.arg.as_ref(), val);
                            }
                        }
                    }
                    ClassFieldInitialization::Class(Some(flags))
                } else {
                    ClassFieldInitialization::Class(None)
                }
            }
        }
    }

    fn as_instance_attribute(&self, field: ClassField, cls: &ClassType) -> Attribute {
        match field.instantiate_for(cls).0 {
            // TODO(stroxler): Clean up this match by making `ClassFieldInner` an
            // enum; the match is messy
            ClassFieldInner::Simple {
                ty,
                descriptor_getter,
                descriptor_setter,
                ..
            } if descriptor_getter.is_some() || descriptor_setter.is_some() => {
                Attribute::descriptor(
                    ty,
                    DescriptorBase::Instance(cls.clone()),
                    descriptor_getter,
                    descriptor_setter,
                )
            }
            ClassFieldInner::Simple { ty, readonly, .. } => match field.initialization() {
                ClassFieldInitialization::Class(_) => bind_instance_attribute(cls, ty),
                ClassFieldInitialization::Instance if readonly => Attribute::read_only(ty),
                ClassFieldInitialization::Instance => Attribute::read_write(ty),
            },
        }
    }

    fn as_class_attribute(&self, field: ClassField, cls: &Class) -> Attribute {
        match &field.0 {
            ClassFieldInner::Simple {
                ty,
                descriptor_getter,
                descriptor_setter,
                ..
            } if descriptor_getter.is_some() || descriptor_setter.is_some() => {
                Attribute::descriptor(
                    ty.clone(),
                    DescriptorBase::ClassDef(cls.clone()),
                    descriptor_getter.clone(),
                    descriptor_setter.clone(),
                )
            }
            ClassFieldInner::Simple {
                initialization: ClassFieldInitialization::Instance,
                ..
            } => Attribute::no_access(NoAccessReason::ClassUseOfInstanceAttribute(cls.clone())),
            ClassFieldInner::Simple {
                initialization: ClassFieldInitialization::Class(_),
                ty,
                ..
            } => {
                if field.depends_on_class_type_parameter(cls) {
                    Attribute::no_access(NoAccessReason::ClassAttributeIsGeneric(cls.clone()))
                } else {
                    bind_class_attribute(cls, ty.clone())
                }
            }
        }
    }

    fn check_class_field_for_override_mismatch(
        &self,
        name: &Name,
        class_field: &ClassField,
        class: &Class,
        is_override: bool,
        range: TextRange,
        errors: &ErrorCollector,
    ) {
        // Perform override checks
        let class_type = match class.self_type() {
            Type::ClassType(class_type) => Some(class_type),
            _ => None,
        };
        if let Some(class_type) = class_type {
            let got = self.as_instance_attribute(class_field.clone(), &class_type);
            let metadata = self.get_metadata_for_class(class);
            let parents = metadata.bases_with_metadata();

            let mut parent_attr_found = false;
            let mut parent_has_any = false;

            for (parent, parent_metadata) in parents {
                parent_has_any = parent_has_any || parent_metadata.has_base_any();

                // todo zeina: skip private properties and dunder methods for now. This will need some special casing.
                if name.starts_with('_') && name.ends_with('_') {
                    continue;
                }

                if let Some(want) = self.type_order().try_lookup_attr(parent.self_type(), name) {
                    parent_attr_found = true;
                    let attr_check = self.is_attr_subset(&got, &want, &mut |got, want| {
                        self.solver().is_subset_eq(got, want, self.type_order())
                    });

                    if !attr_check {
                        self.error(
                            errors,
                            range,
                            ErrorKind::Unknown,
                            format!(
                                "Class member `{}` overrides parent class `{}` in an inconsistent manner",
                                name,
                                parent.name()
                            ),
                        );
                    }
                }
            }
            if is_override && !parent_attr_found && !parent_has_any {
                self.error(
                    errors,
                    range,
                    ErrorKind::Unknown,
                    format!(
                        "Class member `{}` is marked as an override, but no parent class has a matching attribute",
                        name,
                    ),
                );
            }
        };
    }

    pub fn get_class_field_non_synthesized(
        &self,
        cls: &Class,
        name: &Name,
    ) -> Option<Arc<ClassField>> {
        if cls.contains(name) {
            let field = self.get_from_class(cls, &KeyClassField(cls.index(), name.clone()));
            Some(field)
        } else {
            None
        }
    }

    pub fn get_class_field(&self, cls: &Class, name: &Name) -> Option<Arc<ClassField>> {
        if let Some(field) = self.get_class_field_non_synthesized(cls, name) {
            Some(field)
        } else {
            let synthesized_fields =
                self.get_from_class(cls, &KeyClassSynthesizedFields(cls.index()));
            let synth = synthesized_fields.get(name);
            synth.map(|f| f.inner.dupe())
        }
    }

    pub(in crate::alt::class) fn get_class_member(
        &self,
        cls: &Class,
        name: &Name,
    ) -> Option<WithDefiningClass<Arc<ClassField>>> {
        if let Some(field) = self.get_class_field(cls, name) {
            Some(WithDefiningClass {
                value: field,
                defining_class: cls.dupe(),
            })
        } else {
            self.get_metadata_for_class(cls)
                .ancestors(self.stdlib)
                .find_map(|ancestor| {
                    self.get_class_field(ancestor.class_object(), name)
                        .map(|field| WithDefiningClass {
                            value: Arc::new(field.instantiate_for(ancestor)),
                            defining_class: ancestor.class_object().dupe(),
                        })
                })
        }
    }

    pub fn get_instance_attribute(&self, cls: &ClassType, name: &Name) -> Option<Attribute> {
        self.get_class_member(cls.class_object(), name)
            .map(|member| self.as_instance_attribute(Arc::unwrap_or_clone(member.value), cls))
    }

    /// Looks up an attribute on a super instance.
    pub fn get_super_attribute(
        &self,
        lookup_cls: &ClassType,
        super_obj: &ClassType,
        name: &Name,
    ) -> Option<Attribute> {
        self.get_class_member(lookup_cls.class_object(), name)
            .map(|member| self.as_instance_attribute(Arc::unwrap_or_clone(member.value), super_obj))
    }

    /// Gets an attribute from a class definition.
    ///
    /// Returns `None` if there is no such attribute, otherwise an `Attribute` object
    /// that describes whether access is allowed and the type if so.
    ///
    /// Access is disallowed for instance-only attributes and for attributes whose
    /// type contains a class-scoped type parameter - e.g., `class A[T]: x: T`.
    pub fn get_class_attribute(&self, cls: &Class, name: &Name) -> Option<Attribute> {
        self.get_class_member(cls, name)
            .map(|member| self.as_class_attribute(Arc::unwrap_or_clone(member.value), cls))
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
            Arc::unwrap_or_clone(new_member.value).as_raw_special_method_type(cls)
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
            Arc::unwrap_or_clone(init_method.value).as_special_method_type(cls)
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
            Arc::unwrap_or_clone(attr.value).as_special_method_type(metaclass)
        }
    }
}
