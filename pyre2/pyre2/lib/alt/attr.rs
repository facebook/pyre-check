/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use itertools::Either;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_text_size::TextRange;
use starlark_map::small_set::SmallSet;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::callable::CallArg;
use crate::alt::types::class_metadata::EnumMetadata;
use crate::binding::binding::KeyExport;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::export::exports::Exports;
use crate::export::exports::LookupExport;
use crate::module::module_info::ModuleInfo;
use crate::module::module_name::ModuleName;
use crate::types::callable::Param;
use crate::types::callable::Required;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::module::Module;
use crate::types::quantified::Quantified;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::types::AnyStyle;
use crate::types::types::Decoration;
use crate::types::types::Type;

#[derive(Debug)]
enum LookupResult {
    /// The lookup succeeded, resulting in a type.
    Found(Attribute),
    /// The attribute was not found. Callers can use fallback behavior, for
    /// example looking up a different attribute.
    NotFound(NotFound),
    /// There was a Pyre-internal error
    InternalError(InternalError),
}

/// The result of looking up an attribute. We can analyze get and set actions
/// on an attribute, each of which can be allowed with some type or disallowed.
#[derive(Debug)]
pub struct Attribute {
    inner: AttributeInner,
}

/// The result of an attempt to access an attribute (with a get or set operation).
///
/// The operation is either permitted with an attribute `Type`, or is not allowed
/// and has a reason.
#[derive(Clone, Debug)]
enum AttributeInner {
    /// A `NoAccess` attribute indicates that the attribute is well-defined, but does
    /// not allow the access pattern (for example class access on an instance-only attribute)
    NoAccess(NoAccessReason),
    /// A read-write attribute with a closed form type for both get and set actions.
    ReadWrite(Type),
    /// A read-only attribute.
    ReadOnly(Type),
    /// A property is a special attribute were regular access invokes a getter.
    /// It optionally might have a setter method; if not, trying to set it is an access error
    Property(Type, Option<Type>, Class),
    /// A descriptor is a user-defined type whose actions may dispatch to special method calls
    /// for the get and set actions.
    Descriptor(Descriptor),
}

#[derive(Clone, Debug)]
struct Descriptor {
    /// This is the raw type of the descriptor, which is needed both for attribute subtyping
    /// checks in structural types and in the case where there is no getter method.
    descriptor_ty: Type,
    /// Descriptor behavior depends on the base against which the attribute is resolved, so
    /// we have to preserve information about whether it is a class instance or class def.
    base: DescriptorBase,
    /// If `__get__` exists on the descriptor, this is the type of `__get__`
    /// method type (as resolved by accessing it on an instance of the
    /// desriptor). It is typically a `BoundMethod` although it is possible for
    /// a user to erronously define a `__get__` with any type, including a
    /// non-callable one.
    getter: Option<Type>,
    /// If `__set__` exists on the descriptor, this is the type of `__set__`. Similar considerations
    /// to `getter` apply.
    setter: Option<Type>,
}

#[derive(Clone, Debug)]
pub enum DescriptorBase {
    Instance(ClassType),
    ClassDef(Class),
}

#[derive(Debug)]
enum NotFound {
    Attribute(ClassType),
    ClassAttribute(Class),
    ModuleExport(Module),
}

#[derive(Clone, Debug)]
pub enum NoAccessReason {
    /// The attribute is only initialized on instances, but we saw an attempt
    /// to use it as a class attribute.
    ClassUseOfInstanceAttribute(Class),
    /// A generic class attribute exists, but has an invalid definition.
    /// Callers should treat the attribute as `Any`.
    ClassAttributeIsGeneric(Class),
    /// A set operation on a read-only property is an access error.
    SettingReadOnlyProperty(Class),
    /// A descriptor that only has `__get__` should be treated as read-only on instances.
    SettingReadOnlyDescriptor(Class),
    /// We do not allow class-level mutation of descriptors (this is conservative,
    /// it is unspecified whether monkey-patching descriptors should be permitted).
    SettingDescriptorOnClass(Class),
}

#[derive(Debug)]
enum InternalError {
    /// An internal error caused by `as_attribute_base` being partial.
    AttributeBaseUndefined(Type),
}

impl Attribute {
    pub fn no_access(reason: NoAccessReason) -> Self {
        Attribute {
            inner: AttributeInner::NoAccess(reason),
        }
    }

    pub fn read_write(ty: Type) -> Self {
        Attribute {
            inner: AttributeInner::ReadWrite(ty),
        }
    }

    pub fn read_only(ty: Type) -> Self {
        Attribute {
            inner: AttributeInner::ReadOnly(ty),
        }
    }

    pub fn property(getter: Type, setter: Option<Type>, cls: Class) -> Self {
        Attribute {
            inner: AttributeInner::Property(getter, setter, cls),
        }
    }

    pub fn descriptor(
        ty: Type,
        base: DescriptorBase,
        getter: Option<Type>,
        setter: Option<Type>,
    ) -> Self {
        Attribute {
            inner: AttributeInner::Descriptor(Descriptor {
                descriptor_ty: ty,
                base,
                getter,
                setter,
            }),
        }
    }
}

impl NoAccessReason {
    pub fn to_error_msg(&self, attr_name: &Name) -> String {
        match self {
            NoAccessReason::ClassUseOfInstanceAttribute(class) => {
                let class_name = class.name();
                format!(
                    "Instance-only attribute `{attr_name}` of class `{class_name}` is not visible on the class"
                )
            }
            NoAccessReason::ClassAttributeIsGeneric(class) => {
                let class_name = class.name();
                format!(
                    "Generic attribute `{attr_name}` of class `{class_name}` is not visible on the class"
                )
            }
            NoAccessReason::SettingReadOnlyProperty(class) => {
                let class_name = class.name();
                format!(
                    "Attribute `{attr_name}` of class `{class_name}` is a read-only property and cannot be set"
                )
            }
            NoAccessReason::SettingDescriptorOnClass(class) => {
                let class_name = class.name();
                format!(
                    "Attribute `{attr_name}` of class `{class_name}` is a descriptor, which may not be overwritten"
                )
            }
            NoAccessReason::SettingReadOnlyDescriptor(class) => {
                let class_name = class.name();
                format!(
                    "Attribute `{attr_name}` of class `{class_name}` is a read-only descriptor with no `__set__` and cannot be set"
                )
            }
        }
    }
}

impl LookupResult {
    /// We found a simple attribute type.
    ///
    /// This means we assume it is both readable and writable with that type.
    ///
    /// TODO(stroxler) The uses of this eventually need to be audited, but we
    /// need to prioiritize the class logic first.
    fn found_type(ty: Type) -> Self {
        Self::Found(Attribute::read_write(ty))
    }
}

impl NotFound {
    pub fn to_error_msg(self, attr_name: &Name) -> String {
        match self {
            NotFound::Attribute(class_type) => {
                let class_name = class_type.name();
                format!("Object of class `{class_name}` has no attribute `{attr_name}`",)
            }
            NotFound::ClassAttribute(class) => {
                let class_name = class.name();
                format!("Class `{class_name}` has no class attribute `{attr_name}`")
            }
            NotFound::ModuleExport(module) => {
                format!("No attribute `{attr_name}` in module `{module}`")
            }
        }
    }
}

impl InternalError {
    pub fn to_error_msg(self, attr_name: &Name, todo_ctx: &str) -> String {
        match self {
            InternalError::AttributeBaseUndefined(ty) => format!(
                "TODO: {todo_ctx} attribute base undefined for type: {} (trying to access {})",
                ty.deterministic_printing(),
                attr_name
            ),
        }
    }
}

/// A normalized type representation which is convenient for attribute lookup,
/// since many cases are collapsed. For example, Type::Literal is converted to
/// it's corresponding class type.
#[derive(Debug)]
enum AttributeBase {
    ClassInstance(ClassType),
    ClassObject(Class),
    Module(Module),
    Quantified(Quantified),
    Any(AnyStyle),
    Never,
    /// type[Any] is a special case where attribute lookups first check the
    /// builtin `type` class before falling back to `Any`.
    TypeAny(AnyStyle),
    /// Properties are handled via a special case so that we can understand
    /// setter decorators.
    Property(Type),
    /// Result of a super() call. See Type::SuperInstance for details on what these ClassTypes are.
    SuperInstance(ClassType, ClassType),
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    /// Compute the get (i.e. read) type of an attribute. If the attribute cannot be found or read,
    /// error and return `Any`. Use this to infer the type of a direct attribute fetch.
    pub fn type_of_attr_get(
        &self,
        base: Type,
        attr_name: &Name,
        range: TextRange,
        errors: &ErrorCollector,
        todo_ctx: &str,
    ) -> Type {
        let lookup_result = self.lookup_attr(base, attr_name);
        match self.get_type_or_conflated_error_msg(
            lookup_result,
            attr_name,
            range,
            errors,
            todo_ctx,
        ) {
            Ok(ty) => ty,
            Err(msg) => self.error(errors, range, ErrorKind::Unknown, msg),
        }
    }

    /// Compute the get (i.e. read) type of an attribute, if it can be found. If read is not
    /// permitted, error and return `Some(Any)`. If no attribute is found, return `None`. Use this
    /// to infer special methods where we can fall back if it is missing (for example binary operators).
    pub fn type_of_attr_get_if_found(
        &self,
        base: Type,
        attr_name: &Name,
        range: TextRange,
        errors: &ErrorCollector,
        todo_ctx: &str,
    ) -> Option<Type> {
        match self.lookup_attr(base, attr_name) {
            LookupResult::Found(attr) => match self.resolve_get_access(attr, range, errors) {
                Ok(ty) => Some(ty),
                Err(e) => {
                    Some(self.error(errors, range, ErrorKind::Unknown, e.to_error_msg(attr_name)))
                }
            },
            LookupResult::InternalError(e) => Some(self.error(
                errors,
                range,
                ErrorKind::Unknown,
                e.to_error_msg(attr_name, todo_ctx),
            )),
            _ => None,
        }
    }

    /// Look up the `_value_` attribute of an enum class. This field has to be a plain instance
    /// attribute annotated in the class body; it is used to validate enum member values, which are
    /// supposed to all share this type.
    pub fn type_of_enum_value(&self, enum_: &EnumMetadata) -> Option<Type> {
        let base = Type::ClassType(enum_.cls.clone());
        match self.lookup_attr(base, &Name::new_static("_value_")) {
            LookupResult::Found(attr) => match attr.inner {
                AttributeInner::ReadWrite(ty) => Some(ty),
                AttributeInner::ReadOnly(_)
                | AttributeInner::NoAccess(_)
                | AttributeInner::Property(..)
                | AttributeInner::Descriptor(..) => None,
            },
            _ => None,
        }
    }

    /// Here `got` can be either an Expr or a Type so that we can support contextually
    /// typing whenever the expression is available.
    fn check_attr_set(
        &self,
        base: Type,
        attr_name: &Name,
        got: Either<&Expr, &Type>,
        range: TextRange,
        errors: &ErrorCollector,
        todo_ctx: &str,
    ) {
        match self.lookup_attr(base, attr_name) {
            LookupResult::Found(attr) => match attr.inner {
                AttributeInner::NoAccess(e) => {
                    self.error(errors, range, ErrorKind::Unknown, e.to_error_msg(attr_name));
                }
                AttributeInner::ReadWrite(want) => match got {
                    Either::Left(got) => {
                        self.expr(got, Some(&want), errors);
                    }
                    Either::Right(got) => {
                        if !self.solver().is_subset_eq(got, &want, self.type_order()) {
                            self.error(
                                errors,
                                range,
                                ErrorKind::Unknown,
                                format!(
                                    "Could not assign type `{}` to attribute `{}` with type `{}`",
                                    got.clone().deterministic_printing(),
                                    attr_name,
                                    want.deterministic_printing(),
                                ),
                            );
                        }
                    }
                },
                AttributeInner::ReadOnly(_) => {
                    self.error(
                        errors,
                        range,
                        ErrorKind::Unknown,
                        format!("Could not assign to read-only field `{attr_name}`"),
                    );
                }
                AttributeInner::Property(_, None, cls) => {
                    let e = NoAccessReason::SettingReadOnlyProperty(cls);
                    self.error(errors, range, ErrorKind::Unknown, e.to_error_msg(attr_name));
                }
                AttributeInner::Property(_, Some(setter), _) => {
                    let got = match &got {
                        Either::Left(got) => CallArg::Expr(got),
                        Either::Right(got) => CallArg::Type(got, range),
                    };
                    self.call_property_setter(setter, got, range, errors);
                }
                AttributeInner::Descriptor(d) => {
                    match (d.base, d.setter) {
                        (DescriptorBase::Instance(class_type), Some(setter)) => {
                            let got = match &got {
                                Either::Left(got) => CallArg::Expr(got),
                                Either::Right(got) => CallArg::Type(got, range),
                            };
                            self.call_descriptor_setter(setter, class_type, got, range, errors);
                        }
                        (DescriptorBase::Instance(class_type), None) => {
                            let e = NoAccessReason::SettingReadOnlyDescriptor(
                                class_type.class_object().dupe(),
                            );
                            self.error(
                                errors,
                                range,
                                ErrorKind::Unknown,
                                e.to_error_msg(attr_name),
                            );
                        }
                        (DescriptorBase::ClassDef(class), _) => {
                            let e = NoAccessReason::SettingDescriptorOnClass(class.dupe());
                            self.error(
                                errors,
                                range,
                                ErrorKind::Unknown,
                                e.to_error_msg(attr_name),
                            );
                        }
                    };
                }
            },
            LookupResult::InternalError(e) => {
                self.error(
                    errors,
                    range,
                    ErrorKind::Unknown,
                    e.to_error_msg(attr_name, todo_ctx),
                );
            }
            LookupResult::NotFound(e) => {
                self.error(errors, range, ErrorKind::Unknown, e.to_error_msg(attr_name));
            }
        }
    }

    pub fn check_attr_set_with_expr(
        &self,
        base: Type,
        attr_name: &Name,
        got: &Expr,
        range: TextRange,
        errors: &ErrorCollector,
        todo_ctx: &str,
    ) {
        self.check_attr_set(base, attr_name, Either::Left(got), range, errors, todo_ctx)
    }

    pub fn check_attr_set_with_type(
        &self,
        base: Type,
        attr_name: &Name,
        got: &Type,
        range: TextRange,
        errors: &ErrorCollector,
        todo_ctx: &str,
    ) {
        self.check_attr_set(base, attr_name, Either::Right(got), range, errors, todo_ctx)
    }

    pub fn is_attr_subset(
        &self,
        got: &Attribute,
        want: &Attribute,
        is_subset: &mut dyn FnMut(&Type, &Type) -> bool,
    ) -> bool {
        match (&got.inner, &want.inner) {
            (_, AttributeInner::NoAccess(_)) => true,
            (AttributeInner::NoAccess(_), _) => false,
            (
                AttributeInner::Property(_, _, _),
                AttributeInner::ReadOnly(_) | AttributeInner::ReadWrite(_),
            ) => false,
            (
                AttributeInner::ReadOnly(_),
                AttributeInner::Property(_, Some(_), _) | AttributeInner::ReadWrite(_),
            ) => false,
            (
                AttributeInner::ReadWrite(got @ Type::BoundMethod(_)),
                AttributeInner::ReadWrite(want @ Type::BoundMethod(_)),
            ) => is_subset(got, want),
            (AttributeInner::ReadWrite(got), AttributeInner::ReadWrite(want)) => {
                is_subset(got, want) && is_subset(want, got)
            }
            (
                AttributeInner::ReadOnly(got) | AttributeInner::ReadWrite(got),
                AttributeInner::ReadOnly(want),
            ) => is_subset(got, want),
            (AttributeInner::ReadOnly(got), AttributeInner::Property(want, _, _)) => {
                is_subset(
                    // Synthesize a getter method
                    &Type::callable_ellipsis(got.clone()),
                    want,
                )
            }
            (AttributeInner::ReadWrite(got), AttributeInner::Property(want, want_setter, _)) => {
                if !is_subset(
                    // Synthesize a getter method
                    &Type::callable_ellipsis(got.clone()),
                    want,
                ) {
                    return false;
                }
                if let Some(want_setter) = want_setter {
                    // Synthesize a setter method
                    is_subset(
                        want_setter,
                        &Type::callable(
                            vec![Param::PosOnly(got.clone(), Required::Required)],
                            Type::None,
                        ),
                    )
                } else {
                    true
                }
            }
            (
                AttributeInner::Property(got_getter, got_setter, _),
                AttributeInner::Property(want_getter, want_setter, _),
            ) => {
                if !is_subset(got_getter, want_getter) {
                    false
                } else {
                    match (got_setter, want_setter) {
                        (Some(got_setter), Some(want_setter)) => is_subset(got_setter, want_setter),
                        (None, Some(_)) => false,
                        (_, None) => true,
                    }
                }
            }
            (
                AttributeInner::Descriptor(
                    Descriptor {
                        descriptor_ty: got_ty,
                        ..
                    },
                    ..,
                ),
                AttributeInner::Descriptor(
                    Descriptor {
                        descriptor_ty: want_ty,
                        ..
                    },
                    ..,
                ),
            ) => is_subset(got_ty, want_ty),
            (AttributeInner::Descriptor(..), _) | (_, AttributeInner::Descriptor(..)) => false,
        }
    }

    fn resolve_get_access(
        &self,
        attr: Attribute,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Result<Type, NoAccessReason> {
        match attr.inner {
            AttributeInner::NoAccess(reason) => Err(reason),
            AttributeInner::ReadWrite(ty) | AttributeInner::ReadOnly(ty) => Ok(ty),
            AttributeInner::Property(getter, ..) => {
                Ok(self.call_property_getter(getter, range, errors))
            }
            AttributeInner::Descriptor(d, ..) => {
                match d {
                    // Reading a descriptor with a getter resolves to a method call
                    //
                    // TODO(stroxler): Once we have more complex error traces, it would be good to pass
                    // context down so that errors inside the call can mention that it was a descriptor read.
                    Descriptor {
                        base,
                        getter: Some(getter),
                        ..
                    } => Ok(self.call_descriptor_getter(getter, base, range, errors)),
                    // Reading descriptor with no getter resolves to the descriptor itself
                    Descriptor {
                        descriptor_ty,
                        getter: None,
                        ..
                    } => Ok(descriptor_ty),
                }
            }
        }
    }

    /// A convenience function for callers who don't care about reasons a lookup failed and are
    /// only interested in simple, read-write attributes (in particular, this covers instance access to
    /// regular methods, and is useful for edge cases where we handle cases like `__call__` and `__new__`).
    pub fn resolve_as_instance_method(&self, attr: Attribute) -> Option<Type> {
        self.resolve_as_instance_method_with_attribute_inner(attr.inner)
    }

    fn resolve_as_instance_method_with_attribute_inner(
        &self,
        inner: AttributeInner,
    ) -> Option<Type> {
        match inner {
            AttributeInner::ReadWrite(ty) => Some(ty),
            AttributeInner::ReadOnly(_)
            | AttributeInner::NoAccess(_)
            | AttributeInner::Property(..)
            | AttributeInner::Descriptor(..) => None,
        }
    }

    /// A convenience function for callers which want an error but do not need to distinguish
    /// between NotFound and Error results.
    fn get_type_or_conflated_error_msg(
        &self,
        lookup: LookupResult,
        attr_name: &Name,
        range: TextRange,
        errors: &ErrorCollector,
        todo_ctx: &str,
    ) -> Result<Type, String> {
        match lookup {
            LookupResult::Found(attr) => match self.resolve_get_access(attr, range, errors) {
                Ok(ty) => Ok(ty.clone()),
                Err(err) => Err(err.to_error_msg(attr_name)),
            },
            LookupResult::NotFound(err) => Err(err.to_error_msg(attr_name)),
            LookupResult::InternalError(err) => Err(err.to_error_msg(attr_name, todo_ctx)),
        }
    }

    fn lookup_attr(&self, base: Type, attr_name: &Name) -> LookupResult {
        match self.as_attribute_base(base.clone(), self.stdlib) {
            Some(AttributeBase::ClassInstance(class)) => {
                match self.get_instance_attribute(&class, attr_name) {
                    Some(attr) => LookupResult::Found(attr),
                    None => LookupResult::NotFound(NotFound::Attribute(class)),
                }
            }
            Some(AttributeBase::SuperInstance(cls, obj)) => {
                match self.get_super_attribute(&cls, &obj, attr_name) {
                    Some(attr) => LookupResult::Found(attr),
                    None => LookupResult::NotFound(NotFound::Attribute(cls)),
                }
            }
            Some(AttributeBase::ClassObject(class)) => {
                match self.get_class_attribute(&class, attr_name) {
                    Some(attr) => LookupResult::Found(attr),
                    None => {
                        // Classes are instances of their metaclass, which defaults to `builtins.type`.
                        let metadata = self.get_metadata_for_class(&class);
                        let instance_attr = match metadata.metaclass() {
                            Some(meta) => self.get_instance_attribute(meta, attr_name),
                            None => {
                                self.get_instance_attribute(&self.stdlib.builtins_type(), attr_name)
                            }
                        };
                        match instance_attr {
                            Some(attr) => LookupResult::Found(attr),
                            None => LookupResult::NotFound(NotFound::ClassAttribute(class)),
                        }
                    }
                }
            }
            Some(AttributeBase::Module(module)) => match self.get_module_attr(&module, attr_name) {
                // TODO(samzhou19815): Support module attribute go-to-definition
                Some(attr) => LookupResult::found_type(attr),
                None => LookupResult::NotFound(NotFound::ModuleExport(module)),
            },
            Some(AttributeBase::Quantified(q)) => {
                if q.is_param_spec() && attr_name == "args" {
                    LookupResult::found_type(Type::type_form(Type::Args(q)))
                } else if q.is_param_spec() && attr_name == "kwargs" {
                    LookupResult::found_type(Type::type_form(Type::Kwargs(q)))
                } else {
                    let class = q.as_value(self.stdlib);
                    match self.get_instance_attribute(&class, attr_name) {
                        Some(attr) => LookupResult::Found(attr),
                        None => LookupResult::NotFound(NotFound::Attribute(class)),
                    }
                }
            }
            Some(AttributeBase::TypeAny(style)) => {
                let builtins_type_classtype = self.stdlib.builtins_type();
                self.get_instance_attribute(&builtins_type_classtype, attr_name)
                    .and_then(|Attribute { inner }| {
                        self.resolve_as_instance_method_with_attribute_inner(inner)
                            .map(LookupResult::found_type)
                    })
                    .map_or_else(
                        || LookupResult::found_type(style.propagate()),
                        |result| result,
                    )
            }
            Some(AttributeBase::Any(style)) => LookupResult::found_type(style.propagate()),
            Some(AttributeBase::Never) => LookupResult::found_type(Type::never()),
            Some(AttributeBase::Property(getter)) => {
                // TODO(stroxler): it is probably possible to synthesize a forall type here
                // that uses a type var to propagate the setter instead of using a `Decoration`
                // with hardcoded support in `apply_decorator`. Investigate this option later.
                LookupResult::found_type(
                    // TODO(samzhou19815): Support go-to-definition for @property applied symbols
                    Type::Decoration(Decoration::PropertySetterDecorator(Box::new(getter))),
                )
            }
            None => LookupResult::InternalError(InternalError::AttributeBaseUndefined(base)),
        }
    }

    pub fn try_lookup_attr(&self, base: Type, attr_name: &Name) -> Option<Attribute> {
        match self.lookup_attr(base, attr_name) {
            LookupResult::Found(attr) => Some(attr),
            _ => None,
        }
    }

    fn get_module_exports(&self, module_name: ModuleName) -> Option<Exports> {
        self.exports.get(module_name).ok()
    }

    fn get_exported_type(&self, exports: &Exports, from: ModuleName, name: &Name) -> Option<Type> {
        if exports.contains(name, self.exports) {
            Some(
                self.get_from_module(from, &KeyExport(name.clone()))
                    .arc_clone(),
            )
        } else {
            None
        }
    }

    fn get_module_attr(&self, module: &Module, attr_name: &Name) -> Option<Type> {
        let module_name = ModuleName::from_parts(module.path());
        match self.get_module_exports(module_name) {
            None => {
                // We have already errored on `m` when loading the module. No need to emit error again.
                Some(Type::any_error())
            }
            Some(exports) => self
                .get_exported_type(&exports, module_name, attr_name)
                .or_else(|| {
                    // `module_name` could also refer to a package, in which case we need to check if
                    // `module_name.attr_name`:
                    // - Has been imported directly. Unless `module_name` re-exports `attr_name` from itself
                    //   (in which can `attr_name` will be included in the exports map), we want to make sure that
                    //   `module_name.attr_name` is only valid when there's an explicit import statement for either
                    //   `module_name.attr_name` or its submodules. Just importing `module_name`, for example,
                    //   shouldn't automatically make the submodule name `module_name.attr_name` accessible.
                    // - Actually exists as a submodule on the filesystem.
                    let submodule = module.push_path(attr_name.clone());
                    let submodule_name = module_name.append(attr_name);
                    if submodule.is_submodules_imported_directly()
                        && self.get_module_exports(submodule_name).is_some()
                    {
                        Some(submodule.to_type())
                    } else {
                        None
                    }
                }),
        }
    }

    fn as_attribute_base(&self, ty: Type, stdlib: &Stdlib) -> Option<AttributeBase> {
        match ty {
            Type::ClassType(class_type) => Some(AttributeBase::ClassInstance(class_type)),
            Type::ClassDef(cls) => Some(AttributeBase::ClassObject(cls)),
            Type::TypedDict(_) => Some(AttributeBase::ClassInstance(stdlib.mapping(
                stdlib.str().to_type(),
                stdlib.object_class_type().clone().to_type(),
            ))),
            Type::Tuple(Tuple::Unbounded(box element)) => {
                Some(AttributeBase::ClassInstance(stdlib.tuple(element)))
            }
            Type::Tuple(Tuple::Concrete(elements)) => {
                Some(AttributeBase::ClassInstance(if elements.is_empty() {
                    stdlib.tuple(Type::Any(AnyStyle::Implicit))
                } else {
                    stdlib.tuple(self.unions(elements))
                }))
            }
            Type::Tuple(Tuple::Unpacked(box (
                prefix,
                Type::Tuple(Tuple::Unbounded(box middle)),
                suffix,
            ))) => {
                let mut elements = prefix;
                elements.push(middle);
                elements.extend(suffix);
                Some(AttributeBase::ClassInstance(
                    stdlib.tuple(self.unions(elements)),
                ))
            }
            // TODO(yangdanny): the middle part is AttributeBase::Quantified
            // but we can only return one attribute base
            Type::Tuple(Tuple::Unpacked(_)) => Some(AttributeBase::ClassInstance(
                stdlib.tuple(Type::any_implicit()),
            )),
            Type::LiteralString => Some(AttributeBase::ClassInstance(stdlib.str())),
            Type::Literal(lit) => {
                Some(AttributeBase::ClassInstance(lit.general_class_type(stdlib)))
            }
            Type::TypeGuard(_) | Type::TypeIs(_) => {
                Some(AttributeBase::ClassInstance(stdlib.bool()))
            }
            Type::Any(style) => Some(AttributeBase::Any(style)),
            Type::TypeAlias(ta) => self.as_attribute_base(ta.as_value(stdlib), stdlib),
            Type::Type(box Type::ClassType(class)) => {
                Some(AttributeBase::ClassObject(class.class_object().dupe()))
            }
            Type::Type(box Type::Quantified(q)) => Some(AttributeBase::Quantified(q)),
            Type::Type(box Type::Any(style)) => Some(AttributeBase::TypeAny(style)),
            Type::Module(module) => Some(AttributeBase::Module(module)),
            Type::TypeVar(_) => Some(AttributeBase::ClassInstance(stdlib.type_var())),
            Type::ParamSpec(_) => Some(AttributeBase::ClassInstance(stdlib.param_spec())),
            Type::TypeVarTuple(_) => Some(AttributeBase::ClassInstance(stdlib.type_var_tuple())),
            Type::Args(_) => Some(AttributeBase::ClassInstance(stdlib.param_spec_args())),
            Type::Kwargs(_) => Some(AttributeBase::ClassInstance(stdlib.param_spec_kwargs())),
            Type::None => Some(AttributeBase::ClassInstance(stdlib.none_type())),
            Type::Never(_) => Some(AttributeBase::Never),
            Type::Callable(_, _) | Type::Overload(_) => {
                Some(AttributeBase::ClassInstance(stdlib.function_type()))
            }
            Type::BoundMethod(_) => Some(AttributeBase::ClassInstance(stdlib.method_type())),
            Type::Ellipsis => Some(AttributeBase::ClassInstance(stdlib.ellipsis_type())),
            Type::Forall(_, box base) => self.as_attribute_base(base, stdlib),
            Type::Var(v) => {
                if let Some(_guard) = self.recurser.recurse(v) {
                    self.as_attribute_base(self.solver().force_var(v), stdlib)
                } else {
                    None
                }
            }
            Type::Decoration(Decoration::Property(box (getter, _))) => {
                Some(AttributeBase::Property(getter))
            }
            Type::Decoration(Decoration::EnumMember(box ty)) => self.as_attribute_base(ty, stdlib),
            Type::Decoration(_) => None,
            Type::SuperInstance(cls, obj) => Some(AttributeBase::SuperInstance(*cls, *obj)),
            // TODO: check to see which ones should have class representations
            Type::Union(_)
            | Type::SpecialForm(_)
            | Type::Type(_)
            | Type::Intersect(_)
            | Type::Unpack(_)
            | Type::Concatenate(_, _)
            | Type::ParamSpecValue(_)
            | Type::Quantified(_) => None,
        }
    }
}

#[derive(Debug)]
pub struct AttrInfo {
    pub name: Name,
    pub module: Option<ModuleInfo>,
    pub range: Option<TextRange>,
}

impl<'a, Ans: LookupAnswer + LookupExport> AnswersSolver<'a, Ans> {
    fn completions_class(&self, cls: &Class, res: &mut Vec<AttrInfo>) {
        let mut seen = SmallSet::new();
        for c in std::iter::once(cls).chain(
            self.get_metadata_for_class(cls)
                .ancestors(self.stdlib)
                .map(|x| x.class_object()),
        ) {
            if c == self.stdlib.object_class_type().class_object() {
                // Don't want to suggest `__hash__`
                break;
            }
            for fld in c.fields() {
                if seen.insert(fld.clone()) {
                    res.push(AttrInfo {
                        name: fld.clone(),
                        module: Some(c.module_info().dupe()),
                        range: c.field_decl_range(fld),
                    });
                }
            }
        }
    }

    fn completions_class_type(&self, cls: &ClassType, res: &mut Vec<AttrInfo>) {
        self.completions_class(cls.class_object(), res);
    }

    fn completions_module(&self, module: &Module, res: &mut Vec<AttrInfo>) {
        let module_name = ModuleName::from_parts(module.path());
        if let Some(exports) = self.get_module_exports(module_name) {
            res.extend(exports.wildcard(self.exports).iter().map(|x| AttrInfo {
                name: x.clone(),
                module: None,
                range: None,
            }))
        }
    }

    /// List all the attributes available from a type. Used to power completion.
    pub fn completions(&self, base: Type) -> Vec<AttrInfo> {
        let mut res = Vec::new();

        match self.as_attribute_base(base, self.stdlib) {
            Some(AttributeBase::ClassInstance(class)) => {
                self.completions_class_type(&class, &mut res)
            }
            Some(AttributeBase::SuperInstance(class, _)) => {
                self.completions_class_type(&class, &mut res)
            }
            Some(AttributeBase::ClassObject(class)) => self.completions_class(&class, &mut res),
            Some(AttributeBase::Quantified(q)) => {
                self.completions_class_type(&q.as_value(self.stdlib), &mut res)
            }
            Some(AttributeBase::TypeAny(_)) => {
                self.completions_class_type(&self.stdlib.builtins_type(), &mut res)
            }
            Some(AttributeBase::Module(module)) => {
                self.completions_module(&module, &mut res);
            }
            Some(AttributeBase::Any(_)) => {}
            Some(AttributeBase::Never) => {}
            Some(AttributeBase::Property(_)) => {
                // TODO(samzhou19815): Support autocomplete for properties
                {}
            }
            None => {}
        }
        res
    }
}
