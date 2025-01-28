/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;
use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::module::Module;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::types::AnyStyle;
use crate::types::types::Quantified;
use crate::types::types::Type;

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
pub struct Attribute(AttributeAccess);

/// The result of an attempt to access an attribute (with a get or set operation).
///
/// The operation is either permitted with an attribute `Type`, or is not allowed
/// and has a reason.
#[derive(Clone)]
struct AttributeAccess(Result<Type, AccessNotAllowed>);

impl AttributeAccess {
    fn allowed(ty: Type) -> Self {
        AttributeAccess(Ok(ty))
    }

    pub fn not_allowed(reason: AccessNotAllowed) -> Self {
        AttributeAccess(Err(reason))
    }
}

enum NotFound {
    Attribute(ClassType),
    ClassAttribute(Class),
    ModuleExport(Module),
}

#[derive(Clone)]
pub enum AccessNotAllowed {
    /// The attribute is only initialized on instances, but we saw an attempt
    /// to use it as a class attribute.
    ClassUseOfInstanceAttribute(Class),
    /// A generic class attribute exists, but has an invalid definition.
    /// Callers should treat the attribute as `Any`.
    ClassAttributeIsGeneric(Class),
}

enum InternalError {
    /// An internal error caused by `as_attribute_base` being partial.
    AttributeBaseUndefined(Type),
}

impl Attribute {
    pub fn access_allowed(ty: Type) -> Self {
        Attribute(AttributeAccess::allowed(ty))
    }
    pub fn access_not_allowed(reason: AccessNotAllowed) -> Self {
        Attribute(AttributeAccess::not_allowed(reason))
    }

    fn get(self) -> AttributeAccess {
        self.0
    }

    pub fn get_type(self) -> Option<Type> {
        match self.0.0 {
            Ok(ty) => Some(ty),
            _ => None,
        }
    }
}

impl AccessNotAllowed {
    pub fn to_error_msg(&self, attr_name: &Name) -> String {
        match self {
            AccessNotAllowed::ClassUseOfInstanceAttribute(class) => {
                let class_name = class.name();
                format!(
                    "Instance-only attribute `{attr_name}` of class `{class_name}` is not visible on the class"
                )
            }
            AccessNotAllowed::ClassAttributeIsGeneric(class) => {
                let class_name = class.name();
                format!(
                    "Generic attribute `{attr_name}` of class `{class_name}` is not visible on the class"
                )
            }
        }
    }
}

impl LookupResult {
    /// We found a simple attribute type.
    ///
    /// This means we assume it is both readable and writeable with that type.
    ///
    /// TODO(stroxler) The uses of this eventually need to be audited, but we
    /// need to prioiritize the class logic first.
    fn found_type(ty: Type) -> Self {
        Self::Found(Attribute::access_allowed(ty))
    }

    /// A convenience function for callers which do not need to distinguish
    /// between NotFound and Error results.
    pub fn get_type_or_conflated_error_msg(
        self,
        attr_name: &Name,
        todo_ctx: &str,
    ) -> Result<Type, String> {
        match self {
            LookupResult::Found(attr) => match &attr.get() {
                AttributeAccess(Ok(ty)) => Ok(ty.clone()),
                AttributeAccess(Err(err)) => Err(err.to_error_msg(attr_name)),
            },
            LookupResult::NotFound(err) => Err(err.to_error_msg(attr_name)),
            LookupResult::InternalError(err) => Err(err.to_error_msg(attr_name, todo_ctx)),
        }
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
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    /// Compute the get (i.e. read) type of an attribute. If the attribute cannot be found or read,
    /// error and return `Any`. Use this to infer the type of a direct attribute fetch.
    pub fn type_of_attr_get(
        &self,
        base: Type,
        attr_name: &Name,
        range: TextRange,
        todo_ctx: &str,
    ) -> Type {
        match self
            .lookup_attr(base, attr_name)
            .get_type_or_conflated_error_msg(attr_name, todo_ctx)
        {
            Ok(ty) => ty,
            Err(msg) => self.error(range, msg),
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
        todo_ctx: &str,
    ) -> Option<Type> {
        match self.lookup_attr(base, attr_name) {
            LookupResult::Found(attr) => match attr.get() {
                AttributeAccess(Ok(ty)) => Some(ty),
                AttributeAccess(Err(e)) => Some(self.error(range, e.to_error_msg(attr_name))),
            },
            LookupResult::InternalError(e) => {
                Some(self.error(range, e.to_error_msg(attr_name, todo_ctx)))
            }
            _ => None,
        }
    }

    // TODO(stroxler) Revisit the use case of checking `_value`; we probably should enforce that it
    // has a "raw" type (and complain if it is a descriptor of any kind) but the
    // API for that doesn't exist yet and it's a non-urgent edge case.
    //
    /// Compute the get (i.e. read) type of an attribute if it is gettable. If it is not found or
    /// access is denied, return `None`; never produce a type error. Used for internal special cases
    /// like checking enum values.
    pub fn type_of_attr_get_if_gettable(&self, base: Type, attr_name: &Name) -> Option<Type> {
        match self.lookup_attr(base, attr_name) {
            LookupResult::Found(attr) => attr.get_type(),
            _ => None,
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
                LookupResult::found_type(
                    self.get_instance_attribute(&builtins_type_classtype, attr_name)
                        .and_then(|attr| attr.get_type())
                        .map_or_else(|| style.propagate(), |ty| ty),
                )
            }
            Some(AttributeBase::Any(style)) => LookupResult::found_type(style.propagate()),
            Some(AttributeBase::Never) => LookupResult::found_type(Type::never()),
            None => LookupResult::InternalError(InternalError::AttributeBaseUndefined(base)),
        }
    }

    fn get_module_attr(&self, module: &Module, attr_name: &Name) -> Option<Type> {
        match module.as_single_module() {
            Some(module_name) => self.get_import(attr_name, module_name),
            None => {
                // TODO: This is fallable, but we don't detect it yet.
                Some(module.push_path(attr_name.clone()).to_type())
            }
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
            Type::Tuple(Tuple::Concrete(elements)) => Some(AttributeBase::ClassInstance(
                stdlib.tuple(self.unions(&elements)),
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
            Type::Callable(_, _) => Some(AttributeBase::ClassInstance(stdlib.function_type())),
            Type::BoundMethod(_, _) => Some(AttributeBase::ClassInstance(stdlib.method_type())),
            Type::Ellipsis => Some(AttributeBase::ClassInstance(stdlib.ellipsis_type())),
            Type::Forall(_, box base) => self.as_attribute_base(base, stdlib),
            Type::Var(v) => self.as_attribute_base(self.solver().force_var(v), stdlib),
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
