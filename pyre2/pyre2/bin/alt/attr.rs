/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;
use ruff_python_ast::name::Name;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::alt::classes::NoClassAttribute;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::module::Module;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::types::AnyStyle;
use crate::types::types::Quantified;
use crate::types::types::Type;

/// A normalized type representation which is convenient for attribute lookup,
/// since many cases are collapsed. For example, Type::Literal is converted to
/// it's corresponding class type.
pub enum AttributeBase {
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

pub enum LookupResult {
    /// The lookup succeeded, resulting in a type.
    Found(Type),
    /// The attribute was not found. Callers can use fallback behavior, for
    /// example looking up a different attribute.
    NotFound(NotFound),
    /// The lookup failed for some other reason. Fallback behavior does not
    /// make sense and callers should just bail.
    Error(LookupError),
}

pub enum NotFound {
    Attribute(ClassType),
    ClassAttribute(Class),
    ModuleExport(Module),
}

pub enum LookupError {
    /// An internal error caused by `as_attribute_base` being partial.
    AttributeBaseUndefined(Type),
    /// A generic class attribute exists, but has an invalid definition.
    /// Callers should treat the attribute as `Any`.
    ClassAttributeIsGeneric(Class),
}

impl LookupResult {
    /// A convenience function for callers which do not need to distinguish
    /// between NotFound and Error results.
    pub fn ok_or_conflated_error_msg(
        self,
        attr_name: &Name,
        todo_ctx: &str,
    ) -> Result<Type, String> {
        match self {
            LookupResult::Found(ty) => Ok(ty),
            LookupResult::NotFound(err) => Err(err.to_error_msg(attr_name)),
            LookupResult::Error(err) => Err(err.to_error_msg(attr_name, todo_ctx)),
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

impl LookupError {
    pub fn to_error_msg(self, attr_name: &Name, todo_ctx: &str) -> String {
        match self {
            LookupError::AttributeBaseUndefined(ty) => format!(
                "TODO: {todo_ctx} attribute base undefined for type: {}",
                ty.deterministic_printing()
            ),
            LookupError::ClassAttributeIsGeneric(class) => {
                let class_name = class.name();
                format!(
                    "Generic attribute `{attr_name}` of class `{class_name}` is not visible on the class"
                )
            }
        }
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn lookup_attr(&self, ty: Type, attr_name: &Name) -> LookupResult {
        match self.as_attribute_base(ty.clone(), self.stdlib) {
            Some(AttributeBase::ClassInstance(class)) => {
                match self.get_instance_attribute(&class, attr_name) {
                    Some(attr) => LookupResult::Found(attr.value),
                    None => LookupResult::NotFound(NotFound::Attribute(class)),
                }
            }
            Some(AttributeBase::ClassObject(class)) => {
                match self.get_class_attribute(&class, attr_name) {
                    Ok(attr) => LookupResult::Found(attr.value),
                    Err(NoClassAttribute::NoClassMember) => {
                        LookupResult::NotFound(NotFound::ClassAttribute(class))
                    }
                    Err(NoClassAttribute::IsGenericMember) => {
                        LookupResult::Error(LookupError::ClassAttributeIsGeneric(class))
                    }
                }
            }
            Some(AttributeBase::Module(module)) => match self.get_module_attr(&module, attr_name) {
                Some(attr) => LookupResult::Found(attr),
                None => LookupResult::NotFound(NotFound::ModuleExport(module)),
            },
            Some(AttributeBase::Quantified(q)) => {
                if q.is_param_spec() && attr_name == "args" {
                    LookupResult::Found(Type::type_form(Type::Args(q.id())))
                } else if q.is_param_spec() && attr_name == "kwargs" {
                    LookupResult::Found(Type::type_form(Type::Kwargs(q.id())))
                } else {
                    let class = q.as_value(self.stdlib);
                    match self.get_instance_attribute(&class, attr_name) {
                        Some(attr) => LookupResult::Found(attr.value),
                        None => LookupResult::NotFound(NotFound::Attribute(class)),
                    }
                }
            }
            Some(AttributeBase::TypeAny(style)) => {
                let class = self.stdlib.builtins_type();
                LookupResult::Found(
                    self.get_instance_attribute(&class, attr_name)
                        .map_or_else(|| style.propagate(), |attr| attr.value),
                )
            }
            Some(AttributeBase::Any(style)) => LookupResult::Found(style.propagate()),
            Some(AttributeBase::Never) => LookupResult::Found(Type::never()),
            None => LookupResult::Error(LookupError::AttributeBaseUndefined(ty)),
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
            Type::ClassDef(cls) => Some(AttributeBase::ClassObject(cls)),
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
            Type::Callable(_) => Some(AttributeBase::ClassInstance(stdlib.function_type())),
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
            | Type::Quantified(_) => None,
        }
    }
}
