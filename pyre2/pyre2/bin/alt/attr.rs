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

pub enum AttributeBase {
    ClassInstance(ClassType),
    ClassObject(Class),
    Module(Module),
    Quantified(Quantified),
    Any(AnyStyle),
    /// type[Any] is a special case where attribute lookups first check the
    /// builtin `type` class before falling back to `Any`.
    TypeAny(AnyStyle),
}

pub enum LookupError {
    AttributeNotFound(ClassType),
    ClassAttributeNotFound(Class, NoClassAttribute),
    ModuleExportNotFound(Module),
    AttributeBaseUndefined(Type),
}

impl LookupError {
    pub fn to_error_msg(self, attr_name: &Name, todo_ctx: &str) -> String {
        match self {
            LookupError::AttributeNotFound(class_type) => {
                let class_name = class_type.name();
                format!("Object of class `{class_name}` has no attribute `{attr_name}`",)
            }
            LookupError::ClassAttributeNotFound(class, no_class_attribute) => {
                let class_name = class.name();
                match no_class_attribute {
                    NoClassAttribute::NoClassMember => {
                        format!("Class `{class_name}` has no class attribute `{attr_name}`",)
                    }
                    NoClassAttribute::IsGenericMember => format!(
                        "Generic attribute `{attr_name}` of class `{class_name}` is not visible on the class",
                    ),
                }
            }
            LookupError::ModuleExportNotFound(module) => {
                format!("No attribute `{attr_name}` in module `{module}`")
            }
            LookupError::AttributeBaseUndefined(ty) => format!(
                "TODO: {todo_ctx} attribute base undefined for type: {}",
                ty.deterministic_printing()
            ),
        }
    }
}

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn lookup_attr(&self, ty: Type, attr_name: &Name) -> Result<Type, LookupError> {
        match self.as_attribute_base(ty.clone(), self.stdlib) {
            Some(AttributeBase::ClassInstance(class)) => self
                .get_instance_attribute(&class, attr_name)
                .ok_or_else(|| LookupError::AttributeNotFound(class)),
            Some(AttributeBase::ClassObject(class)) => self
                .get_class_attribute(&class, attr_name)
                .map_err(|err| LookupError::ClassAttributeNotFound(class, err)),
            Some(AttributeBase::Module(module)) => self
                .get_module_attr(&module, attr_name)
                .ok_or_else(|| LookupError::ModuleExportNotFound(module)),
            Some(AttributeBase::Quantified(q)) => {
                if q.is_param_spec() && attr_name == "args" {
                    Ok(Type::type_form(Type::Args(q.id())))
                } else if q.is_param_spec() && attr_name == "kwargs" {
                    Ok(Type::type_form(Type::Kwargs(q.id())))
                } else {
                    let class = q.as_value(self.stdlib);
                    self.get_instance_attribute(&class, attr_name)
                        .ok_or_else(|| LookupError::AttributeNotFound(class))
                }
            }
            Some(AttributeBase::TypeAny(style)) => {
                let class = self.stdlib.builtins_type();
                Ok(self
                    .get_instance_attribute(&class, attr_name)
                    .unwrap_or_else(|| style.propagate()))
            }
            Some(AttributeBase::Any(style)) => Ok(style.propagate()),
            None => Err(LookupError::AttributeBaseUndefined(ty)),
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
            // TODO: check to see which ones should have class representations
            Type::Union(_)
            | Type::Never(_)
            | Type::Callable(_)
            | Type::Ellipsis
            | Type::SpecialForm(_)
            | Type::Type(_)
            | Type::Intersect(_)
            | Type::Forall(_, _)
            | Type::Unpack(_)
            | Type::Quantified(_)
            | Type::Var(_) => None,
        }
    }
}
