/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;

use crate::module::module_name::ModuleName;
use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::class::TArgs;
use crate::types::types::Type;

#[derive(Debug, Clone)]
pub struct Stdlib {
    str: Option<Class>,
    bool: Option<Class>,
    int: Option<Class>,
    bytes: Option<Class>,
    float: Option<Class>,
    complex: Option<Class>,
    slice: Option<Class>,
    base_exception: Option<Class>,
    base_exception_group: Option<Class>,
    exception_group: Option<Class>,
    list: Option<Class>,
    dict: Option<Class>,
    set: Option<Class>,
    tuple: Option<Class>,
    iterable: Option<Class>,
    generator: Option<Class>,
    awaitable: Option<Class>,
    coroutine: Option<Class>,
    type_var: Option<Class>,
    param_spec: Option<Class>,
    param_spec_args: Option<Class>,
    param_spec_kwargs: Option<Class>,
    type_var_tuple: Option<Class>,
    type_alias_type: Option<Class>,
    traceback_type: Option<Class>,
    builtins_type: Option<Class>,
    ellipsis_type: Option<Class>,
    none_type: Option<Class>,
    function_type: Option<Class>,
    method_type: Option<Class>,
    // We want an owned ClassType for object because it allows the MRO code to clone less frequently.
    object_class_type: Option<ClassType>,
}

impl Stdlib {
    pub fn new(mut lookup_class: impl FnMut(ModuleName, &Name) -> Option<Class>) -> Self {
        let builtins = ModuleName::builtins();
        let types = ModuleName::types();
        let typing = ModuleName::typing();

        let mut lookup_str =
            |module: ModuleName, name: &'static str| lookup_class(module, &Name::new_static(name));

        Self {
            str: lookup_str(builtins, "str"),
            bool: lookup_str(builtins, "bool"),
            int: lookup_str(builtins, "int"),
            bytes: lookup_str(builtins, "bytes"),
            float: lookup_str(builtins, "float"),
            complex: lookup_str(builtins, "complex"),
            slice: lookup_str(builtins, "slice"),
            base_exception: lookup_str(builtins, "BaseException"),
            base_exception_group: lookup_str(builtins, "BaseExceptionGroup"),
            exception_group: lookup_str(builtins, "ExceptionGroup"),
            list: lookup_str(builtins, "list"),
            dict: lookup_str(builtins, "dict"),
            set: lookup_str(builtins, "set"),
            tuple: lookup_str(builtins, "tuple"),
            builtins_type: lookup_str(builtins, "type"),
            ellipsis_type: lookup_str(types, "EllipsisType"),
            none_type: lookup_str(types, "NoneType"),
            iterable: lookup_str(typing, "Iterable"),
            generator: lookup_str(typing, "Generator"),
            awaitable: lookup_str(typing, "Awaitable"),
            coroutine: lookup_str(typing, "Coroutine"),
            type_var: lookup_str(typing, "TypeVar"),
            param_spec: lookup_str(typing, "ParamSpec"),
            param_spec_args: lookup_str(typing, "ParamSpecArgs"),
            param_spec_kwargs: lookup_str(typing, "ParamSpecKwargs"),
            type_var_tuple: lookup_str(typing, "TypeVarTuple"),
            type_alias_type: lookup_str(typing, "TypeAliasType"),
            traceback_type: lookup_str(types, "TracebackType"),
            function_type: lookup_str(types, "FunctionType"),
            method_type: lookup_str(types, "MethodType"),

            object_class_type: lookup_str(builtins, "object")
                .map(|obj| ClassType::new_for_stdlib(obj, TArgs::default())),
        }
    }

    /// Create a new Stdlib with all types set to `Any``.
    ///
    /// This is needed because bootstrapping a `Stdlib` requires an `AnswersSolver` for the
    /// `lookup`, but `AnswersSolver` itself depends on `Stdlib`.
    ///
    /// It works because the lookups only need a tiny subset of all `AnswersSolver` functionality,
    /// none of which actually depends on `Stdlib`.
    pub fn for_bootstrapping() -> Stdlib {
        Self::new(|_, _| None)
    }

    fn unwrap<T>(x: &Option<T>) -> &T {
        if let Some(x) = x {
            x
        } else {
            unreachable!(
                "Stdlib::primitive called using an stdlib with missing classes (probably a bootstrapping stdlib)"
            )
        }
    }

    fn primitive(cls: &Option<Class>) -> ClassType {
        // Note: this construction will panic if we incorrectly mark a generic type as primitive.
        ClassType::new_for_stdlib(Self::unwrap(cls).clone(), TArgs::default())
    }

    pub fn object_class_type(&self) -> &ClassType {
        Self::unwrap(&self.object_class_type)
    }

    pub fn bool(&self) -> ClassType {
        Self::primitive(&self.bool)
    }

    pub fn builtins_type(&self) -> ClassType {
        Self::primitive(&self.builtins_type)
    }

    pub fn ellipsis_type(&self) -> ClassType {
        Self::primitive(&self.ellipsis_type)
    }

    pub fn none_type(&self) -> ClassType {
        Self::primitive(&self.none_type)
    }

    pub fn int(&self) -> ClassType {
        Self::primitive(&self.int)
    }

    pub fn float(&self) -> ClassType {
        Self::primitive(&self.float)
    }

    pub fn complex(&self) -> ClassType {
        Self::primitive(&self.complex)
    }

    pub fn bytes(&self) -> ClassType {
        Self::primitive(&self.bytes)
    }

    pub fn str(&self) -> ClassType {
        Self::primitive(&self.str)
    }

    pub fn slice(&self, start_ty: Type, stop_ty: Type, step_ty: Type) -> ClassType {
        Self::apply(&self.slice, vec![start_ty, stop_ty, step_ty])
    }

    pub fn base_exception(&self) -> ClassType {
        Self::primitive(&self.base_exception)
    }

    fn apply(cls: &Option<Class>, targs: Vec<Type>) -> ClassType {
        // Note: this construction will panic if we use `apply` with the wrong arity.
        ClassType::new_for_stdlib(Self::unwrap(cls).clone(), TArgs::new(targs))
    }

    pub fn base_exception_group(&self, x: Type) -> ClassType {
        Self::apply(&self.base_exception_group, vec![x])
    }

    pub fn exception_group(&self, x: Type) -> ClassType {
        Self::apply(&self.exception_group, vec![x])
    }

    pub fn tuple(&self, x: Type) -> ClassType {
        Self::apply(&self.tuple, vec![x])
    }

    pub fn list(&self, x: Type) -> ClassType {
        Self::apply(&self.list, vec![x])
    }

    pub fn dict(&self, key: Type, value: Type) -> ClassType {
        Self::apply(&self.dict, vec![key, value])
    }

    pub fn set(&self, x: Type) -> ClassType {
        Self::apply(&self.set, vec![x])
    }

    pub fn iterable(&self, x: Type) -> ClassType {
        Self::apply(&self.iterable, vec![x])
    }

    pub fn generator(&self, yield_ty: Type, send_ty: Type, return_ty: Type) -> ClassType {
        Self::apply(&self.generator, vec![yield_ty, send_ty, return_ty])
    }

    pub fn awaitable(&self, x: Type) -> ClassType {
        Self::apply(&self.awaitable, vec![x])
    }

    pub fn coroutine(&self, yield_ty: Type, send_ty: Type, return_ty: Type) -> ClassType {
        Self::apply(&self.coroutine, vec![yield_ty, send_ty, return_ty])
    }

    pub fn type_var(&self) -> ClassType {
        Self::primitive(&self.type_var)
    }

    pub fn param_spec(&self) -> ClassType {
        Self::primitive(&self.param_spec)
    }

    pub fn type_var_tuple(&self) -> ClassType {
        Self::primitive(&self.type_var_tuple)
    }

    pub fn param_spec_args(&self) -> ClassType {
        Self::primitive(&self.param_spec_args)
    }

    pub fn param_spec_kwargs(&self) -> ClassType {
        Self::primitive(&self.param_spec_kwargs)
    }

    pub fn type_alias_type(&self) -> ClassType {
        Self::primitive(&self.type_alias_type)
    }

    pub fn traceback_type(&self) -> ClassType {
        Self::primitive(&self.traceback_type)
    }

    pub fn function_type(&self) -> ClassType {
        Self::primitive(&self.function_type)
    }

    pub fn method_type(&self) -> ClassType {
        Self::primitive(&self.method_type)
    }
}
