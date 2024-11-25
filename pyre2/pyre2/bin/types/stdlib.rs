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
    list: Option<Class>,
    dict: Option<Class>,
    set: Option<Class>,
    tuple: Option<Class>,
    iterable: Option<Class>,
    generator: Option<Class>,
    awaitable: Option<Class>,
    coroutine: Option<Class>,
    traceback_type: Option<Class>,
    // We want an owned ClassType for object because it allows the MRO code to clone less frequently.
    object_class_type: Option<ClassType>,
}

impl Stdlib {
    /// Those modules required to service the builtins.
    pub fn required() -> Vec<ModuleName> {
        vec![
            ModuleName::builtins(),
            ModuleName::typing(),
            ModuleName::types(),
        ]
    }

    pub fn new(mut lookup_class: impl FnMut(ModuleName, &Name) -> Option<Class>) -> Self {
        let object = lookup_class(ModuleName::builtins(), &Name::new("object"));
        Self {
            str: lookup_class(ModuleName::builtins(), &Name::new("str")),
            bool: lookup_class(ModuleName::builtins(), &Name::new("bool")),
            int: lookup_class(ModuleName::builtins(), &Name::new("int")),
            bytes: lookup_class(ModuleName::builtins(), &Name::new("bytes")),
            float: lookup_class(ModuleName::builtins(), &Name::new("float")),
            complex: lookup_class(ModuleName::builtins(), &Name::new("complex")),
            slice: lookup_class(ModuleName::builtins(), &Name::new("slice")),
            base_exception: lookup_class(ModuleName::builtins(), &Name::new("BaseException")),
            list: lookup_class(ModuleName::builtins(), &Name::new("list")),
            dict: lookup_class(ModuleName::builtins(), &Name::new("dict")),
            set: lookup_class(ModuleName::builtins(), &Name::new("set")),
            tuple: lookup_class(ModuleName::builtins(), &Name::new("tuple")),
            iterable: lookup_class(ModuleName::typing(), &Name::new("Iterable")),
            generator: lookup_class(ModuleName::typing(), &Name::new("Generator")),
            awaitable: lookup_class(ModuleName::typing(), &Name::new("Awaitable")),
            coroutine: lookup_class(ModuleName::typing(), &Name::new("Coroutine")),
            traceback_type: lookup_class(ModuleName::types(), &Name::new("TracebackType")),
            object_class_type: object
                .map(|obj| ClassType::create_with_validated_targs(obj, TArgs::default())),
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

    fn unwrap_class(cls: &Option<Class>) -> &Class {
        if let Some(cls) = cls {
            cls
        } else {
            unreachable!(
                "Stdlib::primitive_class_type called using an stdlib with missing classes (probably a bootstrapping stdlib)"
            )
        }
    }

    fn primitive_class_type(cls: &Option<Class>) -> ClassType {
        // NOTE: if we hardcode in invalid use of `primitive_class_type` here, we will panic later
        // when performing substitutions.
        ClassType::create_with_validated_targs(Self::unwrap_class(cls).clone(), TArgs::default())
    }

    fn primitive(cls: &Option<Class>) -> Type {
        Type::class_type(Self::primitive_class_type(cls))
    }

    pub fn object_class_type(&self) -> &ClassType {
        if let Some(o) = &self.object_class_type {
            o
        } else {
            unreachable!(
                "Stdlib::object_class_type called using an stdlib with missing classes (probably a bootstrapping stdlib)"
            )
        }
    }

    pub fn bool_class_type(&self) -> ClassType {
        Self::primitive_class_type(&self.bool)
    }

    pub fn bool(&self) -> Type {
        Type::class_type(self.bool_class_type())
    }

    pub fn int_class_type(&self) -> ClassType {
        Self::primitive_class_type(&self.int)
    }

    pub fn int(&self) -> Type {
        Type::class_type(self.int_class_type())
    }

    pub fn float_class_type(&self) -> ClassType {
        Self::primitive_class_type(&self.float)
    }

    pub fn complex_class_type(&self) -> ClassType {
        Self::primitive_class_type(&self.complex)
    }

    pub fn bytes_class_type(&self) -> ClassType {
        Self::primitive_class_type(&self.bytes)
    }

    pub fn str_class_type(&self) -> ClassType {
        Self::primitive_class_type(&self.str)
    }

    pub fn str(&self) -> Type {
        Type::class_type(self.str_class_type())
    }

    pub fn slice(&self) -> Type {
        Self::primitive(&self.slice)
    }

    pub fn base_exception(&self) -> Type {
        Self::primitive(&self.base_exception)
    }

    fn apply_class_type(cls: &Option<Class>, targs: Vec<Type>) -> ClassType {
        // NOTE: if we hardcode in invalid use of `apply_class_type` here, we will panic later
        // when performing substitutions.
        ClassType::create_with_validated_targs(Self::unwrap_class(cls).clone(), TArgs::new(targs))
    }

    fn apply(cls: &Option<Class>, targs: Vec<Type>) -> Type {
        Type::class_type(Self::apply_class_type(cls, targs))
    }

    pub fn tuple_class_type(&self, x: Type) -> ClassType {
        Self::apply_class_type(&self.tuple, vec![x])
    }

    pub fn tuple(&self, x: Type) -> Type {
        Type::class_type(self.tuple_class_type(x))
    }

    pub fn list(&self, x: Type) -> Type {
        Self::apply(&self.list, vec![x])
    }

    pub fn dict(&self, key: Type, value: Type) -> Type {
        Self::apply(&self.dict, vec![key, value])
    }

    pub fn set(&self, x: Type) -> Type {
        Self::apply(&self.set, vec![x])
    }

    pub fn iterable(&self, x: Type) -> Type {
        Self::apply(&self.iterable, vec![x])
    }

    pub fn generator(&self, yield_ty: Type, send_ty: Type, return_ty: Type) -> Type {
        Self::apply(&self.generator, vec![yield_ty, send_ty, return_ty])
    }

    pub fn awaitable(&self, x: Type) -> Type {
        Self::apply(&self.awaitable, vec![x])
    }

    pub fn coroutine(&self, yield_ty: Type, send_ty: Type, return_ty: Type) -> Type {
        Self::apply(&self.coroutine, vec![yield_ty, send_ty, return_ty])
    }

    pub fn traceback_type(&self) -> Type {
        Self::primitive(&self.traceback_type)
    }
}
