/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use dupe::Dupe;

use crate::types::class::Class;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::types::AnyStyle;
use crate::types::types::Type;

pub fn unions(xs: Vec<Type>) -> Type {
    if xs.is_empty() {
        return Type::never();
    }
    fn flatten(xs: Vec<Type>, res: &mut Vec<Type>) {
        for x in xs {
            match x {
                Type::Union(xs) => flatten(xs, res),
                _ => res.push(x),
            }
        }
    }
    let mut res = Vec::with_capacity(xs.len());
    flatten(xs, &mut res);

    res.sort();
    res.dedup();
    if res.len() == 1 {
        res.pop().unwrap()
    } else {
        Type::Union(res)
    }
}

pub enum AttributeBase {
    ClassInstance(ClassType),
    ClassObject(Class),
    Any(AnyStyle),
}

pub fn as_attribute_base(ty: Type, stdlib: &Stdlib) -> Option<AttributeBase> {
    match ty {
        Type::ClassType(class_type) => Some(AttributeBase::ClassInstance(class_type)),
        Type::Tuple(Tuple::Unbounded(box element)) => {
            Some(AttributeBase::ClassInstance(stdlib.tuple(element)))
        }
        Type::Tuple(Tuple::Concrete(elements)) => {
            Some(AttributeBase::ClassInstance(stdlib.tuple(unions(elements))))
        }
        Type::LiteralString => Some(AttributeBase::ClassInstance(stdlib.str())),
        Type::Literal(lit) => Some(AttributeBase::ClassInstance(lit.general_class_type(stdlib))),
        Type::TypeGuard(_) | Type::TypeIs(_) => Some(AttributeBase::ClassInstance(stdlib.bool())),
        Type::Any(style) => Some(AttributeBase::Any(style)),
        Type::TypeAlias(ta) => {
            if let Some(t) = ta.as_value() {
                as_attribute_base(t, stdlib)
            } else {
                None
            }
        }
        Type::ClassDef(cls) => Some(AttributeBase::ClassObject(cls)),
        Type::Type(box Type::ClassType(class)) => {
            Some(AttributeBase::ClassObject(class.class_object().dupe()))
        }
        // TODO: check to see which ones should have class representations
        Type::Union(_)
        | Type::Never(_)
        | Type::Callable(_)
        | Type::Ellipsis
        | Type::None
        | Type::SpecialForm(_)
        | Type::Type(_)
        | Type::Intersect(_)
        | Type::Forall(_, _)
        | Type::Unpack(_)
        | Type::Quantified(_)
        | Type::Var(_)
        | Type::Module(_)
        | Type::ParamSpec(_)
        | Type::TypeVar(_)
        | Type::Kwargs(_)
        | Type::Args(_)
        | Type::TypeVarTuple(_) => None,
    }
}
