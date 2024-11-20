/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::tuple::Tuple;
use crate::types::types::AnyStyle;
use crate::types::types::Type;
use crate::types::types::TypeAlias;

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

// Return `Some` when `ty` can be treated as a type for attrbute access purpose, and `None` otherwise
fn as_attribute_base_type(ty: Type, stdlib: &Stdlib) -> Option<Type> {
    match ty {
        Type::ClassType(_) | Type::Any(_) => Some(ty),
        Type::Tuple(Tuple::Unbounded(box element)) => Some(stdlib.tuple(element)),
        Type::Tuple(Tuple::Concrete(elements)) => Some(stdlib.tuple(unions(elements))),
        Type::LiteralString => Some(stdlib.str()),
        Type::Literal(lit) => Some(lit.general_type(stdlib)),
        Type::TypeGuard(_) | Type::TypeIs(_) => Some(stdlib.bool()),
        Type::TypeAlias(TypeAlias { box ty, .. }) => as_attribute_base_type(ty, stdlib),
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
        | Type::ClassDef(_)
        | Type::Var(_)
        | Type::Module(_)
        | Type::ParamSpec(_)
        | Type::TypeVar(_)
        | Type::Kwargs(_)
        | Type::Args(_)
        | Type::TypeVarTuple(_) => None,
    }
}

pub enum ClassAttributeBase {
    ClassType(ClassType),
    Any(AnyStyle),
}

pub fn as_class_attribute_base(ty: Type, stdlib: &Stdlib) -> Option<ClassAttributeBase> {
    match as_attribute_base_type(ty, stdlib) {
        Some(Type::ClassType(class_type)) => Some(ClassAttributeBase::ClassType(class_type)),
        Some(Type::Any(style)) => Some(ClassAttributeBase::Any(style)),
        _ => None,
    }
}
