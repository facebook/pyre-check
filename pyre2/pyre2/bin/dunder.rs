/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::Operator;

pub const ALL: Name = Name::new_static("__all__");
pub const AENTER: Name = Name::new_static("__aenter__");
pub const AEXIT: Name = Name::new_static("__aexit__");
pub const ENTER: Name = Name::new_static("__enter__");
pub const EXIT: Name = Name::new_static("__exit__");
pub const GETITEM: Name = Name::new_static("__getitem__");
pub const INIT: Name = Name::new_static("__init__");
pub const ITER: Name = Name::new_static("__iter__");
pub const NEW: Name = Name::new_static("__new__");
pub const NEXT: Name = Name::new_static("__next__");
pub const SETITEM: Name = Name::new_static("__setitem__");

/// Given the operator, what is the name of the dunder methods that implements
/// inplace mutation. E.g. `a += b` translates to `a.__iadd__(b)`.
pub fn inplace_dunder(op: Operator) -> Name {
    Name::new_static(match op {
        Operator::Add => "__iadd__",
        Operator::Sub => "__isub__",
        Operator::Mult => "__imul__",
        Operator::MatMult => "__imatmul__",
        Operator::Div => "__itruediv__",
        Operator::Mod => "__imod__",
        Operator::Pow => "__ipow__",
        Operator::LShift => "__ilshift__",
        Operator::RShift => "__irshift__",
        Operator::BitOr => "__ior__",
        Operator::BitXor => "__ixor__",
        Operator::BitAnd => "__iand__",
        Operator::FloorDiv => "__ifloordiv__",
    })
}
