/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_python_ast::name::Name;
use ruff_python_ast::CmpOp;
use ruff_python_ast::Operator;

pub const AENTER: Name = Name::new_static("__aenter__");
pub const AEXIT: Name = Name::new_static("__aexit__");
pub const ALL: Name = Name::new_static("__all__");
pub const CALL: Name = Name::new_static("__call__");
pub const CONTAINS: Name = Name::new_static("__contains__");
pub const ENTER: Name = Name::new_static("__enter__");
pub const EXIT: Name = Name::new_static("__exit__");
pub const FILE: Name = Name::new_static("__file__");
pub const GETITEM: Name = Name::new_static("__getitem__");
pub const INIT: Name = Name::new_static("__init__");
pub const INVERT: Name = Name::new_static("__invert__");
pub const ITER: Name = Name::new_static("__iter__");
pub const MATCH_ARGS: Name = Name::new_static("__match_args__");
pub const NAME: Name = Name::new_static("__name__");
pub const NEG: Name = Name::new_static("__neg__");
pub const NEW: Name = Name::new_static("__new__");
pub const NEXT: Name = Name::new_static("__next__");
pub const POS: Name = Name::new_static("__pos__");
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

/// Returns the associated dunder if `op` corresponds to a "rich comparison method":
/// https://docs.python.org/3/reference/datamodel.html#object.__lt__.
pub fn rich_comparison_dunder(op: CmpOp) -> Option<Name> {
    let name = match op {
        CmpOp::Lt => "__lt__",
        CmpOp::LtE => "__le__",
        CmpOp::Eq => "__eq__",
        CmpOp::NotEq => "__ne__",
        CmpOp::Gt => "__gt__",
        CmpOp::GtE => "__ge__",
        _ => return None,
    };
    Some(Name::new_static(name))
}
