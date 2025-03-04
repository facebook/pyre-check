/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::char;
use std::fmt;
use std::fmt::Display;

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprBooleanLiteral;
use ruff_python_ast::ExprBytesLiteral;
use ruff_python_ast::ExprFString;
use ruff_python_ast::ExprStringLiteral;
use ruff_python_ast::ExprUnaryOp;
use ruff_python_ast::FStringElement;
use ruff_python_ast::FStringPart;
use ruff_python_ast::Identifier;
use ruff_python_ast::Int;
use ruff_python_ast::Number;
use ruff_python_ast::UnaryOp;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::ast::Ast;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;

#[cfg(target_pointer_width = "64")]
static_assertions::assert_eq_size!(Lit, [usize; 3]);

/// A literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lit {
    String(Box<str>),
    Int(i64),
    Bool(bool),
    Bytes(Box<[u8]>),
    /// (enum class, member name, raw type assigned to name in class def)
    /// We store the raw type so we can return it when the value or _value_ attribute is accessed.
    Enum(Box<(ClassType, Name, Type)>),
}

impl Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::String(x) => write!(f, "'{x}'"),
            Lit::Int(x) => write!(f, "{x}"),
            Lit::Bool(x) => {
                let s = if *x { "True" } else { "False" };
                write!(f, "{s}")
            }
            Lit::Bytes(xs) => {
                write!(f, "b'")?;
                for x in xs {
                    match char::from_u32(*x as u32) {
                        Some(x) => write!(f, "{x}")?,
                        None => write!(f, "\\x{:02x}", x)?,
                    }
                }
                write!(f, "'")
            }
            Lit::Enum(box (enumeration, member, _)) => {
                let name = &enumeration.name();
                write!(f, "{name}.{member}")
            }
        }
    }
}

impl Lit {
    pub fn from_expr(
        x: &Expr,
        get_enum_member: &dyn Fn(Identifier, &Name) -> Option<Lit>,
        errors: &ErrorCollector,
    ) -> Type {
        let int = |i| match i {
            Some(i) => Lit::Int(i).to_type(),
            None => {
                errors.add(
                    x.range(),
                    "Int literal exceeds range, expected to fit within 64 bits".to_owned(),
                    ErrorKind::Unknown,
                    None,
                );
                Type::any_error()
            }
        };

        match x {
            Expr::UnaryOp(ExprUnaryOp {
                op: UnaryOp::UAdd,
                operand: box Expr::NumberLiteral(n),
                ..
            }) if let Number::Int(i) = &n.value => int(i.as_i64()),
            Expr::UnaryOp(ExprUnaryOp {
                op: UnaryOp::USub,
                operand: box Expr::NumberLiteral(n),
                ..
            }) if let Number::Int(i) = &n.value => int(i.as_i64().and_then(|x| x.checked_neg())),
            Expr::NumberLiteral(n) if let Number::Int(i) = &n.value => int(i.as_i64()),
            Expr::StringLiteral(x) => Self::from_string_literal(x).to_type(),
            Expr::BytesLiteral(x) => Self::from_bytes_literal(x).to_type(),
            Expr::BooleanLiteral(x) => Self::from_boolean_literal(x).to_type(),
            Expr::Attribute(ExprAttribute {
                range,
                value: box Expr::Name(maybe_enum_name),
                attr: member_name,
                ctx: _,
            }) => match get_enum_member(
                Ast::expr_name_identifier(maybe_enum_name.clone()),
                &member_name.id,
            ) {
                Some(lit) => lit.to_type(),
                None => {
                    errors.add(
                        *range,
                        format!(
                            "`{}.{}` is not a valid enum member",
                            maybe_enum_name.id, member_name.id
                        ),
                        ErrorKind::Unknown,
                        None,
                    );
                    Type::any_error()
                }
            },
            Expr::NoneLiteral(_) => Type::None,
            _ => {
                errors.add(
                    x.range(),
                    "Invalid literal expression".to_owned(),
                    ErrorKind::Unknown,
                    None,
                );
                Type::any_error()
            }
        }
    }

    pub fn negate(&self, stdlib: &Stdlib, range: TextRange, errors: &ErrorCollector) -> Type {
        match self {
            Lit::Int(x) => match x.checked_neg() {
                Some(x) => Lit::Int(x).to_type(),
                None => stdlib.int().to_type(), // Loss of precision
            },
            _ => {
                errors.add(
                    range,
                    format!("Cannot negate type {self}"),
                    ErrorKind::Unknown,
                    None,
                );
                Type::any_error()
            }
        }
    }

    pub fn invert(&self, range: TextRange, errors: &ErrorCollector) -> Type {
        match self {
            Lit::Int(x) => {
                let x = !x;
                Lit::Int(x).to_type()
            }
            _ => {
                errors.add(
                    range,
                    format!("Cannot invert type {self}"),
                    ErrorKind::Unknown,
                    None,
                );
                Type::any_error()
            }
        }
    }

    pub fn from_string_literal(x: &ExprStringLiteral) -> Self {
        Lit::String(x.value.to_str().into())
    }

    pub fn from_bytes_literal(x: &ExprBytesLiteral) -> Self {
        Lit::Bytes(x.value.bytes().collect())
    }

    pub fn from_fstring(x: &ExprFString) -> Option<Self> {
        let mut collected_literals = Vec::new();
        for fstring_part in x.value.as_slice() {
            match fstring_part {
                FStringPart::Literal(x) => collected_literals.push(x.value.clone()),
                FStringPart::FString(x) => {
                    for fstring_part in x.elements.iter() {
                        match fstring_part {
                            FStringElement::Literal(x) => collected_literals.push(x.value.clone()),
                            _ => return None,
                        }
                    }
                }
            }
        }
        Some(Lit::String(collected_literals.join("").into_boxed_str()))
    }

    pub fn from_int(x: &Int) -> Option<Self> {
        Some(Lit::Int(x.as_i64()?))
    }

    pub fn from_boolean_literal(x: &ExprBooleanLiteral) -> Self {
        Lit::Bool(x.value)
    }

    /// Convert a literal to a `Type::Literal`.
    pub fn to_type(self) -> Type {
        Type::Literal(self)
    }

    /// Convert a literal to a `ClassType` that is the general class_type of the literal.
    /// For example, `1` is converted to `int`, and `"foo"` is converted to `str`.
    pub fn general_class_type(&self, stdlib: &Stdlib) -> ClassType {
        match self {
            Lit::String(_) => stdlib.str(),
            Lit::Int(_) => stdlib.int(),
            Lit::Bool(_) => stdlib.bool(),
            Lit::Bytes(_) => stdlib.bytes(),
            Lit::Enum(box (class_type, ..)) => class_type.clone(),
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Lit::String(_))
    }
}
