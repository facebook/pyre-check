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
use static_assertions::assert_eq_size;

use crate::alt::types::class_metadata::EnumMetadata;
use crate::ast::Ast;
use crate::error::collector::ErrorCollector;
use crate::module::module_info::ModuleInfo;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;

assert_eq_size!(Lit, [usize; 3]);

/// A literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lit {
    String(Box<str>),
    Int(i64),
    Bool(bool),
    Bytes(Box<[u8]>),
    Enum(Box<(ClassType, Name)>),
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
            Lit::Enum(box (enumeration, member)) => {
                let name = &enumeration.name();
                write!(f, "{name}.{member}")
            }
        }
    }
}

impl Lit {
    pub fn from_expr(
        x: &Expr,
        module_info: &ModuleInfo,
        get_enum_from_name: &dyn Fn(Identifier) -> Option<EnumMetadata>,
        errors: &ErrorCollector,
    ) -> Type {
        let int = |i| match i {
            Some(i) => Lit::Int(i).to_type(),
            None => {
                errors.add(
                    module_info,
                    x.range(),
                    "Int literal exceeds range, expected to fit within 64 bits".to_owned(),
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
            }) => match get_enum_from_name(Ast::expr_name_identifier(maybe_enum_name.clone())) {
                Some(e) => match e.get_member(&member_name.id) {
                    Some(lit) => lit.to_type(),
                    None => {
                        errors.add(
                            module_info,
                            *range,
                            format!(
                                "Enumeration `{}` does not have member `{}`",
                                maybe_enum_name.id, member_name.id
                            ),
                        );
                        Type::any_error()
                    }
                },
                None => {
                    errors.add(
                        module_info,
                        *range,
                        format!(
                            "Literal expression `{}` is not an enumeration",
                            maybe_enum_name.id
                        ),
                    );
                    Type::any_error()
                }
            },
            _ => {
                errors.add(
                    module_info,
                    x.range(),
                    "Invalid literal expression".to_owned(),
                );
                Type::any_error()
            }
        }
    }

    pub fn negate(
        &self,
        stdlib: &Stdlib,
        module_info: &ModuleInfo,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match self {
            Lit::Int(x) => match x.checked_neg() {
                Some(x) => Lit::Int(x).to_type(),
                None => stdlib.int().to_type(), // Loss of precision
            },
            _ => {
                errors.add(module_info, range, format!("Cannot negate type {self}"));
                Type::any_error()
            }
        }
    }

    pub fn invert(
        &self,
        module_info: &ModuleInfo,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        match self {
            Lit::Int(x) => {
                let x = !x;
                Lit::Int(x).to_type()
            }
            _ => {
                errors.add(module_info, range, format!("Cannot invert type {self}"));
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
            Lit::Enum(box (class_type, _)) => class_type.clone(),
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Lit::String(_))
    }
}
