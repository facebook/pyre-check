/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::char;
use std::fmt;
use std::fmt::Display;

use ordered_float::NotNan;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;
use ruff_python_ast::ExprAttribute;
use ruff_python_ast::ExprBooleanLiteral;
use ruff_python_ast::ExprBytesLiteral;
use ruff_python_ast::ExprFString;
use ruff_python_ast::ExprNumberLiteral;
use ruff_python_ast::ExprStringLiteral;
use ruff_python_ast::FStringElement;
use ruff_python_ast::FStringPart;
use ruff_python_ast::Identifier;
use ruff_python_ast::Number;
use ruff_python_ast::UnaryOp;
use ruff_text_size::TextRange;
use static_assertions::assert_eq_size;

use crate::ast::Ast;
use crate::error::collector::ErrorCollector;
use crate::module::module_info::ModuleInfo;
use crate::types::class::ClassType;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;

assert_eq_size!(Lit, [u8; 48]);

/// A literal value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lit {
    String(String),
    Int(i64),
    Float(NotNan<f64>),
    Complex {
        real: NotNan<f64>,
        imag: NotNan<f64>,
    },
    Bool(bool),
    Bytes(Vec<u8>),
    Enum(ClassType, Name),
}

impl Display for Lit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Lit::String(x) => write!(f, "'{x}'"),
            Lit::Int(x) => write!(f, "{x}"),
            Lit::Float(x) => {
                let mut s = x.to_string();
                if !s.contains('.') {
                    s.push_str(".0");
                }
                write!(f, "{s}")
            }
            Lit::Complex { real, imag } => write!(f, "{real}+{imag}j"),
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
            Lit::Enum(enumeration, member) => {
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
        get_enum_class_type: &dyn Fn(Identifier) -> Option<ClassType>,
        errors: &ErrorCollector,
    ) -> Self {
        match x {
            Expr::UnaryOp(x) => {
                match x.op {
                    UnaryOp::UAdd => {
                        Self::from_expr(&x.operand, module_info, get_enum_class_type, errors)
                    }
                    UnaryOp::USub => {
                        Self::from_expr(&x.operand, module_info, get_enum_class_type, errors)
                            .negate(module_info, x.range, errors)
                    }
                    _ => {
                        errors.todo(module_info, "Lit::from_expr", x);
                        Lit::Bool(false)
                    }
                }
            }
            Expr::StringLiteral(x) => Self::from_string_literal(x),
            Expr::BytesLiteral(x) => Self::from_bytes_literal(x),
            Expr::NumberLiteral(x) => Self::from_number_literal(x, module_info, errors),
            Expr::BooleanLiteral(x) => Self::from_boolean_literal(x),
            Expr::Attribute(ExprAttribute {
                range: _,
                value: box Expr::Name(maybe_enum_name),
                attr: member_name,
                ctx: _,
            }) => match get_enum_class_type(Ast::expr_name_identifier(maybe_enum_name.clone())) {
                Some(class_type) => Lit::Enum(class_type, member_name.id.to_owned()),
                _ => {
                    errors.todo(module_info, "Lit::from_expr", x);
                    Lit::Bool(false)
                }
            },
            _ => {
                errors.todo(module_info, "Lit::from_expr", x);
                Lit::Bool(false)
            }
        }
    }

    pub fn negate(
        &self,
        module_info: &ModuleInfo,
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Self {
        match self {
            Lit::Int(x) if let Some(x) = x.checked_neg() => Lit::Int(x),
            _ => {
                errors.add(module_info, range, format!("Cannot negate type {self}"));
                self.clone()
            }
        }
    }

    pub fn from_string_literal(x: &ExprStringLiteral) -> Self {
        Lit::String(x.value.to_str().to_owned())
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
        Some(Lit::String(collected_literals.join("")))
    }

    pub fn from_number_literal(
        x: &ExprNumberLiteral,
        module_info: &ModuleInfo,
        errors: &ErrorCollector,
    ) -> Self {
        match &x.value {
            Number::Int(x) if let Some(x) = x.as_i64() => Lit::Int(x),
            Number::Float(x) if let Ok(x) = NotNan::new(*x) => Lit::Float(x),
            Number::Complex { real, imag }
                if let (Ok(real), Ok(imag)) = (NotNan::new(*real), NotNan::new(*imag)) =>
            {
                Lit::Complex { imag, real }
            }
            _ => {
                errors.todo(module_info, "Lit::from_number_literal", x);
                Lit::Int(0)
            }
        }
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
            Lit::Float(_) => stdlib.float(),
            Lit::Complex { .. } => stdlib.complex(),
            Lit::Enum(class_type, _) => class_type.clone(),
        }
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Lit::String(_))
    }
}
