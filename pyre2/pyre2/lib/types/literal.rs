/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::char;
use std::fmt;
use std::fmt::Display;

use pyrefly_derive::TypeEq;
use pyrefly_derive::Visit;
use pyrefly_derive::VisitMut;
use ruff_python_ast::name::Name;
use ruff_python_ast::ExprBooleanLiteral;
use ruff_python_ast::ExprBytesLiteral;
use ruff_python_ast::ExprFString;
use ruff_python_ast::ExprStringLiteral;
use ruff_python_ast::FStringElement;
use ruff_python_ast::FStringPart;
use ruff_python_ast::Int;

use crate::assert_words;
use crate::types::class::ClassType;
use crate::types::lit_int::LitInt;
use crate::types::stdlib::Stdlib;
use crate::types::types::Type;

assert_words!(Lit, 3);

/// A literal value.
#[derive(
    Debug, Clone, Visit, VisitMut, TypeEq, PartialEq, Eq, PartialOrd, Ord, Hash
)]
pub enum Lit {
    String(Box<str>),
    Int(LitInt),
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
    /// Returns the negated type, or None if literal can't be negated.
    pub fn negate(&self) -> Option<Type> {
        match self {
            Lit::Int(x) => Some(Lit::Int(x.negate()).to_type()),
            _ => None,
        }
    }

    /// Returns `+self` if the `+` operation is allowed, None otherwise.
    pub fn positive(&self) -> Option<Type> {
        match self {
            Lit::Int(_) => Some(self.clone().to_type()),
            Lit::Bool(true) => Some(Lit::Int(LitInt::new(1)).to_type()),
            Lit::Bool(false) => Some(Lit::Int(LitInt::new(0)).to_type()),
            _ => None,
        }
    }

    /// Returns the inverted type, or None if literal can't be inverted.
    pub fn invert(&self) -> Option<Type> {
        match self {
            Lit::Int(x) => {
                let x = x.invert();
                Some(Lit::Int(x).to_type())
            }
            _ => None,
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

    pub fn from_int(x: &Int) -> Self {
        Lit::Int(LitInt::from_ast(x))
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
