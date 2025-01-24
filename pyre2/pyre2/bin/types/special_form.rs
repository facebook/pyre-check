/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Things defined as `Foo: _SpecialForm` which have a builtin meaning.

use std::str::FromStr;

use dupe::Dupe;
use parse_display::Display;
use parse_display::FromStr;
use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;

use crate::types::annotation::Qualifier;
use crate::types::types::NeverStyle;
use crate::types::types::Type;

#[derive(
    Debug, Clone, Copy, Dupe, PartialEq, Eq, Hash, PartialOrd, Ord, Display, FromStr
)]
pub enum SpecialForm {
    Annotated,
    Callable,
    ClassVar,
    Concatenate,
    Final,
    Generic,
    Literal,
    LiteralString,
    Never,
    NoReturn,
    NotRequired,
    Optional,
    Protocol,
    ReadOnly,
    Required,
    #[display("Self")]
    SelfType,
    Tuple,
    Type,
    TypeAlias,
    TypeGuard,
    TypeIs,
    TypedDict,
    Union,
    Unpack,
}

impl SpecialForm {
    pub fn new(name: &Name, annotation: &Expr) -> Option<Self> {
        if !matches!(annotation, Expr::Name(x) if x.id == "_SpecialForm") {
            return None;
        }
        SpecialForm::from_str(name.as_str()).ok()
    }

    pub fn to_type(self) -> Type {
        match self {
            SpecialForm::LiteralString => Type::type_form(Type::LiteralString),
            SpecialForm::Never => Type::type_form(Type::Never(NeverStyle::Never)),
            SpecialForm::NoReturn => Type::type_form(Type::Never(NeverStyle::NoReturn)),
            _ => Type::type_form(Type::SpecialForm(self)),
        }
    }

    pub fn to_qualifier(self) -> Option<Qualifier> {
        match self {
            Self::Annotated => Some(Qualifier::Annotated),
            Self::ClassVar => Some(Qualifier::ClassVar),
            Self::Final => Some(Qualifier::Final),
            Self::NotRequired => Some(Qualifier::NotRequired),
            Self::ReadOnly => Some(Qualifier::ReadOnly),
            Self::Required => Some(Qualifier::Required),
            Self::TypeAlias => Some(Qualifier::TypeAlias),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use super::*;

    #[test]
    fn test_special_form_display() {
        assert_eq!(SpecialForm::Annotated.to_string(), "Annotated");
        assert_eq!(SpecialForm::Callable.to_string(), "Callable");
        assert_eq!(SpecialForm::SelfType.to_string(), "Self");
    }

    #[test]
    fn test_special_form_from_str() {
        assert_eq!(
            SpecialForm::from_str("Annotated").unwrap(),
            SpecialForm::Annotated
        );
        assert_eq!(
            SpecialForm::from_str("Callable").unwrap(),
            SpecialForm::Callable
        );
        assert_eq!(
            SpecialForm::from_str("Self").unwrap(),
            SpecialForm::SelfType
        );
        assert!(SpecialForm::from_str("NotASpecial").is_err());
    }
}
