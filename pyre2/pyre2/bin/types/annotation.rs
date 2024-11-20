/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Implementation of the `Annotation` type.
//! See <https://typing.readthedocs.io/en/latest/spec/annotations.html#type-and-annotation-expressions>

use std::fmt;
use std::fmt::Display;

use parse_display::Display;

use crate::types::types::AnyStyle;
use crate::types::types::Type;
use crate::util::display::intersperse_iter;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Annotation {
    pub qualifiers: Vec<Qualifier>,
    pub ty: Option<Type>,
}

impl Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.qualifiers.is_empty() {
            match &self.ty {
                Some(ty) => write!(f, "{ty}"),
                None => write!(f, "_"),
            }
        } else {
            write!(f, "{}", intersperse_iter(|| self.qualifiers.iter(), "["))?;
            if let Some(ty) = &self.ty {
                write!(f, "[{ty}]")?;
            }
            write!(f, "{}", "]".repeat(self.qualifiers.len() - 1))
        }
    }
}

impl Annotation {
    pub fn new_type(ty: Type) -> Self {
        Self {
            qualifiers: Vec::new(),
            ty: Some(ty),
        }
    }

    pub fn get_type(&self) -> &Type {
        self.ty.as_ref().unwrap_or(&Type::Any(AnyStyle::Implicit))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Display)]
pub enum Qualifier {
    Required,
    NotRequired,
    ReadOnly,
    ClassVar,
    Final,
    #[expect(dead_code)] // Will be used in the future
    InitVar,
    Annotated,
    TypeAlias,
    #[expect(dead_code)] // Will be used in the future
    Unpack,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        assert_eq!(
            Annotation {
                qualifiers: Vec::new(),
                ty: Some(Type::None)
            }
            .to_string(),
            "None"
        );
        assert_eq!(
            Annotation {
                qualifiers: vec![Qualifier::Required, Qualifier::ReadOnly],
                ty: None
            }
            .to_string(),
            "Required[ReadOnly]"
        );
        assert_eq!(
            Annotation {
                qualifiers: vec![Qualifier::Required, Qualifier::ReadOnly],
                ty: Some(Type::LiteralString),
            }
            .to_string(),
            "Required[ReadOnly[LiteralString]]"
        );
    }
}
