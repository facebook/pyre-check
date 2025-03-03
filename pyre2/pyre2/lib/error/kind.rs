/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use convert_case::Case;
use convert_case::Casing;
use dupe::Dupe;
use parse_display::Display;

#[derive(
    Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash, Copy, Dupe, Display
)]
pub enum ErrorKind {
    InvalidAnnotation,
    /// Assigning a value of the wrong type to a variable.
    BadAssignment,
    /// Attempting to access an attribute that does not exist.
    MissingAttribute,
    /// Internal Pyre error.
    InternalError,
    /// Passing an argument that is invalid for reasons besides type.
    InvalidArgument,
    /// An error caused by incorrect inheritance in a class or type definition.
    /// e.g. a metaclass that is not a subclass of `type`.
    InvalidInheritance,
    /// Accessing an attribute that does not exist on a module.
    MissingModuleAttribute,
    /// The attribute exists but does not support this access pattern.
    NoAccess,
    /// An error related to parsing or syntax.
    ParseError,
    /// The attribute exists but cannot be modified.
    ReadOnly,
    /// Attempting to use a feature that is not yet supported.
    #[allow(dead_code)]
    Unsupported,
    /// Unknown or not-yet-defined error.
    Unknown,
}

impl ErrorKind {
    pub fn to_name(self) -> String {
        self.to_string().to_case(Case::Kebab)
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_error_kind_name() {
        assert_eq!(&ErrorKind::Unknown.to_name(), "unknown");
        assert_eq!(&ErrorKind::ParseError.to_name(), "parse-error");
    }
}
