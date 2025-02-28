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
    InvalidArgument,
    InvalidInheritance,
    ParseError,
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
