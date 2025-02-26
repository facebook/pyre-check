/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;

use crate::error::kind::ErrorKind;
use crate::module::module_info::SourceRange;
use crate::module::module_path::ModulePath;
#[derive(Debug, Clone, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub struct Error {
    path: ModulePath,
    range: SourceRange,
    error_kind: ErrorKind,
    msg: String,
    is_ignored: bool,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}: {}", self.path, self.range, self.msg)
    }
}

impl Error {
    pub fn new(
        path: ModulePath,
        range: SourceRange,
        msg: String,
        is_ignored: bool,
        error_kind: ErrorKind,
    ) -> Self {
        Self {
            path,
            range,
            error_kind,
            msg,
            is_ignored,
        }
    }

    pub fn source_range(&self) -> &SourceRange {
        &self.range
    }

    pub fn path(&self) -> &ModulePath {
        &self.path
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn is_ignored(&self) -> bool {
        self.is_ignored
    }
}
