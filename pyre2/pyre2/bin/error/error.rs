/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Debug;
use std::fmt::Display;
use std::path::Path;

use ruff_text_size::TextRange;

use crate::module::module_info::ModuleInfo;
use crate::module::module_info::SourceRange;

#[derive(Debug, Clone)]
pub struct Error {
    pub info: ModuleInfo,
    pub range: TextRange,
    pub msg: String,
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}: {}",
            self.info.path().display(),
            self.source_range(),
            self.msg
        )
    }
}

impl Error {
    pub fn new(info: ModuleInfo, range: TextRange, msg: String) -> Self {
        let res = Self { info, range, msg };
        // We calculate the source_range here, so that if it has out of bounds
        // (e.g. our range/info combo is wrong) we'll get a crash at the right spot.
        res.source_range();
        res
    }

    pub fn source_range(&self) -> SourceRange {
        self.info.source_range(self.range)
    }

    pub fn path(&self) -> &Path {
        self.info.path()
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn is_ignored(&self) -> bool {
        self.info.is_ignored(&self.source_range(), &self.msg)
    }
}
