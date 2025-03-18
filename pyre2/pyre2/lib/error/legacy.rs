/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use serde::Serialize;

use crate::error::error::Error;
use crate::util::prelude::SliceExt;

/// Legacy error structure in Pyre1. Needs to be consistent with the following file:
/// <https://www.internalfb.com/code/fbsource/fbcode/tools/pyre/facebook/arc/lib/error.rs>
///
/// Used to serialize errors in a Pyre1-compatible format.
#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct LegacyError {
    line: usize,
    column: usize,
    stop_line: usize,
    stop_column: usize,
    path: String,
    code: i32,
    /// The kebab-case name of the error kind.
    name: &'static str,
    description: String,
    concise_description: String,
}

impl LegacyError {
    pub fn from_error(error: &Error) -> Self {
        let error_range = error.source_range();
        let msg = error.msg();
        Self {
            line: error_range.start.row.get(),
            column: error_range.start.column.get(),
            stop_line: error_range.end.row.get(),
            stop_column: error_range.end.column.get(),
            path: error.path().to_string(),
            // -2 is chosen because it's an unused error code in Pyre1
            code: -2, // TODO: replace this dummy value
            name: error.error_kind().to_name(),
            description: msg.to_owned(),
            concise_description: msg.to_owned(),
        }
    }
}

#[derive(Serialize, Debug, PartialEq, Eq)]
pub struct LegacyErrors {
    pub errors: Vec<LegacyError>,
}

impl LegacyErrors {
    pub fn from_errors(errors: &[Error]) -> Self {
        Self {
            errors: errors.map(LegacyError::from_error),
        }
    }
}
