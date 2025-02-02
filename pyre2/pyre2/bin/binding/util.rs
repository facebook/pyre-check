/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Various utilities for examining pieces of syntax.

use std::sync::LazyLock;

use regex::Regex;

static IDENTIFIER_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap());

pub fn is_valid_identifier(name: &str) -> bool {
    IDENTIFIER_REGEX.is_match(name)
}
