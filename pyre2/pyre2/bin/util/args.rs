/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::env::args_os;
use std::ffi::OsString;

use anyhow::Context as _;
use argfile::Argument;

/// Do `@` file expansion
pub fn get_args_expanded() -> anyhow::Result<Vec<OsString>> {
    // Most programs drop empty lines, so we do too.
    fn parse_file_skipping_blanks(content: &str, prefix: char) -> Vec<Argument> {
        let mut res = argfile::parse_fromfile(content, prefix);
        res.retain(|x| match x {
            Argument::PassThrough(arg) => !arg.is_empty(),
            _ => true,
        });
        res
    }

    argfile::expand_args_from(args_os(), parse_file_skipping_blanks, argfile::PREFIX)
        .context("When parsing @arg files")
}
