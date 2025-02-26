/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::ffi::OsString;

use anyhow::Context as _;
use argfile::Argument;

pub static ENV_VAR_OVERRIDE_PREFIX: &str = "PYRE_";

/// Do `@` file expansion
pub fn get_args_expanded(args: impl Iterator<Item = OsString>) -> anyhow::Result<Vec<OsString>> {
    // Most programs drop empty lines, so we do too.
    fn parse_file_skipping_blanks(content: &str, prefix: char) -> Vec<Argument> {
        let mut res = argfile::parse_fromfile(content, prefix);
        res.retain(|x| match x {
            Argument::PassThrough(arg) => !arg.is_empty(),
            _ => true,
        });
        res
    }

    argfile::expand_args_from(args, parse_file_skipping_blanks, argfile::PREFIX)
        .context("When parsing @arg files")
}

pub fn clap_env(suffix: &str) -> String {
    ENV_VAR_OVERRIDE_PREFIX.to_owned() + suffix
}
