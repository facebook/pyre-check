// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.
#![feature(exit_status_error)]

use std::process::Command;
use std::process::ExitStatusError;

pub fn cmd(prog: &str, args: &[&str], dir: Option<&std::path::Path>) -> Command {
    let mut prog_cmd = Command::new(prog);
    if let Some(path) = dir {
        prog_cmd.current_dir(path);
    }
    prog_cmd.args(args);
    prog_cmd
}

pub fn workspace_dir(ds: &[&str]) -> std::path::PathBuf {
    let mut cargo_cmd = cmd(
        "cargo",
        &["locate-project", "--workspace", "--message-format=plain"],
        None,
    );
    let output = cargo_cmd.output().unwrap().stdout;
    let root_cargo_toml = std::path::Path::new(std::str::from_utf8(&output).unwrap().trim());
    let mut p = root_cargo_toml.parent().unwrap().to_path_buf();
    for d in ds {
        p.push(d);
    }
    p
}

pub fn run(mut cmd: Command) -> Result<(), ExitStatusError> {
    cmd.spawn().unwrap().wait().ok().unwrap().exit_ok()
}

pub fn fmt_exit_status_err(err: ExitStatusError) -> String {
    format!("error status: {err}")
}

pub fn build_flavor() -> &'static str {
    if cfg!(debug_assertions) {
        "debug"
    } else {
        "release"
    }
}
