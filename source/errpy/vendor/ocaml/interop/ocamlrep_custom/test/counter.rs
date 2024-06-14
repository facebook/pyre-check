// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

#![feature(exit_status_error)]

use std::cell::Cell;

use ocamlrep_custom::caml_serialize_default_impls;
use ocamlrep_custom::CamlSerialize;
use ocamlrep_custom::Custom;
use ocamlrep_ocamlpool::ocaml_ffi;

pub struct Counter(Cell<isize>);

impl CamlSerialize for Counter {
    caml_serialize_default_impls!();
}

ocaml_ffi! {
    fn counter_new() -> Custom<Counter> {
        Custom::from(Counter(Cell::new(0)))
    }

    fn counter_inc(counter: Custom<Counter>) -> Custom<Counter> {
        counter.0.set(counter.0.get() + 1);
        counter
    }

    fn counter_read(counter: Custom<Counter>) -> isize {
        counter.0.get()
    }
}

// Hack! Trick buck into believing that these libraries are used. See [Note:
// Test blocks for Cargo] in `ocamlrep_ocamlpool/test/ocamlpool_test.rs`.
const _: () = {
    #[allow(unused_imports)]
    use anyhow;
    #[allow(unused_imports)]
    use cargo_test_utils;
    #[allow(unused_imports)]
    use tempfile;
};

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use cargo_test_utils::*;
    use tempfile::TempDir;

    #[test]
    fn counter_test() -> Result<()> {
        let tmp_dir = TempDir::with_prefix("ocamlrep_custom_test.")?;
        std::fs::copy(
            "counter_client.ml",
            tmp_dir.path().join("counter_client.ml"),
        )?;
        let compile_cmd = cmd(
            "ocamlopt.opt",
            &[
                "-verbose",
                "-c",
                "counter_client.ml",
                "-o",
                "counter_client_ml.cmx",
            ],
            Some(tmp_dir.path()),
        );
        assert_eq!(run(compile_cmd).map_err(fmt_exit_status_err), Ok(()));
        let link_cmd = cmd(
            "ocamlopt.opt",
            &[
                "-verbose",
                "-o",
                "counter_test",
                "counter_client_ml.cmx",
                "-ccopt",
                &("-L".to_owned() + workspace_dir(&["target", build_flavor()]).to_str().unwrap()),
                "-cclib",
                "-lcounter",
                "-cclib",
                "-locamlrep_ocamlpool",
            ],
            Some(tmp_dir.path()),
        );
        assert_eq!(run(link_cmd).map_err(fmt_exit_status_err), Ok(()));
        let counter_test_cmd = cmd(
            tmp_dir
                .path()
                .join("counter_test")
                .as_path()
                .to_str()
                .unwrap(),
            &[],
            None,
        );
        assert_eq!(run(counter_test_cmd).map_err(fmt_exit_status_err), Ok(()));
        tmp_dir.close()?;
        Ok(())
    }
}
