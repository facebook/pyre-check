// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree.

#![allow(unused_crate_dependencies)]
#![feature(exit_status_error)]

use ocamlrep_ocamlpool::ocaml_ffi;
use ocamlrep_ocamlpool::ocaml_registered_function;
use ocamlrep_ocamlpool::FromOcamlRep;

extern "C" {
    fn ocamlpool_enter();
    fn ocamlpool_reserve_block(tag: u8, size: usize) -> usize;
    fn ocamlpool_leave();
}

// This test attempts to catch off by one issues in ocamlpool.c

// Magic constant needs to fulfill two requirements:
// Needs to be above the OCAMLPOOL_DEFAULT_SIZE constant in ocamlpool.h
//   This requirement is easy to fulfill
// Needs to be the exact size of memory block allocated by ocamlpool_reserve_block
//   which is given by the Chunk_size call in chunk_alloc in ocamlpool.c
//   This requirement requires some magic
const MAGIC_MEMORY_SIZE: usize = 1053183;

ocaml_registered_function! {
    fn f_unit_to_unit();
    fn f_one_arg_to_unit(x: i64);
    fn f_sum_tuple(args: (i64, i64)) -> i64;
}

ocaml_ffi! {
    fn test() {
        unsafe {
            ocamlpool_enter();
            // This line will crash on off by one error
            ocamlpool_reserve_block(0, MAGIC_MEMORY_SIZE);
            ocamlpool_leave();
        }
    }

    fn test_call_ocaml_from_rust() {
        for _ in 0..4 {
            unsafe {
                f_unit_to_unit();
                f_one_arg_to_unit(3);
                assert!(f_sum_tuple((3, 4)) == 7);
            }
        }
    }
}

// [Note: Test blocks for Cargo]
// -----------------------------
// With buck, where testing involves compiling OCaml we make use of
// `ocaml_binary` & `custom_unnittest` rules.
//
// When testing with cargo we instead use `#[cfg(test_blocks)]` within which we
// compile OCaml "manually" using the rust `command` crate. Thus, in these cases
// the `#[cfg(test)]` blocks are for cargo only & not buck. We express that
// according to the following schema:
// ```
//  rust_library(
//     name = "foo_test",
//     ...
//     autocargo = {
//       ...
//       "test": True, // Yes, unittests for Cargo...
//       ...
//     },
//     ...
//     unittests=False, // No! No, no unittests for Buck!
//     ...
//  )
//```
//
// If in such a `#[cfg(test)]` block we now wish to use a crate not otherwise
// dependend upon & put it in the target's `deps` section in `TARGETS` there
// will be an unsused-crate error. If we put it in the target's `test_deps`
// section in `TARGETS` buck will rightly complain that `unittests=False` (so
// how can there be `tests_deps`?).
//
// The workaround I employ is to add `allow(unused_crate_dependencies)` to this
// module. That way they can be enumerated in the `deps` and are thereby
// availabe for use in the `#[cfg(test)]` blocks.

#[cfg(test)]
mod tests {
    use anyhow::Result;
    use cargo_test_utils::*;
    use tempfile::TempDir;

    #[test]
    fn ocamlpool_test() -> Result<()> {
        let tmp_dir = TempDir::with_prefix("ocamlpool_test.")?;
        std::fs::copy(
            "ocamlpool_test.ml",
            tmp_dir.path().join("ocamlpool_test.ml"),
        )?;
        let compile_cmd = cmd(
            "ocamlopt.opt",
            &[
                "-verbose",
                "-c",
                "ocamlpool_test.ml",
                "-o",
                "ocamlpool_test_ml.cmx",
            ],
            Some(tmp_dir.path()),
        );
        assert_eq!(run(compile_cmd).map_err(fmt_exit_status_err), Ok(()));
        let link_cmd = cmd(
            "ocamlopt.opt",
            &[
                "-verbose",
                "-o",
                "ocamlpool_test",
                "ocamlpool_test_ml.cmx",
                "-ccopt",
                &("-L".to_owned() + workspace_dir(&["target", build_flavor()]).to_str().unwrap()),
                "-cclib",
                "-locamlpool_test",
                "-cclib",
                "-locamlrep_ocamlpool",
            ],
            Some(tmp_dir.path()),
        );
        assert_eq!(run(link_cmd).map_err(fmt_exit_status_err), Ok(()));
        let ocamlpool_test_cmd = cmd(
            tmp_dir
                .path()
                .join("ocamlpool_test")
                .as_path()
                .to_str()
                .unwrap(),
            &[],
            None,
        );
        assert_eq!(run(ocamlpool_test_cmd).map_err(fmt_exit_status_err), Ok(()));
        tmp_dir.close()?;
        Ok(())
    }
}
