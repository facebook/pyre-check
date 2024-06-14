/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

#[macro_use]
extern crate rust_to_ocaml_attr;

use std::path::PathBuf;

use clap::ArgAction;
use clap::Parser as ClapParser;

use crate::printers::read_stdin_or_file;
use crate::printers::CSTPrinter;

pub mod ast;
pub mod constants;
pub mod cst_to_ast;
pub mod errors;
pub mod node_wrapper;
pub mod parser_post_process;
pub mod parser_pre_process;
pub mod printers;
pub mod sitter;
pub mod string_helpers;

/// Python Parser which will output CST pretty printed.
/// Usage: `print_cst file.py` or `echo "print('hello')" | print_cst`.
/// With a build system such as buck, `buck run //path/to/errpy:print_cst -- file.py` and `echo "print('test')" | buck run //path/to/errpy:print_cst -`
#[derive(ClapParser)]
struct Args {
    /// Python file to generate CST for
    input_file: PathBuf,

    /// If the error nodes should be filtered in the output CST
    /// (default is to include all nodes)
    #[clap(long, short, action=ArgAction::SetTrue)]
    filter_errors: bool,
}

fn main() {
    let args = <Args as clap::Parser>::parse();

    let input_file = read_stdin_or_file(args.input_file);

    let filter_errors = args.filter_errors;

    CSTPrinter::new(input_file).print_cst(filter_errors);
}
