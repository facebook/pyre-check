// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

#[macro_use]
extern crate rust_to_ocaml_attr;

use std::path::PathBuf;

use crate::printers::parse_module_print_ast_code;
use crate::printers::read_stdin_or_file;
use crate::printers::PrintingMode;

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

/// Python Parser which will output AST pretty printed.
/// Usage: `print_ast file.py` or `echo "print('hello')" | print_ast`.
/// With a build system such as buck, `buck run //path/to/errpy:print_ast -- file.py` and `echo "print('test')" | buck run //path/to/errpy:print_ast -`
#[derive(clap::Parser)]
struct Args {
    /// Python file to generate AST for
    input_file: PathBuf,
}

fn main() {
    let args = <Args as clap::Parser>::parse();

    let input_file = read_stdin_or_file(args.input_file);
    let (ast, errors) = parse_module_print_ast_code(input_file, PrintingMode::ASTAndPrettyPrintAST);
    if errors.is_empty() {
        println!("{}", ast);
    } else {
        print!("{}\n{}", ast, errors);
    }
}
