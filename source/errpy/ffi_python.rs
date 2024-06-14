// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

use pyo3::prelude::*;

#[macro_use]
extern crate rust_to_ocaml_attr;

use cst_to_ast::Parser as CSTToASTParser;

use crate::printers::parse_module_print_ast_code;
use crate::printers::parse_module_print_ast_pretty_and_errors;
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

#[pyfunction]
fn py_parse_module_print_ast(input_code: String) -> PyResult<(String, String)> {
    Ok(parse_module_print_ast_code(
        input_code,
        PrintingMode::ASTOnly,
    ))
}

#[pyfunction]
fn py_parse_module_print_ast_and_pretty_print(input_code: String) -> PyResult<(String, String)> {
    Ok(parse_module_print_ast_code(
        input_code,
        PrintingMode::ASTAndPrettyPrintAST,
    ))
}

#[pyfunction]
fn py_parse_module_print_ast_pretty_print_only(input_code: String) -> PyResult<(String, String)> {
    Ok(parse_module_print_ast_code(
        input_code,
        PrintingMode::PrettyPrintASTOnly,
    ))
}

#[pyfunction]
fn py_parse_module_print_ast_pretty_and_errors(
    input_code: String,
) -> PyResult<(String, String, String)> {
    Ok(parse_module_print_ast_pretty_and_errors(input_code))
}

///
/// run errpy but ignore result - useful for benchmarking
#[pyfunction]
fn py_parse_module(input_code: String) -> PyResult<()> {
    let mut cst_to_ast = CSTToASTParser::new(input_code);
    _ = cst_to_ast.parse();

    Ok(())
}

/// A Python module implemented in Rust. The name of this function must match
/// the `lib.name` setting in the `Cargo.toml`, else Python will not be able to
/// import the module.
#[pymodule]
fn ffi_python(_py: Python<'_>, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(py_parse_module_print_ast, m)?)?;
    m.add_function(wrap_pyfunction!(
        py_parse_module_print_ast_and_pretty_print,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(
        py_parse_module_print_ast_pretty_print_only,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(
        py_parse_module_print_ast_pretty_and_errors,
        m
    )?)?;
    m.add_function(wrap_pyfunction!(py_parse_module, m)?)?;
    Ok(())
}
