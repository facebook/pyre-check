// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

#[macro_use]
extern crate rust_to_ocaml_attr;

use cst_to_ast::Parser as CSTToASTParser;
use parser_pre_process::remove_comments;

use crate::errors::recoverable_error_to_string;

pub mod ast;
pub mod constants;
pub mod cst_to_ast;
pub mod errors;
pub mod node_wrapper;
pub mod parser_post_process;
pub mod parser_pre_process;
pub mod sitter;
pub mod string_helpers;

ocamlrep_ocamlpool::ocaml_ffi! {
    fn parse_module(_source_text: bstr::BString) -> Result< (ast::Mod_, Vec<ast::RecoverableErrorWithLocation>), String> {
        let input_code_as_rust_string = format!("{}", _source_text);
        let input_without_comments = remove_comments(input_code_as_rust_string);

        let mut cst_to_ast = CSTToASTParser::new(input_without_comments);

        match cst_to_ast.parse() {
            Ok(_) => {
                let ast_and_metadata = cst_to_ast.ast_and_metadata;
                let ast = ast_and_metadata.ast.unwrap();
                let recoverable_errors = ast_and_metadata.recoverable_errors;
                let mut errors: Vec<ast::RecoverableErrorWithLocation> = vec![];
                for recoverable_error_with_location in recoverable_errors{
                    let error_message = recoverable_error_to_string(&recoverable_error_with_location.parser_error);
                    let location = &recoverable_error_with_location.location;

                    errors.push(ast::RecoverableErrorWithLocation{
                        error: error_message,
                        lineno: location.lineno,
                        col_offset: location.col_offset,
                        end_lineno: location.end_lineno,
                        end_col_offset: location.end_col_offset
                    });
                }
                Ok((ast, errors))
            }
            Err(e) => Err(format!("Failure parsing input\n{:?}\n", e))
        }
    }
}
