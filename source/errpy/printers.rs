// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

pub mod ast_pretty_print;
pub mod ast_pretty_print_helper;
pub mod ast_print;
use std::fs;
use std::io;
use std::io::BufRead;
use std::path::PathBuf;

use ast_pretty_print_helper::PrintHelper;
use cst_to_ast::get_node_text;
use cst_to_ast::ASTAndMetaData;
use cst_to_ast::Parser as CSTToASTParser;
use errors::recoverable_error_to_string;
use errors::ParserError;
use node_wrapper::build_node_tree;
use node_wrapper::Node;
use parser_pre_process::remove_comments;
use tree_sitter::Parser as TreeSitterParser;
use tree_sitter::TreeCursor;

use crate::cst_to_ast;
use crate::errors;
use crate::node_wrapper;
use crate::node_wrapper::FilteredCST;
use crate::parser_pre_process;

pub enum PrintingMode {
    ASTOnly,
    PrettyPrintASTOnly,
    ASTAndPrettyPrintAST,
}

pub fn parse_module_print_ast_pretty_and_errors(input_code: String) -> (String, String, String) {
    let parsing_and_printer = ParsingAndPrinter::new(input_code);
    (
        parsing_and_printer.get_print_ast(),
        parsing_and_printer.get_pretty_print_ast(),
        parsing_and_printer.get_recoverable_errors(),
    )
}

pub fn read_stdin_or_file(input_parameter: PathBuf) -> String {
    if input_parameter == PathBuf::from("-") {
        let lines = io::stdin().lock().lines();
        let mut user_input = String::new();

        for line in lines {
            let last_input = line.unwrap();
            user_input.push_str(&last_input);
            user_input.push('\n');
        }
        user_input
    } else {
        fs::read_to_string(&input_parameter).unwrap()
    }
}

pub fn parse_module_print_ast_code(
    input_code: String,
    printing_mode: PrintingMode,
) -> (String, String) {
    let (print_ast, pretty_print_ast) = match printing_mode {
        PrintingMode::ASTOnly => (true, false),
        PrintingMode::PrettyPrintASTOnly => (false, true),
        PrintingMode::ASTAndPrettyPrintAST => (true, true),
    };

    let parsing_and_printer = ParsingAndPrinter::new(input_code);
    let pprint_output = &mut String::new();

    if print_ast {
        pprint_output.push_str(parsing_and_printer.get_print_ast().as_str());
    }

    if pretty_print_ast {
        pprint_output.push_str(parsing_and_printer.get_pretty_print_ast().as_str());
    }

    (
        pprint_output.to_string(),
        parsing_and_printer.get_recoverable_errors(),
    )
}

struct ParsingAndPrinter {
    cst_to_ast: CSTToASTParser,
    parse_result: Result<(), ParserError>,
}

impl ParsingAndPrinter {
    pub fn new(input_code: String) -> Self {
        let input_without_comments = remove_comments(input_code);
        let mut cst_to_ast = CSTToASTParser::new(input_without_comments);
        let parse_result = cst_to_ast.parse();
        ParsingAndPrinter {
            cst_to_ast,
            parse_result,
        }
    }

    pub fn get_print_ast(&self) -> String {
        match &self.parse_result {
            Ok(_) => format!("{}\n", self.cst_to_ast.get_ast_and_metadata()),
            Err(e) => format!("Failure parsing python module\n{:?}\n", e),
        }
    }

    pub fn get_pretty_print_ast(&self) -> String {
        let to_print = &mut String::new();
        let printer = &mut PrintHelper::new(to_print, 4);
        self.cst_to_ast.get_ast_and_metadata().pprint(printer);

        to_print.to_string()
    }

    pub fn get_recoverable_errors(&self) -> String {
        extract_errors(self.cst_to_ast.get_ast_and_metadata())
    }
}

///
/// Will return an empty string if there are no errors
fn extract_errors(metadata: &ASTAndMetaData) -> String {
    let mut errors = String::new();
    if !metadata.recoverable_errors.is_empty() {
        let error_count = metadata.recoverable_errors.len();

        let errors_word = if error_count > 1 { "errors" } else { "error" };

        errors.push_str(&format!(
            "{} Recoverable {} detected:\n",
            error_count, errors_word
        ));

        for recoverable_error_with_location in &metadata.recoverable_errors {
            let recoverable_as_string =
                recoverable_error_to_string(&recoverable_error_with_location.parser_error);

            let location = &recoverable_error_with_location.location;
            errors.push_str(
                &format!(
                    "{} at [{}:{} - {}:{}]",
                    recoverable_as_string,
                    location.lineno,
                    location.col_offset,
                    location.end_lineno,
                    location.end_col_offset
                )
                .to_string(),
            );
            errors.push('\n');

            let stack = &recoverable_error_with_location.stack;
            errors.push_str("CST Stack:\n");
            for item in stack.iter() {
                errors.push_str("    ");
                errors.push_str(item);
                errors.push('\n');
            }
        }
    }
    errors
}

pub struct CSTPrinter {
    code: String,
}

impl CSTPrinter {
    pub fn new(code: String) -> CSTPrinter {
        CSTPrinter { code }
    }

    // Prints the sitter nodes in their "derived `Debug` form"
    pub fn print_cst(&self, is_filter_errors: bool) {
        let mut parser = TreeSitterParser::new();
        parser
            .set_language(tree_sitter_python::language())
            .expect("Fail to initialize TreeSitter");
        let tree = parser.parse(&self.code, None).expect("Fail to parse file");

        println!(
            ">>> Tree-Sitter CST Nodes{}:\n",
            if is_filter_errors { " (filtered)" } else { "" }
        );

        if is_filter_errors {
            let filtered_cst = build_node_tree(tree.root_node());
            self.print_filtered_cst_node(&filtered_cst, filtered_cst.get_root(), "");
        } else {
            let mut tree_cursor_root = tree.walk();
            self.print_cst_node(&mut tree_cursor_root, "");
        }
    }

    fn print_filtered_cst_node(&self, filtered_cst: &FilteredCST, node: &Node, indent: &str) {
        println!(
            "{}{:?} :: {}",
            indent,
            node,
            get_node_text(&self.code, node).replace('\n', "\\n")
        );
        for child in node.children(filtered_cst) {
            let new_indent = format!("  {}", indent);
            self.print_filtered_cst_node(filtered_cst, child, new_indent.as_str());
        }
    }

    fn print_cst_node(&self, tree_cursor: &mut TreeCursor<'_>, indent: &str) {
        println!(
            "{}{:?} :: {}",
            indent,
            tree_cursor.node(),
            tree_cursor
                .node()
                .utf8_text(self.code.as_bytes())
                .expect("Invalid Identifier")
                .to_string()
                .replace('\n', "\\n")
        );

        // Preorder traversal of the tree to print out the nodes
        if tree_cursor.goto_first_child() {
            let new_indent = format!("  {}", indent);
            self.print_cst_node(tree_cursor, new_indent.as_str());
            while tree_cursor.goto_next_sibling() {
                self.print_cst_node(tree_cursor, new_indent.as_str());
            }
            tree_cursor.goto_parent();
        }
    }
}
