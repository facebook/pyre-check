/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Mapping of grammar defined here (or variant thereof):
// https://github.com/tree-sitter/tree-sitter-python/blob/master/grammar.js

use std::collections::HashMap;
use std::collections::HashSet;
use std::num::ParseIntError;

use ast::Alias;
use ast::Arg;
use ast::Arguments;
use ast::Boolop;
use ast::Cmpop;
use ast::Comprehension;
use ast::ConstantDesc;
use ast::Excepthandler;
use ast::ExcepthandlerDesc;
use ast::Expr;
use ast::ExprContext;
use ast::ExprDesc;
use ast::Keyword as AstKeyword;
use ast::MatchCase;
use ast::Mod_;
use ast::Num;
use ast::Operator;
use ast::Pattern;
use ast::PatternDesc;
use ast::Stmt;
use ast::StmtDesc;
use ast::Unaryop;
use ast::Withitem;
use constants::HEXA_CONVERSION;
use constants::OCTAL_MAP;
use constants::SPECIAL_CHARS;
use errors::ParserError;
use errors::RecoverableError;
use itertools::join;
use node_wrapper::build_node_tree;
use node_wrapper::Node;
use parser_post_process::ParserPostprocessor;
use parser_post_process::AUTOCOMPLETE_TOKEN;
use sitter::get_node_type;
use sitter::AugAssignOperator;
use sitter::BinaryOperator;
use sitter::ComparisonOperator;
use sitter::Keyword;
use sitter::NodeType;
use sitter::Production;
use sitter::ProductionKind;
use string_helpers::categorize_string;
use string_helpers::string_prefix;
use string_helpers::StringCategory;
use tree_sitter::Node as TSNode;
use tree_sitter::Parser as SitterParser;

use crate::ast;
use crate::constants;
use crate::errors;
use crate::node_wrapper;
use crate::node_wrapper::FilteredCST;
use crate::parser_post_process;
use crate::sitter;
use crate::string_helpers;

type ErrorableResult<T> = std::result::Result<T, ()>;

#[derive(Debug)]
pub struct RecoverableErrorWithLocation {
    pub parser_error: RecoverableError,
    pub location: RecoverableErrorLocation,
    pub stack: Vec<String>,
}

#[derive(Debug)]
pub struct RecoverableErrorLocation {
    pub lineno: usize,
    pub col_offset: usize,
    pub end_lineno: usize,
    pub end_col_offset: usize,
}

///
/// Parser is responsible for driving parsing of a python code String into an internal CST representation
/// before lowering to an AST. The AST is expected to match 1:1 with CPython. The AST is held within an
/// `ASTAndMetaData` instance (and potentitally additional metadata)
#[derive(Debug)]
pub struct Parser {
    code: String,
    pub ast_and_metadata: ASTAndMetaData,
}

#[derive(Debug)]
pub struct FilteredCSTParser<'a> {
    // `Filtered cst parser` is created after an initial phase of parsing in `Parser`
    parser: &'a mut Parser,
    filtered_cst: &'a FilteredCST<'a>,
    // contingent on if we are on lhs or rhs of assignment or del expression
    current_expr_ctx: Vec<Option<ExprContext>>,
    integer_overflow_error: ParseIntError,
    python_keywords: HashSet<String>,
}

///
/// `ASTAndMetaData` presently just holds the lowered AST
#[derive(Debug)]
pub struct ASTAndMetaData {
    // AST root for what was parsed correctly
    pub ast: Option<Mod_>,
    pub recoverable_errors: Vec<RecoverableErrorWithLocation>,
}

impl ASTAndMetaData {
    fn new() -> Self {
        ASTAndMetaData {
            ast: None,
            recoverable_errors: vec![],
        }
    }
}

impl Stmt {
    fn new(desc: StmtDesc, node_start: &Node, node_end: &Node) -> Stmt {
        let start_position = node_start.start_position();
        // Comment node is not to be considered for the end_lineno and end_col_offset
        let end_position = node_end.end_position();

        Stmt {
            desc,
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: Some(end_position.row as isize + 1),
            end_col_offset: Some(end_position.column as isize),
        }
    }
}

impl AstKeyword {
    fn new(arg: Option<String>, value: Expr, node: &Node) -> AstKeyword {
        let start_position = node.start_position();
        let end_position = node.end_position();

        AstKeyword {
            arg,
            value,
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: Some(end_position.row as isize + 1),
            end_col_offset: Some(end_position.column as isize),
        }
    }
}

impl Expr {
    fn new(
        desc: ExprDesc,
        lineno: isize,
        col_offset: isize,
        end_lineno: isize,
        end_col_offset: isize,
    ) -> Expr {
        Expr {
            desc: Box::new(desc),
            lineno,
            col_offset,
            end_lineno: Some(end_lineno),
            end_col_offset: Some(end_col_offset),
        }
    }
}

impl Alias {
    fn new(name: String, asname: Option<String>, node: &Node) -> Alias {
        let start_position = node.start_position();
        let end_position = node.end_position();

        Alias {
            name,
            asname,
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: Some(end_position.row as isize + 1),
            end_col_offset: Some(end_position.column as isize),
        }
    }
}

impl Excepthandler {
    fn new(desc: ExcepthandlerDesc, node: &Node) -> Excepthandler {
        let start_position = node.start_position();
        let end_position = node.end_position();

        Excepthandler {
            desc,
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: Some(end_position.row as isize + 1),
            end_col_offset: Some(end_position.column as isize),
        }
    }
}

impl Arg {
    fn new_simple(arg: String, start_node: &Node, end_node: &Node) -> Arg {
        let start_position = start_node.start_position();
        let end_position = end_node.end_position();

        Arg {
            arg,
            annotation: None,
            type_comment: None,
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: Some(end_position.row as isize + 1),
            end_col_offset: Some(end_position.column as isize),
        }
    }

    fn new_with_type(arg: String, annotation: Expr, start_node: &Node, end_node: &Node) -> Arg {
        let start_position = start_node.start_position();
        let end_position = end_node.end_position();

        Arg {
            arg,
            annotation: Some(annotation),
            type_comment: None,
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: Some(end_position.row as isize + 1),
            end_col_offset: Some(end_position.column as isize),
        }
    }
}

impl Parser {
    pub fn new(code: String) -> Self {
        Parser {
            code,
            ast_and_metadata: ASTAndMetaData::new(),
        }
    }

    pub fn get_ast_and_metadata(&self) -> &ASTAndMetaData {
        &self.ast_and_metadata
    }

    ///
    /// Public entry point to parse code.
    /// Code is defined at construction time (`new`) but it could also be passed
    /// to this function. We could also pass a delta
    pub fn parse(&mut self) -> Result<(), ParserError> {
        let mut cst_to_ast = SitterParser::new();
        cst_to_ast.set_language(tree_sitter_python::language())?;

        // Source file to CST via Tree-sitter
        let mut tree = match cst_to_ast.parse(&self.code, None) {
            Some(t) => t,
            None => return Err(ParserError::DidNotComplete),
        };

        // Error in first parse -> mutate source file and reparse
        if tree.root_node().has_error() {
            self.find_error_nodes(tree.root_node());
            let parser_post_processor = ParserPostprocessor::new();

            // Collect lines that will be mutated
            let mut err_lines: HashSet<usize> = HashSet::new();
            for err in &self.ast_and_metadata.recoverable_errors {
                for line_no in err.location.lineno..=err.location.end_lineno {
                    err_lines.insert(line_no - 1);
                }
            }

            self.code = parser_post_processor.postprocess(&self.code, err_lines);
            tree = match cst_to_ast.parse(&self.code, None) {
                Some(t) => t,
                None => return Err(ParserError::DidNotComplete),
            };
        };

        // Tree-sitter CST to FilteredCST (without ERROR or COMMENT nodes)
        let filtered_cst = build_node_tree(tree.root_node());
        let mut filtered_cst_parser = FilteredCSTParser::new(self, &filtered_cst);

        // FilteredCST to AST
        filtered_cst_parser.parse_module(filtered_cst.get_root());
        Ok(())
    }

    fn assemble_node_stack_tsnode(&mut self, node: &TSNode) -> Vec<String> {
        let mut result: Vec<String> = Vec::new();
        let mut current: Option<TSNode> = Some(*node);
        while let Some(n) = current {
            result.push(format!("{:?}", n));
            current = n.parent();
        }
        result
    }

    ///
    /// Mark all error nodes from the Tree-sitter CST as SyntaxErrors
    fn find_error_nodes(&mut self, node: TSNode) {
        if node.kind() == "ERROR" {
            let parser_error = RecoverableError::SyntaxError("invalid syntax".to_string());

            let start_position = node.start_position();
            let end_position = node.end_position();

            let location = RecoverableErrorLocation {
                lineno: start_position.row + 1,
                col_offset: start_position.column + 1,
                end_lineno: end_position.row + 1,
                end_col_offset: end_position.column + 1,
            };

            let stack = self.assemble_node_stack_tsnode(&node);

            self.ast_and_metadata
                .recoverable_errors
                .push(RecoverableErrorWithLocation {
                    parser_error,
                    location,
                    stack,
                });

            // don't process child nodes of ERROR nodes - otherwise this can
            // lead to a cascade of ERROR nodes being reported
            return;
        }
        for child in node.children(&mut node.walk()) {
            self.find_error_nodes(child);
        }
    }

    fn new_pattern(&mut self, pattern_desc: PatternDesc, node: &Node) -> Pattern {
        let start_position = node.start_position();
        let end_position = node.end_position();

        Pattern {
            desc: Box::new(pattern_desc),
            lineno: start_position.row as isize + 1,
            col_offset: start_position.column as isize,
            end_lineno: end_position.row as isize + 1,
            end_col_offset: end_position.column as isize,
        }
    }

    fn new_expr(&mut self, desc: ExprDesc, node: &Node) -> Expr {
        self.new_expr_with_start_end_node(desc, node, node)
    }

    fn new_expr_with_start_end_node(
        &mut self,
        desc: ExprDesc,
        start_node: &Node,
        end_node: &Node,
    ) -> Expr {
        let start_position = start_node.start_position();
        let end_position = end_node.end_position();

        Expr::new(
            desc,
            start_position.row as isize + 1,
            start_position.column as isize,
            end_position.row as isize + 1,
            end_position.column as isize,
        )
    }
}

impl<'parser> FilteredCSTParser<'parser> {
    pub fn new(parser: &'parser mut Parser, filtered_cst: &'parser FilteredCST) -> Self {
        FilteredCSTParser {
            parser,
            filtered_cst,
            current_expr_ctx: Vec::new(),
            integer_overflow_error: "184467440737095516150".parse::<isize>().err().unwrap(),
            // keywords obtained through running: buck2 run errpy/facebook/scripts:list_python_keywords -- errpy/facebook/scripts/peg_grammar_specs/3.10
            python_keywords: vec![
                "and", "as", "assert", "async", "await", "break", "class", "continue", "def",
                "del", "elif", "else", "except", "finally", "for", "from", "global", "if",
                "import", "in", "is", "lambda", "nonlocal", "not", "or", "pass", "raise", "return",
                "try", "while", "with", "yield",
            ]
            .into_iter()
            .map(String::from)
            .collect(),
        }
    }

    fn assemble_node_stack(&mut self, node: &Node) -> Vec<String> {
        let mut result: Vec<String> = Vec::new();
        let mut current: Option<&Node> = Some(node);

        while let Some(some_node) = current {
            result.push(format!("{:?}", some_node));
            current = some_node.parent(self.filtered_cst);
        }
        result
    }

    fn record_recoverable_error(&mut self, parser_error: RecoverableError, node: &Node) {
        let start_position = node.start_position();
        let end_position = node.end_position();

        let location = RecoverableErrorLocation {
            lineno: start_position.row + 1,
            col_offset: start_position.column + 1,
            end_lineno: end_position.row + 1,
            end_col_offset: end_position.column + 1,
        };

        let stack = self.assemble_node_stack(node);

        self.parser
            .ast_and_metadata
            .recoverable_errors
            .push(RecoverableErrorWithLocation {
                parser_error,
                location,
                stack,
            });
    }

    // Process a module.
    // module: $ => repeat($._statement),
    pub fn parse_module(&mut self, root: &Node) {
        // root must be a module
        if root.kind() != "module" {
            self.parser.ast_and_metadata.ast = Some(Mod_::Module {
                body: vec![],
                type_ignores: vec![],
            });
            return;
        }
        let mut body = vec![];
        self.block(root, &mut body);
        self.parser.ast_and_metadata.ast = Some(Mod_::Module {
            body,
            type_ignores: vec![],
        });
    }

    //
    //
    // Functions that consumes the tree-sitter productions
    //
    //

    // Process a generic block updating `statements`.
    // Generally sequences of `repeat($._statement)`
    fn block(&mut self, block: &Node, statements: &mut Vec<Stmt>) {
        for child in block.named_children(self.filtered_cst) {
            let node_type = get_node_type(child);
            match &node_type {
                NodeType::Production(production) => match &production.production_kind {
                    ProductionKind::COMMENT => (),
                    _ => match self.statement(production) {
                        Ok(statement) => statements.push(statement),
                        // ok to skip statements which have errors within them
                        Err(_) => (),
                    },
                },
                _ => (),
            }
        }
    }

    // Process a StmtDesc
    //
    // _statement: $ => choice(
    //   $._simple_statements,
    //   $._compound_statement
    // ),
    // _simple_statements: $ => seq(
    //   sep1($._simple_statement, SEMICOLON),
    //   optional(SEMICOLON),
    //   $._newline
    // ),
    // _simple_statement: $ => choice(
    //   $.future_import_statement,
    //   $.import_statement,
    //   $.import_from_statement,
    //   $.print_statement,
    //   $.assert_statement,
    //   $.expression_statement, // this recurses down
    //   $.return_statement,
    //   $.delete_statement,
    //   $.raise_statement,
    //   $.pass_statement,
    //   $.break_statement,
    //   $.continue_statement,
    //   $.global_statement,
    //   $.nonlocal_statement,
    //   $.exec_statement
    // ),
    // _compound_statement: $ => choice(
    //   $.if_statement,
    //   $.for_statement,
    //   $.while_statement,
    //   $.try_statement,
    //   $.with_statement,
    //   $.function_definition,
    //   $.class_definition,
    //   $.decorated_definition,
    //   $.match_statement,
    // ),
    fn statement(&mut self, rule: &Production) -> ErrorableResult<Stmt> {
        use ProductionKind::*;

        match &rule.production_kind {
            DECORATED_DEFINITION => self.decorated_definition(rule.node),
            rest => {
                let statement_desc = match rest {
                    // _simple_statement
                    FUTURE_IMPORT_STATEMENT => self.future_import_statement(rule.node)?,
                    IMPORT_STATEMENT => self.import_statement(rule.node)?,
                    IMPORT_FROM_STATEMENT => self.import_from_statement(rule.node)?,
                    ASSERT_STATEMENT => self.assert_statement(rule.node)?,
                    EXPRESSION_STATEMENT => self.expression_statement(rule.node)?,
                    RETURN_STATEMENT => self.return_statement(rule.node)?,
                    DELETE_STATEMENT => self.delete_statement(rule.node)?,
                    RAISE_STATEMENT => self.raise_statement(rule.node)?,
                    PASS_STATEMENT => StmtDesc::Pass,
                    BREAK_STATEMENT => StmtDesc::Break,
                    CONTINUE_STATEMENT => StmtDesc::Continue,
                    GLOBAL_STATEMENT => self.global_statement(rule.node)?,
                    NONLOCAL_STATEMENT => self.nonlocal_statement(rule.node)?,
                    // EXEC_STATEMENT,  // legacy, not sure if we will do these two...
                    // PRINT_STATEMENT, // legacy, not sure if we will do these two...
                    // _compound_statement
                    IF_STATEMENT => self.if_statement(rule.node)?,
                    FOR_STATEMENT => self.for_statement(rule.node)?,
                    WHILE_STATEMENT => self.while_statement(rule.node)?,
                    TRY_STATEMENT => self.try_statement(rule.node)?,
                    WITH_STATEMENT => self.with_statement(rule.node)?,
                    FUNCTION_DEFINITION => self.function_definition(rule.node, vec![])?,
                    CLASS_DEFINITION => self.class_definition(rule.node, vec![])?,
                    MATCH_STATEMENT => self.match_statement(rule.node)?,
                    // ,

                    // uncomment above when writing the production and delete from here
                    // the order above is that in the tree sitter grammar so easier to
                    // check for now
                    PRINT_STATEMENT | EXEC_STATEMENT => {
                        return Err(self.record_recoverable_error(
                            RecoverableError::UnimplementedStatement(format!("{:?}", rule.node)),
                            rule.node,
                        ));
                    }
                    _ => panic!("unexpected statement node: {:?}", rule.node),
                };

                Ok(Stmt::new(statement_desc, rule.node, rule.node))
            }
        }
    }

    // match_statement: $ => seq(
    // 'match',
    //  commaSep1(field('subject', $.expression)),
    //  optional(','),
    //  ':',
    //  repeat(field('alternative', $.case_clause))),
    // ),
    fn match_statement<'a>(&mut self, match_node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut cases: Vec<MatchCase> = vec![];

        for case_clause_node in match_node.children_by_field_name(self.filtered_cst, "alternative")
        {
            let case_clause = self.case_clause(case_clause_node);

            match case_clause {
                Ok(case_clause) => cases.push(case_clause),
                _ => (), // Error will already have been flagged so there isn't
                         // anything special to do here besides ignoring the
                         // case_clause
            }
        }

        let subject_node = match_node
            .child_by_field_name(self.filtered_cst, "subject")
            .expect("subject node");
        let subject = self.expression(subject_node)?;

        Ok(StmtDesc::Match { subject, cases })
    }

    //  case_clause: $ => seq(
    //   'case',
    //   commaSep1(
    //     field(
    //       'pattern',
    //       choice($.case_pattern, $.case_open_sequence_pattern),
    //    )
    //   ),
    //   optional(','),
    //   optional(field('guard', $.if_clause)),
    //   ':',
    //   field('consequence', $._suite)
    // ),
    fn case_clause<'a>(&mut self, case_clause_node: &'a Node<'a>) -> ErrorableResult<MatchCase> {
        let pattern_node = case_clause_node
            .child_by_field_name(self.filtered_cst, "pattern")
            .expect("missing pattern");

        let pattern = match pattern_node.kind() {
            "case_pattern" => self.case_pattern(pattern_node)?,
            _ => {
                let pattern_desc = self.case_open_sequence_pattern(pattern_node)?;
                self.parser.new_pattern(pattern_desc, pattern_node)
            }
        };

        let guard = match &case_clause_node.child_by_field_name(self.filtered_cst, "guard") {
            Some(guard) => Some(self.if_clause(guard)?),
            None => None,
        };

        let body_node = case_clause_node
            .child_by_field_name(self.filtered_cst, "consequence")
            .expect("missing body");
        let mut body = vec![];
        self.block(body_node, &mut body);

        Ok(MatchCase {
            pattern,
            guard,
            body,
        })
    }

    fn case_pattern(&mut self, pattern_node: &Node) -> ErrorableResult<Pattern> {
        let as_or_or_pattern_node = &pattern_node.child(self.filtered_cst, 0).expect("child");

        match as_or_or_pattern_node.kind() {
            "case_as_pattern" => {
                let case_as_pattern = self.case_as_pattern(as_or_or_pattern_node)?;
                Ok(self.parser.new_pattern(case_as_pattern, pattern_node))
            }
            _ => self.case_or_pattern(as_or_or_pattern_node),
        }
    }

    fn case_as_pattern(&mut self, as_pattern_node: &Node) -> ErrorableResult<PatternDesc> {
        let or_pattern_node = as_pattern_node
            .child_by_field_name(self.filtered_cst, "or_pattern")
            .expect("missing as pattern left hand side pattern");
        let pattern = Some(self.case_or_pattern(or_pattern_node)?);

        let name_node = as_pattern_node
            .child_by_field_name(self.filtered_cst, "identifier")
            .expect("missing as pattern identifier");
        let name = Some(self.get_valid_identifier(name_node));

        Ok(PatternDesc::MatchAs { pattern, name })
    }

    // case_or_pattern: $ => seq(
    //  $.case_closed_pattern, repeat(seq('|', $.case_closed_pattern))),
    fn case_or_pattern(&mut self, or_pattern_node: &Node) -> ErrorableResult<Pattern> {
        let mut case_closed_pattern_nodes = vec![];
        for case_closed_pattern_node in or_pattern_node.named_children(self.filtered_cst) {
            case_closed_pattern_nodes.push(case_closed_pattern_node);
        }

        match case_closed_pattern_nodes.len() {
            1 => {
                let case_closed_pattern_node = case_closed_pattern_nodes.pop().unwrap();
                self.case_closed_pattern(case_closed_pattern_node)
            }
            _ => {
                let mut or_choices = vec![];

                for case_closed_pattern_node in case_closed_pattern_nodes {
                    match self.case_closed_pattern(case_closed_pattern_node) {
                        Ok(case_closed_pattern) => {
                            or_choices.push(case_closed_pattern);
                        }
                        _ => (),
                    }
                }
                let match_or = PatternDesc::MatchOr(or_choices);
                Ok(self.parser.new_pattern(match_or, or_pattern_node))
            }
        }
    }

    // case_closed_pattern: $ => choice(
    //  $.case_literal_pattern,
    //  $.dotted_name,
    //  $.case_wildcard_pattern,
    //  $.case_group_pattern,
    //  $.case_sequence_pattern,
    //  $.case_mapping_pattern,
    //  $.case_class_pattern,
    // ),
    fn case_closed_pattern<'a>(
        &mut self,
        case_closed_pattern_node: &'a Node<'a>,
    ) -> ErrorableResult<Pattern> {
        let one_child = &case_closed_pattern_node
            .child(self.filtered_cst, 0)
            .unwrap();

        let node_kind = one_child.kind();

        if node_kind == "case_group_pattern" {
            self.case_group_pattern(one_child)
        } else {
            let pattern_desc = match node_kind {
                "case_literal_pattern" => self.case_literal_pattern(one_child)?,
                "case_wildcard_pattern" => PatternDesc::MatchAs {
                    pattern: None,
                    name: None,
                },
                "dotted_name" => {
                    // One element is translated to a identifier wrapped into a MatchAs
                    // More than one is translated into Attribute access pattern wrapped in a MatchValue
                    let mut name_parts = vec![];
                    for part in one_child.named_children(self.filtered_cst) {
                        name_parts.push(part);
                    }

                    match name_parts.len() {
                        1 => PatternDesc::MatchAs {
                            pattern: None,
                            name: Some(self.get_valid_identifier(name_parts.pop().unwrap())),
                        },
                        _ => {
                            // multiple dot names: `a.b.c` treated as a MatchValue of Attribute accesses
                            let expr_desc = self
                                .wrap_dotted_name_into_attribute_access_for_case_patterns(
                                    name_parts, one_child,
                                );

                            PatternDesc::MatchValue(self.parser.new_expr(expr_desc, one_child))
                        }
                    }
                }
                "case_sequence_pattern" => self.case_sequence_pattern(one_child)?,
                "case_mapping_pattern" => self.case_mapping_pattern(one_child)?,
                "case_class_pattern" => self.case_class_pattern(one_child)?,
                _ => {
                    return Err(self.record_recoverable_error(
                        RecoverableError::UnimplementedStatement(format!(
                            "case_closed_pattern_node of kind: {}",
                            node_kind
                        )),
                        one_child,
                    ));
                }
            };

            Ok(self
                .parser
                .new_pattern(pattern_desc, case_closed_pattern_node))
        }
    }

    // case_literal_pattern: $ => choice(
    //   $.string,
    //   $.concatenated_string,
    //   $.case_literal_pattern_complex_number,
    //   $._integer_or_float,
    //   $.float,
    //   $.true,
    //   $.false,
    //   $.none
    // ),
    fn case_literal_pattern<'a>(
        &mut self,
        literal_pattern_node: &'a Node<'a>,
    ) -> ErrorableResult<PatternDesc> {
        // True, False, None are mapped to MatchSingleton, everything else to MatchValue
        let child_node = &literal_pattern_node.child(self.filtered_cst, 0).unwrap();
        Ok(match child_node.kind() {
            "case_literal_integer_or_float" => {
                let case_literal_integer_or_float =
                    self.case_literal_integer_or_float(child_node)?;
                PatternDesc::MatchValue(case_literal_integer_or_float.0)
            }
            "case_literal_pattern_complex_number" => {
                PatternDesc::MatchValue(self.case_literal_pattern_complex_number(child_node)?)
            }
            "false" => PatternDesc::MatchSingleton(Some(ConstantDesc::Bool(false))),
            "true" => PatternDesc::MatchSingleton(Some(ConstantDesc::Bool(true))),
            "none" => PatternDesc::MatchSingleton(None),
            _ => {
                let value = self.expression(child_node)?;
                PatternDesc::MatchValue(value)
            }
        })
    }

    // case_literal_integer_or_float : $ => seq(
    //  field('neg', optional('-')),
    //  choice($.integer, $.float)),
    // ),
    fn case_literal_integer_or_float<'a>(
        &mut self,
        case_literal_integer_or_float_node: &'a Node<'a>,
    ) -> ErrorableResult<(Expr, bool, bool)> {
        if case_literal_integer_or_float_node
            .child_by_field_name(self.filtered_cst, "neg")
            .is_some()
        {
            let child_node = &case_literal_integer_or_float_node
                .child(self.filtered_cst, 1)
                .unwrap();
            let operand = self.expression(child_node)?;

            let is_complex = match *operand.desc {
                ExprDesc::Constant {
                    value: Some(ConstantDesc::Num(Num::Complex(_))),
                    kind: _,
                } => true,
                _ => false,
            };

            let expr_desc = ExprDesc::UnaryOp {
                op: Unaryop::USub,
                operand,
            };
            Ok((
                self.parser
                    .new_expr(expr_desc, case_literal_integer_or_float_node),
                is_complex,
                true,
            ))
        } else {
            let expression = self.expression(
                case_literal_integer_or_float_node
                    .child(self.filtered_cst, 0)
                    .unwrap(),
            )?;

            let is_complex = match *expression.desc {
                ExprDesc::Constant {
                    value: Some(ConstantDesc::Num(Num::Complex(_))),
                    kind: _,
                } => true,
                _ => false,
            };

            Ok((expression, is_complex, false))
        }
    }

    // case_literal_pattern_complex_number: $ => seq(
    //  field("real_component", $.case_literal_integer_or_float),
    //  field("sign", choice('+', '-')),
    //  field("imaginary_component", $.case_literal_integer_or_float),
    // ),
    fn case_literal_pattern_complex_number<'a>(
        &mut self,
        case_literal_pattern_complex_number_node: &'a Node<'a>,
    ) -> ErrorableResult<Expr> {
        let real_component_node = case_literal_pattern_complex_number_node
            .child_by_field_name(self.filtered_cst, "real_component")
            .unwrap();
        let imaginary_component_node = case_literal_pattern_complex_number_node
            .child_by_field_name(self.filtered_cst, "imaginary_component")
            .unwrap();
        let sign_node = case_literal_pattern_complex_number_node
            .child_by_field_name(self.filtered_cst, "sign")
            .unwrap();

        let (real_component, real_is_complex, _) =
            self.case_literal_integer_or_float(real_component_node)?;
        let (imaginary_component, imaginary_is_complex, imaginary_is_neg) =
            self.case_literal_integer_or_float(imaginary_component_node)?;

        let op = if sign_node.kind() == "+" {
            Operator::Add
        } else {
            // -
            Operator::Sub
        };

        // we do some additional validation to ensure that the specified value is a valid complex number
        // first part must be real, second must be complex (and positive)

        if real_is_complex {
            self.record_recoverable_error(
                RecoverableError::SyntaxError(
                    "first part of complex number must be real".to_string(),
                ),
                real_component_node,
            );
        }

        if imaginary_is_complex {
            if imaginary_is_neg {
                self.record_recoverable_error(
                    RecoverableError::SyntaxError(
                        "second part of complex number must be a positive".to_string(),
                    ),
                    imaginary_component_node,
                );
            }
        } else {
            self.record_recoverable_error(
                RecoverableError::SyntaxError(
                    "second part of complex number must be a imaginary".to_string(),
                ),
                imaginary_component_node,
            );
        }

        Ok(self.parser.new_expr(
            ExprDesc::BinOp {
                left: real_component,
                op,
                right: imaginary_component,
            },
            case_literal_pattern_complex_number_node,
        ))
    }

    // case_class_pattern: $ => choice(
    //  seq($.dotted_name, '(', ')'),
    //  seq($.dotted_name, '(', $.case_positional_patterns, optional(','), ')'),
    //  seq($.dotted_name, '(', $.case_keyword_patterns, optional(','), ')'),
    //  seq($.dotted_name, '(', $.case_positional_patterns, ',', $.case_keyword_patterns, optional(','), ')'),
    // ),
    fn case_class_pattern(&mut self, class_pattern_node: &Node) -> ErrorableResult<PatternDesc> {
        let dotted_name_node = class_pattern_node.child(self.filtered_cst, 0).unwrap();

        let cls: Expr = self.handle_class_pattern_name(dotted_name_node);
        let mut patterns: Vec<Pattern> = vec![];
        let mut kwd_attrs: Vec<String> = vec![];
        let mut kwd_patterns: Vec<Pattern> = vec![];

        let mut child_nodes = vec![];
        for child_node in class_pattern_node.named_children(self.filtered_cst) {
            child_nodes.push(child_node);
        }

        match child_nodes.len() {
            2 => {
                let second_node = child_nodes.get(1).unwrap();
                match second_node.kind() {
                    "case_positional_patterns" => {
                        self.case_positional_patterns(second_node, &mut patterns)
                    }
                    "case_keyword_patterns" => {
                        self.case_keyword_patterns(second_node, &mut kwd_attrs, &mut kwd_patterns)
                    }
                    _ => (),
                }
            }
            3 => {
                let case_positional_patterns_node = child_nodes.get(1).unwrap();
                let case_keyword_patterns_node = child_nodes.get(2).unwrap();

                self.case_positional_patterns(case_positional_patterns_node, &mut patterns);
                self.case_keyword_patterns(
                    case_keyword_patterns_node,
                    &mut kwd_attrs,
                    &mut kwd_patterns,
                )
            }
            _ => (),
        }

        Ok(PatternDesc::MatchClass {
            cls,
            patterns,
            kwd_attrs,
            kwd_patterns,
        })
    }

    fn wrap_dotted_name_into_attribute_access_for_case_patterns(
        &mut self,
        name_parts: Vec<&Node>,
        node: &Node,
    ) -> ExprDesc {
        let mut head_expression: Option<ExprDesc> = None;
        let mut prev_node: Option<&Node> = None;
        for part_node in name_parts {
            let part_as_string = self.get_valid_identifier(part_node);
            let ctx = ExprContext::Load;
            head_expression = Some(match head_expression {
                None => ExprDesc::Name {
                    id: part_as_string,
                    ctx,
                },
                Some(head_expression) => {
                    let head_as_expr = self.parser.new_expr_with_start_end_node(
                        head_expression,
                        node,
                        prev_node.unwrap(),
                    );

                    ExprDesc::Attribute {
                        value: head_as_expr,
                        attr: part_as_string,
                        ctx,
                    }
                }
            });
            prev_node = Some(part_node);
        }

        head_expression.unwrap()
    }

    fn handle_class_pattern_name(&mut self, dotted_name_node: &Node) -> Expr {
        // One element is translated to a Name node
        // More than one is translated into Attribute access pattern of Name nodes
        let mut name_parts = vec![];
        for part in dotted_name_node.named_children(self.filtered_cst) {
            name_parts.push(part);
        }

        let ctx = ExprContext::Load;

        let expr_desc = match name_parts.len() {
            1 => {
                let id = self.get_valid_identifier(name_parts.pop().unwrap());
                ExprDesc::Name { id, ctx }
            }
            _ => self.wrap_dotted_name_into_attribute_access_for_case_patterns(
                name_parts,
                dotted_name_node,
            ),
        };
        self.parser.new_expr(expr_desc, dotted_name_node)
    }

    // case_positional_patterns: $ => prec.left(commaSep1($.case_pattern)),
    fn case_positional_patterns(
        &mut self,
        positional_patterns_node: &Node,
        patterns: &mut Vec<Pattern>,
    ) {
        for case_pattern_node in positional_patterns_node.named_children(self.filtered_cst) {
            match self.case_pattern(case_pattern_node) {
                Ok(pattern) => patterns.push(pattern),
                _ => (),
            }
        }
    }

    // case_keyword_patterns: $ => prec.left(commaSep1($.case_keyword_pattern)),
    fn case_keyword_patterns(
        &mut self,
        keyword_patterns_node: &Node,
        kwd_attrs: &mut Vec<String>,
        kwd_patterns: &mut Vec<Pattern>,
    ) {
        for keyword_pattern_node in keyword_patterns_node.named_children(self.filtered_cst) {
            // we ignore the ErrorableResult here as it will have already been handled in `case_key_value_pattern`
            // we cal .ok() and discard the result here to keep the clippy linter quiet
            self.case_keyword_pattern(keyword_pattern_node, kwd_attrs, kwd_patterns)
                .ok();
        }
    }

    // case_keyword_pattern: $ => seq(field("name", $.identifier), "=", field("value", $.case_pattern)),
    fn case_keyword_pattern(
        &mut self,
        keyword_patterns_node: &Node,
        kwd_attrs: &mut Vec<String>,
        kwd_patterns: &mut Vec<Pattern>,
    ) -> ErrorableResult<()> {
        let key_node = &keyword_patterns_node
            .child_by_field_name(self.filtered_cst, "name")
            .expect("name node of keyword pattern");
        let value_node = &keyword_patterns_node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("value node of keyword pattern");

        kwd_attrs.push(self.get_valid_identifier(key_node));

        kwd_patterns.push(self.case_pattern(value_node)?);

        Ok(())
    }

    // case_mapping_pattern: $ => choice(
    //   seq('{', '}'),
    //   seq('{', $.case_double_star_pattern, optional(','), '}'),
    //   seq('{', $.case_items_pattern, $.case_double_star_pattern, optional(','), '}'),
    //   seq('{', $.case_items_pattern, '}'),
    // ),
    fn case_mapping_pattern(&mut self, group_pattern_node: &Node) -> ErrorableResult<PatternDesc> {
        let child_count = group_pattern_node.child_count();

        let mut keys: Vec<Expr> = vec![];
        let mut patterns: Vec<Pattern> = vec![];
        let mut rest: Option<String> = None;

        if child_count != 2 {
            let second_child = &group_pattern_node
                .child(self.filtered_cst, 1)
                .expect("first child of group mapping pattern node");
            let third_child = &group_pattern_node
                .child(self.filtered_cst, 2)
                .expect("second child of group mapping pattern node");

            match (second_child.kind(), third_child.kind()) {
                ("case_items_pattern", "case_double_star_pattern") => {
                    self.case_items_pattern(second_child, &mut keys, &mut patterns);
                    rest = Some(self.case_double_star_pattern(third_child));
                }
                ("case_double_star_pattern", _) => {
                    rest = Some(self.case_double_star_pattern(second_child));
                }
                ("case_items_pattern", _) => {
                    self.case_items_pattern(second_child, &mut keys, &mut patterns);
                }
                _ => (),
            }
        }

        Ok(PatternDesc::MatchMapping {
            keys,
            patterns,
            rest,
        })
    }

    // case_items_pattern: $ => seq(commaSep1($.case_key_value_pattern), optional(',')),
    fn case_items_pattern(
        &mut self,
        case_items_pattern_node: &Node,
        keys: &mut Vec<Expr>,
        patterns: &mut Vec<Pattern>,
    ) {
        for case_key_value_pattern_node in case_items_pattern_node.named_children(self.filtered_cst)
        {
            // we ignore the ErrorableResult here as it will have already been handled in `case_key_value_pattern`
            // we cal .ok() and discard the result here to keep the clippy linter quiet
            self.case_key_value_pattern(case_key_value_pattern_node, keys, patterns)
                .ok();
        }
    }

    // case_key_value_pattern: $ => seq(
    //  field("key", choice($.case_literal_pattern, $.dotted_name)), ':', field("value", $.case_pattern)
    // ),
    fn case_key_value_pattern<'a>(
        &mut self,
        case_items_pattern_node: &'a Node<'a>,
        keys: &mut Vec<Expr>,
        patterns: &mut Vec<Pattern>,
    ) -> ErrorableResult<()> {
        let key_node = case_items_pattern_node
            .child_by_field_name(self.filtered_cst, "key")
            .expect("key node of key value pattern");
        let value_node = &case_items_pattern_node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("value node of key value pattern");

        let key_expr = match key_node.kind() {
            "dotted_name" => {
                let mut name_parts = vec![];
                for part in key_node.named_children(self.filtered_cst) {
                    name_parts.push(part);
                }

                let expr_desc = self
                    .wrap_dotted_name_into_attribute_access_for_case_patterns(name_parts, key_node);
                self.parser.new_expr(expr_desc, key_node)
            }
            _ => {
                let key_node_child = &key_node.child(self.filtered_cst, 0).unwrap();

                match key_node_child.kind() {
                    "case_literal_pattern_complex_number" => {
                        self.case_literal_pattern_complex_number(key_node_child)?
                    }
                    "case_literal_integer_or_float" => {
                        self.case_literal_integer_or_float(key_node_child)?.0
                    }
                    _ => self.expression(key_node_child)?,
                }
            }
        };
        keys.push(key_expr);

        patterns.push(self.case_pattern(value_node)?);

        Ok(())
    }

    fn case_double_star_pattern(&mut self, group_pattern_node: &Node) -> String {
        let identifier = &group_pattern_node
            .child(self.filtered_cst, 1)
            .expect("identifier");
        self.get_valid_identifier(identifier)
    }

    // case_group_pattern: $ => seq( '(', field("case_pattern", $.case_pattern), ')'),
    fn case_group_pattern(&mut self, group_pattern_node: &Node) -> ErrorableResult<Pattern> {
        let case_pattern_node = &group_pattern_node
            .child_by_field_name(self.filtered_cst, "case_pattern")
            .expect("case_pattern of case_group_pattern");
        self.case_pattern(case_pattern_node)
    }

    // case_sequence_pattern: $ => choice(
    //    seq('[', optional($.case_maybe_sequence_pattern), ']'),
    //    seq('(', optional($.case_open_sequence_pattern), ')'),
    // ),
    fn case_sequence_pattern(
        &mut self,
        sequence_pattern_node: &Node,
    ) -> ErrorableResult<PatternDesc> {
        if sequence_pattern_node.child_count() == 3 {
            let maybe_or_open_sequence_pattern_node =
                &sequence_pattern_node.child(self.filtered_cst, 1).unwrap();
            match maybe_or_open_sequence_pattern_node.kind() {
                "case_maybe_sequence_pattern" => {
                    let mut patterns = vec![];

                    self.case_maybe_sequence_pattern(
                        maybe_or_open_sequence_pattern_node,
                        &mut patterns,
                    );
                    Ok(PatternDesc::MatchSequence(patterns))
                }
                _ => {
                    // "case_open_sequence_pattern"
                    self.case_open_sequence_pattern(maybe_or_open_sequence_pattern_node)
                }
            }
        } else {
            Ok(PatternDesc::MatchSequence(vec![]))
        }
    }

    // case_open_sequence_pattern: $ => seq(
    //  field("maybe_star", $.case_maybe_star_pattern), ',', field("maybe_sequence", optional($.case_maybe_sequence_pattern))
    // ),
    fn case_open_sequence_pattern(
        &mut self,
        open_sequence_pattern_node: &Node,
    ) -> ErrorableResult<PatternDesc> {
        let mut patterns = vec![];

        let maybe_star_pattern_node = open_sequence_pattern_node
            .child_by_field_name(self.filtered_cst, "maybe_star")
            .unwrap();

        let pattern = self.case_maybe_star_pattern(maybe_star_pattern_node);
        if let Ok(pattern) = pattern {
            // if not OK then this is fine as error already reported
            patterns.push(pattern);
        }

        if let Some(maybe_maybe_sequence_pattern_node) =
            open_sequence_pattern_node.child_by_field_name(self.filtered_cst, "maybe_sequence")
        {
            self.case_maybe_sequence_pattern(maybe_maybe_sequence_pattern_node, &mut patterns)
        }

        Ok(PatternDesc::MatchSequence(patterns))
    }

    // case_maybe_sequence_pattern: $ => prec.left(seq(commaSep1($.case_maybe_star_pattern), optional(','))),
    fn case_maybe_sequence_pattern(
        &mut self,
        maybe_sequence_pattern_node: &Node,
        patterns: &mut Vec<Pattern>,
    ) {
        for maybe_star_pattern_node in maybe_sequence_pattern_node.named_children(self.filtered_cst)
        {
            let pattern = self.case_maybe_star_pattern(maybe_star_pattern_node);
            match pattern {
                Ok(pattern) => {
                    patterns.push(pattern);
                }
                _ => (), // error already reported
            }
        }
    }

    // case_maybe_star_pattern: $ => prec.left(choice(
    //   $.case_star_pattern,
    //   $.case_pattern
    // )),
    fn case_maybe_star_pattern(
        &mut self,
        maybe_sequence_pattern_node: &Node,
    ) -> ErrorableResult<Pattern> {
        let one_child = &maybe_sequence_pattern_node
            .child(self.filtered_cst, 0)
            .unwrap();
        match one_child.kind() {
            "case_star_pattern" => Ok(self.case_star_pattern(one_child)),
            _ => self.case_pattern(one_child),
        }
    }

    // case_star_pattern: $ => choice(
    //  seq('*', $.identifier),
    //  seq('*', $.case_wildcard_pattern)
    // ),
    fn case_star_pattern(&mut self, star_pattern_node: &Node) -> Pattern {
        let one_child = &star_pattern_node
            .child(self.filtered_cst, 1)
            .expect("case_star_pattern child node");

        let pattern_desc = match one_child.kind() {
            "identifier" => {
                let name = Some(self.get_valid_identifier(one_child));
                PatternDesc::MatchStar(name)
            }
            _ => PatternDesc::MatchStar(None), // case_wildcard_pattern
        };
        self.parser.new_pattern(pattern_desc, star_pattern_node)
    }

    // decorated_definition: $ => seq(
    //   repeat1($.decorator),
    //   field('definition', choice(
    //     $.class_definition,
    //     $.function_definition
    //   ))
    // ),
    fn decorated_definition<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Stmt> {
        use ProductionKind::*;

        // resolves to a class definition or funcdef
        let mut decorator_list: Vec<Expr> = vec![];

        for child in node.named_children(self.filtered_cst) {
            let node_type = get_node_type(child);
            match &node_type {
                NodeType::Production(production) => match &production.production_kind {
                    FUNCTION_DEFINITION => {
                        let func_def = self.function_definition(child, decorator_list)?;
                        return Ok(Stmt::new(func_def, child, child));
                    }
                    CLASS_DEFINITION => {
                        let class_def = self.class_definition(child, decorator_list)?;
                        return Ok(Stmt::new(class_def, child, child));
                    }
                    DECORATOR => {
                        // decorator
                        let dec_expr_node = child
                            .child(self.filtered_cst, 1)
                            .expect("dectorator missing elaboration");
                        let dec_expr = self.expression(dec_expr_node)?;
                        decorator_list.push(dec_expr);
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        Err(self.record_recoverable_error(RecoverableError::MissingChild, node))
    }

    // global_statement: $ => seq(
    //   'global',
    //   commaSep1($.identifier)
    // ),
    fn global_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut identifiers = vec![];
        self.parse_identifiers(node, &mut identifiers)?;
        Ok(StmtDesc::Global(identifiers))
    }

    // nonlocal_statement: $ => seq(
    //   'nonlocal',
    //   commaSep1($.identifier)
    // ),
    fn nonlocal_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut identifiers = vec![];
        self.parse_identifiers(node, &mut identifiers)?;

        Ok(StmtDesc::Nonlocal(identifiers))
    }

    fn parse_identifiers<'a>(
        &mut self,
        node: &'a Node<'a>,
        identifiers: &mut Vec<String>,
    ) -> ErrorableResult<()> {
        for child in node.named_children(self.filtered_cst) {
            let identifier = self.get_valid_identifier(child);
            identifiers.push(identifier);
        }
        Ok(())
    }

    // Process Function Definition
    //
    // function_definition: $ => seq(
    //   optional('async'),
    //   'def',
    //   field('name', $.identifier),
    //   field('parameters', $.parameters),
    //   optional(
    //     seq(
    //       '->',
    //       field('return_type', $.type)
    //     )
    //   ),
    //   ':',
    //   field('body', $._suite)
    // ),
    fn function_definition<'a>(
        &mut self,
        func_def: &'a Node<'a>,
        decorator_list: Vec<Expr>,
    ) -> ErrorableResult<StmtDesc> {
        let name_node = func_def
            .child_by_field_name(self.filtered_cst, "name")
            .expect("missing function name");
        let name = self.get_valid_identifier(name_node);
        let parameters_node = func_def
            .child_by_field_name(self.filtered_cst, "parameters")
            .expect("missing function parameters");
        let parameters = self.get_parameters(parameters_node)?;
        let body_node = func_def
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing function body");
        let mut body = vec![];
        self.block(body_node, &mut body);

        let return_annotation_node = func_def.child_by_field_name(self.filtered_cst, "return_type");
        let return_annotation_expr = match &return_annotation_node {
            Some(ret_annotation) => {
                let annotation_node = ret_annotation
                    .child(self.filtered_cst, 0)
                    .expect("type node missing type");
                Some(self.expression(annotation_node)?)
            }
            _ => None,
        };

        if self.get_text(
            func_def
                .child(self.filtered_cst, 0)
                .expect("def or async node expected"),
        ) == "async"
        {
            Ok(StmtDesc::AsyncFunctionDef {
                name,
                args: parameters,
                body,
                decorator_list,      // decorators are added by wrapping code
                type_params: vec![], // TODO: pep 695
                returns: return_annotation_expr,
                type_comment: None,
            })
        } else {
            Ok(StmtDesc::FunctionDef {
                name,
                args: parameters,
                body,
                decorator_list,      // decorators are added by wrapping code
                type_params: vec![], // TODO: pep 695
                returns: return_annotation_expr,
                type_comment: None,
            })
        }
    }

    // Load the function parameters
    //
    // parameters: $ => seq(
    //   '(',
    //   optional($._parameters),
    //   ')'
    // ),
    // _parameters: $ => seq(
    //   commaSep1($.parameter),
    //   optional(',')
    // ),
    // parameter: $ => choice(
    //   $.identifier,
    //   $.typed_parameter,
    //   $.default_parameter,
    //   $.typed_default_parameter,
    //   $.list_splat_pattern,
    //   $.tuple_pattern,
    //   $.keyword_separator,
    //   $.positional_separator,
    //   $.dictionary_splat_pattern
    // ),
    fn get_parameters<'a>(&mut self, parameters: &'a Node<'a>) -> ErrorableResult<Arguments> {
        use ProductionKind::*;

        let mut posonlyargs: Vec<Arg> = vec![];
        let mut args: Vec<Arg> = vec![];
        let mut vararg: Option<Arg> = None;
        let mut kwonlyargs: Vec<Arg> = vec![]; // arguments go in here after a vararg or / token
        let mut kw_defaults: Vec<Option<Expr>> = vec![]; //defaults go here after a vararg  or * token
        let mut kwarg: Option<Arg> = None;
        let mut defaults: Vec<Expr> = vec![];

        let mut require_kw_args = false;

        for parameter in parameters.named_children(self.filtered_cst) {
            let parameter_annotation = get_node_type(parameter);
            match &parameter_annotation {
                NodeType::Production(param) => match &param.production_kind {
                    IDENTIFIER => {
                        self.get_parameters_identifier(
                            param.node,
                            &require_kw_args,
                            &mut kwonlyargs,
                            &mut kw_defaults,
                            &mut args,
                        );
                    }
                    TYPED_PARAMETER => {
                        self.get_parameters_typed_parameter(
                            param.node,
                            parameter,
                            &mut require_kw_args,
                            &mut kwonlyargs,
                            &mut kw_defaults,
                            &mut args,
                            &mut vararg,
                            &mut kwarg,
                        )?;
                    }
                    DEFAULT_PARAMETER => {
                        self.get_parameters_default_parameter(
                            param.node,
                            &require_kw_args,
                            &mut kwonlyargs,
                            &mut kw_defaults,
                            &mut args,
                            &mut defaults,
                        )?;
                    }
                    TYPED_DEFAULT_PARAMETER => {
                        self.get_parameters_typed_default_parameter(
                            param.node,
                            &require_kw_args,
                            &mut kwonlyargs,
                            &mut kw_defaults,
                            &mut args,
                            &mut defaults,
                        )?;
                    }
                    LIST_SPLAT_PATTERN => {
                        let ident_node = &param
                            .node
                            .child(self.filtered_cst, 1)
                            .expect("identifier of starred missing");
                        let identifier = self.get_valid_identifier(ident_node);

                        vararg = Some(Arg::new_simple(identifier, ident_node, parameter));
                        require_kw_args = true;
                    }
                    TUPLE_PATTERN => panic!("unimplemented token in get_parameters: TUPLE_PATTERN"),
                    KEYWORD_SEPARATOR => {
                        // all arguments defined past this point are now keyword args
                        require_kw_args = true;
                    }
                    POSITIONAL_SEPARATOR => {
                        // everything declared as an arugment now becomes a posonlyargs
                        while !args.is_empty() {
                            posonlyargs.push(args.remove(0));
                        }
                    }
                    DICTIONARY_SPLAT_PATTERN => {
                        let ident_node = &param
                            .node
                            .child(self.filtered_cst, 1)
                            .expect("identifier of dictionary argument");
                        let identifier = self.get_valid_identifier(ident_node);

                        kwarg = Some(Arg::new_simple(identifier, ident_node, parameter));
                    }
                    _ => {
                        return Err(self.record_recoverable_error(
                            RecoverableError::UnexpectedExpression(format!(
                                "unexpected function parameter: {:?}",
                                param
                            )),
                            parameter,
                        ));
                    }
                },
                _ => (),
            }
        }

        Ok(Arguments {
            posonlyargs,
            args,
            vararg,
            kwonlyargs,
            kw_defaults,
            kwarg,
            defaults,
        })
    }

    // identifier: $ => /[_\p{XID_Start}][_\p{XID_Continue}]*/,
    fn get_parameters_identifier<'a>(
        &mut self,
        node: &'a Node<'a>,
        require_kw_args: &bool,
        kwonlyargs: &mut Vec<Arg>,
        kw_defaults: &mut Vec<Option<Expr>>,
        args: &mut Vec<Arg>,
    ) {
        let identifier = self.get_valid_identifier(node);

        let arg = Arg::new_simple(identifier, node, node);

        match require_kw_args {
            true => {
                kwonlyargs.push(arg);
                kw_defaults.push(None);
            }
            _ => args.push(arg),
        };
    }

    // typed_parameter: $ => prec(PREC.typed_parameter, seq(
    //   choice(
    //     $.identifier,
    //     $.list_splat_pattern,
    //     $.dictionary_splat_pattern
    //   ),
    //   ':',
    //   field('type', $.type)
    // )),
    fn get_parameters_typed_parameter<'a>(
        &mut self,
        node: &'a Node<'a>,
        parameter: &'a Node<'a>,
        require_kw_args: &mut bool,
        kwonlyargs: &mut Vec<Arg>,
        kw_defaults: &mut Vec<Option<Expr>>,
        args: &mut Vec<Arg>,
        vararg: &mut Option<Arg>,
        kwarg: &mut Option<Arg>,
    ) -> ErrorableResult<()> {
        use ProductionKind::*;

        let typed_parameter_node = node
            .child_by_field_name(self.filtered_cst, "type")
            .expect("default param missing type");
        let annotation_node = typed_parameter_node
            .child(self.filtered_cst, 0)
            .expect("type node missing type");

        let annotation_expr = self.expression(annotation_node)?;

        let ident_node = node
            .child(self.filtered_cst, 0)
            .expect("typed param id, *id or **id missing");
        let ident_node_type = get_node_type(ident_node);
        match ident_node_type {
            NodeType::Production(param) => match &param.production_kind {
                IDENTIFIER => {
                    let identifier = self.get_valid_identifier(param.node);
                    let arg = Arg::new_with_type(identifier, annotation_expr, parameter, parameter);
                    match require_kw_args {
                        true => {
                            kwonlyargs.push(arg);
                            kw_defaults.push(None);
                        }
                        _ => args.push(arg),
                    };
                }
                LIST_SPLAT_PATTERN => {
                    let ident_node = &param
                        .node
                        .child(self.filtered_cst, 1)
                        .expect("identifier of starred missing");
                    let identifier = self.get_valid_identifier(ident_node);

                    *vararg = Some(Arg::new_with_type(
                        identifier,
                        annotation_expr,
                        ident_node,
                        parameter,
                    ));
                    // all arguments defined past this point are now keyword args
                    *require_kw_args = true;
                }
                DICTIONARY_SPLAT_PATTERN => {
                    let ident_node = &param
                        .node
                        .child(self.filtered_cst, 1)
                        .expect("identifier of dictionary argument");
                    let identifier = self.get_valid_identifier(ident_node);

                    *kwarg = Some(Arg::new_with_type(
                        identifier,
                        annotation_expr,
                        ident_node,
                        parameter,
                    ));
                }
                _ => panic!("unexpected typed parameter production"),
            },
            _ => (),
        }
        Ok(())
    }

    // default_parameter: $ => seq(
    //   field('name', $.identifier),
    //   '=',
    //   field('value', $.expression)
    // ),
    fn get_parameters_default_parameter<'a>(
        &mut self,
        node: &'a Node<'a>,
        require_kw_args: &bool,
        kwonlyargs: &mut Vec<Arg>,
        kw_defaults: &mut Vec<Option<Expr>>,
        args: &mut Vec<Arg>,
        defaults: &mut Vec<Expr>,
    ) -> ErrorableResult<()> {
        let name_node = &node
            .child_by_field_name(self.filtered_cst, "name")
            .expect("default param missing name");

        let identifier = self.get_valid_identifier(name_node);
        let arg = Arg::new_simple(identifier, name_node, name_node);

        let default_value_node = &node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("default param missing value");
        let default_value = self.expression(default_value_node)?;

        match require_kw_args {
            true => {
                kwonlyargs.push(arg);
                kw_defaults.push(Some(default_value));
            }
            _ => {
                args.push(arg);
                defaults.push(default_value);
            }
        };

        Ok(())
    }

    // typed_default_parameter: $ => prec(PREC.typed_parameter, seq(
    //   field('name', $.identifier),
    //   ':',
    //   field('type', $.type),
    //   '=',
    //   field('value', $.expression)
    // )),
    fn get_parameters_typed_default_parameter<'a>(
        &mut self,
        node: &'a Node<'a>,
        require_kw_args: &bool,
        kwonlyargs: &mut Vec<Arg>,
        kw_defaults: &mut Vec<Option<Expr>>,
        args: &mut Vec<Arg>,
        defaults: &mut Vec<Expr>,
    ) -> ErrorableResult<()> {
        let name_node = &node
            .child_by_field_name(self.filtered_cst, "name")
            .expect("typed default param missing name");

        let typed_default_parameter_node = &node
            .child_by_field_name(self.filtered_cst, "type")
            .expect("typed default param missing name");
        let annotation_node = typed_default_parameter_node
            .child(self.filtered_cst, 0)
            .expect("type node missing type");

        let default_value_node = &node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("typed default param missing name");

        let annotation_expr = self.expression(annotation_node)?;

        let identifier = self.get_valid_identifier(name_node);

        let arg = Arg::new_with_type(
            identifier,
            annotation_expr,
            name_node,
            typed_default_parameter_node,
        );

        let default_value = self.expression(default_value_node)?;

        match require_kw_args {
            true => {
                kwonlyargs.push(arg);
                kw_defaults.push(Some(default_value));
            }
            _ => {
                args.push(arg);
                defaults.push(default_value);
            }
        };

        Ok(())
    }

    // Process a Class Definition
    //
    // class_definition: $ => seq(
    //   'class',
    //   field('name', $.identifier),
    //   field('superclasses', optional($.argument_list)),
    //   ':',
    //   field('body', $._suite)
    // ),
    fn class_definition<'a>(
        &mut self,
        class_def: &'a Node<'a>,
        decorator_list: Vec<Expr>,
    ) -> ErrorableResult<StmtDesc> {
        let name_node = class_def
            .child_by_field_name(self.filtered_cst, "name")
            .expect("missing class name");
        let name = self.get_valid_identifier(name_node);
        let body_node = class_def
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing class body");
        let mut body = vec![];
        self.block(body_node, &mut body);

        let mut bases: Vec<Expr> = vec![];
        let mut keywords: Vec<AstKeyword> = vec![];

        match class_def.child_by_field_name(self.filtered_cst, "superclasses") {
            Some(superclasses_node) => {
                self.argument_list(superclasses_node, &mut bases, &mut keywords)?;
            }
            _ => (),
        }

        Ok(StmtDesc::ClassDef {
            name,
            bases,
            keywords,
            body,
            decorator_list,
            type_params: vec![], // TODO: pep 695
        })
    }

    // assert_statement: $ => seq(
    //   'assert',
    //   commaSep1($.expression)
    // ),
    fn assert_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let test_node = node.child(self.filtered_cst, 1).unwrap();
        let test = self.expression(test_node)?;

        let mut msg = None;
        if node.child_count() == 4 {
            let msg_node = node.child(self.filtered_cst, 3).unwrap();
            msg = Some(self.expression(msg_node)?);
        }

        Ok(StmtDesc::Assert { test, msg })
    }

    fn dotted_name_to_string<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<String> {
        Ok(join(
            node.named_children(self.filtered_cst)
                .map(|x| self.get_valid_identifier(x)),
            ".",
        ))
    }

    fn get_aliases<'a>(
        &mut self,
        node: &'a Node<'a>,
        aliases: &mut Vec<Alias>,
    ) -> ErrorableResult<()> {
        for alias_child in node.named_children(self.filtered_cst) {
            match alias_child.child_by_field_name(self.filtered_cst, "alias") {
                Some(alias_name) => {
                    aliases.push(Alias::new(
                        self.dotted_name_to_string(
                            alias_child
                                .child_by_field_name(self.filtered_cst, "name")
                                .expect("missing aliased_import name"),
                        )?,
                        Some(self.get_valid_identifier(alias_name)),
                        alias_child,
                    ));
                }
                _ => {
                    // straight dotted name: a.b.c etc
                    aliases.push(Alias::new(
                        self.dotted_name_to_string(alias_child)?,
                        None,
                        alias_child,
                    ));
                }
            }
        }
        Ok(())
    }

    // import_statement: $ => seq(
    //   'import',
    //   $._import_list
    // ),
    // aliased_import: $ => seq(
    //   field('name', $.dotted_name),
    //   'as',
    //   field('alias', $.identifier)
    // ),
    fn import_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut aliases: Vec<Alias> = vec![];
        self.get_aliases(node, &mut aliases)?;
        Ok(StmtDesc::Import(aliases))
    }

    // import_from_statement: $ => seq(
    //   'from',
    //   field('module_name', choice(
    //     $.relative_import,
    //     $.dotted_name
    //   )),
    //   'import',
    //   choice(
    //     $.wildcard_import,
    //     $._import_list,
    //     seq('(', $._import_list, ')')
    //   )
    // ),
    //
    // relative_import: $ => seq(
    //  $.import_prefix,
    //  optional($.dotted_name)
    //),
    fn import_from_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut names: Vec<Alias> = vec![];
        let mut level: isize = 0;

        let module_name_node = node
            .child_by_field_name(self.filtered_cst, "module_name")
            .expect("import_from_statement missing module_name");
        let module__ = match module_name_node.kind() {
            "relative_import" => {
                // Relative imports are interesting. From the docs: "level is
                // an integer holding the level of the relative import (0
                // means absolute import)." It can be thought of like
                // directories in a filesystem where by the number of dots
                // preceding a dotted name indicates how many levels upwards
                // one must look for the import dependency

                let import_prefix = module_name_node
                    .child(self.filtered_cst, 0)
                    .expect("import_prefix");
                level = import_prefix.child_count() as isize;

                if module_name_node.child_count() == 2 {
                    let dotted_name = module_name_node
                        .child(self.filtered_cst, 1)
                        .expect("dotted_name");
                    Some(self.dotted_name_to_string(dotted_name)?)
                } else {
                    None
                }
            }
            _ => Some(self.dotted_name_to_string(module_name_node)?),
        };

        let aliases_or_wildcard = &node
            .child(self.filtered_cst, 3)
            .expect("list of imports for import_from_statement");
        match aliases_or_wildcard.kind() {
            "wildcard_import" => {
                names.push(Alias::new(String::from("*"), None, aliases_or_wildcard))
            }
            _ => {
                for alias in node.named_children(self.filtered_cst) {
                    if alias == module_name_node {
                        continue; // skip the `from xyz`, `xzy` node as processed already
                    }
                    match alias.child_by_field_name(self.filtered_cst, "alias") {
                        Some(alias_name) => {
                            names.push(Alias::new(
                                self.dotted_name_to_string(
                                    alias
                                        .child_by_field_name(self.filtered_cst, "name")
                                        .expect("missing aliased_import name"),
                                )?,
                                Some(self.get_valid_identifier(alias_name)),
                                alias,
                            ));
                        }
                        _ => {
                            // straight dotted name: a.b.c etc
                            names.push(Alias::new(self.dotted_name_to_string(alias)?, None, alias));
                        }
                    }
                }
            }
        }

        Ok(StmtDesc::ImportFrom {
            module__,
            names,
            level: Some(level), // optional but always included
        })
    }

    // future_import_statement: $ => seq(
    //   'from',
    //   '__future__',
    //   'import',
    //   choice(
    //     $._import_list,
    //     seq('(', $._import_list, ')'),
    //   )
    // ),
    fn future_import_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut names: Vec<Alias> = vec![];
        self.get_aliases(node, &mut names)?;

        Ok(StmtDesc::ImportFrom {
            module__: Some("__future__".to_string()),
            names,
            level: Some(0), // optional but always included
        })
    }

    // expressions: $ => choice(
    //  $.expression,
    //  $.expression_list
    // ),
    fn expressions<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        match node.kind() {
            "expression_list" => {
                let mut expressions: Vec<Expr> = vec![];
                self.expression_list(node, &mut expressions);

                let tuple_desc = ExprDesc::Tuple {
                    elts: expressions,
                    ctx: self.get_expression_context(),
                };

                Ok(self.parser.new_expr(tuple_desc, node))
            }
            _ => self.expression(node),
        }
    }

    fn expression_list<'a>(&mut self, node: &'a Node<'a>, expressions: &mut Vec<Expr>) {
        for child in node.named_children(self.filtered_cst) {
            // it is ok to leave out a subexpression if there is a problem with it
            match self.expression(child) {
                Ok(arg) => expressions.push(arg),
                _ => (),
            };
        }
    }

    // Process Return StmtDesc
    //
    // return_statement: $ => seq(
    //   'return',
    //   optional($._expressions)
    // ),
    fn return_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut expr = None;
        if node.child_count() == 2 {
            expr = Some(self.expressions(node.child(self.filtered_cst, 1).unwrap())?);
        }

        Ok(StmtDesc::Return(expr))
    }

    // raise_statement: $ => seq(
    //   'raise',
    //   optional($._expressions),
    //   optional(seq('from', field('cause', $.expression)))
    // ),
    fn raise_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut exc = None;
        let mut cause = None;
        match node.child_by_field_name(self.filtered_cst, "cause") {
            Some(from_node) => {
                let expr_node = node.child(self.filtered_cst, 1).unwrap();
                exc = Some(self.expression(expr_node)?);

                cause = Some(self.expression(from_node)?);
            }
            _ => match node.child(self.filtered_cst, 1) {
                Some(expr_node) => {
                    exc = Some(self.expression(expr_node)?);
                }
                _ => (),
            },
        }

        Ok(StmtDesc::Raise { exc, cause })
    }

    // Process an ExprDesc StmtDesc.
    // expression_statement: $ => choice(
    //   $.expression,
    //   seq(commaSep1($.expression), optional(',')),
    //   $.assignment,
    //   $.augmented_assignment,
    //   $.yield
    // ),
    fn expression_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        use ProductionKind::*;

        let expression_statement = if 1 == node.child_count() {
            let child_expression = node.child(self.filtered_cst, 0).ok_or_else(|| {
                self.record_recoverable_error(RecoverableError::MissingChild, node)
            })?;
            let child_expression_type = get_node_type(child_expression);
            match child_expression_type {
                NodeType::Production(ref rule) => {
                    match &rule.production_kind {
                        ASSIGNMENT => {
                            let (mut targets, type_annot, value, type_comment, simple) =
                                self.assignment(rule.node)?;

                            if let Some(annotation) = type_annot {
                                StmtDesc::AnnAssign {
                                    target: targets.pop().unwrap(),
                                    annotation,
                                    value,
                                    simple,
                                }
                            } else {
                                StmtDesc::Assign {
                                    targets,
                                    value: value.unwrap(),
                                    type_comment,
                                }
                            }
                        }
                        AUGMENTED_ASSIGNMENT => self.aug_assign(rule.node)?,
                        YIELD => {
                            let yeild_desc = self.yield_statement(rule.node)?;
                            StmtDesc::Expr(self.parser.new_expr(yeild_desc, node))
                        }
                        _ => {
                            let expression = self.expression(child_expression)?;

                            // If the expression statement has a trailing comma we
                            // should treat it as a tuple of size one, otherwise
                            // it is a plain expression.
                            let ends_in_comma = node
                                .child(self.filtered_cst, node.child_count() - 1)
                                .unwrap()
                                .kind()
                                == ",";
                            if ends_in_comma {
                                let mut expressions = vec![];
                                expressions.push(expression);
                                let tuple_desc = ExprDesc::Tuple {
                                    elts: expressions,
                                    ctx: self.get_expression_context(),
                                };

                                StmtDesc::Expr(self.parser.new_expr(tuple_desc, node))
                            } else {
                                StmtDesc::Expr(expression)
                            }
                        }
                    }
                }
                _ => panic!("should be unreachable for expression"),
            }
        } else {
            // sequence of expressions: seq(commaSep1($.expression), optional(',')),
            let tuple_desc = self.tuple(node)?;
            StmtDesc::Expr(self.parser.new_expr(tuple_desc, node))
        };

        Ok(expression_statement)
    }

    // Delete(expr* targets)
    // delete_statement: $ => seq(
    //   'del',
    //   $._expressions
    // ),
    fn delete_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut expressions: Vec<Expr> = vec![];

        self.set_expression_context(ExprContext::Del);

        let expressions_node = node.child(self.filtered_cst, 1).unwrap();

        match expressions_node.kind() {
            "expression_list" => self.expression_list(expressions_node, &mut expressions),
            _ => {
                let expression = self.expression(expressions_node)?;
                expressions.push(expression);
            }
        };

        self.pop_expression_context();

        Ok(StmtDesc::Delete(expressions))
    }

    // for_statement: $ => seq(
    //   optional('async'),
    //   'for',
    //   field('left', $._left_hand_side),
    //   'in',
    //   field('right', $._expressions),
    //   ':',
    //   field('body', $._suite),
    //   field('alternative', optional($.else_clause))
    // ),
    fn for_statement<'a>(&mut self, for_node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        self.set_expression_context(ExprContext::Store);
        let target_node = for_node
            .child_by_field_name(self.filtered_cst, "left")
            .expect("missing left in for statement");
        let target = self.assign_left_hand_side(target_node)?;
        self.pop_expression_context();

        let iter_node = for_node
            .child_by_field_name(self.filtered_cst, "right")
            .expect("missing right in for statement");
        let iter = self.expressions(iter_node)?;

        let body_node = for_node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing body in for statement");
        let mut body_block = vec![];
        self.block(body_node, &mut body_block);

        let mut orelse_block = vec![];

        let orelse_node = for_node.child_by_field_name(self.filtered_cst, "alternative");
        match &orelse_node {
            Some(orelse_cont) => {
                match &orelse_cont.child_by_field_name(self.filtered_cst, "body") {
                    Some(body_cont) => self.block(body_cont, &mut orelse_block),
                    _ => (),
                }
            }
            _ => (),
        }
        if for_node
            .child(self.filtered_cst, 0)
            .unwrap()
            .kind()
            .eq("async")
        {
            Ok(StmtDesc::AsyncFor {
                target,
                iter,
                body: body_block,
                orelse: orelse_block,
                type_comment: None,
            })
        } else {
            Ok(StmtDesc::For {
                target,
                iter,
                body: body_block,
                orelse: orelse_block,
                type_comment: None,
            })
        }
    }

    fn process_withitem_as_pattern_or_expression<'a>(
        &mut self,
        items: &mut Vec<Withitem>,
        pattern_or_expression: &'a Node<'a>,
    ) -> ErrorableResult<()> {
        let mut optional_vars: Option<Expr> = None;
        let pattern_or_expression_type = &get_node_type(pattern_or_expression);

        match pattern_or_expression_type {
            NodeType::Production(rule) => match &rule.production_kind {
                ProductionKind::AS_PATTERN => {
                    let node = rule.node;
                    let lhs_expression = pattern_or_expression
                        .child(self.filtered_cst, 0)
                        .expect("expression for with_item");
                    let target_expression = node
                        .child(self.filtered_cst, 2)
                        .expect("target for with_item")
                        .child(self.filtered_cst, 0)
                        .expect("pattern target");

                    self.set_expression_context(ExprContext::Store);
                    optional_vars = Some(self.expression(target_expression)?);
                    self.pop_expression_context();

                    let context_expr: Expr = self.expression(lhs_expression)?;

                    items.push(Withitem {
                        context_expr,
                        optional_vars,
                    });
                }
                _ => {
                    if pattern_or_expression.child_count() >= 5 {
                        let sub_expression = pattern_or_expression
                            .child(self.filtered_cst, 4)
                            .expect("target for with_item");
                        if sub_expression.kind() == "as_pattern" {
                            let target_expression = sub_expression
                                .child(self.filtered_cst, 2)
                                .expect("target for with_item as")
                                .child(self.filtered_cst, 0)
                                .expect("pattern target");
                            self.set_expression_context(ExprContext::Store);
                            optional_vars = Some(self.expression(target_expression)?);
                            self.pop_expression_context();
                        }
                    }
                    let context_expr: Expr = self.expression(pattern_or_expression)?;
                    items.push(Withitem {
                        context_expr,
                        optional_vars,
                    });
                }
            },

            _ => (),
        }

        Ok(())
    }

    // with_statement: $ => seq(
    //   optional('async'),
    //   'with',
    //   $.with_clause,
    //   ':',
    //   field('body', $._suite)
    // ),
    //
    // with_clause: $ => choice(
    //   seq(commaSep1($.with_item), optional(',')),
    //   seq('(', commaSep1($.with_item), optional(','), ')')
    // ),
    //
    // with_item: $ => prec.dynamic(1, seq(
    //   field('value', $.expression),
    // )),
    fn with_statement<'a>(&mut self, with_node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let is_async: bool = with_node
            .child(self.filtered_cst, 0)
            .unwrap()
            .kind()
            .eq("async");
        let body_node = with_node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing body in with statement");
        let mut body = vec![];
        self.block(body_node, &mut body);

        let mut items: Vec<Withitem> = vec![];

        let with_clause_node_idx = if is_async { 2 } else { 1 };
        let with_clause_node = with_node
            .child(self.filtered_cst, with_clause_node_idx)
            .unwrap();

        for with_item_node in with_clause_node.named_children(self.filtered_cst) {
            let expression_node = &with_item_node
                .child(self.filtered_cst, 0)
                .expect("with_item to wrap an expression or as_pattern");

            let expression_node_type = &get_node_type(expression_node);

            match expression_node_type {
                NodeType::Production(rule) => match &rule.production_kind {
                    ProductionKind::TUPLE => {
                        for tuple_child_node in expression_node.named_children(self.filtered_cst) {
                            match self.process_withitem_as_pattern_or_expression(
                                &mut items,
                                tuple_child_node,
                            ) {
                                Ok(_) => (),
                                Err(error) => return Err(error),
                            }
                        }
                    }
                    _ => {
                        self.process_withitem_as_pattern_or_expression(
                            &mut items,
                            expression_node,
                        )?;
                    }
                },
                _ => {
                    self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!("{:?}", expression_node)),
                        expression_node,
                    );
                }
            };
        }

        if is_async {
            Ok(StmtDesc::AsyncWith {
                items,
                body,
                type_comment: None,
            })
        } else {
            Ok(StmtDesc::With {
                items,
                body,
                type_comment: None,
            })
        }
    }

    // try_statement: $ => seq(
    //   'try',
    //   ':',
    //   field('body', $._suite),
    //   choice(
    //     seq(
    //       repeat1($.except_clause),
    //       optional($.else_clause),
    //       optional($.finally_clause)
    //     ),
    //     seq(
    //       repeat1($.except_group_clause),
    //       optional($.else_clause),
    //       optional($.finally_clause)
    //     ),
    //     $.finally_clause
    //   )
    // ),
    //
    // except_clause: $ => seq(
    //   'except',
    //   optional(seq(
    //     $.expression,
    //     optional(seq(
    //       choice('as', ','),
    //       $.expression
    //     ))
    //   )),
    //   ':',
    //   $._suite
    // ),
    //
    // except_group_clause: $ => seq(
    //   'except*',
    //   seq(
    //     $.expression,
    //     optional(seq(
    //       'as',
    //       $.expression
    //     ))
    //   ),
    //   ':',
    //   $._suite
    // ),
    //
    // finally_clause: $ => seq(
    //   'finally',
    //   ':',
    //   $._suite
    // ),
    fn try_statement<'a>(&mut self, try_node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let mut body: Vec<Stmt> = vec![];
        let mut handlers: Vec<Excepthandler> = vec![];
        let mut orelse: Vec<Stmt> = vec![];
        let mut finalbody: Vec<Stmt> = vec![];
        let mut trystar = false;

        let body_node = try_node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing body in for statement");
        self.block(body_node, &mut body);

        for child_node in try_node.named_children(self.filtered_cst) {
            match child_node.kind() {
                "except_clause" => {
                    let mut type__: Option<Expr> = None;
                    let mut name: Option<String> = None;
                    let mut body: Vec<Stmt> = vec![];
                    self.block(
                        child_node
                            .child(self.filtered_cst, child_node.child_count() - 1)
                            .expect("exception handler body"),
                        &mut body,
                    );

                    if child_node.child_count() > 3 {
                        // not just `except: ...`
                        let expr_node = &child_node
                            .child(self.filtered_cst, 1)
                            .expect("expression or as_pattern");
                        let expr_type = &get_node_type(expr_node);
                        type__ = match expr_type {
                            NodeType::Production(rule) => match &rule.production_kind {
                                ProductionKind::AS_PATTERN => {
                                    let node = rule.node;
                                    let lhs_expression = node
                                        .child(self.filtered_cst, 0)
                                        .expect("expression for exception handler");
                                    let target_expression = node
                                        .child(self.filtered_cst, 2)
                                        .expect("target for exception handler")
                                        .child(self.filtered_cst, 0)
                                        .expect("pattern target");

                                    name = Some(self.get_valid_identifier(target_expression));

                                    Some(self.expression(lhs_expression)?)
                                }
                                _ => Some(self.expression(expr_node)?),
                            },
                            _ => panic!("unexpected statement handling: {:?}", expr_type),
                        };
                    }

                    handlers.push(Excepthandler::new(
                        ExcepthandlerDesc::ExceptHandler {
                            type__,
                            name,
                            body,
                            star: false,
                        },
                        child_node,
                    ));
                }
                "except_group_clause" => {
                    trystar = true;
                    let mut name: Option<String> = None;
                    let mut body: Vec<Stmt> = vec![];
                    self.block(
                        child_node
                            .child(self.filtered_cst, child_node.child_count() - 1)
                            .expect("exception handler body"),
                        &mut body,
                    );

                    let expr_node = &child_node
                        .child(self.filtered_cst, 1)
                        .expect("expression or as_pattern");
                    let expr_type = &get_node_type(expr_node);
                    let type__ = match expr_type {
                        NodeType::Production(rule) => match &rule.production_kind {
                            ProductionKind::AS_PATTERN => {
                                let node = rule.node;
                                let lhs_expression = node
                                    .child(self.filtered_cst, 0)
                                    .expect("expression for exception handler");
                                let target_expression = node
                                    .child(self.filtered_cst, 2)
                                    .expect("target for exception handler")
                                    .child(self.filtered_cst, 0)
                                    .expect("pattern target");

                                name = Some(self.get_valid_identifier(target_expression));

                                self.expression(lhs_expression)?
                            }
                            _ => self.expression(expr_node)?,
                        },
                        _ => panic!("unexpected statement handling: {:?}", expr_type),
                    };

                    handlers.push(Excepthandler::new(
                        ExcepthandlerDesc::ExceptHandler {
                            type__: Some(type__),
                            name,
                            body,
                            star: true,
                        },
                        child_node,
                    ));
                }
                "else_clause" => {
                    self.block(
                        child_node.child(self.filtered_cst, 2).expect("else body"),
                        &mut orelse,
                    );
                }
                "finally_clause" => {
                    self.block(
                        child_node
                            .child(self.filtered_cst, 2)
                            .expect("finally body"),
                        &mut finalbody,
                    );
                }
                _ => (),
            }
        }

        if trystar {
            Ok(StmtDesc::TryStar {
                body,
                handlers,
                orelse,
                finalbody,
            })
        } else {
            Ok(StmtDesc::Try {
                body,
                handlers,
                orelse,
                finalbody,
            })
        }
    }

    // while_statement: $ => seq(
    //   'while',
    //   field('condition', $.expression),
    //   ':',
    //   field('body', $._suite),
    //   optional(field('alternative', $.else_clause))
    // ),
    fn while_statement<'a>(&mut self, for_node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let test_node = for_node
            .child_by_field_name(self.filtered_cst, "condition")
            .expect("missing condition in while statement");
        let test = self.expression(test_node)?;

        let body_node = for_node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing body in for statement");
        let mut body = vec![];
        self.block(body_node, &mut body);

        let mut orelse = vec![];

        let orelse_node = for_node.child_by_field_name(self.filtered_cst, "alternative");
        match &orelse_node {
            Some(orelse_cont) => {
                match &orelse_cont.child_by_field_name(self.filtered_cst, "body") {
                    Some(body_cont) => self.block(body_cont, &mut orelse),
                    _ => (),
                }
            }
            _ => (),
        }

        Ok(StmtDesc::While { test, body, orelse })
    }

    // Process If StmtDesc
    //
    // if_statement: $ => seq(
    //   'if',
    //   field('condition', $.expression),
    //   ':',
    //   field('consequence', $._suite),
    //   repeat(field('alternative', $.elif_clause)),
    //   optional(field('alternative', $.else_clause))
    // ),
    fn if_statement<'a>(&mut self, if_node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let condition_node = if_node
            .child_by_field_name(self.filtered_cst, "condition")
            .expect("missing condition in if statement");
        let condition = self.expression(condition_node)?;
        let block_node = if_node
            .child_by_field_name(self.filtered_cst, "consequence")
            .expect("missing consequence in if statement");
        let mut block = vec![];
        self.block(block_node, &mut block);

        let mut elif_elses = vec![];

        for elif_or_else in if_node.children_by_field_name(self.filtered_cst, "alternative") {
            elif_elses.push(elif_or_else);
        }

        let mut last_orelse = vec![];

        for elif_or_else in elif_elses.iter().rev() {
            match elif_or_else.child_by_field_name(self.filtered_cst, "body") {
                Some(else_body) => {
                    last_orelse = vec![];
                    self.block(else_body, &mut last_orelse);
                }
                _ => {
                    //elif body
                    let elif_condition_node = elif_or_else
                        .child_by_field_name(self.filtered_cst, "condition")
                        .expect("missing condition in if statement");
                    let elif_condition = self.expression(elif_condition_node)?;

                    let elif_block_node = elif_or_else
                        .child_by_field_name(self.filtered_cst, "consequence")
                        .expect("missing consequence in if statement");
                    let mut elif_block = vec![];
                    self.block(elif_block_node, &mut elif_block);

                    let elif_statement = Stmt::new(
                        StmtDesc::If {
                            test: elif_condition,
                            body: elif_block,
                            orelse: last_orelse,
                        },
                        elif_or_else,
                        if_node,
                    );

                    last_orelse = vec![];
                    last_orelse.push(elif_statement);
                }
            }
        }

        Ok(StmtDesc::If {
            test: condition,
            body: block,
            orelse: last_orelse,
        })
    }

    // seems overly complex...
    // yield: $ => prec.right(seq(
    //   'yield',
    //   choice(
    //     seq(
    //       'from',
    //       $.expression
    //     ),
    //     optional($._expressions)
    //   )
    // )),
    fn yield_statement<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let yield_statement = match node.child_count() {
            2 => {
                let rhs_expr = node
                    .child(self.filtered_cst, 1)
                    .expect("expected yield rhs");
                let expr = self.expressions(rhs_expr)?;

                ExprDesc::Yield(Some(expr))
            }
            3 => {
                let rhs_expr = node
                    .child(self.filtered_cst, 2)
                    .expect("expected yield from rhs");
                let expr = self.expression(rhs_expr)?;

                ExprDesc::YieldFrom(expr)
            }
            _ => ExprDesc::Yield(None),
        };

        Ok(yield_statement)
    }

    // augmented_assignment: $ => seq(
    //   field('left', $._left_hand_side),
    //   field('operator', choice(
    //     '+=', '-=', '*=', '/=', '@=', '//=', '%=', '**=',
    //     '>>=', '<<=', '&=', '^=', '|='
    //   )),
    //   field('right', $._right_hand_side)
    // ),
    fn aug_assign<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<StmtDesc> {
        let target_node = node
            .child_by_field_name(self.filtered_cst, "left")
            .expect("missing left in aug_assign");
        let operator_node = node
            .child_by_field_name(self.filtered_cst, "operator")
            .expect("missing operator in aug_assign");
        let value_node = node
            .child_by_field_name(self.filtered_cst, "right")
            .expect("missing right in aug_assign");

        let target = self.assign_left_hand_side(target_node)?;

        let operator_type = get_node_type(operator_node);
        let operator = match &operator_type {
            &NodeType::AugAssignOperator(op) => Operator::from(op),
            _ => panic!("missing AugAssignOperator operator"),
        };

        let mut targets = vec![];
        let value = self.assign_right_hand_side(value_node, &mut targets)?;

        Ok(StmtDesc::AugAssign {
            target,
            op: operator,
            value,
        })
    }

    // assignment: $ => seq(
    //   field('left', $._left_hand_side),
    //   choice(
    //     seq('=', field('right', $._right_hand_side)),
    //     seq(':', field('type', $.type)),
    //     seq(':', field('type', $.type), '=', field('right', $._right_hand_side))
    //   )
    // ),
    fn assignment<'a>(
        &mut self,
        node: &'a Node<'a>,
    ) -> Result<(Vec<Expr>, Option<Expr>, Option<Expr>, Option<String>, isize), ()> {
        let mut targets = vec![];

        let lhs = node
            .child_by_field_name(self.filtered_cst, "left")
            .expect("missing left hand side");

        // simple is a 'boolean integer' (what?) set to True for a Name node in target
        // that do not appear in between parenthesis and are hence pure names and not expressions.
        let simple: isize = if lhs.kind() == "identifier" { 1 } else { 0 };

        let lhs_expr = self.assign_left_hand_side(lhs)?;

        targets.push(lhs_expr);

        // deal with types...
        let ty = None;

        let type_annot =
            if let Some(type_node) = node.child_by_field_name(self.filtered_cst, "type") {
                let type_expr_node = type_node
                    .child(self.filtered_cst, 0)
                    .expect("expression of type node");
                Some(self.expression(type_expr_node)?)
            } else {
                None
            };

        // get right hand side, if any
        let rhs = if let Some(rhs_node) = node.child_by_field_name(self.filtered_cst, "right") {
            Some(self.assign_right_hand_side(rhs_node, &mut targets)?)
        } else {
            None
        };

        Ok((targets, type_annot, rhs, ty, simple))
    }

    // _right_hand_side: $ => choice(
    //   $._list_splat_or_expression,
    //   $.splat_or_expressions,
    //   $.assignment,
    //   $.augmented_assignment,
    //   $.yield
    // ),
    //
    // list_splat_or_expressions: $ => prec.right(seq(
    //     $._list_splat_or_expression,
    //     choice(
    //       ',',
    //       seq(
    //         repeat1(seq(
    //           ',',
    //           $._list_splat_or_expression
    //         )),
    //         optional(',')
    //       ),
    //     )
    //   )),
    //
    //  _list_splat_or_expression: $ => choice($.list_splat, $.expression),
    fn assign_right_hand_side<'a>(
        &mut self,
        rhs_node: &'a Node<'a>,
        targets: &mut Vec<Expr>,
    ) -> ErrorableResult<Expr> {
        use ProductionKind::*;

        let expr_type = &get_node_type(rhs_node);
        let rhs = match expr_type {
            NodeType::Production(rule) => match &rule.production_kind {
                LIST_SPLAT => {
                    let starred = self.starred(rule.node)?;
                    self.parser.new_expr(starred, rhs_node)
                }
                LIST_SPLAT_OR_EXPRESSIONS => {
                    // call tuple here as the two subrules needed by list_splat_or_expressions are list_splat
                    // and expression which are both covered in tuple
                    let tuple_desc = self.tuple(rule.node)?;
                    self.parser.new_expr(tuple_desc, rhs_node)
                }
                ASSIGNMENT => {
                    let (mut targetsx, _type_annot, rhsx, _ty, _simple) =
                        self.assignment(rule.node)?;
                    targets.append(&mut targetsx);
                    rhsx.unwrap()
                    // deal with types...
                }
                AUGMENTED_ASSIGNMENT => {
                    panic!(
                        "not yet implemented assign_right_hand_side - AUGMENTED_ASSIGNMENT {:?}",
                        rule.node
                    )
                }
                YIELD => {
                    let yield_desc = self.yield_statement(rule.node)?;
                    self.parser.new_expr(yield_desc, rhs_node)
                }
                _ => self.expression(rhs_node)?,
            },
            _ => self.expression(rhs_node)?,
        };
        Ok(rhs)
    }

    //
    // pattern_list: $ => seq(
    //   $.pattern,
    //   choice(
    //     ',',
    //     seq(
    //       repeat1(seq(
    //         ',',
    //         $.pattern
    //       )),
    //       optional(',')
    //     )
    //   )
    // ),
    //
    fn pattern_list<'a>(&mut self, pattern_list_node: &'a Node<'a>) -> Expr {
        // pattern lists are processed like tuples of patterns
        let mut patterns = vec![];

        for pattern_node in pattern_list_node.named_children(self.filtered_cst) {
            match self.pattern(pattern_node) {
                Ok(expression) => patterns.push(expression),
                _ => (),
            }
        }

        let tuple_desc = ExprDesc::Tuple {
            elts: patterns,
            ctx: self.get_expression_context(),
        };

        self.parser.new_expr(tuple_desc, pattern_list_node)
    }

    //
    // _left_hand_side: $ => choice(
    //   $.pattern,
    //   $.pattern_list,
    // ),
    //
    fn assign_left_hand_side<'a>(&mut self, lhs: &'a Node<'a>) -> ErrorableResult<Expr> {
        self.set_expression_context(ExprContext::Store);
        let lhs_type = &get_node_type(lhs);
        // left hand side, assignment target
        let lhs_expr = match lhs_type {
            NodeType::Production(rule) => match &rule.production_kind {
                // we can treat pattern_list as a tuple
                ProductionKind::PATTERN_LIST => self.pattern_list(lhs),
                _ => self.pattern(lhs)?,
            },
            _ => {
                return Err(self.record_recoverable_error(
                    RecoverableError::UnexpectedExpression(format!(
                        "unexpected assignment left hand side: {:?}",
                        lhs
                    )),
                    lhs,
                ));
            }
        };
        self.pop_expression_context();
        Ok(lhs_expr)
    }

    // pattern: $ => choice(
    //     $.identifier,
    //     alias('match', $.identifier), // ambiguity with match statement: only ":" at end of line decides if "match" keyword
    //     $.keyword_identifier,
    //     $.subscript,
    //     $.attribute,
    //     $.list_splat_pattern,
    //     $.tuple_pattern,
    //     $.list_pattern
    // ),
    fn pattern<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        use ProductionKind::*;

        let node_type = &get_node_type(node);
        let expr: Expr = match &node_type {
            NodeType::Production(rule) => match &rule.production_kind {
                IDENTIFIER => {
                    let name_desc = self.name(rule.node);
                    self.parser.new_expr(name_desc, rule.node)
                }
                KEYWORD_IDENTIFIER => {
                    panic!(
                        "not yet implemented pattern - KEYWORD_IDENTIFIER {:?}",
                        rule.node
                    )
                }
                SUBSCRIPT => {
                    let subscript = self.subscript(node)?;
                    self.parser.new_expr(subscript, rule.node)
                }
                ATTRIBUTE => {
                    let attribute = self.attribute(node)?;
                    self.parser.new_expr(attribute, rule.node)
                }
                LIST_SPLAT_PATTERN => self.list_splat_pattern(node)?,
                TUPLE_PATTERN => self.tuple_pattern(rule.node)?,
                LIST_PATTERN => self.list_pattern(rule.node)?,
                _ => {
                    return Err(self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "unexpected token in pattern: {:?}",
                            node
                        )),
                        node,
                    ));
                }
            },
            _ => {
                return Err(self.record_recoverable_error(
                    RecoverableError::UnexpectedExpression(format!(
                        "unexpected token in pattern: {:?}",
                        node
                    )),
                    node,
                ));
            }
        };

        Ok(expr)
    }

    fn comma_separated_patterns<'a>(&mut self, node: &'a Node<'a>, sub_patterns: &mut Vec<Expr>) {
        for child in node.named_children(self.filtered_cst) {
            // it is ok to leave out a sub-pattern if there is a problem with it
            match self.pattern(child) {
                Ok(arg) => sub_patterns.push(arg),
                _ => (),
            };
        }
    }

    // tuple_pattern: $ => seq(
    //   '(',
    //   optional($._patterns),
    //   ')'
    // ),
    fn tuple_pattern<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        if node.child_count() == 3 {
            // if single item in tuple, unwrap this and teat as individual item
            // e.g.
            // `(a) = g` would be treated simply as a=g (normal assignment)
            // but, `(a,) = g` would be treated as `(a,) = g` (tuple assignment)
            let expr_node = &node.child(self.filtered_cst, 1).unwrap();
            self.pattern(expr_node)
        } else {
            let mut sub_patterns = vec![];
            self.comma_separated_patterns(node, &mut sub_patterns);

            let tuple_pattern = ExprDesc::Tuple {
                elts: sub_patterns,
                ctx: self.get_expression_context(),
            };

            Ok(self.parser.new_expr(tuple_pattern, node))
        }
    }

    // list_pattern: $ => seq(
    //   '[',
    //   optional($._patterns),
    //   ']'
    // ),
    fn list_pattern<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        let mut sub_patterns = vec![];
        self.comma_separated_patterns(node, &mut sub_patterns);

        let list_pattern = ExprDesc::List {
            elts: sub_patterns,
            ctx: self.get_expression_context(),
        };

        Ok(self.parser.new_expr(list_pattern, node))
    }

    // list_splat_pattern: $ => seq(
    //   '*',
    //   choice($.identifier, $.keyword_identifier, $.subscript, $.attribute)
    // ),
    fn list_splat_pattern<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        use ProductionKind::*;

        // first node is *, second is the choice of ...
        let actual_pattern_node = &node.child(self.filtered_cst, 1).unwrap();

        let node_type = &get_node_type(actual_pattern_node);
        let value: Expr = match &node_type {
            NodeType::Production(rule) => match &rule.production_kind {
                IDENTIFIER => {
                    let text_desc = self.name(rule.node);
                    self.parser.new_expr(text_desc, rule.node)
                }
                KEYWORD_IDENTIFIER => {
                    panic!(
                        "KEYWORD_IDENTIFIER not yet implemented for list_splat_pattern {:?}",
                        rule.node
                    )
                }
                SUBSCRIPT => {
                    let subscript = self.subscript(rule.node)?;
                    self.parser.new_expr(subscript, rule.node)
                }
                ATTRIBUTE => {
                    let attribute_desc = self.attribute(rule.node)?;
                    self.parser.new_expr(attribute_desc, rule.node)
                }
                _ => {
                    return Err(self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "unexpected token in list_splat_pattern: {:?}",
                            actual_pattern_node
                        )),
                        actual_pattern_node,
                    ));
                }
            },
            _ => {
                return Err(self.record_recoverable_error(
                    RecoverableError::UnexpectedExpression(format!(
                        "unexpected token in list_splat_pattern: {:?}",
                        actual_pattern_node
                    )),
                    actual_pattern_node,
                ));
            }
        };

        let starred = ExprDesc::Starred {
            value,
            ctx: self.get_expression_context(),
        };

        Ok(self.parser.new_expr(starred, node))
    }

    // Process an ExprDesc.
    // expression: $ => choice(
    //   $.comparison_operator,
    //   $.not_operator,
    //   $.boolean_operator,
    //   $.lambda,
    //   $.primary_expression,
    //   $.conditional_expression,
    //   $.named_expression,
    // ),
    fn expression<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        use ProductionKind::*;

        let node_type = &get_node_type(node);

        let expr = match node_type {
            NodeType::Production(rule) => match &rule.production_kind {
                COMPARISON_OPERATOR => {
                    let comparison_op_desc = self.comparison_operator(rule.node)?;
                    self.parser.new_expr(comparison_op_desc, rule.node)
                }
                NOT_OPERATOR => {
                    let not_op_desc = self.not_operator(rule.node)?;
                    self.parser.new_expr(not_op_desc, rule.node)
                }
                BOOLEAN_OPERATOR => {
                    let bool_op_desc = self.bool_op(rule.node)?;
                    self.parser.new_expr(bool_op_desc, rule.node)
                }
                LAMBDA => {
                    let lambda_desc = self.lambda(rule.node)?;
                    self.parser.new_expr(lambda_desc, rule.node)
                }
                CONDITIONAL_EXPRESSION => {
                    let if_desc = self.if_exp(rule.node)?;
                    let node = rule.node;
                    let sub_node = node
                        .child(self.filtered_cst, 4)
                        .expect("if_exp missing orelse");
                    let start_position = node.start_position();
                    let mut end_position = node.end_position();

                    if sub_node.kind() == "as_pattern" {
                        end_position = node
                            .child(self.filtered_cst, 4)
                            .expect("if_exp missing orelse")
                            .child(self.filtered_cst, 0)
                            .expect("orelse missing child")
                            .end_position();
                    }
                    Expr::new(
                        if_desc,
                        start_position.row as isize + 1,
                        start_position.column as isize,
                        end_position.row as isize + 1,
                        end_position.column as isize,
                    )
                }
                NAMED_EXPRESSION => {
                    let named_desc = self.named_expression(rule.node)?;
                    self.parser.new_expr(named_desc, rule.node)
                }
                AS_PATTERN => {
                    let body_node = node
                        .child(self.filtered_cst, 0)
                        .expect("as_pattern missing body");
                    let node_type = &get_node_type(body_node);
                    self.primary_expression(node_type, body_node)?
                }
                _ => self.primary_expression(node_type, rule.node)?,
            },
            _ => {
                return Err(self.record_recoverable_error(
                    RecoverableError::UnexpectedExpression(format!("{:?}", node_type)),
                    node,
                ));
            }
        };
        Ok(expr)
    }

    // Process a Primary ExprDesc
    // primary_expression: $ => choice(
    //   $.await,
    //   $.binary_operator,
    //   $.identifier,
    //   alias("match", $.identifier),
    //   $.keyword_identifier,
    //   $.string,
    //   $.concatenated_string,
    //   $.integer,
    //   $.float,
    //   $.true,
    //   $.false,
    //   $.none,
    //   $.unary_operator,
    //   $.attribute,
    //   $.subscript,
    //   $.call,
    //   $.list,
    //   $.list_comprehension,
    //   $.dictionary,
    //   $.dictionary_comprehension,
    //   $.set,
    //   $.set_comprehension,
    //   $.tuple,
    //   $.parenthesized_expression,
    //   $.generator_expression,
    //   $.ellipsis,
    // ),
    fn primary_expression(&mut self, node_type: &NodeType, node: &Node) -> ErrorableResult<Expr> {
        use ProductionKind::*;

        let exprdesc: ExprDesc = match node_type {
            NodeType::Production(rule) => match &rule.production_kind {
                AWAIT => self.await_expr(rule.node)?,
                BINARY_OPERATOR => self.binary_op(rule.node)?,
                // TODO: soft keywords like `match` and that story with python and tree-sitter
                IDENTIFIER => self.name(rule.node),
                KEYWORD_IDENTIFIER => self.name(node),
                STRING => self.raw_string(rule.node, rule.node)?,
                CONCATENATED_STRING => self.concatenated_string(rule.node)?,
                INTEGER => self.integer(rule.node)?,
                FLOAT => self.float(rule.node)?,
                TRUE => self.constant(ConstantDesc::Bool(true)),
                FALSE => self.constant(ConstantDesc::Bool(false)),
                NONE => self.none(),
                UNARY_OPERATOR => self.unary_op(rule.node)?,
                ATTRIBUTE => self.attribute(rule.node)?,
                SUBSCRIPT => self.subscript(rule.node)?,
                CALL => self.call(rule.node)?,
                LIST => self.list(rule.node)?,
                LIST_COMPREHENSION => self.list_comp(rule.node)?,
                DICTIONARY => self.dictionary(rule.node)?,
                DICTIONARY_COMPREHENSION => self.dictionary_comprehension(rule.node)?,
                SET => self.set(rule.node)?,
                SET_COMPREHENSION => self.set_comp(rule.node)?,
                TUPLE => self.tuple(rule.node)?,
                PARENTHESIZED_EXPRESSION => return self.parenthesized_expression(rule.node),
                GENERATOR_EXPRESSION => self.generator_expression(rule.node)?,
                ELLIPSIS => self.constant(ConstantDesc::Ellipsis),
                _ => {
                    return Err(self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "unexpected token: {:?}",
                            node
                        )),
                        node,
                    ));
                }
            },
            _ => {
                return Err(self.record_recoverable_error(
                    RecoverableError::UnexpectedExpression(format!("{:?}", node)),
                    node,
                ));
            }
        };
        Ok(self.parser.new_expr(exprdesc, node))
    }

    // parenthesized_expression: $ => prec(PREC.parenthesized_expression, seq(
    //  '(',
    //   choice($.expression, $.yield),
    //   ')'
    // )),
    fn parenthesized_expression<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        let middle_node = &node
            .child(self.filtered_cst, 1)
            .expect("middle node of parenthesized_expression");
        if middle_node.kind() == "yield" {
            let yield_desc = self.yield_statement(middle_node)?;
            Ok(self.parser.new_expr(yield_desc, middle_node))
        } else {
            self.expression(middle_node)
        }
    }

    // named_expression: $ => seq(
    //   field('name', $._named_expresssion_lhs),
    //   ':=',
    //   field('value', $.expression)
    // ),
    fn named_expression<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let target_node = &node
            .child_by_field_name(self.filtered_cst, "name")
            .expect("named_expression missing name field");

        self.set_expression_context(ExprContext::Store);
        let target = self.expression(target_node)?;
        self.pop_expression_context();

        let value_node = &node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("named_expression missing value field");
        let value = self.expression(value_node)?;

        Ok(ExprDesc::NamedExpr { target, value })
    }

    // list_comprehension: $ => seq(
    //   '[',
    //   field('body', $.expression),
    //   $._comprehension_clauses,
    //   ']'
    // ),
    fn list_comp<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut generators = vec![];
        let elt = self.comprehension_core(node, &mut generators)?;

        Ok(ExprDesc::ListComp { elt, generators })
    }

    // set_comprehension: $ => seq(
    //   '{',
    //   field('body', $.expression),
    //   $._comprehension_clauses,
    //   '}'
    // ),
    fn set_comp<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut generators = vec![];
        let elt = self.comprehension_core(node, &mut generators)?;
        Ok(ExprDesc::SetComp { elt, generators })
    }

    // generator_expression: $ => seq(
    //   '(',
    //   field('body', $.expression),
    //   $._comprehension_clauses,
    //   ')'
    // ),
    fn generator_expression<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut generators = vec![];
        let elt = self.comprehension_core(node, &mut generators)?;
        Ok(ExprDesc::GeneratorExp { elt, generators })
    }

    // dictionary_comprehension: $ => seq(
    //   '{',
    //   field('body', $.pair),
    //   $._comprehension_clauses,
    //   '}'
    // ),
    fn dictionary_comprehension<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut generators = vec![];
        let (key, value) = self.dictionary_pair(node, &mut generators)?;

        Ok(ExprDesc::DictComp {
            key,
            value,
            generators,
        })
    }

    fn dictionary_pair<'a>(
        &mut self,
        node: &'a Node<'a>,
        generators: &mut Vec<Comprehension>,
    ) -> ErrorableResult<(Expr, Expr)> {
        //let elt =
        let pair_node = node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing pair in dictionary_comprehension");

        let key_node = pair_node
            .child_by_field_name(self.filtered_cst, "key")
            .expect("missing key in pair node of dictionary");
        let key = self.expression(key_node)?;

        let value_node = pair_node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("missing value in pair node of dictionary");
        let value = self.expression(value_node)?;

        self.comprehension_clauses(node, generators)?;
        Ok((key, value))
    }

    fn comprehension_core<'a>(
        &mut self,
        node: &'a Node<'a>,
        generators: &mut Vec<Comprehension>,
    ) -> ErrorableResult<Expr> {
        let body_node = node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing body in comprehension");
        let elt = self.expression(body_node)?;

        self.comprehension_clauses(node, generators)?;
        Ok(elt)
    }

    // _comprehension_clauses: $ => seq(
    //  $.for_in_clause,
    //  repeat(choice(
    //    $.for_in_clause,
    //    $.if_clause
    //  ))
    // ),
    fn comprehension_clauses<'a>(
        &mut self,
        node: &'a Node<'a>,
        generators: &mut Vec<Comprehension>,
    ) -> ErrorableResult<()> {
        use ProductionKind::*;

        for child_node in node.named_children(self.filtered_cst) {
            //_comprehension_clauses
            let child_type = &get_node_type(child_node);
            match child_type {
                // for_in_clause
                NodeType::Production(prod) => match prod.production_kind {
                    FOR_IN_CLAUSE => {
                        let comp = self.comprehension_clause(child_node)?;
                        generators.push(comp);
                    }
                    IF_CLAUSE => {
                        let expr = self.if_clause(child_node)?;
                        generators.last_mut().unwrap().ifs.push(expr);
                    }
                    _ => (),
                },
                _ => (), // skip other nodes
            }
        }
        Ok(())
    }

    // _comprehension_clauses: $ => seq(
    //   $.for_in_clause,
    //   repeat(choice(
    //     $.for_in_clause,
    //     $.if_clause
    //   ))
    // ),
    //
    // _expression_within_for_in_clause: $ => choice(
    //     $.expression,
    //     alias($.lambda_within_for_in_clause, $.lambda)
    //   ),
    //
    // for_in_clause: $ => prec.left(seq(
    //   optional('async'),
    //   'for',
    //   field('left', $._left_hand_side),
    //   'in',
    //   field('right', commaSep1($._expression_within_for_in_clause)),
    //   optional(',')
    // )),
    fn comprehension_clause<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Comprehension> {
        let left_node = &node
            .child_by_field_name(self.filtered_cst, "left")
            .expect("comprehension_clause missing left field");

        let right_node = &node
            .child_by_field_name(self.filtered_cst, "right")
            .expect("comprehension_clause missing right field");

        self.set_expression_context(ExprContext::Store);
        let target = self.assign_left_hand_side(left_node)?;
        self.pop_expression_context();

        self.set_expression_context(ExprContext::Load);
        let iter = self.expression(right_node)?; // TODO this should call _expression_within_for_in_clause, which takes lambda too
        self.pop_expression_context();

        let ifs = vec![];

        Ok(Comprehension {
            target,
            iter,
            ifs,
            is_async: node.child(self.filtered_cst, 0).unwrap().kind().eq("async"),
        })
    }

    // if_clause: $ => seq(
    //   'if',
    //   $.expression
    // ),
    fn if_clause<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<Expr> {
        let expr_node = node
            .child(self.filtered_cst, 1)
            .expect("if_clause missing expression");

        self.expression(expr_node)
    }

    //
    // Process Binary Operators
    //
    // binary_operator: $ => {
    //   const table = [
    //     [prec.left, '+', PREC.plus],
    //     [prec.left, '-', PREC.plus],
    //     [prec.left, '*', PREC.times],
    //     [prec.left, '@', PREC.times],
    //     [prec.left, '/', PREC.times],
    //     [prec.left, '%', PREC.times],
    //     [prec.left, '//', PREC.times],
    //     [prec.right, '**', PREC.power],
    //     [prec.left, '|', PREC.bitwise_or],
    //     [prec.left, '&', PREC.bitwise_and],
    //     [prec.left, '^', PREC.xor],
    //     [prec.left, '<<', PREC.shift],
    //     [prec.left, '>>', PREC.shift],
    //   ];

    //   return choice(...table.map(([fn, operator, precedence]) => fn(precedence, seq(
    //     field('left', $.primary_expression),
    //     field('operator', operator),
    //     field('right', $.primary_expression)
    //   ))));
    // },
    fn binary_op<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let lhs_node = node
            .child_by_field_name(self.filtered_cst, "left")
            .expect("missing lhs in binary op");
        let left = self.expression(lhs_node)?;
        let operator_node = node
            .child_by_field_name(self.filtered_cst, "operator")
            .expect("missing operator in binary op");
        let operator = match get_node_type(operator_node) {
            NodeType::BinaryOperator(op) => Operator::try_from(op)
                .expect("expected NodeType::BinaryOperator to have valid binary operator"),
            _ => {
                return Err(self.record_recoverable_error(
                    RecoverableError::MissingOperator(operator_node.kind().into()),
                    node,
                ));
            }
        };
        let rhs_node = node
            .child_by_field_name(self.filtered_cst, "right")
            .expect("missing rhs in binary op");
        let right = self.expression(rhs_node)?;

        Ok(ExprDesc::BinOp {
            left,
            op: operator,
            right,
        })
    }

    // Process Attribute
    //
    // attribute: $ => prec(PREC.call, seq(
    //   field('object', $.primary_expression),
    //   '.',
    //   field('attribute', $.identifier)
    // )),
    fn attribute<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let lhs = node
            .child_by_field_name(self.filtered_cst, "object")
            .expect("missing left hand side (attribute.object)");
        let lhs_type = get_node_type(lhs);
        self.set_expression_context(ExprContext::Load);
        let value = self.primary_expression(&lhs_type, lhs)?;
        self.pop_expression_context();
        let rhs = node
            .child_by_field_name(self.filtered_cst, "attribute")
            .expect("missing right hand side (attribute.attribute)");
        let raw_attr = self.get_valid_identifier(rhs);

        // Handle AUTOCOMPLETE token
        let attr = if raw_attr == AUTOCOMPLETE_TOKEN {
            String::from("")
        } else {
            raw_attr
        };

        Ok(ExprDesc::Attribute {
            value,
            attr,
            ctx: self.get_expression_context(),
        })
    }

    fn set_expression_context(&mut self, ctx: ExprContext) {
        self.current_expr_ctx.push(Some(ctx));
    }

    fn pop_expression_context(&mut self) {
        self.current_expr_ctx.pop();
    }

    fn get_expression_context(&mut self) -> ExprContext {
        match self.current_expr_ctx.last() {
            Some(None) => ExprContext::Load,
            Some(not_none) => match not_none {
                Some(ExprContext::Store) => ExprContext::Store,
                Some(ExprContext::Del) => ExprContext::Del,
                _ => ExprContext::Load,
            },
            _ => ExprContext::Load,
        }
    }

    // _collection_elements: $ => seq(
    //  commaSep1(choice(
    //   $.yield, $.list_splat, $.parenthesized_list_splat, s.expression
    //  )),
    //  optional(',')
    // ),
    fn collection_elements(&mut self, node: &Node, exp_list: &mut Vec<Expr>) {
        for child in node.named_children(self.filtered_cst) {
            // it is ok to leave out a subexpression if there is a problem with it
            match child.kind() {
                "yield" => {
                    // I don't think we should support yield here
                    self.record_recoverable_error(
                        RecoverableError::UnimplementedStatement(format!("{:?}", &child)),
                        child,
                    );
                }
                "list_splat" => {
                    match self.starred(child) {
                        Ok(starred) => exp_list.push(self.parser.new_expr(starred, child)),
                        _ => (),
                    };
                }
                "parenthesized_list_splat" => {
                    // TODO: add support for parenthesized_list_splat
                    self.record_recoverable_error(
                        RecoverableError::UnimplementedStatement(format!("{:?}", &child)),
                        child,
                    );
                }
                _ => {
                    match self.expression(child) {
                        Ok(arg) => exp_list.push(arg),
                        _ => (),
                    };
                }
            };
        }
    }

    // Process List expression
    // list: $ => seq(
    //   '[',
    //   optional($._collection_elements),
    //   ']'
    // ),
    fn list<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut expressions = vec![];
        self.collection_elements(node, &mut expressions);

        Ok(ExprDesc::List {
            elts: expressions,
            ctx: self.get_expression_context(),
        })
    }

    // Process Tuple expression
    // tuple: $ => seq(
    //   '(',
    //   optional($._collection_elements),
    //   ')'
    // ),
    fn tuple<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut expressions = vec![];
        self.collection_elements(node, &mut expressions);

        Ok(ExprDesc::Tuple {
            elts: expressions,
            ctx: self.get_expression_context(),
        })
    }

    // Process Set expression
    // set: $ => seq(
    //   '{',
    //   $._collection_elements,
    //   '}'
    // ),
    fn set<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let mut expressions = vec![];
        self.collection_elements(node, &mut expressions);

        Ok(ExprDesc::Set(expressions))
    }

    //Process Set expression
    fn if_exp<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let body_node = node
            .child(self.filtered_cst, 0)
            .expect("if_exp missing body");
        let body = self.expression(body_node)?;

        let test_node = node
            .child(self.filtered_cst, 2)
            .expect("if_exp missing test");
        let test = self.expression(test_node)?;

        let orelse_node = node
            .child(self.filtered_cst, 4)
            .expect("if_exp missing orelse");
        let orelse = self.expression(orelse_node)?;

        Ok(ExprDesc::IfExp { test, body, orelse })
    }

    // dictionary: $ => seq(
    //   '{',
    //   optional(commaSep1(choice($.pair, $.dictionary_splat))),
    //   optional(','),
    //   '}'
    // ),
    //
    // dictionary_splat: $ => seq(
    //   '**',
    //   $.expression
    // ),
    fn dictionary(&mut self, node: &Node) -> ErrorableResult<ExprDesc> {
        use ProductionKind::*;

        let mut keys = vec![];
        let mut values = vec![];

        for pair_or_dictionary_splat in node.named_children(self.filtered_cst) {
            let pair_or_dictionary_splat_type = get_node_type(pair_or_dictionary_splat);
            match &pair_or_dictionary_splat_type {
                NodeType::Production(param) => match &param.production_kind {
                    PAIR => {
                        let key_node = pair_or_dictionary_splat
                            .child(self.filtered_cst, 0)
                            .ok_or_else(|| {
                                self.record_recoverable_error(
                                    RecoverableError::MissingChild,
                                    pair_or_dictionary_splat,
                                )
                            })?;

                        let key = self.expression(key_node)?;
                        keys.push(Some(key));

                        let value_node = pair_or_dictionary_splat
                            .child(self.filtered_cst, 2)
                            .ok_or_else(|| {
                                self.record_recoverable_error(
                                    RecoverableError::MissingChild,
                                    pair_or_dictionary_splat,
                                )
                            })?;

                        let value = self.expression(value_node)?;
                        values.push(value);
                    }
                    DICTIONARY_SPLAT => {
                        let value_node = pair_or_dictionary_splat
                            .child(self.filtered_cst, 1)
                            .ok_or_else(|| {
                                self.record_recoverable_error(
                                    RecoverableError::MissingChild,
                                    pair_or_dictionary_splat,
                                )
                            })?;

                        keys.push(None);
                        let value = self.expression(value_node)?;
                        values.push(value);
                    }
                    _ => {
                        self.record_recoverable_error(
                            RecoverableError::UnexpectedExpression(format!(
                                "unexpected dictionary production: {:?}",
                                param
                            )),
                            node,
                        );
                    }
                },
                _ => {
                    self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "unexpected dictionary production: {:?}",
                            node
                        )),
                        node,
                    );
                }
            }
        }

        Ok(ExprDesc::Dict { keys, values })
    }

    // Process Call
    //
    // call: $ => prec(PREC.call, seq(
    //   field('function', $.primary_expression),
    //   field('arguments', choice(
    //     $.generator_expression,
    //     $.argument_list
    //   ))
    // )),
    fn call<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let function = node
            .child_by_field_name(self.filtered_cst, "function")
            .expect("missing function in call");

        let function_type = &get_node_type(function);
        let func = self.primary_expression(function_type, function)?;

        let argument_or_generator = node
            .child_by_field_name(self.filtered_cst, "arguments")
            .expect("missing arguments (or generator) in call");

        let mut args = vec![];
        let mut keywords = vec![];

        match argument_or_generator.kind() {
            "generator_expression" => {
                let generator_expression = self.generator_expression(argument_or_generator)?;
                args.push(
                    self.parser
                        .new_expr(generator_expression, argument_or_generator),
                );
            }
            _ => {
                self.argument_list(argument_or_generator, &mut args, &mut keywords)?;
            }
        }

        Ok(ExprDesc::Call {
            func,
            args,
            keywords,
        })
    }

    // Process Argument List
    //
    // argument_list: $ => seq(
    //   '(',
    //   optional(commaSep1(
    //     choice(
    //       $.expression,
    //       $.list_splat,
    //       $.dictionary_splat,
    //       alias($.parenthesized_list_splat, $.parenthesized_expression),
    //       $.keyword_argument
    //     )
    //   )),
    //   optional(','),
    //   ')'
    // ),
    fn argument_list(
        &mut self,
        node: &Node,
        arg_list: &mut Vec<Expr>,
        keyword_list: &mut Vec<AstKeyword>,
    ) -> ErrorableResult<()> {
        use ProductionKind::*;

        for child in node.named_children(self.filtered_cst) {
            let child_type = get_node_type(child);

            match &child_type {
                NodeType::Production(rule) => match &rule.production_kind {
                    //TODO: alias($.parenthesized_list_splat, $.parenthesized_expression), - what does this resolve to?
                    LIST_SPLAT => {
                        let starred = self.starred(child)?;
                        arg_list.push(self.parser.new_expr(starred, child));
                    }
                    DICTIONARY_SPLAT => {
                        let keywordarg = self.dictionary_splat(child)?;
                        keyword_list.push(keywordarg);
                    }
                    KEYWORD_ARGUMENT => {
                        let keywordarg = self.keyword_argument(child)?;
                        keyword_list.push(keywordarg);
                    }
                    _ => {
                        let expr = self.expression(child)?;
                        arg_list.push(expr);
                    }
                },
                _ => {
                    self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "unexpected argument handling: {:?}",
                            child_type
                        )),
                        child,
                    );
                }
            };
        }
        Ok(())
    }

    // list_splat: $ => seq(
    //   '*',
    //   $.expression,
    // ),
    fn starred<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let identifier = node
            .child(self.filtered_cst, 1)
            .expect("missing identifier in starred");

        let value = self.expression(identifier)?;

        Ok(ExprDesc::Starred {
            value,
            ctx: self.get_expression_context(),
        })
    }

    // subscript: $ => prec(PREC.call, seq(
    //   field('value', $.primary_expression),
    //   '[',
    //   commaSep1(field('subscript', choice($.expression, $.slice))),
    //   optional(','),
    //   ']'
    // )),
    //
    // slice: $ => seq(
    //   optional($.expression),
    //   ':',
    //   optional($.expression),
    //   optional(seq(':', optional($.expression)))
    // ),
    fn subscript<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let value_node = node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("value field in subscript");

        // subscripts and their slies are always loaded, even if they are on the lhs of an assignment operation
        self.set_expression_context(ExprContext::Load);
        let value = self.expression(value_node)?;

        // if many slices, then wrapped inside a Tuple, otherwise slice on its own if only one
        let mut slices: Vec<Expr> = vec![];

        for subscript_node in node.children_by_field_name(self.filtered_cst, "subscript") {
            let mut slice_elements: Vec<Option<Expr>> = vec![];

            let mut last_expr: Option<Expr> = None;

            if subscript_node.kind() == "slice" {
                for slice_child in subscript_node.children(self.filtered_cst) {
                    // if : or something else
                    let token = self.get_valid_identifier(slice_child);
                    if token == ":" {
                        slice_elements.push(last_expr);
                        last_expr = None;
                    } else {
                        last_expr = Some(self.expression(slice_child)?);
                    }
                }
                slice_elements.push(last_expr);

                slices.push(self.parser.new_expr(
                    ExprDesc::Slice {
                        lower: slice_elements.remove(0),
                        upper: {
                            if slice_elements.is_empty() {
                                None
                            } else {
                                slice_elements.remove(0)
                            }
                        },
                        step: {
                            if slice_elements.is_empty() {
                                None
                            } else {
                                slice_elements.remove(0)
                            }
                        },
                    },
                    subscript_node,
                ));
            } else {
                // single expression
                slices.push(self.expression(subscript_node)?);
            }
        }

        let ends_in_comma = node
            .child(self.filtered_cst, node.child_count() - 2)
            .unwrap()
            .kind()
            == ",";

        let slice = if !ends_in_comma && slices.len() == 1 {
            slices.pop().expect("should be at least one slice")
        } else {
            // if ends in comma or if there are more than one slice
            let start_position = node
                .child(self.filtered_cst, 2)
                .expect("first element of tuple")
                .start_position();

            let end_position = node
                .child(self.filtered_cst, node.child_count() - 2)
                .expect("']' node in subscript")
                .end_position();

            Expr::new(
                ExprDesc::Tuple {
                    elts: slices,
                    ctx: self.get_expression_context(),
                },
                start_position.row as isize + 1,
                start_position.column as isize,
                end_position.row as isize + 1,
                end_position.column as isize,
            )
        };

        self.pop_expression_context();

        Ok(ExprDesc::Subscript {
            value,
            slice,
            ctx: self.get_expression_context(),
        })
    }

    // dictionary_splat: $ => seq(
    //   '**',
    //   $.expression
    // ),
    fn dictionary_splat<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<AstKeyword> {
        let identifier = node
            .child(self.filtered_cst, 1)
            .expect("missing identifier in dictionary_splat");

        let value = self.expression(identifier)?;

        //Ok(Box::new(DictionaryFuncArg::new(
        Ok(AstKeyword::new(None, value, node))
    }

    // keyword_argument: $ => seq(
    //   field('name', choice($.identifier, $.keyword_identifier, alias("match", $.identifier))),
    //   '=',
    //   field('value', $.expression)
    // ),
    fn keyword_argument<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<AstKeyword> {
        let lhs = node
            .child_by_field_name(self.filtered_cst, "name")
            .expect("missing lhs in keyword_argument");
        // TODO: keywords (await and async) are permitted in this location but
        // we use get_valid_identifier anyway as it would be strange for anyone
        // to be giving a variable name (await and async) in real code.
        // If this turns out to be a problem then we can add a modifed version
        // of get_valid_identifier that permits await and async to be used as
        // identifiers
        let arg = self.get_valid_identifier(lhs);

        let rhs = node
            .child_by_field_name(self.filtered_cst, "value")
            .expect("missing rhs in keyword_argument");

        let value = self.expression(rhs)?;

        //Ok(Box::new(DictionaryFuncArg::new(
        Ok(AstKeyword::new(Some(arg), value, node))
    }

    // Process a Comparison Operator
    // comparison_operator: $ => prec.left(PREC.compare, seq(
    //   $.primary_expression,
    //   repeat1(seq(
    //     field('operators',
    //       choice(
    //         '<',
    //         '<=',
    //         '==',
    //         '!=',
    //         '>=',
    //         '>',
    //         '<>',
    //         'in',
    //         alias(seq('not', 'in'), 'not in'),
    //         'is',
    //         alias(seq('is', 'not'), 'is not')
    //       )),
    //     $.primary_expression
    //   ))
    // )),
    fn comparison_operator<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        // we have to do some hoo haa re skipping nodes and reading ahead to accommodate the
        // fact that `is not` and `not in` are represented as two separate sets of nodes in the cst

        let mut left = None;
        let mut ops = vec![];
        let mut comparators = vec![];

        // we need to examine more than one item so in a vector is needed
        let mut all_items = vec![];

        for child in node.children(self.filtered_cst) {
            all_items.push(child);
        }
        let mut next_itr = all_items.iter();
        next_itr.next();
        let mut skip_next = false;
        for child in all_items.iter() {
            if skip_next {
                skip_next = false;
                continue;
            }
            let child_type = &get_node_type(child);
            match child_type {
                NodeType::Production(_) => match left {
                    // comparitor
                    None => left = Some(self.primary_expression(child_type, child)?),
                    Some(_) => comparators.push(self.primary_expression(child_type, child)?),
                },
                _ => {
                    // must be an operator
                    if let Some(cmp_operator) = get_comp_op(child_type) {
                        ops.push(cmp_operator);

                        match cmp_operator {
                            Cmpop::IsNot | Cmpop::NotIn => {
                                skip_next = true;
                            }
                            _ => (),
                        }
                    }
                }
            }
        }

        Ok(ExprDesc::Compare {
            left: left
                .ok_or_else(|| self.record_recoverable_error(RecoverableError::MissingLhs, node))?,
            ops,
            comparators,
        })
    }

    // not_operator: $ => prec(PREC.not, seq(
    //   'not',
    //   field('argument', $.expression)
    // )),
    fn not_operator<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let arg = node
            .child_by_field_name(self.filtered_cst, "argument")
            .expect("missing argument in not operator");
        let operand = self.expression(arg)?;

        Ok(ExprDesc::UnaryOp {
            op: Unaryop::Not,
            operand,
        })
    }

    // await: $ => prec(PREC.unary, seq(
    //   'await',
    //   $.expression
    // )),
    fn await_expr<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let arg = node
            .child(self.filtered_cst, 1)
            .expect("missing argument in await");
        let arg = self.expression(arg)?;

        Ok(ExprDesc::Await(arg))
    }

    // lambda_within_for_in_clause: $ => seq(
    //     'lambda',
    //     field('parameters', optional($.lambda_parameters)),
    //     ':',
    //     field('body', $._expression_within_for_in_clause)
    //   ),
    //
    // lambda: $ => prec(PREC.lambda, seq(
    //   'lambda',
    //   field('parameters', optional($.lambda_parameters)),
    //   ':',
    //   field('body', $.expression)
    // )),
    fn lambda<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let args = match node.child_by_field_name(self.filtered_cst, "parameters") {
            Some(params_node) => self.get_parameters(params_node)?,
            _ => Arguments {
                posonlyargs: vec![],
                args: vec![],
                vararg: None,
                kwonlyargs: vec![],
                kw_defaults: vec![],
                kwarg: None,
                defaults: vec![],
            },
        };

        let body_node = node
            .child_by_field_name(self.filtered_cst, "body")
            .expect("missing body in lambda");
        let body = self.expression(body_node)?;

        Ok(ExprDesc::Lambda { args, body })
    }

    // boolean_operator: $ => choice(
    //   prec.left(PREC.and, seq(
    //     field('left', $.expression),
    //     field('operator', 'and'),
    //     field('right', $.expression)
    //   )),
    //   prec.left(PREC.or, seq(
    //     field('left', $.expression),
    //     field('operator', 'or'),
    //     field('right', $.expression)
    //   ))
    // ),
    fn bool_op<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let op_node = node
            .child_by_field_name(self.filtered_cst, "operator")
            .expect("missing operator in unary operator");
        let op_type = get_node_type(op_node);
        let operator = match op_type {
            NodeType::Keyword(Keyword::AND) => Boolop::And,
            NodeType::Keyword(Keyword::OR) => Boolop::Or,
            _ => panic!("unexpected boolean operator node {:?}", op_type),
        };

        let mut values: Vec<Expr> = vec![];

        for child_name in &["left", "right"] {
            let child_node = node
                .child_by_field_name(self.filtered_cst, child_name)
                .expect("missing child node in boolean_operator");
            let child_node_expression = self.expression(child_node)?;

            if let ExprDesc::BoolOp { op: child_op, .. } = &*child_node_expression.desc {
                if child_node.kind() != "parenthesized_expression" && child_op == &operator {
                    // Consecutive operations with the same operator, such as a
                    // or b or c, are collapsed into one node with several
                    // values. Unless child node is wrapped up within a param ()
                    // in which case it is not collapsed. e.g.
                    // a or b or c   <-    collapsed
                    // a or (b or c) < not collapsed

                    if let ExprDesc::BoolOp {
                        values: child_values,
                        ..
                    } = *child_node_expression.desc
                    {
                        values.extend(child_values);
                    }

                    continue;
                }
            }
            values.push(child_node_expression);
        }

        Ok(ExprDesc::BoolOp {
            op: operator,
            values,
        })
    }

    // unary_operator: $ => prec(PREC.unary, seq(
    //   field('operator', choice('+', '-', '~')),
    //   field('argument', $.primary_expression)
    // )),
    fn unary_op<'a>(&mut self, node: &'a Node<'a>) -> ErrorableResult<ExprDesc> {
        let operator_node = node
            .child_by_field_name(self.filtered_cst, "operator")
            .expect("missing operator in unary operator");
        let operator_type = get_node_type(operator_node);
        let operator = match operator_type {
            NodeType::BinaryOperator(BinaryOperator::MINUS) => Unaryop::USub,
            NodeType::BinaryOperator(BinaryOperator::PLUS) => Unaryop::UAdd,
            NodeType::BinaryOperator(BinaryOperator::TILDE) => Unaryop::Invert,
            _ => panic!("unexpected unary operator node {:?}", operator_type),
        };

        let arg = node
            .child_by_field_name(self.filtered_cst, "argument")
            .expect("missing argument in not operator");
        let arg_type = get_node_type(arg);
        let operand = self.primary_expression(&arg_type, arg)?;

        Ok(ExprDesc::UnaryOp {
            op: operator,
            operand,
        })
    }

    //
    //
    // AST node constructors
    //
    //

    // none: $ => 'None',
    fn none(&mut self) -> ExprDesc {
        ExprDesc::Constant {
            value: None,
            kind: None,
        }
    }

    fn constant(&mut self, const_value: ConstantDesc) -> ExprDesc {
        ExprDesc::Constant {
            value: Some(const_value),
            kind: None,
        }
    }

    /// replace all {{ with { and }} with }
    fn tidy_double_braces(&mut self, from_string: String) -> String {
        from_string.replace("{{", "{").replace("}}", "}")
    }

    fn is_triple_quote_multiline(&mut self, string: &str) -> bool {
        // possible to have "\"" - double brackets inside double brackets as input
        string.starts_with("\"\"\"") && string.ends_with("\"\"\"") && string.len() >= 6
            || string.starts_with("\'\'\'") && string.ends_with("\'\'\'") && string.len() >= 6
            || string.starts_with("f\"\"\"") && string.ends_with("\"\"\"") && string.len() >= 7
            || string.starts_with("f\'\'\'") && string.ends_with("\'\'\'") && string.len() >= 7
    }

    /// multiline f strings are interesting and require some giggling around so
    /// that we can consistantly extract substring strings from the string.
    /// Essentially we must:
    /// - Offset 2 extra chars off start of String (""" is 2 more chars wider than ")
    /// - remove newlines and do some offset calculations
    /// - keep track of column offsets on a per line basis
    fn format_multiline_string_node_text_inplace(
        &mut self,
        node_text: &mut String,
        base_row: usize,
        prev_idx: &mut usize,
        multiline_offsets: &mut HashMap<usize, usize>,
    ) {
        let mut cur_row = base_row;
        multiline_offsets.insert(cur_row, 0);
        // move chars across one at a time and update offsets as approperiate
        let mut new_node_text = String::from("");
        let mut prev_char_is_backslash = false;

        for ch in node_text.chars() {
            if ch == '\n' {
                if !prev_char_is_backslash {
                    new_node_text.push('\\');
                    new_node_text.push('n');
                } else {
                    new_node_text.pop();
                }

                cur_row += 1;
                multiline_offsets.insert(cur_row, new_node_text.len());
            } else {
                new_node_text.push(ch);
            }
            prev_char_is_backslash = ch == '\\';
        }
        node_text.clear();
        node_text.push_str(new_node_text.as_str());
        if self.is_triple_quote_multiline(node_text) {
            *prev_idx += 2;
        }
    }

    fn handle_format_string_escape_sequence(&mut self, maybe_escape_sequence: &Node) -> usize {
        let escape_sequence = self.get_text(maybe_escape_sequence);
        if escape_sequence.to_uppercase().starts_with("\\U") {
            let escape_sequence = self.get_text(maybe_escape_sequence);
            let unicode = u32::from_str_radix(&escape_sequence[2..], 16).unwrap();

            // correctly determine the amount to offset based on the number of bytes consumed by the unicode character...
            // see also: https://en.wikipedia.org/wiki/UTF-8#Encoding
            match char::from_u32(unicode).unwrap().len_utf8() {
                2 => 4,
                4 => 6,
                _ => 3,
            }
        } else if escape_sequence.starts_with("\\x") {
            2
        } else {
            0
        }
    }

    /// walk all interpolated nodes and
    /// push each one to expressions as FormattedValue's
    /// push any intervening string chunks to expressions as strings
    /// but watch out for unicode escape_sequences as these are to be treated as an offset
    fn handle_format_string_interpolation_node_inplace<'a>(
        &mut self,
        maybe_interpolation_node: &'a Node<'a>,
        expressions: &mut Vec<Expr>,
        base_col: usize,
        base_row: usize,
        unicode_offset: &mut usize,
        unicode_offset_since_last_interpolation: &mut usize,
        prev_idx: &mut usize,
        is_multiline: bool,
        node_text: &str,
        apostrophe_or_quote: &String,
        multiline_offsets: &HashMap<usize, usize>,
        current_row: &mut Option<usize>,
        prev_end_row: &mut usize,
    ) -> ErrorableResult<()> {
        let start_row = maybe_interpolation_node.start_position().row;

        // unicode_offset only applies to entries on the same row as the unicode character
        // after this the offset should no longer apply:
        if let Some(some_current_row) = current_row {
            if start_row != *some_current_row {
                *unicode_offset = 0;
                *unicode_offset_since_last_interpolation = 0;
                *current_row = Some(start_row);
            }
        } else {
            *current_row = Some(start_row);
        }

        if maybe_interpolation_node.kind() == "string_content" {
            for maybe_escape_sequence in maybe_interpolation_node.named_children(self.filtered_cst)
            {
                if maybe_escape_sequence.kind() == "escape_sequence" {
                    let offset = self.handle_format_string_escape_sequence(maybe_escape_sequence);
                    *unicode_offset += offset;
                    *unicode_offset_since_last_interpolation += offset;
                }
            }
        } else if maybe_interpolation_node.kind() == "interpolation" {
            let end_row = maybe_interpolation_node.end_position().row;
            let mut start_col = maybe_interpolation_node.start_position().column;
            let mut end_col = maybe_interpolation_node.end_position().column;

            start_col -= *unicode_offset;
            end_col -= *unicode_offset;

            if is_multiline && start_row > base_row {
                // if multiline, we need line column offset adjustment
                // might be a CPython bug
                start_col += multiline_offsets.get(&start_row).unwrap();
                end_col += multiline_offsets.get(&end_row).unwrap();
            } else if is_multiline && start_row == base_row && start_row != end_row {
                // if multiline and interpolation brackets begin in first line and
                // end in further lines, we need line column adjustment for ending row
                start_col -= base_col;
                end_col += multiline_offsets.get(&end_row).unwrap();
            } else {
                // single line f-literal or first line of multiline f-literal
                // => decrease by code before the f-literal
                start_col -= base_col;
                end_col -= base_col;
            }

            // add next FormattedValue corresponding to {} region
            let interpolation_expression = maybe_interpolation_node
                .child(self.filtered_cst, 1)
                .expect("expression node of interpolation node");

            let mut format_spec: Option<Expr> = None;
            let mut conversion: Option<isize> = Some(-1);
            let mut has_equals = false;

            // format_specifier and/or type_conversion may be specified for interpolation_node
            self.extract_interpolation_node_optionals(
                maybe_interpolation_node,
                &mut format_spec,
                &mut conversion,
                &mut has_equals,
            );

            let value = self.expression(interpolation_expression)?;
            let start_offset = if base_row == start_row { base_col } else { 0 };
            let end_offset = if base_row == end_row { base_col } else { 0 };
            if start_col > *prev_idx {
                // indicates that there is a string at one of the following two locations
                // start of the f-string before the first {} (formatted value)
                // in between two {}'s
                // strings after the last {} are handled after iterating through
                // the interpolation nodes
                let mut string_before_tidy_braces =
                    self.tidy_double_braces(node_text[*prev_idx..start_col].to_string());

                // foo{x=} expands to foox={x}, so the end position of the prefix string needs to be extended
                let mut equals_offset = 0;
                if has_equals {
                    let interpolation_contents_equals = interpolation_expression
                        .utf8_text(self.parser.code.as_bytes())
                        .expect("Could not fetch param name");
                    string_before_tidy_braces =
                        string_before_tidy_braces + interpolation_contents_equals + "=";
                    equals_offset = interpolation_contents_equals.len() as isize + 2;
                }
                // in multiline string allowed to have " inside """ """
                // so we have to "\"" or '\'' respectively to correctly preprocess substring
                if is_multiline {
                    string_before_tidy_braces = string_before_tidy_braces.replace(
                        apostrophe_or_quote,
                        &format!("{}{}", "\\", apostrophe_or_quote),
                    );
                }
                let string_desc = self.process_string(
                    format!(
                        "{}{}{}",
                        apostrophe_or_quote, string_before_tidy_braces, apostrophe_or_quote
                    ),
                    maybe_interpolation_node,
                );
                let start_offset = if base_row == *prev_end_row {
                    base_col
                } else {
                    0
                };
                // unicode_offset includes the unicode characters in the substring immediately preceding the current interpolation
                let prefix_unicode_offset =
                    *unicode_offset - *unicode_offset_since_last_interpolation;
                expressions.push(Expr::new(
                    string_desc,
                    *prev_end_row as isize + 1,
                    (*prev_idx - multiline_offsets.get(prev_end_row).unwrap_or(&0)
                        + start_offset
                        + prefix_unicode_offset) as isize,
                    start_row as isize + 1,
                    maybe_interpolation_node.start_position().column as isize + equals_offset,
                ));
            } else if has_equals {
                // Create new const expression for cases like {x=} with no preceding literal substring
                // the new const expression has the same position as the variable name except the ending
                // column is incremented by 1 to account for the =
                let expr = interpolation_expression
                    .utf8_text(self.parser.code.as_bytes())
                    .expect("Could not fetch param name")
                    .to_string()
                    + "=";
                let expr = self.string(format!("'{}'", expr), false);
                expressions.push(Expr::new(
                    expr,
                    value.lineno,
                    value.col_offset,
                    value.end_lineno.unwrap(),
                    value.end_col_offset.unwrap() + 1,
                ));
            }
            let formatted_value = Expr::new(
                ExprDesc::FormattedValue {
                    value,
                    conversion,
                    format_spec,
                },
                start_row as isize + 1,
                (start_col - multiline_offsets.get(&start_row).unwrap_or(&0)
                    + start_offset
                    + *unicode_offset) as isize,
                end_row as isize + 1,
                (end_col - multiline_offsets.get(&end_row).unwrap_or(&0)
                    + end_offset
                    + *unicode_offset) as isize,
            );
            expressions.push(formatted_value);
            *prev_end_row = end_row;
            *prev_idx = end_col;
            *unicode_offset_since_last_interpolation = 0;
        }
        Ok(())
    }

    //
    // string: $ => seq(
    //    field('prefix', alias($._string_start, '"')),
    //    repeat(choice(
    //      field('interpolation', $.interpolation),
    //      field('string_content', $.string_content)
    //    )),
    //    field('suffix', alias($._string_end, '"'))
    //  ),
    //
    //
    // string_content: $ => prec.right(0, repeat1(
    //    choice(
    //      $._escape_interpolation,
    //      $.escape_sequence,
    //      $._not_escape_sequence,
    //      $._string_content
    //    ))),
    //
    //  interpolation: $ => seq(
    //    token.immediate('{'),
    //    field('expression', $._f_expression),
    //    optional('='),
    //    optional(field('type_conversion', $.type_conversion)),
    //    optional(field('format_specifier', $.format_specifier)),
    //    '}'
    //  ),
    // ),
    // origin_node will be different from format_node where the format string exists within
    // a concatenated string
    // 1. Check if this f string is multiline
    // 2. Calculate offsets for each row in multiline string
    // 3. Iterate by string collecting: string before {}, strings in {} and strings after
    // 4. All subparts pushed respectively to expressions
    fn format_string<'a>(
        &mut self,
        format_node: &'a Node<'a>,
        origin_node: &'a Node<'a>,
        node_text: String,
    ) -> ErrorableResult<ExprDesc> {
        let mut expressions: Vec<Expr> = vec![];

        let quotes = ['\'', '"'];
        let quote_idx = node_text.chars().position(|c| quotes.contains(&c)).unwrap();
        let apostrophe_or_quote = node_text[quote_idx..quote_idx + 1].to_string();

        let mut prev_idx = string_prefix(&node_text).len() + 1;
        let mut node_text = node_text;
        let mut multiline_offsets = HashMap::new();
        let base_row = format_node.start_position().row;
        let base_col = format_node.start_position().column;

        let is_multiline = node_text.contains('\n') || self.is_triple_quote_multiline(&node_text);
        if is_multiline {
            self.format_multiline_string_node_text_inplace(
                &mut node_text,
                base_row,
                &mut prev_idx,
                &mut multiline_offsets,
            );
        }

        let mut has_interpolation_nodes = false;

        let mut unicode_offset: usize = 0;
        let mut unicode_offset_since_last_interpolation: usize = 0;
        let mut current_row = None;
        let mut prev_end_row = base_row;
        for interpolation_node in format_node.named_children(self.filtered_cst) {
            if interpolation_node.kind() == "interpolation" {
                has_interpolation_nodes = true;
            }
            self.handle_format_string_interpolation_node_inplace(
                interpolation_node,
                &mut expressions,
                base_col,
                base_row,
                &mut unicode_offset,
                &mut unicode_offset_since_last_interpolation,
                &mut prev_idx,
                is_multiline,
                &node_text,
                &apostrophe_or_quote,
                &multiline_offsets,
                &mut current_row,
                &mut prev_end_row,
            )?;
        }
        // adjusted_node_text is only required in case if there is leftover string
        // dropping last " symbol
        let mut adjusted_node_text = node_text[..node_text.len() - 1].to_string();
        if is_multiline {
            // the characters used to demarcate the end of the string are
            // two characters wider, so we take 2 away: """ vs "
            adjusted_node_text = adjusted_node_text[..adjusted_node_text.len() - 2].to_string();
        }

        if adjusted_node_text.len() > prev_idx {
            let expr = if has_interpolation_nodes {
                // add remainder of string as node at end of format string
                let mut after_last_tidy_braces =
                    self.tidy_double_braces(adjusted_node_text[prev_idx..].to_string());
                // in multiline string allowed to have " inside """ """
                // so we have to "\"" or '\'' respectively to correctly preprocess substring
                if is_multiline {
                    after_last_tidy_braces = after_last_tidy_braces.replace(
                        &apostrophe_or_quote,
                        &format!("{}{}", "\\", apostrophe_or_quote),
                    );
                }
                let string_desc = self.process_string(
                    format!(
                        "{}{}{}",
                        apostrophe_or_quote, after_last_tidy_braces, apostrophe_or_quote
                    ),
                    origin_node,
                );
                let start_offset = if prev_end_row == base_row {
                    base_col
                } else {
                    0
                };
                // don't add the unicode characters inside the current substring to the start position
                let prefix_unicode_offset =
                    unicode_offset - unicode_offset_since_last_interpolation;
                let end_offset = if is_multiline { 3 } else { 1 };
                Expr::new(
                    string_desc,
                    prev_end_row as isize + 1,
                    (prev_idx + prefix_unicode_offset
                        - *multiline_offsets.get(&prev_end_row).unwrap_or(&0)
                        + start_offset) as isize,
                    format_node.end_position().row as isize + 1,
                    format_node.end_position().column as isize - end_offset,
                )
            } else {
                // no interpolation nodes, just treat as normal string and cut of f from start
                let normal_string = self.tidy_double_braces(node_text[1..].to_string());
                let string_desc = self.process_string(normal_string, format_node);
                let start_offset = if is_multiline { 4 } else { 2 };
                let end_offset = if is_multiline { 3 } else { 1 };
                Expr::new(
                    string_desc,
                    format_node.start_position().row as isize + 1,
                    start_offset + format_node.start_position().column as isize,
                    format_node.end_position().row as isize + 1,
                    format_node.end_position().column as isize - end_offset,
                )
            };

            expressions.push(expr);
        }

        Ok(ExprDesc::JoinedStr(expressions))
    }

    /// Interpolation nodes can contain:
    /// a conversion is an integer:
    /// * -1: no formatting
    /// * 115: !s string formatting
    /// * 114: !r repr formatting
    /// * 97: !a ascii formatting
    /// format_spec is a JoinedStr node representing the formatting of the value.
    ///  Both conversion and format_spec can be set at the same time.
    fn extract_interpolation_node_optionals(
        &mut self,
        interpolation_node: &Node,
        format_spec: &mut Option<Expr>,
        conversion: &mut Option<isize>,
        has_equals: &mut bool,
    ) {
        let interpolation_node_count = interpolation_node.child_count();
        *has_equals = false;
        if interpolation_node_count > 3 {
            for node_id in 2..(interpolation_node_count - 1) {
                let interpolation_component_node = &interpolation_node
                    .child(self.filtered_cst, node_id)
                    .expect("interpolation_node child");
                match interpolation_component_node.kind() {
                    "format_specifier" => {
                        let mut format_spec_expressions = vec![];
                        let interpolation_node_str =
                            self.get_text(interpolation_component_node)[1..].to_string();
                        let string_desc =
                            self.string(format!("'{}'", interpolation_node_str,), false);
                        format_spec_expressions.push(Expr::new(
                            string_desc,
                            interpolation_component_node.start_position().row as isize + 1,
                            // +1 to skip the colon
                            interpolation_component_node.start_position().column as isize + 1,
                            interpolation_component_node.end_position().row as isize + 1,
                            interpolation_component_node.end_position().column as isize,
                        ));
                        *format_spec = Some(self.parser.new_expr(
                            ExprDesc::JoinedStr(format_spec_expressions),
                            interpolation_component_node,
                        ));
                    }
                    "type_conversion" => {
                        // the following magic numbers are defined in the Python
                        // language spec:
                        // https://docs.python.org/3.10/library/ast.html#ast.FormattedValue
                        *conversion =
                            Some(match self.get_text(interpolation_component_node).as_str() {
                                "!s" => 115,
                                "!r" => 114,
                                "!a" => 97,
                                _ => -1,
                            });
                    }
                    "=" => *has_equals = true,
                    _ => (),
                }
            }

            // Only set conversion by = if neither explicit conversion nor explicit formatters are set.
            if *has_equals && format_spec.is_none() && conversion == &mut Some(-1isize) {
                *conversion = Some(114)
            }
        }
    }

    /// create one large string from a number contained in a Vec<Expr>
    /// if any ' 's are within the strings then all are wrapped in double
    /// quotes
    fn sew_strings_together(&mut self, strings: Vec<Expr>, conc_str_node: &Node) -> ExprDesc {
        let mut one_big_string = String::from("");
        let mut needs_doublequote = false;
        let mut is_previous_token_byte = false;
        let mut first = true;

        for child_string in strings {
            if let ExprDesc::Constant {
                value: Some(ConstantDesc::Str(astring) | ConstantDesc::ByteStr(astring)),
                kind: _,
            } = &*child_string.desc
            {
                let is_byte = match &*child_string.desc {
                    ExprDesc::Constant {
                        value: Some(ConstantDesc::ByteStr(_)),
                        kind: _,
                    } => true,
                    _ => false,
                };

                if !first && is_byte != is_previous_token_byte {
                    self.record_recoverable_error(
                        RecoverableError::SyntaxError(
                            "cannot mix bytes and nonbytes literals".to_string(),
                        ),
                        conc_str_node,
                    );
                }
                first = false;
                is_previous_token_byte = is_byte;
                let segment = &astring[1..astring.len() - 1].to_string();
                if segment.contains('\'') {
                    needs_doublequote = true;
                }
                one_big_string.push_str(segment);
            }
        }

        let prefix = if is_previous_token_byte { "b" } else { "" };
        let quote_style = if needs_doublequote { "\"" } else { "'" };
        one_big_string = format!("{}{}{}{}", prefix, quote_style, one_big_string, quote_style);
        self.process_string(one_big_string, conc_str_node)
    }

    fn extract_concatenated_strings<'a>(
        &mut self,
        conc_str_node: &'a Node<'a>,
        strings: &mut Vec<Expr>,
    ) -> ErrorableResult<bool> {
        let mut contains_f_string = false;
        for child_string_node in conc_str_node.named_children(self.filtered_cst) {
            let child_string_expr = self.raw_string(child_string_node, conc_str_node)?;
            if let ExprDesc::JoinedStr(_) = child_string_expr {
                contains_f_string = true;
            }
            strings.push(self.parser.new_expr(child_string_expr, child_string_node));
        }
        Ok(contains_f_string)
    }

    // concatenated_string: $ => seq(
    //   $.string,
    //   repeat1($.string)
    // ),
    /// Concatenated strings are just regular strings which are made up of a number
    /// of other strings apread across multiple consecutive lines and
    /// delimited by a \ followed by a newline.
    /// most of the time, if all nodes are regular strings then we need
    /// just return a concatenated string.
    /// BUT if any nodes are an f string then return one big concatenated f string
    /// must be returned. This is achieved by creating a new f string and adding all
    /// f string and regular string components into one large expression list
    /// when joining f strings it is expected that string nodes are to be
    /// merged together to form one large string at the boundry point
    /// f-string([a:FormattedValue, b:FormattedValue, c:str]) + f-string([d:str, e:FormattedValue, f:FormattedValue])
    /// => f-string([a:FormattedValue, b:FormattedValue, concat(c+d):str, e:FormattedValue, f:FormattedValue])
    fn concatenated_string<'a>(
        &mut self,
        conc_str_node: &'a Node<'a>,
    ) -> ErrorableResult<ExprDesc> {
        let mut strings: Vec<Expr> = vec![];
        let contains_f_string = self.extract_concatenated_strings(conc_str_node, &mut strings)?;
        if contains_f_string {
            let mut expressions: Vec<Expr> = vec![];

            for mut child_string in strings {
                if let ExprDesc::JoinedStr(mut child_expressions) = *child_string.desc {
                    if !expressions.is_empty() && !child_expressions.is_empty() {
                        let last_item: &Expr = expressions.last().unwrap();
                        if let ExprDesc::Constant {
                            value: Some(ConstantDesc::Str(_) | ConstantDesc::ByteStr(_)),
                            kind: _,
                        } = &*last_item.desc
                        {
                            let to_add_first_item = child_expressions.first().unwrap();
                            if let ExprDesc::Constant {
                                value: Some(ConstantDesc::Str(_) | ConstantDesc::ByteStr(_)),
                                kind: _,
                            } = &*to_add_first_item.desc
                            {
                                // last item and first are strings, so we need to sew these together
                                let start_line = last_item.lineno;
                                let start_col = last_item.col_offset;
                                let end_line = to_add_first_item.end_lineno.unwrap();
                                let end_col = to_add_first_item.end_col_offset.unwrap();
                                let mut to_sew = vec![];
                                to_sew.push(expressions.pop().unwrap());
                                to_sew.push(child_expressions.remove(0));
                                let sewn_desc = self.sew_strings_together(to_sew, conc_str_node);
                                expressions.push(Expr::new(
                                    sewn_desc, start_line, start_col, end_line, end_col,
                                ));
                                // and the rest of the items can be extended into the expressions list
                            }
                        }
                    }

                    expressions.extend(child_expressions);
                } else if let ExprDesc::Constant {
                    value: Some(ConstantDesc::Str(_) | ConstantDesc::ByteStr(_)),
                    kind: _,
                } = &*child_string.desc
                {
                    // sew previous lines together if both string constants
                    if !expressions.is_empty() {
                        let last_item = expressions.last().unwrap();
                        if let ExprDesc::Constant {
                            value: Some(ConstantDesc::Str(_) | ConstantDesc::ByteStr(_)),
                            kind: _,
                        } = &*last_item.desc
                        {
                            // last item was a string, so lets sew them together
                            let start_line = last_item.lineno;
                            let start_col = last_item.col_offset;
                            let end_line = child_string.end_lineno.unwrap();
                            let end_col = child_string.end_col_offset.unwrap();
                            let mut to_sew = vec![];
                            to_sew.push(expressions.pop().unwrap());
                            to_sew.push(child_string);
                            let sewn_desc = self.sew_strings_together(to_sew, conc_str_node);
                            child_string =
                                Expr::new(sewn_desc, start_line, start_col, end_line, end_col);
                        }
                    }
                    expressions.push(child_string);
                }
            }

            Ok(ExprDesc::JoinedStr(expressions))
        } else {
            Ok(self.sew_strings_together(strings, conc_str_node))
        }
    }

    fn raw_string<'a>(
        &mut self,
        raw_string_node: &'a Node<'a>,
        origin_node: &'a Node<'a>,
    ) -> ErrorableResult<ExprDesc> {
        // collect escape sequences and parse into corresponding unicode characters
        let mut escape_sequences = vec![];
        for string_content_nodes in
            raw_string_node.children_by_field_name(self.filtered_cst, "string_content")
        {
            for child in string_content_nodes.named_children(self.filtered_cst) {
                if child.kind() == "escape_sequence" {
                    let escape_sequence = self.get_text(child);
                    if escape_sequence.to_uppercase().starts_with("\\U")
                        || escape_sequence.starts_with("\\x")
                    {
                        let escape_code = u32::from_str_radix(&escape_sequence[2..], 16).unwrap();
                        let escape_sequence = if escape_code < 32 // unprintable ascii
                            || escape_code == 92  // backslash
                            || escape_code == 173  // soft hyphen
                            || (127..=160).contains(&escape_code)
                        {
                            // code corresponds to a non-printable character, or character
                            // with a special escape sequence. we need to manually create
                            // the new escape sequence
                            match escape_code {
                                9 => String::from("\\t"),
                                10 => String::from("\\n"),
                                13 => String::from("\\r"),
                                92 => String::from("\\\\"),
                                _ => format!("\\x{:02x}", escape_code),
                            }
                        } else {
                            let as_unicode = std::char::from_u32(escape_code);
                            // In cases where escape_code is not a valid u32, it
                            // seems that CPython formats the unicode character
                            // as escape_sequence.to_lowercase() so we do the same
                            match as_unicode {
                                Some(formatted) => format!("{}", formatted),
                                None => escape_sequence.to_lowercase(),
                            }
                        };
                        escape_sequences.push((
                            escape_sequence,
                            child.start_byte() - raw_string_node.start_byte(),
                            child.end_byte() - raw_string_node.start_byte(),
                        ));
                    }
                }
            }
        }
        let string_contents = self.escape_decode_text(raw_string_node, &escape_sequences);
        let categorization = self.categorize_string(&string_contents, origin_node, false);

        if categorization.is_format {
            self.format_string(raw_string_node, origin_node, string_contents)
        } else {
            Ok(self.process_string(string_contents, origin_node))
        }
    }

    /// Retrieves actual text representation of the string, additionally converting any escape
    /// sequences (like \xc3, \xXX) found in the previous steps to actual unicode form
    /// (unless there are no escape sequences or the string is a byte string)
    fn escape_decode_text(
        &mut self,
        raw_string_node: &Node,
        escape_sequences: &[(String, usize, usize)],
    ) -> String {
        let original_contents = self.get_text(raw_string_node);

        if escape_sequences.is_empty()
            || self
                .categorize_string(&original_contents, raw_string_node, false)
                .is_byte
        {
            original_contents
        } else {
            // substitute the new characters
            let mut string_contents = String::new();
            let mut left = 0;
            for (escape_sequence, start_byte, end_byte) in escape_sequences.iter() {
                string_contents.push_str(&original_contents[left..*start_byte]);
                string_contents.push_str(escape_sequence);
                left = *end_byte;
            }
            string_contents.push_str(&original_contents[left..]);
            string_contents
        }
    }

    // - Add an extra '\' for non special characters which are escaped (e.g. \c => \\c)
    //   Equivalent to matching Regex::new(r#"(?<!^)(?<!\)\(\\)*([^0-7abfnrtuvxNU '"\])"#) and replacing by \\\\${1}${2}
    //   i.e. <not beginning of string nor '\'><odd number of '\'><non escapable Python char>
    // - Translate hexadecimal characters which are not escaped (e.g. \0 => \x00)
    fn process_escaped_chars(&mut self, string_contents: String) -> String {
        let mut new_node_text: String = String::from("");
        let mut octal_seq: String = String::from("");
        let mut is_escaped: bool;
        let mut prev_backslashes = 0;

        for ch in string_contents.chars() {
            is_escaped = prev_backslashes % 2 == 1;

            // Start of a 1-3 escaped digits sequence denoting an octal character to translate e.g. '\1' == '\01' == '\001' => "\x01",
            if is_escaped && octal_seq.len() < 3 && ('0'..'8').contains(&ch) {
                if octal_seq.is_empty() {
                    new_node_text.pop();
                }
                octal_seq.push(ch);
                continue;
            // End of octal sequence
            } else if is_escaped
                && !octal_seq.is_empty()
                && (!('0'..'8').contains(&ch) || octal_seq.len() == 3)
            {
                if let Some(octal_conv) = OCTAL_MAP.get(&octal_seq.parse::<u16>().unwrap()) {
                    new_node_text.push_str(octal_conv);
                }
                octal_seq = String::from("");
                prev_backslashes = 0;
                is_escaped = false;
            }

            // Backslash counter
            if ch == '\\' {
                prev_backslashes += 1;
                new_node_text.push(ch);
            } else {
                // Non special char
                if !SPECIAL_CHARS.contains(&ch) {
                    if is_escaped {
                        // For non special chars escaped (odd number of '\', add an additional '\' to result in an even number of '\' (not escaped)
                        new_node_text.push('\\');
                    }
                    new_node_text.push(ch);
                // Hexa chars which are not escaped are translated, e.g. '\a' => "\x07",
                } else if let Some(hex_str) = HEXA_CONVERSION.get(&ch) {
                    if is_escaped {
                        for hex_ch in hex_str.chars() {
                            new_node_text.push(hex_ch);
                        }
                    } else {
                        new_node_text.push(ch);
                    }
                // Special chars left as is
                } else {
                    new_node_text.push(ch);
                }
                prev_backslashes = 0;
            }
        }

        new_node_text.to_string()
    }

    /// categorize_string will attempt to figure out what th prefix is of a string
    /// Failures are logged as recoverable and the string is otherwise categorized
    /// As a vanilla string with no prefix
    fn categorize_string(
        &mut self,
        string_with_prefix: &str,
        node: &Node,
        record_invalid_prefix: bool,
    ) -> StringCategory {
        match categorize_string(string_with_prefix) {
            Ok(string_category) => string_category,
            Err(invalid_string_category_error) => {
                if record_invalid_prefix {
                    self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "Invalid string prefix: {:?}",
                            invalid_string_category_error.invalid_prefix
                        )),
                        node,
                    );
                }

                StringCategory {
                    ..Default::default()
                }
            }
        }
    }

    /// process_string performs the following:
    ///  1. We check if a string is prefixed with a 'b' or 'r', if so
    ///  this is stripped out and added back as a prefix to the output of the
    ///  following steps (except 'r' which is thrown away)...
    ///  2. We consider '\' at the end of a line, which is treated as a line
    ///  continuation (ignored), or as a literal '\' for raw strings (prefixed with 'r')
    ///  3. We convert multiline strings to single line strings (denoted
    ///  via single quote marks) and insert newlines with escapes for newlines
    ///  4. We remove all quote mark escape characters \' -> ', \" -> "
    ///  5. Strings created via double quotes ("") are converted to single
    ///  quote (''), unless they contain a single quote (') or if they contain both a
    ///  single quote (') and a double quote (", originally \" but \ is removed in step
    ///  4). Whereas strings created with single quotes ('') containing other single
    ///  quotes but no double quotes (") are converted to double quoted strings.
    ///  6. We add escape characters again if needed (' -> \' or " -> \"). In practice,
    ///  this is relevant only when the the string contains both single (') and double
    ///  (") quotes.
    fn process_string(&mut self, string_contents: String, node: &Node) -> ExprDesc {
        // TODO: this method is getting quite unweildly, we should refactor it.
        let mut string_contents = string_contents;
        let categorization = self.categorize_string(&string_contents, node, true);

        let byte = categorization.is_byte;
        let raw = categorization.is_raw;

        if raw || byte {
            string_contents = string_contents[1..].to_string();
        }
        let double_quote_string = string_contents.starts_with('\"');
        let is_triple_quote = self.is_triple_quote_multiline(&string_contents);
        if raw {
            string_contents = string_contents.replace('\\', "\\\\");
        } else {
            let mut new_string_contents = String::from("");
            let mut prev_backslashes = 0;
            for (i, ch) in string_contents.chars().enumerate() {
                // Drop \n when prefixed by the line continuation char \ (and drops the continuation char)
                // except when the line continuation char is itself escaped (edge case for multiline strings only)
                if ch == '\n' && prev_backslashes % 2 == 1 {
                    new_string_contents.pop();
                } else {
                    // replace \' by ' and \" by " except at the end
                    if prev_backslashes > 0 && (ch == '\'' || ch == '"') {
                        if i < string_contents.len() - 3
                            || (!is_triple_quote && i < string_contents.len() - 1)
                        {
                            new_string_contents.pop();
                        }
                    }
                    new_string_contents.push(ch);
                }
                if ch == '\\' {
                    prev_backslashes += 1;
                } else {
                    prev_backslashes = 0;
                }
            }
            string_contents = new_string_contents;
        }

        string_contents = self.process_escaped_chars(string_contents);

        string_contents = string_contents.replace('\n', "\\n").replace('\t', "\\t");
        // check is line multiline string and replace with single double quote
        if self.is_triple_quote_multiline(&string_contents) {
            string_contents = string_contents[2..string_contents.len() - 2].to_string();
        }

        string_contents = {
            // convert string to being wrapped in '' (or "") unless there are inner ' s (or " s)
            if string_contents.starts_with('\"')
                && (!string_contents.contains('\'')
                    || string_contents.contains('\'')
                        && string_contents[1..string_contents.len() - 1].contains('\"'))
                && string_contents.len() > 1
            {
                string_contents = string_contents[1..string_contents.len() - 1].to_string();
                format!("'{}'", string_contents)
            } else if string_contents.starts_with('\'')
                && !string_contents.contains('\"')
                && string_contents.len() > 1
            {
                // wrap only if there is a ' in the middle of the string
                // e.g. '\\''  -> "\\'"
                let index: Option<usize> = string_contents[1..].find('\'').map(|i| i + 1);
                match index {
                    Some(i) if i > 0 && i < string_contents.len() - 1 => {
                        string_contents = string_contents[1..string_contents.len() - 1].to_string();

                        format!("\"{}\"", string_contents)
                    }
                    _ => string_contents,
                }
            } else {
                string_contents
            }
        };

        string_contents = {
            if string_contents.starts_with('\"') && string_contents.len() > 1 {
                string_contents = string_contents[1..string_contents.len() - 1].to_string();
                string_contents = string_contents.replace('\"', "\\\"");
                if !raw && double_quote_string {
                    // \' revert back to \\' when located in a double quote string
                    string_contents = string_contents.replace("\\'", "\\\\'");
                }
                format!("\"{}\"", string_contents)
            } else if string_contents.len() > 1 {
                string_contents = string_contents[1..string_contents.len() - 1].to_string();
                string_contents = string_contents.replace('\'', "\\\'");
                if !raw && !double_quote_string {
                    // \" revert back to \\" when located in a single quote string
                    string_contents = string_contents.replace("\\\"", "\\\\\"");
                }
                format!("'{}'", string_contents)
            } else {
                string_contents
            }
        };

        self.string(string_contents, byte)
    }

    // string: $ => seq(
    //  field('prefix', $.string_start),
    //  repeat(choice(
    //    field('interpolation', $.interpolation),
    //    field('string_content', $.string_content)
    //  )),
    //  field('suffix', $.string_end)
    //),
    fn string(&mut self, const_value: String, is_byte_string: bool) -> ExprDesc {
        ExprDesc::Constant {
            value: Some(if is_byte_string {
                ConstantDesc::ByteStr(const_value)
            } else {
                ConstantDesc::Str(const_value)
            }),
            kind: None,
        }
    }

    fn parse_integer_from_str_radix(
        &mut self,
        const_value: &str,
        node: &Node,
        radix: u32,
        variant_name: &str,
    ) -> ErrorableResult<isize> {
        match isize::from_str_radix(&const_value[2..], radix) {
            Ok(value) => Ok(value),
            Err(error_msg) => Err(self.record_recoverable_error(
                RecoverableError::UnexpectedExpression(format!(
                    "cannot parse integer (Not a binary {:?}): {:?} as {:?}",
                    variant_name, const_value, error_msg
                )),
                node,
            )),
        }
    }

    // integer: $ => token(choice(
    //   seq(
    //     choice('0x', '0X'),
    //     repeat1(/_?[A-Fa-f0-9]+/),
    //   ),
    //   seq(
    //     choice('0o', '0O'),
    //     repeat1(/_?[0-7]+/),
    //   ),
    //   seq(
    //     choice('0b', '0B'),
    //     repeat1(/_?[0-1]+/),
    //   ),
    //   seq(
    //     repeat1(/[0-9]+_?/),
    //     choice(
    //       optional(/[Ll]/), // long numbers
    //       optional(/[jJ]/) // complex numbers
    //     )
    //   )
    // )),
    fn integer(&mut self, node: &Node) -> ErrorableResult<ExprDesc> {
        let const_value = self.get_text(node).replace('_', "");

        let integer_value = if const_value.starts_with("0b") || const_value.starts_with("0B") {
            // binary integer
            self.parse_integer_from_str_radix(&const_value, node, 2, "binary")?
        } else if const_value.starts_with("0x") || const_value.starts_with("0X") {
            // hexadecimal integer
            self.parse_integer_from_str_radix(&const_value, node, 16, "hexadecimal")?
        } else if const_value.starts_with("0o") || const_value.starts_with("0O") {
            // octal integer
            self.parse_integer_from_str_radix(&const_value, node, 8, "octal")?
        } else {
            if const_value.ends_with('j') || const_value.ends_with('J') {
                // imaginary part of complex numbers are always parsed like floats
                // even when they have an integer component as here
                return self.float(node);
            }

            match const_value.parse::<isize>() {
                Ok(value) => value,
                Err(ref e) if *e == self.integer_overflow_error => {
                    // TODO: use ParseIntError.kind() to detect integer overflow of
                    // parse of const_value when Meta is on rust 2022.
                    // In rust 2021 ParseIntError.kind is private
                    // For now, store an overflow Err from parsing a large integer
                    // Adapted from https://github.com/rust-lang/rust/issues/22639
                    // and https://github.com/uutils/coreutils/pull/2882/
                    return Ok(ExprDesc::Constant {
                        value: Some(ConstantDesc::Num(Num::BigInt(const_value))),
                        kind: None,
                    });
                }
                Err(error_msg) => {
                    return Err(self.record_recoverable_error(
                        RecoverableError::UnexpectedExpression(format!(
                            "cannot parse integer: {:?} as {:?}",
                            const_value, error_msg
                        )),
                        node,
                    ));
                }
            }
        };

        Ok(ExprDesc::Constant {
            value: Some(ConstantDesc::Num(Num::Int(integer_value))),
            kind: None,
        })
    }

    // float: $ => {
    //   const digits = repeat1(/[0-9]+_?/);
    //   const exponent = seq(/[eE][\+-]?/, digits)

    //   return token(seq(
    //     choice(
    //       seq(digits, '.', optional(digits), optional(exponent)),
    //       seq(optional(digits), '.', digits, optional(exponent)),
    //       seq(digits, exponent)
    //     ),
    //     optional(choice(/[Ll]/, /[jJ]/))
    //   ))
    // },
    fn float(&mut self, node: &Node) -> ErrorableResult<ExprDesc> {
        let mut const_value = self.get_text(node).replace('_', "");

        let is_complex = const_value.ends_with('j') || const_value.ends_with('J');
        if is_complex {
            const_value = const_value[0..const_value.len() - 1].to_string();
        }

        let float_value = match const_value.parse::<f64>() {
            Ok(value) => value,
            Err(error_msg) => {
                return Err(self.record_recoverable_error(
                    RecoverableError::UnexpectedExpression(format!(
                        "cannot parse float: {:?} as {:?}",
                        const_value, error_msg
                    )),
                    node,
                ));
            }
        };

        Ok(ExprDesc::Constant {
            value: Some(ConstantDesc::Num(if is_complex {
                Num::Complex(float_value)
            } else {
                Num::Float(float_value)
            })),
            kind: None,
        })
    }

    ///
    /// Will return an identifier as a String and will record a
    /// recoverable error if the identifier is invalid
    /// (e.g. keyword, empty space etc)
    fn get_valid_identifier(&mut self, node: &Node) -> String {
        let identifier: String = self.get_text(node);
        self.check_identifier_valid(&identifier, node);
        identifier
    }

    ///
    /// Will record a recoverable error if the identifier
    /// is invalid (e.g. keyword, empty space etc)
    fn check_identifier_valid(&mut self, identifier: &String, node: &Node) {
        if self.python_keywords.contains(identifier) {
            self.record_recoverable_error(
                RecoverableError::SyntaxError(format!(
                    "keyword: {:?} cannot be used as identifier",
                    identifier
                )),
                node,
            )
        } else if identifier.is_empty() {
            self.record_recoverable_error(
                RecoverableError::SyntaxError(
                    "empty string cannot be used as identifier".to_string(),
                ),
                node,
            )
        }
    }

    fn name(&mut self, node: &Node) -> ExprDesc {
        let identifier: String = self.get_valid_identifier(node);

        ExprDesc::Name {
            id: identifier,
            ctx: self.get_expression_context(),
        }
    }

    // Get a copy of the source code behind this node.
    // For identifiers that is the identifer name.
    fn get_text(&self, node: &Node) -> String {
        get_node_text(&self.parser.code, node)
    }
}

pub fn get_node_text(code: &String, node: &Node) -> String {
    node.utf8_text(code.as_bytes())
        .expect("Invalid Identifier") // deal with errors
        .to_string()
}

fn get_comp_op(operator: &NodeType) -> Option<Cmpop> {
    match operator {
        NodeType::ComparisonOperator(cmp_operator) => Some(match cmp_operator {
            ComparisonOperator::LESS_THAN => Cmpop::Lt,
            ComparisonOperator::LESS_THAN_EQUAL => Cmpop::LtE,
            ComparisonOperator::EQUAL => Cmpop::Eq,
            ComparisonOperator::NOT_EQUAL => Cmpop::NotEq,
            ComparisonOperator::GREATER_THAN_EQUAL => Cmpop::GtE,
            ComparisonOperator::GREATER_THAN => Cmpop::Gt,
            ComparisonOperator::IN | ComparisonOperator::NOT | ComparisonOperator::IS => {
                panic!("unexpected comparison operator node {:?}", cmp_operator)
            }
        }),
        NodeType::Keyword(Keyword::IN) => Some(Cmpop::In),
        NodeType::Keyword(Keyword::IS) => Some(Cmpop::Is),
        NodeType::Keyword(Keyword::IS_NOT) => Some(Cmpop::IsNot),
        NodeType::Keyword(Keyword::NOT_IN) => Some(Cmpop::NotIn),
        _ => None,
    }
}

impl TryFrom<BinaryOperator> for Operator {
    type Error = FromBinaryOperatorError;
    fn try_from(operator: BinaryOperator) -> Result<Operator, Self::Error> {
        match operator {
            BinaryOperator::AT => Ok(Self::MatMult),
            BinaryOperator::BITWISE_AND => Ok(Self::BitAnd),
            BinaryOperator::BITWISE_OR => Ok(Self::BitOr),
            BinaryOperator::BITWISE_XOR => Ok(Self::BitXor),
            BinaryOperator::LEFT_SHIFT => Ok(Self::LShift),
            BinaryOperator::RIGHT_SHIFT => Ok(Self::RShift),
            BinaryOperator::PLUS => Ok(Self::Add),
            BinaryOperator::MINUS => Ok(Self::Sub),
            BinaryOperator::STAR => Ok(Self::Mult),
            BinaryOperator::PERCENT => Ok(Self::Mod),
            BinaryOperator::SLASH => Ok(Self::Div),
            BinaryOperator::DOUBLE_SLASH => Ok(Self::FloorDiv),
            BinaryOperator::DOUBLE_STAR => Ok(Self::Pow),
            _ => Err(FromBinaryOperatorError(operator)),
        }
    }
}

#[derive(Copy, Clone, Debug, thiserror::Error)]
#[error("invalid binary operator: {0:?}")]
pub struct FromBinaryOperatorError(BinaryOperator);

impl From<AugAssignOperator> for Operator {
    fn from(operator: AugAssignOperator) -> Operator {
        match operator {
            AugAssignOperator::AT_EQUAL => Self::MatMult,
            AugAssignOperator::BITWISE_AND_EQUAL => Self::BitAnd,
            AugAssignOperator::BITWISE_OR_EQUAL => Self::BitOr,
            AugAssignOperator::BITWISE_XOR_EQUAL => Self::BitXor,
            AugAssignOperator::LEFT_SHIFT_EQUAL => Self::LShift,
            AugAssignOperator::RIGHT_SHIFT_EQUAL => Self::RShift,
            AugAssignOperator::PLUS_EQUAL => Self::Add,
            AugAssignOperator::MINUS_EQUAL => Self::Sub,
            AugAssignOperator::STAR_EQUAL => Self::Mult,
            AugAssignOperator::PERCENT_EQUAL => Self::Mod,
            AugAssignOperator::SLASH_EQUAL => Self::Div,
            AugAssignOperator::DOUBLE_SLASH_EQUAL => Self::FloorDiv,
            AugAssignOperator::DOUBLE_STAR_EQUAL => Self::Pow,
        }
    }
}
