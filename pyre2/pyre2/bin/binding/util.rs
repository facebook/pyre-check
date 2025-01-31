/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Various utilities for examining pieces of syntax.

use std::sync::LazyLock;

use regex::Regex;
use ruff_python_ast::Expr;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtExpr;

use crate::config::Config;

/// Given the body of a function, what are the potential statements that
/// could be the last ones to be executed, where the function then falls off the end.
///
/// * Return None to say we can't really figure it out (e.g. parse error, empty statements).
/// * Return Some([]) to say that we can never reach the end (e.g. always return, raise)
/// * Return Some(xs) to say this set might be the last statement.
pub fn function_last_statements<'a>(x: &'a [Stmt], config: &Config) -> Option<Vec<&'a Stmt>> {
    match x.last() {
        None => None,
        Some(Stmt::Return(_)) => Some(Vec::new()),
        Some(Stmt::If(x)) => {
            let mut res = Vec::new();
            for (_, body) in config.pruned_if_branches(x) {
                res.extend(function_last_statements(body, config)?);
            }
            Some(res)
        }
        Some(x) => Some(vec![x]),
    }
}

pub fn is_ellipse(x: &[Stmt]) -> bool {
    match x {
        [
            Stmt::Expr(StmtExpr {
                value: box Expr::EllipsisLiteral(_),
                ..
            }),
        ] => true,
        _ => false,
    }
}

static IDENTIFIER_REGEX: LazyLock<Regex> =
    LazyLock::new(|| Regex::new("^[a-zA-Z_][a-zA-Z0-9_]*$").unwrap());

pub fn is_valid_identifier(name: &str) -> bool {
    IDENTIFIER_REGEX.is_match(name)
}
