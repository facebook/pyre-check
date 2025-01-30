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

/// Does this sequence of statements every potentially fall off the end.
/// Assume there are not statements after a Never (which would be redundant).
/// Assumes this is not inside a loop body.
///
/// * Return None to say this is not a never.
/// * Return Some(xs) to say if all these statements are never, it is a never.
///
/// then it is a never if the sequence of collected statements is never.
pub fn is_never<'a>(x: &'a [Stmt], config: &Config) -> Option<Vec<&'a Stmt>> {
    match x.last() {
        None => None,
        Some(Stmt::Return(_)) => Some(Vec::new()),
        Some(Stmt::If(x)) => {
            let mut res = Vec::new();
            for (_, body) in config.pruned_if_branches(x) {
                res.extend(is_never(body, config)?);
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
