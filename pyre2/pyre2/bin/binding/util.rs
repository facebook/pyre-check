/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Various utilities for examining pieces of syntax.

use std::sync::LazyLock;

use regex::Regex;
use ruff_python_ast::ExceptHandler;
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
    fn f<'a>(config: &Config, x: &'a [Stmt], res: &mut Vec<&'a Stmt>) -> Option<()> {
        match x.last()? {
            Stmt::Return(_) | Stmt::Raise(_) => {}
            stmt @ Stmt::If(x) => {
                let mut last_test = None;
                for (test, body) in config.pruned_if_branches(x) {
                    last_test = test;
                    f(config, body, res)?;
                }
                if last_test.is_some() {
                    // The final `if` can fall through, so the `if` itself might be the last statement.
                    res.push(stmt);
                }
            }
            Stmt::Try(x) => {
                if !x.finalbody.is_empty() {
                    f(config, &x.finalbody, res)?;
                } else {
                    if x.orelse.is_empty() {
                        f(config, &x.body, res)?;
                    } else {
                        f(config, &x.orelse, res)?;
                    }
                    for handler in &x.handlers {
                        match handler {
                            ExceptHandler::ExceptHandler(x) => f(config, &x.body, res)?,
                        }
                    }
                    // If we don't have a matching handler, we raise an exception, which is fine.
                }
            }
            x => res.push(x),
        }
        Some(())
    }

    let mut res = Vec::new();
    f(config, x, &mut res)?;
    Some(res)
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
