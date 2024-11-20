//! Various utilities for examining pieces of syntax.

use ruff_python_ast::Expr;
use ruff_python_ast::Stmt;
use ruff_python_ast::StmtExpr;

use crate::ast::Ast;
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
            for (test, body) in Ast::if_branches(x) {
                let b = config.evaluate_bool_opt(test);
                if b != Some(false) {
                    res.extend(is_never(body, config)?);
                }
                if b == Some(true) {
                    break;
                }
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
