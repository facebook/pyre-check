// Copyright (c) Meta Platforms, Inc. and affiliates.
//
// This source code is licensed under the MIT license found in the
// LICENSE file in the root directory of this source tree

#[derive(Debug, thiserror::Error)]
pub enum ParserError {
    #[error("failed to set tree_sitter language: {0}")]
    Language(#[from] tree_sitter::LanguageError),
    #[error("parser timed out or was cancelled")]
    DidNotComplete,
}

#[derive(Debug, thiserror::Error)]
pub enum RecoverableError {
    #[error("encountered unexpected expression")]
    UnexpectedExpression(String),
    #[error("encountered unimplemented expression")]
    UnimplementedStatement(String),
    #[error("expected a node, but it was missing")]
    MissingChild, // TODO: add String parameter to state which child is missing
    #[error("expected comparison to have lhs node, but it was missing")]
    MissingLhs,
    #[error("expected BinaryOperator node, but got unexpected node kind: {0}")]
    MissingOperator(String),
    #[error("Syntax error resulted in ERROR node")]
    SyntaxError(String),
}

pub fn recoverable_error_to_string(recoverable_error: &RecoverableError) -> String {
    match recoverable_error {
        RecoverableError::UnexpectedExpression(expression_name) => {
            format!("UnexpectedExpression: {}", expression_name)
        }
        RecoverableError::UnimplementedStatement(statement_name) => {
            format!("UnimplementedStatement: {}", statement_name)
        }
        RecoverableError::MissingChild => "MissingChild".to_string(),
        RecoverableError::MissingLhs => "MissingLhs".to_string(),
        RecoverableError::MissingOperator(operator) => {
            format!("MissingOperator: {}", operator)
        }
        RecoverableError::SyntaxError(invalid_syntax) => {
            format!("SyntaxError: {}", invalid_syntax)
        }
    }
}
