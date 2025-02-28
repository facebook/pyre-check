/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/*
 * Most function calls are resolved by converting the callee to a CallTarget and
 * calling AnswersSolver::call_infer with the call target and the arguments. This
 * file contains the implementations of a few special calls that need to be hard-coded.
 */

use ruff_python_ast::Expr;
use ruff_python_ast::Keyword;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::error::collector::ErrorCollector;
use crate::error::kind::ErrorKind;
use crate::types::callable::unexpected_keyword;
use crate::types::types::Type;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    pub fn call_assert_type(
        &self,
        args: &[Expr],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if args.len() == 2 {
            let expr_a = &args[0];
            let expr_b = &args[1];
            let a = self.expr_infer(expr_a, errors);
            let b = self.expr_untype(expr_b, errors);
            let a = self.canonicalize_all_class_types(
                self.solver().deep_force(a).explicit_any().anon_callables(),
                expr_a.range(),
            );
            let b = self.canonicalize_all_class_types(
                self.solver().deep_force(b).explicit_any().anon_callables(),
                expr_b.range(),
            );
            if a != b {
                self.error(
                    errors,
                    range,
                    ErrorKind::Unknown,
                    format!(
                        "assert_type({}, {}) failed",
                        a.deterministic_printing(),
                        b.deterministic_printing()
                    ),
                );
            }
        } else {
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                format!(
                    "assert_type needs 2 positional arguments, got {:#?}",
                    args.len()
                ),
            );
        }
        for keyword in keywords {
            unexpected_keyword(
                &|msg| {
                    self.error(errors, range, ErrorKind::Unknown, msg);
                },
                "assert_type",
                keyword,
            );
        }
        Type::None
    }

    pub fn call_reveal_type(
        &self,
        args: &[Expr],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        if args.len() == 1 {
            let t = self.solver().deep_force(self.expr_infer(&args[0], errors));
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                format!("revealed type: {}", t.deterministic_printing()),
            );
        } else {
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                format!(
                    "reveal_type needs 1 positional argument, got {}",
                    args.len()
                ),
            );
        }
        for keyword in keywords {
            unexpected_keyword(
                &|msg| {
                    self.error(errors, range, ErrorKind::Unknown, msg);
                },
                "reveal_type",
                keyword,
            );
        }
        Type::None
    }

    /// Simulates a call to `typing.cast`, whose signature is
    /// `(typ: type[T], val: Any) -> T: ...`
    /// (ignoring corner cases like special forms and forward references).
    /// The actual definition has additional overloads to accommodate said corner
    /// cases, with imprecise return types, which is why we need to hard-code this.
    pub fn call_typing_cast(
        &self,
        args: &[Expr],
        keywords: &[Keyword],
        range: TextRange,
        errors: &ErrorCollector,
    ) -> Type {
        let mut typ = None;
        let mut val = None;
        let mut extra = 0;
        match args {
            [] => {}
            [arg1] => {
                typ = Some(arg1);
            }
            [arg1, arg2, tail @ ..] => {
                typ = Some(arg1);
                val = Some(arg2);
                extra += tail.len();
            }
        }
        for keyword in keywords {
            match keyword.arg.as_ref().map(|id| id.as_str()) {
                Some("typ") => {
                    if typ.is_some() {
                        self.error(
                            errors,
                            range,
                            ErrorKind::Unknown,
                            "`typing.cast` got multiple values for argument `typ`".to_owned(),
                        );
                    }
                    typ = Some(&keyword.value);
                }
                Some("val") => {
                    if val.is_some() {
                        self.error(
                            errors,
                            range,
                            ErrorKind::Unknown,
                            "`typing.cast` got multiple values for argument `val`".to_owned(),
                        );
                    }
                    val = Some(&keyword.value);
                }
                _ => {
                    extra += 1;
                }
            }
        }
        if extra > 0 {
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                format!("`typing.cast` expected 2 arguments, got {}", extra + 2),
            );
        }
        let ret = if let Some(t) = typ {
            match self.untype_opt(self.expr_infer(t, errors), range) {
                Some(t) => t,
                None => self.error(
                    errors,
                    range,
                    ErrorKind::Unknown,
                    "First argument to `typing.cast` must be a type".to_owned(),
                ),
            }
        } else {
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                "`typing.cast` missing required argument `typ`".to_owned(),
            )
        };
        if val.is_none() {
            self.error(
                errors,
                range,
                ErrorKind::Unknown,
                "`typing.cast` missing required argument `val`".to_owned(),
            );
        }
        ret
    }
}
