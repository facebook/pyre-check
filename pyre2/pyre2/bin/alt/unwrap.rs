/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use ruff_text_size::TextRange;

use crate::alt::answers::AnswersSolver;
use crate::alt::answers::LookupAnswer;
use crate::types::types::Type;
use crate::types::types::Var;

impl<'a, Ans: LookupAnswer> AnswersSolver<'a, Ans> {
    fn fresh_var(&self) -> Var {
        self.solver().fresh_unwrap(self.uniques)
    }

    fn force_var(&self, var: Var) -> Type {
        self.solver().force_var(var)
    }

    fn is_subset_eq(&self, got: &Type, want: &Type) -> bool {
        self.solver().is_subset_eq(got, want, self.type_order())
    }

    // If `error_range` is None, do not report errors
    pub fn unwrap_awaitable(&self, ty: &Type, error_range: Option<TextRange>) -> Type {
        let var = self.fresh_var();
        let awaitable_ty = self.stdlib.awaitable(var.to_type()).to_type();
        let is_awaitable = self.is_subset_eq(ty, &awaitable_ty);
        if !is_awaitable && let Some(range) = error_range {
            self.error(range, "Expression is not awaitable".to_owned())
        } else {
            self.force_var(var)
        }
    }
}
