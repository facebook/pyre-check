/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::error::context::ErrorContext;
use crate::error::context::TypeCheckKind;
use crate::types::display::TypeDisplayContext;
use crate::types::types::Type;

impl ErrorContext {
    pub fn format(&self) -> String {
        match self {
            ErrorContext::BadContextManager(cm) => {
                format!("Cannot use `{cm}` as a context manager")
            }
        }
    }
}

impl TypeCheckKind {
    pub fn format_error(&self, got: &Type, want: &Type) -> String {
        let mut ctx = TypeDisplayContext::new();
        ctx.add(got);
        ctx.add(want);
        match self {
            Self::FunctionReturn(func, defining_cls) => {
                let func_name = match defining_cls {
                    Some(cls) => {
                        ctx.add(cls);
                        format!("{}.{}", ctx.display(cls), func)
                    }
                    None => format!("{func}"),
                };
                format!(
                    "For return type of function `{}`, expected `{}`, got `{}`",
                    func_name,
                    ctx.display(want),
                    ctx.display(got)
                )
            }
            Self::Unknown => {
                format!("EXPECTED {} <: {}", ctx.display(got), ctx.display(want))
            }
        }
    }
}
