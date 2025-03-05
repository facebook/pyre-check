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
            Self::MagicMethodReturn(cls, func) => {
                ctx.add(cls);
                format!(
                    "Expected `{}.{}` to return `{}`, got `{}`",
                    ctx.display(cls),
                    func,
                    ctx.display(want),
                    ctx.display(got)
                )
            }
            Self::ImplicitFunctionReturn(has_explicit_return) => {
                if *has_explicit_return {
                    format!(
                        "Function declared to return `{}`, but one or more paths are missing an explicit `return`",
                        ctx.display(want),
                    )
                } else {
                    format!(
                        "Function declared to return `{}` but is missing an explicit `return`",
                        ctx.display(want)
                    )
                }
            }
            Self::ExplicitFunctionReturn => format!(
                "Function declared to return `{}`, actually returns `{}`",
                ctx.display(want),
                ctx.display(got)
            ),
            Self::TypeGuardReturn => format!(
                "Expected type guard function to return `bool`, actually returns `{}`",
                ctx.display(got)
            ),
            Self::TypedDictKey(key) => format!(
                "TypedDict key `{}` declared with type `{}`, cannot assign `{}`",
                key,
                ctx.display(want),
                ctx.display(got),
            ),
            Self::ExplicitTypeAnnotation => format!(
                "Expected declared type `{}`, got `{}`",
                ctx.display(want),
                ctx.display(got)
            ),
            Self::Unknown => {
                format!("EXPECTED {} <: {}", ctx.display(got), ctx.display(want))
            }
            Self::Test => {
                format!(
                    "TEST TEST TEST - got: {}, want: {}",
                    ctx.display(got),
                    ctx.display(want)
                )
            }
        }
    }
}
