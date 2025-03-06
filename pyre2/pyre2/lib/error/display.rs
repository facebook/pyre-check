/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::error::context::ErrorContext;
use crate::error::context::TypeCheckKind;
use crate::module::module_name::ModuleName;
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
    pub fn format_error(&self, got: &Type, want: &Type, current_module: ModuleName) -> String {
        let mut ctx = TypeDisplayContext::new();
        ctx.add(got);
        ctx.add(want);
        match self {
            Self::MagicMethodReturn(cls, func) => {
                ctx.add(cls);
                format!(
                    "Return type `{}` of function `{}.{}` is not assignable to expected return type `{}`",
                    ctx.display(got),
                    ctx.display(cls),
                    func,
                    ctx.display(want),
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
                "Returned type `{}` is not assignable to declared return type `{}`",
                ctx.display(got),
                ctx.display(want),
            ),
            Self::TypeGuardReturn => format!(
                "Returned type `{}` is not assignable to expected return type `bool` of type guard functions",
                ctx.display(got)
            ),
            Self::CallArgument(param, func_id) => {
                let param_desc = match param {
                    Some(name) => format!("parameter `{name}`"),
                    None => "parameter".to_owned(),
                };
                let func_desc = match func_id {
                    Some(func) => format!(" in function `{}`", func.format(current_module)),
                    None => "".to_owned(),
                };
                format!(
                    "Argument `{}` is not assignable to {} with type `{}`{}",
                    ctx.display(got),
                    param_desc,
                    ctx.display(want),
                    func_desc,
                )
            }
            Self::FunctionParameterDefault(param) => format!(
                "Default `{}` is not assignable to parameter `{}` with type `{}`",
                ctx.display(got),
                param,
                ctx.display(want),
            ),
            Self::TypedDictKey(key) => format!(
                "`{}` is not assignable to TypedDict key `{}` with type `{}`",
                ctx.display(got),
                key,
                ctx.display(want),
            ),
            Self::Attribute(attr) => format!(
                "`{}` is not assignable to attribute `{}` with type `{}`",
                ctx.display(got),
                attr,
                ctx.display(want),
            ),
            Self::AnnotatedName(var) => format!(
                "`{}` is not assignable to variable `{}` with type `{}`",
                ctx.display(got),
                var,
                ctx.display(want),
            ),
            // In an annotated assignment, the variable, type, and assigned value are all in the
            // same statement, so we can make the error message more concise and assume the context
            // is clear from the surrounding code.
            Self::AnnAssign => format!(
                "`{}` is not assignable to `{}`",
                ctx.display(got),
                ctx.display(want)
            ),
            Self::ExceptionClass => format!(
                "Invalid exception class: `{}` does not inherit from `{}`",
                ctx.display(got),
                ctx.display(want),
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
