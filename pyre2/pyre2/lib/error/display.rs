/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use crate::error::context::TypeCheckContext;
use crate::types::display::TypeDisplayContext;
use crate::types::types::Type;

impl TypeCheckContext {
    pub fn format_error(&self, got: &Type, want: &Type) -> String {
        let mut ctx = TypeDisplayContext::new();
        ctx.add(got);
        ctx.add(want);
        format!("EXPECTED {} <: {}", ctx.display(got), ctx.display(want))
    }
}
