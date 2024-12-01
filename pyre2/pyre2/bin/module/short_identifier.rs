/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;

use ruff_python_ast::ExprName;
use ruff_python_ast::Identifier;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;

use crate::module::module_info::ModuleInfo;
use crate::util::display::DisplayWith;

/// An identifier, where we can drop the `Name` part because it came from a `ModuleInfo`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ShortIdentifier(TextRange);
impl ShortIdentifier {
    pub fn new(name: &Identifier) -> Self {
        Self(name.range)
    }

    pub fn expr_name(x: &ExprName) -> Self {
        // Not represented as an Identifier, but literally in the source code in the same way
        Self(x.range)
    }
}

impl Ranged for ShortIdentifier {
    fn range(&self) -> TextRange {
        self.0
    }
}

impl DisplayWith<ModuleInfo> for ShortIdentifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, ctx: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", ctx.code_at(self.0))
    }
}
