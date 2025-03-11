/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use ruff_python_ast::Expr;
use ruff_python_ast::ExprCall;
use ruff_python_ast::ExprYield;
use ruff_python_ast::ExprYieldFrom;
use ruff_python_ast::Stmt;
use ruff_text_size::Ranged;
use ruff_text_size::TextRange;
use ruff_text_size::TextSize;

use crate::module::module_info::ModuleInfo;
use crate::util::display::DisplayWith;
use crate::util::display::DisplayWithCtx;

// Special module-specific types

impl ModuleInfo {
    pub fn display<'a>(&'a self, x: &'a impl DisplayWith<ModuleInfo>) -> impl Display + 'a {
        x.display_with(self)
    }
}

impl DisplayWith<ModuleInfo> for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        // We have a special case for NoneLiteral because we might manufacture these
        // during type checking (e.g. in the return position)
        if let Expr::NoneLiteral(_) = self {
            write!(f, "None")
        } else {
            write!(f, "{}", m.code_at(self.range()))
        }
    }
}

impl DisplayWith<ModuleInfo> for ExprYield {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", m.code_at(self.range()))
    }
}

impl DisplayWith<ModuleInfo> for ExprYieldFrom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", m.code_at(self.range()))
    }
}

impl DisplayWith<ModuleInfo> for ExprCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", m.code_at(self.range()))
    }
}

impl DisplayWith<ModuleInfo> for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", m.code_at(self.range()))
    }
}

impl DisplayWith<ModuleInfo> for TextRange {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", m.source_range(*self))
    }
}

impl DisplayWith<ModuleInfo> for TextSize {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, m: &ModuleInfo) -> fmt::Result {
        write!(f, "{}", m.source_location(*self))
    }
}
