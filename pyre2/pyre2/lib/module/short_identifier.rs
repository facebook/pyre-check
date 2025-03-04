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

#[cfg(test)]
mod tests {
    use std::path::Path;
    use std::sync::Arc;

    use dupe::Dupe;
    use ruff_python_ast::Expr;
    use ruff_python_ast::Stmt;
    use ruff_python_ast::StmtAssign;

    use super::*;
    use crate::error::collector::ErrorCollector;
    use crate::error::style::ErrorStyle;
    use crate::module::module_name::ModuleName;
    use crate::module::module_path::ModulePath;

    fn from_expr(x: &Expr) -> ShortIdentifier {
        match x {
            Expr::Name(x) => ShortIdentifier::expr_name(x),
            _ => unreachable!(),
        }
    }

    #[test]
    fn test_display_short_identifier() {
        let module_info = ModuleInfo::new(
            ModuleName::from_str("foo"),
            ModulePath::filesystem(Path::new("foo.py").to_owned()),
            Arc::new("hello_world = Baz123.attribute".to_owned()),
        );
        let module = module_info.parse(&ErrorCollector::new(
            module_info.dupe(),
            ErrorStyle::Delayed,
        ));
        let show = |x: &ShortIdentifier| module_info.display(x).to_string();
        if let Stmt::Assign(StmtAssign {
            targets: x1,
            value: box Expr::Attribute(x23),
            ..
        }) = &module.body[0]
        {
            assert_eq!(show(&from_expr(&x1[0])), "hello_world");
            assert_eq!(show(&from_expr(&x23.value)), "Baz123");
            assert_eq!(show(&ShortIdentifier::new(&x23.attr)), "attribute");
            return;
        }
        unreachable!();
    }
}
