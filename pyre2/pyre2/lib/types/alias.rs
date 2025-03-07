/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! Things defined as `Dict = _Alias()` which should really be defined as `Dict = dict`.
//! But there are a few nuances where it is a bit different, e.g. `Dict()` doesn't create a dictionary.
//! See <https://github.com/python/typeshed/pull/13589> for details.

use ruff_python_ast::name::Name;
use ruff_python_ast::Expr;

use crate::module::module_name::ModuleName;

/// Does it match `_Alias()`
fn is_alias_call(x: &Expr) -> bool {
    if let Expr::Call(x) = x
        && x.arguments.is_empty()
        && let Expr::Name(x) = &*x.func
        && x.id == "_Alias"
    {
        return true;
    }
    false
}

/// Must be defined in `typing`.
pub fn resolve_typeshed_alias(
    module: ModuleName,
    lhs: &Name,
    rhs: &Expr,
) -> Option<(ModuleName, Name)> {
    if module != ModuleName::typing() && module != ModuleName::typing_extensions() {
        return None;
    }
    if !is_alias_call(rhs) {
        return None;
    }
    match lhs.as_str() {
        "List" => Some((ModuleName::builtins(), Name::new("list"))),
        "Dict" => Some((ModuleName::builtins(), Name::new("dict"))),
        "Set" => Some((ModuleName::builtins(), Name::new("set"))),
        "FrozenSet" => Some((ModuleName::builtins(), Name::new("frozenset"))),
        "DefaultDict" => Some((ModuleName::collections(), Name::new("defaultdict"))),
        "Counter" => Some((ModuleName::collections(), Name::new("Counter"))),
        "Deque" => Some((ModuleName::collections(), Name::new("deque"))),
        "ChainMap" => Some((ModuleName::collections(), Name::new("ChainMap"))),
        "OrderedDict" => Some((ModuleName::collections(), Name::new("OrderedDict"))),
        _ => None,
    }
}
